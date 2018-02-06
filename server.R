#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(quantmod)
library(egcm)
library(urca)
library(gridExtra)
library(cowplot)
library(DT)

half_life <- function(egcm_inp) {
  ##half life
  y <- egcm_inp$residuals
  y_lag <- Lag(y, k = 1)
  delta_y <- diff(y)
  half_df <- data_frame(y = y[-1], y_lag = as.numeric(y_lag)[-1], delta_y = as.numeric(delta_y))
  half_lm <- lm(delta_y ~ y_lag, data = half_df)
  lambda <- summary(half_lm)$coefficients[2]
  ret <- -log(2)/lambda
  ret
}



shinyServer(function(input, output) {

  ###get training data
  train_ser <- reactive({yegcm(input$ticker_X, input$ticker_Y, start = input$trn_start, end = input$trn_end, include.const = T)})
  ###get testing data
  get_ser <- reactive({yegcm(input$ticker_X, input$ticker_Y, start = input$tst_start, end = input$tst_end, include.const = T)})

  ###cointegration plots from egcm
  output$train_coint_plot <- renderPlot({
    plot(train_ser())
  })

  ###print output from egcm on training data
  output$train_coint_mod <- renderPrint({
    print(train_ser())
  })

  ###print hedge ratio from training data
  output$hedge_output <- renderPrint({
    hr <- round(train_ser()$beta, digits = 2)
    paste('The hedge ratio is ', hr, '. This means a portfolio should short ', hr, ' units of ', input$ticker_X, ' for every unit of ', input$ticker_Y, sep = '')
  })

  ###half life of mean reactivity
  output$half_life <- renderPrint({
    hl <- round(half_life(train_ser()), digits = 2)
    paste('Half-life of mean reversion is', hl, sep = ' ')
  })

  #############testing data
  test_obj <- reactive({
    ##risk free rate
    rf_rate <- .0008

    y_ser <- get_ser()$S2
    x_ser <- get_ser()$S1
    tot_len <- length(y_ser)

    ###look back period
    hl <- round(input$lookback_period)

    init_cap <- input$init_cap
    curr_cap <- init_cap
    prev_cap <- curr_cap
    cap_ser <- rep(curr_cap, tot_len)
    cap_mult <- 1 - input$reserve_cap/100
    ###hedge ratio
    hedge_ratio <- input$hedge_input

    #####get SPY for this period
    SPY <- getSymbols('SPY', from = input$tst_start,
                      to = input$tst_end, auto.assign = F)$SPY.Adjusted
    SPY_qty1 <- as.numeric(init_cap / SPY[hl + 1])
    #####

    if (length(y_ser) != length(x_ser)) {
      stop('series have different lengths')
    }
    tot_len <- length(y_ser)
    ret_ser <- rep(NA, tot_len)
    spread_ser <- rep(NA, tot_len)
    time_ser <- rep(NA, tot_len)
    lb_hedge <- rep(NA, tot_len)
    lb_spread <- rep(NA, tot_len)
    lb_sd <- rep(NA, tot_len)
    bs_event <- rep('None', tot_len)
    bs_price <- rep(NA, tot_len)
    bs_event_tr <- rep(NA, tot_len)
    curr_ret <- rep(0, tot_len)
    # bs_time <- rep(NA, tot_len)
    profit <- rep(0, tot_len)
    SPY_ret_ser <- rep(NA, tot_len)
    in_short <- F
    in_long <- F
    for(i in (hl + 1):tot_len) {
      curr_ind <- (i - hl):i

      lookback_y <- as.numeric(y_ser[curr_ind])
      lookback_x <- as.numeric(x_ser[curr_ind])

      lookback_spread <- mean(lookback_y - hedge_ratio * lookback_x)
      lookback_sd <- sd(lookback_y - hedge_ratio * lookback_x)

      lower_thr <- lookback_spread - lookback_sd
      upper_thr <- lookback_spread + lookback_sd

      lb_spread[i] <- lookback_spread
      lb_sd[i] <- lookback_sd

      # lm_fit <- lm(lookback_y ~ lookback_x)
      #
      # lookback_hedge <- summary(lm_fit)$coefficients[2]
      # lb_hedge[i] <- lookback_hedge
      # lookback_spread <- summary(lm_fit)$coefficients[1]
      # lookback_sd <- sd(lm_fit$residuals)

      curr_y <- as.numeric(y_ser[i])
      curr_x <- as.numeric(x_ser[i])

      # curr_time <- get_ser()$index[i]
      # time_ser[i] <- curr_time

      curr_spread <- as.numeric(curr_y - hedge_ratio * curr_x)
      spread_ser[i] <- curr_spread


      ####exiting a trade
      if (in_short & (curr_spread < upper_thr)) { ##currently in short
        bs_price[i] <- curr_spread * sh_qty * sign(curr_spread)
        profit[i] <- (prev_price * sign(prev_price) - curr_spread * sign(curr_spread)) * sh_qty
        curr_cap <- curr_cap + profit[i]
        bs_event[i] <- 'Buy'
        bs_event_tr[i] <- 'Buy exit short'
        curr_ret[i] <- profit[i]/prev_cap
        # bs_time[i] <- curr_time
        in_short <- F
        in_long <- F
      }
      if (in_long & (curr_spread > lower_thr)) { ##currently in long
        bs_price[i] <- curr_spread * sh_qty * sign(curr_spread)
        profit[i] <- (curr_spread * sign(curr_spread) - prev_price * sign(prev_price)) * sh_qty
        curr_cap <- curr_cap + profit[i]
        bs_event[i] <- 'Sell'
        bs_event_tr[i] <- 'Sell exit long'
        curr_ret[i] <- profit[i]/prev_cap
        # bs_time[i] <- curr_time
        in_short <- F
        in_long <- F
      }
      ####
      ####entering a trade
      if ((curr_spread > upper_thr) & !in_short & curr_cap > init_cap / 100) { ##not currently in trade, enter short
        sh_qty <- curr_cap / curr_spread * cap_mult
        bs_price[i] <- curr_spread * sh_qty * sign(curr_spread)
        prev_price <- curr_spread
        bs_event[i] <- 'Sell'
        bs_event_tr[i] <- 'Sell enter short'
        prev_cap <- curr_cap
        # bs_time[i] <- curr_time
        in_short <- T
        in_long <- F
      }
      if ((curr_spread < lower_thr) & !in_long & curr_cap > init_cap / 100) { ##not currently in trade, enter long
        sh_qty <- curr_cap / curr_spread * cap_mult
        bs_price[i] <- curr_spread * sh_qty * sign(curr_spread)
        prev_price <- curr_spread
        bs_event[i] <- 'Buy'
        bs_event_tr[i] <- 'Buy enter long'
        prev_cap <- curr_cap
        # bs_time[i] <- curr_time
        in_short <- F
        in_long <- T
      }
      ####

      cap_ser[i] <- curr_cap
      ret_ser[i] <- (curr_cap - init_cap) / init_cap
      # prev_cap <- curr_cap

      ####SPY returns
      SPY_cap <- as.numeric(SPY_qty1 * SPY[i])
      SPY_ret_ser[i] <- (SPY_cap - init_cap) / init_cap
      ####

    }
    ##total returns of the strategy
    strat_ret <- (curr_cap - init_cap) / init_cap
    ##total returns of SPY
    SPY_tot_ret <- SPY_ret_ser[i]
    ##sharpe ratio
    strat_sharpe <- (mean(unique(ret_ser[ret_ser != 0]), na.rm = T) - rf_rate) / sd(unique(ret_ser[ret_ser != 0]), na.rm = T)

    ser_df <- data_frame(Time = get_ser()$index, Spread = spread_ser, Spread_lb = lb_spread, sd_lb = lb_sd,
                         Pairs_cumul_ret = ret_ser,
                         SPY_cumul_ret = SPY_ret_ser, ret = SPY_ret_ser, Event = bs_event) %>%
      na.omit()

    bs_df <- data_frame(Time = get_ser()$index, Price = round(bs_price, 2),
                        Event = bs_event_tr, Profit = round(profit, 2), Current_Return_pct = round(curr_ret * 100, 2)) %>%
      na.omit()


    return(list(strat_ret = round(strat_ret, 2) * 100, SPY_tot_ret = round(SPY_tot_ret, 2) * 100, ser_df = ser_df,
                sharpe = round(strat_sharpe, 2), rf_rate = rf_rate * 100, bs_df = bs_df,
                SPY_end = round(SPY_cap, 2), strat_end = round(curr_cap, 2)))
  })
  #############

  output$sharpe_ratio <- renderPrint({
    paste('Sharpe ratio (risk free rate of ', test_obj()$rf_rate, '%): ', test_obj()$sharpe, sep = '')
  })
  output$test_strat_total_return <- renderPrint({
    paste('Total returns of the pairs trading strategy: ', test_obj()$strat_ret, '%. Ending balance is ', test_obj()$strat_end, sep = '')
  })
  output$test_SPY_total_return <- renderPrint({
    paste('Total returns from holding the S&P 500: ', test_obj()$SPY_tot_ret, '%. Ending balance is ', test_obj()$SPY_end, sep = '')
  })
  output$plot_ret_events <- renderPlot({
    ser_gath <- test_obj()$ser_df %>%
      gather(key = 'Strategy', value = value, Pairs_cumul_ret, SPY_cumul_ret) %>%
      mutate(pos_neg = ifelse(value >=0, 'Positive', 'Negative'))

    pt_ret <- ggplot(ser_gath, aes(x = Time, y = value * 100)) +
      geom_line(aes(linetype = Strategy)) +
      geom_hline(yintercept = 0) +
      # geom_area(aes(fill = pos_neg)) +
      ylab('Cumulative Return (%)') +
      labs(title = 'Cumulative Returns')

    pt_events <- ggplot(test_obj()$ser_df, aes(x = Time, y = Spread)) +
      geom_point(aes(color = Event), alpha = .6) +
      geom_ribbon(aes(ymin = Spread_lb - sd_lb, ymax = Spread_lb + sd_lb), alpha = .4) +
      scale_color_manual(values = c('blue', 'black', 'red')) +
      labs(title = 'Buy/Sell Events')
    theme_bw()

    # grid.arrange(pt_ret, pt_events)
    plot_grid(pt_ret, pt_events, ncol = 1, align = 'v')
  })
  output$bs_table <- renderDataTable({
    test_obj()$bs_df
  })

})
