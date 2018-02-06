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
