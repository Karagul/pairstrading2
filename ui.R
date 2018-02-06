#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(
  navbarPage('',
             tabPanel('Training Data',
                      sidebarLayout(
                        sidebarPanel(
                          textInput('ticker_Y',
                                    'Input the Yahoo finance ticker for stock Y',
                                    value = 'EWC'),
                          textInput('ticker_X',
                                    'Input the Yahoo finance ticker for stock X',
                                    value = 'EWA'),
                          textInput('trn_start',
                                    'Input the start date',
                                    value = '2006-01-01'),
                          textInput('trn_end',
                                    'Input the end date',
                                    value = '2006-12-31')
                        ),
                        mainPanel(
                          plotOutput('train_coint_plot'),
                          verbatimTextOutput('train_coint_mod'),
                          verbatimTextOutput('hedge_output'),
                          verbatimTextOutput('half_life')
                        )
                      )
             ),
             tabPanel('Testing Data',
                      sidebarLayout(
                        sidebarPanel(
                          textInput('tst_start',
                                    'Input the start date',
                                    value = '2007-01-01'),
                          textInput('tst_end',
                                    'Input the end date',
                                    value = '2007-12-31'),
                          numericInput('lookback_period',
                                    'Input the number of days for the lookback period',
                                    min = 1,
                                    value = 20),
                          numericInput('init_cap',
                                    'Input the initial capital',
                                    min = 0,
                                    value = 1000),
                          numericInput('reserve_cap',
                                       'Input the % capital reserved on every trade',
                                       min = 0,
                                       max = 100,
                                       value = 15),
                          numericInput('hedge_input',
                                    'Input the hedge ratio',
                                    min = 0,
                                    value = 1)
                        ),
                        mainPanel(
                          verbatimTextOutput('sharpe_ratio'),
                          verbatimTextOutput('test_strat_total_return'),
                          verbatimTextOutput('test_SPY_total_return'),
                          plotOutput('plot_ret_events'),
                          dataTableOutput('bs_table')
                        )
                      )
             )
  )
)
