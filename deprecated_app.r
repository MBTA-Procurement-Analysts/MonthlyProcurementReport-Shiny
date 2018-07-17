# MonthlyProcurementReport-Shiny/app.r
# Recreating a MPR using Plotly and Shiny
# By: Nianmin Guo

# Setup -------------------------------------------------------------------

# library(tidyverse)
# library(plotly)
# library(kableExtra)
# library(lubridate)
# library(DT)
# library(scales)
# library(shiny)
# library(crosstalk)
# library(magrittr)
# 
# # Clear workspace
# rm(list = ls())
# 
# # Plotly APIs
# Sys.setenv("plotly_username" = "Zenmai0822")
# Sys.setenv("plotly_api_key" = "1qC2QkZBYFrJzOG9RW9i")
# 
# 
# # VARIABLES  -- VERIFY BEFORE RUNNING -------------------------------------
# 
# # Working Directory
# setwd("C:/Users/nguo/Documents/github/MonthlyProcurementReport-Shiny")
# 
# source("2-overall-spending.r")
# 
# 
# 
# server <- function(input, output) {
#   
#   output$p1_line_all <- renderPlotly(plot_line_month_all)
#   output$d1_all <- renderDT(DT::datatable(po_all_top10_table, 
#                                           rownames = FALSE,
#                                           options = list(dom = "t",
#                                                          columnDefs = list(list(className = 'dt-left', targets = 0:1),
#                                                                            list(className = 'dt-right', targets = 2))),
#                                           caption = "Top 10 POs from all") %>% 
#                               formatCurrency(c("Sum Amount")))
#   
#   output$p2_line_98pc <- renderPlotly(plot_line_month_98perc)
#   output$d2_98pc <- renderDT(DT::datatable(po_98perc_top10_table, 
#                                            rownames = FALSE,
#                                            options = list(dom = "t",
#                                                           columnDefs = list(list(className = 'dt-left', targets = 0:1),
#                                                                             list(className = 'dt-right', targets = 2))),
#                                            caption = "Top 10 POs from 98% percentile.") %>% 
#                                formatCurrency(c("Sum Amount")))
#   
#   output$d3_prev2mo <- renderDT(DT::datatable(po_prev2mos_all_table,
#                                               rownames = FALSE, 
#                                               options = list(dom = "t")) %>% 
#                                   formatCurrency(c("Sum")) %>% 
#                                   formatStyle('Unit', 
#                                               target = 'row',
#                                               fontWeight = styleEqual(c("May Total", "Jun Total"), c('bold', 'bold')),
#                                               backgroundColor = styleEqual(c("May Total", "Jun Total"), c("#dedede", "#dedede"))))
#   
#   output$p3_pie_spend_bunit <- renderPlotly(plot_pie_spend_bunit)
#   
#   output$d4_ytd_bunit <- renderDT(DT::datatable(po_spend_bunit_all, 
#                                                 rownames = FALSE, 
#                                                 options = list(dom = "t")) %>%
#                                     formatCurrency(c("Sum")) %>% 
#                                     formatStyle('Unit',
#                                                 target = 'row',
#                                                 fontWeight = styleEqual(c("Grand Total"), c('bold')), 
#                                                 backgroundColor = styleEqual(c("Grand Total"), c("#dedede"))))
# }
# Scratchpad --------------------------------------------------------------
