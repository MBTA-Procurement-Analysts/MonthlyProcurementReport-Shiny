# MonthlyProcurementReport-Shiny/crosstalk_test.r
# Project-specific sandbox for Crosstalk
# By: Nianmin Guo

# Setup -------------------------------------------------------------------

library(tidyverse)
library(plotly)
library(kableExtra)
library(lubridate)
library(DT)
library(scales)
library(shiny)
library(crosstalk)
library(magrittr)

# Clear workspace
rm(list = ls())

setwd("C:/Users/nguo/Documents/github/MonthlyProcurementReport-Shiny")

# Plotly APIs
Sys.setenv("plotly_username" = "Zenmai0822")
Sys.setenv("plotly_api_key" = "1qC2QkZBYFrJzOG9RW9i")

# Dollar Formatting for Tables
usd <- dollar_format(largest_with_cents = 1e+15, prefix = "$")

# Data Inport -------------------------------------------------------------

(raw_po <- readxl::read_excel("plotly-report.xlsx", 
                              sheet = "sheet1", 
                              skip = 1, 
                              col_types = c("text", "text", "numeric", "text", "date", 
                                            "text", "text", "text", "text", "text", 
                                            "numeric", "numeric", "text", "date", "text", 
                                            "numeric")))

# from Pivot table, only a groupby month and unit summary
(pivot_po <- readxl::read_excel("plotly-report.xlsx", sheet = "Monthly PO Breakdown"))

# Pivot Table Data Handling -----------------------------------------------

(pivot_po %<>% 
   slice(1:(n()-1)) %>% # Remove last line (G. Total)
   mutate(Month = rep(month.abb, each = 5)) %>% # create new variable that is (Inflate a Month list 5 times each)
   rename(`Biz Unit` = `Row Labels`, `Sum` = `Sum of Sum Amount`, `Count` = `Distinct Count of PO No.` ) %>%  # Rename Cols
   select(Month, everything()) %>% # Rearrange columns
   filter(!Month == `Biz Unit`) %>% # Filter cases where Biz Unit is the month, gets rids of monthly totals
   mutate_at(c("Month", "Biz Unit"), as_factor)) # mutate to factors for Month and BUnit

(monthly_po_spend <- pivot_po %>% 
    group_by(Month) %>% 
    summarise(Monthly_Sum = sum(`Sum`), Monthly_Count = sum(`Count`))) 


# Raw Data Handling -------------------------------------------------------

# FY Month Factors, July first
fy_factors <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                "Jan", "Feb", "Mar", "Apr", "May", "Jun")


# PER LINE ITEMS

(raw_req <- raw_po %>% 
    mutate(Month = month(`PO Date`, label = TRUE, abbr = TRUE)) %>% # Month for PO, in factor
    mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>% 
    select(-Line) %>% 
    group_by(`PO No.`) %>% 
    mutate(`Sum` = sum(`Sum Amount`)) %>% 
    select(Sum, everything()) %>%
    select(Month, everything())) # bump Month to first 

# PER PO, DISTINCT VARS BY LINE ITEM HAS BEEN REMOVED

(raw_po %<>% 
    mutate(Month = month(`PO Date`, label = TRUE, abbr = TRUE)) %>% # Month for PO, in factor
    mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>% 
    select(-Line) %>% 
    group_by(`PO No.`) %>% 
    mutate(Sum = sum(`Sum Amount`), Sum_Qty = sum(`PO Qty`)) %>% 
    select(Month, everything(), -`PO Qty`, -`Req Line`, -`Sum Amount`, -Item, -`Mfg Itm ID`, -`Mfg ID`, -`Req ID`) %>% 
    ungroup(`Po No.`) %>% 
    dplyr::distinct() %>% 
    rename(`Sum Amount` = Sum))

# 0 to 98 percentile of PO, in Sum Amount
(po_98perc <- raw_po %>% 
    arrange(`Sum Amount`) %>% 
    top_n(round(-0.98 * nrow(.)), `Sum Amount`))

(monthly_98perc <- po_98perc %>% 
    group_by(Month) %>% 
    summarise(Monthly_Sum = sum(`Sum Amount`)))

# top 10 of 0 to 98 percentile
(po_98perc_top10 <- po_98perc %>% 
    top_n(10, `Sum Amount`) %>% 
    arrange(desc(`Sum Amount`)) %>% 
    select(`PO No.`, `Month`, `Sum Amount`))

(monthly_all <- raw_po %>% 
    group_by(Month) %>% 
    summarise(Monthly_Sum = sum(`Sum Amount`)))

# top 10 of all
(po_all_top10 <- raw_po %>% 
    top_n(10, `Sum Amount`) %>% 
    arrange(desc(`Sum Amount`)) %>% 
    select(`PO No.`, `Month`, `Sum Amount`))

# Spend by BUnit

(po_sum <- raw_po %>% 
    summarise(Sum = sum(`Sum Amount`)))

(po_spend_bunit <- raw_po %>% 
    group_by(Unit) %>% 
    summarise(Sum = sum(`Sum Amount`), Count = n()) %>%
    mutate(Perc = Sum / po_sum[[1]]) %>% 
    arrange(desc(Perc)))

# Putting Unit sorted by percent into factors, so that plotly can plot in correct order.
(bunit_spend_fct <- po_spend_bunit %>% 
    pull(Unit))

(po_spend_bunitfct <- po_spend_bunit %>% 
    mutate_at("Unit", ~parse_factor(., levels = bunit_spend_fct)) %>% 
    mutate(CumSum = cumsum(Perc) - 0.5 * Perc))

# Outputs the median. 5%ile, abd 95%ile of 98%ile POs, by month 
(po_month_98perc_9505 <- po_98perc %>% 
    group_by(Month) %>%
    arrange(`Sum Amount`) %>% 
    summarise(Median = median(`Sum Amount`), 
              NinetyFifthPct = dplyr::nth(`Sum Amount`, round(0.95 * n())),
              FifthPct = dplyr::nth(`Sum Amount`, round(0.05 * n()))))


# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  verticalLayout(
    plotlyOutput("p1"),
    DTOutput("d1")
  )
)

server <- function(input, output) {
  
  share_raw_po <- SharedData$new(raw_po)
  
  output$p1 <- renderPlotly(
    {
      selected <- input$d1_rows_selected
      
      if (!length(selected)) {
        plot <- crosstalk::data(share_raw_po) %>% 
          dplyr::group_by(Month) %>% 
          summarise(Monthly_Sum = sum(`Sum Amount`)) %>% 
          plot_ly(x = ~Month, SSy = ~"Sum Amount", type = "scatter", mode = "line", name = "All Data") %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Selected PO(s)'))
      } else if (length(selected)) {
        plot_selected <- raw_po %>% 
          plot_ly() %>% 
          add_trace(x = ~Month, y = ~"Sum Amount", type = "scatter", mode = "line", color = I('black'), name = "All Data")
        plot_selected <- add_trace(plot_selected, data = raw_po[selected, , drop = F], x = ~Month, y = ~"Sum Amount", type = "scatter", 
                                   mode = "line", color = I("red"), name = "selected PO(s)")
      }
    }
  )
  
  output$d1 <- DT::renderDataTable({
    DT::datatable(po_all_top10, 
                  rownames = FALSE,
                  options = list(dom = "t",
                                 columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                   list(className = 'dt-right', targets = 2))),
                  caption = "Top 10 POs from all")
  })
  
  # output$p1_line_all <- renderPlotly(plot_line_month_all)
  # output$d1_all <- renderDT(DT::datatable(po_all_top10_table, 
  #                                         rownames = FALSE,
  #                                         options = list(dom = "t",
  #                                                        columnDefs = list(list(className = 'dt-left', targets = 0:1),
  #                                                                          list(className = 'dt-right', targets = 2))),
  #                                         caption = "Top 10 POs from all"))
  # 
  # output$p2_line_98pc <- renderPlotly(plot_line_month_98perc)
  # output$d2_98pc <- renderDT(DT::datatable(po_98perc_top10_table, 
  #                                          rownames = FALSE,
  #                                          options = list(dom = "t",
  #                                                         columnDefs = list(list(className = 'dt-left', targets = 0:1),
  #                                                                           list(className = 'dt-right', targets = 2))),
  #                                          caption = "Top 10 POs from 98% percentile."))
  # 
  # output$d3_prev2mo <- renderDT(DT::datatable(po_prev2mos_all_table,
  #                                             rownames = FALSE, 
  #                                             options = list(dom = "t")) %>% 
  #                                 formatCurrency(c("Sum")) %>% 
  #                                 formatStyle('Unit', 
  #                                             target = 'row',
  #                                             fontWeight = styleEqual(c("May Total", "Jun Total"), c('bold', 'bold')),
  #                                             backgroundColor = styleEqual(c("May Total", "Jun Total"), c("#dedede", "#dedede"))))
  # 
  # output$p3_pie_spend_bunit <- renderPlotly(plot_pie_spend_bunit)
  # 
  # output$d4_ytd_bunit <- renderDT(DT::datatable(po_spend_bunit_all, 
  #                                               rownames = FALSE, 
  #                                               options = list(dom = "t")) %>%
  #                                   formatCurrency(c("Sum")) %>% 
  #                                   formatStyle('Unit',
  #                                               target = 'row',
  #                                               fontWeight = styleEqual(c("Grand Total"), c('bold')), 
  #                                               backgroundColor = styleEqual(c("Grand Total"), c("#dedede"))))
}

shinyApp(ui, server)
