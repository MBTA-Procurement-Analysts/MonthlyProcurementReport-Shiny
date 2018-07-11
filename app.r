# MonthlyProcurementReport-Shiny/app.r
# Recreating a MPR using Plotly and Shiny
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

setwd("C:/Users/nguo/Documents/github/MonthlyProcurementReport-Shiny")

# Plotly APIs
Sys.setenv("plotly_username"="Zenmai0822")
Sys.setenv("plotly_api_key"="1qC2QkZBYFrJzOG9RW9i")

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


(raw_po %<>% 
    mutate(Month = month(`PO Date`, label = TRUE, abbr = TRUE)) %>% # Month for PO, in factor
    mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>% 
    select(Month, everything())) # bump Month to first 

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
    select(`PO No.`, `PO Date`, `Sum Amount`))

(monthly_all <- raw_po %>% 
    group_by(Month) %>% 
    summarise(Monthly_Sum = sum(`Sum Amount`)))

# top 10 of all
(po_all_top10 <- raw_po %>% 
    top_n(10, `Sum Amount`) %>% 
    arrange(desc(`Sum Amount`)) %>% 
    select(`PO No.`, `PO Date`, `Sum Amount`))

# Spend by BUnit

(po_sum <- raw_po %>% 
    summarise(Sum = sum(`Sum Amount`)))

(po_spend_bunit <- raw_po %>% 
    group_by(Unit) %>% 
    summarise(Sum = sum(`Sum Amount`)) %>%
    mutate(Perc = Sum / po_sum[[1]]) %>% 
    arrange(desc(Perc)))


# Plotting ----------------------------------------------------------------

(plot_box_month_all <- raw_po %>% 
   plot_ly(x = ~`Month`, 
           y = ~`Sum Amount`, 
           type = "box"))

(plot_box_month_98perc <- po_98perc %>% 
    plot_ly(x = ~Month, 
            y = ~`Sum Amount`, 
            type = "box"))

(plot_line_month_98perc <- monthly_98perc %>% 
    plot_ly(x = ~Month, 
            y = ~`Monthly_Sum`, 
            type = "scatter", 
            mode = "lines",
            hoveron = "points", 
            marker = list(symbol = 200,
                          size = 8)) %>% 
    layout(xaxis = list(title = "FY '18 Month"),
           yaxis = list(title = "Monthly Sum (98%ile)")))

(plot_line_month_all <- monthly_all %>% 
    plot_ly(x = ~`Month`, 
            y = ~`Monthly_Sum`, 
            type = "scatter", 
            mode = "lines",
            hoveron = "points", 
            marker = list(symbol = 200,
                          size = 8)) %>% 
    layout(xaxis = list(title = "FY '18 Month"),
           yaxis = list(title = "Monthly Sum")))

(plot_monthly_po_spend <- plot_ly(monthly_po_spend) %>% add_lines(x = ~Month, y = ~Monthly_Sum))

(plot_monthly_po_count <- plot_ly(monthly_po_spend) %>% add_lines(x = ~Month, y = ~Monthly_Count))


# Data Tables -------------------------------------------------------------

(po_98perc_top10_table <- po_98perc_top10 %>% 
   mutate_at("Sum Amount", ~usd(.)) %>% 
   mutate_at("PO Date", date))

(po_98perc_top10_dt <- datatable(po_98perc_top10_table, 
                                 rownames = FALSE,
                                 options = list(dom = "t",
                                                columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                  list(className = 'dt-right', targets = 2))),
                                 caption = "Top 10 POs from 98% percentile."))

(po_all_top10_table <- po_all_top10 %>% 
    mutate_at("Sum Amount", ~usd(.)) %>% 
    mutate_at("PO Date", date))

(po_all_top10_dt <- datatable(po_all_top10_table, 
                              rownames = FALSE,
                              options = list(dom = "t",
                                             columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                               list(className = 'dt-right', targets = 2))),
                              caption = "Top 10 POs from all"))

(prevmos <- c("May", "Jun"))

# Hack, not a tidy table, don't learn from this

(po_2moago_bunit_table <- raw_po %>% 
    filter(Month == prevmos[1]) %>% 
    group_by(Month, Unit) %>% 
    summarise(Count = n(), Sum = sum(`Sum Amount`)))

(po_1moago_bunit_table <- raw_po %>% 
    filter(Month == prevmos[2]) %>% 
    group_by(Month, Unit) %>% 
    summarise(Count = n(), Sum = sum(`Sum Amount`)))

(po_prev2mos_bunit_table <- raw_po %>% 
    filter(Month %in% prevmos) %>% 
    group_by(Month) %>% 
    summarise(Count = n(), Sum = sum(`Sum Amount`)) %>% 
    mutate(Unit = paste(as.character(Month), "Total")) %>% 
    select(Month, Unit, everything()))

(po_prev2mos_gt_table <- po_prev2mos_bunit_table %>% summarise(Month = NA, Unit = "Grand Total", Count = sum(Count), Sum = sum(Sum)))

(po_prev2mos_all_table <- bind_rows(po_2moago_bunit_table, 
                                    po_prev2mos_bunit_table[1,], 
                                    po_1moago_bunit_table, 
                                    po_prev2mos_bunit_table[2,]) %>% 
    ungroup(Month) %>% 
    select(-Month))

(po_prev2mos_all_datatable <- DT::datatable(po_prev2mos_all_table,
                                            rownames = FALSE, 
                                            options = list(dom = "t")) %>% 
    formatCurrency(c("Sum")) %>% 
    formatStyle('Unit', 
                target = 'row',
                fontWeight = styleEqual(c("May Total", "Jun Total"), c('bold', 'bold')),
                backgroundColor = styleEqual(c("May Total", "Jun Total"), c("#dedede", "#dedede")))
  )

# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  verticalLayout(
    fluidRow(column(6, 
                    h1("Overall PO Count and Spend")),
             column(2, offset = 4, 
                    img(src = "t-logo.jpg", 
                        style = "float:right",
                        width = "100px", 
                        height = "100px"))),
    tags$hr(),
    fluidRow(column(8, 
                    h2("All POs FY 2018"),
                    plotlyOutput("p1_line_all"),
                    DTOutput("d1_all")),
             
             column(4,
                    h2("98th Percentile POs FY 2018"),
                    plotlyOutput("p2_line_98pc"),
                    # note the 596 outliers removed 
                    DTOutput("d2_98pc"))),
    tags$hr(),
    fluidRow(column(4, DTOutput("d3_prev2mo")))
  )
)

server <- function(input, output) {
  
  output$p1_line_all <- renderPlotly(plot_line_month_all)
  output$d1_all <- renderDT(DT::datatable(po_all_top10_table, 
                                          rownames = FALSE,
                                          options = list(dom = "t",
                                                         columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                           list(className = 'dt-right', targets = 2))),
                                          caption = "Top 10 POs from all"))
  
  output$p2_line_98pc <- renderPlotly(plot_line_month_98perc)
  output$d2_98pc <- renderDT(DT::datatable(po_98perc_top10_table, 
                                           rownames = FALSE,
                                           options = list(dom = "t",
                                                          columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                            list(className = 'dt-right', targets = 2))),
                                           caption = "Top 10 POs from 98% percentile."))
  
  output$d3_prev2mo <- renderDT(DT::datatable(po_prev2mos_all_table,
                                              rownames = FALSE, 
                                              options = list(dom = "t")) %>% 
                                  formatCurrency(c("Sum")) %>% 
                                  formatStyle('Unit', 
                                              target = 'row',
                                              fontWeight = styleEqual(c("May Total", "Jun Total"), c('bold', 'bold')),
                                              backgroundColor = styleEqual(c("May Total", "Jun Total"), c("#dedede", "#dedede"))))
}

shinyApp(ui, server)