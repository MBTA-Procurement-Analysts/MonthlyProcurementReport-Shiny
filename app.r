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


# Plotly APIs
Sys.setenv("plotly_username" = "Zenmai0822")
Sys.setenv("plotly_api_key" = "1qC2QkZBYFrJzOG9RW9i")


# VARIABLES  -- VERIFY BEFORE RUNNING -------------------------------------

# Working Directory
setwd("C:/Users/nguo/Documents/github/MonthlyProcurementReport-Shiny")

# Two previous months, in chronological order

prevmos <- c("May", "Jun")

# or use this function that takes date of your local machine.
## prevmos <- as.character(month(month(today()) - c(2,1), label = TRUE, abbr = TRUE))

# Constants, Functions ----------------------------------------------------

# Dollar Formatting for Tables, not used now since DT can format itself
usd <- dollar_format(largest_with_cents = 1e+15, prefix = "$")

# FY Month Factors, July first
fy_factors <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Data Import -------------------------------------------------------------

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


# Plotting: Using ---------------------------------------------------------

plot_line_month_98perc <- monthly_98perc %>% 
  plot_ly(x = ~Month, 
          y = ~`Monthly_Sum`, 
          type = "scatter", 
          mode = "lines",
          hoveron = "points", 
          marker = list(symbol = 200,
                        size = 8)) %>% 
  layout(xaxis = list(title = "FY '18 Month"),
         yaxis = list(title = "Monthly Sum"))

plot_line_month_all <- monthly_all %>% 
  plot_ly(x = ~`Month`, 
          y = ~`Monthly_Sum`, 
          type = "scatter", 
          mode = "lines",
          hoveron = "points", 
          marker = list(symbol = 200,
                        size = 8)) %>% 
  layout(xaxis = list(title = "FY '18 Month"),
         yaxis = list(title = "Monthly Sum"))

plot_pie_spend_bunit <- po_spend_bunitfct %>% 
  plot_ly(x = 0, 
          y = ~Perc, 
          color = ~Unit, 
          type = "bar") %>% 
  add_text(x = 0, 
           y = ~CumSum, 
           text = ~Unit, 
           textfont = list(color = c("#000000"), 
                           size = 12)) %>% # make text labels go to half of current y
  layout(xaxis = list(title = "Business Unit",
                      showticklabels = FALSE, 
                      range = list(-0.5, 0.5)),
         yaxis = list(title = "", 
                      tickformat = "%"), 
         barmode = 'stack', 
         bargap = 0.45, 
         showlegend = FALSE)

# Plotting: Unused --------------------------------------------------------

plot_box_month_all <- raw_po %>% 
  plot_ly(x = ~`Month`, 
          y = ~`Sum Amount`, 
          type = "box")

plot_box_month_98perc <- po_98perc %>% 
  plot_ly(x = ~Month, 
          y = ~`Sum Amount`, 
          type = "box")

plot_monthly_po_spend <- plot_ly(monthly_po_spend) %>% 
  add_lines(x = ~Month, y = ~Monthly_Sum)

plot_monthly_po_count <- plot_ly(monthly_po_spend) %>% 
  add_lines(x = ~Month, y = ~Monthly_Count)

# Median Line of POs by month, with shading of 95% and 5% regions by month
# Pending
plot_line_mean_month_98perc <- po_month_98perc_9505 %>% 
  plot_ly(x = ~Month, 
          y = ~NinetyFifthPct,
          line = list(color = 'transparent'), 
          name = "95th Percentile", type = 'scatter', mode = 'lines', showlegend = FALSE) %>% 
  add_trace(x = ~Month,
            y = ~FifthPct,
            fill = 'tonexty',
            line = list(color = ' transparent'),
            fillcolor = 'rgba(0,0,255,0.2)',
            name = "5th Percentile") %>% 
  add_trace(x = ~Month,
            y = ~Median, 
            line = list(color = 'rgb(0,0,255)'), 
            name = "Median")

# Data Tables -------------------------------------------------------------

(po_98perc_top10_table <- po_98perc_top10 %>% 
   mutate_at("PO Date", date))

(po_all_top10_table <- po_all_top10 %>% 
    mutate_at("PO Date", date))

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

(po_prev2mos_gt_table <- po_prev2mos_bunit_table %>% 
    summarise(Month = NA, Unit = "Grand Total", Count = sum(Count), Sum = sum(Sum)))

(po_prev2mos_all_table <- bind_rows(po_2moago_bunit_table, 
                                    po_prev2mos_bunit_table[1,], 
                                    po_1moago_bunit_table, 
                                    po_prev2mos_bunit_table[2,]) %>% 
    ungroup(Month) %>% 
    select(-Month))

(po_spend_bunit_table <- po_spend_bunit %>% 
    select(Unit, Count, Sum))

(po_spend_gt_table <- po_spend_bunit_table %>% 
    summarise(Unit = "Grand Total",  Count = sum(Count), Sum = sum(Sum)))


(po_spend_bunit_all <- bind_rows(po_spend_bunit_table,
                                 po_spend_gt_table))

# Shiny -------------------------------------------------------------------

ui <- fluidPage(responsive = TRUE, 
                #theme = "bootstrap.css",
  verticalLayout(
    fluidRow(column(8, 
                    h1("Overall PO Count and Spend")),
             column(2, offset = 2, 
                    img(src = "t-logo.jpg", 
                        style = "float:right",
                        width = "75px", 
                        height = "75px",
                        class = "p-1"))),
    tags$hr(),
    fluidRow(column(8, 
                    h2("All POs FY 2018"),
                    p("With 12,378 POs"),
                    plotlyOutput("p1_line_all"),
                    DTOutput("d1_all")),
             
             column(4,
                    h2("POs FY 2018"),
                    p("With 248 outliers (2%) removed"),
                    plotlyOutput("p2_line_98pc"),
                    DTOutput("d2_98pc"))),
    tags$hr(),
    fluidRow(column(6, 
                    h3("POs of Previous 2 Months"),
                    DTOutput("d3_prev2mo")),
             column(6,
                    fluidRow(
                      h3("FY18 Total Spend by Business Unit"), 
                      column(4, align = "center", plotlyOutput("p3_pie_spend_bunit")),
                      column(8, DTOutput("d4_ytd_bunit"))))),
    tags$hr(),
    p(style = "text-align:right", "Draft for Discussion and Policy Purposes Only")))

server <- function(input, output) {
  
  output$p1_line_all <- renderPlotly(plot_line_month_all)
  output$d1_all <- renderDT(DT::datatable(po_all_top10_table, 
                                          rownames = FALSE,
                                          options = list(dom = "t",
                                                         columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                           list(className = 'dt-right', targets = 2))),
                                          caption = "Top 10 POs from all") %>% 
                              formatCurrency(c("Sum Amount")))
  
  output$p2_line_98pc <- renderPlotly(plot_line_month_98perc)
  output$d2_98pc <- renderDT(DT::datatable(po_98perc_top10_table, 
                                           rownames = FALSE,
                                           options = list(dom = "t",
                                                          columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                            list(className = 'dt-right', targets = 2))),
                                           caption = "Top 10 POs from 98% percentile.") %>% 
                               formatCurrency(c("Sum Amount")))
  
  output$d3_prev2mo <- renderDT(DT::datatable(po_prev2mos_all_table,
                                              rownames = FALSE, 
                                              options = list(dom = "t")) %>% 
                                  formatCurrency(c("Sum")) %>% 
                                  formatStyle('Unit', 
                                              target = 'row',
                                              fontWeight = styleEqual(c("May Total", "Jun Total"), c('bold', 'bold')),
                                              backgroundColor = styleEqual(c("May Total", "Jun Total"), c("#dedede", "#dedede"))))
  
  output$p3_pie_spend_bunit <- renderPlotly(plot_pie_spend_bunit)
  
  output$d4_ytd_bunit <- renderDT(DT::datatable(po_spend_bunit_all, 
                                                rownames = FALSE, 
                                                options = list(dom = "t")) %>%
                                    formatCurrency(c("Sum")) %>% 
                                    formatStyle('Unit',
                                                target = 'row',
                                                fontWeight = styleEqual(c("Grand Total"), c('bold')), 
                                                backgroundColor = styleEqual(c("Grand Total"), c("#dedede"))))
}

shinyApp(ui, server)

# Scratchpad --------------------------------------------------------------
