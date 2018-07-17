# File: 2-overall-spendings.r
# Created by: Mickey Guo
# Overall FY18 PO Count and Spend, 
#   as well as Last 2 Mo / FY18 BUnit PO Count and Spend 


# Setup Constants, Variables, Functions -----------------------------------

# Dollar Formatting for Tables, not used now since DT can format itself
usd <- dollar_format(largest_with_cents = 1e+15, prefix = "$")

# FY Month Factors, July first
fy_factors <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Two previous months, in chronological order

prevmos <- c("May", "Jun")

# or use this function that takes date of your local machine.
## prevmos <- as.character(month(month(today()) - c(2,1), label = TRUE, abbr = TRUE))

# Data Import -------------------------------------------------------------

# Raw file before 7/16/2018
(raw_po <- readxl::read_excel("data/plotly-report.xlsx", 
                              sheet = "sheet1", 
                              skip = 1, 
                              col_types = c("text", "text", "numeric", "text", "date", 
                                            "text", "text", "text", "text", "text", 
                                            "numeric", "numeric", "text", "date", "text", 
                                            "numeric")))
# Raw file after 7/16/2018
(raw_po <- readxl::read_excel("data/07162018-1-Spending-sheet.xlsx", 
                              sheet = "Raw Data", 
                              col_types = c("text", "text", "numeric", "text", "date", 
                                            "text", "text", "text", "text", "text", 
                                            "numeric", "numeric", "text", "text", "text", 
                                            "text")))

# from Pivot table, only a groupby month and unit summary
# (pivot_po <- readxl::read_excel("plotly-report.xlsx", sheet = "Monthly PO Breakdown"))

# Pivot Table Data Handling -----------------------------------------------

# (pivot_po %<>% 
#    slice(1:(n()-1)) %>% # Remove last line (G. Total)
#    mutate(Month = rep(month.abb, each = 5)) %>% # create new variable that is (Inflate a Month list 5 times each)
#    rename(`Biz Unit` = `Row Labels`, `Sum` = `Sum of Sum Amount`, `Count` = `Distinct Count of PO No.` ) %>%  # Rename Cols
#    select(Month, everything()) %>% # Rearrange columns
#    filter(!Month == `Biz Unit`) %>% # Filter cases where Biz Unit is the month, gets rids of monthly totals
#    mutate_at(c("Month", "Biz Unit"), as_factor)) # mutate to factors for Month and BUnit
# 
# (monthly_po_spend <- pivot_po %>% 
#     group_by(Month) %>% 
#     summarise(Monthly_Sum = sum(`Sum`), Monthly_Count = sum(`Count`))) 


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
    select(Month, everything(), -`PO Qty`, -`Sum Amount`, -`Mfg Itm ID`, -`Mfg ID`, -`Level 1`, -`Level 2`) %>% 
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

# Spend and count by BUnit

(po_sum <- raw_po %>% 
    summarise(Sum = sum(`Sum Amount`), Count = n()))

(po_spend_bunit <- raw_po %>% 
    group_by(Unit) %>% 
    summarise(Sum = sum(`Sum Amount`), Count = n()) %>%
    mutate(Perc = Sum / po_sum[[1]]) %>% 
    arrange(desc(Perc)))

(po_count_bunit <- raw_po %>% 
    group_by(Unit) %>% 
    summarise(Sum = sum(`Sum Amount`), Count = n()) %>%
    mutate(PercCnt = Count / po_sum[[2]]) %>% 
    arrange(desc(PercCnt)))

# Putting Unit sorted by percent into factors, so that plotly can plot in correct order.
(bunit_spend_fct <- po_spend_bunit %>% 
    pull(Unit))

(bunit_count_fct <- po_count_bunit %>% 
    pull(Unit))

(po_spend_bunitfct <- po_spend_bunit %>% 
    mutate_at("Unit", ~parse_factor(., levels = bunit_spend_fct)) %>% 
    mutate(CumSum = cumsum(Perc) - 0.5 * Perc))

(po_count_bunitfct <- po_count_bunit %>% 
    mutate_at("Unit", ~parse_factor(., levels = bunit_count_fct)) %>% 
    mutate(CumSum = cumsum(PercCnt) - 0.5 * PercCnt))

# Outputs the median. 5%ile, abd 95%ile of 98%ile POs, by month 
(po_month_98perc_9505 <- po_98perc %>% 
    group_by(Month) %>%
    arrange(`Sum Amount`) %>% 
    summarise(Median = median(`Sum Amount`), 
              NinetyFifthPct = dplyr::nth(`Sum Amount`, round(0.95 * n())),
              FifthPct = dplyr::nth(`Sum Amount`, round(0.05 * n()))))

(po_month_98perc_median <- po_98perc %>% 
    group_by(Month) %>% 
    summarise(`Monthly_Sum` = sum(`Sum Amount`)) %>% 
    summarise(`Overall_Median` = median(`Monthly_Sum`), `Overall_SD` = sd(`Monthly_Sum`)) %>% 
    mutate(`Upper_Limit` = `Overall_Median` + 1 * `Overall_SD`, 
           `Lower_Limit` = `Overall_Median` - 1 * `Overall_SD`,
           `Upper_Limit2x` = `Overall_Median` + 2 * `Overall_SD`, 
           `Lower_Limit2x` = `Overall_Median` - 2 * `Overall_SD`))

# Plotting: Using ---------------------------------------------------------

(plot_line_month_98perc <- monthly_98perc %>% 
   plot_ly(x = ~Month, 
           y = ~`Monthly_Sum`, 
           type = "scatter", 
           mode = "lines",
           hoveron = "points", 
           marker = list(symbol = 200,
                         size = 8)) %>% 
   layout(xaxis = list(title = "FY '18 Month"),
          yaxis = list(title = "Monthly Sum")))

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



(plot_pie_spend_bunit <- po_spend_bunitfct %>% 
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
           yaxis = list(title = "% Spend", 
                        tickformat = "%"), 
           barmode = 'stack', 
           bargap = 0.45, 
           showlegend = FALSE))

(plot_pie_count_bunit <- po_count_bunitfct %>% 
    plot_ly(x = 0, 
            y = ~PercCnt, 
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
           yaxis = list(title = "PO Count", 
                        tickformat = "%"), 
           barmode = 'stack', 
           bargap = 0.45, 
           showlegend = FALSE))


# Plotting: Unused --------------------------------------------------------

# 98 percentile monthly line plot with median +- sd overlays, not using since not significant 
plot_line_month_98perc_median <- monthly_98perc %>% 
  plot_ly(x = ~Month, 
          y = ~`Monthly_Sum`, 
          type = "scatter", 
          mode = "lines",
          hoveron = "points", 
          marker = list(symbol = 200,
                        size = 8)) %>% 
  layout(xaxis = list(title = "FY '18 Month"),
         yaxis = list(title = "Monthly Sum"),
         shapes = list(
           list(type = "rect", 
                fillcolor = "blue",
                line = list(color = "blue"), 
                opacity = 0.2, 
                x0 = 'Jul', x1 = 'Jun', xref = "x",
                y0 = po_month_98perc_median$Lower_Limit,
                y1 = po_month_98perc_median$Upper_Limit,
                yref = "y"), 
           list(type = "rect", 
                fillcolor = "blue",
                line = list(color = "blue"), 
                opacity = 0.2, 
                x0 = 'Jul', x1 = 'Jun', xref = "x",
                y0 = po_month_98perc_median$Lower_Limit2x,
                y1 = po_month_98perc_median$Upper_Limit2x,
                yref = "y")))

# plot_box_month_all <- raw_po %>% 
#   plot_ly(x = ~`Month`, 
#           y = ~`Sum Amount`, 
#           type = "box")
# 
# plot_box_month_98perc <- po_98perc %>% 
#   plot_ly(x = ~Month, 
#           y = ~`Sum Amount`, 
#           type = "box")
# 
# plot_monthly_po_spend <- plot_ly(monthly_po_spend) %>% 
#   add_lines(x = ~Month, y = ~Monthly_Sum)
# 
# plot_monthly_po_count <- plot_ly(monthly_po_spend) %>% 
#   add_lines(x = ~Month, y = ~Monthly_Count)
# 
# # Median Line of POs by month, with shading of 95% and 5% regions by month
# # Pending
# plot_line_mean_month_98perc <- po_month_98perc_9505 %>% 
#   plot_ly(x = ~Month, 
#           y = ~NinetyFifthPct,
#           line = list(color = 'transparent'), 
#           name = "95th Percentile", type = 'scatter', mode = 'lines', showlegend = FALSE) %>% 
#   add_trace(x = ~Month,
#             y = ~FifthPct,
#             fill = 'tonexty',
#             line = list(color = ' transparent'),
#             fillcolor = 'rgba(0,0,255,0.2)',
#             name = "5th Percentile") %>% 
#   add_trace(x = ~Month,
#             y = ~Median, 
#             line = list(color = 'rgb(0,0,255)'), 
#             name = "Median")

# Data Tables -------------------------------------------------------------

(po_98perc_top10_table <- po_98perc_top10)

(po_all_top10_table <- po_all_top10)

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

uipg2 <- tabPanel("PO Count and Spend", verticalLayout(
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
                  p("With 12,377 POs"),
                  plotlyOutput("pg2_p1_line_all"),
                  DTOutput("pg2_d1_all")),
           
           column(4,
                  h2("POs FY 2018"),
                  p("With 248 outliers (2%) removed. Shaded area represents normal range (1SD)."),
                  plotlyOutput("pg2_p2_line_98pc"),
                  DTOutput("pg2_d2_98pc"))),
  tags$hr(),
  fluidRow(column(6, 
                  h3("POs of Previous 2 Months"),
                  DTOutput("pg2_d3_prev2mo")),
           column(6,
                  fluidRow(
                    h3("FY18 Total Spend by Business Unit"), 
                    column(4, align = "center", plotlyOutput("pg2_p3_pie_spend_bunit")),
                    column(8, DTOutput("pg2_d4_ytd_bunit"))))),
  tags$hr(),
  p(style = "text-align:right", "Draft for Discussion and Policy Purposes Only")))