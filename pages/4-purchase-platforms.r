# File: 4overall-spendings.r
# Created by: Mickey Guo
# Spend and PO Counts based on Purchasing Platforms
#   Counts of POs <$1000 (BUnit and Platform)

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
# This page shares some of the data used by page 2 (Overall spending)
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


# Raw Data Handling -------------------------------------------------------

# PER LINE ITEMS

(pg4_raw_req <- raw_po %>% 
   mutate(Month = month(`PO Date`, label = TRUE, abbr = TRUE)) %>% # Month for PO, in factor
   mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>% 
   select(-Line) %>% 
   group_by(`PO No.`) %>% 
   mutate(`Sum` = sum(`Sum Amount`)) %>% 
   select(Sum, everything()) %>%
   select(Month, everything())) # bump Month to first 

# PER PO, DISTINCT VARS BY LINE ITEM HAS BEEN REMOVED

(pg4_raw_po <- raw_po %>%
    mutate(Month = month(`PO Date`, label = TRUE, abbr = TRUE)) %>% # Month for PO, in factor
    mutate_at("Month", ~parse_factor(., levels = fy_factors)) %>%
    select(-Line) %>%
    group_by(`PO No.`) %>%
    mutate(Sum = sum(`Sum Amount`), Sum_Qty = sum(`PO Qty`)) %>%
    select(Month, everything(), -`PO Qty`, -`Sum Amount`, -`Mfg Itm ID`, -`Mfg ID`, -`Level 1`, -`Level 2`) %>%
    ungroup(`Po No.`) %>%
    dplyr::distinct() %>%
    rename(`Sum Amount` = Sum))

# Added Threshold Categorization
# L: >= [50'000, up]; M: (3'500, 50'000); S: [0, 3'500]

(pg4_threshold_po <- pg4_raw_po %>% 
    mutate(Threshold = case_when(
      `Sum Amount` >= 50000 ~ "L",
      `Sum Amount` < 50000 & `Sum Amount` > 3500 ~ "M", 
      `Sum Amount` <= 3500 ~ "S" 
    )))

(pg4_threshold_table <- pg4_threshold_po %>% 
    group_by(Threshold) %>% 
    summarise(Count = n(), Spend = sum(`Sum Amount`)) %>% 
    mutate(PercCnt = Count / sum(Count), PercSum = Spend / sum(Spend)))

# Shiny -------------------------------------------------------------------

uipg4 <- tabPanel("Spend/Count by Platforms", 
                  verticalLayout(
  fluidRow(column(8, 
                  h1("Spend and PO Counts based on Purchasing Platforms")),
           column(2, offset = 2, 
                  img(src = "t-logo.jpg", 
                      style = "float:right",
                      width = "75px", 
                      height = "75px",
                      class = "p-1"))),
  tags$hr(),
  tags$hr(),
  p(style = "text-align:right", "Draft for Discussion and Policy Purposes Only")))