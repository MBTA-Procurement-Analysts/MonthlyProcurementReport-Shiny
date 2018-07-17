# File: ui.r
# Created by: Mickey Guo
# UI Function of the Procurement Report Shiny App

# Setup, Library Imports --------------------------------------------------

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

# Plotly APIs, no use right now but just in case
Sys.setenv("plotly_username" = "Zenmai0822")
Sys.setenv("plotly_api_key" = "1qC2QkZBYFrJzOG9RW9i")


# Variables, Verify before Running ----------------------------------------

# Working Directory
setwd("C:/Users/nguo/Documents/github/MonthlyProcurementReport-Shiny")


# Collect Variables for All Pages -----------------------------------------
# So that the server and ui can use objects from those pages

source("pages/2-overall-spending.r")
source("pages/4-purchase-platforms.r")


# UI Function, make sure this stays at the bottom -------------------------

# titlelogo <- fluidPage(fillRow(img(src = "t-logo.jpg",
#                                    width = "28px",
#                                    height = "28px",
#                                    class = "p-5"), p("Procurement Report FY 2018"), flex = NA))

navbarPage(title = "Procurement Report FY 2018",
                 uipg2,
                 tabPanel("Page 3"),
                 uipg4
)