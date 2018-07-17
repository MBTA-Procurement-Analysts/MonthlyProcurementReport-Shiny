# Procurement Report Shiny App

This is a web-based Monthly Procurement Report utilizing `R`, and its packages. Proper data is needed to display the product.

## Dependencies
```
library(tidyverse)
library(plotly)
library(kableExtra)
library(lubridate)
library(DT)
library(scales)
library(shiny)
library(crosstalk)
library(magrittr)
```

## Setup and Run

Confirm the data files are in place. They are not a part of this repository. 

Then, in `R`:
```
setwd(Your_Working_Directory)
library(shiny)
runApp()
``` 
