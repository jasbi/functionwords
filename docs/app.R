library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
library(feather)
library(RColorBrewer)
library(purrr)
library(yaml)

#source("data_prep.R") # Only rerun if modified this file!

source("styling.R")
source("data.R")
source("literature.R")
source("plots.R")

shinyApp(ui = ui, server = server)