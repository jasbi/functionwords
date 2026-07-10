library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
library(arrow)
library(RColorBrewer)
library(purrr)
library(yaml)

source("styling.R")
source("data.R") # Data loads directly from processed_data/word_model_db.RDS via docs/data.R.
source("literature.R")
source("layout_and_plots.R")

shinyApp(ui = ui, server = server)