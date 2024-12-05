# load packages –---------------------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(DT)

# load data –-------------------------------------------------------------------

weather <- read_csv("data/weather.csv")

# create app –------------------------------------------------------------------

shinyApp(
  ui = page_sidebar(
    title = "Weather forecast"
    # theme selection goes here

    # sidebar widgets and data reference go here

    # value boxes go here

    # plot and data panels go here
  ),
  server = function(input, output, session) {

    # server code goes here

  }
)
