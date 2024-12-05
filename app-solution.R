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
    title = "Weather Forecasts",
    theme = bs_theme(bootswatch = "litera"),
    sidebar = sidebar(
      selectInput(
        "city", "Select a city",
        choices = c("Chicago", "Durham", "Los Angeles", "New York", "Omaha", "Seattle"),
        selected = "Durham"
      ),
      selectInput(
        "var", "Select a variable",
        choices = c()
      )
    ),
    layout_column_wrap(
      value_box(
        title = "Number of clear",
        value = textOutput("n_clear_days"),
        showcase = bs_icon("sun-fill"),
        p("days"),
        theme = "yellow"
      ),
      value_box(
        title = "Number of rainy",
        value = textOutput("n_rainy_days"),
        showcase = bs_icon("cloud-drizzle-fill"),
        p("days"),
        theme = "blue"
      ),
      value_box(
        title = "Number of snowy",
        value = textOutput("n_snowy_days"),
        showcase = bs_icon("cloud-snow-fill"),
        p("days"),
        theme = "light"
      )
    ),
    navset_card_tab(
      nav_panel(title = "Plot", plotOutput("plot")),
      nav_panel(title = "Data", dataTableOutput("data"))
    )
  ),
  server = function(input, output, session) {

    # weather for selected city
    weather_city <- reactive({
      weather |>
        filter(City %in% input$city)
    })

    # weather variables that are numeric and not constant for selected city
    weather_vars <- reactive({
      weather_city() |>
        select(where(is.numeric)) |>
        select(where(function(x) var(x) != 0)) |>
        names()
    })

    # update the select input when weather_vars changes
    observe({
      updateSelectInput(inputId = "var", choices = weather_vars())
    })

    # make plot
    output$plot <- renderPlot({
      req(input$var)
      weather_city() |>
        ggplot(aes_string(x = "DateTime", y = input$var)) +
        geom_line(linewidth = 1) +
        labs(title = paste(input$city, "-", input$var)) +
        scale_x_datetime(date_breaks = "week", date_labels = "%b %d") +
        theme_bw(base_size = 14)
    })

    # make table
    output$data <- DT::renderDT({
      weather_city() |>
        select(City, DateTime, input$var)
    })

    # calculate value boxes
    output$n_clear_days <- renderText(weather_city() |> mutate(day = day(DateTime)) |> relocate(day) |> group_by(day) |> distinct(Conditions) |> pull(Conditions) |> str_count("Clear") |> sum())
    output$n_rainy_days <- renderText(weather_city() |> mutate(day = day(DateTime)) |> relocate(day) |> group_by(day) |> distinct(Conditions) |> pull(Conditions) |> str_count("Rain") |> sum())
    output$n_snowy_days <- renderText(weather_city() |> mutate(day = day(DateTime)) |> relocate(day) |> group_by(day) |> distinct(Conditions) |> pull(Conditions) |> str_count("Snow") |> sum())
  }
)
