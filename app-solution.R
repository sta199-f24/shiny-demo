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
    title = "Weather forecast",
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
      ),
      hr(),
      p("The data come from the Visual Crossing API.")
    ),
    layout_column_wrap(
      value_box(
        title = "City",
        value = textOutput("city"),
        showcase = bs_icon("geo-fill"),
        p("Dec 1 - 18, 2024"),
        theme = "info"
      ),
      value_box(
        title = "Number of clear days",
        value = textOutput("n_clear_days"),
        showcase = bs_icon("sun-fill"),
        theme = "yellow"
      ),
      value_box(
        title = "Number of rainy days",
        value = textOutput("n_rainy_days"),
        showcase = bs_icon("cloud-drizzle-fill"),
        theme = "blue"
      ),
      value_box(
        title = "Number of snowy days",
        value = textOutput("n_snowy_days"),
        showcase = bs_icon("cloud-snow-fill"),
        theme = "light"
      )
    ),
    navset_card_tab(
      nav_panel(title = "Plot", plotOutput("plot")),
      nav_panel(title = "Data", dataTableOutput("data"))
    )
  ),
  server = function(input, output, session) {

    # output for selected city
    output$city <- renderText({ str_to_upper(input$city) })

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
        labs(title = input$var) +
        scale_x_datetime(
          date_breaks = "week",
          date_minor_breaks = "day",
          date_labels = "%a, %b %d"
        ) +
        theme_bw(base_size = 14) +
        geom_vline(
          xintercept = now(),
          linetype = "dotted", linewidth = 0.8,
          color = "#c95c54"
        ) +
        geom_label(
          x = now(),
          y = max(weather_city() |> select(input$var)) * 0.9,
          label = "today",
          color = "#c95c54",
          size = 6
        )
    })

    # make table
    output$data <- DT::renderDT({
      weather_city() |>
        select(City, DateTime, input$var)
    })

    # calculate value boxes
    weather_day_conditions <- reactive({
      weather_city() |>
        mutate(day = yday(DateTime)) |>
        select(day, Conditions) |>
        group_by(day) |>
        distinct(Conditions) |>
        summarize(day_conditions = glue_collapse(Conditions, sep = ", ")) |>
        mutate(
          clear = str_detect(day_conditions, "Clear"),
          rain = str_detect(day_conditions, "Rain"),
          snow = str_detect(day_conditions, "Snow")
        )
    })

    output$n_clear_days <- renderText(sum(weather_day_conditions()$clear))
    output$n_rainy_days <- renderText(sum(weather_day_conditions()$rain))
    output$n_snowy_days <- renderText(sum(weather_day_conditions()$snow))
  }
)
