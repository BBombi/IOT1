if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(rstudioapi, shiny, shinydashboard, DT, dplyr, highcharter,
               plotly)

# USER INTERFACE
ui <- dashboardPage(
  # Header ----
  dashboardHeader(title = "Energy", titleWidth = 250
  ),
  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datsets", tabName = "datasets", 
               icon = icon("charging-station")),
      menuItem("Graphs", tabName = "graph", icon = icon("chart-area"),
               menuItem("Historic Values", tabName = "historic",
                        icon = icon("chart-bar")),
               menuItem("Predictions", tabName = "predictions",
                        icon = icon("chart-line"))),
      menuItem("Text", tabName = "text", icon = icon("adn"))
    )
  ),
  # Body ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets", box(dataTableOutput("datasetTable"), 
                                        width=10)), 
      tabItem(tabName = "historic",
              selectInput(inputId = "select", label = "Select a granuality",
                          choices = list("Month", "Day", "Representative day"),
                          selected = "Month"), box(plotlyOutput("plot"), 
                                                   verbatimTextOutput("event"), 
                                                   width = 600)),
      tabItem(tabName = "predictions"
              # , selectInput(inputId = "select", label = "Select a granuality",
              #             choices = list("Day", "Month"),
              #             selected = "Day"), box(plotOutput("plot"), width = 600)
              ),
      tabItem(tabName = "text", box(plotlyOutput("plotday"),
                                    verbatimTextOutput("event"), width = 600))
    )
  )
)

# SERVER ----
server <- function(input, output) {
  get.day <- reactive({
    switch (input$select,
      "Month" = plot_ly(Submeter_df1, x = Submeter_df1$yearmonth, 
                      y = Submeter_df1$Kitchen, name = "Kitchen", 
                      type = "scatter", mode = "lines") %>% 
        add_trace(y = Submeter_df1$Conditioning, 
                  name = "A/C-Heater room", mode = "lines") %>%
        add_trace(y = Submeter_df1$Laundry, name = "Laundry", 
                  mode = "lines") %>%
        add_trace(y = Submeter_df1$No_submetered, 
                  name = "Not Submetered", mode = "lines") %>%
        add_trace(y = Submeter_df1$Global_Energy, name = "Global Energy", 
                  mode = "lines") %>%
        layout(title = "Monthly energy consumed per submeter",
               xaxis = list(title = "Time"), 
               yaxis = list(title = "Energy (kW/h)")),
      "Day" = plot_ly(Day_submeter, x = Day_submeter$Date, 
                      y = Day_submeter$Kitchen, 
                      name = "Kitchen", type = "scatter", mode = "lines") %>% 
        add_trace(y = Day_submeter$Conditioning, 
                  name = "A/C-Heater room", mode = "lines") %>%
        add_trace(y = Day_submeter$Laundry, name = "Laundry", 
                  mode = "lines") %>%
        add_trace(y = Day_submeter$No_submetered, 
                  name = "Not Submetered", mode = "lines") %>%
        add_trace(y = Day_submeter$Global_Energy, name = "Global Energy", 
                  mode = "lines") %>%
        layout(title = "Daily energy consumed per submeter",
               xaxis = list(title = "Time"), 
               yaxis = list(title = "Energy (kW/h)")),
      "Representative day" = plot_ly(Rep_day_df, x = Rep_day_df$Hour,
                                     y = Rep_day_df$kitchen_energy,
                                     name = "Kitchen", type = "scatter",
                                     mode = "lines") %>%
        add_trace(y = Rep_day_df$conditioning_energy,
                  name = "A/C-Heater room", mode = "lines") %>%
        add_trace(y = Rep_day_df$laundry_energy, name = "Laundry",
                  mode = "lines") %>%
        add_trace(y = Rep_day_df$Energy_no_submetered,
                  name = "Not Submetered", mode = "lines") %>%
        add_trace(y = Rep_day_df$Global_Energy, name = "Global",
                  mode = "lines") %>%
        layout(title = "Representative day of Energy consumed per submeter",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Energy (kW/h)"))
    )
  })
  output$plot <- renderPlotly({
    get.day()
  })
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point" else d
  })
  output$plotday <- renderPlotly({
    plot_ly(Rep_day_df, x = Rep_day_df$Hour, 
            y = Rep_day_df$kitchen_energy, 
            name = "Kitchen", type = "scatter", 
            mode = "lines") %>% 
      add_trace(y = Rep_day_df$conditioning_energy, 
                name = "A/C-Heater room", mode = "lines") %>%
      add_trace(y = Rep_day_df$laundry_energy, name = "Laundry", 
                mode = "lines") %>%
      add_trace(y = Rep_day_df$Energy_no_submetered, 
                name = "Not Submetered", mode = "lines") %>%
      add_trace(y = Rep_day_df$Global_Energy, name = "Global", 
                mode = "lines") %>%
      layout(title = "Representative day of Energy consumed per submeter",
             xaxis = list(title = "Time"), 
             yaxis = list(title = "Energy (kW/h)"))
  })
}

# RUNNING APP
shinyApp(ui, server)
