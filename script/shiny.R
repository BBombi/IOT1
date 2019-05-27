if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(rstudioapi, shiny, shinydashboard, DT, dplyr, highcharter,
               plotly)

# USER INTERFACE
ui <- dashboardPage(
  skin = "green",
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
                                                   width = 600),
              hr(),
              dateRangeInput("dates", label = h3("Date range"),
                             start = min(Submeter_df1$Date),
                             end = max(Submeter_df1$Date),
                             min = min(Submeter_df1$Date),
                             max = max(Submeter_df1$Date)),
              selectInput(inputId = "select2", label = "Select a granuality",
                          choices = list("Month", "Day"),
                          selected = "Month"),
              fluidRow(infoBoxOutput(width = 6, "box1")),
              fluidRow(infoBoxOutput(width = 6, "box2")),
              fluidRow(infoBoxOutput(width = 6, "box3")),
              fluidRow(infoBoxOutput(width = 6, "box4")),
              fluidRow(infoBoxOutput(width = 6, "box5"))),
      tabItem(tabName = "predictions", box(plotlyOutput("foreplot"), 
                                           width = 600)),
      tabItem(tabName = "text", box())
    )
  )
)

# SERVER ----
server <- function(input, output) {
  filteredData <- reactive({
    switch (input$select2,
      "Month" = subset(Submeter_df1, Submeter_df1$Date >= input$dates[1] & 
                         Submeter_df1$Date <= input$dates[2]),
      "Day" =subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                      Day_submeter$Date <= input$dates[2])
    )
})
  get.granularity <- reactive({
    switch (input$select,
      "Month" = plot_ly(Submeter_df1, x = Submeter_df1$Date, 
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
    get.granularity()
  })
  output$foreplot <- renderPlotly({
    plot_ly() %>%
      add_lines(x = time(Monthly_dfts), y = Monthly_dfts,
                color = I("black"), name = "observed") %>%
      add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], 
                  ymax = fore$upper[, 2],
                  color = I("gray95"), name = "95% confidence") %>%
      add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], 
                  ymax = fore$upper[, 1],
                  color = I("gray80"), name = "80% confidence") %>%
      add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), 
                name = "prediction") %>%
      layout(title = "Monthly energy consume predicted",
             xaxis = list(title = "Time"), 
             yaxis = list(title = "Energy (kW/h)"))
  })
  # Infobox1
  output$box1 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Global Energy consumed", paste0(round(sum(data_text$Global_Energy)*
                                    normal_fare$Price._per_kWh[4],2),
                            "€"), 
      icon = icon("euro-sign"), color = "purple", fill = TRUE)
  })
  # Infobox2
  output$box2 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "A/C-Heater Energy consumed", paste0(round(sum(data_text$Conditioning)*
                                              normal_fare$Price._per_kWh[4],2),
                                      "€"), 
      icon = icon("euro-sign"), color = "orange", fill = TRUE)
  })
  # Infobox3
  output$box3 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Laundry Energy consumed", paste0(round(sum(data_text$Laundry)*
                                                   normal_fare$Price._per_kWh[4],2),
                                           "€"), 
      icon = icon("euro-sign"), color = "green", fill = TRUE)
  })
  # Infobox4
  output$box4 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Not submetered Energy consumed", paste0(round(sum(data_text$No_submetered)*
                                                normal_fare$Price._per_kWh[4],2),
                                        "€"), 
      icon = icon("euro-sign"), color = "maroon", fill = TRUE)
  })
  # Infobox5
  output$box5 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Kitvhen Energy consumed", paste0(round(sum(data_text$Kitchen)*
                                                       normal_fare$Price._per_kWh[4],2),
                                               "€"), 
      icon = icon("euro-sign"), color = "navy", fill = TRUE)
  })
}

# RUNNING APP
shinyApp(ui, server)

