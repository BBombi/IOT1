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
      menuItem("Energy costs", tabName = "tariff", 
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
      # Tariff ----
      tabItem(tabName = "tariff",
              selectInput(inputId = "reactivepw",
                          label = "Select reactive power",
                          choices = list("3 kVA", "6 kVA", "9 kVA", "12 kVA",
                                         "15 kVA", "18 kVA", "24 kVA",
                                         "30 kVA", "36 kVA"),
                          selected = "12 kVA"),
              hr(),
                  dateRangeInput(inputId = "dates2", label = "Date range",
                                      start = min(Day_submeter$Date),
                                      end = max(Day_submeter$Date),
                                      min = min(Day_submeter$Date),
                                      max = max(Day_submeter$Date)),
                  fluidRow(infoBoxOutput(width = 6, "box0"),
                           infoBoxOutput(width = 6, "box7"),
                           infoBoxOutput(width = 9, "box6"))),
      # Historic ----
      tabItem(tabName = "historic",
              tabsetPanel(
                tabPanel("Tab 1",
              selectInput(inputId = "select", label = "Select a granuality",
                          choices = list("Month", "Day", "Representative day"),
                          selected = "Month"),
              box(plotlyOutput("plot"), width = 600),
              hr(),
              dateRangeInput("dates", label = h3("Date range"),
                             start = min(Day_submeter$Date),
                             end = max(Day_submeter$Date),
                             min = min(Day_submeter$Date),
                             max = max(Day_submeter$Date)),
              fluidRow(infoBoxOutput(width = 6, "box1"),
              infoBoxOutput(width = 6, "box2"),
              infoBoxOutput(width = 6, "box3"),
              infoBoxOutput(width = 6, "box4"),
              infoBoxOutput(width = 6, "box5"))),
              tabPanel("Tab 2", box(plotlyOutput("plot2"), width = 600)))),
      # Predictions ----
      tabItem(tabName = "predictions", box(plotlyOutput("foreplot"), 
                                           width = 600),
              fluidRow(infoBoxOutput(width = 9, "box8"))),
      # Text ----
      tabItem(tabName = "text", box())
    )
  )
)

# SERVER ----
server <- function(input, output) {
  filteredData <- reactive({
    switch (input$reactivepw,
      "3 kVA" = normal_fare$Subscrition_price[1]/365,
      "6 kVA" = normal_fare$Subscrition_price[2]/365,
      "9 kVA" = normal_fare$Subscrition_price[3]/365,
      "12 kVA" = normal_fare$Subscrition_price[4]/365,
      "15 kVA" = normal_fare$Subscrition_price[5]/365,
      "18 kVA" = normal_fare$Subscrition_price[6]/365,
      "24 kVA" = normal_fare$Subscrition_price[7]/365,
      "30 kVA" = normal_fare$Subscrition_price[8]/365,
      "36 kVA" = normal_fare$Subscrition_price[9]/365
    )
})
  get.granularity <- reactive({
    switch (input$select,
      "Month" = subset(Submeter_df1, Submeter_df1$Date >= input$dates[1] & 
                                     Submeter_df1$Date <= input$dates[2]),
      "Day" = subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                       Day_submeter$Date <= input$dates[2])
    )
  })
  output$plot <- renderPlotly({
    subset <- get.granularity()
    plot_ly(subset, x = subset$Date, 
            y = subset$Kitchen, name = "Kitchen", 
            type = "scatter", mode = "lines") %>% 
      add_trace(y = subset$Conditioning, 
                name = "A/C-Heater room", mode = "lines") %>%
      add_trace(y = subset$Laundry, name = "Laundry", 
                mode = "lines") %>%
      add_trace(y = subset$No_submetered, 
                name = "Not Submetered", mode = "lines") %>%
      add_trace(y = subset$Global_Energy, name = "Global Energy", 
                mode = "lines") %>%
      layout(title = "Energy consumed per submeter",
             xaxis = list(title = "Time"), 
             yaxis = list(title = "Energy (kW/h)"))
  })
  # Representative day plot
  output$plot2 <- renderPlotly({
    plot_ly(Rep_day_df, x = Rep_day_df$Hour, y = Rep_day_df$Kitchen, name = "Kitchen", 
            type = "scatter", mode = "lines") %>% 
      add_trace(y = Rep_day_df$Conditioning, name = "Conditioning", 
                mode = "lines") %>%
      add_trace(y = Rep_day_df$Laundry, name = "Laundry", mode = "lines") %>%
      add_trace(y = Rep_day_df$No_submetered, name = "Not Submetered", 
                mode = "lines") %>%
      add_trace(y = Rep_day_df$Global_Energy, name = "Global", mode = "lines") %>%
      layout(title = "Representative day of Energy consumed per submeter",
             xaxis = list(title = "Time"), yaxis = list(title = "Energy (kW/h)"))
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
  # Infobox0
  output$box0 <- renderInfoBox({
    data_text <- nrow(subset(Day_submeter, Day_submeter$Date >= input$dates2[1] & 
                          Day_submeter$Date <= input$dates2[2]))
    infoBox(
      "Reactive Power fare", paste0(round(data_text*filteredData(),2),
                                           "€"), 
      icon = icon("euro-sign"), color = "light-blue", fill = TRUE)
  })
  # Infobox1
  output$box1 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Global Energy consumed", paste0(round(sum(data_text$Global_Energy)*
                                    normal_fare$Price._per_kWh[4],2),
                            "€"),paste(round(sum(data_text$Global_Energy),2),
                                       "kW/h"), 
      icon = icon("euro-sign"), color = "purple", fill = TRUE)
  })
  # Infobox2
  output$box2 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "A/C-Heater Energy consumed", paste0(round(sum(data_text$Conditioning)*
                                              normal_fare$Price._per_kWh[4],2),
                                      "€"),paste(round(sum(data_text$Conditioning),2),
                                                 "kW/h"), 
      icon = icon("euro-sign"), color = "orange", fill = TRUE)
  })
  # Infobox3
  output$box3 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Laundry Energy consumed", paste0(round(sum(data_text$Laundry)*
                                                   normal_fare$Price._per_kWh[4],2),
                                           "€"),paste(round(sum(data_text$Laundry),2),
                                                      "kW/h"), 
      icon = icon("euro-sign"), color = "green", fill = TRUE)
  })
  # Infobox4
  output$box4 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates[1] & 
                          Day_submeter$Date <= input$dates[2])
    infoBox(
      "Not submetered Energy consumed", paste0(round(sum(data_text$No_submetered)*
                                                normal_fare$Price._per_kWh[4],2),
                                        "€"),paste(round(sum(data_text$No_submetered),2),
                                                   "kW/h"), 
      icon = icon("euro-sign"), color = "maroon", fill = TRUE)
  })
  # Infobox5
  output$box5 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates2[1] & 
                          Day_submeter$Date <= input$dates2[2])
    infoBox(
      "Kitchen Energy consumed", paste0(round(sum(data_text$Kitchen)*
                                                       normal_fare$Price._per_kWh[4],2),
                                               "€"),paste(round(sum(data_text$Kitchen),2),
                                                          "kW/h"), 
      icon = icon("euro-sign"), color = "navy", fill = TRUE)
  })
  # Infobox6
  output$box6 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates2[1] & 
                          Day_submeter$Date <= input$dates2[2])
    data_text2 <- nrow(subset(Day_submeter, Day_submeter$Date >= input$dates2[1] & 
                               Day_submeter$Date <= input$dates2[2]))
    infoBox(
      "Total Power bill", paste0(round(data_text2*filteredData()+
                                            (sum(data_text$Global_Energy)*
                                               normal_fare$Price._per_kWh[4]),2),
                                    "€"), "Due to the picks of power consumtion, 
      the actual Power subscrited is 12 kVA",
      icon = icon("euro-sign"), color = "black", fill = TRUE)
  })
  # Infobox7
  output$box7 <- renderInfoBox({
    data_text <- subset(Day_submeter, Day_submeter$Date >= input$dates2[1] & 
                          Day_submeter$Date <= input$dates2[2])
    infoBox(
      "Global Energy consumed", paste0(round(sum(data_text$Global_Energy)*
                                               normal_fare$Price._per_kWh[4],2),
                                       "€"), 
      icon = icon("euro-sign"), color = "purple", fill = TRUE)
  })
  # Forecast table
  output$forecasttable <- renderTable({
    foretable <- round(as.data.frame(fore) * normal_fare$Price._per_kWh[4],2)
    
  })
  # Infobox8
  output$box8 <- renderInfoBox({
    infoBox(
      "Global Energy consumed", tableOutput("forecasttable"), 
      icon = icon("euro-sign"), color = "purple", fill = TRUE)
  })
}

# RUNNING APP ----
shinyApp(ui, server)
