# Settings -----
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load(chron, ggplot2, tidyr, plyr, RMySQL, lubridate, reshape2, 
               quantmod, scales, RColorBrewer, sqldf, dplyr, prophet, 
               compareDF, reshape, rstudioapi, stringi, plotly, padr, 
               DescTools, anytime, forecast, seasonal, data.table,
               prophet, purrr) #ggfortify not used because corrumpt the script

current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# Data set INFORMATION -----
## sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). 
## sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
## sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

# Load data-----
j <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010") # Should it be only from 2007 to 2009?
df <- c()
for (i in 1:length(j)) {
  X <- dbGetQuery(con, 
                  paste("SELECT * FROM ",
                        j[i]))
  df <- rbind(df,X)
}
rm(X, i, j)
df$id <- NULL

df <- df %>% dplyr::rename(Kitchen = Sub_metering_1, Laundry = Sub_metering_2, 
                           Conditioning = Sub_metering_3, 
                           GAP = Global_active_power, 
                           GRP = Global_reactive_power, 
                           Intensity = Global_intensity)

## Combine Date and Time attribute values in a new attribute column ----
df <- cbind(df,paste(df$Date,df$Time), stringsAsFactors = FALSE)
colnames(df)[10] <- "DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]
df$DateTime <- ymd_hms(df$DateTime)

df <- df %>% mutate(Total_Power = GAP + GRP)
summary(df$Total_Power) # Because the highest "Total Power" is 11.3 kW/h, the power hiredmust be up to 12 kVA
# Only 0.65 % of the time the total power needed is higher than 5.5 kVA. By hiring 6kVA, customer will reduce the power bill up to 50€ a year.
summary(df$Intensity)
summary(df$Voltage) # Voltage is a bit higher than expected, as the standard voltage in France is 230V

df$year <- year(df$DateTime)

# check if there are any time gap----
df$gap <- c(NA, with(df, DateTime[-1] - DateTime[-nrow(df)]))

which(df$gap > 1)

x1 <- df[(c((which(df$gap > 3)-1),(which(df$gap > 3)))),1]
x1 <- as.data.frame(x1)

rm(x1)
df$gap <- NULL

# PAD function (package PADR) to "fill the gaps" with NAs----
newDF <- rbind(df %>% filter(year == 2007) %>% 
                 pad(), df %>% filter(year == 2008) %>% 
                 pad(), df %>% filter(year == 2009) %>% 
                 pad(), df %>% filter(year == 2010) %>%
                 pad())

newDF$year <- NULL
# Fill NAs with data ----
# For the ones that are less than three minutes:
for (i in 4:ncol(newDF)){
  newDF[ ,i] <- na.locf(newDF[ ,i], maxgap = 3)
} #We consider that the 3 min gap is the time the meters and submeters need for software updates.

# For all the others
for (i in 4:ncol(newDF)) {
  newDF[which(is.na(newDF[ ,i]) == TRUE), i] <- Mode(newDF[ ,i])
}
rm(i)
# Create attributes from "DateTime" ----

newDF$Date <- date(newDF$DateTime)
newDF$Year <- floor_date(newDF$Date, unit = "year")
newDF$Month <- floor_date(newDF$Date, unit = "month")
newDF$Week <- floor_date(newDF$Date, unit = "week")
# newDF$Hour <- floor_date(newDF$Date, unit = "hour") # Not working
newDF$Time <- NULL

# Creating a list ----
group <- c( "Date", "Week", "Month", "Year")
granularity <- c()
for (i in group) {
  granularity[[i]] <- newDF %>% dplyr::filter(Year > 2006) %>% 
    dplyr::group_by_at(i) %>% 
    dplyr::summarise(GAP = sum(GAP/60), GRP = sum(GRP/60), 
                     Intenity = mean(Intensity), Voltage = mean(Voltage),
                     Kitchen = sum(Kitchen/1000), 
                     Laundry = sum(Laundry/1000), 
                     Conditioning = sum(Conditioning/1000),
                     No_submetered = (sum(GAP/60) - 
                                        (sum(Kitchen) + sum(Conditioning) + 
                                           sum(Laundry))/1000))
}

prophet_df <- granularity

setnames(prophet_df$Date, old = c("Date", "GAP"), new = c("ds", "y"))
setnames(prophet_df$Week, old = c("Week", "GAP"), new = c("ds", "y"))
setnames(prophet_df$Month, old = c("Month", "GAP"), new = c("ds", "y"))
setnames(prophet_df$Year, old = c("Year", "GAP"), new = c("ds", "y"))

rm(df, con, group, i)

# Applying a function to the list: ----
m <- lapply(prophet_df, function(x) prophet(df = x, yearly.seasonality = T,
                                            weekly.seasonality = T,
                                            daily.seasonality = T))

# Extend dataframe into the future
future <- lapply(m, function(x) make_future_dataframe(m = x, periods = 365))
future$Week <- make_future_dataframe(m$Week, periods = 155, freq = 'week')
future$Month <- make_future_dataframe(m$Month, periods = 24, freq = 'month')
future$Year <- make_future_dataframe(m$Year, periods = 2, freq = 'year')

# Generate forecast
forecast <- map2(m, future, predict)

dyplot.prophet(x = m$Date, fcst = forecast$Date)

cv <- cross_validation(model = m$Date, horizon = 30, units = "days")

metrics <- performance_metrics(df = cv, metrics = NULL, rolling_window = 0.1)
summary(metrics)
head(metrics)

dyplot.prophet(x = m$Week, fcst = forecast$Week)

cv2 <- cross_validation(model = m$Week, horizon = 30, units = "weeks")

metrics2 <- performance_metrics(df = cv2, metrics = NULL, rolling_window = 0.1)
summary(metrics2)

dyplot.prophet(x = m$Month, fcst = forecast$Month)

