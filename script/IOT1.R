# Settings -----
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(chron, ggplot2, tidyr, plyr, RMySQL, lubridate, reshape2, 
               quantmod, scales, RColorBrewer, sqldf, dplyr, prophet, 
               compareDF, reshape, rstudioapi, stringi, plotly, padr, 
               DescTools, anytime, forecast, seasonal) #ggfortify not used because corrumpt the script

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

df <- df %>% dplyr::rename(kitchen = Sub_metering_1, laundry = Sub_metering_2, 
                    conditioning = Sub_metering_3)

head(df)
tail(df)

## Combine Date and Time attribute values in a new attribute column ----
df <- cbind(df,paste(df$Date,df$Time), stringsAsFactors = FALSE)
colnames(df)[10] <- "DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]
# df$DateTime <- as.POSIXct(df$DateTime, "%Y/%m/%d %H:%M:%S") # Is it necessary?
df$DateTime <- ymd_hms(df$DateTime)
# attr(df$DateTime, "tzone") <- "Europe" # Is it necessary?

df <- df %>% mutate(Total_Power = Global_active_power + Global_reactive_power)
summary(df$Total_Power) # Because the highest "Total Power" is 11.3 kW/h, the power hiredmust be up to 12 kVA
  # Only 0.65 % of the time the total power needed is higher than 5.5 kVA. By hiring 6kVA, customer will reduce the power bill up to 50€ a year.
summary(df$Global_intensity)
summary(df$Voltage) # Voltage is a bit higher than expected, as the standard voltage in France is 230V

df$year <- year(df$DateTime) # To filter in or out the years

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
newDF$year <- year(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$weekday <- weekdays.POSIXt(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)
newDF$quarter <- quarter(newDF$DateTime)
newDF$Time <- strftime(newDF$DateTime,format="%H:%M:%S")
newDF$Time2 <- as.numeric(hms(newDF$Time))
newDF$yearmonth <- as.yearmon(newDF$DateTime)

## Power Fares----
# Off-peak time is between 02:00 and 07:00; and between 14:00 and 17:00
# Off-peak price per kWh is 0,1230 €
# Peak time is between 17:00 and 02:00; and between 07:00 and 14:00
# Peak Price per kWh is 0,1580 €
normal_fare <- read.csv("dataset/NormalFares.csv")
peak_fare <- read.csv("dataset/PeakFares.csv")

## Creating a new variable for "Peak" consumes ----
x <- as.numeric(hms(c("02:00:00", "07:00:00", "14:00:00", "17:00:00")))

newDF$tariff <- ifelse(newDF$Time2 > x[1] & newDF$Time2 < x[2] | 
                         newDF$Time2 > x[3] & newDF$Time2 < x[4], 
                       "valey", "peak")
rm(x)

# Splitting the data by day ----
Daily_df <- newDF %>% dplyr::filter(year > 2006) %>% 
  dplyr::group_by(Date, Time) %>% 
  dplyr::summarise(x = sum(Global_active_power/60))
Daily_df <- Daily_df %>% dplyr::group_by(Date) %>% 
  dplyr::summarise(Energy = sum(x))
Daily_df$year <- year(Daily_df$Date)
Daily_df$month <- month(Daily_df$Date)
Daily_df$monthf <- factor(Daily_df$month, levels = as.character(1:12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     ordered = TRUE)
Daily_df$week <- week(Daily_df$Date)
Daily_df$yearmonth <- as.yearmon(Daily_df$Date)
Daily_df$yearmonthf <- as.character(Daily_df$yearmonth)
Daily_df$weekday <- as.POSIXlt(Daily_df$Date)$wday
Daily_df$weekdayf <- factor(Daily_df$weekday, levels = rev(0:6), 
                       labels = rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 
                                      "Sat")), ordered = TRUE)
Daily_df$Date <- date(Daily_df$Date)

Daily_df <- ddply(Daily_df,.(yearmonthf),transform,monthweek=1+week-min(week))

ggplot(Daily_df, aes(monthweek, weekdayf, fill = Energy)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + 
  scale_fill_gradient(low="gold", high="red") + 
  ggtitle("Total Power Consume") + xlab("Week of Month") + ylab("") + 
  theme_bw()

 #Converting it to a Time Series
Daily_dfts <- ts(Daily_df$Energy, frequency = 365, start = c(2007,1))

autoplot(Daily_dfts)
 ## Applying time series linear regression to the Time series:

fit_Daily_df <- tslm(Daily_dfts ~ trend + season)
summary(fit_Daily_df)
plot(forecast(fit_Daily_df, h=5))

checkresiduals(fit_Daily_df)
CV(fit_Daily_df) # Horrible!

## Decomposing the Time series:
decomposed_Daily_dfts <- decompose(Daily_dfts)
plot(decomposed_Daily_dfts)
summary(decomposed_Daily_dfts)
decomposed_Daily_dfts$random

x <- stl(Daily_dfts, "periodic")
x$time.series[,1] # Seasonal
x$time.series[,2] # Trend
x$time.series[,3] # Random
y <- (sum(abs(x$time.series[,3])))/nrow(Daily_df) # Absolute Mean Error

rm(x, y)

# Now by month ----
Monthly_df <- Daily_df %>% dplyr::group_by(yearmonth) %>% 
  dplyr::summarise(Energy = sum(Energy))
Monthly_df$year <- year(Monthly_df$yearmonth)
Monthly_df$month <- month(Monthly_df$yearmonth)
Monthly_df$monthf <- factor(Monthly_df$month, levels = as.character(1:12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     ordered = TRUE)

ggplot(Monthly_df, aes(x = yearmonth, y = Energy)) + 
  geom_line(color = "#00AFBB", size = 1) + ylab("Energy (kW/h)") +
  xlab("Year") + ggtitle("Energy consumed per month")

Monthly_df$Year <- as.factor(Monthly_df$year)
ggplot(Monthly_df, aes(x = monthf, y = Energy, colour = Year, group = Year)) +
  geom_line() + ylab("Energy (kW/h)") + xlab("Month") +
  ggtitle("Energy consume per month") + geom_point()

Monthly_df$Date <- date(Monthly_df$yearmonth)

 #Converting it to a Time Series
Monthly_dfts <- ts(Monthly_df$Energy, frequency = 12, start = c(2007,1))

plot.ts(Monthly_dfts)
autoplot(Monthly_dfts)

## Applying time series linear regression to the Time series:

fit_Monthly_df <- tslm(Monthly_dfts ~ trend + season)
summary(fit_Monthly_df)
plot(forecast(fit_Monthly_df, h=5, level=c(80,90)))

checkresiduals(fit_Monthly_df)
CV(fit_Monthly_df) # Better AdjR^2, AIC and BIC, but still pretty bad.
round(accuracy(fit_Monthly_df)) # Seems to have good metrics

## Decomposing the Time series:
decomposed_Monthly_dfts <- decompose(Monthly_dfts)
autoplot(decomposed_Monthly_dfts) + xlab("Year") +
  ggtitle("Decomposition of monthly electrical consume")

Monthly_dfts %>% seas(x11="") %>% autoplot() + 
  ggtitle("X11 decomposition of electrical consume")

fit <- Monthly_dfts %>% seas(x11="")

require(ggplot2)
autoplot(Monthly_dfts, series = "Data") + 
  autolayer(fit$data[,4], series = "Trend") + 
  autolayer(fit$data[,3], series = "Seasonally Adjusted") + xlab("Year") +
  ggtitle("X11 decomposition of electrical consume") +
  scale_colour_manual(values=c("darkgray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend")) 

round(accuracy(ses(window(Monthly_dfts, start=2007), h=10)),2) #Not really good metrix.

autoplot(Monthly_dfts) + autolayer(holt(Monthly_dfts, h=15),
                                   series= "Holt's method", PI = FALSE) +
  autolayer(holt(Monthly_dfts, damped = TRUE, phi = 0.9, h=15),
            series = "Damped Holt's method", PI = FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Energy consumed per month (kW/h)") +
  guides(colour = guide_legend(title = "Forecast")) 

round(accuracy(holt(window(Monthly_dfts, start=2007, h=10))),2) #Worse metrix than the SES.

summary(decomposed_Monthly_dfts)
decomposed_Monthly_dfts$random

x <- stl(Monthly_dfts, "periodic")
x$time.series[,1] # Seasonal
x$time.series[,2] # Trend
x$time.series[,3] # Random
y <- (sum(abs(x$time.series[,3])))/nrow(Monthly_df) # Absolute Mean Error

rm(x, y)

# Forecasting with decomposition ----
fit <- stl(Monthly_dfts, t.window=13, s.window="periodic",
           robust=TRUE)

fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("Monthly power consume (kW/h)") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% forecast(method="naive") %>%
  autoplot() + ylab("Monthly power consume (kW/h)") + xlab("Year")

## Forecast Holt-Winters:
adjusted_Monthly_dfts <- Monthly_dfts - decomposed_Monthly_dfts$seasonal
autoplot(adjusted_Monthly_dfts)
plot(decompose(adjusted_Monthly_dfts))

Monthly_dfts_HW <- hw(adjusted_Monthly_dfts,start = 2007, 
                       damped = TRUE, seasonal = "additive", h=10)

round(accuracy(hw(adjusted_Monthly_dfts)),2) # Seems to have the best performance so far.

plot(Monthly_dfts_HW, ylim = c(575, 950))

 ## Forecast with Prophet:
Prophet_df <- data.frame(ds = Daily_df$Date, y = Daily_df$Energy)
Prophet_df <- add_country_holidays(Prophet_df, "France")

Prophet_fit <- prophet(Prophet_df)
cross_validation(Prophet_fit, horizon = 30, units = "days", initial = 900)
performance_metrics(cross_validation(Prophet_fit, horizon = 30, 
                                     units = "days", initial = 900), 
                    rolling_window = 0.1)

 #Forecast with auto-ARIMA:
auto.arima(Monthly_dfts) %>% forecast(h=10) %>% autoplot(include=80)
round(accuracy(auto.arima(Monthly_dfts)),2) # Pretty close to the HW one.

# Data splitting ----
trainSet <- window(Monthly_dfts, 2007, c(2009,12))
Monthly_dffit1 <- meanf(trainSet,h=12)
Monthly_dffit2 <- rwf(trainSet,h=12)
Monthly_dffit3 <- snaive(trainSet,h=12)
Monthly_dffit4 <- hw(trainSet, h=12)
Monthly_dffit5 <- auto.arima(trainSet) %>% forecast(h=12)
Monthly_dffit6 <- tslm(trainSet ~ trend + season) %>% forecast(h=12)
Monthly_dffit7 <- ets(trainSet) %>% forecast(h=12)

testSet <- window(Monthly_dfts, c(2010,01))
round(accuracy(Monthly_dffit1, testSet),2)
round(accuracy(Monthly_dffit2, testSet),2) # Worst one by far
round(accuracy(Monthly_dffit3, testSet),2)
round(accuracy(Monthly_dffit4, testSet),2)
round(accuracy(Monthly_dffit5, testSet),2) # better at the training test, but worse at the test set
round(accuracy(Monthly_dffit6, testSet),2) # better at the test set, but worse at the test set. It has the lowest RMSE
round(accuracy(Monthly_dffit7, testSet),2) # Best one so far

gglagplot(Monthly_dfts) # The relationship is strongly positive at lag 12, reflecting the strong seasonality in the data.
ggAcf(Monthly_dfts, lag=24)
ggsubseriesplot(Monthly_dfts)
ggseasonplot(Monthly_dfts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Power consumed (kW/h)") +
  ggtitle("Seasonal plot: Monthly power consume")

autoplot(window(Monthly_dfts, start=2009)) +
  autolayer(Monthly_dffit1, series="Mean", PI=FALSE) +
  autolayer(Monthly_dffit2, series="Naïve", PI=FALSE) +
  autolayer(Monthly_dffit3, series="Seasonal naïve", PI=FALSE) +
  autolayer(Monthly_dffit4, series="Holt-Winters", PI=FALSE) +
  autolayer(Monthly_dffit5, series="ARIMA", PI=FALSE) +
  autolayer(Monthly_dffit6, series="Time Series Linear Model", PI=FALSE) +
  autolayer(Monthly_dffit7, series="Exponential Smoothing Model", PI=FALSE) +
  xlab("Year") + ylab("Energy (kW/h)") +
  ggtitle("Forecasts for monthly power consume") +
  guides(colour=guide_legend(title="Forecast"))

fore <- forecast(Monthly_dfts, h = 6, level = c(80, 95))

plot_ly() %>%
  add_lines(x = time(Monthly_dfts), y = Monthly_dfts,
            color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), 
            name = "prediction") %>%
  layout(title = "Monthly energy consume predicted",
         xaxis = list(title = "Time"), 
         yaxis = list(title = "Energy (kW/h)"))

trainSet2 <- window(Daily_dfts, 2007, c(2010,300))
Daily_dffit1 <- meanf(trainSet2,h=65)
Daily_dffit2 <- rwf(trainSet2,h=65)
Daily_dffit3 <- snaive(trainSet2,h=65)
# Daily_dffit4 <- hw(trainSet2, h=65) # Not working due to high frequency
Daily_dffit5 <- auto.arima(trainSet2) %>% forecast(h=65)
Daily_dffit6 <- tslm(trainSet2 ~ trend + season) %>% forecast(h=65)

testSet2 <- window(Daily_dfts, c(2010,300))
round(accuracy(Daily_dffit1, testSet2),2)
round(accuracy(Daily_dffit2, testSet2),2) 
round(accuracy(Daily_dffit3, testSet2),2)
round(accuracy(Daily_dffit5, testSet2),2)
round(accuracy(Daily_dffit6, testSet2),2)

autoplot(window(Daily_dfts, start = c(2009,300))) +
  autolayer(Daily_dffit1, series="Mean", PI=FALSE) +
  autolayer(Daily_dffit2, series="Naïve", PI=FALSE) +
  autolayer(Daily_dffit3, series="Seasonal naïve", PI=FALSE) +
  autolayer(Daily_dffit5, series="ARIMA", PI=FALSE) +
  autolayer(Daily_dffit3, series="Time Series Linear Model", PI=FALSE) +
  xlab("Year") + ylab("Energy (kW/h)") +
  ggtitle("Forecasts for monthly power consume") +
  guides(colour=guide_legend(title="Forecast"))

# MSE----

e <- tsCV(Monthly_dfts, snaive, drift=TRUE, h=1)
e2 <- tsCV(Monthly_dfts, hw, drift=TRUE, h=1)

sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(e2^2, na.rm=TRUE))

sqrt(mean(residuals(snaive(Monthly_dfts, drift=TRUE))^2, na.rm=TRUE))
sqrt(mean(residuals(hw(Monthly_dfts, drift=TRUE))^2, na.rm=TRUE))

# Now by month and tariff ----
df4 <- newDF %>% dplyr::group_by(Date, hour, tariff) %>% 
  dplyr::summarise(sum_total_power = sum(Global_active_power/60))
df4$yearmonth <- as.yearmon(df4$Date)
df4$PeakPower <- ifelse(df4$tariff == "peak", 
                              (df4$sum_total_power), 0)
df4$OffPeakPower <- ifelse(df4$tariff == "valey", 
                                 (df4$sum_total_power), 0)
df4$PeakCost <- df4$PeakPower * peak_fare$Peak_Price_per_kWh[3]
df4$OffPeakCost <- df4$OffPeakPower * 
  peak_fare$Off_Peak_Price_per_kWh[3]
df4$variable_tariff_cost <- df4$PeakCost + df4$OffPeakCost
df4 <- df4 %>% dplyr::group_by(yearmonth) %>% 
  dplyr::summarise(Monthly_fare = sum(variable_tariff_cost) + 
              peak_fare$Subscription_price[3]/12)
head(df4)
df4$year <- year(df4$yearmonth)
df4$month <- month(df4$yearmonth)

ggplot(df4, aes(x = yearmonth, y = Monthly_fare)) + 
  geom_line(color = "#01EC13", size = 1) + ylab("Euros") +
  xlab("") + ggtitle("Peak/Valey tariff monthly fares")

df5 <- newDF %>% dplyr::group_by(yearmonth) %>% 
  dplyr::summarise(sum_total_power = sum(Global_active_power/60))
head(df5)
df5$Monthly_fare <- df5$sum_total_power * normal_fare$Price._per_kWh[4] + 
  (normal_fare$Subscrition_price[4]/12)
df5$year <- year(df5$yearmonth)
df5$month <- month(df5$yearmonth)

ggplot(df5, aes(x = yearmonth, y = Monthly_fare)) + 
  geom_line(color = "#FF5733", size = 1) + ylab("Euros") +
  xlab("") + ggtitle("Normal tariff monthly fares")

df5$sum_total_power <- NULL
df4$Tariff <- "Peak/Valey Tariff"
df5$Tariff <- "Normal Tariff"

comparison <- compare_df(df4, df5, "yearmonth")
comparison$comparison_df
comparison$html_output

Tariff_df <- as.data.frame(rbind(df4,df5))
Tariff_df$year <- NULL
Tariff_df$month <- NULL

ggplot(data = Tariff_df, aes(x = yearmonth, y = Monthly_fare)) + 
  geom_col(color = "blue", fill = "lightblue") + ylab("Monthly bill (€)") +
  facet_wrap(Tariff~., scales = "free") + xlab("Year")

ggplot(Tariff_df, aes(x = yearmonth, y = Monthly_fare, colour = Tariff)) +
  geom_line() + ylab("Monthly bill (€)") + xlab("Year")

#Comparing both tariffs by rating them.
df7 <- as.data.frame(cbind(df4,df5))
df7[ ,c(3, 4, 6, 8, 9, 10)] <- NULL
colnames(df7) <- c("yearmonth", "Monthly_fare", "Tariff", "Normal_Monthly_fare")
df7$ratio <- round((df7$Monthly_fare/df7$Normal_Monthly_fare)*100,2)

df8 <- as.data.frame(cbind(df4,df5))
df8[ ,c(3, 4, 5, 6, 8, 9)] <- NULL
colnames(df8) <- c("yearmonth", "Monthly_fare", "Normal_Monthly_fare", "Tariff")
df8$ratio <- round((df8$Normal_Monthly_fare/df8$Normal_Monthly_fare)*100,2)
df8 <- df8[ ,c(1, 2, 4, 3, 5)]

RatingTf_df <- as.data.frame(rbind(df7,df8))
rm(df4, df5, df7, df8)
RatingTf_df$Monthly_fare <- NULL
RatingTf_df$Normal_Monthly_fare <- NULL
ggplot(RatingTf_df, aes(x = yearmonth, y = ratio, colour = Tariff)) +
  geom_line() + ylab("Normal fare ratio") + xlab("Year")

# Plotting some weeks ----
Weekly_df <- Daily_df %>% 
  dplyr::group_by(weekday, week, Date, year, yearmonthf) %>% 
  dplyr::summarise(Energy = sum(Energy))
Weekly_df$week <- stringi::stri_datetime_fields(Weekly_df$Date)$WeekOfMonth
Weekly_df$weekdayx <- factor(Weekly_df$weekday, labels = c(7, 1, 2, 3, 4, 5, 
                                               6), ordered = TRUE)
Weekly_df$weekdayf <- factor(Weekly_df$weekdayx, levels = (1:7), 
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", 
                                      "Sat", "Sun"), ordered = TRUE)

ggplot(Weekly_df, aes(x = weekdayf, y = Energy, colour = week, group = week)) +
  geom_line() + geom_point() + ylab("Energy (kW/h)") + xlab("Week day") +
  ggtitle("Energy consume per day")

# Plotting by submeters----
Submeter_df <- newDF %>% dplyr::group_by(Date, Time, yearmonth) %>% 
  dplyr::summarise(Kitchen = sum(kitchen/1000), Laundry = sum(laundry/1000), 
            Conditioning = sum(conditioning/1000), 
            Global_Energy = sum(Global_active_power/60))

Submeter_df$No_submetered <- (Submeter_df$Global_Energy - 
                                Submeter_df$Kitchen - Submeter_df$Laundry - 
                                Submeter_df$Conditioning)

Submeter_ts <- Submeter_df %>% dplyr::group_by(Date) %>% 
  dplyr::summarise(Kitchen = round(sum(Kitchen),2),
            Laundry = round(sum(Laundry),2), 
            Conditioning = round(sum(Conditioning),2),
            Global_Energy = round(sum(Global_Energy),0),
            No_submetered = round(sum(No_submetered),2))%>%
  dplyr::select(Global_Energy, Kitchen, Laundry, Conditioning, 
                No_submetered) %>% 
  ts(frequency = 365, start = c(2007,1))

ggplot2::autoplot(Submeter_ts) + ylab("Energy (kW/h)") + xlab("Date") + 
  ggtitle("Daily energy consumed per submeter")

Submeter_df1 <- Submeter_df %>% dplyr::group_by(yearmonth) %>% 
  dplyr::summarise(Kitchen = round(sum(Kitchen),2),
            Laundry = round(sum(Laundry),2), 
            Conditioning = round(sum(Conditioning),2),
            Global_Energy = round(sum(Global_Energy),0),
            No_submetered = round(sum(No_submetered),2))

Submeter_df1$Date <- date(Submeter_df1$yearmonth)

Submeterdf1ts <- Submeter_df1 %>% dplyr::select(Global_Energy, Kitchen, 
                                                Laundry, Conditioning,
                                                No_submetered) %>% 
  ts(frequency = 12, start = c(2007,1))

ggplot2::autoplot(Submeterdf1ts) + ylab("Energy (kW/h)") + xlab("Month") + 
  ggtitle("Monthly energy consumed per submeter")

Day_submeter <- Submeter_df %>% dplyr::group_by(Date) %>% 
  dplyr::summarise(Kitchen = round(sum(Kitchen),2),
            Laundry = round(sum(Laundry),2), 
            Conditioning = round(sum(Conditioning),2),
            Global_Energy = round(sum(Global_Energy),0),
            No_submetered = round(sum(No_submetered),2))%>%
  dplyr::select(Global_Energy, Kitchen, Laundry, Conditioning, 
                No_submetered, Date)

# Plotting for a representative day ----
Rep_day_df <- newDF %>% dplyr::group_by(DateTime, weekday, hour, day) %>% 
  dplyr::summarise(kitchen_energy = sum(kitchen/1000), 
            laundry_energy = sum(laundry/1000), 
            conditioning_energy = sum(conditioning/1000), 
            Global_Energy = sum(Global_active_power/60))

Rep_day_df <- Rep_day_df %>% dplyr::group_by(day, hour) %>% 
  dplyr::summarise(kitchen_energy = round(sum(kitchen_energy),0), 
            laundry_energy = round(sum(laundry_energy),0), 
            conditioning_energy = round(sum(conditioning_energy),0), 
            Global_Energy = round(sum(Global_Energy),0))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Rep_day_df <- Rep_day_df %>% dplyr::group_by(hour) %>% 
  dplyr::summarise(kitchen_energy = getmode(kitchen_energy), 
            laundry_energy = getmode(laundry_energy), 
            conditioning_energy = getmode(conditioning_energy), 
            Global_Energy = getmode(Global_Energy))

Rep_day_df$Energy_no_submetered <- (Rep_day_df$Global_Energy - Rep_day_df$kitchen_energy - 
                               Rep_day_df$laundry_energy - Rep_day_df$conditioning_energy)

Rep_day_df$Hour <- factor(Rep_day_df$hour, levels = as.character(0:23), 
                   labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                              "10", "11", "12", "13", "14", "15", "16", "17",
                              "18", "19", "20", "21", "22", "23"), 
                   ordered = TRUE)

plot_ly(Rep_day_df, x = Rep_day_df$Hour, y = Rep_day_df$kitchen_energy, name = "Kitchen", 
        type = "scatter", mode = "lines") %>% 
  add_trace(y = Rep_day_df$conditioning_energy, name = "Conditioning", 
            mode = "lines") %>%
  add_trace(y = Rep_day_df$laundry_energy, name = "Laundry", mode = "lines") %>%
  add_trace(y = Rep_day_df$Energy_no_submetered, name = "Not Submetered", 
            mode = "lines") %>%
  add_trace(y = Rep_day_df$Global_Energy, name = "Global", mode = "lines") %>%
  layout(title = "Representative day of Energy consumed per submeter",
         xaxis = list(title = "Time"), yaxis = list(title = "Energy (kW/h)"))


