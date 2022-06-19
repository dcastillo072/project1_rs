## Reading data
data <- read.csv("activity.csv")
head(data)

data$day <- weekdays(as.Date(data$date))
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")

data_clean <- data[!is.na(data$steps),]

