#running the required packages
library(ggplot2)
library(ggthemes)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(ggmap)
library(maps)
library(mapdata)

#creating a vector of colors for the plot
colors = c("#05a357", "#0576a3", "#d27dfa", "#f26bae", "#f2d56b", "#f05d5d", "#34baad")

#reading the data into their designated variables
apr_data <- read_csv("~/Desktop/Uber-dataset/uber-raw-data-apr14.csv")
may_data <- read_csv("~/Desktop/Uber-dataset/uber-raw-data-may14.csv")
jun_data <- read_csv("~/Desktop/Uber-dataset/uber-raw-data-jun14.csv")
jul_data <- read_csv("~/Desktop/Uber-dataset/uber-raw-data-jul14.csv")
aug_data <- read_csv("~/Desktop/Uber-dataset/uber-raw-data-aug14.csv")
sep_data <- read_csv("~/Desktop/Uber-dataset/uber-raw-data-sep14.csv")

#combining all of this data into a single dataframe called "uber_data_2014"
uber_data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

#Format the Date/Time column
uber_data_2014$`Date/Time` <- as.POSIXct(uber_data_2014$`Date/Time`, format = "%m/%d/%Y %H:%M:%S")
uber_data_2014$Time <- format(as.POSIXct(uber_data_2014$`Date/Time`, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
uber_data_2014$`Date/Time` <- ymd_hms(uber_data_2014$`Date/Time`)

#Creating day, month, year, hour, minute, seconds factors
uber_data_2014$Day <- factor(day(uber_data_2014$`Date/Time`))
uber_data_2014$Month <- factor(month(uber_data_2014$`Date/Time`, label = TRUE))
uber_data_2014$Year <- factor(year(uber_data_2014$`Date/Time`))
uber_data_2014$DayofWeek <- factor(wday(uber_data_2014$`Date/Time`, label = TRUE))
uber_data_2014$Hour <- factor(hour(hms(uber_data_2014$Time)))
uber_data_2014$Minute <- factor(minute(hms(uber_data_2014$Time)))
uber_data_2014$Second <- factor(second(hms(uber_data_2014$Time)))

#View the first few rows of our new dataset
head(uber_data_2014)

#Plotting Trips by the Hour
#Aggregating data based on the hour
hour_data <- uber_data_2014 %>% group_by(Hour) %>% dplyr::summarize(Total = n())
datatable(hour_data)

# Using ggplot to visualize our data
ggplot(hour_data, aes(Hour, Total)) + geom_bar(stat = "identity", fill="#f05d5d") + ggtitle("Trips Made Every Hour") + theme(legend.position = "none") + scale_y_continuous(labels = comma)

#Plotting Data for Trips Made Every Hour by Month
month_hour_data <- uber_data_2014 %>% group_by(Month, Hour) %>% dplyr::summarize(Total = n())
ggplot(month_hour_data, aes(Hour, Total, fill = Month)) + geom_bar(stat = "identity") + ggtitle("Trips Made by Hour and Month") + scale_y_continuous(labels = comma)

#Plotting Data by Trips Made EVeryday of the Month
day_data <- uber_data_2014 %>% group_by(Day) %>% dplyr::summarize(Total = n())
datatable(day_data)
ggplot(day_data, aes(Day, Total)) + geom_bar(stat = "identity", fill = "#0576a3") + ggtitle("Trips Made Everyday") + theme(legend.position = "none") + scale_y_continuous(labels = comma)

#Plotting Trips made Everyday by Month
day_month_data <- uber_data_2014 %>% group_by(Month, Day) %>% dplyr::summarize(Total = n())
ggplot(day_month_data, aes(Day, Total, fill = Month)) + geom_bar(stat = "identity") + ggtitle("Trips by Day and Month") + scale_y_continuous(labels = comma) + scale_fill_manual(values = colors)

#Plotting Data by Number of Trips Taken During Months in a Year
month_data <- uber_data_2014 %>% group_by(Month) %>% dplyr::summarize(Total = n())
datatable(month_data)
ggplot(month_data, aes(Month, Total)) + geom_bar(stat = "identity", fill = "#34baad") + ggtitle("Trips by Month") + theme(legend.position = "none") + scale_y_continuous(labels = comma) + scale_fill_manual(values = colors)

#Plotting Trips Taken During Months by Day of the Week
dayofweek_month_data <- uber_data_2014 %>% group_by(Month, DayofWeek) %>% dplyr::summarize(Total = n())
ggplot(dayofweek_month_data, aes(x = Month, y = Total, fill = DayofWeek)) + geom_bar(position = "dodge", stat = "identity") + scale_fill_manual(values = colors)

#Plotting Number of Trips by Bases The following visualization shows the number of trips taken by passengers from each of the bases. There are five bases.
ggplot(uber_data_2014, aes(Base)) + geom_bar(fill = "#f2d56b") + scale_y_continuous(labels = comma) + ggtitle("Trips by Bases")

#Plotting Number of Trips by Bases and Month
base_month_data <- uber_data_2014 %>% group_by(Month, Base) %>% dplyr::summarize(Total = n())
ggplot(base_month_data, aes(x = Base, y = Total, fill = Month)) + geom_bar(position = "dodge", stat = "identity") + scale_fill_manual(values = colors)

#Plotting Number of Trips by Bases and Day of the week
base_dayofweek_data <- uber_data_2014 %>% group_by(DayofWeek, Base) %>% dplyr::summarize(Total = n())
ggplot(base_dayofweek_data, aes(x = Base, y = Total, fill = DayofWeek)) + geom_bar(position = "dodge", stat = "identity") + scale_fill_manual(values = colors)

#Creating a Heatmap Visualization of day, hour and month - to allow us to simultaenously visualize clusters of samples and features 11. Plotting heatmap by Hour and Day
day_and_hour <- uber_data_2014 %>% group_by(Day, Hour) %>% dplyr::summarize(Total = n())
datatable(day_and_hour)
ggplot(day_and_hour, aes(Day, Hour, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Hour and Day")

#Plotting heatmap by Day and Month
day_and_month <- uber_data_2014 %>% group_by(Day, Month) %>% dplyr::summarize(Total = n())
ggplot(day_and_month, aes(Day, Month, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Month and Day")

#Plotting heatmap by Month and Day of the Week
month_and_dayofweek <- uber_data_2014 %>% group_by(Month, DayofWeek) %>% dplyr::summarize(Total = n())
ggplot(month_and_dayofweek, aes(DayofWeek, Month, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Day of Week and Month")

#Plotting heatmap by Mnth and Bases
month_and_bases <- uber_data_2014 %>% group_by(Month, Base) %>% dplyr::summarize(Total = n())
ggplot(month_and_bases, aes(Base, Month, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Month and Bases")

#Plotting heatmap by Bases and Day of the Week
bases_and_dayofweek <- uber_data_2014 %>% group_by(DayofWeek, Base) %>% dplyr::summarize(Total = n())
ggplot(bases_and_dayofweek, aes(Base, DayofWeek, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Bases and Day of Week")

#Creating a Map Visualization of Rides in New York Based on Latitude and Longitude Visualizing rides in the city by creating a geo-plot that will help us to visualize the rides in 2014 (April - September) and by the bases in the same period.
mean_lon <- mean(uber_data_2014$Lon)
mean_lat <- mean(uber_data_2014$Lat)

# Get map of New York City
NY_map <- get_map(location = c(mean_lon, mean_lat), zoom = 10, scale = "auto", maptype = "roadmap", color = "bw")
NY <- ggmap(NY_map)
NY

# Plot Uber pickup locations on the New York City map generated above based on latitude and longitude
NY_map_plots <- NY + geom_point(data = uber_data_2014, aes(x = Lon, y = Lat), color = "#f05d5d", size = 0.5, na.rm = TRUE)
NY_map_plots

#Creating a Map Visualization of Rides in New York by Bases
# Plot Uber pickup locations on the New York City map generated above by Bases
NY_base_plot <- NY + geom_point(data = uber_data_2014, aes(x = Lon, y = Lat, color = Base), size = 0.5, na.rm = TRUE)
NY_base_plot





