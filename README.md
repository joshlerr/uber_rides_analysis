# uber_rides_analysis  
# Introduction  
![png-clipart-easy-taxi-e-hailing-transport-uber-taxi-text-logo](https://user-images.githubusercontent.com/118494139/234085381-e60f5a53-290d-47c7-9b91-b98bc3968318.png)

I will be doing an analysis on uber rides that were called in different time and in different locations in the US within the months April, May, June, July, August, and september. this analysis involves a lot of pivot tables since we filter out different dataframes according to the information we want to get. The analysis of this project is divided into three main parts. the first part contains different bar charts and pivot tables that represnet uber rides by day, hour, month...etc. second part contains different heatmaps. the last part contains a leaflet geospatial map and prediction model.  
# Cleaning and selecting the data  
 since the datasets that were given were 6(1 for each month), we needed to bind it together and create a date schema for it so we can analyze it. i also created a new column for month to name it rather than put just the dates in form of number.
```r
bind_table<-rbind(ap_14,aug_14,jul_14,jun_14,may_14,sep_14)
date_schema<- separate(bind_table, col = Date.Time, into = c("Date", "Time"), sep = " ")
schema_table<- date_schema
schema_table$Time<- format(as.POSIXct(schema_table$Time, format = "%H:%M:%S"), format = "%H:%M")
schema_table$Hour<-format(as.POSIXct(schema_table$Time, format = "%H:%M"), format = "%H")
schema_table$month<-format(as.Date(schema_table$Date, "%m/%d/%Y"),"%b")
```  
# filtering and creating pivot charts  
1. After i divided the dataset into the format i wanted, it was easy for me to filter and precicely choose with what i wanted to filture out. the first thing i wanted to see was a table and chart of uber rides in each Hour, or simply speaking, the number (count) of uber orders at each hour. 
```r
hourly<-schema_table%>%
  group_by(Hour)%>%
  summarise(trips = n())%>%
  pivot_longer(cols = trips, names_to = "hourly_tips", values_to = "trip_count")
ggplot(hourly, aes(x = Hour, y = trip_count)) +
  geom_bar(stat = "identity", color = "black", fill = "dark green") +
  labs(title = "Trips on each hour",
       x = "Hour of uber order",
       y = "Trip Count")
```  
2. After creating a chart that represents uber rides frequency(count) by Hour, i also wanted to see which days had the most rides within the the six months. to do this, i had to create a new column in my table which had Days. so i used the code below to split my month and use my Days to create another chart.  
```r
for_month<-schema_table
for_month$Date <- mdy(for_month$Date)
for_month$Day <- weekdays(for_month$Date)
daily<-for_month%>%
  group_by(Day)%>%
  summarise(trips = n())

ggplot(daily, aes(x = Day, y = trips)) +
  geom_bar(stat = "identity", color = "black", fill = "dark grey") +
  labs(title = "Trips on each day",
       x = "day of uber order",
       y = "Trip Count")
```
3. Among the other questions asked by the professor was a chart that explains the number of uber trips by day and month. and to do this, i had to group the dataset by Day and by Month,summarise the trips, and fill it with different color.  
```r
for_month %>%
  group_by(Day,month) %>%
  summarise(trips = n()) %>%
  arrange(match(Day, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
          match(month, month.name)) %>%
  ggplot(aes(x = month, y = trips, fill = Day)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Trips", fill = "Day of the Week", title = "Number of Uber Trips by Day and Month") +
  theme_bw()
  ```
4. the table below represents a dataframe that includes uber rides by hour and month. next to that, we start doing the shinyApp to represent all the charts with their explanations 
![Screenshot 2023-04-24 143134](https://user-images.githubusercontent.com/118494139/234100152-df8b3618-2a7d-408c-b0f5-a0737f43a06e.png)




