# uber_rides_analysis  
# Introduction  
I will be doing an analysis on uber rides that were called in different time and in different locations in the US within the months April, May, June, July, August, and september. this analysis involves a lot of pivot tables since we filter out different dataframes according to the information we want to get. The analysis of this project is divided into three main parts. the first part contains different bar charts and pivot tables that represnet uber rides by day, hour, month...etc. second part contains different heatmaps. the last part contains a leaflet geospatial map and prediction model.  
# Cleaning and selecting the data  
since the datasets that were given were 6(1 for each month), we needed to bind it together and create a date schema for it so we can analyze it. i also created a new column for month to name it rather than put just the dates in form of number.
```
bind_table<-rbind(ap_14,aug_14,jul_14,jun_14,may_14,sep_14)
date_schema<- separate(bind_table, col = Date.Time, into = c("Date", "Time"), sep = " ")
schema_table<- date_schema
schema_table$Time<- format(as.POSIXct(schema_table$Time, format = "%H:%M:%S"), format = "%H:%M")
schema_table$Hour<-format(as.POSIXct(schema_table$Time, format = "%H:%M"), format = "%H")
schema_table$month<-format(as.Date(schema_table$Date, "%m/%d/%Y"),"%b")
```



