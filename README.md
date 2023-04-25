# Uber_rides_analysis  
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
4. the table below represents a dataframe that includes uber rides by hour and month. it also shows rides taken each day, their month, hour, and year. next to that, I did the shinyApps that represent the barcharts and a shinyApp that shows the heatmaps which is set in different folders before the readme. but on the read me,lets start looking at the leaflet to show the map and locations points.  
![Screenshot 2023-04-24 143134](https://user-images.githubusercontent.com/118494139/234100152-df8b3618-2a7d-408c-b0f5-a0737f43a06e.png)  
# Geospatial leaflet map  
```r
leaf_let<-date_schema
leaf_let$datetime <- as.POSIXct(paste(leaf_let$Date, leaf_let$Time), format = "%m/%d/%Y %H:%M:%S")

# Define UI
ui <- fluidPage(
  
  # Use shinyjs to reset map
  useShinyjs(),
  extendShinyjs(text = "shinyjs.resetMap = function() { map.setView([40.7128, -74.0060], 13); }", functions = list(
    resetMap = JS("function() { map.setView([40.7128, -74.0060], 13); }")
  )),
  
  
  # Search input and search button
  sidebarPanel(
    textInput("search", "Search Address:"),
    actionButton("go", "Go")
  ),
  
  # Reset map button and measure button
  tags$div(
    id = "buttons",
    actionButton("reset", "Reset Map"),
    actionButton("measure", "Measure Distance")
  ),
  
  # Leaflet map and info box
  mainPanel(
    leafletOutput("map"),
    verbatimTextOutput("info")
  )
)

# Define server
server <- function(input, output, session) {
  
  # Initialize leaflet map with default view
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.0060, lat = 42.7137, zoom = 13)
  })
  
  # Add markers with pop-up info based on data frame
  data <- data.frame(
    name = c("Reyes Holdings", "blackrock"),
    lat = c(42.6892, 42.7484),
    lng = c(-75.0445, -74.9857),
    info = c("The Statue of Liberty is a colossal neoclassical sculpture on Liberty Island in New York Harbor within New York City.", "The Empire State Building is a 102-story Art Deco skyscraper in Midtown Manhattan, New York City.")
  )
  
  # Create reactive values for markers and search results
  markers <- reactiveValues(data = data)
  searchResults <- reactiveValues(data = NULL)
  
  # Add markers to map
  observe({
    leafletProxy("map", data = markers$data) %>%
      clearMarkers() %>%
      addMarkers(lng = ~lng, lat = ~lat, popup = ~name)
  })
  
  # Update markers based on search results
  observe({
    if (!is.null(searchResults$data)) {
      leafletProxy("map", data = searchResults$data) %>%
        clearMarkers() %>%
        addMarkers(lng = ~lng, lat = ~lat, popup = ~name)
    }
  })
}
shinyApp(ui = ui, server = server)
```





