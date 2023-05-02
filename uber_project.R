library(tidyverse)
library(rpart)
library(rpart.plot)
library(leaflet)
library(shinyjs)
library(shiny)
library(ggplot2)
library(lubridate)
library(tidytext)
library(textdata)
library(dplyr)
rm(list=ls())
setwd("~/uber data")

ap_14<-read.csv("uber-raw-data-apr14.csv")
aug_14<-read.csv("uber-raw-data-aug14.csv")
jul_14<-read.csv("uber-raw-data-jul14.csv")
jun_14<-read.csv("uber-raw-data-jun14.csv")
may_14<-read.csv("uber-raw-data-may14.csv")
sep_14<-read.csv("uber-raw-data-sep14.csv")


bind_table<-rbind(ap_14,aug_14,jul_14,jun_14,may_14,sep_14)


date_schema<- separate(bind_table, col = Date.Time, into = c("Date", "Time"), sep = " ")

leaf_let<-date_schema

schema_table<- date_schema

schema_table$Time<- format(as.POSIXct(schema_table$Time, format = "%H:%M:%S"), format = "%H:%M")
schema_table$Hour<-format(as.POSIXct(schema_table$Time, format = "%H:%M"), format = "%H")
schema_table$month<-format(as.Date(schema_table$Date, "%m/%d/%Y"),"%b")



#number of trips per
schema<-schema_table%>%
  group_by(month)%>%
  summarise(trips = n())%>%
  pivot_wider(names_from = month, values_from = trips)

hourly<-schema_table%>%
  group_by(Hour)%>%
  summarise(trips = n())%>%
  pivot_longer(cols = trips, names_to = "hourly_tips", values_to = "trip_count")

ggplot(hourly, aes(x = Hour, y = trip_count)) +
  geom_bar(stat = "identity", color = "black", fill = "dark green") +
  labs(title = "Trips on each hour",
       x = "Hour of uber order",
       y = "Trip Count")


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

for_month %>%
  group_by(Day,month) %>%
  summarise(trips = n()) %>%
  arrange(match(Day, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
          match(month, month.name)) %>%
  ggplot(aes(x = month, y = trips, fill = Day)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Trips", fill = "Day of the Week", title = "Number of Uber Trips by Day and Month") +
  theme_bw()

for_month %>%
  group_by(Base, month) %>%
  summarise(trips = n()) %>%
  ggplot(aes(x = Base, y = trips, fill = month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Base", y = "Number of Trips", fill = "Month", title = "Number of Uber Trips by Base and Month") +
  theme_bw()




date_schema$Date <- as.Date(date_schema$Date, format = "%m/%d/%Y")
date_schema$Time <- as.POSIXct(date_schema$Time, format = "%H:%M:%S")

# Extract hour and month from Date and Time columns
date_schema$Hour <- format(date_schema$Time, format = "%H")
date_schema$Minute<-format(date_schema$Time, format = "%M")
date_schema$Time <- as.numeric(date_schema$Hour) + as.numeric(date_schema$Minute)/60
date_schema$Month <- format(date_schema$Date, format = "%b")

# Convert Hour and Month columns to numeric for plotting
date_schema$Hour <- as.numeric(date_schema$Hour)
date_schema$Minute <- as.numeric(date_schema$Minute)
date_schema$Month <- as.factor(date_schema$Month)


less_than_1_hour <- filter(date_schema, Time < 1)


df_trip_count <- less_than_1_hour %>%
  group_by(Month) %>%
  summarize(Trip_Count = n())

ggplot(df_trip_count, aes(x = Month, y = Trip_Count)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(title = "Trips early in the morning by each month Month",
       x = "Month",
       y = "Trip Count")


night_owl<-filter(date_schema, Time > 23)
night_count<-night_owl%>%
  group_by(Month) %>%
  summarize(Trip_Count = n())

colors <- c("blue", "green", "red", "purple", "pink", "yellow")

# Plot a bar chart of trip counts for each month
ggplot(night_count, aes(x = Month, y = Trip_Count, fill = Month)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colors) +
  labs(title = "Trips at midnight by Month",
       x = "Month",
       y = "Trip Count")




# Define the UI
ui <- fluidPage(
  titlePanel("Uber Rides Analysis shown bar charts"),
  #Display the first chart
  plotOutput("chart1"),
  
  textOutput("explanation1"),
  
  #Display the second chart
  plotOutput("chart2"),
  
  textOutput("explanation2"),
  
  # Display the third chart
  plotOutput("chart3"),
  
  textOutput("explanation3"),
  
  # Display the fourth chart
  plotOutput("chart4"),
  
  textOutput("explanation4"),
  
  #Display the fifth chart
  plotOutput("chart5"),
  
  textOutput("explanation5")
  
  
)

# Define the server
server <- function(input, output) {
  #Render the first chart
  output$chart1 <- renderPlot({
    hourly<-schema_table%>%
      group_by(Hour)%>%
      summarise(trips = n())%>%
      pivot_longer(cols = trips, names_to = "hourly_tips", values_to = "trip_count")
    
    ggplot(hourly, aes(x = Hour, y = trip_count)) +
      geom_bar(stat = "identity", color = "black", fill = "dark green") +
      labs(title = "Trips on each hour",
           x = "Hour of uber order",
           y = "Trip Count")
  })
  
  #Render the second chart
  output$chart2 <- renderPlot({
    daily<-for_month%>%
      group_by(Day)%>%
      summarise(trips = n())
    
    ggplot(daily, aes(x = Day, y = trips)) +
      geom_bar(stat = "identity", color = "black", fill = "dark grey") +
      labs(title = "Trips on each day",
           x = "day of uber order",
           y = "Trip Count")
  })
  
  # Render the third chart
  output$chart3 <- renderPlot({
    for_month %>%
      group_by(Day,month) %>%
      summarise(trips = n()) %>%
      arrange(match(Day, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
              match(month, month.name)) %>%
      ggplot(aes(x = month, y = trips, fill = Day)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Month", y = "Number of Trips", fill = "Day of the Week", title = "Number of Uber Trips by Day and Month") +
      theme_bw()
  })
  
  # Render the fourth chart
  output$chart4 <- renderPlot({
    for_month %>%
      group_by(Base, month) %>%
      summarise(trips = n()) %>%
      ggplot(aes(x = Base, y = trips, fill = month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Base", y = "Number of Trips", fill = "Month", title = "Number of Uber Trips by Base and Month") +
      theme_bw()
  })
  
  
  #Render the fifth chart
  output$chart5 <- renderPlot({
    night_owl<-filter(date_schema, Time > 23)
    night_count<-night_owl%>%
      group_by(Month) %>%
      summarize(Trip_Count = n())
    
    colors <- c("blue", "green", "red", "purple", "pink", "yellow")
    
    # Plot a bar chart of trip counts for each month
    ggplot(night_count, aes(x = Month, y = Trip_Count, fill = Month)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = colors) +
      labs(title = "Trips at midnight by Month",
           x = "Month",
           y = "Trip Count")
    
  })
  
  
  output$explanation1 <- renderText({
    "This chart shows the trip count on each hour. '00' represents '12AM', and after that it goes upto '23', which represents '11 PM'.
    As we can see from the chart, the most uber calls was at about '17' which is at '5 pm.' this makes sense because this is the usual hour were people go out of work and call uber. the least number of orders according to the chart is at '02'
    which means '2 AM'."
  })
  
  output$explanation2 <- renderText({
    "from our uber trips data, we also wanted to know which day of the week had the most uber orders. to do this, i split the months into days
    and grouped each days. after that i counted each days travel over the 6 months given. as seen on the chart, Thursday had the most uber calls, while sunday had the least."
  })
  
  output$explanation3 <- renderText({
    "As the data represents, there are a lot of uber trips in each day of the month. And to know this, we counted each trips in a days travel in each month. This chart shows the number of Uber rides that took place by day of the week and month.
    Each bar represents a day of the week, and the bars are grouped by month. As shown in the chart, Tuesday of september is the busiest day while sunday of april is the least busy day"
    
  })
  
  output$explanation4 <- renderText({
    "This chart represents the number of uber rides compared by its base and month.Each bar represents a base, and the bars are grouped by month. As we can understand from the graph, Base 'B02617' conducted the most trips especially on September.
    Base 'B02764' conducted the lowest, which occured on both June and july"
  })
  
  output$explanation5 <- renderText({
    "I filtered out the time to midnight and checked how many uber calls there were at midnight.
    this chart represents which month had the most and least uber calls at midnight. September has the most calls on midnight, and April had the least "
  })
  
  
}

# Run the app
shinyApp(ui, server)


hour_and_day <- for_month %>%
  group_by(Hour, Day) %>%
  summarise(count = n())

ggplot(hour_and_day, aes(x = Hour, y = Day, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Hour of Day", y = "Day of Week", fill = "Number of Rides") +
  theme_minimal()


month_and_day <- for_month %>%
  group_by(month, Day) %>%
  summarise(count = n())

# Create the heatmap with ggplot2
ggplot(month_and_day, aes(x = month, y = Day, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  labs(x = "Month", y = "Day of Week", fill = "Number of Rides") +
  theme_minimal()

for_month$weekly <- as.POSIXct(paste(for_month$Date, for_month$Time))

month_and_week <- for_month %>%
  mutate(month = lubridate::month(weekly, label = TRUE), week = lubridate::week(weekly)) %>%
  group_by(month, week) %>%
  summarise(count = n())


ggplot(month_and_week, aes(x = month, y = week, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Month", y = "Week of Year", fill = "Number of Rides") +
  theme_minimal()

base_and_day <- for_month %>%
  group_by(Base, Day) %>%
  summarise(count = n())

ggplot(base_and_day, aes(x = Base, y = Day, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Uber Base", y = "Day of Week", fill = "Number of Rides") +
  ggtitle("Uber Rides by Base and Day of Week") +
  theme_minimal()




ui <- fluidPage(
  
  # Add title to the app
  titlePanel("Uber Rides Analysis shown by heat map"),
  
  #Display the first chart
  plotOutput("chart1"),
  
  textOutput("explanation1"),
  
  #Display the second chart
  plotOutput("chart2"),
  
  textOutput("explanation2"),
  
  # Display the third chart
  plotOutput("chart3"),
  
  textOutput("explanation3"),
  
  # Display the fourth chart
  plotOutput("chart4"),
  
  textOutput("explanation4")
  
  
)

# Define the server
server <- function(input, output) {
  #Render the first chart
  output$chart1 <- renderPlot({
    ggplot(hour_and_day, aes(x = Hour, y = Day, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = "Hour of Day", y = "Day of Week", fill = "Number of Rides") +
      ggtitle("Uber Rides by Hour and Day of Week") +
      theme_minimal()
  })
  
  #Render the second chart
  output$chart2 <- renderPlot({
    ggplot(month_and_day, aes(x = month, y = Day, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "green") +
      labs(x = "Month", y = "Day of Week", fill = "Number of Rides") +
      ggtitle("Uber Rides by Base and Day of Week") +
      theme_minimal()
  })
  
  # Render the third chart
  output$chart3 <- renderPlot({
    ggplot(month_and_week, aes(x = month, y = week, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(x = "Month", y = "Week of Year", fill = "Number of Rides") +
      ggtitle("month and week of the year") +
      theme_minimal()
  })
  
  # Render the fourth chart
  output$chart4 <- renderPlot({
    ggplot(base_and_day, aes(x = Base, y = Day, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "Uber Base", y = "Day of Week", fill = "Number of Rides") +
      ggtitle("Uber Rides by Base and Day of Week") +
      theme_minimal()
  })
  
  
  output$explanation1 <- renderText({
    "This heat map resembles the different hours of each day of the week where uber rides are frequently called. the darker the color is,
      the more uber rides that are called."
  })
  
  output$explanation2 <- renderText({
    "this heat map shows on which days of the month most rides were called. again, the darker the color, the more rides called,
      we can also see that April, relatively had wednesday as the most days called, august had saturday and sunday,
      July, had wednesday and thurdsay, Jun had thursday, May had thursay, and september had friday, saturday, thursday and tuesday."
  })
  
  output$explanation3 <- renderText({
    "As the data represents, there are a lot of uber trips in the first 20 week of the year in April. next 10 week is dominated
      by may, the next 15 by Jun, then by July, then by August, then the rest was dominated by september."
    
  })
  
  output$explanation4 <- renderText({
    "from the chart above, we can see that base 'B02512' and base 'B02764' had very few rides. and amongst the Base who had a lot of rides, they usually gave rides on wednesdays, thursays, and fridays."
  })
  
  
}    

# Run the app
shinyApp(ui, server)    


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
    info = c("Reyes holdings is one of the biggest comopany in Chicago")
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


#prediction model
modeling<-date_schema
modeling <- cbind(modeling, model.matrix(~Base, data = modeling)[,-1])
set.seed(123)
train <- sample(nrow(modeling), 0.7 * nrow(modeling))
train_data <- modeling[train,]
test_data <- modeling[-train,]
train_data_subset <- train_data[1:30, ]
test_data_subset<-test_data[1:30, ]
dim(train_data_subset)
tree_model <- rpart(Date ~ Lon + Lat + Time + Base, data = train_data_subset, method = "class")
rpart.plot(tree_model, box.palette = "Greens")  
