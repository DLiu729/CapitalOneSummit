# David Liu - Capital One Summit Web App
#rm(list=ls())
library(shiny)
library(rsconnect)
library(rgdal)
library(leaflet)
library(reshape)
library(eeptools)
library(plyr)
library(maps)
library(sp)
library(rgeos)
library(stringr)
library(mapview)
library(dplyr)
library(lattice)
library(gmapsdistance)
library(geosphere)
library(ggplot2)
library(reshape2)
library(shinythemes)

tripData <- read.csv(file = "los-angeles-metro-bike-share-trip-data/metro-bike-share-trip-data.csv", header = TRUE)
## Start/stop stations are most popular
startingStation <- tripData[, c('Starting.Station.ID'), drop=FALSE]
endingStation <- tripData[, c('Ending.Station.ID'), drop=FALSE]
startingStationpop = count(startingStation, Starting.Station.ID)
endingStationpop = count(endingStation, Ending.Station.ID)
startingStationpop$n = as.numeric(startingStationpop$n)
endingStationpop$n = as.numeric(endingStationpop$n)
startingStationpop <- startingStationpop[order(-startingStationpop$n),]
startingStationpop$Starting.Station.ID <- factor(startingStationpop$Starting.Station.ID)
ggplot(data = startingStationpop, aes(x = Starting.Station.ID, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Starting Station ID") + ylab("Frequency") + ggtitle("Number of Riders vs. Starting Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
endingStationpop <- endingStationpop[order(-endingStationpop$n),]
endingStationpop$Ending.Station.ID <- factor(endingStationpop$Ending.Station.ID)
ggplot(data = endingStationpop, aes(x = Ending.Station.ID, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Ending Station ID") + ylab("Frequency") + ggtitle("Number of Riders vs. Ending Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
startingStationpop10 <- startingStationpop[c(1:10),]
endingStationpop10 <- endingStationpop[c(1:10),]
names(startingStationpop10) <- c("Starting Station ID", "Frequency")
names(endingStationpop10) <- c("Ending Station ID", "Frequency")
# Top 3 Start Stations: 3069, 3030, 3005
# Top 3 Stop Stations: 3005, 3031, 3014

## Average distance traveled (12.5 kmph is the average casual biker speed)
oneWayTrip <- tripData[tripData$Trip.Route.Category == "One Way",]
roundTrip <- tripData[tripData$Trip.Route.Category == "Round Trip",]
roundTripDistance <- roundTrip[, c('Duration'), drop=FALSE]
# Meters
roundTripDistance$Duration <- roundTripDistance$Duration/3600*12.5*1000
oneWayTrip$StartLatLong <- paste(oneWayTrip$Starting.Station.Latitude, oneWayTrip$Starting.Station.Longitude, sep="+")
oneWayTrip$EndLatLong <- paste(oneWayTrip$Ending.Station.Latitude, oneWayTrip$Ending.Station.Longitude, sep="+")
oneWayTripSample <- oneWayTrip[c(1:1000), ]
# Code to obtain distances between two points based on google maps bicycling
# set.api.key("AIzaSyC8pYCshk2BMlbIx9UWATPhK-PBkG1IbiE")
# distance <- data.frame(matrix(ncol = 1, nrow = nrow(oneWayTripSample)))
# for(i in 1:nrow(oneWayTripSample))
# {
#   distance[i, 1] <- gmapsdistance(oneWayTripSample[i, 17], oneWayTripSample[i, 18], mode = "bicycling")$Distance
# }
startingStationSampleLatLong <- oneWayTripSample[, c('Starting.Station.Longitude', 'Starting.Station.Latitude')]
endingStationSampleLatLong <- oneWayTripSample[, c('Ending.Station.Longitude', 'Ending.Station.Latitude')]
#distanceHaversine <- data.frame(matrix(ncol = 1, nrow = nrow(oneWayTripSample)))
distanceSampleHaversine <- data.frame(distHaversine(startingStationSampleLatLong, endingStationSampleLatLong))
#distancePercentDiff <- (distance - distanceSampleHaversine)/distanceSampleHaversine * 100
#meandistancePercentDiff <- mean(distancePercentDiff$matrix.ncol...1..nrow...nrow.oneWayTripSample..)
# From sample comparing Google Maps bicycling distance vs. haversine distance, on average haversine underestimates by 36.9%
startingStationLatLong <- oneWayTrip[, c('Starting.Station.Longitude', 'Starting.Station.Latitude')]
endingStationLatLong <- oneWayTrip[, c('Ending.Station.Longitude', 'Ending.Station.Latitude')]
distanceHaversine <- data.frame(distHaversine(startingStationLatLong, endingStationLatLong))
# Meters
distanceAdjusted <- distanceHaversine * 1.369
colnames(distanceAdjusted)[1] = "Duration"
distanceAll <- rbind(distanceAdjusted, roundTripDistance)
avgdistance <- mean(distanceAll$Duration, na.rm = TRUE)
# Avg Distance = 7814.64 m

## Riders regular part of commute
RegularTrip <- tripData[tripData$Passholder.Type == "Monthly Pass" | tripData$Passholder.Type == "Flex Pass" | tripData$Passholder.Type == "Staff Annual",]
PassholderTypeDistribution = count(tripData, Passholder.Type)
ggplot(data = PassholderTypeDistribution, aes(x = Passholder.Type, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Passholder Type") + ylab("Frequency") + ggtitle("Number of Riders vs. Passholder Type")
PassholderTypeDistributionNew <- PassholderTypeDistribution
names(PassholderTypeDistributionNew) = c("Passholder Type", "Frequency")
# 91203 out of 132427 used a monthly or flex pass --> 68.9%

## Data Visuals or Trends possibilities
# Average Duration vs. Hour of Day
TimeVsStart <- tripData[, c('Duration', 'Start.Time', 'Passholder.Type', 'Trip.Route.Category', 'Starting.Station.ID', 'Ending.Station.ID')]
timeResults <- data.frame(str_split_fixed(as.character(TimeVsStart$Start.Time), 'T', 2))
TimeVsStart <- cbind(TimeVsStart, timeResults)
timeHourMinSecSplit <- data.frame(str_split_fixed(as.character(TimeVsStart$X2), ':', 3))
colnames(timeHourMinSecSplit)[1] = "Hour"
colnames(timeHourMinSecSplit)[2] = "Minute"
colnames(timeHourMinSecSplit)[3] = "Second"
TimeVsStart <- cbind(TimeVsStart, timeHourMinSecSplit)
durationMeanHour <- aggregate(Duration~Hour, data = TimeVsStart, mean)
plot(durationMeanHour$Hour, durationMeanHour$Duration, main = "Average Duration vs. Hour of Day", xlab = "Hour of Day", ylab = "Average Duration (sec)")

# Average Duration vs. Passholder Type
TypeVsDuration <- tripData[, c('Passholder.Type', 'Duration')]
durationMeanType <- aggregate(Duration~Passholder.Type, data = TypeVsDuration, mean)
plot(durationMeanType$Passholder.Type, durationMeanType$Duration, main = "Average Duration vs. Passholder Type", xlab = "Passholder Type", ylab = "Average Duration (sec)")

# Average Duration vs. Month 
timeYearMonthDaySplit <- data.frame(str_split_fixed(as.character(TimeVsStart$X1), '-', 3))
colnames(timeYearMonthDaySplit)[1] = "Year"
colnames(timeYearMonthDaySplit)[2] = "Month"
colnames(timeYearMonthDaySplit)[3] = "Day"
TimeVsStart <- cbind(TimeVsStart, timeYearMonthDaySplit)
durationMeanMonth <- aggregate(Duration~Month, data = TimeVsStart, mean)
plot(durationMeanMonth$Month, durationMeanMonth$Duration, main = "Average Duration vs. Month", xlab = "Month", ylab = "Average Duration (sec)")

# Passholder Type vs. Month
TypeVsMonth <- TimeVsStart[, c('Passholder.Type', 'Month')]
TypeVsMonthCounts <- ddply(TypeVsMonth, .(TypeVsMonth$Passholder.Type, TypeVsMonth$Month), nrow)
names(TypeVsMonthCounts) <- c("Passholder.Type", "Month", "Freq")
FlexTypeVsMonths <- TypeVsMonthCounts[TypeVsMonthCounts$Passholder.Type == 'Flex Pass',]
MonthlyTypeVsMonths <- TypeVsMonthCounts[TypeVsMonthCounts$Passholder.Type == 'Monthly Pass',]
StaffTypeVsMonths <- TypeVsMonthCounts[TypeVsMonthCounts$Passholder.Type == 'Staff Annual',]
WalkTypeVsMonths <- TypeVsMonthCounts[TypeVsMonthCounts$Passholder.Type == 'Walk-up',]
plot(FlexTypeVsMonths$Month, FlexTypeVsMonths$Freq, main = "Flex Pass Usage By Month", xlab = "Month", ylab = "Frequency")
plot(MonthlyTypeVsMonths$Month, MonthlyTypeVsMonths$Freq, main = "Monthly Pass Usage By Month", xlab = "Month", ylab = "Frequency")
plot(StaffTypeVsMonths$Month, StaffTypeVsMonths$Freq, main = "Staff Pass Usage By Month", xlab = "Month", ylab = "Frequency")
plot(WalkTypeVsMonths$Month, WalkTypeVsMonths$Freq, main = "Walk-Up By Month", xlab = "Month", ylab = "Frequency")
ggplot(data = TypeVsMonthCounts, aes(x = Month, y = Freq, fill = Passholder.Type)) + geom_bar(stat="identity", position = "dodge") + xlab("Month") + ylab("Frequency") + ggtitle("Frequency based on Month and Passholder Type") + labs(fill = "Passholder Type")
TypeVsMonthTable <- TypeVsMonthCounts
names(TypeVsMonthTable) <- c("Passholder Type", "Month", "Frequency")

# Number of Riders by Month
RidersByMonth = count(TimeVsStart, Month)
plot(RidersByMonth$Month, RidersByMonth$n, main = "Riders By Month", xlab = "Month", ylab = "Frequency")

# Trip Route Category by Month
RoutecatVsMonth <- TimeVsStart[, c('Trip.Route.Category', 'Month')]
RoutecatVsMonthCounts <- ddply(RoutecatVsMonth, .(RoutecatVsMonth$Trip.Route.Category, RoutecatVsMonth$Month), nrow)
names(RoutecatVsMonthCounts) <- c("Trip.Route.Category", "Month", "Freq")
OneWayVsMonthCounts <- RoutecatVsMonthCounts[RoutecatVsMonthCounts$Trip.Route.Category == "One Way",]
RoundTripVsMonthCounts <- RoutecatVsMonthCounts[RoutecatVsMonthCounts$Trip.Route.Category == "Round Trip",]
plot(OneWayVsMonthCounts$Month, OneWayVsMonthCounts$Freq, main = "One Way Trips By Month", xlab = "Month", ylab = "Frequency")
plot(RoundTripVsMonthCounts$Month, RoundTripVsMonthCounts$Freq, main = "Round Trips By Month", xlab = "Month", ylab = "Frequency")
ggplot(data = RoutecatVsMonthCounts, aes(x = Month, y = Freq, fill = Trip.Route.Category)) + geom_bar(stat="identity", position = "dodge") + xlab("Month") + ylab("Frequency") + ggtitle("Frequency based on Month and Trip Route Category") + labs(fill = "Trip Route Category")
RoutecatVsMonthTable <- RoutecatVsMonthCounts
names(RoutecatVsMonthTable) <- c("Trip Route Category", "Month", "Frequency")

# Trip Route Category-Passholder type combinations
CategoryVsType <- tripData[, c('Passholder.Type', 'Trip.Route.Category')]
CategoryVsTypeCounts <- ddply(CategoryVsType, .(CategoryVsType$Passholder.Type, CategoryVsType$Trip.Route.Category), nrow)
names(CategoryVsTypeCounts) <- c("Passholder.Type", "Trip.Route.Category", "Freq")
ggplot(data = CategoryVsTypeCounts, aes(x = Passholder.Type, y = Freq, fill = Trip.Route.Category)) + geom_bar(stat="identity", position = "dodge") + xlab("Passholder Type") + ylab("Frequency") + ggtitle("Frequency based on Passholder Type and Trip Route Category") + labs(fill = "Trip Route Category")
CategoryVsTypeTable <- CategoryVsTypeCounts
names(CategoryVsTypeTable) <- c('Passholder Type', 'Trip Route Category', 'Frequency')

# Net Change of Bikes in a Day
OneWayByDay <- TimeVsStart[TimeVsStart$Trip.Route.Category == 'One Way',]
StartingByDay <- OneWayByDay[, c('X1', 'Starting.Station.ID')]
EndingByDay <- OneWayByDay[, c('X1', 'Ending.Station.ID')]
StartingByDayCounts <- ddply(StartingByDay, .(StartingByDay$X1, StartingByDay$Starting.Station.ID), nrow)
EndingByDayCounts <- ddply(EndingByDay, .(EndingByDay$X1, EndingByDay$Ending.Station.ID), nrow)
names(StartingByDayCounts) <- c("Day", "Station_ID", "StartingFreq")
names(EndingByDayCounts) <- c("Day", "Station_ID", "EndingFreq")
NetChangeDayCounts <- merge(StartingByDayCounts, EndingByDayCounts, by = c('Day', 'Station_ID'))
NetChangeDayCounts$counter <- rep(1, nrow(NetChangeDayCounts))
NetChangeDayCounts$NetChange <- NetChangeDayCounts$EndingFreq - NetChangeDayCounts$StartingFreq
NetChangeDayCounts <- NetChangeDayCounts[, c('Station_ID', 'counter', 'NetChange')]
NetChangeDayCounts <- aggregate(.~Station_ID, data = NetChangeDayCounts, sum)
NetChangeDayCounts$NetChangePerDay <- NetChangeDayCounts$NetChange/NetChangeDayCounts$counter
NetChangeDayCounts$Station_ID <- factor(NetChangeDayCounts$Station_ID)
NetChangeDayTable <- NetChangeDayCounts[, c('Station_ID', 'NetChangePerDay')]
NetChangeDayTable <- NetChangeDayTable[order(-abs(NetChangeDayTable$NetChangePerDay)),]
NetChangeDayTable <- NetChangeDayTable[c(1:20),]
names(NetChangeDayTable) <- c('Station ID', 'Net Change per day')
ggplot(data = NetChangeDayCounts, aes(x = Station_ID, y = NetChangePerDay)) + geom_bar(stat="identity", position = "dodge") + xlab("Station ID") + ylab("Average Net Change of Bikes Per Day") + ggtitle("Average Net Change of Bikes Per Day Vs. Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ui <- fluidPage(theme = shinytheme("united"),
  navbarPage("Los Angeles Bike Sharing Data Analysis",
             tabPanel("Summary",
                      p(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "SummaryInput", 
                            label = "Topics",
                            choices = c("Analysis Goals", "Summary of Answers to Questions", "Summary of Answers to Bonuses"))),
                        mainPanel(
                          h3(textOutput("SummaryOutput")),
                          textOutput("Question1Output"),
                          textOutput("Question2Output"),
                          textOutput("Question3Output"),
                          textOutput("Question4Output")
                        )
                      )
             ),
             tabPanel("Data Visualizations",
                      p(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "DataVisualizationsInput",
                            label = "Graphs",
                            choices = c("Number of Riders vs. Starting Station ID", "Number of Riders vs. Ending Station ID", "Number of Riders vs. Passholder Type","Average Duration vs. Hour of Day", 
                                        "Average Duration vs. Passholder Type", "Average Duration vs. Month", "Passholder Type vs. Month",
                                        "Number of Riders vs. Month", "Trip Route Category vs. Month", "Trip Route Category-Passholder Type Combinations", 
                                        "Average Net Change of Bikes Per Day"))),
                        mainPanel(
                          plotOutput("DataVisualizationsOutput")
                        )
                      )
             ),
             tabPanel("Answers and Justification",
                      p(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "AnswersInput",
                            label = "Questions",
                            choices = c("Most popular start/stop stations", "Average Distance traveled", "Regular commute bike sharers",
                                        "Ridership changing with the seasons", "Net Change of bikes over the day", "Trip Route Category-Passholder type combinations"))),
                        mainPanel(
                          textOutput("AnswersOutput"),
                          tableOutput("AnswersTableOutput"),
                          tableOutput("AnswersTable2Output")
                        )
                      )
             )
  )
)
server <- function(input, output) {
  output$SummaryOutput <- renderText({
    paste("I sought out to analyze several relationships to make bike sharing in Los Angeles more efficient and possibly more profitable.
          My first goal was to highlight what was related to high duration bike rides. This could possibly
          help a company or organization target high profit areas or times if prices were determined by an hourly rate. 
          Another goal was to look at general factors as highlighted in the deliverables. This includes the most popular start and stop stations,
          average distance traveled, and the amount of regular bike sharing commuters. A third goal followed along the lines of the first bonus feature.
          I looked at the relationships between various factors and the month of the bike ride. This could help identify patterns
          across a season or multiple months. The final goal was to help identify parts of improvement related to the second and third features.
          By observing the net change of bikes and breakdown of Trip Route Category-Passholder type combinations, bikes can be more efficiently placed and 
          various passes can be marketed correctly to reflect the customer base.")
  })
  output$DataVisualizationsOutput <- renderPlot({
    ggplot(data = startingStationpop, aes(x = Starting.Station.ID, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Starting Station ID") + ylab("Frequency") + ggtitle("Number of Riders vs. Starting Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  output$AnswersOutput <- renderText({
    paste("The top 10 most starting and ending stations are listed below with the number of bikes
          starting from or ending at the specific station during the data set period. Based on both tables, we can see some common
          stations that can be considered general hubs of bicycling traffic. 9 out of the 10 stations appear on both lists. The only ones that
          do not appear on both are 3067, which appears as a starting station, and 3063, which appears as a stopping station.")
  })
  output$AnswersTableOutput <- renderTable(startingStationpop10)
  output$AnswersTable2Output <- renderTable(endingStationpop10)
  observeEvent(
    input$SummaryInput
    ,{
      if(input$SummaryInput == "Analysis Goals")
      {
        output$SummaryOutput <- renderText({
          paste("I sought out to analyze several relationships to make bike sharing in Los Angeles more efficient and possibly more profitable.
                My first goal was to highlight what was related to high duration bike rides. This could possibly
                help a company or organization target high profit areas or times if prices were determined by an hourly rate. 
                Another goal was to look at general factors as highlighted in the deliverables. This includes the most popular start and stop stations,
                average distance traveled, and the amount of regular bike sharing commuters. A third goal followed along the lines of the first bonus feature.
                I looked at the relationships between various factors and the month of the bike ride. This could help identify patterns
                across a season or multiple months. The final goal was to help identify parts of improvement related to the second and third features.
                By observing the net change of bikes and breakdown of Trip Route Category-Passholder type combinations, bikes can be more efficiently placed and 
                various passes can be marketed correctly to reflect the customer base.")
        })
        output$Question1Output <- renderText({
        })
        output$Question2Output <- renderText({
        })
        output$Question3Output <- renderText({
        })
        output$Question4Output <- renderText({
        })
        }
      if(input$SummaryInput == "Summary of Answers to Questions")
      {
        output$SummaryOutput <- renderText({
        })
        output$Question1Output <- renderText({
          paste("1. The relationships I found most appealing were those that had to deal with average duration. The ones I ended up choosing were
                Average Duration vs. Hour of Day, Average Duration vs. Passholder Type, and Average Duration vs. Month. Specifically, I was fascinated
                by the high average duration doing the non-rush hours of the day and night. This indicates that people might need to get to work, school, 
                or other obligations fast, and therefore spend shorter times on the bike compared to those who use the bikes late at night or early in the morning.
                This led me to the second point, which is that those that used monthly passes had the lowest average duration. Monthly pass, isolated for 
                any factor, was by far the most prevelant Passholder type. Therefore, those with monthly passes having the lowest average times leads to double down
                on my belief that those with monthly passes that utilize the bike share system are the ones most likely to use them for non-recreational purposes 
                (i.e. work, school). I was interested in average duration vs. month because of the strangeness of the results. Average duration was highest during
                the winter months, when people in general should be averse to biking. It's something I think the company should take note in case there are
                explained factors behind this. Data Visuals all located in the Data Visualizations tab")
        })
        output$Question2Output <- renderText({
          paste("2. Start: 3069, 3030, 3005, Stop: 3005, 3031, 3014")
        })
        output$Question3Output <- renderText({
          paste("3. Average Distance Traveled: 7814.64 meters")
        })
        output$Question4Output <- renderText({
          paste("4. 91203 out of 132427 used a monthly, flex, or staff annual pass = 68.9%")
        })
        }
      if(input$SummaryInput == "Summary of Answers to Bonuses")
      {
        output$SummaryOutput <- renderText({
        })
        output$Question1Output <- renderText({
          paste("1. Ridership changed with seasons significantly, specifically that there are more riders in the summer months.
                In terms of types of passes used, there is an increase across the board, with monthly pass and walk-up rider amounts
                experiencing the biggest upward swing during summer months. Flex pass members have a small upward gain during summer months.
                Average trip duration by month was opposite of what I expected, as it was greater during the winter months compared to the summer and fall months.
                Related plots can be found in the Data Visualizations tab. More in-depth explanation can be found in the answers and justifications tab.")
        })
        output$Question2Output <- renderText({
          paste("2. There is a net change of bikes over the course of a day across numerous stations. Based on my Net Change graph in Data Visualizations,
                which takes the average net change of bikes (end - starting) per day, several bikes need to be taken from stations such as 3000, 3005, 3042, and 3021, and placed 
                at stations such as 3068, 3024, 3030, and 3055. More in-depth explanation can be found in the answers and justifications tab.")
        })
        output$Question3Output <- renderText({
          paste("3. The most popular Trip Route Category-Passholder combination is Monthly Pass, One Way with Walk-Up, One Way coming in second. 
                The least popular were the combinations involving the staff annual pass and the Flex Pass, Round Trip combination.
                The visual breakdown of Trip Route Category-Passholder type combinations can be found in the Data Visualizations tab. 
                More in-depth explanation can be found in the answers and justifications tab.")
        })
        output$Question4Output <- renderText({
        })
        }
        })
  observeEvent(
    input$DataVisualizationsInput
    ,{
      if(input$DataVisualizationsInput == "Number of Riders vs. Starting Station ID")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = startingStationpop, aes(x = Starting.Station.ID, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Starting Station ID") + ylab("Frequency") + ggtitle("Number of Riders vs. Starting Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
      }
      if(input$DataVisualizationsInput == "Number of Riders vs. Ending Station ID")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = endingStationpop, aes(x = Ending.Station.ID, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Ending Station ID") + ylab("Frequency") + ggtitle("Number of Riders vs. Ending Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
      }
      if(input$DataVisualizationsInput == "Number of Riders vs. Passholder Type")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = PassholderTypeDistribution, aes(x = Passholder.Type, y = n)) + geom_bar(stat="identity", position = "dodge") + xlab("Passholder Type") + ylab("Frequency") + ggtitle("Number of Riders vs. Passholder Type")
        })
      }
      if(input$DataVisualizationsInput == "Average Duration vs. Hour of Day")
      {
        output$DataVisualizationsOutput <- renderPlot({
          plot(durationMeanHour$Hour, durationMeanHour$Duration, main = "Average Duration vs. Hour of Day", xlab = "Hour of Day", ylab = "Average Duration (sec)")
        })
      }
      if(input$DataVisualizationsInput == "Average Duration vs. Passholder Type")
      {
        output$DataVisualizationsOutput <- renderPlot({
          plot(durationMeanType$Passholder.Type, durationMeanType$Duration, main = "Average Duration vs. Passholder Type", xlab = "Passholder Type", ylab = "Average Duration (sec)")
        })
      }
      if(input$DataVisualizationsInput == "Average Duration vs. Month")
      {
        output$DataVisualizationsOutput <- renderPlot({
          plot(durationMeanMonth$Month, durationMeanMonth$Duration, main = "Average Duration vs. Month", xlab = "Month", ylab = "Average Duration (sec)")
        })
      }
      if(input$DataVisualizationsInput == "Passholder Type vs. Month")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = TypeVsMonthCounts, aes(x = Month, y = Freq, fill = Passholder.Type)) + geom_bar(stat="identity", position = "dodge") + xlab("Month") + ylab("Frequency") + ggtitle("Frequency based on Month and Passholder Type") + labs(fill = "Passholder Type")
        })
      }
      if(input$DataVisualizationsInput == "Number of Riders vs. Month")
      {
        output$DataVisualizationsOutput <- renderPlot({
          plot(RidersByMonth$Month, RidersByMonth$n, main = "Riders By Month", xlab = "Month", ylab = "Frequency")
        })
      }
      if(input$DataVisualizationsInput == "Trip Route Category vs. Month")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = RoutecatVsMonthCounts, aes(x = Month, y = Freq, fill = Trip.Route.Category)) + geom_bar(stat="identity", position = "dodge") + xlab("Month") + ylab("Frequency") + ggtitle("Frequency based on Month and Trip Route Category") + labs(fill = "Trip Route Category")
        })
      }
      if(input$DataVisualizationsInput == "Trip Route Category-Passholder Type Combinations")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = CategoryVsTypeCounts, aes(x = Passholder.Type, y = Freq, fill = Trip.Route.Category)) + geom_bar(stat="identity", position = "dodge") + xlab("Passholder Type") + ylab("Frequency") + ggtitle("Frequency based on Passholder Type and Trip Route Category") + labs(fill = "Trip Route Category")
        })
      }
      if(input$DataVisualizationsInput == "Average Net Change of Bikes Per Day")
      {
        output$DataVisualizationsOutput <- renderPlot({
          ggplot(data = NetChangeDayCounts, aes(x = Station_ID, y = NetChangePerDay)) + geom_bar(stat="identity", position = "dodge") + xlab("Station ID") + ylab("Average Net Change of Bikes Per Day") + ggtitle("Average Net Change of Bikes Per Day Vs. Station ID") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
      }
    })
  observeEvent(
    input$AnswersInput
    ,{
      if(input$AnswersInput == "Most popular start/stop stations")
      {
        output$AnswersOutput <- renderText({
          paste("The top 10 most starting and ending stations are listed below with the number of bikes
                starting from or ending at the specific station during the data set period. Based on both tables, we can see some common
                stations that can be considered general hubs of bicycling traffic. 9 out of the 10 stations appear on both lists. The only ones that
                do not appear on both are 3067, which appears as a starting station, and 3063, which appears as a stopping station.")
        })
        output$AnswersTableOutput <- renderTable(startingStationpop10)
        output$AnswersTable2Output <- renderTable(endingStationpop10)
        }
      if(input$AnswersInput == "Average Distance traveled")
      {
        output$AnswersOutput <- renderText({
          paste("The average distance traveled was 7814.64 meters. This is about 5 miles. To come up with this number, I had to make numerous assumptions.
                Instead of omitting the round-trips, I looked up the average casual bike speed, which was between 10-15 km per hour. I took the median, 12.5 km per hour
                and multiplied this to the duration of the round-trip. For one way trips, I was not satisfied using the haversine formula as this
                doesn't represent the path a human would actually bike. Instead, I took a few samples of the distance calculated using the Haversine formula between two
                latitude/longitude points and compared that to the distance calculated using the Google Maps Distance Matrix API through the gmapsdistance library in R.
                I calculated on average for each sample, the percent differene between the Google Maps API calculation and the Haversine formula calculation. The Google Map distance, 
                on average, was 36.9% more distance than the haversine formula. I used the haversine formula on all the one-way trips, and multiplied the distances by 1.369
                to account for the discrepancy. Averaging this yielded 7814.64 meters as the average distance.")
        })
        output$AnswersTableOutput <- renderTable({})
        output$AnswersTable2Output <- renderTable({})
        }
      if(input$AnswersInput == "Regular commute bike sharers")
      {
        output$AnswersOutput <- renderText({
          paste("As seen below, there is a significant amount of people using Monthly Passes and people who Walk-up. 
                I considered regular bike share commuters to be those using flex pass, monthly pass, or staff annual. Adding those up yields 91203 out of 132427 riders.
                This is about 68.9%, or a little over two-thirds of the users use bike shareres as regular commute.")
        })
        output$AnswersTableOutput <- renderTable(PassholderTypeDistributionNew)
        output$AnswersTable2Output <- renderTable({})
        }
      if(input$AnswersInput == "Ridership changing with the seasons")
      {
        output$AnswersOutput <- renderText({
          paste("Ridership changed with seasons dramatically based on the number of trips. There were thousands more trips in the months of August and September compared to 
                several other months. This makes sense as the temperature is hot enough for biking weather compared to the winter months.
                There was a significant difference nominally for both monthly pass and walk-up users during the aforementioned months, with small gains for
                in flex pass trips. However, the proportion of monthly pass users and walk-up users roughly remained the same relative to each other. Given a higher number of total rides
                during August and September, it was expected that one way trips increased dramatically during the summer/fall months as well. 
                What was surprising was that average trip duration by month was opposite of what I expected, as it was greater during the winter months than summer/fall months. 
                There could be a couple of explanations. As mentioned before about the monthly pass, one way theory, people could be going to work
                more often using the bike share system, which would take less time than a free roam. There's also an argument that because less people would
                bike share during winter months, those that do are the ones like to do it longer due to a myriad of reasons.
                This could be anything from cardio exercise to practicing for a cycling competition. Below are the graphs of Trip Route Category and Passholder type by month")
        })
        output$AnswersTableOutput <- renderTable({TypeVsMonthTable})
        output$AnswersTable2Output <- renderTable({RoutecatVsMonthTable})
      }
      if(input$AnswersInput == "Net Change of bikes over the day")
      {
        output$AnswersOutput <- renderText({
          paste("Below is the top 20 net changes of bikes over the day by station ID. This was done by counting the number of
          times a bike started subtracting the number of times a bike started at a station ID during the day by the number of times
          a bike ended at the station ID (ending - starting). Only one-way trips were considered since round-trips obviously start and end at the same place.
          The final net change was measured for each day. The net changes were summed up for each station ID and divided by the total number of days
          a bike had either started or ended at the station ID. The top value of 12 from station 3009 should be 
          analyzed with caution, as that station only had one day of activity, so that could be due to variance. All the other
          values in the top 20 have dozens of days of changes. Even so, there were some large values, as the second largest net charge had a deficit 
          of over 8 bikes. That means significant movement of bikes is necessary to balance out the bikes for the next day.")
        })
        output$AnswersTableOutput <- renderTable({NetChangeDayTable})
        output$AnswersTable2Output <- renderTable({})
        }
      if(input$AnswersInput == "Trip Route Category-Passholder type combinations")
      {
        output$AnswersOutput <- renderText({
          paste("Monthly Pass, One Way was the most popular Trip Route Category-Passholder combination with Walk-Up, One Way coming in second. 
          The least popular were the combinations involving the staff annual pass and the Flex Pass, Round Trip combination. I believe there are
          multiple explanations for the popularity of Monthly Pass, One Way. I believe most of the people that use Monthly Pass are those that need
          the bikes for work, school, or other important matters. It's worth it to purchase monthly passes as it allows for consistent use. 
          However, I believe the flex pass is not utilized because of the lack of flexibility in the long run. People take vacations,
          are off from school, or are sick. In any case, for weeks or long stretches of time, bikes are not needed. Combined, it makes sense that
          monthly pass, one way (one way to work, school), is the most popular trip route category-passholder combination. Walk-up, one way also makes
          sense for the second most common combination as there's plenty of times when a bike ride could be the most convenient mode of transportation
          at the moment for a quicker commute. Due to the probably limited amount of staff, the low amount of uses for the staff annual pass
          makes complete sense. The Flex pass potentially yields less results due to its inflexibility. As a result, the market should be focused
          on monthly passes.")
        })
        output$AnswersTableOutput <- renderTable({CategoryVsTypeTable})
        output$AnswersTable2Output <- renderTable({})
        }
      })
      }
shinyApp(ui = ui, server = server)