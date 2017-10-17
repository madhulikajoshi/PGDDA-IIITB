###Uber Supply Demand gap assignment###############################################
###Submitted by - Madhulika Joshi##################################################

#Load libraries which will be used
library(stringr)
library(lubridate)
library(ggplot2)

#Setting working directories
setwd("C:/Users/Madhulika/Desktop/Upgrad/module 2_EDA")

#Reading csv into a dataframe
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = TRUE)

#Checking structure and summary
str(uber) #String variables are read as factors. Timestamp needs to be modified.

summary(uber)

View(uber) #Timestamp has got "/" and "-" character which needs to be handled

##########Cleaning Data############################################################

#Replacing all "/" with "-" for getting a consistent format
uber$Request.timestamp <-
  str_replace_all(uber$Request.timestamp, "/", "-")

#Changing the timpstamp to POSIXlt format
uber$Request.timestamp <-
  as.POSIXlt(uber$Request.timestamp, format = "%d-%m-%Y %H:%M")

#Replacing all "/" with "-" for getting a consistent format
uber$Drop.timestamp <-
  str_replace_all(uber$Drop.timestamp, "/", "-")

#Changing the timpstamp to POSIXlt format
uber$Drop.timestamp <-
  as.POSIXlt(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M")

#####################Derived metrics#################################################

#Finding the name for day of the week using lubridate package
uber$Request.Weekday <- wday(uber$Request.timestamp, label = TRUE)

#Extracting request time from timestamp
uber$Request_time <- strftime(uber$Request.timestamp, "%H:%M")

#Extracting drop time from timestamp
uber$Drop_time <- strftime(uber$Drop.timestamp, "%H:%M")

#####Function to accept time and convert it into timeslot
##added a dummy date to convert time string to posix format
##0 - 4:00 AM - early morning
##4:00- 12:00 PM- morning
## 12:00 PM- 5:00PM - afternoon
## 5:00 PM- 9:00 PM - evening
## 9:00 - 12:00 PM - night
fn_get_day_slot <-
  function(x) {
    cut(
      strptime(paste("01/01/01", x), "%y/%m/%d %H:%M"),
      breaks = as.POSIXlt(paste(
        "2001-01-01",
        c(
          "00:00:00",
          "04:00:00",
          "12:00:00",
          "17:00:00",
          "21:00:00",
          "23:59:59"
        )
      )),
      labels = c('early morning', 'morning', 'afternoon', 'evening', 'night')
    )
  }

###Creating timeslot values for Request time and drop time####
uber$Req_timeslot <- fn_get_day_slot(uber$Request_time)

uber$Drop_timeslot <- fn_get_day_slot(uber$Drop_time)

####Creating a variable to highlight whether the request was completed or not#######
uber$Demand_met[which(is.na(uber$Drop.timestamp))] <- "No"

uber$Demand_met[which(!is.na(uber$Drop.timestamp))] <- "Yes"

####writing to csv ##############################################
write.csv(uber, "uber_data.csv", row.names = F)

###############Univariate/ Bivariate/ Segmented analysis##########################################

nrow(uber) ##6745 records/ requests created

###Close to 42% trips completed,58% could not be completed
table(uber$Status) * 100 / nrow(uber)

##plotting request status as a bar plot to show status frequency in the dataset
ggplot(uber, aes(x = Status)) + geom_bar() + geom_text(stat = "count", aes(label =
..count..), vjust = -1) + labs(x = "Request Status", 
    y = "number of requests", title = "Request distribution")

##plotting completed requests and not completed in a plot using Demand_met= "Yes" or "No"
##as a bar plot
ggplot(uber, aes(x = Demand_met)) + geom_bar() + geom_text(stat = "count", aes(label =
            ..count..), vjust = -1) + labs(x = "Request Completed?", y = "number of requests", title = "Request distribution")

##Finding details for Airport-City and City-Airport, shows almost similar distribution
ggplot(uber, aes(x = Demand_met, fill = factor(uber$Pickup.point))) + geom_bar() + geom_text(stat =
            "count", aes(label = ..count..), vjust = -1) + labs(x = "Request Status", y =
                    "number of requests", title = "Request distribution across pickup points")

##Finding number of days for sample data
num_days <- length(unique(date(uber$Request.timestamp)))

##Fnding the days of the week, data available for weekdays only Mon-Fri
unique(uber$Request.Weekday)

##Finding distribution of requests around all days(evenly distributed, similar trend each day)
table(uber$Request.Weekday)

##Plotting data across days to understand distribution
ggplot(uber, aes(
  x = uber$Request.Weekday,
  fill = factor(uber$Pickup.point)
)) + geom_bar() + labs(x = "Weekday", y = "number of requests", title = "Request distribution across weekdays")

##Finding number of active drivers in system
num_drivers <- length(which(!is.na(unique(uber$Driver.id))))

##Average City-Airport successful trips
to_airport <-
  nrow(subset(uber, uber$Demand_met == "Yes" &
                uber$Pickup.point == "City")) / num_days
to_airport

##City-Airport Trips per driver(close to 1 trip/day made by each driver)
to_airport / num_drivers


##Average Airport-City successful trips
to_city <- nrow(subset(uber, uber$Demand_met == "Yes" &
                         uber$Pickup.point == "Airport")) / num_days

##Airport-City Trips per driver (0.88, ie less than 1 ) 
##showing that drivers do not get passengers every day from Airport-City
to_city / num_drivers

##Finding demand for different times of the day
table(uber$Req_timeslot)

##Plot shows demand is high for City-Airport in morning and Airport-City in evening/night
ggplot(uber, aes(x = Req_timeslot, fill = factor(Pickup.point))) + geom_bar()+ labs(x="Request timeslot", y="number of requests", title = "Request distribution across timeslots")

##Finding demand and supply gap for City-Airport
uber_city <- subset(uber, Pickup.point == "City")

##1440 requests were either cancelled or no drivers were found
##for morning making this the problematic timezone
table(uber_city$Demand_met, uber_city$Req_timeslot)

## Plotting supply-demand gap for City-Airport trips, morning time has highest gap
ggplot(uber_city,
       aes(x = Req_timeslot, fill = factor(Demand_met))) + geom_bar(position =
"dodge")+ labs(x="Request timeslot", y="number of requests", title = "Demand-Supply gap for City-Airport")

##Finding the demand and supply gap for Airport-City
uber_airport <- subset(uber, Pickup.point == "Airport")

##1145 requests were either cancelled or no drivers were found for
##evening making this the problematic time zone
table(uber_airport$Demand_met, uber_airport$Req_timeslot)

##Plotting supply-demand gap for Airport-City trips, evening time has highest gap
ggplot(uber_airport,
       aes(x = Req_timeslot, fill = factor(Demand_met))) + geom_bar(position =
      "dodge")+ labs(x="Request timeslot", y="number of requests", title = "Demand-Supply gap for Airport-City")

#######Finding actual gap in demand and supply#################################

##Gap in demand-supply From city to airport
nrow(uber_city) - nrow(subset(uber_city, Demand_met == "Yes"))

##Average Gap per day
nrow(subset(uber_city, Demand_met == "No")) / num_days ##Average 400 trips

##Average Gap for morning as most cancellations happen for morning
nrow(subset(uber_city, Demand_met == "No" &
              Req_timeslot == "morning")) / num_days ##Average 288 trips per day for morning

##Gap in demand-supply From airport to city
nrow(subset(uber_airport, Demand_met == "No"))

##Average Gap per day
nrow(subset(uber_airport, Demand_met == "No")) / num_days ##Average 382 trips

##Average Gap for evening and night as most of the times we get cabs not found
nrow(subset(
  uber_airport,
  Demand_met == "No" &
    Req_timeslot %in% c("evening", "night")
)) / num_days ##Average 313 trips per day for evening and night timeslots

###The above analysis concludes that we are short of roughly 300 drivers
##to meet this demand assuming that on an average a driver can take 1 trip
##from City-Airport and 1 trip from Airport-City.