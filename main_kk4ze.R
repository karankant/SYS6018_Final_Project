library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(chron)
library(gridExtra)

setwd("~/GitHub/SYS6018_Final_Project")
flights  <- read_csv ("data/flights.csv")
airports <- read_csv ("data/airports.csv")
airlines <- read_csv ("data/airlines.csv")

flights2<-flights
#flights<-flights2


#=================Fixing airline codes==================#
# 
  
  dfFlights <- flights
  
  #check unique airports for
  origin_airports <- unique (flights$ORIGIN_AIRPORT)
  destination_airports <- unique (flights$DESTINATION_AIRPORT)
  airport_lookup <- unique (flights$IATA_CODE)
  
  #check how many airports are
  cat ("UNMATCHED ORIGIN AIRPORTS: ", length (setdiff (origin_airports, airport_lookup)))
  
  cat ("UNMATCHED DESTINATION AIRPORTS: ", length (setdiff (destination_airports, airport_lookup)))
  
  
  
  dfAirportID <- read.csv ("data/L_AIRPORT_ID.csv",
                           colClasses=c("character", "character"), col.names=c("AirportID", "Description"))
  
  dfAirport <- read.csv ("data/L_AIRPORT.csv",
                         colClasses=c("character", "character"), col.names=c("IATA_CODE", "Description"))
  
  airportlist<-merge(dfAirportID,dfAirport)
  
  #keep distinct airport code
  airportlist<-airportlist %>% distinct(AirportID, .keep_all = TRUE)
  
  #Checking if an IATA code has multiple airport IDs
  airport_aggr<-airportlist %>% group_by(IATA_CODE)%>% summarise(length(IATA_CODE))
  airport_aggr_ID<-airportlist %>% group_by(AirportID)%>% summarise(no_rows=length(AirportID))
  airport_aggr_ID<-airport_aggr_ID[airport_aggr_ID$no_rows>1,]
  
  
  #replace 5 digit number airport IDs with corresponding IATA code for ORIGIN and DESTINATION airports
  indexes = which(nchar(flights$ORIGIN_AIRPORT) == 5)
  flights$ORIGIN_AIRPORT[indexes] = unlist(lapply(flights$ORIGIN_AIRPORT[indexes], function(x){
    value = airportlist$IATA_CODE[airportlist$AirportID == x]
    return(value)
  }))
  
  indexes = which(nchar(flights$DESTINATION_AIRPORT) == 5)
  flights$DESTINATION_AIRPORT[indexes] = unlist(lapply(flights$DESTINATION_AIRPORT[indexes], function(x){
    value = airportlist$IATA_CODE[airportlist$AirportID == x]
    return(value)
  }))
  
  
  #==================DATA CLEANUP=======================#
  
  
  #removing canceled flights
  flights = flights[flights$CANCELLED == 0,]
  nrow(flights)
  
  #[1] 5729195
  
  #removing missing values in Arrival Delays
  flights = flights[complete.cases(flights$ARRIVAL_DELAY), ]
  nrow(flights)
  
  #[1] 5714008
  
  #remove diverted flights
  flights = flights[flights$DIVERTED == 0,]
  nrow(flights)
  
  #remove delays less than -60 min
  
  flights = flights[flights$ARRIVAL_DELAY > -60,]
  nrow(flights)
  
  cols<-c('MONTH','DAY','DAY_OF_WEEK','AIRLINE','FLIGHT_NUMBER','TAIL_NUMBER','ORIGIN_AIRPORT','DESTINATION_AIRPORT','SCHEDULED_DEPARTURE','SCHEDULED_TIME','DISTANCE','ARRIVAL_DELAY')
  flights<-flights[,cols]
  nrow(flights)
  
  # aggr_airport_origin<-flights %>% group_by(ORIGIN_AIRPORT)%>% summarise(no_rows=length(ORIGIN_AIRPORT))
  # aggr_airport_origin<-as.data.frame(aggr_airport_origin)
  # aggr_airport_origin<-aggr_airport_origin[aggr_airport_origin$no_rows>20,1]
  # flights3<-dplyr::filter(flights, ORIGIN_AIRPORT %in% aggr_airport_origin)
  # flights<-flights3
  # nrow(flights)
  # #5713422 rows
  # 
  # aggr_airport_dest<-flights %>% group_by(DESTINATION_AIRPORT)%>% summarise(no_rows=length(DESTINATION_AIRPORT))
  # aggr_airport_dest<-as.data.frame(aggr_airport_dest)
  # aggr_airport_dest<-aggr_airport_dest[aggr_airport_dest$no_rows>.05*max(no_rows),1]
  # 
  # flights<-dplyr::filter(flights, DESTINATION_AIRPORT %in% aggr_airport_dest)
  
  # #remove flights that occur less than 20 times
  # aggr_airport_total<-flights %>% group_by(ORIGIN_AIRPORT,DESTINATION_AIRPORT)%>% summarise(count=n())
  # aggr_airport_total<-as.data.frame(aggr_airport_total)
  # aggr_airport_total<-aggr_airport_total[aggr_airport_origin$count>20,1:2]
  # df3 = merge(flights_final, aggr_airport_total, by = c("ORIGIN_AIRPORT","DESTINATION_AIRPORT"))
  # nrow(df3)
  # 

#Rounding departure times into 30 min intervals
flights2<-flights
flights$SCHEDULED_DEPARTURE<-sub("(\\d+)(\\d{2})", "\\1:\\2", flights$SCHEDULED_DEPARTURE) # put in delimiter ':'
flights$SCHEDULED_DEPARTURE<-as.POSIXct(flights$SCHEDULED_DEPARTURE, format = '%H:%M')
flights$SCHEDULED_DEPARTURE<-lubridate::round_date(flights$SCHEDULED_DEPARTURE, "30 minutes") 
flights$SCHEDULED_DEPARTURE<-format(flights$SCHEDULED_DEPARTURE,'%H:%M')

#Converting variables to categorical
flights$MONTH = as.factor(flights$MONTH)
flights$DAY = as.factor(flights$DAY)
flights$DAY_OF_WEEK = as.factor(flights$DAY_OF_WEEK)
flights$AIRLINE = as.factor(flights$AIRLINE)
flights$FLIGHT_NUMBER = as.factor(flights$FLIGHT_NUMBER)
flights$TAIL_NUMBER = as.factor(flights$TAIL_NUMBER)
flights$ORIGIN_AIRPORT = as.factor(flights$ORIGIN_AIRPORT)
flights$DESTINATION_AIRPORT = as.factor(flights$DESTINATION_AIRPORT)
#flights$SCHEDULED_DEPARTURE = as.factor(flights$SCHEDULED_DEPARTURE)
flights$SCHEDULED_TIME<-as.numeric(flights$SCHEDULED_TIME)
flights$DISTANCE<-as.numeric(flights$DISTANCE)

#======================Saving cleaned CSV=========================#

write.csv(flights, file = "data/flights_clean.csv")



#flights<-read_csv ("data/flights_clean.csv")

#===================EDA==========================#

df.info <- function(x) {
  dat  <- as.character(substitute(x))  ##data frame name
  size <- format(object.size(x), units="Mb")  ##size of data frame in Mb
  
  ##column information
  column.info <- data.frame( column        = names(sapply(x, class)),
                             class         = sapply(x, class),
                             unique.values = sapply(x, function(y) length(unique(y))),
                             missing.count = colSums(is.na(x)),
                             missing.pct   = round(colSums(is.na(x)) / nrow(x) * 100, 2))
  
  row.names(column.info) <- 1:nrow(column.info)
  
  list(data.frame     = data.frame(name=dat, size=size),
       dimensions     = data.frame(rows=nrow(x), columns=ncol(x)),
       column.details = column.info)
}

df.info(flights)
df.info(airports)
df.info(airlines)

unique(flights$AIRLINE)
#unique airlines:  [1] "AS" "AA" "US" "DL" "NK" "UA" "HA" "B6" "OO" "EV" "F9" "WN" "MQ" "VX"

boxplot(flights[ ,11],main="Distance Boxplot")

boxplot(flights[ ,12],main="Arrival Delay Boxplot")

#a few useful histograms

hist(log(flights$ARRIVAL_DELAY))
hist(flights$SCHEDULED_TIME)
hist(flights$SCHEDULED_DEPARTURE)


#checking airline codes
airlines
unique(flights$AIRLINE)
sort(airlines$IATA_CODE) == sort(unique(flights$AIRLINE))


#Total flights by destination airport 

airportstats_destination<-flights%>%
  group_by(DESTINATION_AIRPORT)%>%
  summarise(count=n(),
            mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY),
            min_ARRIVAL_DELAY<- min(ARRIVAL_DELAY),
            max_ARRIVAL_DELAY<- max(ARRIVAL_DELAY)
            ) %>% arrange(desc(count))


#plotting airport breakup
bp_destination<- ggplot(airportstats_destination[1:10,], aes(x="", y=count, fill=DESTINATION_AIRPORT))+
  geom_bar(width = 1, stat = "identity")
bp_destination
pie <- bp_destination + coord_polar("y", start=0)
pie

airportstats_origin<-flights%>%
  group_by(ORIGIN_AIRPORT)%>%
  summarise(count=n(),
            mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY),
            min_ARRIVAL_DELAY<- min(ARRIVAL_DELAY),
            max_ARRIVAL_DELAY<- max(ARRIVAL_DELAY)
  ) %>% arrange(desc(count))

#stats and plots for top 5 busiest airports
ATL_stats<-flights[flights$ORIGIN_AIRPORT=='ATL',]%>%
  group_by(AIRLINE)%>%
  summarise(count=n(),
            delay=mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY)
            )
p1<-ggplot(ATL_stats, aes(x=AIRLINE,y=count))+labs(title="Airline Breakup-ATL")+geom_bar(stat="identity")
q1<-ggplot(ATL_stats, aes(x=AIRLINE,y=delay))+labs(title="Delay Breakup per Airline-ATL")+geom_bar(stat="identity")

ORD_stats<-flights[flights$ORIGIN_AIRPORT=='ORD',]%>%
  group_by(AIRLINE)%>%
  summarise(count=n(),
            delay=mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY)
  )
p2<-ggplot(ORD_stats, aes(x=AIRLINE,y=count))+labs(title="Airline Breakup-ORD")+geom_bar(stat="identity")
q2<-ggplot(ORD_stats, aes(x=AIRLINE,y=delay))+labs(title="Delay Breakup per Airline-ORD")+geom_bar(stat="identity")

DFW_stats<-flights[flights$ORIGIN_AIRPORT=='DFW',]%>%
  group_by(AIRLINE)%>%
  summarise(count=n(),
            delay=mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY)
  )
p3<-ggplot(DFW_stats, aes(x=AIRLINE,y=count))+labs(title="Airline Breakup-DFW")+geom_bar(stat="identity")
q3<-ggplot(DFW_stats, aes(x=AIRLINE,y=delay))+labs(title="Delay Breakup per Airline-DFW")+geom_bar(stat="identity")

DEN_stats<-flights[flights$ORIGIN_AIRPORT=='DEN',]%>%
  group_by(AIRLINE)%>%
  summarise(count=n(),
            delay=mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY)
  )
p4<-ggplot(DEN_stats, aes(x=AIRLINE,y=count))+labs(title="Airline Breakup-DEN")+geom_bar(stat="identity")
q4<-ggplot(DEN_stats, aes(x=AIRLINE,y=delay))+labs(title="Delay Breakup per Airline-DEN")+geom_bar(stat="identity")


LAX_stats<-flights[flights$ORIGIN_AIRPORT=='LAX',]%>%
  group_by(AIRLINE)%>%
  summarise(count=n(),
            delay=mean_ARRIVAL_DELAY<-mean(ARRIVAL_DELAY)
  )

p5<-ggplot(LAX_stats, aes(x=AIRLINE,y=count))+labs(title="Airline Breakup-LAX")+geom_bar(stat="identity")
q5<-ggplot(LAX_stats, aes(x=AIRLINE,y=delay))+labs(title="Delay Breakup per Airline-LAX")+geom_bar(stat="identity")


grid.arrange(p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,nrow = 2)
