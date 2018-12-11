#Andrew Dahbura, amd6ua
#SYS 6018 Final Project

setwd("~/Desktop/Fall 2018/SYS 6018/Final Project")
library(tidyverse)
library(readr)
library(caret)
library(lubridate)
library(chron)
library(gridExtra)
library(ggplot2)

#Load data sets
flights  <- read_csv ("data/flights.csv")
airports <- read_csv ("data/airports.csv")
airlines <- read_csv ("data/airlines.csv")

#############################################################################
#                                                                           #
#                         Data Cleaning                                     #
#                                                                           #
#############################################################################

###Tasks for data cleaning###
#1. Clean ORIGIN_AIRPORT and DESTINATION_AIRPORT using lookup files
#2. Remove all cancelled flights
#3. Remove all diverted flights
#4. Remove rows containing NA for response variable and impute other NA's
#5. Remove flight if ARRIVAL_DELAY < -60
#6. Remove columns containing information that will not be present in test data
#7. Convert scheduled departure times to half hour intervals
#8. Covert categorical variables to factors w/ levels

#1.
#Looks like the origin and destination airports use two different codes
#https://www.kaggle.com/smiller933/fixing-airport-codes/notebook

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

#2.
#removing cancelled flights
flights = flights[flights$CANCELLED == 0,]
nrow(flights)

#3.
#remove diverted flights
flights = flights[flights$DIVERTED == 0,]
nrow(flights)

#4.
#removing rows containing missing values in response variable ARRIVAL_DELAY
flights = flights[complete.cases(flights$ARRIVAL_DELAY), ]
nrow(flights)

#5.
#Remove flights that arrived more than 60 minutes early since this 
#is assumed to be a complete abnormality
flights = flights[flights$ARRIVAL_DELAY > -60,]
nrow(flights)

#6.
#Subset to desired columns for modelling
cols<-c('MONTH','DAY','DAY_OF_WEEK','AIRLINE','ORIGIN_AIRPORT','DESTINATION_AIRPORT','SCHEDULED_DEPARTURE','SCHEDULED_TIME','DISTANCE','ARRIVAL_DELAY')
flights <- flights[,cols]

#7.
#Convert scheduled departure time to half hour intervals
flights$SCHEDULED_DEPARTURE<-sub("(\\d+)(\\d{2})", "\\1:\\2", flights$SCHEDULED_DEPARTURE) # put in delimitter
flights$SCHEDULED_DEPARTURE2<-as.POSIXct(flights$SCHEDULED_DEPARTURE, format = '%H:%M')
flights$SCHEDULED_DEPARTURE3<-lubridate::round_date(flights$SCHEDULED_DEPARTURE2, "30 minutes") 
flights$SCHEDULED_DEPARTURE<-format(flights$SCHEDULED_DEPARTURE3,'%H:%M')
flights <- as.data.frame(flights)
flights <- flights[, -c(11,12)]

#8.
#Convert categorical variables to factors
#convert destination airport to factor
flights$DESTINATION_AIRPORT <- as.factor(flights$DESTINATION_AIRPORT)
#convert origin_airport to factor
flights$ORIGIN_AIRPORT <- as.factor(flights$ORIGIN_AIRPORT)
#convert month to factor
flights$MONTH <- as.factor(flights$MONTH)
#convert day to factor
flights$DAY <- as.factor(flights$DAY)
#convert day of week to factor
flights$DAY_OF_WEEK <- as.factor(flights$DAY_OF_WEEK)
#convert airline to factor
flights$AIRLINE <- as.factor(flights$AIRLINE)
#convert scheduled departure to factor
flights$SCHEDULED_DEPARTURE <- as.factor(flights$SCHEDULED_DEPARTURE)


#############################################################################
#                                                                           #
#                         Data Exploration                                  #
#                                                                           #
#############################################################################

###Tasks for data exploration###
#1. Summary statistics for flights dataframe: size, #rows, #cols, each column's type, 
#   unique values, #NA's and percentage NA's.
#2. Number of flights by airport to determine busier airports (Karan)
#3. Number of flights by month to see heavier traffic during certain travel seasons or holidays (Rakesh)
#4. Summary statistics by airline— to assess punctuality of each airline 
#percent of flights by company and average delay across flights for each company. Economies of 
#scale effect here? (Karan)
#5. Summary statistics for response variable


#1.
#Write function to view important data set information
#Function will show us the df name, size, #rows, #cols, and each column's type, unique values, #NA's
#and percentage NA's.
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

#View flights information --> Karant insert output here
df.info(flights)




#5.
#Response variable summary statistics
summary(flights$ARRIVAL_DELAY)


hist <- hist(flights$ARRIVAL_DELAY, xlab = 'ARRIVAL_DELAY', ylab = "Count")

#For every variable, create a histogram to check the distribution
for (i in 1:9) {
  hist(flights[,i], xlab = colnames(flights)[i], main=colnames(flights)[i])
}

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
airportstats_destination<-airportstats_destination[1:5,]
#plotting airport breakup
bp_destination<-ggplot(airportstats_destination, aes(x=DESTINATION_AIRPORT,y=count))+labs(title="Airline Breakup-total")+geom_bar(stat="identity")
bp_destination

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

#############################################################################
#                                                                           #
#                      Model Preprocessing Using Airport Clusters           #
#                                                                           #
#############################################################################

#Convert response variable to binary for classification and clustering
#converting arrival_delay into a classification response variable
flights$ARRIVAL_DELAY[(flights$ARRIVAL_DELAY < 15)] = 0
flights$ARRIVAL_DELAY[flights$ARRIVAL_DELAY >= 15] = 1





#############################################################################
#                                                                           #
#                       Logistic Regression                                 #
#                                                                           #
#############################################################################









#############################################################################
#                                                                           #
#                       Random Forrest                                      #
#                                                                           #
#############################################################################








#############################################################################
#                                                                           #
#                      Gradient Boosted Trees                               #
#                                                                           #
#############################################################################

