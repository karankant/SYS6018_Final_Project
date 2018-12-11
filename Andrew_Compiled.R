#Andrew Dahbura, amd6ua
#SYS 6018 Final Project

setwd("~/Desktop/Fall 2018/SYS 6018/Final Project")
library(tidyverse)
library(readr)
library(caret)

#Load data sets
flights  <- read_csv ("flights.csv")
airports <- read_csv ("airports.csv")
airlines <- read_csv ("airlines.csv")

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

dfAirportID <- read.csv ("L_AIRPORT_ID.csv",
                         colClasses=c("character", "character"), col.names=c("AirportID", "Description"))

dfAirport <- read.csv ("L_AIRPORT.csv",
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

