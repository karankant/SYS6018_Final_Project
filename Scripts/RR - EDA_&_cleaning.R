#import libraries
library(tidyverse)
library('plotly')
#reading data
rm(list = ls())
setwd("/Users/rakeshravi/Documents/Data Mining - R/Project/")
flights  <- read.csv ("flights.csv",  stringsAsFactors=FALSE)
airports <- read.csv ("airports.csv", stringsAsFactors=FALSE)
airlines <- read.csv ("airlines.csv", stringsAsFactors=FALSE)

#checking the number of rows for flights
nrow(flights)

#[1] 5819079

#function for summary statistics
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

#observations - lots of character variables that can be converted to factors
#airports and airlines are just lookup files that can be ignored

unique(flights$AIRLINE)
sort(airlines$IATA_CODE) == sort(unique(flights$AIRLINE))

#variables that have a narrow range of values
table(flights$YEAR, flights$MONTH)
table(flights$DAY)
table(flights$DAY_OF_WEEK)
table(flights$DIVERTED, flights$CANCELLED)

#summary of flights times
## create summary of flight time
keep <- c("SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME","ARRIVAL_DELAY",
          "AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY",
          "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")

summary(flights[ ,keep])


## create boxplot of delay metrics
boxplot(flights[ ,keep[4:length(keep)]], col=4:length(keep), main="Delay Boxplots")



#looks like the origin and destination airports use two different codes
#https://www.kaggle.com/smiller933/fixing-airport-codes/notebook

origin_airports <- unique (flights$ORIGIN_AIRPORT)
destination_airports <- unique (flights$DESTINATION_AIRPORT)
airport_lookup <- unique (airports$IATA_CODE)

cat ("UNMATCHED ORIGIN AIRPORTS: ", length (setdiff (origin_airports, airport_lookup)))

cat ("UNMATCHED DESTINATION AIRPORTS: ", length (setdiff (destination_airports, airport_lookup)))

#clearly that is a lot of airports

#reading a lookup file for both
lu_airport <- read.csv("L_AIRPORT.csv", stringsAsFactors= FALSE )
lu_airport_ID <- read.csv("L_AIRPORT_ID.csv", stringsAsFactors = FALSE)

#merging the lookups for both kinds of code
final_lu <- merge(lu_airport, lu_airport_ID, by = "Description")
final_lu$Description <- NULL
head(final_lu)

#subset dataframe that has only one type of code or the codes that are numeric in nature
#use regular expressions to detect the rows that have numbers in their values 

#Experiment 1
#check if the number of airpots with only numbers and the airports with a mix of numbers & alphabets match the total rows
k = nrow(flights[which(grepl('^[0-9]',flights$ORIGIN_AIRPORT)),]) + nrow(flights[which(!grepl('^[0-9]',flights$ORIGIN_AIRPORT)),])
k
## 5819079
#looks like it matches the total number of rows


#Experiment 2 
#Check if the code patterns are consistent for origin and destinations
#Numerical Pattern
i = nrow(flights[which(grepl('^[0-9]',flights$ORIGIN_AIRPORT)),])
j = nrow(flights[which(grepl('^[0-9]',flights$DESTINATION_AIRPORT)),])         
i
j

# > i
# [1] 486165
# > j
# [1] 486165   
      
#Alphanumeric Pattern
i = nrow(flights[which(!grepl('^[0-9]',flights$ORIGIN_AIRPORT)),])
j = nrow(flights[which(!grepl('^[0-9]',flights$DESTINATION_AIRPORT)),])   
i
j

# > i
# [1] 5332914
# > j
# [1] 5332914

#looks like the pattern holds up
#we can go ahead and subset and merge with the lookup

#splitting original dataframe into two based on the type of code used in origin and destination airport
flights_num <- flights[which(grepl('^[0-9]',flights$ORIGIN_AIRPORT)),]
flights_alpha <- flights[which(!grepl('^[0-9]',flights$ORIGIN_AIRPORT)),]

#Imputing origin airport using ID lookup
f <- flights_num$ORIGIN_AIRPORT
df <- data.frame (ID = as.character (f), stringsAsFactors = FALSE)
colnames(df) <- "Description"
colnames(final_lu)[2] <- "Description"
num_records = nrow (df)
final_lu <- final_lu [! (final_lu$Code.x %in% c('BSM', 'NYL')),]
df <- merge(x = df, y = final_lu, by="Description", all.x = TRUE)
flights_num$ORIGIN_AIRPORT <- df[,2]

#Imputing destination airport using ID lookup
g <- flights_num$DESTINATION_AIRPORT
df <- data.frame (ID = as.character (g), stringsAsFactors = FALSE)
colnames(df) <- "Description"
colnames(final_lu)[2] <- "Description"
num_records = nrow (df)
final_lu <- final_lu [! (final_lu$Code.x %in% c('BSM', 'NYL')),]
df <- merge(x = df, y = final_lu, by="Description", all.x = TRUE)
flights_num$DESTINATION_AIRPORT <- df[,2]

#the above step reduces the number of levels in a factor by half

#combining dataframes row-wise
flights_final = rbind(flights_num, flights_alpha)
#calculating the number of rows
nrow(flights_final)

save.image(file = "blah.RData")
#check if there are the numerical codes of airports are gone
i = nrow(flights_final[which(!grepl('^[0-9]',flights_final$ORIGIN_AIRPORT)),])
j = nrow(flights_final[which(!grepl('^[0-9]',flights_final$DESTINATION_AIRPORT)),])   
i
j

# > i
# [1] 5819079
# > j
# [1] 5819079

#codes are uniform now

#removing cancelled flights
flights_final = flights_final[flights_final$CANCELLED == 0,]
nrow(flights_final)

#[1] 5729195

#removing missing values in Arrival Delays
flights_clean = flights_final[complete.cases(flights_final$ARRIVAL_DELAY), ]
nrow(flights_clean)

#[1] 5714008

#remove diverted flights
flights_clean = flights_clean[flights_clean$DIVERTED == 0,]
nrow(flights_clean)

#[1] 5714008

#flights that arrived more than 60 minutes early
flights_clean = flights_clean[flights_clean$ARRIVAL_DELAY > -60,]
nrow(flights_clean)

#[1] 5713508

#checking threshold for dropping airport combinations with very few flights
summary = flights_clean %>% group_by(ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>% summarise(count=n())
colnames(summary)[3] <- "total_count"
summary <- summary[summary$total_count > 20,]
k <- summary %>% group_by(total_count) %>% summarise(count = n())
#plot to demonstrate the distribution
plot(k$total_count,k$count, xlab = "frequency" , ylab = "counts")

#dataframe with airport combinations that have less than 20 flights
#it removes over 400k rows and it makes sense to do this to only check model performance
df3 = merge(flights_final, summary, by = c("ORIGIN_AIRPORT","DESTINATION_AIRPORT"))
nrow(df3)-nrow(flights_clean)
#calculating values that are below 5 percential
quantile(summary$total_count,0.05)

#removing columns that we would not be using for predictive modelling
keep = c("MONTH",
         "DAY",
         "DAY_OF_WEEK" ,
         "AIRLINE", 
         "FLIGHT_NUMBER",
         "TAIL_NUMBER",
         "ORIGIN_AIRPORT",
         "DESTINATION_AIRPORT",
         "SCHEDULED_DEPARTURE",
         "SCHEDULED_TIME",
         "DISTANCE",
         "ARRIVAL_DELAY")

flights_m <- flights_clean[,keep]

#converting arrival_delay into a classification response variable
flights_m$ARRIVAL_DELAY[flights_m$ARRIVAL_DELAY < 15] = 0
flights_m$ARRIVAL_DELAY[flights_m$ARRIVAL_DELAY >= 15] = 1
tail(flights_m)

#variable conversions 
flights_m$MONTH = as.factor(flights_m$MONTH)
flights_m$DAY = as.factor(flights_m$DAY)
flights_m$DAY_OF_WEEK = as.factor(flights_m$DAY_OF_WEEK)
flights_m$AIRLINE = as.factor(flights_m$AIRLINE)
flights_m$FLIGHT_NUMBER = as.factor(flights_m$FLIGHT_NUMBER)
flights_m$TAIL_NUMBER = as.factor(flights_m$TAIL_NUMBER)
flights_m$ORIGIN_AIRPORT = as.factor(flights_m$ORIGIN_AIRPORT)
flights_m$DESTINATION_AIRPORT = as.factor(flights_m$DESTINATION_AIRPORT)
flights_m$SCHEDULED_DEPARTURE = as.factor(flights_m$SCHEDULED_DEPARTURE)

#time object conversion
flights_m$SCHEDULED_DEPARTURE <- as.POSIXct(flights_m$SCHEDULED_DEPARTURE, format = '%H%M')
       
#checking types before modelling
str(flights_m)

#BASELINE MODEL - Logistic Regression 

#simplifying the model
flights_m$MONTH = as.numeric(as.character(flights_m$MONTH))
flights_m$DAY = as.numeric(as.character(flights_m$DAY))
flights_m$DAY_OF_WEEK = as.numeric(as.character(flights_m$DAY_OF_WEEK))
flights_m$ARRIVAL_DELAY= as.numeric(as.character(flights_m$ARRIVAL_DELAY))

#splitting into training and testing
library(caret)
Train <- createDataPartition(flights_m$ARRIVAL_DELAY, p=0.6, list=FALSE)
training <- flights_m[ Train, ]
testing <- flights_m[ -Train, ]
var_list = c("MONTH","DAY","DAY_OF_WEEK","SCHEDULED_TIME","DISTANCE","ARRIVAL_DELAY")
testing = testing[,var_list]


#training model
mod_fit <- train(ARRIVAL_DELAY ~ MONTH + DAY + DAY_OF_WEEK + 
                   SCHEDULED_TIME + DISTANCE,  data=training, method="glm", family="binomial")

#summary of model
summary(mod_fit)

#variable importance as per logistic regression
varImp(mod_fit)


#making predictions
pred = predict(mod_fit, newdata=testing[,1:5])
accuracy <- table(pred, testing[,"ARRIVAL_DELAY"])
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data=pred, testing$ARRIVAL_DELAY)


#ROC Curve
library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(mod_fit, newdata=testing[,1:5])
preds <- prediction(pred, testing$ARRIVAL_DELAY)
perf <- performance(preds, measure = "tpr", x.measure = "fpr")
plot(perf)

#AUC
auc <- performance(preds, measure = "auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.5367374

#Sensitivity vs Specificity Curve to determine the right hyperparameters
sens <- data.frame(x=unlist(performance(preds, "sens")@x.values), 
                   y=unlist(performance(preds, "sens")@y.values))
spec <- data.frame(x=unlist(performance(preds, "spec")@x.values), 
                   y=unlist(performance(preds, "spec")@y.values))

sens %>% ggplot(aes(x,y)) + 
  geom_line() + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") +
  theme(axis.title.y.right = element_text(colour = "red"), legend.position="none") 

#------------------------------------------------------------------------------------------------------------------------
#some exploratory data analysis
#Let's examine the relationship between airtime and distance
distime <- flights_clean %>% 
  group_by (AIRLINE) %>%
  summarise(
    mean_arrdelay= mean(ARRIVAL_DELAY, na.rm = T),
    sd_arrdelay = sd(ARRIVAL_DELAY, na.rm = T),
    max_arrdelay = max(ARRIVAL_DELAY, na.rm = T)
  )
ann1 <- flights_final %>%  group_by(DISTANCE,AIR_TIME) %>% summarise(count = n())
plot(x = ann1$DISTANCE,y = ann1$AIR_TIME,main = "Time and Distance",xlab = "Distance",ylab = "Time in air",col = "lightcoral")

#the plot validates airtime is proportional to distance

#Market Share Analysis
marketShare <- flights %>% 
  group_by(AIRLINE) %>%
  dplyr::summarise(Count = n(),
                   mean_ARRIVAL_DELAY = mean(ARRIVAL_DELAY),
                   median_ARRIVAL_DELAY = median(ARRIVAL_DELAY),
                   min_ARRIVAL_DELAY = min(ARRIVAL_DELAY),
                   max_ARRIVAL_DELAY = max(ARRIVAL_DELAY),
                   mean_DEPARTURE_DELAY = mean(DEPARTURE_DELAY),
                   median_DEPARTURE_DELAY = median(DEPARTURE_DELAY),
                   min_DEPARTURE_DELAY = min(DEPARTURE_DELAY),
                   max_DEPARTURE_DELAY = max(DEPARTURE_DELAY)
  ) %>% arrange(desc(Count))


plot_ly(marketShare, labels = ~AIRLINE, values = ~Count, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        marker = list(
          line = list(color = '#FFFFFF', width = 1)),
        showlegend = FALSE)
#there are many airlines that have amore than 5% share and it would be interesting to see if predictive models
#perform better on individual airlines

