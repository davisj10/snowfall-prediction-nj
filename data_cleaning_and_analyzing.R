# author: Justin Davis
# Data Mining I Project

# loading packages
library(plyr)
library(readr)
library(weathermetrics)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(ggmap)
library(mapproj)
library(devtools)
library(muRL)
install_github('arilamstein/choroplethrZip@v1.4.0')
library(choroplethrZip)
library(tibble)
library(tidyverse)
library(cluster)
library(factoextra)
library(FNN)
library(caret)
library(usedist)

# set seed here
set.seed(10)

### DEFINING FUNCTIONS HERE ###

# function to read in each csv and add a column for the zip code
read_csv_filename <- function(filename){
  ret <- read.csv(filename, stringsAsFactors = FALSE)
  ret$Zip <- regmatches(filename, regexpr("[0-9][0-9][0-9][0-9][0-9]", filename))
  ret
}

# Get lower triangle of correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# function to normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

### LOADING DATA AND PERFORMING CLEANING ###

# loading all data for all zip codes
mydir = "C:/Users/jadtr/Desktop/School/Spring 2020/Data Mining I/Project"
myfiles = list.files(path=mydir, pattern="edit.csv", full.names=TRUE)
#myfiles

# combining all zip codes into one data frame
data = ldply(myfiles, read_csv_filename)
str(data)
# any NA values -- returns false so we are good!
any(is.na(data))

# convert date_time column to character, so we can convert a date object later on
data$date_time <- as.character(data$date_time)

# preview the result
head(data)

# remove unnecessary columns
# some columns have repeat data and some should not have any noticable impact on predictions
data = subset(data, select = -c(visibility, uvIndex.1, 
                                moon_illumination, moonrise, moonset, sunrise, 
                                sunset, tempC, FeelsLikeC))
head(data)

# renaming date column
data = rename(data, c("Date" = date_time))
head(data)

# converting celsius columns to fahrenheit
data = rename(data, c("MaxTempF" = maxtempC, "MinTempF" = mintempC, "DewPointF" = DewPointC, 
                      "HeatIndexF" = HeatIndexC, "WindChillF" = WindChillC))
head(data)
data$MaxTempF <- celsius.to.fahrenheit(data$MaxTempF, round = 1)
data$MinTempF <- celsius.to.fahrenheit(data$MinTempF, round = 1)
data$DewPointF <- celsius.to.fahrenheit(data$DewPointF, round = 1)
data$HeatIndexF <- celsius.to.fahrenheit(data$HeatIndexF, round = 1)
data$WindChillF <- celsius.to.fahrenheit(data$WindChillF, round = 1)

# converting dates from characters to dates
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# convert snow totals and precipitation to inches
data = rename(data, c("TotalSnow_IN" = totalSnow_cm, "PrecipIN" = precipMM))
data$TotalSnow_IN <- metric_to_inches(data$TotalSnow_IN, unit.from = "cm", 3)
data$PrecipIN <- metric_to_inches(data$PrecipIN, unit.from = "mm", 3)
head(data)

# add column for average temp
data$AvgTempF <- ((data$MaxTempF + data$MinTempF)/2)
head(data)

### AGGREGATION BEING DONE HERE ###

# aggregating the daily forecasts to get monthly and yearly data sets
snow_year_month_zip_date <- data %>%
  group_by(Zip, yearMonth = floor_date(Date, "month")) %>%
  summarize(snow=sum(TotalSnow_IN), prec=sum(PrecipIN), minTempF = min(MinTempF),
            maxTempF = max(MaxTempF), avgTempF = mean(AvgTempF), avgSunHours = mean(sunHour),
            avgUVIndex = mean(uvIndex), avgDewPointF = mean(DewPointF), avgHeatIndexF = mean(HeatIndexF),
            avgWindChillF = mean(WindChillF), avgWindGustKmph = mean(WindGustKmph), avgWindSpeedKmph = mean(windspeedKmph),
            avgWindDir = mean(winddirDegree), avgPressure = mean(pressure), avgHumidity = mean(humidity), avgCloudCover = mean(cloudcover))
# reformatting the date to get rid of the day
snow_year_month_zip <- snow_year_month_zip_date
snow_year_month_zip$yearMonth <- format(snow_year_month_zip$yearMonth, "%Y-%m")
snow_year_month_zip <- as.data.frame(snow_year_month_zip)
#snow_year_month_zip

# aggregating the daily forecasts to get yearly data sets
snow_year_zip <- data %>%
  group_by(Zip, year = floor_date(Date, "year")) %>%
  summarize(snow=sum(TotalSnow_IN), prec=sum(PrecipIN), minTempF = min(MinTempF),
            maxTempF = max(MaxTempF), avgTempF = mean(AvgTempF), avgSunHours = mean(sunHour),
            avgUVIndex = mean(uvIndex), avgDewPointF = mean(DewPointF), avgHeatIndexF = mean(HeatIndexF),
            avgWindChillF = mean(WindChillF), avgWindGustKmph = mean(WindGustKmph), avgWindSpeedKmph = mean(windspeedKmph),
            avgWindDir = mean(winddirDegree), avgPressure = mean(pressure), avgHumidity = mean(humidity), avgCloudCover = mean(cloudcover))
# reformatting the date to get rid of the month and day
snow_year_zip$year <- format(snow_year_zip$year, "%Y")
snow_year_zip <- as.data.frame(snow_year_zip)
#snow_year_zip

### THE DATA AGGREGATED SO FAR NOW NEEDS TO BE GROUPED INTO SEASONS
## for instance, snow season of 2009-2010 goes from Oct 2009 - Mar 2010

# the data for month and year grouping has been computed already
seasons_year_zip <- snow_year_month_zip_date
# don't have the rest of the data for the 2008-2009 season, so get rid of the 2009 portion
seasons_year_zip <- seasons_year_zip[!(seasons_year_zip$yearMonth == "2009-01-01") & !(seasons_year_zip$yearMonth == "2009-02-01") & !(seasons_year_zip$yearMonth == "2009-03-01"),]
# extract the month portion of the date
t <- format(seasons_year_zip$yearMonth, "%m")
# place it back into dataset
seasons_year_zip$yearMonth <- t
# set up a condition to only use months within the season
condition <- seasons_year_zip$yearMonth %in% c("01", "02", "03", "10", "11", "12")
# keep rows that are true
seasons_year_zip <- seasons_year_zip[condition,]
# now, aggregate all variables for every 6 rows - each 6 rows is a season for a zip code
grouped_seasons <- seasons_year_zip %>% 
  group_by(Zip, season = as.integer(gl(n(), 6, n()))) %>% 
  summarize(snow=sum(snow), prec=sum(prec), minTempF = min(minTempF),
            maxTempF = max(maxTempF), avgSunHours = mean(avgSunHours),
            avgUVIndex = mean(avgUVIndex), avgDewPointF = mean(avgDewPointF), avgHeatIndexF = mean(avgHeatIndexF),
            avgWindChillF = mean(avgWindChillF), avgWindGustKmph = mean(avgWindGustKmph), avgWindSpeedKmph = mean(avgWindSpeedKmph),
            avgWindDir = mean(avgWindDir), avgPressure = mean(avgPressure), avgHumidity = mean(avgHumidity), avgCloudCover = mean(avgCloudCover))
# checking it worked
head(grouped_seasons,12)
# convert season numbers to year ranges
seasons <- c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
s <- grouped_seasons$season
s <- seasons[s]
grouped_seasons$season <- s
# check it worked
head(grouped_seasons$season)

### PERFORMING BASIC STAT CALCULATIONS AND GRAPHS ###

# plotting the year versus snow amounts for all zip codes
ggplot(data = grouped_seasons, aes(x = season, y = snow, colour = Zip)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Snow Totals per Season and Zip Code")

# plotting the year versus average temp for all zip codes
ggplot(data = grouped_seasons, aes(x = season, y = abs(maxTempF-minTempF)/2, colour = Zip)) + geom_point() +
  ylab("Avg Temp (F)") +
  geom_hline(yintercept = c(), color="blue") +
  ggtitle("Average Temp per Season and Zip") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
## we can see from this that the temperatures never really changes much year to year, however,
## for 2020, the average temperature is much lower -- this is because there is only data for the
## first 3 months, which are some of the coldest

## plotting one more time with precipiation amounts
ggplot(data = grouped_seasons, aes(x = season, y = prec, colour = Zip)) + geom_point() + 
  ggtitle("Precipitation per Season and Zip") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## let's plot using season date now
## let's examine the snow fall totals at each zip for each season
ggplot(data = grouped_seasons, aes(x = Zip, y = snow, colour = Zip)) + geom_point() + facet_wrap(~season) + theme(axis.text.x=element_blank()) +
  ggtitle("Snow Totals for Season over All Seasons")

### let's search for some specific features now ###

# 1 - the highest snow amount in a year and where it was (let's extract the whole row!)
# the highest amount should be in north jersey since they typically get more snow
max_snow <- grouped_seasons[which.max(grouped_seasons$snow),]
max_snow
# examining it on the map, this point is right near the northernmost point of NJ - this makes sense!

# 2 - now let's look at the lowest snowfall total
min_snow <- grouped_seasons[which.min(grouped_seasons$snow),]
min_snow
## again, this gives us a point closest to the bottom of NJ and near the shore





####### DOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
### aggregate avg temp for each season and do a graph with a line!!!!!!!!!!






### Plotting of all zip codes we will be using here along with snow totals for each zip code

# plotting the zip codes on the map
uniqueZips <- distinct(data, Zip)
# rename Zip to zip so function can plot the zips
uniqueZips <- rename(uniqueZips, c("zip" = Zip))
#uniqueZips - list the zip codes
# plot zips on map of NJ
zip.plot(uniqueZips, map.type = "county", region = "new jersey", col = "red", pch = 20)

# plotting snow totals on map of nj based on zip codes
# get the average snow fall amount from all the seasons
zip_snow <- grouped_seasons %>% 
  group_by(region = Zip) %>% 
  summarize(value = mean(snow,2))

# create a condition to remove zip codes that cannot be mapped
condition2 <- zip_snow$region %in% c("08645", "08754", "08803", "08875", "08906")
# remove bad zip codes
zip_snow <- zip_snow[!condition2,]

# create a map of NJ to show the average snowfall total
# maps values to zip code sections - sections with no data will be grouped together

# this shows totals with boundaries 
zip_choropleth(zip_snow, 
               state_zoom = "new jersey", 
               title      = "2009-2020 Average Snow Total Per Zip",
               legend     = "Snow (Inches)") + coord_map()

data("zip.regions")
# same plot as before, but gets rid of boundary lines
choro = choroplethrZip::ZipChoropleth$new(zip_snow)
choro$prepare_map()
choro$legend = "Snowfall (Inches)"
ec_zips = zip.regions[zip.regions$state.name %in% "new jersey", "region"]
ec_df   = choro$choropleth.df[choro$choropleth.df$region %in% ec_zips, ]
ec_plot = choro$render_helper(ec_df, "", choro$theme_clean()) + 
          ggtitle("2009-2020 Average Snow Total Per Zip")
ec_plot + coord_map()

### CLUSTERING DONE HERE

## first normalize all data rows - all are numeric, so it will make it easier!
# aggregate first to get average amounts for each zip code over all seasons
average_seasons <- grouped_seasons %>%
  group_by(Zip) %>%
  summarize(snow=mean(snow), prec=mean(prec), minTempF = min(minTempF),
            maxTempF = max(maxTempF), avgSunHours = mean(avgSunHours),
            avgUVIndex = mean(avgUVIndex), avgDewPointF = mean(avgDewPointF), avgHeatIndexF = mean(avgHeatIndexF),
            avgWindChillF = mean(avgWindChillF), avgWindGustKmph = mean(avgWindGustKmph), avgWindSpeedKmph = mean(avgWindSpeedKmph),
            avgWindDir = mean(avgWindDir), avgPressure = mean(avgPressure), avgHumidity = mean(avgHumidity), avgCloudCover = mean(avgCloudCover))
# normalize all columns using the nomralize function to get values between 0 and 1
normalized <- data.frame(average_seasons$Zip, apply(average_seasons[,2:16], 2, scale))
normalized = rename(normalized, c("Zip" = 1))
# change row names to zip code for distance matrix calculations
normalized_rows <- normalized[,-1]
rownames(normalized_rows) <- normalized[,1]

# now let's get the distance matrix!
distance <- get_dist(normalized_rows)
# display the distance matrix
fviz_dist(distance, show_labels = TRUE, lab_size = 7)
# display part of the distance matrix to show on powerpoint
fviz_dist(dist_subset(distance, c(1:15,1:15)), show_labels = TRUE, lab_size = 9)

# examine the elbow graph to determine best k - between 2 or 4
fviz_nbclust(normalized_rows, kmeans, method = "wss")
# silhouette shows 2 as the best, with 4 & 10 as close seconds 
fviz_nbclust(normalized_rows, kmeans, method = "silhouette")

# now let's compute the clustering - with a k value of 4
set.seed(10)
k4 <- kmeans(normalized_rows, centers = 4, nstart = 25)
# plot the clusters
s <- fviz_cluster(k4, data = normalized_rows)
# k4$cluster - shows the zips and cluster numbers

# add cluster number as a column and seperate rows based on cluster
clustered_data <- cbind(uniqueZips, clusterNum = k4$cluster)
cluster1 <- clustered_data[clustered_data$clusterNum == 1,]
cluster2 <- clustered_data[clustered_data$clusterNum == 2,]
cluster3 <- clustered_data[clustered_data$clusterNum == 3,]
cluster4 <- clustered_data[clustered_data$clusterNum == 4,]

# plot to see where they are side-by-side
par(mfrow=c(1,4))
clust_map_1 <- zip.plot(cluster1, map.type = "county", region = "new jersey", col = "red", pch = 20, cex = 2) + title("Cluster 1")
clust_map_2 <- zip.plot(cluster2, map.type = "county", region = "new jersey", col = "green", pch = 20, cex = 2) + title("Cluster 2")
clust_map_3 <- zip.plot(cluster3, map.type = "county", region = "new jersey", col = "blue", pch = 20, cex = 2) + title("Cluster 3")
clust_map_4 <- zip.plot(cluster4, map.type = "county", region = "new jersey", col = "purple", pch = 20, cex = 2) + title("Cluster 4")

### CORRELATION AND PREDICTION BELOW

### graph correlation between variables - make a heatmap
## code inspired from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# remove the zip and season columns
num_seasons <- grouped_seasons[,-c(1:2)]
# first normalize the variables
num_seasons <- data.frame(apply(num_seasons, 2 , scale))
# find correlation values between all variables
cormat <- round(cor(num_seasons),2)
# find correlation values that affect snow totals
cormat_snow <- round(cor(num_seasons, num_seasons$snow),2)
# get values in lower triangle and only display them
lower_tri <- get_lower_tri(cormat)
heat_map <- melt(lower_tri, na.rm = TRUE)

# plot a heat map of the correlation values
ggplot(data = heat_map, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Correlation between Variables") +
  coord_fixed()

### k-nearest neighbor regression to predict snowfall amounts

# first sort rows by season
sorted_seasons <- grouped_seasons[order(grouped_seasons$season),]
# remove zip code and season column - not needed
knn_data <- sorted_seasons[,-c(1:2)]
# select the variables that have a positive pearson correlation 
knn_data <- subset(knn_data, select = c("snow", "prec", "maxTempF", "avgWindDir", "avgPressure", "avgCloudCover"))
# scale the data down
knn_data_scaled <- apply(knn_data, 2, scale)
# convert to data frame
knn_data <- data.frame(knn_data_scaled)

# split our data into testing and training data - ~90% training and ~10% testing
# this model trains on the first 10 seasons and predicts the last 1
smp_ind <- floor(nrow(knn_data)-1*69)

# get testing and training sets
train_data <- subset(knn_data[1:smp_ind,], select = -c(snow))
test_data <- subset(knn_data[(smp_ind+1):nrow(knn_data),], select = -c(snow))

# getting the actual outcome values for both sets
snow_outcome <- knn_data %>% select(snow)
train_outcome <- as.data.frame(snow_outcome[1:smp_ind,])
train_outcome = rename(train_outcome, c("snow" = 1))
test_outcome <- as.data.frame(snow_outcome[(smp_ind+1):nrow(knn_data),])
test_outcome = rename(test_outcome, c("snow" = 1))

### Plot model accuracy vs different values of k

# create a data frame to store accuracy for each value of k
# trying k-values from 1-100 (excluding 2)
k <- as.data.frame(c(1,3:100))
# rename column
k = rename(k, "k" = c(1))
# add an accuracy column that is initially 0
k$accuracy <- 0
for (i in k$k) {
  # perform regression here
  results <- knn.reg(train_data, test_data, train_outcome, k = i)
  # get predicted values
  pred_values <- results[["pred"]]
  # add actual test values to data set
  examine <- test_outcome
  # rename the column
  examine = rename(examine, "actual" = c(1))
  ## need to unscale data now
  snow_scale_mean <- mean(sorted_seasons$snow)
  snow_scale_sd <- sd(sorted_seasons$snow)
  unscaled_pred <- pred_values * snow_scale_sd + snow_scale_mean
  unscaled_actual <- test_outcome * snow_scale_sd + snow_scale_mean
  # add unscaled values back
  examine$actual <- as.numeric(unlist(unscaled_actual))
  # add predicted values
  examine$pred <- unscaled_pred
  # add the differences as a column
  examine$diff <- abs(examine$actual-examine$pred)
  # create a column for error
  examine$error <- (examine$diff/examine$actual)*100
  examine$error <- as.double(examine$error, 2)
  # get number of good guesses
  numGood <- length(which(examine$error <= 30))
  # get the percentage correct (accuracy)
  percGood <- (numGood/nrow(test_data))*100
  if(i != 1) {
    k$accuracy[i-1] <- percGood
  }
  else { # skipped k = 2
    k$accuracy[i] <- percGood
  }
}

# plot the accuracy versus k-value for all the k-values we tested
ggplot(data = k, aes(k, accuracy)) + geom_point() + geom_line(color = "red") + ggtitle("Accuracy of k-values") + 
  theme(plot.title = element_text(hjust = 0.5))

# determine the best k from those tested
best_k <- k$k[which.max(k$accuracy)]
# get the accuracy
k$accuracy[best_k-1]

# get the zip codes for the test set
test_zips <- sorted_seasons[(smp_ind+1):nrow(knn_data),]$Zip
# add the zip codes to the examine data set
examine$zips <- test_zips
# add a group variable so can tell if it was a good prediction or not
examine$group <- ifelse(examine$error >= 30, 1, 0)

# plot the error for the 2019-2020 season
ggplot(data = examine, aes(fill = factor(group), x = zips, y = error)) + 
  geom_bar(stat="identity") +
  ggtitle("Prediction Error for Season 2019-2020 with k = 52") +
  xlab("Zip") + ylab("% error") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.text=element_text(size=6)) +
  geom_hline(yintercept=30, linetype="dashed", color = "red", size = 1) +
  scale_fill_manual(name="Predictions",
                     labels=c("Good","Bad"),
                     values=c("royalblue4","red4"))

# plotting good and bad zips on the map
good_zips <- subset(data.frame(examine[examine$group == 0,]), select = c("zips"))
bad_zips <- subset(data.frame(examine[examine$group == 1,]), select = c("zips"))
par(mfrow=c(1,2))
gz_map <- zip.plot(good_zips, map.type = "county", region = "new jersey", col = "royalblue4", pch = 20, cex = 1.5) + title("Good Zips")
bz_map <- zip.plot(bad_zips, map.type = "county", region = "new jersey", col = "red4", pch = 20, cex = 1.5) + title("Bad Zips")
