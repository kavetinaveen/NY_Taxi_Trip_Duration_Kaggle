# ============================ Title: New York City Taxi Trip Duration ============================ #

# Setting working directory
# = = = = = = = = = = = = = =
filepath <- c("/Users/nkaveti/Documents/Kaggle/New York City Taxi Trip Duration")
setwd(filepath)

# Loading required package
# = = = = = = = = = = = = =
library(data.table)
library(dplyr)
library(plotly)
library(geosphere)
library(zipcode)

# Reading data
# = = = = = = = =
train <- fread("train_with_zipcode.csv")
test <- fread("test_with_zipcode.csv")

colnames(train)
# [1] "id"                 "vendor_id"          "pickup_datetime"    "dropoff_datetime"   "passenger_count"   
# [6] "pickup_longitude"   "pickup_latitude"    "dropoff_longitude"  "dropoff_latitude"   "store_and_fwd_flag"
# [11] "trip_duration"     

# Calculating trip distance using long/lat
train[, distance := distHaversine(c(pickup_longitude, pickup_latitude), c(dropoff_longitude, dropoff_latitude)), by = id]

test[, distance := distHaversine(c(pickup_longitude, pickup_latitude), c(dropoff_longitude, dropoff_latitude)), by = id]

# Finding location based features (Zipcode)
data("zipcode")
zipcode <- as.data.table(zipcode)
zipcode <- zipcode[state == "NY", ]

# findZipcode <- function(lat_long){
#   zipcode_dist <- data.table(id = train$id)
#   for(i in 1:nrow(zipcode)){
#     euc_dist <- cbind((lat_long$pickup_longitude - zipcode$longitude[i])^2, (lat_long$pickup_latitude - zipcode$latitude[i])^2)
#     euc_dist <- sqrt(rowSums(euc_dist))
#     zipcode_dist <- cbind(zipcode_dist, X = euc_dist)
#     colnames(zipcode_dist)[ncol(zipcode_dist)] <- as.character(zipcode$zip[i])
#   }
#   
#   return(zipcode_dist)
# }

findZipcode <- function(long_lat){
  euc_dist <- sort(sqrt((zipcode$longitude - long_lat[1])^2 + (zipcode$latitude - long_lat[2])^2), index = TRUE)
  return(zipcode$zip[euc_dist$ix[1]])
}

# zipcode_dist <- data.table(id = train$id)
# train_lat_long <- train[, c("pickup_longitude", "pickup_latitude")]
# strt <- Sys.time()
# for(i in 1:10){
#   # temp <- distm(train[, c("pickup_longitude", "pickup_latitude")], zipcode[i, c("longitude", "latitude")])
#   temp <- cbind((train_lat_long$pickup_longitude - unlist(zipcode[i, "longitude"]))^2, (train_lat_long$pickup_latitude - unlist(zipcode[i, "latitude"]))^2)
#   temp <- sqrt(rowSums(temp))
#   zipcode_dist <- cbind(zipcode_dist, X = temp)
#   colnames(zipcode_dist)[ncol(zipcode_dist)] <- as.character(zipcode$zip[i])
# }
# cat("Time taken: ", Sys.time() - strt, "\n")


strt <- Sys.time()
train[, zipcode_from := findZipcode(c(pickup_longitude, pickup_latitude)), by = id]
train[, zipcode_to := findZipcode(c(dropoff_longitude, dropoff_latitude)), by = id]
test[, zipcode_from := findZipcode(c(pickup_longitude, pickup_latitude)), by = id]
test[, zipcode_to := findZipcode(c(pickup_longitude, pickup_latitude)), by = id]
cat("Time taken: ", Sys.time() - strt, "\n")
fwrite(train, "train_with_zipcode.csv")
fwrite(test, "test_with_zipcode.csv")

# Removing outliers based on trip duration and distance
summary(train$trip_duration)
ub_td <- with(train, mean(trip_duration) + 3*sd(trip_duration))
lb_td <- with(train, mean(trip_duration) - 3*sd(trip_duration))
train <- train[trip_duration >= lb_td & trip_duration < ub_td, ]

ub_tdis <- with(train, mean(distance) + 3 * sd(distance))
lb_tdis <- with(train, mean(distance) - 3 * sd(distance))
train <- train[distance >= lb_tdis & distance < ub_tdis, ]

# Data pre-processing
# = = = = = = = = = = =
intersect(train$id, test$id) # There should be no intersection between IDs of train and test

prop_vendor_train <- as.data.table(prop.table(table(train$vendor_id)))
prop_vendor_test <- as.data.table(prop.table(table(test$vendor_id))) # Proportion of vendors in train and test is same

prop_vendor <- cbind(prop_vendor_train, prop_vendor_test[, -1])
colnames(prop_vendor) <- c("Vendor_ID", "Vendor % in train", "Vendor % in test") # Aggregating proportion of vendors from both train and test

p1 <- plot_ly(data = prop_vendor, x = ~Vendor_ID, y = ~`Vendor % in train`, type = "bar", name = "train") %>%  add_trace(y = ~`Vendor % in test`, name = "test") %>% layout(yaxis = list(title = 'Proportion'), barmode = 'group', xaxis = list(title = "Vendor ID")) # Plot of vendor proportion in both train and test
p1

mean_trip_dur_vid <- train[, .(mean_trip_duration = mean(trip_duration)), by = vendor_id]
p2 <- plot_ly(data = mean_trip_dur_vid, x = ~vendor_id, y = ~mean_trip_duration, type = "bar") %>% layout(yaxis = list(title = "Mean trip duration"), xaxis = list(title = "Vendor ID")) # Average trip duration by vendor level
p2

Freq_table_pc_v2 <- as.data.table(train[vendor_id == 2, table(passenger_count)])
colnames(Freq_table_pc_v2)[2] <- "Frequncy_V2"
Freq_table_pc_v1 <- as.data.table(train[vendor_id == 1, table(passenger_count)])
colnames(Freq_table_pc_v1)[2] <- "Frequncy_V1"
Freq_table_pc_v2 <- merge(Freq_table_pc_v2, Freq_table_pc_v1, by = "passenger_count", all.x = TRUE)
Freq_table_pc_v2$Frequncy_V1[is.na(Freq_table_pc_v2$Frequncy_V1)] <- 0

p3 <- plot_ly(data = Freq_table_pc_v2, x = ~passenger_count, y = ~Frequncy_V1, type = "bar", name = "Vendor 1") %>% add_trace(y = ~Frequncy_V2, name = "Vendor 2") %>%  layout(yaxis = list(title = "Frequency"), xaxis = list(title = "Passenger Count"))
p3

# Variable type conversions
train[, pickup_datetime := strptime(pickup_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
train[, dropoff_datetime := strptime(dropoff_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]

train[, store_and_fwd_flag := as.factor(store_and_fwd_flag)]
test[, store_and_fwd_flag := as.factor(store_and_fwd_flag)]

# Date-time related features generation
train[, month := month(pickup_datetime)]
train[, dow := weekdays(pickup_datetime)]
train[, hour := hour(pickup_datetime)]
train[, minute := minute(pickup_datetime)]
train[, second := second(pickup_datetime)]

test[, month := month(pickup_datetime)]
test[, dow := weekdays(pickup_datetime)]
test[, hour := hour(pickup_datetime)]
test[, minute := minute(pickup_datetime)]
test[, second := second(pickup_datetime)]

# Trip count and duration on month level 
trip_count_mon <- train[, .(trip_count = length(id)), by = .(vendor_id, month)]
trip_count_mon <- trip_count_mon[!is.na(month), ]

trip_count_mon$month <- factor(trip_count_mon$month, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))

p4 <- plot_ly(data = trip_count_mon[vendor_id == 1, ],  x = ~month, y = ~trip_count, type = "bar", name = "Vendor 1") %>% add_trace(y = trip_count_mon$trip_count[trip_count_mon$vendor_id == 2], name = "Vendor 2") %>% layout(xaxis = list(title = "Month"), yaxis = list(title = "Trip Count"))
p4

mean_trip_dur_mon <- train[, .(mean_trip_dur = mean(trip_duration)), by = .(vendor_id, month)]
mean_trip_dur_mon <- mean_trip_dur_mon[!is.na(month), ]

mean_trip_dur_mon$month <- factor(mean_trip_dur_mon$month, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))

p5 <- plot_ly(data = mean_trip_dur_mon[vendor_id == 1, ],  x = ~month, y = ~mean_trip_dur, type = "bar", name = "Vendor 1") %>% add_trace(y = mean_trip_dur_mon$mean_trip_dur[mean_trip_dur_mon$vendor_id == 2], name = "Vendor 2") %>% layout(xaxis = list(title = "Month"), yaxis = list(title = "Mean Trip Duration"))
p5

# Trip count and duration on dow level
trip_count_dow <- train[, .(trip_count = length(id)), by = .(vendor_id, dow)]
trip_count_dow <- trip_count_dow[!is.na(dow), ]

p6 <- plot_ly(data = trip_count_dow[vendor_id == 1, ],  x = ~dow, y = ~trip_count, type = "bar", name = "Vendor 1") %>% add_trace(y = trip_count_dow$trip_count[trip_count_dow$vendor_id == 2], name = "Vendor 2") %>% layout( yaxis = list(title = "Trip Count"), xaxis = list(title = "Day of week"))
p6

mean_trip_dur_dow <- train[, .(mean_trip_dur = mean(trip_duration)), by = .(vendor_id, dow)]
mean_trip_dur_dow <- mean_trip_dur_dow[!is.na(dow), ]

p7 <- plot_ly(data = mean_trip_dur_dow[vendor_id == 1, ],  x = ~dow, y = ~mean_trip_dur, type = "bar", name = "Vendor 1") %>% add_trace(y = mean_trip_dur_dow$mean_trip_dur[mean_trip_dur_dow$vendor_id == 2], name = "Vendor 2") %>% layout(xaxis = list(title = "Day of week"), yaxis = list(title = "Mean Trip Duration"))
p7

# Trip count and duration on dow and hour level
trip_count_dow_hour <- train[, .(trip_count = length(id)), by = .(dow, hour)]
trip_count_dow_hour <- trip_count_dow_hour[!is.na(dow), ]

trip_count_dow_hour <- dcast(trip_count_dow_hour, ... ~dow, value.var = "trip_count")
p8 <- plot_ly(data = trip_count_dow_hour,  x = ~hour, y = ~Monday, type = "scatter", mode = "lines", name = "Monday") %>% add_trace(y = ~Tuesday, name = "Tuesday") %>% add_trace(y = ~Wednesday, name = "Wednesday") %>% add_trace(y = ~Thursday, name = "Thursday") %>% add_trace(y = ~Friday, name = "Friday") %>% add_trace(y = ~Saturday, name = "Saturday") %>% add_trace(y = ~Sunday, name = "Sunday")
p8

trip_dur_dow_hour <- train[, .(mean_trip_dur = mean(trip_duration)), by = .(dow, hour)]
trip_dur_dow_hour <- trip_dur_dow_hour[!is.na(dow), ]

trip_dur_dow_hour <- dcast(trip_dur_dow_hour, ... ~dow, value.var = "mean_trip_dur")
p9 <- plot_ly(data = trip_dur_dow_hour,  x = ~hour, y = ~Monday, type = "scatter", mode = "lines", name = "Monday") %>% add_trace(y = ~Tuesday, name = "Tuesday") %>% add_trace(y = ~Wednesday, name = "Wednesday") %>% add_trace(y = ~Thursday, name = "Thursday") %>% add_trace(y = ~Friday, name = "Friday") %>% add_trace(y = ~Saturday, name = "Saturday") %>% add_trace(y = ~Sunday, name = "Sunday")
p9






take_sample <- sample(nrow(train_sub), round(nrow(train_sub)*0.05))
with(train_sub, plot(trip_duration[take_sample], distance[take_sample]))

p4 <- plot_ly(data = train_sub[take_sample], x = ~trip_duration, y = ~distance, type = "scatter", mode = "points") %>% layout(xaxis = list(title = "Trip Duration"), yaxis = list(title = "Distance"))
p4


# Average trip durations of Zipcode from
mean_dur_zip_from_train <- train[, mean(trip_duration), by = zipcode_from]
mean_dur_zip_from_train <- merge(mean_dur_zip_from_train, zipcode, by.x = "zipcode_from", by.y = "zip", all.x = TRUE)

pal <- brewer.pal(11, "RdYlGn")
pal <- colorRampPalette(pal)
palData <- classIntervals(mean_dur_zip_from_train$V1, style="quantile")
mean_dur_zip_from_train$colors <- findColours(palData, pal(11))

leaflet(mean_dur_zip_from_train) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(mean(mean_dur_zip_from_train$longitude), mean(mean_dur_zip_from_train$latitude), zoom = 10) %>%
  addCircleMarkers(~longitude, ~latitude, weight = 4, radius = 2, opacity = 1, color = ~colors, fillColor = ~colors)

# Average trip durations of Zipcode to in train data 
mean_dur_zip_to_train <- train[, mean(trip_duration), by = zipcode_to]
mean_dur_zip_to_train <- merge(mean_dur_zip_to_train, zipcode, by.x = "zipcode_to", by.y = "zip", all.x = TRUE)

pal <- brewer.pal(11, "RdYlGn")
pal <- colorRampPalette(pal)
palData <- classIntervals(mean_dur_zip_to_train$V1, style="quantile")
mean_dur_zip_to_train$colors <- findColours(palData, pal(11))

leaflet(mean_dur_zip_to_train) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(mean(mean_dur_zip_to_train$longitude), mean(mean_dur_zip_to_train$latitude), zoom = 10) %>%
  addCircleMarkers(~longitude, ~latitude, weight = 4, radius = 2, opacity = 1, color = ~colors, fillColor = ~colors)


# Average trip durations of Zipcode from in test data
mean_dur_zip_to_test <- test[, mean(trip_duration), by = zipcode_to]
mean_dur_zip_to_test <- merge(mean_dur_zip_to_test, zipcode, by.x = "zipcode_to", by.y = "zip", all.x = TRUE)

pal <- brewer.pal(11, "RdYlGn")
pal <- colorRampPalette(pal)
palData <- classIntervals(mean_dur_zip_to_test$V1, style="quantile")
mean_dur_zip_to_test$colors <- findColours(palData, pal(11))

leaflet(mean_dur_zip_to_test) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(mean(mean_dur_zip_to_test$longitude), mean(mean_dur_zip_to_test$latitude), zoom = 10) %>%
  addCircleMarkers(~longitude, ~latitude, weight = 4, radius = 2, opacity = 1, color = ~colors, fillColor = ~colors)


freq_trips <- train[, .(num_trips = sum(dummy)), by = .(zipcode_from, zipcode_to)]
freq_trips <- freq_trips[order(num_trips, decreasing = TRUE), ]
freq_trips$num_trips




