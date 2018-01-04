# Lab III-i
# TDDD01
# Anton Mo Eriksson
# 2017-12-18

setwd("/home/ame/git/tdde01/TDDE01/lab-3")
set.seed(1234567890)
library(geosphere)

stations <- read.csv("./stations.csv", fileEncoding = "Latin1")
temps <- read.csv("./temps50k.csv")
st <- merge(stations, temps, by="station_number")


# These three values are up to the students
h_distance <- 100000
h_date <- 20
h_time <- 2

# The point to predict Stockholm chosen (up to the students) 
lat <-59.34200  
long <- 18.05750

stockholm = c(long, lat)

date <- "2008-08-08" # The date to predict (up to the students)

times <- c("04:00:00", 
           "06:00:00",
           "08:00:00",
           "10:00:00", 
           "12:00:00",
           "14:00:00",
           "16:00:00", 
           "18:00:00",
           "20:00:00",
           "22:00:00",
           "24:00:00")
temp <- vector(length=length(times))
n = length(times)
prediction = matrix(nrow = n, ncol = 2)
# Students’ code here

measurments = st[as.Date(st$date) <= as.Date(date), ]

# From slides
#  Gaussian kernel: k(u) = exp(−∣∣u∣∣^2 ) where ∣∣ ⋅ ∣∣ is the Euclidean norm.
gaussian_kernel <- function(u) {
  return (exp(-(u^2)))
}

distance_kernel <- function(point_of_intrest, station) {
  distance = distHaversine(point_of_intrest, station)
  return (gaussian_kernel((distance / h_distance)))
}

day_kernel <- function(day_of_intrest, measurment_day) {
  day_diffrent = difftime(day_of_intrest, measurment_day, units = "days")
  day_diffrent = as.numeric(day_diffrent)
 
  day_diffrent = day_diffrent %% 365
  day_diffrent[day_diffrent > (365 / 2)] = 365 - day_diffrent[day_diffrent > (365 / 2)]
  
  return(gaussian_kernel((day_diffrent / h_date)))
}

hour_kernel <- function(hour_of_intrest, measure_hour) {
  
  hour_diffrent = as.numeric(difftime(strptime(hour_of_intrest, format = "%H:%M:%S"),
                           strptime(measure_hour,  format = "%H:%M:%S"),
                           units = "hour"))
  hour_diffrent = abs(hour_diffrent)
  hour_diffrent[hour_diffrent > 12] = 24 - hour_diffrent[hour_diffrent > 12]
  return(gaussian_kernel((hour_diffrent / h_time)))
}

calculate_kernels <- function(time_of_intrest) {
  
  station_distance = distance_kernel(stockholm, station_pos)
  #plot(station_distance, main = "Distance to Stockholm")
  
  date_distance = day_kernel(date, dates)
  
  time_distance = hour_kernel(time_of_intrest, time_hour)
  
  kernel_sum = station_distance + date_distance + time_distance
  kernel_multiplication = station_distance * date_distance * time_distance
  
  #Normalising...
  kernel_sum = (sum(kernel_sum * measurments$air_temperature) / sum(kernel_sum)) 
  kernel_multiplication = (sum(kernel_multiplication * measurments$air_temperature) / sum(kernel_multiplication))
  
  return (c(kernel_sum, kernel_multiplication))
}

index = 0
for (index in 1:n) {
  print(index)
  prediction[3] = calculate_kernels(times[3])
}
plot()

# Tests

station_pos = measurments[, c("longitude", "latitude")]
station_distance = distance_kernel(stockholm, station_pos)
plot(station_distance)

# Days 
dates = measurments$date
date_distance = day_kernel(date, dates)

# Times
for (i in 1:n) {
  time_hour = measurments$time
  time_distance = hour_kernel(times[i], time_hour)

  kernel_sum = station_distance + date_distance + time_distance
  kernel_multiplication = station_distance * date_distance * time_distance
  
  plot(kernel_sum, main = "Sum. Kernel", ylab = "Sum of three previus kernels")
  plot(kernel_multiplication, main = "Mul. Kernel", ylab = "Multiplication of three previus kernels")
  
  
  prediction_sum[i,] = (sum(kernel_sum * measurments$air_temperature) / sum(kernel_sum)) 
  prediction_mul[i,] = (sum(kernel_multiplication * measurments$air_temperature) / sum(kernel_multiplication))
}

plot(seq(4, 24, 2), prediction_sum[,1], "o", main = "Weather the 2008-08-08",
     ylab = "Temprature in Celsius", xlab = "Time of the day")
plot(seq(4, 24, 2), prediction_mul[,1], "o", main = "Weather the 2008-08-08",
     ylab = "Temprature in Celsius", xlab = "Time of the day") 
