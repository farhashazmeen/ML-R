set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

gaussian = function(v){
  return(exp(-v^2))
}

kernel.distance = function(data,observation){
  max = 1000
  #print(c(observation$longitude,observation$latitude))
  #print(c(data$longitude, data$latitude))
  dist = distHaversine(c(observation$longitude,observation$latitude), c(data$longitude, data$latitude)) / max
  #print(dist)
  return(gaussian(dist))
}

kernel.date_distance = function(data, observation){
  max = 8
  dist = as.numeric(difftime(observation$date, data$date, units = "days")) / max
 # print(dist)
  return(gaussian(dist))
}

kernel.time_distance = function(data,observation){
  max = 4
  dist = as.numeric(difftime(observation$time, data$time, units = "hours")) / max
 # print(dist)
  return(gaussian(dist))
}


kernel.method = function(data, observation){
  dist1 = kernel.distance(data,observation)
  dist2 = kernel.date_distance(data,observation)
  dist3 = kernel.time_distance(data,observation)
  return(dist1+dist2+dist3)
}

kernel = function(data, observation){
  data = fix_time(data)
  observation = fix_time(observation)
  dist = apply(data,1,function(x){
    d = data.frame(latitude=as.numeric(x["latitude"]), longitude=as.numeric(x["longitude"]), date=(x["date"]), time=(x["time"]))
    return(kernel.method(d,observation))
  })
 # print(dist)
  best = which.min(dist)
  print(data[best,]$air_temperature)
  return(dist)
}

fix_time = function(data){
  data$time = as.POSIXct(data$time,format="%H:%M:%S")
  return(data)
}

h_distance = 1000
h_time = 6
h_date = 9

a <- 58.4274
b <- 14.826
date <- "2013-11-04"
times <- c("04:00:00", "06:00:00", "24:00:00")
n = length(times)
temp <- vector(length=length(times))

# # Students' code here
data = data.frame(date=rep(date,n), time=times, longitude=rep(a,n), latitude=rep(b,n))

t1 = data.frame(latitude=a, longitude=b,date=date, time=times[1])
data_h = head(st)
print(t1)
#print(st[1,])
print(head(st))
print(kernel(st,t1))

plot(temp, type="o")