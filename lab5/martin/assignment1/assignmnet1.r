set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", stringsAsFactors=FALSE)
temps <- read.csv("temps50k.csv", stringsAsFactors=FALSE)
st <- merge(stations,temps,by="station_number")

st = st[,c("longitude", "latitude" , "date", "time", "air_temperature")]
# To reduce time
#ind <- sample(1:50000, 5000)

#st <- st[ind,]

h_distance = 1000000
h_time = 2
h_date = 3

gaussian = function(v){
  return(exp(-v^2))
}

kernel.distance = function(data_pos, obs_pos){
  dist = distHaversine(obs_pos,data_pos) 
  return(gaussian(dist/ h_distance))
}

kernel.date_distance = function(data_date, obs_date){
  dist = as.numeric(difftime(obs_date, data_date, units = "days")) 
  dist = abs(dist)
  return(gaussian(dist  / h_date))
}

kernel.time_distance = function(data_time,obs_time){
  dist = as.numeric(difftime(obs_time, data_time, units = "hours"))
  dist = abs(dist)
  return(gaussian(dist/ h_time))
}

kernel = function(data, observation){
  data_fixed = fix_time(data)
  observation = fix_time(observation)
  
  data_pos  = data_fixed[,c("longitude", "latitude")]
  obs_pos   = c(observation$longitude, observation$latitude)
  data_date = data_fixed$date
  obs_date  = observation$date
  data_time = data_fixed$time
  obs_time  = observation$time
  
  dist_pos = kernel.distance(obs_pos,data_pos)
  dist_date = kernel.date_distance(obs_date,data_date)
  dist_time = kernel.time_distance(obs_time,data_time)
  dist = dist_pos + dist_date + dist_time
  data$distance = dist
  data$dist_pos = dist_pos
  data$dist_date = dist_date
  data$dist_time = dist_time
  
  plot_sample = data$dist_pos
  plot(1:length(plot_sample),plot_sample)
  
  plot_sample = data[order(data$date),]$dist_date
  plot(1:length(plot_sample),plot_sample)
  
  plot_sample = data[order(data$time),]$dist_time
  plot(1:length(plot_sample),plot_sample)
  
  return(sum(data$distance * data$air_temperature) / sum(data$distance))
}

fix_time = function(data){
  data$time = as.POSIXct(data$time,format="%H:%M:%S")
  data$date = sub('\\d{4}(?=-)', '2016', data$date, perl=TRUE)
  return(data)
}

a <- 58.4274
b <- 14.826
date <- "2013-06-24"
times <- c("12:00:00")
n = length(date)
temp <- vector(length=length(date))

# # Students' code here


data = data.frame(date=rep(date,n), time=times, longitude=rep(a,n), latitude=rep(b,n))
for(i in 1:nrow(data)){
  temp[i]  = kernel(st, data[i,])
}
print(temp)

plot(temp, type="o")