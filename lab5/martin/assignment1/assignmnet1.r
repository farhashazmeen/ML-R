set.seed(1234567890)
library(geosphere)
library(ggplot2)

# Read the data files
stations <- read.csv("stations.csv", stringsAsFactors=FALSE)
temps <- read.csv("temps50k.csv", stringsAsFactors=FALSE)
st <- merge(stations,temps,by="station_number")

# Filter only interesting features
st = st[,c("longitude", "latitude" , "date", "time", "air_temperature")]

# To reduce time
#ind <- sample(1:50000, 5000)
#st <- st[ind,]

# Weights for ech of the kernels
h_distance = 1000000
h_time = 2
h_date = 3

# Gaussian functiondistance function
gaussian = function(v){
  return(exp(-v^2))
}

# Distance kernel for long and lat
kernel.distance = function(data_pos, obs_pos){
  # Use haversine distance
  dist = distHaversine(obs_pos,data_pos) 
  return(gaussian(dist/ h_distance))
}

# Distance kernel for date
kernel.date_distance = function(data_date, obs_date){
  # Use diff in days
  dist = as.numeric(difftime(obs_date, data_date, units = "days")) 
  dist = abs(dist)
  dist[dist > 182] = 365 - dist[dist > 182]
  #print(dist)
  return(gaussian(dist  / h_date))
}

# Distance kernel for time
kernel.time_distance = function(data_time,obs_time){
  # Use diff in hours
  dist = as.numeric(difftime(obs_time, data_time, units = "hours"))
  dist = abs(dist)
  dist[dist > 12] = 24 - dist[dist > 12]
  return(gaussian(dist/ h_time))
}

# Kernel function, serves as wrapper for distance kernels
kernel = function(data, observation, index){
  data_fixed = fix_time(data)
  observation = fix_time(observation)
  
  # Etract feature vectors
  data_pos  = data_fixed[,c("longitude", "latitude")]
  obs_pos   = c(observation$longitude, observation$latitude)
  data_date = data_fixed$date
  obs_date  = observation$date
  data_time = data_fixed$time
  obs_time  = observation$time
  
  # Calcualte kernels
  dist_pos = kernel.distance(obs_pos,data_pos)
  dist_date = kernel.date_distance(obs_date,data_date)
  dist_time = kernel.time_distance(obs_time,data_time)
  dist = dist_pos + dist_date + dist_time
  data_fixed$distance = dist
  data_fixed$dist_pos = dist_pos
  data_fixed$dist_date = dist_date
  data_fixed$dist_time = dist_time

  # Plot distance kernels
  setEPS()
  postscript(paste(index,sep="","_dist.eps"))
  plot_sample = data_fixed$dist_pos
  plot(1:length(plot_sample),plot_sample, ylab = "long/lat", xlab = "Index")
  dev.off()
  
  setEPS()
  postscript(paste(index,sep="","_date.eps"))
  plot_sample = data_fixed[order(data_fixed$date),]$dist_date
  plot(1:length(plot_sample),plot_sample, ylab = "Date (days)", xlab = "Index")
  dev.off()
  
  setEPS()
  postscript(paste(index,sep="","_time.eps"))
  plot_sample = data_fixed[order(data_fixed$time),]$dist_time
  plot(1:length(plot_sample),plot_sample, ylab = "Time (hours)", xlab = "Index")
  dev.off()
  
  # Pick the N best observations
  #n = nrow(data)
  selection = data_fixed#data[order(data$distance, decreasing = TRUE),][n,]

  # Return the mean temperature over the picked obsrvations
  return(sum(selection$distance * selection$air_temperature) / sum(selection$distance))
}

# Make usre time and date are in propper formats
fix_time = function(data){
  data$time = as.POSIXct(data$time,format="%H:%M:%S")
  data$date = sub('\\d{4}(?=-)', '2016', data$date, perl=TRUE)
  return(data)
}

# Set observation featuers
a <- 58.4274
b <- 14.826
date <- c("2016-07-12")
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
n = length(time)
temp <- vector(length=length(date))

# # Students' code here

data = data.frame(date=rep(date,n/length(date)), time=rep(times,n/length(date)), longitude=rep(a,n), latitude=rep(b,n))
for(i in 1:nrow(data)){
  temp[i]  = kernel(st, data[i,],i)
}

print(temp)
setEPS()
postscript(paste("result",sep=".","eps"))
plot(temp, type="o")
dev.off()
