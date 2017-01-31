library(jsonlite)

# Input file
filename  <- "D:/Atelier/data_all_Paris.jjson_2016-12-01-1480569957.gz"

# Load from json
list_data <- fromJSON(sprintf("[%s]", paste(readLines(filename), collapse=",")))

# Store list as data frame
data1 <- do.call('rbind',list_data)


# Process variables
data2 <- data1
data2$date_time <- as.POSIXct(data1$last_update/1000, 
                                origin="1970-01-01")

data2$station   <- as.factor(paste(data2$contract_name, data2$number, sep ="_"))
data2           <- data2[, c("station",
                             "status",
                             "date_time",
                             "bike_stands", 
                             "available_bike_stands",
                             "available_bikes")]

# Order and convert to time-series by station
data2ts <- function(df,time_step)
{
  print("------")
  print(paste("Processing", df$station[1], "station"))
  # Initiate time-series
  time_series = data.frame()
  
  # Initiate time 
  date_time = as.POSIXct(floor(as.numeric(df$date_time[1])/time_step)*time_step, 
                         origin="1970-01-01")
  # Initiate value
  series = df[1, 3:5]
  
  # Store initial time and value
  time_series = rbind(time_series, cbind(date_time, series))
  
  # Propagate time-series
  for (i in (2:nrow(df)))
  { 
    # Convert raw-data time as time-series measurement
    new_time = as.POSIXct(floor(as.numeric(df$date_time[i])/time_step)*time_step, 
                          origin="1970-01-01")
    # If expected time : update value for future storage...
    if (new_time == date_time) 
    { 
      series = df[i,3:5]
    }
    # ... otherwise store same values as long as needed
    else
    { 
      while(new_time > date_time)
      {
        # Update time
        date_time = as.POSIXct(date_time + time_step, 
                               origin="1970-01-01")
        # Store unchanged value
        time_series = rbind(time_series, cbind(date_time, series))
      } 
      # Update value for future storage
      series = df[i,3:5]
    }
  }
  print(paste(nrow(time_series), "obs. in time series"))
  return(time_series)
}

# Make hourly time-series
stations <- split(data2, as.factor(data2$station))
stations <- lapply(stations, function(x){x[order(x$date_time),]})
ts_list  <- lapply(stations, data2ts, 3600)  

# Compute availability ratio
ts_list2 <- lapply(ts_list,
                   function(x) 
                     {x$avl_ratio <- (100 *(1- x$available_bike_stands / x$bike_stands))})

# Store as matrix and filter incomplete time-series
data_ts <- do.call('cbind',ts_list2[which(unlist(sapply(ts_list2,length)) == 721)])

# Add date_time as row name
date_time <- as.character(sort(unique(ts_list[[1]][, 1])))
rownames(data_ts) <- date_time
mat_ts <- t(data_ts)

# Save to directory
saveRDS(object = mat_ts, file= "D:/Atelier/mat_ts.rds")
