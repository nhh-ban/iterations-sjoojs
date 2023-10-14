transform_metadata_to_df <- function(df){
  df[[1]] %>% 
    map(as_tibble) |> 
    list_rbind() |> 
    mutate(latestData = map_chr(latestData, 1, .default = ""))  |> 
    mutate(latestData = as_datetime(latestData, tz = "UTC"))  |> 
    mutate(location = map(location, unlist)) |>  
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}


# time function, task 4A
to_iso8601 <- function(datetime_variable, offset_days = 0){
  datetime_variable <- as_datetime(datetime_variable)
  offset_days <- days(x = offset_days)
  datetime_variable <- datetime_variable + offset_days
  result <- format(datetime_variable, "%Y-%m-%dT%H:%M:%SZ")
  print(result)
}
to_iso8601("2016-09-04 10:11:11",-4) #testing

# transform volumes
transform_volumes <- function(vol_qry) {
  # Extracting the relevant data
  edges <- vol_qry$data$trafficData$volume$byHour$edges
  
  # Transforming to a dataframe
  df <- data.frame(
    from = sapply(edges, function(x) x$node$from),
    to = sapply(edges, function(x) x$node$to),
    volume = sapply(edges, function(x) x$node$total$volumeNumbers$volume)
  )
  
  # Convert to appropriate data types
  df$from <- as.POSIXct(df$from, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  df$to <- as.POSIXct(df$to, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  df$volume <- as.numeric(df$volume)
  
  return(df)
}
