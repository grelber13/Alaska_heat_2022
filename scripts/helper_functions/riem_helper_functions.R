sort_list_df <- function(df_l) {
  out <- do.call(cbind, df_l)
  
  name_of_dfs <- names(df_l)
  order <- c(1:length(name_of_dfs))
  sequence_of_dfs <- match(order,name_of_dfs)
  out_l <- list()
  for(i in 1:length(name_of_dfs))
  {
    out_l[[i]] <- df_l[[sequence_of_dfs[i]]]
  }
  
  names(out_l) <- order
  return(out_l)
}

nearby_ASOS_stations <- function (lat_lon_df, lat_colname = "latitude", lon_colname = "longitude", 
          station_data, radius = NULL, limit = NULL) 
{
  lat_lon_df <- as.data.frame(lat_lon_df)
  lat_lon_df$id <- as.character(lat_lon_df$id)
  lat_lon_df[, lat_colname] <- as.numeric(as.character(lat_lon_df[,lat_colname]))
  lat_lon_df[, lon_colname] <- as.numeric(as.character(lat_lon_df[,lon_colname]))
  
  station_data <- station_data %>% 
    dplyr::select_(~id, ~name, ~latitude, ~longitude) %>% 
    dplyr::distinct_()
  location_stations <- as.data.frame(lat_lon_df) %>% 
    split(.[,"id"]) %>% purrr::map(function(x) {
      station_ids <- meteo_distance(station_data = station_data, 
                                    lat = x[, lat_colname], 
                                    long = x[, lon_colname], 
                                    radius = radius, limit = limit)
      return(station_ids)
    })
  return(location_stations)
}
