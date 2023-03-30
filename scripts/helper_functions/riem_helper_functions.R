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
    dplyr::select(id, name, latitude, longitude) %>% 
    dplyr::distinct()
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

#'Write function to clean `riem_measures` output 
getmode <- function(v) {
  uniqv <- unique(v)[which(!is.na(unique(v)))]
  uniqv[which.max(tabulate(match(v[which(!is.na(v))], uniqv)))]
}

pull_riem <- function(test_station_ID,
                      start_date,
                      end_date) {
  if(nchar(str_split(start_date,"-")[[1]][1])!=4 ||
     nchar(str_split(start_date,"-")[[1]][2])!=2 ||
     nchar(str_split(start_date,"-")[[1]][3])!=2) {
    stop("Dates should be character strings in 'YYYY-mm-dd' format.")
  }
  if(nchar(str_split(end_date,"-")[[1]][1])!=4 ||
     nchar(str_split(end_date,"-")[[1]][2])!=2 ||
     nchar(str_split(end_date,"-")[[1]][3])!=2) {
    stop("Dates should be character strings in 'YYYY-mm-dd' format.")
  }
  hours <- seq(as.POSIXct(paste0(start_date," 00:00:00")),
               as.POSIXct(paste0(end_date," 23:00:00")),
               by="hour")
  hours_df <- data.frame(valid=hours) %>%
    mutate(date=as.Date(substr(valid,1,10),tryFormats=c("%Y-%m-%d")),
           hour_char=substr(valid,1,14))
  hours_df <- hours_df %>%
    dplyr::select(-valid)
  hours_df$station=test_station_ID
  
  riem_df <- riem_measures(station = test_station_ID, 
                           date_start = as.Date(min(hours_df$date)),
                           date_end = as.Date(max(hours_df$date))) 
  if(!is.null(riem_df)) {
    riem_df <- riem_df %>%
      mutate(date=as.Date(valid,tryFormats=c("%Y-%m-%d %H:%M:%S"))) %>%
      dplyr::select(station,date,tmpf,relh,valid) %>%
      mutate(hour_char=substr(valid,1,14))
    if (nrow(riem_df)>0) {
      riem_df <- right_join(riem_df,hours_df,
                            by=c("station","date","hour_char")) %>%
        arrange(date,hour_char)
      
      riem_hr_df <- riem_df %>%
        mutate(HI_test = ifelse(!is.na(tmpf)&!is.na(relh),
                                ((0.5 * (tmpf+61 + ((tmpf-68)*1.2) + (relh*0.094)))+tmpf)/2,NA)) %>%
        mutate(hr_HI_temp=ifelse(HI_test>80,
                                 -42.379 + 2.04901523*tmpf + 10.14333127*relh - .22475541*tmpf*relh - .00683783*tmpf*tmpf - .05481717*relh*relh + .00122874*tmpf*tmpf*relh + .00085282*tmpf*relh*relh - .00000199*tmpf*tmpf*relh*relh,
                                 0.5*(tmpf+61+((tmpf-68)*1.2)+(relh*0.094)))) %>%
        mutate(hr_HI=ifelse(HI_test<=80,
                            hr_HI_temp,
                            ifelse(relh<13 & tmpf<112,
                                   hr_HI_temp-(((13-relh)/4)*(((17-abs(tmpf-95))/17)^(1/2))),
                                   ifelse(relh>85 & tmpf<87,
                                          hr_HI_temp+(((relh-85)/10)*((87-tmpf)/5)),
                                          hr_HI_temp)))) %>%
        group_by(station,hour_char,date) %>%
        filter(hr_HI==max(hr_HI,na.rm=T)) %>%
        rename(hr_tmpf=tmpf,
               hr_relh=relh) %>%
        dplyr::select(-HI_test,-hr_HI_temp)
      
      riem_day_df <- riem_hr_df %>%
        mutate(hr_meas_tmpf=ifelse(is.na(hr_tmpf),0,1),
               hr_meas_relh=ifelse(is.na(hr_relh),0,1),
               hr_meas_HI=ifelse(is.na(hr_HI),0,1)) %>%
        group_by(station,date) %>%
        summarise(day_tmpf=max(hr_tmpf,na.rm=T),
                  day_relh=mean(hr_relh,na.rm=T),
                  day_HI=max(hr_HI,na.rm=T),
                  n_hr_meas_tmpf=sum(hr_meas_tmpf),
                  n_hr_meas_relh=sum(hr_meas_relh),
                  n_hr_meas_HI=sum(hr_meas_HI),
                  .groups='drop')
      
      riem_day_df <- riem_day_df %>%
        mutate(less_18hr_meas_tmpf=ifelse(n_hr_meas_tmpf<18,1,0),
               less_18hr_meas_relh=ifelse(n_hr_meas_relh<18,1,0),
               less_18hr_meas_HI=ifelse(n_hr_meas_relh<18,1,0),
               less_12hr_meas_tmpf=ifelse(n_hr_meas_tmpf<12,1,0),
               less_12hr_meas_relh=ifelse(n_hr_meas_relh<12,1,0),
               less_12hr_meas_HI=ifelse(n_hr_meas_relh<12,1,0)) %>%
        rename(id=station)
    } else {riem_day_df <- riem_df}
  } else {
    print(paste0("FYI, there were no data for ",
                 test_station_ID,
                 " for the specified dates."))
    riem_day_df <- data.frame(id=test_station_ID)
  }
  return(riem_day_df)
}

