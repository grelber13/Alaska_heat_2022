ghcnd_stations <- function (refresh = FALSE, ...) 
{
  assert(refresh, "logical")
  stopifnot(length(refresh) == 1)
  sta <- get_stations(refresh, ...)
  inv <- get_inventory(refresh, ...)
  df <- merge(sta, inv[, -c(2, 3)], by = "id")
  tibble::as_tibble(df[stats::complete.cases(df), ])
}
get_stations <- function(refresh = FALSE, ...) {
  ff <- paste("/home/grelber/VA data/temp_GHCND_data", "ghcnd-stations.txt",sep="/")
  ffrds <- paste("/home/grelber/VA data/temp_GHCND_data", "ghcnd-stations.rds",sep="/")
  if (file.exists(ffrds) && !refresh) {
    cache_mssg(ffrds)
    return(readRDS(ffrds))
  } else {
    if (file.exists(ff)) unlink(ff)
    if (file.exists(ffrds)) unlink(ffrds)
    res <- GET_retry(
      "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt",
      disk = ff, ...)
    df <- read.fwf(as_tc_p(res),
                   widths = c(11, 9, 11, 7, 2, 31, 5, 10),
                   header = FALSE, strip.white = TRUE, comment.char = "",
                   stringsAsFactors = FALSE)
    nms <- c("id","latitude", "longitude", "elevation",
             "state", "name", "gsn_flag", "wmo_id")
    df <- stats::setNames(df, nms)
    saveRDS(df, file = ffrds)
    unlink(ff)
    return(df)
  }
}

get_inventory <- function(refresh = FALSE, ...) {
  gg <- paste("/home/grelber/VA data/temp_GHCND_data", "ghcnd-inventory.txt",sep="/")
  ggrds <- paste("/home/grelber/VA data/temp_GHCND_data", "ghcnd-inventory.rds",sep="/")
  if (file.exists(ggrds) && !refresh) {
    cache_mssg(ggrds)
    return(readRDS(ggrds))
  } else {
    if (file.exists(gg)) unlink(gg)
    if (file.exists(ggrds)) unlink(ggrds)
    res <- GET_retry(
      "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt",
      disk = gg, ...)
    df <- read.fwf(as_tc_p(res),
                   widths = c(11, 9, 10, 5, 5, 5),
                   header = FALSE, strip.white = TRUE, comment.char = "",
                   stringsAsFactors = FALSE)
    nms <- c("id","latitude", "longitude",
             "element", "first_year", "last_year")
    df <- stats::setNames(df, nms)
    saveRDS(df, file = ggrds)
    unlink(gg)
    return(df)
  }
}

GET_retry <- function(url, ..., times = 3) {
  cliret <- crul::HttpClient$new(url)
  res <- suppressWarnings(cliret$get(...))
  if (res$status_code > 226) {
    message("Request failed - Retrying")
    stat <- 500
    i <- 0
    while (stat > 226 && i <= times) {
      i <- i + 1
      res <- suppressWarnings(cliret$get(...))
      stat <- res$status_code
    }
    if (res$status_code > 226) stop("Request failed, try again", call. = FALSE)
  }
  return(res)
}
as_tc_p <- function(x) textConnection(x$parse("latin1"))

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

roenv <- new.env()
roenv$cache_messages <- TRUE

rnoaa_options <- function(cache_messages = TRUE) {
  roenv$cache_messages <- cache_messages
  return(NULL)
}

stract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
cache_mssg <- function(file) {
  if (roenv$cache_messages) {
    fi <- file.info(file)
    if (NROW(fi) > 1) {
      message("in directory: ", dirname(file[1]))
      to_get <- min(c(3, length(file)))
      ss <- file[seq_len(to_get)]
      ss_str <- paste0(basename(ss), collapse = ", ")
      if (to_get < length(file)) ss_str <- paste0(ss_str, " ...")
      message("using cached files (first 3): ", ss_str)
      message(
        sprintf("[%s] date created: %s",
                basename(row.names(fi)[1]), fi[1,"ctime"]))
    } else {
      if (!fi$isdir) {
        size <- round(fi$size/1000000, 3)
        chaftdec <- nchar(stract(as.character(size), '^[0-9]+'))
        if (chaftdec > 1) size <- round(size, 1)
        message("using cached file: ", file)
        message(
          sprintf("date created (size, mb): %s (%s)", fi$ctime, size))
      } else {
        message("using cached directory: ", file)
        message(
          sprintf("date created: %s", fi$ctime))
      }
    }
  }
}

meteo_nearby_stations<-function (lat_lon_df, lat_colname = "latitude", lon_colname = "longitude", 
          station_data = ghcnd_stations(), var = "all", year_min = NULL, 
          year_max = NULL, radius = NULL, limit = NULL) 
{
  lat_lon_df <- as.data.frame(lat_lon_df)
  var <- tolower(var)
  lat_lon_df$id <- as.character(lat_lon_df$id)
  lat_lon_df[, lat_colname] <- as.numeric(as.character(lat_lon_df[, 
                                                                  lat_colname]))
  lat_lon_df[, lon_colname] <- as.numeric(as.character(lat_lon_df[, 
                                                                  lon_colname]))
  if (is.null(year_min)) 
    year_min <- min(station_data$first_year, na.rm = TRUE)
  if (is.null(year_max)) 
    year_max <- max(station_data$last_year, na.rm = TRUE)
  if (length(var) == 1 && var == "all") 
    var <- unique(station_data$element)
  dots <- list(~last_year >= year_min & first_year <= year_max & 
                 element %in% toupper(var) & !is.na(element))
  station_data <- dplyr::filter_(station_data, .dots = dots) %>% 
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
