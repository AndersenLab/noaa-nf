#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(lubridate)
library(tidyverse)
library(stationaRy)
library(geosphere)

# args[1] = input dataframe
# args[2] = station dataframe
# args[3] = time_period
# args[4] = number of events per month
# args[5] = important trait
# args[6] = all_data

df <- readr::read_tsv(args[1])
time_period <- as.numeric(args[3])
events <- as.numeric(args[4])
all_data <- args[6]
important_trait <- args[5]

# download station data
station_inventory <- read.csv(args[2]) %>%
    dplyr::mutate(id = paste(USAF, WBAN, sep = "-"))

if(important_trait == "NULL") important_trait <- NULL
if(all_data == "NULL") all_data <- NULL

#update wi for start/end dates
days <- (time_period*30)/2 #use 30 days to represent a month

wi <- df %>%
    dplyr::mutate(isolation_date = mdy(as.character(isolation_date)),
                  start_date = isolation_date - ddays(days),
                  end_date = isolation_date + ddays(days))

#Check if you need to download more than one year
total_years <- year(wi$end_date[1]) - year(wi$start_date[1]) + 1

# filter out stations that have few recordings per month and keep stations with correct number of years of data
st <- station_inventory %>%
    dplyr::filter(YEAR >= year(wi$start_date[1]), YEAR <= year(wi$end_date[1])) %>%
    tidyr::gather(month, recordings, JAN:DEC) %>%
    dplyr::mutate(keep = ifelse(recordings > events, TRUE, FALSE)) %>%
    tidyr::spread(month, recordings) %>%
    dplyr::filter(keep == TRUE) %>%
    na.omit() %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(num_years = n()) %>%
    dplyr::filter(num_years == total_years)

# if no stations, enter NA
if(nrow(st) == 0) {
    print("Error. No station data avaiable for this time period. Entering 'NA'.")
    wi <- wi %>%
        dplyr::mutate(nearest_station = NA,
                      station_distance = NA,
                      station_lat = NA,
                      station_lon = NA,
                      station_elev = NA,
                      trait = NA,
                      value = NA)
} else {
    # Filter stations based on location and year with data, find closest station
    stations <- stationaRy::get_isd_stations(lower_lat = wi$latitude[1] - 10,
                                 upper_lat = wi$latitude[1] + 10,
                                 lower_lon = wi$longitude[1] - 10,
                                 upper_lon = wi$longitude[1] + 10) %>%
        dplyr::mutate(id = paste(usaf, wban, sep = "-")) %>%
        dplyr::filter(id %in% st$id) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dist_to_worm = geosphere::distm(c(wi$longitude[1], wi$latitude[1]), c(lon, lat))[1]) %>%
        dplyr::arrange(dist_to_worm)
    
    # assign nearest station and station distance to worm
    wi <- wi %>%
        dplyr::mutate(nearest_station = stations$id[1],
                      station_distance = stations$dist_to_worm[1]/1000)
    
    #Get station data using stationaRy package
    station_data <- stationaRy::get_isd_station_data(station_id = wi$nearest_station[1], 
                                                            startyear = year(wi$start_date[1]), 
                                                            endyear = year(wi$end_date[1]),
                                                            select_additional_data = all_data) %>%
        dplyr::mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
        dplyr::filter(date >= wi$start_date[1], date <= wi$end_date[1]) %>%
        dplyr::select(lat:date, -time, -date)
    
    #If there is no data for important_variable, choose another station
    if(!is.null(important_trait)) {
        count <- 2
        while(length(which(!is.na(station_data[[important_trait]]))) == 0) {
            # assign nearest station and station distance to worm
            wi <- wi %>%
                dplyr::mutate(nearest_station = stations$id[count],
                              station_distance = stations$dist_to_worm[count]/1000)
            
            #Get station data using stationaRy package
            station_data <- stationaRy::get_isd_station_data(station_id = wi$nearest_station[1], 
                                                             startyear = year(wi$start_date[1]), 
                                                             endyear = year(wi$end_date[1]),
                                                             select_additional_data = all_data) %>%
                dplyr::mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
                dplyr::filter(date >= wi$start_date[1], date <= wi$end_date[1]) %>%
                dplyr::select(lat:date, -time, -date)
            
            count <- count + 1
        }
    }
    
    # BETTER WAY TO DEAL WITH MISSING DATA??
    # remove NA values from station_data (hack: just remove anything with '99' or more or if val = 9)
    # some columns might have a 9 or 99 that is a real value. This is conservative.
    station_data <- data.frame(lapply(station_data, function(x) as.numeric(as.character(x))))
    
    # not pretty but it works
    for(i in 1:ncol(station_data)) {
        for(j in 1:nrow(station_data)) {
            if(!identical(integer(0), grep(99, station_data[j,i])) | identical(station_data[j,i], 9)) {
                station_data[j,i] <- NA
            }
        }
    }
    
    # ADDITIONAL DATA NEED TO BE TIDYED???

    # add traits to wi dataframe
    get_quartiles <- function(trait) {
        val <- station_data %>%
            dplyr::pull(trait)
        
        # get quartiles of data
        min <- min(val, na.rm = T)
        max <- min(val, na.rm = T)
        q10 <- quantile(val, probs = 0.1, na.rm = T)[[1]]
        q25 <- quantile(val, probs = 0.25, na.rm = T)[[1]]
        mean <- mean(val, na.rm = T)[[1]]
        median <- median(val, na.rm = T)[[1]]
        q75 <- quantile(val, probs = 0.75, na.rm = T)[[1]]
        q90 <- quantile(val, probs = 0.9, na.rm = T)[[1]]
        
        return(paste(c(min, q10, q25, mean, median, q75, q90, max), collapse = ","))
    }
    
    # add station latitude/longitude/elevation to wi dataframe
    wi <- wi %>%
        dplyr::mutate(station_lat = station_data$lat[1],
                      station_lon = station_data$lon[1],
                      station_elev = station_data$elev[1])
    
    # remove lat, long, and elev now
    station_data <- station_data %>%
        dplyr::select(-c(lat, lon, elev))
    
    # add quartiles for rest of station_data to wi dataframe
    for(i in names(station_data)) {
        wi <- wi %>%
            dplyr::mutate(!!i := get_quartiles(i)) %>% #assign variable dynamically!!!
            tidyr::separate(!!i, into = c(paste0("min.", i), paste0("q10.", i),
                                          paste0("q25.", i), paste0("mean.", i),
                                          paste0("median.", i), paste0("q75.", i),
                                          paste0("q90.", i), paste0("max.", i)), sep = ",")
    }
    
    # save dataframe
    wi <- wi %>%
        tidyr::gather(trait, value, -c(isotype:station_elev))
}
    
    readr::write_tsv(wi, paste0(wi$isotype[1], ".noaa.tsv"))
    

    