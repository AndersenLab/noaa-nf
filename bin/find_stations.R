#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# source for get_isd_station_data_fix
source(args[6])

library(lubridate)
#library(tidyverse)
library(tidyr)
library(readr)
library(stationaRy)
library(geosphere)
library(dplyr)

# args[1] = input dat#aframe
# args[2] = station dataframe
# args[3] = time_period
# args[4] = number of events per month
# args[5] = important trait
# args[6] = get_isd_station_data.R location

# inputs
df <- readr::read_tsv(args[1])
time_period <- as.numeric(args[3])
events <- as.numeric(args[4])
all_data <- "AA1"
important_trait <- args[5]

# download station data
station_inventory <- read.csv(args[2]) %>%
    dplyr::mutate(id = paste(USAF, WBAN, sep = "-"))

if(important_trait == "NULL") important_trait <- NULL
if(all_data == "NULL") all_data <- NULL

# function to add traits to wi dataframe
get_quartiles <- function(trait) {
    val <- station_data %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(min = min(get(trait), na.rm = T),
                         max = max(get(trait), na.rm = T),
                         q10 = quantile(get(trait), probs = 0.1, na.rm = T)[[1]],
                         q25 = quantile(get(trait), probs = 0.25, na.rm = T)[[1]],
                         mean = mean(get(trait), na.rm = T),
                         median = median(get(trait), na.rm = T),
                         q75 = quantile(get(trait), probs = 0.75, na.rm = T)[[1]],
                         q90 = quantile(get(trait), probs = 0.9, na.rm = T)[[1]],
                         sd = sd(get(trait), na.rm = T))
    
    # remove Inf values
    val <- do.call(data.frame,lapply(val, function(x) replace(x, is.infinite(x),NA)))
    val <- do.call(data.frame,lapply(val, function(x) replace(x, is.nan(x),NA)))
    
    # # get quartiles of data - means
    min_mean <- mean(val$min, na.rm = T)
    max_mean <- mean(val$max, na.rm = T)
    q10_mean <- mean(val$q10, na.rm = T)
    q25_mean <- mean(val$q25, na.rm = T)
    mean_mean <- mean(val$mean, na.rm = T)
    median_mean <- mean(val$median, na.rm = T)
    q75_mean <- mean(val$q75, na.rm = T)
    q90_mean <- mean(val$q90, na.rm = T)
    
    # sd
    sd_mean <- mean(val$sd, na.rm = T)
    sd_median <- median(val$sd, na.rm = T)
    sd_q25 <- quantile(val$sd, probs = 0.25, na.rm = T)[[1]]
    sd_q75 <- quantile(val$sd, probs = 0.75, na.rm = T)[[1]]
    mean_sd <- sd(val$mean, na.rm = T)
    
    # get quartiles of data - mins
    min_min <- min(val$min, na.rm = T)
    min_q10 <- quantile(val$min, probs = 0.1, na.rm = T)[[1]]
    min_q25 <- quantile(val$min, probs = 0.25, na.rm = T)[[1]]
    # min_mean <- mean(val$min,na.rm = T)
    min_median <- median(val$min, na.rm = T)
    min_q75 <- quantile(val$min, probs = 0.75, na.rm = T)[[1]]
    min_q90 <- quantile(val$min, probs = 0.90, na.rm = T)[[1]]
    min_max <- max(val$min, na.rm = T)
    
    # get quartiles of data - maxs
    max_min <- min(val$max, na.rm = T)
    max_q10 <- quantile(val$max, probs = 0.1, na.rm = T)[[1]]
    max_q25 <- quantile(val$max, probs = 0.25, na.rm = T)[[1]]
    # max_mean <- mean(val$max,na.rm = T)
    max_median <- median(val$max, na.rm = T)
    max_q75 <- quantile(val$max, probs = 0.75, na.rm = T)[[1]]
    max_q90 <- quantile(val$max, probs = 0.90, na.rm = T)[[1]]
    max_max <- max(val$max, na.rm = T)
    
    return(paste(c(min_mean, max_mean, q10_mean, q25_mean, mean_mean, median_mean, q75_mean, q90_mean, 
                   min_min, min_q10, min_q25, min_median, min_q75, min_q90, min_max,
                   max_min, max_q10, max_q25, max_median, max_q75, max_q90, max_max,
                   sd_mean, sd_median, sd_q25, sd_q75, mean_sd), collapse = ","))
}

#update wi for start/end dates
days <- (time_period*30)/2 #use 30 days to represent a month

wi <- df %>%
    dplyr::mutate(isolation_date = lubridate::ymd(as.character(isolation_date)),
                  start_date = isolation_date - lubridate::ddays(days),
                  end_date = isolation_date + lubridate::ddays(days))

# if the time period is unavailable, push it back
# for example, 3 year data collected on 12/14/17
if(wi$end_date[1] > lubridate::ymd(Sys.Date())) {
    fake_isolation_date <- lubridate::ymd(Sys.Date()) - lubridate::ddays(days)
    wi$start_date[1] <- fake_isolation_date - lubridate::ddays(days)
    wi$end_date[1] <- fake_isolation_date + lubridate::ddays(days)
}

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

failed <- FALSE
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
    stations <- stationaRy::get_isd_stations(lower_lat = wi$latitude[1] - 15,
                                             upper_lat = wi$latitude[1] + 15,
                                             lower_lon = wi$longitude[1] - 15,
                                             upper_lon = wi$longitude[1] + 15) %>%
        dplyr::mutate(id = paste(usaf, wban, sep = "-")) %>%
        dplyr::filter(id %in% st$id) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dist_to_worm = geosphere::distm(c(wi$longitude[1], wi$latitude[1]), c(lon, lat))[1]) %>%
        dplyr::arrange(dist_to_worm)
    if(nrow(stations) == 0) {
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

        # assign nearest station and station distance to worm
        wi <- wi %>%
            dplyr::mutate(nearest_station = stations$id[1],
                            station_distance = stations$dist_to_worm[1]/1000)

        #Get station data using stationaRy package
        #had to copy the function and change some lines of code because it was downloading extra years it didn't need
        #use try/catch if there is an error with the file not existing, try the next closest station
        station_data <- try(get_isd_station_data_fix(station_id = wi$nearest_station[1], 
                                                        startyear = year(wi$start_date[1]), 
                                                        endyear = year(wi$end_date[1]),
                                                        select_additional_data = all_data) %>%
                                dplyr::mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
                                dplyr::filter(date >= wi$start_date[1], date <= wi$end_date[1]) %>%
                                dplyr::select(lat:date, -time))

        #Check to see if the station_data file downloaded, if not, try the next closest station
        count <- 2
        while(class(station_data)[1] == "try-error") {
            print(paste(count, length(stations)))
            if(count > length(stations)) {
                failed <- TRUE
                break    
            }
            wi <- wi %>%
                dplyr::mutate(nearest_station = stations$id[count],
                                station_distance = stations$dist_to_worm[count]/1000)

            #Get station data using stationaRy package
            station_data <- try(get_isd_station_data_fix(station_id = wi$nearest_station[1], 
                                                            startyear = year(wi$start_date[1]), 
                                                            endyear = year(wi$end_date[1]),
                                                            select_additional_data = all_data) %>%
                                    dplyr::mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
                                    dplyr::filter(date >= wi$start_date[1], date <= wi$end_date[1]) %>%
                                    dplyr::select(lat:date, -time))

            count <- count + 1
        }


        #If there is no data for important_variable, choose another station
        if(!is.null(important_trait) && failed == FALSE) {

            # count <- 2
            while(class(station_data)[1] == "try-error" || length(which(!is.na(station_data[[important_trait]]))) == 0) {
                print(paste(count, length(stations)))
                if(count > length(stations)) {
                    failed <- TRUE
                    break
                }
                # assign nearest station and station distance to worm
                wi <- wi %>%
                    dplyr::mutate(nearest_station = stations$id[count],
                                    station_distance = stations$dist_to_worm[count]/1000)

                #Get station data using stationaRy package
                station_data <- try(get_isd_station_data_fix(station_id = wi$nearest_station[1], 
                                                                startyear = year(wi$start_date[1]), 
                                                                endyear = year(wi$end_date[1]),
                                                                select_additional_data = all_data) %>%
                                        dplyr::mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
                                        dplyr::filter(date >= wi$start_date[1], date <= wi$end_date[1]) %>%
                                        dplyr::select(lat:date, -time))

                count <- count + 1
            }
        }

        if( failed == FALSE){

            # check to make sure precipitation is downloaded, if not insert NA
            if(!"aa1_1" %in% names(station_data)) {
                station_data <- station_data %>%
                    dplyr::mutate(aa1_1 = NA, aa1_2 = NA, aa1_3 = NA, aa1_4 = NA)
            }

            # remove missing data
            station_data$wd[station_data$wd == 999] <- NA
            station_data$ws[station_data$ws == 9999] <- NA
            station_data$ceil_hgt[station_data$ceil_hgt == 99999] <- NA
            station_data$temp[station_data$temp == '+9999'] <- NA
            station_data$dew_point[station_data$dew_point == '+9999'] <- NA
            station_data$atmos_pres[station_data$atmos_pres == 99999] <- NA
            station_data$aa1_1[station_data$aa1_1 == 99] <- NA
            station_data$aa1_2[station_data$aa1_2 == 9999] <- NA
            station_data$aa1_1[station_data$aa1_4 %in% c(2, 3, 6, 7)] <- NA
            station_data$aa1_2[station_data$aa1_4 %in% c(2, 3, 6, 7)] <- NA

            # Clean up the preciptation columns
            # aa1_1 = period quantity (hours), aa1_2 = depth dimension, aa1_4 = quality
            station_data <- station_data %>%
                dplyr::mutate(precip = aa1_2 / aa1_1) %>%
                dplyr::select(-c(aa1_1, aa1_2, aa1_3, aa1_4))

            # add station latitude/longitude/elevation to wi dataframe
            wi <- wi %>%
                dplyr::mutate(station_lat = station_data$lat[1],
                            station_lon = station_data$lon[1],
                            station_elev = station_data$elev[1])

            # remove lat, long, and elev now
            station_data <- station_data %>%
                dplyr::select(-c(lat, lon, elev))

            # add quartiles for rest of station_data to wi dataframe
            for(j in (names(station_data)[names(station_data) != "date"])) {
                wi <- wi %>%
                    dplyr::mutate(!!j := get_quartiles(j)) %>% #assign variable dynamically!!!
                    tidyr::separate(!!j, into = c(paste0("min_mean.", j), 
                                                paste0("max_mean.", j),
                                                paste0("q10_mean.", j),
                                                paste0("q25_mean.", j),
                                                paste0("mean_mean.", j),
                                                paste0("median_mean.", j),
                                                paste0("q75_mean.", j),
                                                paste0("q90_mean.", j),
                                                paste0("min_min.", j),
                                                paste0("min_q10.", j),
                                                paste0("min_q25.", j),
                                                # paste0("min_mean", j),
                                                paste0("min_median.", j),
                                                paste0("min_q75.", j),
                                                paste0("min_q90.", j),
                                                paste0("min_max.", j),
                                                paste0("max_min.", j),
                                                paste0("max_q10.", j),
                                                paste0("max_q25.", j),
                                                # paste0("max_mean", j),
                                                paste0("max_median.", j),
                                                paste0("max_q75.", j),
                                                paste0("max_q90.", j),
                                                paste0("max_max.", j),
                                                paste0("sd_mean.", j),
                                                paste0("sd_median.", j),
                                                paste0("sd_q25.", j),
                                                paste0("sd_q75.", j),
                                                paste0("mean_sd.", j)), 
                                    sep = ",")
            }

            # save dataframe
            wi <- wi %>%
                tidyr::gather(trait, value, -c(isotype:station_elev))
        } else {
            print("Error. No station data avaiable for this time period. Entering 'NA'.")
            wi <- wi %>%
            dplyr::mutate(nearest_station = NA,
                        station_distance = NA,
                        station_lat = NA,
                        station_lon = NA,
                        station_elev = NA,
                        trait = NA,
                        value = NA)
        }
    }
}

readr::write_tsv(wi, paste0(wi$isotype[1], ".noaa.tsv"))
    

    
