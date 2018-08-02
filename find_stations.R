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

# df <- read.csv("~/Dropbox/AndersenLab/LabFolders/Katie/git/noaa-nf/test_data/noaa_test.csv") %>% dplyr::filter(isotype == "CB4856")
# time_period <- 3
# events <- 10
# all_data <- "AA1"
# important_trait <- "temp"
# station_inventory <- read.csv("~/isd-inventory.csv") %>%dplyr::mutate(id = paste(USAF, WBAN, sep = "-"))

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
        dplyr::select(lat:date, -time)
    
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
                dplyr::select(lat:date, -time)
            
            count <- count + 1
        }
    }
    
    # BETTER WAY TO DEAL WITH MISSING DATA??
    # remove NA values from station_data (hack: just remove anything with '99' or more or if val = 9)
    # some columns might have a 9 or 99 that is a real value. This is conservative.
    # station_data <- data.frame(lapply(station_data, function(x) as.numeric(as.character(x))))
    
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
    

    # # not pretty but it works
    # for(i in 1:(ncol(station_data)-1)) {
    #     for(j in 1:nrow(station_data)) {
    #         if(!identical(integer(0), grep(99, station_data[j,i])) | identical(station_data[j,i], 9)) {
    #             station_data[j,i] <- NA
    #         }
    #     }
    # }
    
    # Clean up the preciptation columns
    # aa1_1 = period quantity (hours), aa1_2 = depth dimension, aa1_4 = quality
    station_data <- station_data %>%
        dplyr::mutate(precip = aa1_2 / aa1_1) %>%
        dplyr::select(-c(aa1_1, aa1_2, aa1_3, aa1_4))

    # add traits to wi dataframe
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
                             q90 = quantile(get(trait), probs = 0.9, na.rm = T)[[1]])

        
        # # get quartiles of data
        min <- mean(val$min, na.rm = T)
        max <- mean(val$max, na.rm = T)
        q10 <- mean(val$q10, na.rm = T)
        q25 <- mean(val$q25, na.rm = T)
        mean <- mean(val$mean, na.rm = T)
        median <- mean(val$median, na.rm = T)
        q75 <- mean(val$q75, na.rm = T)
        q90 <- mean(val$q90, na.rm = T)
        
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
    for(i in (names(station_data)[names(station_data) != "date"])) {
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
    

    