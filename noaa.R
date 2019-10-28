source("~/Dropbox/AndersenLab/LabFolders/Katie/git/noaa-nf/get_isd_station_data_fix.R")

library(lubridate)
library(tidyverse)
library(stationaRy)
options(geonamesUsername="katiesevans")
library(geosphere)

#### parameters ####
infile <- "~/Dropbox/AndersenLab/LabFolders/Katie/projects/collaborations/soil_ph/data/processed/noaa/20180810_WI_36months_raw.csv"
time_period <- 36
events <- 10
all_data <- "AA1"
important_trait <- "temp"
#####################

# read in raw data and filter out strains with no location
# add absolute value of latitude and longitude
# right now, add elevation because it is not in my file, can remove later
wi_all <- read.csv(infile) %>%
    dplyr::select(-X) %>%
    tidyr::drop_na(latitude, longitude, isolation_date) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(abslat = abs(latitude),
                  abslong = abs(longitude),
                  elevation = geonames::GNsrtm3(latitude, longitude)$srtm3)

download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-inventory.csv", destfile = "isd-inventory.csv")
station_inventory <- read.csv("isd-inventory.csv") %>%
    dplyr::mutate(id = paste(USAF, WBAN, sep = "-"))

# create final dataframe
wi_noaa <- NULL

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

# do for each strain
for(i in 230:length(unique(wi_all$isotype))) {
    df <- wi_all %>%
        dplyr::filter(isotype == wi_all$isotype[i])

    #update wi for start/end dates
    days <- (time_period*30)/2 #use 30 days to represent a month
    
    wi <- df %>%
        dplyr::mutate(isolation_date = ymd(as.character(isolation_date)),
                      start_date = isolation_date - ddays(days),
                      end_date = isolation_date + ddays(days))
    
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
    # only keep stations that have data for the months of interest
    st <- station_inventory %>%
        dplyr::filter(YEAR >= year(wi$start_date[1]), YEAR <= year(wi$end_date[1])) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(num_years = n()) %>%
        dplyr::filter(num_years == total_years) %>%
        dplyr::ungroup() %>%
        tidyr::gather(month, recordings, JAN:DEC) %>%
        dplyr::mutate(keep = ifelse(recordings > events, TRUE, FALSE)) %>%
        dplyr::mutate(newmonth = case_when(month == "JAN" ~ 01,
                                           month == "FEB" ~ 02,
                                           month == "MAR" ~ 03,
                                           month == "APR" ~ 04,
                                           month == "MAY" ~ 05,
                                           month == "JUN" ~ 06,
                                           month == "JUL" ~ 07,
                                           month == "AUG" ~ 08,
                                           month == "SEP" ~ 09,
                                           month == "OCT" ~ 10,
                                           month == "NOV" ~ 11,
                                           month == "DEC" ~ 12),
                      newdate = paste0(as.character(YEAR), "-", as.character(newmonth), "-01")) %>%
        dplyr::filter(newdate >= wi$start_date[1] - months(1), newdate <= wi$end_date[1] + months(1)) %>%
        dplyr::group_by(id, YEAR) %>%
        dplyr::mutate(recordings = ifelse(keep == F, NA, recordings),
                      numna = sum(is.na(recordings)) / n()) %>%
        dplyr::filter(numna <= 0.25) %>%
        dplyr::select(-newmonth, -newdate, -numna) %>%
        tidyr::spread(month, recordings) %>%
        dplyr::filter(keep == TRUE)
    
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
        if(!is.null(important_trait)) {
            # count <- 2
            while(class(station_data)[1] == "try-error" || length(which(!is.na(station_data[[important_trait]]))) == 0) {
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
                tidyr::separate(!!j, into = c(paste0("min.", j), paste0("q10.", j),
                                              paste0("q25.", j), paste0("mean.", j),
                                              paste0("median.", j), paste0("q75.", j),
                                              paste0("q90.", j), paste0("max.", j)), sep = ",")
        }
        
        # save dataframe
        wi <- wi %>%
            tidyr::gather(trait, value, -c(isotype:station_elev))
    }
    
    # add to final dataframe
    wi_noaa <- rbind(wi_noaa, wi)
}

readr::write_tsv(wi_noaa, "~/Dropbox/AndersenLab/LabFolders/Katie/projects/collaborations/soil_ph/data/processed/noaa/20180823_winoaa_36mo.tsv")
