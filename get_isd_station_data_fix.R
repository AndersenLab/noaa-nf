#' Get met station data from the ISD dataset
#' @description Obtain one or more years of meteorological data for a station
#' from the NCEI Integrated Surface Dataset (ISD).
#' @param station_id a station identifier composed of the station's USAF and
#' WBAN numbers, separated by a hyphen.
#' @param startyear the starting year for the collected data.
#' @param endyear the ending year for the collected data.
#' @param full_data include additional meteorological data found in the
#' dataset's additional data section?
#' @param add_data_report selecting TRUE will provide a data frame with
#' information on which additional data categories are available for the
#' selected station during the specified years.
#' @param select_additional_data a vector of categories for additional
#' meteorological data to include (instead of all available categories).
#' @param use_local_files option to use data files already available locally.
#' @param local_file_dir path to local meteorological data files.
#' @return Returns a data frame with 18 variables. Times are recorded 
#' using the Universal Time Code (UTC) in the source data. Times are adjusted
#' to local standard time for the station's locale.
#' \describe{
#'   \item{usaf}{A character string identifying the fixed weather 
#'     station from the USAF Master Station Catalog.
#'     USAF is an acronym for United States Air Force.}
#'   \item{wban}{A character string for the fixed weather
#'     station NCDC WBAN identifier.  
#'     NCDC is an acronym for National Climatic Data Center. 
#'     WBAN is an acronym for Weather Bureau, Air Force and Navy.}
#'   \item{year}{A numeric, four digit value giving the year of the 
#'     observation.}
#'   \item{month}{A numeric value (one or two digits) giving the month
#'     of the observation.}
#'   \item{day}{A numeric value (one or two digits) giving the day of the 
#'     month of the observation.}
#'   \item{hour}{A numeric value (one or two digits) giving the hour of 
#'     the observation.}
#'   \item{minute}{A numeric value (one or two digits) giving the minute 
#'     of the hour in which the observation was recorded.}
#'   \item{lat}{Latitude (degrees) rounded to three decimal places.}
#'   \item{lon}{Longitude (degrees) rounded to three decimal places.}
#'   \item{elev}{Numeric value for the elevation as measured in meters. 
#'     The minimum value is -400 with a maximum of 8850. Elevation in feet
#'     can be approximated by \code{elev * 3.28084}}
#'   \item{wd}{The angle of wind direction, measured in a clockwise 
#'     direction, between true north and the direction from which
#'     the wind is blowing. For example, \code{wd = 90} indicates the 
#'     wind is blowing from due east. \code{wd = 225} indicates the 
#'     wind is blowing from the south west. The minimum value is 1, and the
#'     maximum value is 360.}
#'   \item{ws}{Wind speed in meters per second.  Wind speed in feet per 
#'     second can be estimated by \code{ws * 3.28084}}
#'   \item{ceil_hgt}{The height above ground level of the lowest clould cover
#'     or other obscuring phenomena amounting to at least 5/8 sky 
#'     coverate.  Measured in meters.  Unlimited height (no obstruction)
#'     is denoted by the value 22000}
#'   \item{temp}{Air temperature measured in degrees Celsius. Conversions 
#'     to degrees Farenheit may be calculated with 
#'     \code{(temp * 9) / 5 + 32}}.
#'   \item{dew_point}{The temperature in degrees Celsius to which a 
#'     given parcel of air must be cooled at constant pressure and 
#'     water vapor content in order for saturation to occur.}
#'   \item{atmos_pres}{The air pressure in hectopascals relative to 
#'     Mean Sea Level (MSL)}
#'   \item{rh}{Relative humidity, measured as a percentage,
#'     as calculated using the August-Roche-Magnus approximation}
#'   \item{time}{A POSIXct object with the date-time of the observation.}
#' }
#' 
#' @source 
#' \url{http://www.ncdc.noaa.gov/isd}\cr
#' \url{http://www1.ncdc.noaa.gov/pub/data/ish/ish-format-document.pdf}
#' 
#' Calculating Humidity: \cr
#' \url{https://en.wikipedia.org/wiki/Clausius\%E2\%80\%93Clapeyron_relation#Meteorology_and_climatology}
#' @examples 
#' \dontrun{
#' # Obtain a listing of all stations within a bounding box and
#' # then isolate a single station and obtain a string with the
#' # \code{usaf} and \code{wban} identifiers.
#' # Pass that identifier string to the \code{get_isd_station_data}
#' # function to obtain a data frame of meteorological data for
#' # the year 2010
#' stations_within_domain <-
#'   get_isd_stations(
#'     lower_lat = 49.000,
#'     upper_lat = 49.500,
#'     lower_lon = -123.500,
#'     upper_lon = -123.000)
#'                         
#' cypress_bowl_snowboard_stn <-
#'   select_isd_station(
#'     stn_df = stations_within_domain,
#'     name = "cypress bowl snowboard")
#' 
#' cypress_bowl_snowboard_stn_met_data <-
#'   get_isd_station_data(
#'     station_id = cypress_bowl_snowboard_stn,
#'     startyear = 2010,
#'     endyear = 2010)
#'  
#' # Get a vector of available additional data categories for a station
#' # during the specied years
#' additional_data_categories <- 
#'   get_isd_station_data(
#'     station_id = "722315-53917",
#'     startyear = 2014,
#'     endyear = 2015,
#'     add_data_report = TRUE)
#'  
#' # Obtain two years of data from data files stored on disk (in this
#' # case, inside the package itself)
#' df_mandatory_data_local <- 
#'   get_isd_station_data(
#'     station_id = "999999-63897",
#'     startyear = 2013,
#'     endyear = 2014,
#'     use_local_files = TRUE,
#'     local_file_dir = system.file(package = "stationaRy"))
#' }
#' @import readr dplyr downloader progress
#' @importFrom stringr str_detect str_extract
#' @importFrom plyr round_any
#' @importFrom lubridate year month mday hour minute
#' @export

# load packages needed
library(readr)
library(dplyr)
library(downloader)
library(progress)
library(stringr)
library(plyr)
library(lubridate)

get_isd_station_data_fix <- function(station_id,
                                 startyear,
                                 endyear,
                                 full_data = FALSE,
                                 add_data_report = FALSE,
                                 select_additional_data = NULL,
                                 use_local_files = FALSE,
                                 local_file_dir = NULL){
    
    usaf <- wban <- year <- NA
    
    # Check whether `startyear` and `endyear` are both numeric
    if (!is.numeric(startyear) | !is.numeric(endyear)) {
        stop("Please enter numeric values for the starting and ending years")
    }
    
    # Check whether `startyear` and `endyear` are in the correct order
    if (startyear > endyear) {
        stop("Please enter the starting and ending years in the correct order")
    }
    
    # # Obtain the GMT offset value for this ISD station
    gmt_offset <-
        as.numeric(
            dplyr::filter(
                get_isd_stations(),
                usaf == as.numeric(unlist(strsplit(station_id,
                                                   "-"))[1]),
                wban == as.numeric(unlist(strsplit(station_id,
                                                   "-"))[2]))[,11])
    # 
    # # if 'gmt_offset' is positive, then also download year of data previous to
    # # beginning of series
    # if (gmt_offset > 0) startyear <- startyear - 1
    # 
    # # if 'gmt_offset' is negative, then also download year of data following the
    # # end of series
    # if (gmt_offset < 0 & year(Sys.time()) != endyear) endyear <- endyear + 1
    
    if (use_local_files){
        
        for (i in startyear:endyear){
            if (i == startyear){
                data_files <- vector(mode = "character")
            }
            
            data_files <- 
                c(data_files,
                  paste0(
                      sprintf(
                          "%06d",
                          as.numeric(unlist(strsplit(station_id,
                                                     "-"))[1])),
                      "-",
                      sprintf(
                          "%05d",
                          as.numeric(unlist(strsplit(station_id,
                                                     "-"))[2])),
                      "-", i, ".gz"))
        }
        
        # Verify that local files are available
        all_local_files_available <-
            all(file.exists(paste0(local_file_dir, "/", data_files)))
    }
    
    if (use_local_files == FALSE) {
        
        # Create a temporary folder to deposit downloaded files
        temp_folder <- tempdir()
        
        # If a station ID string provided,
        # download the gzip-compressed data files for the years specified
        for (i in startyear:endyear){
            if (i == startyear){
                data_files <- vector(mode = "character")
            }
            
            data_file_to_download <- 
                paste0(
                    sprintf("%06d",
                            as.numeric(unlist(strsplit(station_id,
                                                       "-"))[1])),
                    "-",
                    sprintf("%05d",
                            as.numeric(unlist(strsplit(station_id,
                                                       "-"))[2])),
                    "-", i, ".gz")
            
            try(download(
                url = paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", i,
                             "/", data_file_to_download),
                destfile = file.path(temp_folder, data_file_to_download)),
                silent = TRUE)
            
            if (file.info(
                file.path(temp_folder,
                          data_file_to_download))$size > 1){
                
                data_files <- c(data_files,
                                data_file_to_download)
            }
        }
    }
    
    if (add_data_report) {
        
        # Create vector of additional data categories
        data_categories <-
            c("AA1", "AB1", "AC1", "AD1", "AE1",
              "AG1", "AH1", "AI1", "AJ1", "AK1",
              "AL1", "AM1", "AN1", "AO1", "AP1",
              "AU1", "AW1", "AX1", "AY1", "AZ1",
              "CB1", "CF1", "CG1", "CH1", "CI1",
              "CN1", "CN2", "CN3", "CN4", "CR1",
              "CT1", "CU1", "CV1", "CW1", "CX1",
              "CO1", "CO2", "ED1", "GA1", "GD1",
              "GF1", "GG1", "GH1", "GJ1", "GK1",
              "GL1", "GM1", "GN1", "GO1", "GP1",
              "GQ1", "GR1", "HL1", "IA1", "IA2",
              "IB1", "IB2", "IC1", "KA1", "KB1",
              "KC1", "KD1", "KE1", "KF1", "KG1",
              "MA1", "MD1", "ME1", "MF1", "MG1",
              "MH1", "MK1", "MV1", "MW1", "OA1",
              "OB1", "OC1", "OE1", "RH1", "SA1",
              "ST1", "UA1", "UG1", "UG2", "WA1",
              "WD1", "WG1")
        
        # Get additional data portions of records, exluding remarks
        for (i in 1:length(data_files)){
            
            if (use_local_files == FALSE){
                add_data <- 
                    readLines(file.path(temp_folder,
                                        data_files[i]))
            }
            
            if (use_local_files == TRUE){
                add_data <- 
                    readLines(file.path(local_file_dir,
                                        data_files[i]))
            }
            
            if (i == 1){
                all_add_data <- add_data
            }
            
            if (i > 1){
                all_add_data <- c(all_add_data, add_data)
            }
        }
        
        # Obtain data counts for all additional parameters
        for (i in 1:length(data_categories)){
            if (i == 1){
                data_categories_counts <-
                    vector(mode = "numeric",
                           length = length(data_categories))
            }
            
            data_categories_counts[i] <-
                sum(stringr::str_detect(all_add_data, data_categories[i]))
        }
        
        # Determine which data categories have data
        data_categories_available <-
            data_categories[which(data_categories_counts > 0)]
        
        # Get those data counts that are greater than 0
        data_categories_counts <-
            data_categories_counts[which(data_categories_counts > 0)]
        
        # Create a data frame composed of categories and their counts
        data_categories_df <- 
            data.frame(category = data_categories_available,
                       total_count = data_categories_counts)
        
        return(data_categories_df)
    }
    
    # Define column widths for fixed-width data in the mandatory section of
    # the ISD data files
    column_widths <- 
        c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
          7, 5, 5, 5, 4, 3, 1, 1, 4, 1,
          5, 1, 1, 1, 6, 1, 1, 1, 5, 1,
          5, 1, 5, 1)
    
    if (use_local_files) {
        
        data_files <- 
            file.path(local_file_dir, data_files)
        
    } else {
        
        data_files <- 
            file.path(temp_folder, data_files)
    }
    
    for (i in seq(data_files)){
        
        if (file.exists(data_files[i])){
            
            # Read data from mandatory data section of each file,
            # which is a fixed-width string
            data <- 
                read_fwf(
                    data_files[i],
                    fwf_widths(column_widths),
                    col_types = "ccciiiiiciicicciccicicccccccicicic")
            
            # Remove select columns from data frame
            data <- 
                data[, c(2:8, 10:11, 13, 16, 19, 21, 29, 31, 33)]
            
            # Apply new names to the data frame columns
            names(data) <-
                c("usaf", "wban", "year", "month",
                  "day", "hour", "minute", "lat", "lon",
                  "elev", "wd", "ws", "ceil_hgt",
                  "temp", "dew_point", "atmos_pres")
            
            # Correct the latitude values
            data$lat <- data$lat/1000
            
            # Correct the longitude values
            data$lon <- data$lon/1000
            
            # Correct the wind direction values
            data$wd <- 
                ifelse(data$wd == 999, NA, data$wd)
            
            # Correct the wind speed values
            data$ws <- 
                ifelse(data$ws == 9999, NA, data$ws/10)
            
            # Correct the temperature values
            data$temp <- 
                ifelse(data$temp == 9999, NA, data$temp/10)
            
            # Correct the dew point values
            data$dew_point <- 
                ifelse(data$dew_point == 9999, NA, data$dew_point/10)
            
            # Correct the atmospheric pressure values
            data$atmos_pres <- 
                ifelse(data$atmos_pres == 99999, NA, data$atmos_pres/10)
            
            # Correct the ceiling height values
            data$ceil_hgt <- 
                ifelse(data$ceil_hgt == 99999, NA, data$ceil_hgt)
            
            # Calculate RH values using the August-Roche-Magnus approximation
            for (j in 1:nrow(data)){
                
                if (j == 1) rh <- vector("numeric")
                
                rh_j <- 
                    ifelse(is.na(data$temp[j]) | is.na(data$dew_point[j]), NA,
                           100 * (exp((17.625 * data$dew_point[j]) /
                                          (243.04 + data$dew_point[j]))/
                                      exp((17.625 * (data$temp[j])) /
                                              (243.04 + (data$temp[j])))))
                
                # Round data to the nearest 0.1
                rh_j <- plyr::round_any(as.numeric(rh_j), 0.1, f = round)
                
                rh <- c(rh, rh_j)
            }
            
            # Add RH values to the data frame
            data$rh <- rh
            
            if (i == 1) {
                large_data_frame <- data
            }
            
            if (i > 1) {
                large_data_frame <- bind_rows(large_data_frame, data)
            }
        }
    }
    
    # Create POSIXct time values from the time elements
    large_data_frame$time <- 
        ISOdatetime(
            year = large_data_frame$year,
            month = large_data_frame$month,
            day = large_data_frame$day,
            hour = large_data_frame$hour,
            min = large_data_frame$minute,
            sec = 0,
            tz = "GMT") + 
        (gmt_offset * 3600)
    
    # Update time component columns to reflect corrected dates/times
    large_data_frame$year <- year(large_data_frame$time)
    large_data_frame$month <- month(large_data_frame$time) 
    large_data_frame$day <- mday(large_data_frame$time)
    large_data_frame$hour <- hour(large_data_frame$time)
    large_data_frame$minute <- minute(large_data_frame$time)
    
    # Ensure that data frame columns are correctly classed
    large_data_frame$usaf <- as.character(large_data_frame$usaf)
    large_data_frame$wban <- as.character(large_data_frame$wban) 
    large_data_frame$year <- as.numeric(large_data_frame$year)
    large_data_frame$month <- as.numeric(large_data_frame$month)
    large_data_frame$day <- as.numeric(large_data_frame$day)
    large_data_frame$hour <- as.numeric(large_data_frame$hour)
    large_data_frame$minute <- as.numeric(large_data_frame$minute)
    large_data_frame$lat <- as.numeric(large_data_frame$lat)
    large_data_frame$lon <- as.numeric(large_data_frame$lon)
    large_data_frame$elev <- as.numeric(large_data_frame$elev)
    large_data_frame$wd <- as.numeric(large_data_frame$wd)
    large_data_frame$ws <- as.numeric(large_data_frame$ws)
    large_data_frame$ceil_hgt <- as.numeric(large_data_frame$ceil_hgt)
    large_data_frame$temp <- as.numeric(large_data_frame$temp)
    large_data_frame$dew_point <- as.numeric(large_data_frame$dew_point)
    large_data_frame$atmos_pres <- as.numeric(large_data_frame$atmos_pres)
    large_data_frame$rh <- as.numeric(large_data_frame$rh)
    
    # # if 'gmt_offset' is positive, add back a year to 'startyear'
    # if (gmt_offset > 0) startyear <- startyear + 1
    # 
    # # if 'gmt_offset' is negative, subtract the added year from 'endyear'
    # if (gmt_offset < 0 & year(Sys.time()) != endyear) endyear <- endyear - 1
    
    # If additional data categories specified, then set 'full_data' to TRUE
    # to enter that conditional block
    if (!is.null(select_additional_data)) full_data <- TRUE
    
    if (full_data == FALSE){
        
        # Filter data frame to only include data for requested years
        large_data_frame <- dplyr::filter(large_data_frame, year >= startyear &
                                              year <= endyear)
        
        return(large_data_frame)
    }
    
    if (full_data == TRUE){
        
        # Get additional data portions of records, exluding remarks
        for (i in 1:length(data_files)){
            
            if (use_local_files == FALSE){
                
                add_data <- 
                    readLines(data_files[i])
            }
            
            if (use_local_files == TRUE){
                
                add_data <- 
                    readLines(data_files[i])
            }
            
            if (i == 1){
                all_add_data <- add_data
            }
            
            if (i > 1){
                all_add_data <- c(all_add_data, add_data)
            }
        }
        
        # Create vector of additional data categories
        data_categories <-
            c("AA1", "AB1", "AC1", "AD1", "AE1", "AG1", "AH1", "AI1", "AJ1",
              "AK1", "AL1", "AM1", "AN1", "AO1", "AP1", "AU1", "AW1", "AX1",
              "AY1", "AZ1", "CB1", "CF1", "CG1", "CH1", "CI1", "CN1", "CN2",
              "CN3", "CN4", "CR1", "CT1", "CU1", "CV1", "CW1", "CX1", "CO1",
              "CO2", "ED1", "GA1", "GD1", "GF1", "GG1", "GH1", "GJ1", "GK1",
              "GL1", "GM1", "GN1", "GO1", "GP1", "GQ1", "GR1", "HL1", "IA1",
              "IA2", "IB1", "IB2", "IC1", "KA1", "KB1", "KC1", "KD1", "KE1",
              "KF1", "KG1", "MA1", "MD1", "ME1", "MF1", "MG1", "MH1", "MK1",
              "MV1", "MW1", "OA1", "OB1", "OC1", "OE1", "RH1", "SA1", "ST1",
              "UA1", "UG1", "UG2", "WA1", "WD1", "WG1")
        
        expanded_column_names <-
            list(
                "AA1" = c("aa1_liq_precip_period_quantity",
                          "aa1_liq_precip_depth_dimension",
                          "aa1_liq_precip_condition_code",
                          "aa1_liq_precip_quality_code")
            )
        
        # Function for getting data from an additional data category
        get_df_from_category <- function(category_key,
                                         field_lengths,
                                         scale_factor,
                                         data_types,
                                         add_data) {
            
            # Parse string of characters representing data types
            if (class(data_types) == "character" &
                length(data_types) == 1 &
                all(unique(unlist(strsplit(data_types, ""))) %in% c("c", "n"))) {
                
                for (i in 1:nchar(data_types)){
                    
                    if (i == 1){
                        subst_data_types <- vector(mode = "character")
                        
                        # Create a progress bar object
                        pb <- progress::progress_bar$new(
                            format = "  processing :what [:bar] :percent",
                            total = nchar(data_types))
                        
                    }
                    subst_data_types <- 
                        c(subst_data_types,
                          ifelse(substr(data_types, i, i) == "n",
                                 "numeric", "character"))
                    
                }
                
                data_types <- subst_data_types
            }
            
            data_strings <- 
                stringr::str_extract(add_data, paste0(category_key, ".*"))
            
            for (i in 1:length(field_lengths)){
                
                if (i == 1){
                    df_from_category <-
                        as.data.frame(mat.or.vec(nr = length(data_strings),
                                                 nc = length(field_lengths)))
                    
                    colnames(df_from_category) <- 
                        paste(tolower(category_key),
                              rep = 1:length(field_lengths),
                              sep = "_")
                    
                    substr_start <- 4
                    substr_end <- substr_start + (field_lengths[i] - 1)
                }
                
                if (i > 1){
                    
                    substr_start <- substr_end + 1
                    substr_end <- substr_start + (field_lengths[i] - 1)
                }
                
                if (data_types[i] == "numeric"){
                    
                    for (j in 1:length(data_strings)){
                        
                        if (j == 1) data_column <- vector(mode = data_types[i])
                        
                        data_element <-
                            ifelse(!is.na(data_strings[j]),
                                   as.numeric(substr(data_strings[j],
                                                     substr_start,
                                                     substr_end))/scale_factor[i],
                                   NA)
                        
                        data_column <- c(data_column, data_element)
                    }
                }
                
                if (data_types[i] == "character"){
                    
                    for (j in 1:length(data_strings)){
                        
                        if (j == 1) data_column <- vector(mode = data_types[i])
                        
                        data_element <-
                            ifelse(!is.na(data_strings[j]),
                                   substr(data_strings[j],
                                          substr_start,
                                          substr_end),
                                   NA)
                        
                        data_column <- c(data_column, data_element)
                    }
                }
                
                df_from_category[,i] <- data_column
                
                # Add tick to progress bar
                pb$tick(tokens = list(what = category_key))
            }
            
            return(df_from_category)
        }
        
        # Determine which additional parameters have been measured
        for (i in 1:length(data_categories)){
            
            if (i == 1){
                data_categories_counts <-
                    vector(mode = "numeric",
                           length = length(data_categories))
            }
            
            data_categories_counts[i] <-
                sum(stringr::str_detect(all_add_data, data_categories[i]))
        }
        
        # Filter those measured parameters and obtain string of identifiers
        significant_params <- data_categories[which(data_categories_counts >= 1)]
        
        # Filter the significantly available extra parameters by those specified
        if (!is.null(select_additional_data)){
            
            significant_params <-
                significant_params[which(significant_params %in%
                                             select_additional_data)]
        }
        
        # AA1 - liquid precipitation: period quantity, depth dimension
        if (data_categories[1] %in% significant_params){
            
            additional_data <-
                get_df_from_category(
                    "AA1",
                    c(2, 4, 1, 1),
                    c(1, 10, NA, NA),
                    "nncc",
                    all_add_data)
            
            large_data_frame <- bind_cols(large_data_frame, additional_data)
        }

        
        # If the tz offset is 0, return the data frame without filtering it
        # if (gmt_offset == 0){
        #     return(large_data_frame) 
        # }
        
        # Filter data frame to only include data for requested years
        large_data_frame <- 
            dplyr::filter(large_data_frame, 
                          year >= startyear &
                              year <= endyear)
        
        return(large_data_frame)
    }
}
