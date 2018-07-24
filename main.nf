#! usr/bin/env nextflow

// params.in
// params.time_period
// params.important_trait
// params.additional_data
// params.events
// params.out

// takes input csv with columns "isotype", "latitude", "longitude", and "isolation_date"
// and returns a tsv file for each strain with added elevation from geonames package
process split_WI {
  input:
  file "infile" from Channel.fromPath(params.in)

  output:
  file("*.raw.tsv") into locations

  """
  
  #!/usr/bin/env Rscript --vanilla
  library(tidyverse)
  options(geonamesUsername="katiesevans")
  library(geonames)

  # read in raw data and filter out strains with no location
  # add absolute value of latitude and longitude
  # right now, add elevation because it is not in my file, can remove later
  wi <- read.csv("${infile}") %>%
      tidyr::drop_na(latitude, longitude, isolation_date) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(abslat = abs(latitude),
                    abslong = abs(longitude),
                    elevation = geonames::GNsrtm3(latitude, longitude)\$srtm3)

  # make new csv file for each strain
  for(i in unique(wi\$isotype)) {
   df <- wi %>%
       dplyr::filter(isotype == i)
   readr::write_tsv(df, paste0(i, ".raw.tsv"))
  }

  """
}

process downloadStations { 
  publishDir "${workflow.projectDir}/", mode: 'copy'

  output:
    file("isd-inventory.csv")

  """
  wget 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-inventory.csv'
  """
}


// for each strain, finds the closest weather station and download relevant data
process findStations {
  cpus 4
  tag { wi_location }

  input:
  file wi_location from locations.flatten()

  output:
  file("*.noaa.tsv") into noaa_files


  """
  Rscript --vanilla "${workflow.projectDir}/find_stations.R" "${wi_location}" "${workflow.projectDir}/isd-inventory.csv" "${params.time_period}" "${params.events}" "${params.important_trait}" "${params.additional_data}"

  """

}


process joinData {
    publishDir "analysis-${params.out}/", mode: 'copy'
    
    input:
        val(strain_file) from noaa_files.toSortedList()

    output:
        file("phenotype-${params.out}.tsv") into output

    """
    # use this to only print the header of the first line
    awk 'FNR>1 || NR==1' ${strain_file.join(" ")} > phenotype-${params.out}.tsv
    """
}

