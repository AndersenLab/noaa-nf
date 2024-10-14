#! usr/bin/env nextflow

params.samples = "${workflow.projectDir}/test_data/wild_isolates.csv"
params.isd_inventory = "${workflow.projectDir}/isd-inventory.csv"

if (params.out == null){
    params.outdir = "noaa_analysis_${params.day}"
} else {
    params.outdir = params.out
}

if (params.help | params.debug){
    params.in = "${workflow.projectDir}/test_data/noaa_test.csv"
} else {
    params.in = params.samples
}

// takes input csv with columns "isotype", "latitude", "longitude", and "isolation_date"
// and returns a tsv file for each strain with added elevation from geonames package

workflow {
    Channel.fromPath(params.in) | split_WI
    split_WI.out.flatten()
        .combine(Channel.of( params.isd_inventory) ) | findStations
    findStations.out.toSortedList() | joinData
}
process split_WI {
  label 'xs'

  input:
    path(infile)

  output:
    path("*.raw.tsv")

  """
  
  #!/usr/bin/env Rscript --vanilla
  library(dplyr)
  library(tidyr)
  library(readr)
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

// process downloadStations { 
//   publishDir "${workflow.projectDir}/", mode: 'copy'

//   output:
//     file("isd-inventory.csv")

//   """
//   wget 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-inventory.csv'
//   """
// }


// for each strain, finds the closest weather station and download relevant data
process findStations {
  label "sm"

  tag { wi_location }

  input:
    tuple path(wi_location), path(isd_inventory)

  output:
    path("*.noaa.tsv")


  """
  Rscript --vanilla "${workflow.projectDir}/bin/find_stations.R" "${wi_location}" "${isd_inventory}" "${params.months}" "${params.events}" "${params.important_trait}" "${workflow.projectDir}/bin/get_isd_station_data_fix.R"

  """

}


process joinData {
    publishDir "${params.outdir}/", mode: 'copy'

    executor "local"
    container null

    input:
        val(strain_file)

    output:
        file("phenotypes.tsv")

    """
    # use this to only print the header of the first line
    awk 'FNR>1 || NR==1' ${strain_file.join(" ")} > phenotypes.tsv
    """
}

