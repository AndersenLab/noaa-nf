# noaa-nf

Nextflow pipeline to pull weather data from NOAA weather stations around the world by GPS coordinates. Specified for Andersen Lab looking at weather surrounding date of isolation of *C. elegans* isotypes.

## Requirements
Requires several R packages including "tidyverse", "geonames", "lubridate", "geosphere", and "stationaRy".

```
install.packages(c("tidyverse", "geonames", "lubridate", "geosphere", "devtools"))
devtools::install_github("rich-iannone/stationaRy")
```

Also requires nextflow installation. Check out this [help page](https://www.nextflow.io/docs/latest/getstarted.html) for more info.
```
wget -qO- https://get.nextflow.io | bash
```

## Usage
```
nextflow run AndersenLab/noaa-nf --in='infile.csv'
```

## Input
Input file is a `.csv` file containing columns `isotype`, `latitude`, `longitude`, and `isolation_date`. The isolation date should be in `YYYY-MM-DD` format.

| isotype | latitude | longitude | isolation_date |
| --- | --- | --- | --- |
| AB1 | -34.93 | 138.59 | 1/1/83 |
| CB4856 | 21.33 | -157.86 | 8/1/72 |
| ... | ... | ... | ... |
| QX1211 | 37.7502 | -122.433 | 11/26/07 |

## Optional Parameters
| params | default | options | explanation |
| --- | --- | --- | --- |
| `in` | NA | name of input file | name of input file, required |
| `months` | 3 | Any number | Number of months surrounding date to collect weather data for |
| `important_trait` | NULL | Any one trait (listed below) | Chooses closest weather station to GPS coordinate that has sufficient data for this trait |
| `events` | 10 | Any number | keeps weather stations with at least `events` records per month |
| `out` | `date` | name of output file | name of output file |

## Example with options
```
nextflow run AndersenLab/noaa-nf --in='input.csv' --months='12' --events='100' --out='example_test'
```


## Traits
### Default traits
By default, the `stationaRy` package downloads 8 weather traits:

| trait | description |
| --- | --- |
| wd | The angle of wind direction, measured in a clockwise direction, between true north and the direction from which the wind is blowing. |
| ws | Wind speed in meters per second |
| ceil_hgt | The height above ground level of the lowest clould cover or other obscuring phenomena amounting to at least 5/8 sky coverate. Measured in meters.  |
| temp | Air temperature measured in degrees Celsius. |
| dew_point | The temperature in degrees Celsius to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur. |
| atmos_pres | The air pressure in hectopascals relative to Mean Sea Level (MSL). |
| rh | Relative humidity, measured as a percentage, as calculated using the August-Roche-Magnus approximation. |
| precip | Liquid precipitation, hourly, in inches. |

