# noaa-nf

Nextflow pipeline to pull weather data from NOAA weather stations around the world by GPS coordinates. Specified for Andersen Lab looking at weather surrounding date of isolation of *C. elegans* isotypes.

### Requirements
Requires several R packages including "tidyverse", "geonames", "lubridate", "geosphere", and "stationaRy".

```
install.packages(c("tidyverse", "geonames", "lubridate", "geosphere"))
devtools::install_github("rich-iannone/stationaRy")
```

Also requires nextflow installation. Check out this [help page](https://www.nextflow.io/docs/latest/getstarted.html) for more info.
```
wget -qO- https://get.nextflow.io | bash
```

### Usage
```
nextflow run AndersenLab/noaa-nf --in='infile.csv'
```

### Input
Input file is a `.csv` file containing columns `isotype`, `latitude`, `longitude`, and `isolation_date`

| isotype | latitude | longitude | isolation_date |
| --- | --- | --- | --- |
| AB1 | -34.93 | 138.59 | 1/1/83 |
| CB4856 | 21.33 | -157.86 | 8/1/72 |
| ... | ... | ... | ... |
| QX1211 | 37.7502 | -122.433 | 11/26/07 |

## Optional Parameters
| params | default | options | explanation |
| --- | --- | --- | --- |
| `months` | 3 | Any number | Number of months surrounding date to collect weather data for |
| `important_trait` | NULL | Any one trait (listed below) | Chooses closest weather station to GPS coordinate that has sufficient data for this trait |
| `additional_data` | NULL | Any collection of extra traits (listed below) | Can download more than the 7 default traits |
| `events` | 10 | Any number | keeps weather stations with at least `events` records per month |
| `out` | `date` | name of output file | name of output file |

## Example with options
```
nextflow run AndersenLab/noaa-nf --in='input.csv' --months='12' --events='100' --out='example_test'
```

### Missing Data
Each trait downloaded from the NOAA weather stations (through `stationaRy`) has a different value for missing data. Most traits are either '9', '999', '-999' or '9999' etc. Currently, we employ an overly conservative removal of missing data by removing any value that equals 9 or any value that has two or more consecutive 9s (999, -99, 299...). In the future, this will be cleared up to be more accurate.

### Traits
## Default traits
By default, the `stationaRy` package downloads 7 weather traits:

| trait | description |
| --- | --- |
| wd | The angle of wind direction, measured in a clockwise direction, between true north and the direction from which the wind is blowing. |
| ws | Wind speed in meters per second |
| ceil_hgt | The height above ground level of the lowest clould cover or other obscuring phenomena amounting to at least 5/8 sky coverate. Measured in meters.  |
| temp | Air temperature measured in degrees Celsius. |
| dew_point | The temperature in degrees Celsius to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur. |
| atmos_pres | The air pressure in hectopascals relative to Mean Sea Level (MSL). |
| rh | Relative humidity, measured as a percentage, as calculated using the August-Roche-Magnus approximation. |

## Additional traits
More traits can be downloaded in addition to the 7 default traits. Currently, no additional calculations have been with this data, look for updates and use at your own risk. Trait information can be found [here](https://www1.ncdc.noaa.gov/pub/data/ish/ish-format-document.pdf).

| trait | description |
| --- | --- |
|AA1	|liquid precipitation: period quantity, depth dimension|
|AB1	|liquid precipitation: monthly total|
|AC1	|precipitation observation history|
|AD1	|liquid precipitation, greatest amount in 24 hours, for the month|
|AE1	|liquid precipitation, number of days with specific amounts, for the month|
|AG1	|precipitation estimated observation|
|AH1	|liquid precipitation maximum short duration, for the month (1)|
|AI1	|liquid precipitation maximum short duration, for the month (2)|
|AJ1	|liquid precipitation maximum short duration, for the month (2)|
|AK1	|snow depth greatest depth on the ground, for the month|
|AL1	|snow accumulation|
|AM1	|snow accumulation greatest amount in 24 hours, for the month|
|AN1	|snow accumulation for the month|
|AO1	|liquid precipitation|
|AP1	|15-minute liquid precipitation|
|AU1	|present weather observation|
|AW1	|present weather observation |
|AX1	|past weather observation (1)|
|AY1	|past weather observation (2)|
|AZ1	|past weather observation (3)|
|CB1	|subhourly observed liquid precipitation: secondary sensor|
|CF1	|hourly fan speed|
|CG1	|subhourly observed liquid precipitation: primary sensor|
|CH1	|hourly/subhourly RH/temperatures|
|CI1	|hourly RH/temperatures|
|CN1	|hourly battery voltage|
|CN2	|hourly diagnostics|
|CN3	|secondary hourly diagnostics (1)|
|CN4	|secondary hourly diagnostics (2)|
|CR1	|CRN control|
|CT1	|subhourly temperatures|
|CU1	|hourly temperatures|
|CV1	|hourly temperature extremes|
|CW1	|subhourly wetness|
|CX1	|hourly geonor vibrating wire summary|
|CO1	|network metadata|
|CO2	|US cooperative network element time offset|
|ED1	|runway visual range|
|GA1	|sky cover layer|
|GD1	|sky cover summation state|
|GF1	|sky condition observation|
|GG1	|below station cloud layer|
|GH1	|hourly solar radiation|
|GJ1	|sunshine observation (1)|
|GK1	|sunshine observation (2)|
|GL1	| sunshine observation for the month|
|GM1	|solar irradiance|
|GN1	|solar radiation|
|GO1	|net solar radiation|
|GP1	|modeled solar irradiance|
|GQ1	|hourly solar angle|
|GR1	|hourly extraterrestrial radiation|
|HL1	|hail data|
|IA1	|ground surface data|
|IA2	|ground surface observation|
|IB1	|hourly surface temperature|
|IB2	|hourly surface temperature sensor|
|IC1	|ground surface observation - pan evaporation|
|KA1	|temperature data|
|KB1	|average air temperature|
|KC1	|extreme air temperature for the month|
|KD1	|heating/cooling degree days|
|KE1	|extreme temperatures, number of days exceeding criteria, for the month|
|KF1	|hourly calculated temperature|
|KG1	|average dew point and wet bulb temperature|
|MA1	|atmospheric pressure observation|
|MD1	|atmospheric pressure change|
|ME1	|geopotential height isobaric level|
|MF1	|atmospheric pressure observation (STP/SLP)|
|MG1	|atmospheric pressure observation|
|MH1	|atmospheric pressure observation - average station pressure for the month|
|MK1	|atmospheric pressure observation - maximum sea level pressure for the month|
|MV1	|present weather in vicinity observation|
|MW1	|present weather observation |
|OA1	|supplementary wine observation |
|OB1	|hourly/sub-hourly wind section|
|OC1	|wind gust observation|
|OE1	|summary of day wind observation|
|RH1	|relative humidity|
|SA1	|sea surface temperature observation|
|ST1	|soil temperature|
|UA1	|wave measurement|
|UG1	|wave measurement primary swell|
|UG2	|wave measurement secondary swell|
|WA1	|platform ice accretion|
|WD1	|water surface ice observation|
|WG1	|water surface ice historical observation|
