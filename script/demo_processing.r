library(tidyverse)

nations = readr::read_csv('./data/gbd/Nation_List.csv')
subnations = readr::read_csv('./data/gbd/Nation2Subnations.csv')
unique(subnations$Country)
us.states = subnations |> 
  dplyr::filter(Country == "United States of America") |> 
  dplyr::select(state = Subnations) |> 
  dplyr::pull(state)
us.states = us.states[-match("Georgia",us.states)]

.bind_us_disease = \(disease_type){
  disease_prov = stringr::str_c("./data/gbd/",disease_type,"/",disease_type,"_integrated.csv") |> 
    arrow::open_csv_dataset() |> 
    dplyr::filter(location %in% us.states) |> 
    dplyr::collect()
  disease_georgia = stringr::str_c("./data/gbd/",disease_type,"/",disease_type,"_province.csv") |> 
    arrow::open_csv_dataset() |> 
    dplyr::filter(location == "Georgia") |> 
    dplyr::collect()
  disease_us = dplyr::bind_rows(disease_prov,disease_georgia)
}

us_diseases = fs::dir_ls('./data/gbd/',type = "directory") |> 
  stringr::str_sub(12,-1) |> 
  purrr::map_dfr(.bind_us_disease)

arrow::write_parquet(
  us_diseases,
  "./data/gbd/us_diseases.parquet",
  compression = "zstd", 
  use_dictionary = TRUE  
)

usd = arrow::read_parquet('./data/gbd/us_diseases.parquet') |> 
  dplyr::filter(measure == "Incidence" &
                metric  == "Number" &
                sex == "Both" &
                age == "All ages") |> 
  dplyr::select(state = location,year,disease = cause,val) |> 
  dplyr::collect()

us_fires = arrow::read_parquet('./data/us_fires.parquet') |> 
  dplyr::select(year = ig_year,state = location, 
                fire_area = tot_ar_km2_split,
                fire_dur = event_dur) |> 
  dplyr::group_by(year,state) |> 
  dplyr::summarise(fire_area = sum(fire_area)) |> 
  dplyr::ungroup() |> 
  dplyr::collect()

usd = dplyr::left_join(usd, us_fires, by = dplyr::join_by(year,state))

fs::dir_ls('./data/air/concentration by monitor/') |> 
  purrr::walk(\(.x) zip::unzip(.x,exdir = './data/air/'))

airp = fs::dir_ls('./data/air/', regexp = ".csv$") |> 
  purrr::map_dfr(readr::read_csv) |> 
  dplyr::filter(`Certification Indicator` %in% c("Certification not required",
                                                 "Certified",
                                                 "Certified - QA issues identified")) |> 
  dplyr::filter((`Parameter Name` == "PM2.5 - Local Conditions" & `Sample Duration` == "24-HR BLK AVG") |
                (`Parameter Name` == "PM10 - LC" & `Sample Duration` == "24 HOUR") |
                (`Parameter Name` == "Ozone" & `Sample Duration` == "8-HR RUN AVG BEGIN HOUR") |
                (`Parameter Name` == "Carbon monoxide" & `Sample Duration` == "8-HR RUN AVG END HOUR") |
                (`Parameter Name` == "Nitrogen dioxide (NO2)" & `Pollutant Standard` == "NO2 Annual 1971") |
                (`Parameter Name` == "Formaldehyde" & `Sample Duration` == "24 HOUR") |
                (`Parameter Name` == "Average Ambient Temperature" & `Sample Duration` == "24 HOUR") |
                (`Parameter Name` == "Average Ambient Pressure" & `Sample Duration` == "24 HOUR") |
                (`Parameter Name` == "Relative Humidity" & `Sample Duration` == "1 HOUR") |
                (`Parameter Name` == "Wind Speed - Scalar" & `Sample Duration` == "1 HOUR") |
                (`Parameter Name` == "Wind Direction - Resultant" & `Sample Duration` == "1 HOUR"))

arrow::write_parquet(
  airp,
  "./data/air/us_air.parquet",
  compression = "zstd", 
  use_dictionary = TRUE  
)

