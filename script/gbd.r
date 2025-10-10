library(tidyverse)

nations = readr::read_csv('./data/gbd/Nation_List.csv')
subnations = readr::read_csv('./data/gbd/Nation2Subnations.csv')
unique(subnations$Country)
subnations |> 
  dplyr::filter(Country == "United States of America")

cvds_prov = readr::read_rds('./data/gbd/Cardiovascular diseases/Cardiovascular_diseases.rds') |> 
  tibble::as_tibble()
cvds_prov = readr::read_csv('./data/gbd/Cardiovascular diseases/Cardiovascular_diseases_province.csv')
dalys_cvds = cvds_prov |> 
  dplyr::filter(measure == "DALYs (Disability-Adjusted Life Years)" &
                metric  == "Rate" &
                sex == "Both" &
                age == "Age-standardized")



cvds_prov = arrow::open_dataset("./data/gbd/Cardiovascular diseases/Cardiovascular_diseases_integrated.csv",
                                format = "csv")

metric_cvds = cvds_prov |> 
  dplyr::distinct(measure,metric,sex,age) |> 
  dplyr::collect()

metric_cvds |> 
  dplyr::filter(measure == "Incidence" &
                metric  == "Number" &
                sex == "Both") |> 
  dplyr::distinct(age) |> 
  dplyr::pull(age)

dalys_cvds = cvds_prov |> 
  dplyr::filter(measure == "Incidence" &
                metric  == "Number" &
                sex == "Both" &
                age == "All ages") |> 
  dplyr::collect()

cal_cvds = dalys_cvds |> 
  dplyr::filter(location == "California")

uscan = sf::read_sf('../../download/uscan/fired_uscan_2000_to_2024_events.gpkg')



dalys_cvds |> 
  dplyr::count(location,year) |> 
  dplyr::arrange(dplyr::desc(n)) |> 
  dplyr::filter(n == 2) |> 
  dplyr::distinct(location)

dalys_cvds |> 
  dplyr::filter(location == "North Africa and Middle East") |> 
  dplyr::arrange(year)

dalys_cvds |> 
  dplyr::filter(location == "Punjab") |> 
  dplyr::arrange(year)

dalys_cvds |> 
  dplyr::filter(location == "South Asia") |> 
  dplyr::arrange(year)




sf::write_sf(gaul,'./data/gbd/gaul_location_match.gdb')


gaul  |> 
  dplyr::filter(country == "Norway")

fs::dir_ls('./data/gbd/map/', regexp = "^./data/gbd/map/MAP.*\\.geojson$") |> 
  purrr::map_dfr(sf::read_sf) |> 
  sf::st_cast("MULTIPOLYGON") -> subregions

sf::read_sf('./data/gbd/map/WORLD_MAP.geojson') -> global_maps

gaul = sf::read_sf('d:/project/GAUL2024/data/gaul2024.gdb/',
                   layer = "level1") |> 
  dplyr::select(Country = gaul0_name,
                Subnations = gaul1_name)

gaul |> 
  sf::st_drop_geometry() |> 
  dplyr::pull(Country) |> 
  unique()
  readr::write_csv('./data/gbd/gaul2024.csv')

subnations |> 
  dplyr::left_join(gaul,
                   by = dplyr::join_by(Country == Country,
                                       Subnations == Subnations)) -> subnations.sf

subnations.sf |> 
  sf::st_as_sf() |> 
  dplyr::filter(sf::st_is_empty(SHAPE)) |> 
  dplyr::distinct(Country)

subnations.sf |> 
  sf::st_as_sf() |> 
  dplyr::filter(sf::st_is_empty(SHAPE)) |> 
  dplyr::filter(Country == "South Africa")

gaul |> 
  dplyr::filter(Country == "South Africa")

gaul |> 
  dplyr::filter(Country == "Norway") |> 
  dplyr::pull(Subnations)

subnations |> 
  dplyr::filter(Country == "Norway")

subregion |> 
  dplyr::filter(Subnations == "Georgia")

nations |> 
  dplyr::filter(Country == "Georgia")

subregion |> 
  dplyr::filter(Subnations == "Punjab")

nations |> 
  dplyr::filter(Country == "Punjab")

nations |> 
  dplyr::filter(Country == "United States of America")

nations |> 
  dplyr::filter(Country == "Canada")
