library(tidyverse)

cvds_prov = readr::read_rds('./data/gbd/Cardiovascular diseases/Cardiovascular_diseases.rds') |> 
  tibble::as_tibble()
cvds_prov = readr::read_csv('./data/gbd/Cardiovascular diseases/Cardiovascular_diseases_province.csv')
dalys_cvds = cvds_prov |> 
  dplyr::filter(measure == "DALYs (Disability-Adjusted Life Years)" &
                metric  == "Rate" &
                sex == "Both" &
                age == "Age-standardized")

cvds_prov = arrow::open_dataset("./data/gbd/Cardiovascular diseases/Cardiovascular_diseases_integrated.csv",format = "csv")

dalys_cvds = cvds_prov |> 
  dplyr::filter(measure == "DALYs (Disability-Adjusted Life Years)" &
                metric  == "Rate" &
                sex == "Both" &
                age == "Age-standardized") |> 
  dplyr::collect()

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

nations = readr::read_csv('./data/gbd/Nation_List.csv')
subnations = readr::read_csv('./data/gbd/Nation2Subnations.csv')
unique(subnations$Country)

fs::dir_ls('./data/gbd/map/', regexp = "^./data/gbd/map/MAP.*\\.geojson$") |> 
  sf::read_sf()

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
