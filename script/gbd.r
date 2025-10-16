library(tidyverse)

nations = readr::read_csv('./data/gbd/Nation_List.csv')
subnations = readr::read_csv('./data/gbd/Nation2Subnations.csv')
unique(subnations$Country)
us.states = subnations |> 
  dplyr::filter(Country == "United States of America") |> 
  dplyr::select(state = Subnations)

# cvds_prov = readr::read_rds('./data/gbd/Cardiovascular diseases/Cardiovascular_diseases.rds') |> 
#   tibble::as_tibble()
# cvds_prov = readr::read_csv('./data/gbd/Cardiovascular diseases/Cardiovascular_diseases_province.csv')
# dalys_cvds = cvds_prov |> 
#   dplyr::filter(measure == "DALYs (Disability-Adjusted Life Years)" &
#                 metric  == "Rate" &
#                 sex == "Both" &
#                 age == "Age-standardized")

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

us_cvds = cvds_prov |> 
  dplyr::filter(measure == "Incidence" &
                metric  == "Number" &
                sex == "Both" &
                age == "All ages" &
                location %in% us.states$state) |> 
  dplyr::collect()

us_cvds |>
  dplyr::count(location,year) |>
  dplyr::arrange(dplyr::desc(n))

us_cvds |>
  dplyr::count(location,year) |>
  dplyr::filter(n == 2) |>
  dplyr::distinct(location)

us_cvds = arrow::open_dataset("./data/gbd/Cardiovascular diseases/Cardiovascular_diseases_province.csv",
                              format = "csv") |> 
  dplyr::filter(measure == "Incidence" &
                metric  == "Number" &
                sex == "Both" &
                age == "All ages" &
                location == "Georgia") |> 
  dplyr::collect() |> 
  dplyr::bind_rows(dplyr::filter(us_cvds,location != "Georgia"))

us_fires = arrow::read_parquet('./data/us_fires.parquet') |> 
  dplyr::select(ig_year,state = location, 
                fire_area = tot_ar_km2_split,
                fire_dur = event_dur) |> 
  dplyr::group_by(ig_year,state) |> 
  dplyr::summarise(fire_area = sum(fire_area)) |> 
  dplyr::ungroup() |> 
  dplyr::collect()

unique(us_fires$state)
unique(us_cvds$location)

match(unique(us_fires$state),unique(us_cvds$location))

df = us_fires |> 
  dplyr::filter(ig_year >= 2001 & ig_year <= 2023) |> 
  dplyr::left_join(us_cvds,
                   by = dplyr::join_by(state == location,
                                       ig_year == year)) |> 
  dplyr::select(state,year = ig_year,fire_area,val) |> 
  dplyr::arrange(year)

source('./script/utils.r')

cal_res = infocausality::surd(dplyr::filter(df,state == "California"),
                              "val", c("fire_area","fire_area","fire_area"),
                              lag = rep(2:4,times = 1), bin = 10, cores = 4)
utils_plot_surd(cal_res) + ggview::canvas(5,2)

tex_res = infocausality::surd(dplyr::filter(df,state == "Texas"),
                              "val", c("fire_area","fire_area","fire_area"),
                              lag = rep(2:4,times = 1), bin = 10, cores = 4)
utils_plot_surd(tex_res) + ggview::canvas(5,2)

tEDM::fnn(dplyr::filter(df,state == "Texas"), "val",
          eps = stats::sd(dplyr::pull(dplyr::filter(df,state == "Texas"),val)))

tEDM::simplex(dplyr::filter(df,state == "Texas"), "val", "fire_area")

tEDM::ccm(dplyr::filter(df,state == "Texas"), "val", "fire_area", 
          E = 3, k = 11, libsizes = 11:23)

df |> 
  dplyr::filter(state == "California")

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
