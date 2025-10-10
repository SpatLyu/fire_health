sf::sf_use_s2(FALSE)

gaul = sf::read_sf('./data/gaul_location_match.gdb') |> 
  dplyr::select(country,location) |> 
  sf::st_make_valid()

gaul |> 
  dplyr::filter(!sf::st_is_valid(gaul))

fires = fs::dir_ls('./data/ignitions/',regexp = ".parquet$") |> 
  purrr::map_dfr(arrow::read_parquet) |> 
  dplyr::mutate(x = lon, y = lat) |> 
  sf::st_as_sf(coords = c("x", "y"), crs = "EPSG:4326")

sf::st_join(fires,gaul) |> 
  sf::st_drop_geometry() -> fires