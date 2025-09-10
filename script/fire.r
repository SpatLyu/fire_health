fs::dir_ls('./data/ignitions_shp/',regexp = ".shp$") |> 
  purrr::walk(\(.shp) {
    sf::read_sf(.shp) |> 
      sf::st_drop_geometry() |> 
      arrow::write_parquet(stringr::str_c("./data/ignitions/ignitions",
                                          stringr::str_sub(.shp,-8,-5), ".parquet"))
  })

world = geocn::load_world_country(center = "west") |> 
  dplyr::select(country = country2)

sf::sf_use_s2(FALSE)
ig2024 = arrow::read_parquet('./data/ignitions/ignitions2024.parquet') |> 
  sf::st_as_sf(coords = c("lon","lat"),
               crs = "epsg:4326") |> 
  sf::st_join(world)

ig2024 |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(size = mean(size),
                   n = dplyr::n(),
                   geometry = sf::st_union(geometry)) |> 
  sf::st_drop_geometry() |> 
  dplyr::right_join(world, by = "country") |> 
  sf::st_as_sf() -> ig24

ggplot2::ggplot() +
  ggplot2::geom_sf(data = ig24, ggplot2::aes(fill = size),
                   size = 0.5, color = "grey60") +
  ggplot2::scale_fill_viridis_c(
    option = "inferno", 
    na.value = "transparent",
    name = "Fire size",
    direction = -1
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = ig24, ggplot2::aes(fill = n),
                   size = 0.5, color = "grey60") +
  ggplot2::scale_fill_viridis_c(
    option = "inferno", 
    na.value = "transparent",
    name = "Fire size",
    direction = -1
  )


fire = terra::rast('./data/GeoTIFF_monthly_summaries/fire_size/Monthly_Qdeg_fire_size_202412.tif')
terra::plot(fire)

ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = fire) +
  tidyterra::scale_fill_grass_c(palette = "ryb") +
  # ggplot2::scale_fill_gradientn(
  #   colours = c("red", "darkred", "yellow"),
  #   na.value = "transparent",
  #   name = "Fire size"
  # ) +
  # ggplot2::scale_fill_viridis_c(
  #   option = "inferno",  # 火焰感配色
  #   na.value = "transparent",
  #   name = "Fire size",
  #   direction = -1
  # ) +
  ggplot2::geom_sf(data = world, fill = "transparent",
                   size = 0.5,color = "grey60") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent"),
    legend.position = "right"
  )
