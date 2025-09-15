fs::dir_ls('./data/ignitions_shp/',regexp = ".shp$") |> 
  purrr::walk(\(.shp) {
    sf::read_sf(.shp) |> 
      sf::st_drop_geometry() |> 
      arrow::write_parquet(stringr::str_c("./data/ignitions/ignitions",
                                          stringr::str_sub(.shp,-8,-5), ".parquet"))
  })

library(tidyverse)
library(patchwork)

fires23 = stringr::str_c("./data/ignitions/ignitions",2022:2024,".parquet") |> 
  purrr::map_dfr(arrow::read_parquet) |> 
  dplyr::mutate(start_date = lubridate::ymd(start_date),
                end_date   = lubridate::ymd(end_date)) |> 
  dplyr::filter(lubridate::year(start_date) >= 2023 &
                lubridate::year(start_date) < 2024) |> 
  dplyr::mutate(month_date = lubridate::floor_date(start_date, "month"),
                month_lab  = lubridate::month(month_date, label = TRUE, abbr = TRUE))

fires23_month = fires23 |> 
  dplyr::group_by(month_date) |> 
  dplyr::summarise(
    n_fires = dplyr::n(),
    mean_size = mean(size),
    mean_perimeter = mean(perimeter),
    .groups = "drop"
  )

Sys.setlocale("LC_TIME", "English")  
fig1a = ggplot2::ggplot(data = fires23_month) +
  ggplot2::geom_line(ggplot2::aes(x = month_date, y = n_fires),
                     color = "firebrick", linewidth = 1) + 
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B"
  ) +
  ggplot2::labs(
    x = "Month",
    y = "Number of wildfire events (2023, global)",
    title = "Monthly global wildfire events in 2023"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

fig1b = ggplot2::ggplot(fires23_month, ggplot2::aes(x = month_date)) +
  ggplot2::geom_line(ggplot2::aes(y = mean_size * 2, color = "Mean size"),
                     size = 1) +
  ggplot2::geom_line(ggplot2::aes(y = mean_perimeter, color = "Mean perimeter"),
                     size = 1, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "Mean fire perimeter",
    sec.axis = ggplot2::sec_axis(~./2, name = "Mean fire size")
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  ggplot2::scale_color_manual(values = c("Mean size" = "firebrick",
                                         "Mean perimeter" = "darkblue")) +
  ggplot2::labs(
    x = "Month",
    title = "Monthly Mean Fire Size and Perimeter (2023)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.05, 0.95),        
    legend.justification = c("left", "top"), 
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

fig1 = fig1a + fig1b +
  patchwork::plot_layout(nrow = 1) +
  patchwork::plot_annotation(tag_levels = 'a')
fig1 + ggview::canvas(10.5,5)
ggview::save_ggplot(fig1 + ggview::canvas(10.5,5),
                    "./figure/monthly_global_fires_ts.pdf",
                    device = cairo_pdf)

sf::sf_use_s2(FALSE)

world = geocn::load_world_country() |> 
  dplyr::select(country = country2)
  
fires23.country = fires23 |> 
  sf::st_as_sf(coords = c("lon","lat"),
               crs = "epsg:4326") |> 
  sf::st_join(world) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(country,month_lab) |> 
  dplyr::summarise(
    n_fires = dplyr::n(),
    mean_size = mean(size),
    mean_perimeter = mean(perimeter),
    .groups = "drop"
  ) |> 
  dplyr::right_join(world, by = "country") |> 
  sf::st_as_sf() |> 
  dplyr::filter(!is.na(month_lab))

fig2 = ggplot2::ggplot() +
  ggplot2::geom_sf(data = world,
                   linewidth = 0.3, color = "grey60") +
  ggplot2::geom_sf(data = fires23.country,
                   ggplot2::aes(fill = n_fires),
                   linewidth = 0.3, color = "grey60") +
  ggplot2::scale_fill_viridis_c(
    option = "inferno", 
    na.value = "transparent",
    name = "Number of fires",
    direction = -1
  ) +
  ggplot2::facet_wrap(~ month_lab) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.key.width = ggplot2::unit(2, "cm"),   
    legend.key.height = ggplot2::unit(0.5, "cm")
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(
      barwidth = 20,  
      barheight = 0.6 
    )
  )

fig2 + ggview::canvas(10.5,6.05)
ggview::save_ggplot(fig2 + ggview::canvas(10.5,6.05),
                    "./figure/monthly_global_fires_maps.pdf",
                    device = cairo_pdf)



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
  #   option = "inferno",
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
