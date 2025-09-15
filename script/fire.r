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

fires23.intensity = fires23 |> 
  sf::st_as_sf(coords = c("lon","lat"),
               crs = "epsg:4326") |> 
  sf::st_join(world) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(burned_area = sum(size)) |> 
  dplyr::right_join(world, by = "country") |> 
  sf::st_as_sf() |> 
  dplyr::mutate(burned_area = dplyr::if_else(is.na(burned_area),0,burned_area)) |> 
  dplyr::mutate(intensity_norm = burned_area / as.numeric(units::set_units(sf::st_area(geometry),"km^2")))

fs::dir_ls('./data',regexp = ".tif$") |> 
  terra::rast() -> air_pollutions
names(air_pollutions) = c("CO","NO2","O3","PM25","SO2")

terra::zonal(air_pollutions,terra::vect(world),fun = "mean",na.rm = TRUE) |> 
  tibble::as_tibble() |> 
  dplyr::bind_cols(fires23.intensity) |> 
  sf::st_as_sf() |> 
  dplyr::filter(country != "Antarctica") -> fires23.global

plot_element = \(col_name,legend_name){
  fig = ggplot2::ggplot() +
    ggplot2::geom_sf(data = world,
                     linewidth = 0.3, color = "grey60") +
    ggplot2::geom_sf(data = fires23.global,
                     ggplot2::aes(fill = .data[[col_name]]),
                     linewidth = 0.3, color = "grey60") +
    ggplot2::scale_fill_viridis_c(
      option = "inferno", 
      na.value = "transparent",
      name = legend_name,
      direction = -1
    ) +
    ggplot2::theme_bw()
  return(fig)
}

purrr::map2(c("CO","NO2","O3","PM25","SO2","burned_area"),
            c("CO","NO2","O3","PM2.5","SO2","Burned Area"),
            plot_element) |> 
  patchwork::wrap_plots(ncol = 2) +
  patchwork::plot_annotation(tag_levels = 'a') -> fig3
  
fig3 + ggview::canvas(9.5,6.5)
ggview::save_ggplot(fig3 + ggview::canvas(9.5,6.5),
                    "./figure/global_fires_variables.pdf",
                    device = cairo_pdf)


fires23.africa = fires23 |> 
  sf::st_as_sf(coords = c("lon","lat"),
               crs = "epsg:4326") |> 
  sf::st_join(world) |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(continent == "Africa")

fires23_month.africa = fires23.africa  |> 
  dplyr::group_by(month_date) |> 
  dplyr::summarise(
    n_fires = dplyr::n(),
    mean_size = mean(size),
    mean_perimeter = mean(perimeter),
    .groups = "drop"
  )

fig4a = ggplot2::ggplot(data = fires23_month.africa) +
  ggplot2::geom_line(ggplot2::aes(x = month_date, y = n_fires),
                     color = "firebrick", linewidth = 1) + 
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B"
  ) +
  ggplot2::labs(
    x = "Month",
    y = "Number of wildfire events (2023, Africa)",
    title = "Monthly African wildfire events in 2023"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

fig4b = ggplot2::ggplot(fires23_month.africa, ggplot2::aes(x = month_date)) +
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
    title = "Monthly Mean Fire Size and Perimeter in Africa(2023)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.05, 0.95),        
    legend.justification = c("left", "top"), 
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

fig4 = fig4a + fig4b +
  patchwork::plot_layout(nrow = 1) +
  patchwork::plot_annotation(tag_levels = 'a')
fig4 + ggview::canvas(10.5,5)
ggview::save_ggplot(fig4 + ggview::canvas(10.5,5),
                    "./figure/monthly_african_fires_ts.pdf",
                    device = cairo_pdf)


fires23.canada = fires23 |> 
  sf::st_as_sf(coords = c("lon","lat"),
               crs = "epsg:4326") |> 
  sf::st_join(world) |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(country == "Canada")

fires23_month.canada = fires23.canada  |> 
  dplyr::group_by(month_date) |> 
  dplyr::summarise(
    n_fires = dplyr::n(),
    mean_size = mean(size),
    mean_perimeter = mean(perimeter),
    .groups = "drop"
  )

fig5a = ggplot2::ggplot(data = fires23_month.canada) +
  ggplot2::geom_line(ggplot2::aes(x = month_date, y = n_fires),
                     color = "firebrick", linewidth = 1) + 
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B"
  ) +
  ggplot2::labs(
    x = "Month",
    y = "Number of wildfire events (2023, Canada)",
    title = "Monthly Canadian wildfire events in 2023"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

fig5b = ggplot2::ggplot(fires23_month.canada, ggplot2::aes(x = month_date)) +
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
    title = "Monthly Mean Fire Size and Perimeter in Canada(2023)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.45, 0.95),        
    legend.justification = c("left", "top"), 
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

fig5 = fig5a + fig5b +
  patchwork::plot_layout(nrow = 1) +
  patchwork::plot_annotation(tag_levels = 'a')
fig5 + ggview::canvas(10.5,5)
ggview::save_ggplot(fig5 + ggview::canvas(10.5,5),
                    "./figure/monthly_canadian_fires_ts.pdf",
                    device = cairo_pdf)

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

cor_mat = cor(sf::st_drop_geometry(dplyr::select(fires23.global,
                                                 -country,
                                                 -intensity_norm)), 
              use = "complete.obs")
corrplot::corrplot(cor_mat, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45, addCoef.col = "black")

nb = spdep::poly2nb(fires23.global)

spEDM::sc.test(fires23.global,"burned_area","PM25", k = 8)

spEDM::simplex(fires23.global,"burned_area","PM25")
spEDM::gccm(fires23.global,"burned_area","PM25", libsizes = seq(8,248,20), 
            E = 8 , k = 8) -> g1
plot(g1,ylimits = c(-0.05,0.15),xlimits = c(8,228))


fire = terra::rast('./data/GeoTIFF_monthly_summaries/fire_size/Monthly_Qdeg_fire_size_202307.tif')
terra::plot(fire)

ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = fire) +
  tidyterra::scale_fill_grass_c(palette = "ryb") +
  ggplot2::geom_sf(data = world, fill = "transparent",
                   size = 0.5,color = "grey60") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent"),
    legend.position = "right"
  )
