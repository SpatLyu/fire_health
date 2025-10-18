require("magrittr")
require("geofacet")
source('./script/utils.r')

diseases = fs::dir_ls('./data/gbd/',type = "directory") |> 
  stringr::str_sub(12,-1)

dt = readr::read_csv('./data/us_fire_health.csv') |> 
  dplyr::filter(year >= 2001) |> 
  dplyr::arrange(year) %>% 
  dplyr::select(state,dplyr::all_of(c(diseases,names(.)[9:20])))

us.states = unique(dt$state)
match(us.states,geofacet::us_state_grid2$name)

analysis_diseases = \(state_name,disease = "Cardiovascular diseases",lag = 0){
  df = dt |> 
    dplyr::filter(state == state_name) |>
    dplyr::select(-state)
  
  res <- tryCatch({
    invisible(infocausality::surd(df, disease,
                                  c("fire_area","co","o3","pm10","no2","pm25"),
                                  lag = lag, bin = 5, cores = 4))
  }, error = function(e) {
    message("Error in state ", state_name, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(res)) return(NULL)
  
  return(utils_process_surd_result(res) |> 
           dplyr::filter(type %in% c("unique","info leak")) |> 
           dplyr::mutate(state = state_name))
}

plot_causation_us = \(disease = "Cardiovascular diseases",lag = 0){
  causation_lag = purrr::map_dfr(us.states,
                                 \(.x) analysis_diseases(.x,disease,lag)) |> 
    dplyr::filter(type != "info leak") |> 
    dplyr::mutate(label = dplyr::case_match(
      label,
      "U[X1]" ~ "Fire",
      "U[X2]" ~ "CO",
      "U[X3]" ~ "O3",
      "U[X4]" ~ "PM10",
      "U[X5]" ~ "NO2",
      "U[X6]" ~ "PM2.5",
    )) 
  
  ggplot2::ggplot(causation_lag, 
                  ggplot2::aes(label, value, fill = label)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    geofacet::facet_geo(~ state,grid = "us_state_grid2") +
    ggplot2::scale_x_discrete(name = paste0(disease," with Lag ",lag)) +
    ggplot2::scale_y_continuous(
      name = NULL,
      breaks = function(x) {
        rng = range(x, na.rm = TRUE)
        pretty(rng, n = 3)
      },
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_fill_discrete(name = NULL) +  
    ggplot2::guides(fill = ggplot2::guide_legend(
      direction = "horizontal",
      nrow = 1
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      legend.position = "inside", 
      legend.justification = c("right", "bottom"))
}

for (i in seq_along(diseases)) {
  purrr::map(0:3,\(.x) 
             ggview::save_ggplot(plot_causation_us(diseases[i],.x) + ggview::canvas(10,8),
                                 paste0("./figure/",diseases[i],"_causation_lag",.x,".pdf"),
                                 device = cairo_pdf))
}


fig1 = patchwork::wrap_plots(purrr::map(0:3,\(.x) 
                                        plot_causation_us(diseases[1],.x)), 
                             ncol = 2) +
  #patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'a')

fig1 + ggview::canvas(20,16)
ggview::save_ggplot(fig1 + ggview::canvas(20,16),
                    paste0("./figure/",diseases[1],"_causation.pdf"),
                    device = cairo_pdf)

plot_causation_us(diseases[1],3) + ggview::canvas(10,8)

cal_df = dt |> 
  dplyr::filter(state == "California") |> 
  dplyr::select(-state)

corr_matrix = cor(as.matrix(cal_df),
                  use = "pairwise.complete.obs")
corrplot::corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black")

cal_res = infocausality::surd(cal_df, "Cardiovascular diseases",
                              c("co","o3","pm10","no2","pm25"), lag = 0, bin = 5, cores = 4)
utils_process_surd_result(cal_res) |> 
  dplyr::filter(type == "unique")

cal_res1 = infocausality::surd(cal_df, "Cardiovascular diseases",
                              c("co","o3","pm10","no2","pm25"), lag = 1, bin = 5, cores = 4)
utils_process_surd_result(cal_res1) |> 
  dplyr::filter(type == "unique")
utils_plot_surd(cal_res) + ggview::canvas(5,2)

pfm = cbind(
  dplyr::filter(dt,state == "California")[,"Cardiovascular diseases",drop = TRUE],
  infocausality:::RcppGenTSLagMulti(as.matrix(dplyr::filter(dt,state == "California")[,names(dt)[9:20],drop = FALSE]),
                    rep(1,length.out = length(names(dt)[9:20])))
)