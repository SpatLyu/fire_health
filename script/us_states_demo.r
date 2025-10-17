require("magrittr")

diseases = fs::dir_ls('./data/gbd/',type = "directory") |> 
  stringr::str_sub(12,-1)

dt = readr::read_csv('./data/us_fire_health.csv') |> 
  dplyr::filter(year >= 2001) |> 
  dplyr::arrange(year) %>% 
  dplyr::select(state,dplyr::all_of(c(diseases,names(.)[9:20])))

us.states = unique(dt$state)
match(us.states,geofacet::us_state_grid2$name)

source('./script/utils.r')

analysis_diseases = \(state_name,disease = "Cardiovascular diseases",lag = 0){
  df = dt |> 
    dplyr::filter(state == state_name) |>
    dplyr::select(-state)
  
  res <- tryCatch({
    invisible(infocausality::surd(df, "Cardiovascular diseases",
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

causation_lag0 = purrr::map_dfr(us.states,analysis_diseases) |> 
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

causation_lag0 = causation_lag0 |> 
  dplyr::bind_rows(tidyr::crossing(label = c("Fire","CO","O3","PM10","NO2","PM2.5"),
                                   value = 1e-10,
                                   type = "unique",
                                   state = us.states[-match(unique(causation_lag0$state), us.states)]))

require("geofacet")
cl0 = ggplot2::ggplot(causation_lag0, 
                ggplot2::aes(label, value, fill = label)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  geofacet::facet_geo(~ state,grid = "us_state_grid2") +
  ggplot2::theme_bw()

cl0 + ggview::canvas(10,8)

a = analysis_diseases("Texas",1)
a = analysis_diseases("California",1)
tex_df = 

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