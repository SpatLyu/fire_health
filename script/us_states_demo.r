dt = readr::read_csv('./data/us_fire_health.csv') |> 
  dplyr::filter(year >= 2001)

dt |> 
  dplyr::filter(state == 'California') |> 
  tibble::view()

fs::dir_ls('./data/gbd/',type = "directory") |> 
  stringr::str_sub(12,-1)