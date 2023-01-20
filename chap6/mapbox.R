Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiMnN0bmRhcmQiLCJhIjoiY2xicnZjczloMDEzajNvbnk2d255ZHp6ayJ9.A35ianZRE4hWac0ekBDhKg')


df_univ <- read_excel("D:/R/git/datavisualization/plotly/RnPy/chap6/university.xlsx", 
                      col_types = c('text', 'numeric', 'numeric'))
                       
map_data_lev2 <- getData("GADM", country = "KOR", level = 2, type = "sf")

plot_dat_seoul <- plot_dat |> filter(GID_1 == 'KOR.16_1')

plot_mapbox(plot_dat_seoul) |>
  add_sf() |>
  add_markers(data = df_univ, 
              x = ~lon, y = ~lat, 
              text = ~학교명)
