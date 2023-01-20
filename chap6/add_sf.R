library(raster)
library(tidyverse)
library(plotly)
library(sf)

# Get the map data in sf format
map_data <- getData("GADM", country = "KOR", level = 1, type = "sf")

# Transform sf data to modern crs object to avoid further warning message
st_crs(map_data) <- st_crs(map_data)
View(map_data)
# Generate some random data for each region
department <- map_data %>% as.data.frame() %>% .[, 10]

set.seed(10, sample.kind="Rounding") 
data <- sample(x = 0:1200, size = length(department), replace = T)

map_dat <- data.frame(department = department,
                      data = data)

map_dat <- map_dat %>% 
  mutate(Class = cut(data,
                     breaks = c(-Inf, 50, 100, 200, 500, 1000, Inf),
                     labels = c("< 50", "50 - 100", "100 - 200",
                                "200 - 500", "500 - 1000", "> 1000")))

plot_dat <- map_data %>% as.data.frame() %>%
  left_join(map_dat, by = c("HASC_1" = "department")) %>%
  st_as_sf()


plot_ly(plot_dat) %>%
  add_sf(type = "scatter", 
         stroke = I("transparent"), 
         span = I(1), 
         alpha = 1,
         split = ~NAME_1,
#         legendgroup = ~Class, # group legends together by class
#         name = ~Class,        # so region names aren't shown in the legend
         color = ~data,
         showlegend = F,       # don't show a legend for each region
         colors = "Blues",
         text = ~paste0(NAME_1, "\n", data),
         hoveron = "fills",
         hoverinfo = "text") %>%
  config(displayModeBar = F) %>%  
  layout(showlegend = T, traceorder = "grouped") 
