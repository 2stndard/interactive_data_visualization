library(plotly)
library(tidyverse)
if (!require("processx")) install.packages("processx")
fig <- total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 5, by = -1)]
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = list(text = '', font = list(family = '나눔고딕'))), 
         yaxis = list(title = list(text = '10만명당 사망자수 누계', font = list(family = '나눔고딕'))), 
         margin = margins_R)

orca(fig)

export(p = fig, #the graph to export
       file = "graph1.svg",
       selenium = RSelenium::rsDriver(browser = "chrome",
                                      extraCapabilities = list(
                                        chromeOptions = list(
                                          args = c("--window-size=1920,1080",
                                                   "--disable-gpu")
                                        )
                                      ),
                                      verbose = FALSE))

export(p = fig, #the graph to export
       file = "graph1.svg")
##https://github.com/plotly/plotly.R/issues/1142



htmlwidgets::saveWidget(
  widget = fig, #the plotly object
  file = "figure.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)


RColorBrewer::brewer.pal(9, 'Blues')


plot_ly() %>%
  add_trace(
    type = "choroplethmapbox",
    # See how this GeoJSON URL was generated at
    # https://plotly-r.com/data-raw/us-states.R
    geojson = paste(c(
      "https://gist.githubusercontent.com/cpsievert/",
      "7cdcb444fb2670bd2767d349379ae886/raw/",
      "cf5631bfd2e385891bb0a9788a179d7f023bf6c8/", 
      "us-states.json"
    ), collapse = ""),
    locations = row.names(state.x77),
    z = state.x77[, "Population"] / state.x77[, "Area"],
    span = I(0)
  ) %>%
  layout(
    mapbox = list(
      style = "light",
      zoom = 4,
      center = list(lon = -98.58, lat = 39.82)
    )
  ) %>%
  config(
    mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"),
    # Workaround to make sure image download uses full container
    # size https://github.com/plotly/plotly.js/pull/3746
    toImageButtonOptions = list(
      format = "svg", 
      width = NULL, 
      height = NULL
    )
  )



"https://gist.githubusercontent.com/cpsievert/7cdcb444fb2670bd2767d349379ae886/raw/cf5631bfd2e385891bb0a9788a179d7f023bf6c8/us-states.json"


data(trails, package = 'mapview')

plotly::plot_mapbox(maps::korea.cities)
