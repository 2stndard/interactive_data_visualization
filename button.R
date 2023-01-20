library(dlstats)

#x <- cran_stats(c("ggplot2", "plotly", "rbokeh", "highcharter"))
x1 <- cran_stats(c("ggplot2", "plotly", "rbokeh", "highcharter", 'lattice', 'esquisse', 'leaflet', 'dygraphs', 'ggvis', 'colourpicker', 'patchwork', 'ggforce'))

fig <- x1 |> plot_ly() |> 
  add_lines(x = ~end, y = ~downloads, color = ~package, colors = ~ifelse(package == 'plotly', 'darkblue', 'gray')) |>
  config(toImageButtonOptions = list(format = 'svg'))


plotly::export(fig, file = 'download.svg')

fig <- plotly_json(fig, FALSE)
write(fig,'ggplotly.json')


R_layout_scatter |> layout(title = list(text = "<span style = 'font-size:15pt'><span style = 'color:red;font-weight:bold;'> 졸업자</span><span style = 'font-size:10pt'> 대비</span> <span style = 'color:blue;font-weight:bold;'>취업률</span></span>", 
                                        x = 0.5, xanchor = 'center', yanchor = 'top')
) |>
  config(toImageButtonOptions = list(format = 'svg'))
