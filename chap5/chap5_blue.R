################### fig 5-1
total_deaths_5_nations_by_day <- df_covid19 |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  filter(!is.na(total_deaths_per_million))

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 5, by = -1)]
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-1', 
                                     height = 600, width = 900))


############ fig5-2

last_day = max(distinct(total_deaths_5_nations_by_day, date) |> pull()) + 180

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]) |>
  add_annotations( 
    x =~ (total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(date) |> pull()), 
    y = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(total_deaths_per_million) |> pull()),
    text = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(location) |> pull()), 
    textposition = 'middle right', xanchor = 'left', showarrow = FALSE
  ) |>
  add_annotations( 
    x = '2022-02-01', 
    y = ~(total_deaths_5_nations_by_day |> filter(date == '2022-02-01', iso_code == 'KOR') |> select(total_deaths_per_million) |> pull()),
    text = '설날', 
    textposition = 'middle right', xanchor = 'right'
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', range = c('2020-02-15', format(last_day, format="%Y-%m-%d"))), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R,
         showlegend = FALSE) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-2', 
                                     height = 600, width = 900))




################### fig 5-3

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', rangeslider = list(visible = T)), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         showlegend = T, margin = margins_R, 
         title='Time Series with Rangeslider',
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-3', 
                                     height = 600, width = 900))






################### fig 5-4
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         xaxis = list(title = '', 
                      range = c(min(total_deaths_5_nations_by_day$date),
                                max(total_deaths_5_nations_by_day$date)),
                      rangeslider = list(visible = T), 
                      rangeselector=list(
                        buttons=list(
                          list(count=7, label="1 Week before", step="day", stepmode="backward"),
                          list(count=1, label="1 month before", step="month", stepmode="backward"),
                          list(count=6, label="6 months before", step="month", stepmode="backward"),
                          list(count=1, label="new years day", step="year", stepmode="todate"),
                          list(count=1, label="1 year before", step="year", stepmode="backward")
                        )
                      )
         ),
         showlegend = T, margin = list(t = 75, b = 25, l = 25, r = 25)
  ) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-4', 
                                     height = 600, width = 900))




################### fig 5-5 **************************
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         hovermode="x")

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         hovermode="y")

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         hovermode="x unified")

total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         hovermode="y unified")



################### fig 5-6 ******************************
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', 
                      spikemode = 'across'), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      spikemode = 'toaxis'), 
         margin = margins_R, 
         hovermode="x")



################### fig 5-7
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', spikemode = 'across', tickformat = '%Y년 %m월',
                      dtick = 'M3'), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      spikemode = 'toaxis'), 
         margin = margins_R, 
         hovermode="x") |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-7', 
                                     height = 600, width = 900))



################### fig 5-8 *********************************
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location, colors = RColorBrewer::brewer.pal(7, 'Blues')[3:7]) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', spikemode = 'across', tickformat = '%Y년 %m월',
                      tickformatstops = list(
                        list(dtickrange=list(NULL, 1000), value="%H:%M:%S.%L 밀리초"),
                        list(dtickrange=list(1000, 60000), value="%H:%M:%S 초"),
                        list(dtickrange=list(60000, 3600000), value="%H:%M 분"),
                        list(dtickrange=list(3600000, 86400000), value="%H:%M 시"),
                        list(dtickrange=list(86400000, 604800000), value="%e. %b 일"),
                        list(dtickrange=list(604800000, "M1"), value="%e. %b 주"),
                        list(dtickrange=list("M1", "M12"), value="%b '%y 월"),
                        list(dtickrange=list("M12", NULL), value="%Y 년")
                      )
         ), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      spikemode = 'toaxis'), 
         margin = margins_R, 
         hovermode="x")





################### 5-9
library(tqk)
library(lubridate)
##  주가 코드를 가져옴
code <- code_get()

start_day = as.Date('2022-10-07')
end_day = as.Date('2023-01-13')

## 삼성전자 코드값을 가져옴
sse_code <- code |> filter(name == '삼성전자') |>
  select(code) |> pull()

##  삼성전자의 최근 100일 주가를 가져옴
samsung <- tqk_get(sse_code, from=start_day, to=end_day)
samsung |> head()

samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-9', 
                                     height = 600, width = 900))




################# 5-10
samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue'))
  ) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F)), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-9', 
                                     height = 600, width = 900))




####################### 5-11
fig1 <- samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue'))
  ) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F)),
         yaxis = list(title = '주가'),
         showlegend = FALSE)

fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color = I('gray'), showlegend = FALSE) |>
  layout(yaxis = list(title = '거래량'))

subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2,
        shareX = TRUE) |> layout(margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-9', 
                                     height = 600, width = 900))


######################## 5-12
fig1 <- samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue'))
  ) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F), 
                      rangebreaks=list(
                        list(bounds=list("sat", "mon")), 
                        list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
                      )
         ),
         yaxis = list(title = '주가'),
         showlegend = FALSE)

fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color =I('gray'), showlegend = FALSE) |>
  layout(xaxis = list(rangebreaks=list(
    list(bounds=list("sat", "mon")), 
    list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
  )
  ),
  yaxis = list(title = '거래량'))

subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2,
        shareX = TRUE) |>
  layout(margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-12', 
                                     height = 600, width = 900))






###################### 5-13
total_deaths_5_nations_since_100day <- total_deaths_5_nations_by_day |>
  filter((iso_code %in% c('KOR'))) |>
  filter(date > max(date)-100) 

p1 <- total_deaths_5_nations_since_100day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~new_cases , color = I('darkblue'), connectgaps = T
  )

p2 <- total_deaths_5_nations_since_100day |>
  filter((iso_code %in% c('KOR'))) |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~new_cases , color = I('darkblue'), connectgaps = T 
  ) |> 
  layout(xaxis = list(
    rangebreaks=list(
      list(bounds=list("sun", "tue")), 
      list(values=list('2022-03-02'))
    )
  )
  )

subplot(p1 |> 
          layout(annotations = list(x = 0.5 , y = 1.05, 
                                    text = "주말이 포함된 확진자수", showarrow = F, 
                                    xref='paper', yref='paper', xanchor = 'center')),
        p2 |> 
          layout(annotations = list(x = 0.5 , y = 1.05, 
                                    text = "주말이 제거된 확진자수", showarrow = F, 
                                    xref='paper', yref='paper', xanchor = 'center')), 
        nrows = 2, margin = 0.05) |>
  layout(title = '우리나라의 코로나19 확진자수 추세', 
         hovermode = "x unified", 
         margin = margins_R, showlegend = FALSE) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-13', 
                                     height = 600, width = 900))





####################### 5-14
if(!require(zoo)) {
  install.packages('zoo')
  library(zoo)
}

samsung_moving <- samsung %>% 
  mutate(MA_5 = zoo::rollmean(x = close, # column to take
                              k = 5, # rolling time period
                              align = "right", #leave values above the top
                              fill = NA), 
         MA_20 = zoo::rollmean(x = close, k = 20,  
                               align = "right", fill = NA), 
         MA_40 = zoo::rollmean(x = close, k = 40, 
                               align = "right", fill = NA)
  ) 

fig1 <- samsung_moving |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue')),
    showlegend = FALSE
  ) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F), 
                      rangebreaks=list(
                        list(bounds=list("sat", "mon")), 
                        list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
                      )
         ),
         yaxis = list(title = '주가'))

fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'solid', color = '#08306B'), 
                          x = ~date, y = ~MA_5, name = '5일 이동평균')

fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'dash', color = '#2171B5'), 
                          x = ~date, y = ~MA_20, name = '20일 이동평균')

fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'dot', color = '#6BAED6'), 
                          x = ~date, y = ~MA_40, name = '40일 이동평균')

fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color =I('gray'), showlegend = FALSE) |>
  layout(xaxis = list(rangebreaks=list(
    list(bounds=list("sat", "mon")), 
    list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
  )
  ),
  yaxis = list(title = '거래량'))

subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2,
        shareX = TRUE) |>
  layout(margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-14', 
                                     height = 600, width = 900))



################# 5-15
fig <- samsung |> mutate(lag = close - lag(close)) |>
  plot_ly()

fig |> add_trace(type = 'waterfall', 
                 name = "등락", orientation = "v",
                 x = ~date, y = ~lag,
                 increasing = list(marker = list(color = 'red')),
                 decreasing = list(marker = list(color = 'blue')
                 )
) |>
  layout(xaxis = list(rangeslider = list(visible = FALSE), 
                      rangebreaks = list(
                        list(bounds=c("sat", "mon")), #hide weekends
                        list(values=c("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10"))  # hide Christmas and New Year's
                      )), 
         yaxis = list(title_text="주가 등락(원)"),
         title = list(text = "삼성전자 주가 Waterfall Chart", x = 0.5),
         showlegend = FALSE, margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-15', 
                                     height = 600, width = 900))





################# 5-16
df_funnel <- df_취업률 |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')

df_funnel |>
  plot_ly() |>
  add_trace(type = 'funnel', x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, textinfo = "text+percent initial") |>
  layout(title = '유지취업자 Funnel Chart', 
         yaxis = list(categoryorder = "total descending"), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-16', 
                                     height = 600, width = 900))






################## 5-17
df_funnel_전문대학 <- df_취업률 |>
  filter(과정구분 == '전문대학과정') |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')

df_funnel_대학 <- df_취업률 |>
  filter(과정구분 == '대학과정') |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')

df_funnel_전문대학 |> 
  plot_ly() |>
  add_trace(type = 'funnel', name = '전문대학', 
            x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, texttemplate = '%{text:,.0f}',
            marker = list(color = '#3182BD')) |>
  add_trace(data = df_funnel_대학, type = 'funnel', name = '대학',
            x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, texttemplate = '%{text:,.0f}', 
            marker = list(color = '#9ECAE1')) |>
  layout(title = '유지취업자 Funnel Chart', 
         yaxis = list(categoryorder = "total descending"), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-17', 
                                     height = 600, width = 900))



######################## 5-18
df_funnelarea <- df_covid19_100_wide |>
  summarise(아프리카 = sum(확진자_아프리카), 
            아시아 = sum(확진자_아시아), 
            유럽 = sum(확진자_유럽), 
            북미 = sum(확진자_북미), 
            남미 = sum(확진자_남미), 
            오세아니아 = sum(확진자_오세아니아)) |>
  pivot_longer(1:6, names_to = '대륙', values_to = '전체확진자')

df_funnelarea |>
  plot_ly() |>
  add_trace(type = 'funnelarea', text = ~대륙, values = ~전체확진자, 
            textinfo = "text+value+percent") |>
  layout(title = '최근 100일간 대륙별 확진자수 Funnelarea 차트', 
         margin = margins_R, 
         funnelareacolorway = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 4, by = -1)]) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-18', 
                                     height = 600, width = 900))





######################## 5-19
df_sankey |> plot_ly(
  type = "sankey", orientation = "h",
  node = list(
    label = node,
    color = c(rep('lightblue', 3), rep('darkblue', 7)),
    pad = 15, thickness = 20,
    line = list(color = "black", width = 0.5)),
  link = list(
    source = ~과정구분_node,
    target = ~졸업구분_node,
    value =  ~학생수)
) |>
  layout(title = '대학과정별 졸업자의 졸업 후 진로', 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap5/fig/5-19', 
                                     height = 600, width = 900))
