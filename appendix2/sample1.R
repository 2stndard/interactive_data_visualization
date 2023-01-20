library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

total_deaths_5_nations_by_day <- df_covid19 |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  filter(!is.na(total_deaths_per_million))

last_day = max(distinct(total_deaths_5_nations_by_day, date) |> pull()) + 180

fig <- total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T, 
            color = ~location) |>
  add_annotations( 
    x =~ (total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(date) |> pull()), 
    y = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(total_deaths_per_million) |> pull()),
    text = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> select(location) |> pull()), 
    textposition = 'middle right', xanchor = 'left', showarrow = FALSE
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', range = c('2020-02-15', format(last_day, format="%Y-%m-%d"))), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R,
         showlegend = FALSE)

max_deaths_per_million_by_day <- total_deaths_5_nations_by_day |> group_by(location) |>
  summarise(최대사망자 = max(new_deaths_per_million, na.rm = TRUE))

deaths_per_million_in_lateast <- total_deaths_5_nations_by_day |> group_by(location) |>
  filter(is.na(new_deaths_per_million) == FALSE) |>
  filter(date == max(date)) |>
  select(iso_code, date, new_deaths_per_million)

df_gauge <- left_join(max_deaths_per_million_by_day, deaths_per_million_in_lateast, by = 'location') |> arrange(location)

## 한국 게이지 인디케이터 생성
fig_gauge_kor <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
            value = pull(df_gauge[3, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[3, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[3, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.5, pull(df_gauge[3, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.75, pull(df_gauge[3, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[3, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 프랑스 게이지 인디케이터 생성
fig_gauge_fra <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
            value = pull(df_gauge[1, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[1, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[1, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.5, pull(df_gauge[1, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.75, pull(df_gauge[1, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[1, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 일본 게이지 인디케이터 생성
fig_gauge_jpn <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
            value = pull(df_gauge[2, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[2, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[2, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.5, pull(df_gauge[2, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.75, pull(df_gauge[2, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[2, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 영국 게이지 인디케이터 생성
fig_gauge_gbr <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
            value = pull(df_gauge[4, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[4, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[4, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.5, pull(df_gauge[4, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.75, pull(df_gauge[4, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[4, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 미국 게이지 인디케이터 생성
fig_gauge_usa <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
            value = pull(df_gauge[5, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[5, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[5, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.5, pull(df_gauge[5, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.75, pull(df_gauge[5, 2])*1.2), color = "gray")),
              threshold = list(line = list(color = 'white'),
                               value = pull(df_gauge[5, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## Dash 앱의 초기화
app <- dash_app()

##  Dash 앱 레이아웃 설정
app |> set_layout(
  ##  첫 번째 div 설정
  htmlDiv(children = list(
    ## 첫 레벨 제목 설정
    htmlH1(id = 'header', '코로나 19 사망자수 추세',
           ## 제목의 스타일 설정
           style = list(textAlign = 'center', color = 'darkblue'))
  )
  ),
  ##  두 번째 div 설정
  htmlDiv(children = list(
    ##  두 번째 좌측 div 설정
    htmlDiv(children = list(
      htmlLabel('날짜 선택', style = list(position = 'absolute', top = '10%', left = '5%')),
      dccDatePickerSingle(id = 'datepicker', date = max(distinct(total_deaths_5_nations_by_day, date) |> pull()), 
                          placeholder='Select a date', clearable=TRUE,
                          style = list(position = 'absolute', top = '15%', left = '3%', width = '10px'))),
      style = list(width = '20%', display = 'inline-block')),
    ##  두 번째 우측 div 설정
    htmlDiv(
      dccGraph(id = 'line_5_nations', figure=fig), 
      style = list(width = '80%', display = 'inline-block'))
  )
  ),
  htmlH2('인구 10만명당 사망자수',
         ## 제목의 스타일 설정
         style = list(textAlign = 'center', color = 'darkblue')),
  ##  세 번째 div  설정
  htmlDiv(id = 'third block', 
          children = list(
            dccGraph(id = 'indicator_kor',figure = fig_gauge_kor,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_fra',figure = fig_gauge_fra,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_jpn',figure = fig_gauge_jpn,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_gbr',figure = fig_gauge_gbr,
                     style = list(width = '20%', display = 'inline-block')),
            dccGraph(id = 'indicator_usa',figure = fig_gauge_usa,
                     style = list(width = '20%', display = 'inline-block'))
          )
  ), 
  htmlDiv(id = 'output')
)

app |> add_callback(
  list(
    output(id = 'indicator_kor', property = 'figure'), 
    output(id = 'indicator_fra', property = 'figure'), 
    output(id = 'indicator_jpn', property = 'figure'), 
    output(id = 'indicator_gbr', property = 'figure'), 
    output(id = 'indicator_usa', property = 'figure'), 
    output(id = 'line_5_nations', property = 'figure')),
  input(id = 'datepicker', property = 'date'),
  function(date_value) {
    
    deaths_per_million_update <- total_deaths_5_nations_by_day |> 
      filter(is.na(new_deaths_per_million) == FALSE) |>
      filter(date == date_value) |>
      select(location, new_deaths_per_million) |> arrange(location)
    ########
    ## 한국 게이지 인디케이터 생성
    fig_gauge_kor <- df_gauge |> plot_ly() |>
      add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
                value = pull(deaths_per_million_update[3, 2]), 
                gauge = list(axis = list(
                  range = list(NULL, pull(df_gauge[3, 2])*1.2)),
                  steps = list(
                    list(range = c(0, pull(df_gauge[3, 2])*1.2*0.5), color = "lightgray"),
                    list(range = c(pull(df_gauge[3, 2])*1.2*0.5, pull(df_gauge[3, 2])*1.2*0.75), color = "darkgray"),
                    list(range = c(pull(df_gauge[3, 2])*1.2*0.75, pull(df_gauge[3, 2])*1.2), color = "gray")),
                  threshold = list(line = list(color = 'white'),
                                   value = pull(df_gauge[3, 2])), 
                  bar = list(color = "darkblue")), 
                number = list(suffix = '명')) 
    
                ## 프랑스 게이지 인디케이터 생성
                fig_gauge_fra <- df_gauge |> plot_ly() |>
                  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
                            value = pull(deaths_per_million_update[1, 2]), 
                            gauge = list(axis = list(
                              range = list(NULL, pull(df_gauge[1, 2])*1.2)),
                              steps = list(
                                list(range = c(0, pull(df_gauge[1, 2])*1.2*0.5), color = "lightgray"),
                                list(range = c(pull(df_gauge[1, 2])*1.2*0.5, pull(df_gauge[1, 2])*1.2*0.75), color = "darkgray"),
                                list(range = c(pull(df_gauge[1, 2])*1.2*0.75, pull(df_gauge[1, 2])*1.2), color = "gray")),
                              threshold = list(line = list(color = 'white'),
                                               value = pull(df_gauge[1, 2])), 
                              bar = list(color = "darkblue")), 
                            number = list(suffix = '명'))
                
                ## 일본 게이지 인디케이터 생성
                fig_gauge_jpn <- df_gauge |> plot_ly() |>
                  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
                            value = pull(deaths_per_million_update[2, 2]), 
                            gauge = list(axis = list(
                              range = list(NULL, pull(df_gauge[2, 2])*1.2)),
                              steps = list(
                                list(range = c(0, pull(df_gauge[2, 2])*1.2*0.5), color = "lightgray"),
                                list(range = c(pull(df_gauge[2, 2])*1.2*0.5, pull(df_gauge[2, 2])*1.2*0.75), color = "darkgray"),
                                list(range = c(pull(df_gauge[2, 2])*1.2*0.75, pull(df_gauge[2, 2])*1.2), color = "gray")),
                              threshold = list(line = list(color = 'white'),
                                               value = pull(df_gauge[2, 2])), 
                              bar = list(color = "darkblue")), 
                            number = list(suffix = '명'))
                
                ## 영국 게이지 인디케이터 생성
                fig_gauge_gbr <- df_gauge |> plot_ly() |>
                  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
                            value = pull(deaths_per_million_update[4, 2]), 
                            gauge = list(axis = list(
                              range = list(NULL, pull(df_gauge[4, 2])*1.2)),
                              steps = list(
                                list(range = c(0, pull(df_gauge[4, 2])*1.2*0.5), color = "lightgray"),
                                list(range = c(pull(df_gauge[4, 2])*1.2*0.5, pull(df_gauge[4, 2])*1.2*0.75), color = "darkgray"),
                                list(range = c(pull(df_gauge[4, 2])*1.2*0.75, pull(df_gauge[4, 2])*1.2), color = "gray")),
                              threshold = list(line = list(color = 'white'),
                                               value = pull(df_gauge[4, 2])), 
                              bar = list(color = "darkblue")), 
                            number = list(suffix = '명'))
                
                ## 미국 게이지 인디케이터 생성
                fig_gauge_usa <- df_gauge |> plot_ly() |>
                  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
                            value = pull(deaths_per_million_update[5, 2]), 
                            gauge = list(axis = list(
                              range = list(NULL, pull(df_gauge[5, 2])*1.2)),
                              steps = list(
                                list(range = c(0, pull(df_gauge[5, 2])*1.2*0.5), color = "lightgray"),
                                list(range = c(pull(df_gauge[5, 2])*1.2*0.5, pull(df_gauge[5, 2])*1.2*0.75), color = "darkgray"),
                                list(range = c(pull(df_gauge[5, 2])*1.2*0.75, pull(df_gauge[5, 2])*1.2), color = "gray")),
                              threshold = list(line = list(color = 'white'),
                                               value = pull(df_gauge[5, 2])), 
                              bar = list(color = "darkblue")), 
                            number = list(suffix = '명'))
                
                fig_temp <- fig |> layout(shapes = list(type = 'line',
                                                        y0 = 0, y1 = max(total_deaths_5_nations_by_day$total_deaths_per_million), yref = "y", 
                                                        x0 = date_value, x1 = date_value,
                                                        line = list(color = 'black', dash="dot")))
                
                list(fig_gauge_kor, fig_gauge_fra, fig_gauge_jpn, fig_gauge_gbr, fig_gauge_usa, fig_temp)
                ########        
  }
)

##  Dash 앱 실행
app |> run_app()