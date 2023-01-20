library(showtext)
showtext_auto()

library(tidyverse)
library(readxl)
library(readr)
library(lubridate)

df_covid19 <- read_csv(file = "D:/R/data/Rnpy/owid-covid-data.csv",
                       col_types = cols(date = col_date(format = "%Y-%m-%d")
                       )
)
df_covid19_100 <- df_covid19 |> 
  filter(iso_code %in% c('KOR', 'OWID_ASI', 'OWID_EUR', 'OWID_OCE', 'OWID_NAM', 'OWID_SAM', 'OWID_AFR')) |>
  filter(date >= max(date) - 100) |>
  mutate(location = case_when(
    location == 'South Korea' ~ '한국', 
    location == 'Asia' ~ '아시아', 
    location == 'Europe' ~ '유럽', 
    location == 'Oceania' ~ '오세아니아', 
    location == 'North America' ~ '북미', 
    location == 'South America' ~ '남미', 
    location == 'Africa' ~ '아프리카')) |>
  mutate(location = fct_relevel(location, '한국', '아시아', '유럽', '북미', '남미', '아프리카', '오세아니아')) |>
  arrange(date)

df_covid19_100_wide <- df_covid19_100 |>
  select(date, location, new_cases, people_fully_vaccinated_per_hundred) |>
  rename('date' = 'date', '확진자' = 'new_cases', '백신접종완료자' = 'people_fully_vaccinated_per_hundred') |>
  pivot_wider(id_cols = date, names_from = location, 
              values_from = c('확진자', '백신접종완료자')) |>
  arrange(date)

df_covid19_stat <- df_covid19 |> 
  group_by(iso_code, continent, location) |>
  summarise(인구수 = max(population, na.rm = T), 
            전체사망자수 = sum(new_deaths, na.rm = T), 
            백신접종자완료자수 = max(people_fully_vaccinated, na.rm = T),
            인구백명당백신접종완료율 = max(people_fully_vaccinated_per_hundred, na.rm = T),
            인구백명당부스터접종자수 = max(total_boosters_per_hundred, na.rm = T)) |> 
  ungroup() |>
  mutate(십만명당사망자수 = round(전체사망자수 / 인구수 *100000, 5),
         백신접종완료율 = 백신접종자완료자수 / 인구수)

margins_R <- list(t = 50, b = 25, l = 25, r = 25)



############################################################################################

## R 코드

df_취업률 <- read_excel('D:/R/data/Rnpy/2021년 학과별 고등교육기관 취업통계.xlsx', 
                     ## '학과별' 시트의 데이터를 불러오는데,
                     sheet = '학과별',
                     ## 앞의 13행을 제외하고
                     skip = 13, 
                     ## 첫번째 행은 열 이름으로 설정
                     col_names = TRUE, 
                     ## 열의 타입을 설정, 처음 9개는 문자형으로 다음 79개는 수치형으로 설정
                     col_types = c(rep('text', 9), rep('numeric', 79)))

## df_취업률에서 첫번째부터 9번째까지의 열과 '계'로 끝나는 열을 선택하여 다시 df_취업률에 저장
df_취업률 <- df_취업률 |> 
  select(1:9, ends_with('계'), '입대자')

## df_취업률에서 졸업자가 500명 이하인 학과 2000개 샘플링
df_취업률_500 <- df_취업률 |> 
  filter(졸업자_계 < 500) |>
  mutate(id = row_number()) |>
  filter(row_number() %in% seq(from = 1, to = nrow(df_취업률), by = 4))

## 열 이름을 적절히 설정
names(df_취업률_500)[10:12] <- c('졸업자수', '취업률', '취업자수')


####################################################################################



if(!require(plotly)) {
  install.packages('plotly')
  library(plotly)
}



####################################################################################

## 긴 형태의 100일 코로나19 데이터에서
df_covid19_100 |> 
  ## 한국 데이터만을 필터링 
  filter(iso_code == 'KOR') |>
  ## scatter 트레이스의 markers와 lines 모드의 plotly 시각화 생성
  plot_ly(type = 'scatter', mode = 'markers+lines',
          ## X, Y 축에 변수 매핑
          x = ~date, y = ~new_cases,
          ## 마커 색상 설정
          marker = list(color = '#264E86'),
          ## 라인 색상과 대시 설정
          line = list(color = '#5E88FC', dash = 'dash')
  ) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-2'))


####################################################################################
df_취업률_500 |> 
  filter(졸업자수 < 500) |> 
  plot_ly() |>          ## plotly 초기화
  ## scatter 트레이스에 makers 모드 설정
  add_trace(type = 'scatter', mode = 'markers',  
            x = ~졸업자수, y = ~취업자수, 
            ## marker 사이즈와 색상 설정
            marker = list(size = 3, color = 'darkblue')) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-4'))


####################################################################################
df_취업률_500 |>
  ## X축은 졸업자수, Y축은 취업자수, name은 대계열로 매핑한 plotly 객체 생성
  plot_ly(x = ~졸업자수, y = ~취업자수, name = ~대계열, color = ~대계열, colors = 'Blues') |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-6'))



####################################################################################
p1 <- df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly(x = ~location, y = ~new_cases, text = ~new_cases, 
          textposition = 'inside', 
          textfont = list(color = 'white', size = 5.5), color = I('#1f77b4')) |> 
  layout(margin = margins_R)

p2 <- df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly(x = ~location, y = ~new_cases, text = ~new_cases, 
          textposition = 'outside', 
          textfont = list(color = 'black', size = 6), color = I('#1f77b4')) |> 
  layout(margin = margins_R)

p3 <- df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly(x = ~location, y = ~new_cases, text = ~new_cases, 
          textposition = 'auto', 
          textfont = list(size = 5.5), marker = list(color = '#1f77b4')) |> 
  layout(margin = margins_R)

p4 <- df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly(x = ~location, y = ~new_cases, text = ~new_cases, 
          textposition = 'none', 
          textfont = list(color = 'black', size = 7), color = I('#1f77b4')) |> 
  layout(title = list(text = '지역별 코로나19 확진자수'),
         xaxis = list(title = '지역', tickfont = list(size = 7)),
         yaxis = list(title = '확진자수'), 
         margin = margins_R)

subplot(
  p1 |> layout(annotations = list(x = 0.5 , y = 1.20, text = "textposition = 'inside'", showarrow = F, xref='paper', yref='paper', xanchor = 'center')),
  p2 |> layout(annotations = list(x = 0.5 , y = 1.2, text = "textposition = 'outside'", showarrow = F, xref='paper', yref='paper', xanchor = 'center')), 
  p3 |> layout(annotations = list(x = 0.5 , y = 1.2, text = "textposition = 'auto'", showarrow = F, xref='paper', yref='paper', xanchor = 'center')),
  p4 |> layout(annotations = list(x = 0.5 , y = 1.2, text = "textposition = 'none'", showarrow = F, xref='paper', yref='paper', xanchor = 'center')),
  nrows = 2, margin = 0.1
) |> hide_legend() |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-8'))


####################################################################################
df_취업률_500 |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수,
            hoverinfo = 'y') |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-10'))  ## hoverinfo 설정


####################################################################################
df_취업률_500 |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, hovertext = ~대계열,
            ## hovertamplate의 설정
            hovertemplate = ' 졸업자:%{x}, 취업자:%{y}, 대계열:%{hovertext}') |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-12'))  ## hoverinfo 설정



####################################################################################
df_취업률_500 |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, name = ~대계열, color = ~대계열, colors = 'Blues',
            ## showlegend을 FALSE로 설정
            showlegend = FALSE) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-14'))  ## hoverinfo 설정



####################################################################################
R_layout_scatter <- df_취업률_500 |> 
  filter(졸업자수 < 500) |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수) |> 
  ## title 속성의 설정
  layout(title = list(text = '<b>졸업자 대비 취업자수</b>', 
                      x = 0.5, xanchor = 'center', yanchor = 'top'))


R_layout_scatter |> 
  ## title의 HTML inline 설정
  layout(title = list(text = "<span style = 'font-size:15pt'><span style = 'color:red;font-weight:bold;'> 졸업자</span><span style = 'font-size:10pt'> 대비</span> <span style = 'color:blue;font-weight:bold;'>취업자</span></span>", 
                      x = 0.5, xanchor = 'center', yanchor = 'top')) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-16'))


####################################################################################
R_layout_scatter <- R_layout_scatter |>
  layout(xaxis = list(
    title = list(text = '<b>학과 졸업자수</b><sub>(명)</sub>'), ## 정상적 방법
    color =  'black', zerolinecolor = 'black', zerolinewidth = 3,
    gridcolor = 'gray', gridwidth = 1), 
    yaxis = list(
      title = '<b>학과 취업자수</b><sub>(명)</sub>', 
      color = 'black', zerolinecolor = 'black', zerolinewidth = 3,
      gridcolor = 'gray', gridwidth = 1) ## 약식 방법
  )

R_layout_scatter  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-18'))


####################################################################################
R_layout_scatter |>
  layout(xaxis = list(range = c(0, 350),  ## X축의 range 설정
                      rangemode = 'nonnegative'),  ## X축의 rangemode 설정
         yaxis = list(range = c(0, 300),  ## Y축의 range 설정
                      rangemode = 'tozero'), ## Y축의 rangemode 설정
         margin = list(pad = 5))  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-20'))


####################################################################################
R_layout_line <- df_covid19_100_wide |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_한국, name = '한국', color = I("#084594"))

R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_아시아, name = '아시아', showlegend = FALSE, color = I("#2171B5"))

R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_유럽, name = '유럽', color = I("#4292C6"))

R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_북미, name = '북미', color = I("#6BAED6"))

R_layout_line <- R_layout_line |> 
  layout(title = list(text = '대륙별 신규 확진자수 추이',
                      xanchor = 'center', yanchor = 'top'),
         legend = list(orientation = 'v',  bordercolor = 'gray', borderwidth = 2,
                       x = 0.95, y = 0.95, xanchor = 'right'), 
         showlegend = TRUE
  )

R_layout_line <- R_layout_line |> 
  ## 여백 설정
  layout(margin = list(t = 50, b = 25, l = 25, r = 25))

R_layout_line  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-22'))


####################################################################################
R_layout_line <- R_layout_line |> 
  ## 폰트 설정
  layout(font = list(family = "나눔고딕", color = 'MidnightBlue', size = 12))

R_layout_line  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-24'))


####################################################################################
## 서브플롯 생성을 위한 기본 plotly 객체 생성
p_line_wide <- df_covid19_100_wide |> plot_ly()

## 첫 번째 서브 플롯 생성
p1_blue <- p_line_wide |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_한국, color = I(RColorBrewer::brewer.pal(9, 'Blues')[9]), name = '한국') |> 
  layout(title = list(text = '한국'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 두 번째 서브 플롯 생성
p2_blue <- p_line_wide |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_아시아, color = I(RColorBrewer::brewer.pal(9, 'Blues')[8]), name = '아시아') |> 
  layout(title = list(text = '아시아'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 세 번째 서브 플롯 생성
p3_blue <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_유럽, color = I(RColorBrewer::brewer.pal(9, 'Blues')[7]), name = '유럽') |> 
  layout(title = list(text = '유럽'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 네 번째 서브 플롯 생성
p4_blue <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_북미, color = I(RColorBrewer::brewer.pal(9, 'Blues')[6]), name = '북미') |> 
  layout(title = list(text = '북미'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 댜섯 번째 서브 플롯 생성
p5_blue <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_남미, color = I(RColorBrewer::brewer.pal(9, 'Blues')[5]), name = '남미') |> 
  layout(title = list(text = '남미'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 여섯 번째 서브 플롯 생성
p6_blue <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date,
            y = ~확진자_아프리카, color = I(RColorBrewer::brewer.pal(9, 'Blues')[4]), name = '오세아니아') |> 
  layout(title = list(text = '아프리카'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 일곱 번째 서브 플롯 생성
p7_blue <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_오세아니아, color = I(RColorBrewer::brewer.pal(9, 'Blues')[3]), name = '아프리카') |> 
  layout(title = list(text = '오세아니아'),
         xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

subplot(p1_blue, p2_blue, p3_blue, p4_blue, p5_blue, p6_blue, p7_blue, nrows = 3) |>
  layout(## 전체 제목 설정
    title = '최근 100일간 코로나19 확진자수',
    ## 전체 여백 설정
    margin = margins_R
  )  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-25'))


####################################################################################
subplots_blue <- df_covid19_100 |>
  ## 국가명으로 그룹화
  group_by(location) |>
  ## 그룹화한 각각의 데이터 그룹들에 적용할 코드 설정
  do(
    ## 각 그룹화한 데이터를 사용해 plotly 객체 생성    
    p = plot_ly(.) |> 
      ## line 모드의 스캐터 trace 추가
      add_trace(type = 'scatter', mode = 'lines',
                ## X, Y축에 변수 매핑, color를 설정
                x = ~date, y = ~new_cases, color = ~location, 
                name = ~location, colors = c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF")) |>
      add_annotations(x = 0.5 , y = 1.02, text = ~location, 
                      showarrow = F, xref='paper', 
                      yref='paper', xanchor = 'center') |>
      ## layout으로 X, Y축을 설정
      layout(xaxis = list(tickfont = list(size = 10)),  
             yaxis = list(title = list(text = '확진자수')))
  ) |>
  ## 생성된 plotly 객체들을 subplot 생성
  subplot(nrows = 3, margin = 0.04) |>
  ## 생성된 subplot의 layout 설정
  layout(showlegend = TRUE, 
         title = '최근 100일간 코로나19 확진자수',
         margin = margins_R)

subplots_blue  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-26'))


####################################################################################
subplots_blue |> 
  ## 범례는 제거
  layout(showlegend = FALSE)  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-28'))


####################################################################################
subplot(
  p1_blue, p2_blue, p3_blue, plotly_empty(), p4_blue, p5_blue, plotly_empty(), p6_blue, p7_blue,
  ## 서브플롯은 3개의 열로 설정
  nrows = 3,
  ## 서브플롯간의 여백 설정
  heights = c(0.5, 0.25, 0.25), 
  widths = c(0.5, 0.25, 0.25), 
  margin = 0.04) |> 
  ## 범례는 제거
  layout(showlegend = TRUE,
         ## 전체 제목 설정
         title = '최근 100일간 코로나19 확진자수',
         ## 전체 여백 설정
         margin = margins_R)  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-29'))



####################################################################################
subplot(
  ## 아래의 nrows가 2이기 때문에 맨 위 열에 p1 하나를 위치시킴
  p1_blue, 
  ## subplot()으로 p2부터 p7까지를 묶어 하나의 플롯으로 만듬
  subplot(p2_blue, p3_blue, p4_blue, p5_blue, p6_blue, p7_blue,
          ## 서브플롯은 2개의 열로 설정함으로써 2행 3열 서브 플롯 생성
          nrows = 2), 
  ## 전체 서브 플롯은 2열로 구성
  nrows = 2
) |> 
  ## 범례는 제거
  layout(showlegend = TRUE,
         ## 전체 제목 설정
         title = '최근 100일간 코로나19 확진자수',
         ## 전체 여백 설정
         margin = margins_R)  |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap2/fig/vector/2-30'))


####################################################################################





