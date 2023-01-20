##  R code
## 데이터 전처리를 위한 패키지 설치 및 로딩
if(!require(readr)) {
  install.packages('readr')
  library(readr)
}

if(!require(readxl)) {
  install.packages('readxl')
  library(readxl)
}

if(!require(lubridate)) {
  install.packages('lubridate')
  library(lubridate)
}

if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

## 1. covid19 원본 데이터 셋 로딩
## covid19 데이터 로딩(파일을 다운로드 받은 경우)
# df_covid19 <- read_csv(file = "데이터저장경로/owid-covid-data.csv",
#                             col_types = cols(Date = col_date(format = "%Y-%m-%d")
#                                              )
#                             )
## covid19 데이터 로딩(온라인에서 바로 로딩할 경우)
df_covid19 <- read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
                       col_types = cols(Date = col_date(format = "%Y-%m-%d")
                       )
)
## 2. 전체 데이터셋 중 최근 100일간의 데이터를 필터링한 df_covid19_100 생성
df_covid19_100 <- df_covid19 |> 
  ## 한국 데이터와 각 대륙별 데이터만을 필터링
  filter(iso_code %in% c('KOR', 'OWID_ASI', 'OWID_EUR', 'OWID_OCE', 'OWID_NAM', 'OWID_SAM', 'OWID_AFR')) |>
  ## 읽은 데이터의 마지막 데이터에서 100일전 데이터까지 필터링
  filter(date >= max(date) - 100) |>
  ## 국가명을 한글로 변환
  mutate(location = case_when(
    location == 'South Korea' ~ '한국', 
    location == 'Asia' ~ '아시아', 
    location == 'Europe' ~ '유럽', 
    location == 'Oceania' ~ '오세아니아', 
    location == 'North America' ~ '북미', 
    location == 'South America' ~ '남미', 
    location == 'Africa' ~ '아프리카')) |>
  ## 국가 이름의 순서를 설정 
  mutate(location = fct_relevel(location, '한국', '아시아', '유럽', '북미', '남미', '아프리카', '오세아니아')) |>
  ## 날짜로 정렬
  arrange(date)


## 3. df_covid19_100을 한국과 각 대륙별열로 배치한 넓은 형태의 데이터프레임으로 변환
df_covid19_100_wide <- df_covid19_100 |>
  ## 날짜, 국가명, 확진자와, 백신접종완료자 데이터만 선택
  select(date, location, new_cases, people_fully_vaccinated_per_hundred) |>
  ## 열 이름을 적절히 변경
  rename('date' = 'date', '확진자' = 'new_cases', '백신접종완료자' = 'people_fully_vaccinated_per_hundred') |>
  ## 넓은 형태의 데이터로 변환
  pivot_wider(id_cols = date, names_from = location, 
              values_from = c('확진자', '백신접종완료자')) |>
  ## 날짜로 정렬
  arrange(date)

## 4. covid19 데이터를 국가별로 요약한 df_covid19_stat 생성
df_covid19_stat <- df_covid19 |> 
  group_by(iso_code, continent, location) |>
  summarise(인구수 = max(population, na.rm = T), 
            인당GDP = max(gdp_per_capita, na.rm = T),
            전체확진자수 = sum(new_cases, na.rm = T),
            전체사망자수 = sum(new_deaths, na.rm = T), 
            십만명당중환자실 = last(icu_patients_per_million),
            재생산지수 = last(reproduction_rate),
            봉쇄지수 = max(stringency_index), 
            전체검사자수 = max(total_tests, na.rm = T), 
            신규검사자수 = sum(new_tests, na.rm = T),
            전체백신접종자수 = max(total_vaccinations, na.rm = T),
            백신접종자완료자수 = max(people_fully_vaccinated, na.rm = T),
            부스터접종자수 = max(total_boosters, na.rm = T),
            인구백명당백신접종완료률 = max(people_fully_vaccinated_per_hundred, na.rm = T),
            인구백명당부스터접종자수 = max(total_boosters_per_hundred, na.rm = T)
  ) |> 
  ungroup() |>
  mutate(십만명당사망자수 = round(전체사망자수 / 인구수 *100000, 5),
         백신접종완료률 = 백신접종자완료자수 / 인구수)




############################################################################################

## R 코드

## R 코드

df_취업률 <- read_excel('d:/R/data/2020년 학과별 고등교육기관 취업통계.xlsx', 
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
## 실행결과 II - 1R. R의 plot_ly()를 사용한 초기화
## R에서 plotly 객체 초기화
## plot_ly(df_covid19_100)와 동일
df_covid19_100 |> 
  plot_ly()



## 긴 형태의 100일 코로나19 데이터에서
## 긴 형태의 100일 코로나19 데이터에서
df_covid19_100 |> 
  ## 한국 데이터만을 필터링 
  filter(iso_code == 'KOR') |>
  ## X축에 data, Y축에 new_cases를 매핑하여 plot_ly()로 시각화 생성
  plot_ly(type = 'scatter', x = ~date, y = ~new_cases, 
          mode = 'markers+lines', 
          marker = list(color = '#264E86'), 
          line = list(color = '#5E88FC', 
                      dash = 'dash'
          )
  ) 




margins_R <- list(t = 50, b = 25, l = 25, r = 25)

## 긴 형태의 100일 코로나19 데이터에서
df_covid19_100 |> 
  ## 한국 데이터만을 필터링 
  filter(iso_code == 'KOR') |>
  ## X축에 data, Y축에 new_cases를 매핑하여 plot_ly()로 시각화 생성
  plot_ly(type = 'scatter', x = ~date, y = ~new_cases, 
          mode = 'markers+lines', 
          marker = list(color = '#264E86'), 
          line = list(color = '#5E88FC', 
                      dash = 'dash'
          )
  ) |>
  layout(
    title = "코로나 19 발생 현황",
    xaxis = list(
      title = "날짜",
      showgrid = F),
    yaxis = list(title = "확진자수"), 
    margin = margins_R
  )





df_covid19_100 |> 
  ## 한국 데이터만을 필터링 
  filter(iso_code == 'KOR') |>
  ## X축에 data, Y축에 new_cases를 매핑하여 plot_ly()로 시각화 생성
  plot_ly(type = 'scatter', x = ~date, y = ~new_cases, 
          mode = 'markers+lines', 
          marker = list(color = '#264E86'), 
          line = list(color = '#5E88FC', 
                      dash = 'dash'
          )
  ) |>
  plotly_json()





df_취업률_500 |> 
  filter(졸업자수 < 500) |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, 
            marker = list(size = 3, color = 'darkblue'))





## df_취업률_500에서 
df_취업률_500 |> 
  ## X축은 졸업자수, Y축은 취업자수로 매핑한 plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수)






df_취업률_500 |>
  ## X축은 졸업자수, Y축은 취업자수, name은 대계열로 매핑한 plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, name = ~대계열)





## 긴 형태의 100일간 코로나19 데이터 중에
df_covid19_100 |>
  ## 국가명으로 그룹화
  group_by(location) |>
  ## 확진자수의 합계를 new_cases로 산출
  summarise(new_cases = sum(new_cases)) |>
  ## X축을 location, Y축과 text를 new_case로 매핑
  plot_ly() |>
  add_trace(type = 'bar', x = ~location, y = ~new_cases, text = ~new_cases) 





## 긴 형태의 100일간 코로나19 데이터 중에
df_covid19_100 |>
  ## 국가명으로 그룹화
  group_by(location) |>
  ## 확진자수의 합계를 new_cases로 산출
  summarise(new_cases = sum(new_cases)) |>
  ## X축을 location, Y축과 text를 new_case로 매핑
  plot_ly() |>
  add_trace(type = 'bar', x = ~location, y = ~new_cases, text = ~new_cases, 
            ## textposition을 'inside'로 설정
            textposition = 'inside')

df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~location, y = ~new_cases, text = ~new_cases, 
            ## textposition을 'outside'로 설정
            textposition = 'outside')

df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~location, y = ~new_cases, text = ~new_cases, 
            ## textposition을 'auto'로 설정
            textposition = 'auto')

df_covid19_100 |>
  group_by(location) |>
  summarise(new_cases = sum(new_cases)) |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~location, y = ~new_cases, text = ~new_cases, 
            ## textposition을 'none'으로 설정
            textposition = 'none')






## 긴 형태의 100일간 코로나19 데이터 중에
df_covid19_100 |>
  ## 국가명으로 그룹화
  group_by(location) |>
  ## 확진자수의 합계를 new_cases로 산출
  summarise(new_cases = sum(new_cases)) |>
  ## X축을 location, Y축과 text를 new_case로 매핑
  plot_ly() |>
  add_trace(type = 'bar', x = ~location, y = ~new_cases, text = ~new_cases, 
            ## textposition을 'inside'로 설정
            textposition = 'inside',
            ## texttemplate를 설정
            texttemplate = '확진자수:%{text:,}'
  )





df_취업률_500 |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수,
            hoverinfo = 'y')





df_취업률_500 |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수,
            ## hovertext를 학과명으로 매핑
            hovertext = ~paste0('중계열:', 중계열, '\n', '소계열:', 소계열))





df_취업률_500 |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, hovertext = ~대계열,
            ## hovertamplate의 설정
            hovertemplate = ' 졸업자:%{x}, 취업자:%{y}, 대계열:%{hovertext}')





df_취업률_500 |> 
  ## alpha를 0.3으로 설정
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, alpha = 0.3)

df_취업률_500 |> 
  ## opacity를 0.3으로 설정
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, opacity = 0.3)





df_취업률_500 |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, name = ~대계열,
            ## showlegend을 FALSE로 설정
            showlegend = FALSE)





R_layout_scatter <- df_취업률_500 |> 
  filter(졸업자수 < 500) |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수) |> 
  layout(title = list(text = '<b>졸업자 대비 취업자수</b>', 
                      x = 0.5, xanchor = 'center', yanchor = 'top')
  )

R_layout_scatter





R_layout_scatter |> 
  layout(title = list(text = "<span style = 'font-size:15pt'><span style = 'color:red;font-weight:bold;'> 졸업자</span><span style = 'font-size:10pt'> 대비</span> <span style = 'color:blue;font-weight:bold;'>취업자</span></span>", 
                      x = 0.5, xanchor = 'center', yanchor = 'top'))





R_layout_scatter <- R_layout_scatter |> 
  layout(paper_bgcolor = 'lightgray', plot_bgcolor = 'lightgray')

R_layout_scatter





R_layout_scatter <- R_layout_scatter |>
  layout(xaxis = list(title = list(text = '<b>학과 졸업자수</b><sub>(명)</sub>'), color =  'black',
                      zerolinecolor = 'black', zerolinewidth = 3, 
                      gridcolor = 'gray', gridwidth = 1), ## 정상적 방법 
         yaxis = list(title = '<b>학과 취업자수</b><sub>(명)</sub>', color = 'black',
                      zerolinecolor = 'black', zerolinewidth = 3, 
                      gridcolor = 'gray', gridwidth = 1) ## 약식 방법
  )

R_layout_scatter





R_layout_scatter <- R_layout_scatter |> 
  layout(xaxis = list(tickmode = 'array',
                      ticktext = c('소규모', '중규모', '대규모'),
                      tickvals = c(100, 300, 400)
  ),  ## 정상적 방법 
  yaxis = list(tickmode = 'linear', tick0 = 100, dtick = 100) ## 약식 방법
  )
R_layout_scatter





R_layout_scatter |>
  layout(xaxis = list(range = c(0, 350), rangemode = 'nonnegative'),
         yaxis = list(range = c(0, 300), rangemode = 'tozero'), 
         margin = list(pad = 5))





R_layout_line <- df_covid19_100_wide |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_한국, name = '한국')

R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_아시아, name = '아시아', showlegend = FALSE)

R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_유럽, name = '유럽')

R_layout_line <- R_layout_line |> 
  add_trace(type = 'scatter', mode = 'lines',
            x = ~date, y = ~확진자_북미, name = '북미')

R_layout_line <- R_layout_line |> 
  layout(title = list(text = '<b>대륙별 신규 확진자수 추이</b>',
                      x = 0.5, xanchor = 'center', yanchor = 'top'),
         legend = list(orientation = 'v',  bordercolor = 'gray', borderwidth = 2,
                       x = 0.95, y = 0.95, xanchor = 'right')
  )

R_layout_line





R_layout_line <- R_layout_line |> 
  layout(margin = list(t = 50, b = 25, l = 25, r = 25))

R_layout_line





R_layout_scatter |> 
  layout(width = 450, height = 700)





R_layout_line <- R_layout_line |> 
  layout(font = list(family = "나눔고딕", color = 'MidnightBlue', size = 12))

R_layout_line





## 서브플롯 생성을 위한 기본 plotly 객체 생성
p_line_wide <- df_covid19_100_wide |> plot_ly()

## 첫 번째 서브 플롯 생성
p1 <- p_line_wide |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_한국, name = '한국') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 두 번째 서브 플롯 생성
p2 <- p_line_wide |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_아시아, name = '아시아') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 세 번째 서브 플롯 생성
p3 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_유럽, name = '유럽') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 네 번째 서브 플롯 생성
p4 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_북미, name = '북미') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 댜섯 번째 서브 플롯 생성
p5 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_남미, name = '남미') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

## 여섯 번째 서브 플롯 생성
p6 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date,
            y = ~확진자_아프리카, name = '아프리카') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))
## 일곱 번째 서브 플롯 생성
p7 <- p_line_wide  |> 
  add_trace(type = 'scatter', mode = 'lines', x = ~date, 
            y = ~확진자_오세아니아, name = '오세아니아') |> 
  layout(xaxis = list(tickfont = list(size = 10)), 
         yaxis = list(title = list(text = '확진자수')))

subplots <- subplot(p1, p2, p3, p4, p5, p6, p7, nrows = 3) |>
  layout(## 전체 제목 설정
    title = '최근 100일간 코로나19 확진자수',
    ## 전체 여백 설정
    margin = margins_R
  )

subplots





df_covid19_100 |>
  ## 국가명으로 그룹화
  group_by(location) |>
  ## 그룹화한 각각의 데이터 그룹들에 적용할 코드 설정
  do(
    ## 각 그룹화한 데이터를 사용해 plotly 객체 생성    
    p = plot_ly(.) |> 
      ## line 모드의 스캐터 trace 추가
      add_trace(type = 'scatter', mode = 'lines',
                ## X, Y축에 변수 매핑, color를 설정
                x = ~date, y = ~new_cases, name = ~location) |>
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





subplots |> 
  ## 범례는 제거
  layout(showlegend = FALSE)





subplot(
  p1, p2, p3, plotly_empty(), p4, p5, plotly_empty(), p6, p7,
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
         margin = margins_R)





subplot(
  ## 아래의 nrows가 2이기 때문에 맨 위 열에 p1 하나를 위치시킴
  p1, 
  ## subplot()으로 p2부터 p7까지를 묶어 하나의 플롯으로 만듬
  subplot(p2, p3, p4, p5, p6, p7,
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
         margin = margins_R)






subplot(
  p1, 
  subplot(p2, p3, p4, p5, p6, p7,
          nrows = 2, 
          ## shareX, shareY를 TRUE로 설정하여 축 공유
          shareX = TRUE, shareY = TRUE), 
  nrows = 2, margin = 0.04
  ## shareX, shareY를 TRUE로 설정하여 축 공유
) |> 
  layout(showlegend = TRUE, 
         title = '최근 100일간 코로나19 확진자수',
         margin = margins_R)





ggplotly <- df_covid19_100 |> 
  ggplot(aes(x = date, y = new_cases, color = location )) +
  geom_line(aes(group = location, linetype = location)) +
  scale_x_date(breaks = '1 months') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = '날짜', y = '확진자수', linetype = '지역', color = '지역')

## ggplot객체를 plotly 객체로 변환
ggplotly(ggplotly)





