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
df_취업률_500 |> 
  plot_ly() |>
  add_markers(x = ~졸업자수, y = ~취업자수, color = ~대계열) |>
  layout(title = list(text = '<b>졸업자 대비 취업자수</b>', font = list(color = 'white')), 
         margin = list(t = 50, b = 25, l = 25, r = 25), 
         paper_bgcolor = 'black', plot_bgcolor = 'black', 
         xaxis = list(color = 'white', ticksuffix = '명'), 
         yaxis = list(color = 'white', gridcolor = 'gray', ticksuffix = '명', dtick = 100), 
         legend = list(font = list(color = 'white')))




####################################################################################

df_취업률_500 |> 
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~졸업자수, y = ~취업자수, color = ~대계열) |>
  layout(title = list(text = '<b>졸업자 대비 취업자수</b>', font = list(color = 'white')), 
         margin = list(t = 50, b = 25, l = 25, r = 25), 
         paper_bgcolor = 'black', plot_bgcolor = 'black', 
         xaxis = list(color = 'white', ticksuffix = '명'), 
         yaxis = list(color = 'white', gridcolor = 'gray', ticksuffix = '명', dtick = 100), 
         legend = list(font = list(color = 'white')))




####################################################################################

lm_trend <- lm(data = df_취업률_500, 취업자수 ~ 졸업자수)

loess_trend <- loess(data = df_취업률_500, 취업자수 ~ 졸업자수)

df_loess_trend <- data.frame(X = df_취업률_500$졸업자수, Y = fitted(loess_trend)) |>
  arrange(X)

df_취업률_500 |>
  plot_ly(type = 'scatter', mode = 'markers') |>
  add_trace(x = ~졸업자수, y = ~취업자수, showlegend = FALSE) |>
  add_trace(mode = 'lines', x = ~졸업자수, y = ~fitted(lm_trend), 
            name = '선형 추세선', line = list(dash = 'dot')) |>
  add_trace(data = df_loess_trend, mode = 'lines', 
            x = ~X, y = ~Y, name = 'loess 추세선'
  )





####################################################################################

p <- df_취업률_500 |>
  ggplot(aes(x = 졸업자수, y = 취업자수, color = 대계열)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  geom_smooth(method = 'loess', se= FALSE, linetype = 2)

ggplotly(p)





####################################################################################

df_covid19_stat |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers', 
            x = ~백신접종완료률, y = ~인구백명당부스터접종자수, text = ~location,
            marker = list(size = ~십만명당사망자수, opacity = 0.5, sizemode = 'area'))





####################################################################################

## 취업률 데이터를 사용해 plotly 객체 생성
p_histogram <- df_취업률_500 |> plot_ly()

p_histogram |> 
  ## histogram trace로 X축을 취업률로 매핑, name을 취업률로 설정
  add_histogram(x = ~취업률, name = '취업률', 
                xbins = list(start = 0, end = 100, size = 2.5)) |>
  ## 제목과 여백 설정
  layout(title = '취업률 histogram', margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

## 취업률 데이터를 사용해 plotly 객체 생성
p_histogram <- df_취업률_500 |> plot_ly()

p_histogram |> 
  ## histogram trace로 X축을 취업률로 매핑, name을 취업률로 설정
  add_histogram(x = ~취업률, color = ~과정구분,  opacity = 0.4,
                xbins = list(size = 5)) |>
  ## 제목과 여백 설정
  layout(title = '취업률 histogram', 
         barmode = "overlay",          
         margin = list(t = 50, b = 25, l = 25, r = 25)
  )





####################################################################################

## 취업률 데이터를 사용해 plotly 객체 생성
p_histogram <- df_취업률_500 |> plot_ly()

p_histogram |> 
  ## histogram trace로 X축을 취업률로 매핑, name을 취업률로 설정
  add_histogram(x = ~취업률, name = '취업률',
                xbins = list(start = 0, end = 100, size = 2.5), 
                cumulative = list(enabled=TRUE)) |>
  ## 제목과 여백 설정
  layout(title = '취업률 histogram', margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

p_histogram |> 
  add_trace(type = 'histogram',  ## add_histogram()과 동의 함수 
            x = ~대계열, 
            ## 히그토그램 막대 함수를 'count'로 설정
            histfunc = 'count') |>
  layout(title = '취업률 histogram', 
         yaxis = list(title = list(text = '학과수')), 
         margin = list(t = 50, b = 25, l = 25, r = 25))

p_histogram |> 
  add_trace(type = 'histogram', x = ~대계열, y = ~as.character(취업률), 
            ## 히그토그램 막대 함수를 'sum'으로 설정
            histfunc = 'sum') |>
  ## Y축을 선형으로 설정
  layout(yaxis=list(type='linear',title = list(text = '취업률 합계')), 
         title = '취업률 histogram', 
         margin = list(t = 50, b = 25, l = 25, r = 25))

p_histogram |> 
  add_trace(type = 'histogram', x = ~대계열, y = ~as.character(취업률), 
            ## 히그토그램 막대 값을 'average'로 설정
            histfunc = 'avg') |>
  ## Y축을 선형으로 설정
  layout(yaxis=list(type='linear',title = list(text = '취업률 평균')), 
         title = '취업률 histogram', 
         margin = list(t = 50, b = 25, l = 25, r = 25))

p_histogram |> 
  add_trace(type = 'histogram', x = ~대계열, y = ~as.character(취업률),
            ##히그토그램 막대 값을 'max'로 설정
            histfunc = 'max') |>
  ## Y축을 선형으로 설정
  layout(yaxis=list(type='linear',title = list(text = '취업률 최대값')), 
         title = '취업률 histogram', 
         margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

df_취업률 |> 
  plot_ly() |> 
  add_boxplot(x = ~대계열, y = ~취업률_계)|>
  ## boxmode를 group으로 설정
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

df_취업률 |> 
  plot_ly() |> 
  add_boxplot(x = ~대계열, y = ~취업률_계, boxmean = 'sd', notched = TRUE)|>
  ## boxmode를 group으로 설정
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

df_취업률 |> 
  plot_ly() |> 
  add_boxplot(x = ~대계열, y = ~취업률_계, 
              ## color를 과정구분으로 매핑
              color = ~과정구분)|>
  ## boxmode를 group으로 설정
  layout(boxmode = "group", title = list(text = '대학 계열별 취업률 분포'), 
         margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

fig <- df_covid19_100_wide |> plot_ly()

fig <- fig |> 
  add_boxplot(y = ~확진자_한국, name = '한국', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

fig <- fig |> 
  add_boxplot(y = ~확진자_아시아, name = '아시아', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

fig <- fig |> 
  add_boxplot(y = ~확진자_유럽, name = '유럽', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

fig <- fig |> 
  add_boxplot(y = ~확진자_북미, name = '북미', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

fig <- fig |> 
  add_boxplot(y = ~확진자_남미, name = '남미', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

fig <- fig |> 
  add_boxplot(y = ~확진자_아프리카, name = '아프리카', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

fig <- fig |> 
  add_boxplot(y = ~확진자_오세아니아, name = '오세아니아', boxpoints = "all", jitter = 0.3,
              pointpos = -1.8)

## boxmode를 group으로 설정
fig |>  layout(title = list(text = '한국 및 대륙별 일별 확진자 분포'), 
               xaxis = list(title = '대륙명'),
               yaxis = list(title = '확진자수(명)'),
               margin = list(t = 50, b = 25, l = 25, r = 25), 
               paper_bgcolor='lightgray', plot_bgcolor='lightgray')






####################################################################################

df_취업률_500 |> 
  plot_ly() |> 
  ## 바이올린 trace 추가
  add_trace(type = 'violin', x = ~대계열, y = ~취업률) |>
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

df_취업률_500 |> 
  plot_ly() |> 
  ## 바이올린 trace 추가
  add_trace(type = 'violin', x = ~대계열, y = ~취업률, 
            box = list(visible = TRUE),
            meanline = list(visible = TRUE)) |>
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = list(t = 50, b = 25, l = 25, r = 25))





####################################################################################

df_취업률_500 |> 
  plot_ly() |>   ## 대학과정을 필터링한 데이터 설정
  add_trace(data = df_취업률_500 |> filter(과정구분 == '대학과정'),
            ## 바이올린 trace로 추가
            type = 'violin', x = ~대계열, y = ~취업률, name = '대학', 
            ## side, box의 설정
            side = 'positive', box = list(visible = TRUE, width = 0.5), 
            ## meanline의 속성 설정
            meanline = list(visible = TRUE, width = 1)) |>
  ## 전문대학과정을 필터링한 데이터 설정
  add_trace(data = df_취업률_500 |> filter(과정구분 == '전문대학과정'), 
            type = 'violin', x = ~대계열, y = ~취업률, name = '전문대학', 
            side = 'negative', box = list(visible = TRUE, width = 0.5), 
            meanline = list(visible = TRUE, width = 1)) |> 
  layout(violinmode = "overlay", 
         title = list(text = '대학 계열별 취업률 분포'), 
         margin = list(t = 50, b = 25, l = 25, r = 25))



