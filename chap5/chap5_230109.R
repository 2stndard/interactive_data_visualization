pacman::p_load("tidyverse", "readxl", "readr", "lubridate", 'plotly')

## 1. covid19 원본 데이터 셋 로딩
## covid19 데이터 로딩(파일을 다운로드 받은 경우)
df_covid19 <- read_csv(file = "D:/R/git/datavisualization/plotly/RnPy/owid-covid-data_221203.csv",
                       col_types = cols(date = col_date(format = "%Y-%m-%d")
                       )
)
## covid19 데이터 로딩(온라인에서 바로 로딩할 경우)
# df_covid19 <- read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
#                             col_types = cols(Date = col_date(format = "%Y-%m-%d")
#                                              )
#                             )
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
            전체사망자수 = sum(new_deaths, na.rm = T), 
            백신접종자완료자수 = max(people_fully_vaccinated, na.rm = T),
            인구백명당백신접종완료율 = max(people_fully_vaccinated_per_hundred, na.rm = T),
            인구백명당부스터접종자수 = max(total_boosters_per_hundred, na.rm = T)) |> 
  ungroup() |>
  mutate(십만명당사망자수 = round(전체사망자수 / 인구수 *100000, 5),
         백신접종완료율 = 백신접종자완료자수 / 인구수)

## 여백 설정을 위한 변수 설정
margins_R <- list(t = 50, b = 25, l = 25, r = 25)


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

## df_취업률에서 졸업자가 500명 이하인 학과 중 25% 샘플링
df_취업률_500 <- df_취업률 |> 
  filter(졸업자_계 < 500) |>
  mutate(id = row_number()) |>
  filter(row_number() %in% seq(from = 1, to = nrow(df_취업률), by = 4))

## 열 이름을 적절히 설정
names(df_취업률_500)[10:12] <- c('졸업자수', '취업률', '취업자수')

################################################################################
## 5개국 데이터로 전처리
total_deaths_5_nations_by_day <- df_covid19 |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  filter(!is.na(total_deaths_per_million))

total_deaths_5_nations_by_day |>
  plot_ly() |>
  ## scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  ## layout의 제목, 축제목, 여백 속성 설정
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R)




################################################################################
## 마지막 일로부터 180일 후 날짜 계산
last_day = max(distinct(total_deaths_5_nations_by_day, date) |> pull()) + 180

total_deaths_5_nations_by_day |>
  plot_ly() |>
  ## scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  ## 각국의 마지막 일옆에 국가명 주석 추가 
  add_annotations( 
    x =~ (total_deaths_5_nations_by_day |> filter(date == max(date)) |> 
            select(date) |> pull()), 
    y = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> 
            select(total_deaths_per_million) |> pull()),
    text = ~(total_deaths_5_nations_by_day |> filter(date == max(date)) |> 
               select(location) |> pull()), 
    textposition = 'middle right', xanchor = 'left', showarrow = FALSE
  ) |>
  ## 설날 주석을 추가
  add_annotations( 
    x = '2022-02-01', 
    y = ~(total_deaths_5_nations_by_day |> 
            filter(date == '2022-02-01', iso_code == 'KOR') |> 
            select(total_deaths_per_million) |> pull()),
    text = '설날', 
    textposition = 'middle right', xanchor = 'right'
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', 
                      range = c('2020-02-15', format(last_day, format="%Y-%m-%d"))), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R,
         showlegend = FALSE)



################################################################################
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T
  ) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', 
                      ## rangeslider 속성 설정
                      rangeslider = list(visible = T)), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         showlegend = T, margin = margins_R, 
         title = 'Time Series with Rangeslider')




################################################################################
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         xaxis = list(title = '', 
                      range = c(min(total_deaths_5_nations_by_day$date),
                                max(total_deaths_5_nations_by_day$date)),
                      ## rangeslider 속성 설정
                      rangeslider = list(visible = T),
                      ##  rangeselector  속성 설정
                      rangeselector=list(
                        ## rangeselector의 buttons 속성 설정
                        buttons=list(
                          list(count=7, label='1 Week before', step='day', stepmode='backward'),
                          list(count=1, label='1 month before', step='month', stepmode='backward'),
                          list(count=6, label='6 months before', step='month', stepmode='backward'),
                          list(count=1, label='new years day', step='year', stepmode='todate'),
                          list(count=1, label='1 year before', step='year', stepmode='backward')
                        ))),
         showlegend = T, margin = list(t = 75, b = 25, l = 25, r = 25))



################################################################################
##  호버모드가 x인 시각화
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R,
         ## 호버 모드 설정
         hovermode="x")

##  호버모드가 y인 시각화
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         ## 호버 모드 설정
         hovermode="y")

##  호버모드가 x unified인 시각화
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         ## 호버 모드 설정
         hovermode="x unified")

##  호버모드가 y unified인 시각화
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계'), 
         margin = margins_R, 
         ## 호버 모드 설정
         hovermode="y unified")



################################################################################
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', 
                      ##  X축의 spikemode 설정
                      spikemode = 'across'), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      ##  Y축의 spikemode 설정
                      spikemode = 'toaxis'), 
         hovermode='x', 
         margin = margins_R)



################################################################################
total_deaths_5_nations_by_day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', spikemode = 'across',
                      ## X축 눈금 라벨 설정
                      tickformat = '%Y년 %m월',
                      ## X축 눈금 간격 설정
                      dtick = 'M3'), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      spikemode = 'toaxis'), 
         hovermode = 'x', 
         margin = margins_R)



################################################################################
total_deaths_5_nations_by_day |>
  ## plotly 객체 생성
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , linetype = ~location, connectgaps = T) |>
  layout(title = '코로나 19 사망자수 추세', 
         xaxis = list(title = '', spikemode = 'across', tickformat = '%Y년 %m월',
                      ##  tickformatstops 설정
                      tickformatstops = list(
                        ## 1000밀리초까지의 tickformat
                        list(dtickrange=list(NULL, 1000), value="%H:%M:%S.%L 밀리초"),
                        ## 1초 ~ 1분까지의 tickformat
                        list(dtickrange=list(1000, 60000), value="%H:%M:%S 초"),
                        ## 1분 ~ 1시간까지의 tickformat
                        list(dtickrange=list(60000, 3600000), value="%H:%M 분"),
                        ## 1시간 ~ 1일까지의 tickformat
                        list(dtickrange=list(3600000, 86400000), value="%H:%M 시"),
                        ## 1일 ~ 1주까지의 tickformat
                        list(dtickrange=list(86400000, 604800000), value="%e. %b 일"),
                        ## 1주 ~ 1월까지의 tickformat
                        list(dtickrange=list(604800000, "M1"), value="%e. %b 주"),
                        ## 1월 ~ 1년까지의 tickformat
                        list(dtickrange=list("M1", "M12"), value="%b '%y 월"),
                        ## 1년 이상의 tickformat
                        list(dtickrange=list("M12", NULL), value="%Y 년")
                      )), 
         yaxis = list(title = '10만명당 사망자수 누계', 
                      spikemode = 'toaxis'), 
         hovermode = 'x', 
         margin = margins_R)



################################################################################
##  관련 패키지 로딩
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



################################################################################
samsung |> plot_ly() |>
  add_trace(
    ## candlestick 트레이스를 추가
    type="candlestick", x = ~date,
    ## OHLC 데이터 설정
    open = ~open, close = ~close,
    high = ~high, low = ~low) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         margin = margins_R)



################################################################################
samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    ##  상승시 선 색상 설정
    increasing = list(line = list(color = 'red')), 
    ##  하락시 선 색상 설정
    decreasing = list(line = list(color = 'blue'))) |> 
  layout(title = "삼성전자 Candlestick Chart",
         ## rangeslider는 안보이도록 설정
         xaxis = list(rangeslider = list(visible = F)), 
         margin = margins_R)



################################################################################
fig1 <- samsung |> plot_ly() |>
  add_trace(
    type="candlestick", x = ~date,
    open = ~open, close = ~close,
    high = ~high, low = ~low, 
    increasing = list(line = list(color = 'red')), 
    decreasing = list(line = list(color = 'blue'))) |> 
  layout(title = "삼성전자 Candlestick Chart", 
         xaxis = list(rangeslider = list(visible = F)),
         yaxis = list(title = '주가'),
         showlegend = FALSE)

##  거래량 막대 그래프인 bar 트레이스 추가
fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color = I('gray'), showlegend = FALSE) |>
  layout(yaxis = list(title = '거래량'))

##  서브플롯으로 거래량 그래프 설정
subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2, shareX = TRUE) |>
  layout(margin = margins_R)




################################################################################
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
                      ##  rangebreaks 설정
                      rangebreaks=list(
                        ## 주말 제거
                        list(bounds=list("sat", "mon")), 
                        ## 특정 공휴일 제거
                        list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
                      )),
         yaxis = list(title = '주가'),
         showlegend = FALSE)

fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color =I('gray'), showlegend = FALSE) |>
  layout(xaxis = list(rangebreaks=list(   ##  rangebreaks 설정
    ## 주말 제거
    list(bounds=list("sat", "mon")), 
    ## 특정 공휴일 제거
    list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10", "2022-12-30"))
  )),
  yaxis = list(title = '거래량'))

subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2, shareX = TRUE) |>
  layout(margin = margins_R)



################################################################################
##  최근 100일간의 우리나라 코로나19 신규확진자 데이터 전처리
total_deaths_5_nations_since_100day <- total_deaths_5_nations_by_day |>
  filter((iso_code %in% c('KOR'))) |>
  filter(date > max(date)-100) 

##  주말이 포함된 scatter 트레이스 생성
p1 <- total_deaths_5_nations_since_100day |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~new_cases , color = I('darkblue'), connectgaps = T)

##   주말이 제거된 scatter 트레이스 생성
p2 <- total_deaths_5_nations_since_100day |>
  filter((iso_code %in% c('KOR'))) |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~new_cases , color = I('darkblue'), connectgaps = T) |> 
  layout(xaxis = list(rangebreaks=list(list(bounds=list("sun", "tue")), 
                                       list(values=list('2022-03-02'))
  )))
## 서브 플롯 생성
subplot(
  p1 |> layout(annotations = list(x = 0.5 , y = 1.05,
                                  text = "주말이 포함된 확진자수", showarrow = F,
                                  xref='paper', yref='paper', xanchor = 'center')),
  p2 |> layout(annotations = list(x = 0.5 , y = 1.05,
                                  text = "주말이 제거된 확진자수", showarrow = F, 
                                  xref='paper', yref='paper', xanchor = 'center')), 
  nrows = 2, margin = 0.05) |>
  layout(title = '우리나라의 코로나19 확진자수 추세', hovermode = "x unified", 
         margin = margins_R, showlegend = FALSE)




################################################################################
##  zoo 패키지 설치 및 로딩
if(!require(zoo)) {
  install.packages('zoo')
  library(zoo)
}

##  5일, 20일, 40일 이동평균 산출
samsung_moving <- samsung %>% 
  mutate(MA_5 = zoo::rollmean(x = close, k = 5, 
                              align = "right", fill = NA), 
         MA_20 = zoo::rollmean(x = close, k = 20,  
                               align = "right", fill = NA), 
         MA_40 = zoo::rollmean(x = close, k = 40, 
                               align = "right", fill = NA)) 

## casdlestick 트레이스 생성
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
                        list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10"))
                      )),
         yaxis = list(title = '주가'))

##  5일 이동평균선 추가
fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'solid'), 
                          x = ~date, y = ~MA_5, name = '5일 이동평균')

##  20일 이동평균선 추가
fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'dash'), 
                          x = ~date, y = ~MA_20, name = '20일 이동평균')

##  40일 이동평균선 추가
fig1 <- fig1 |> add_trace(type = 'scatter', mode = 'lines', 
                          line = list(dash = 'dot'), 
                          x = ~date, y = ~MA_40, name = '40일 이동평균')

##  거래량 그래프 추가
fig2 <- samsung %>% plot_ly() |>
  add_trace(type = 'bar', x=~date, y=~volume, type='bar',
            color =I('gray'), showlegend = FALSE) |>
  layout(xaxis = list(rangebreaks=list(
    list(bounds=list("sat", "mon")), 
    list(values = list("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10"))
  )),
  yaxis = list(title = '거래량'))

##  서브플롯 설정
subplot(fig1, fig2, heights = c(0.7,0.2), nrows=2, shareX = TRUE) |>
  layout(margin = margins_R)





################################################################################
## 전일 종가 대비 등락가 전처리
fig <- samsung |> mutate(lag = close - lag(close)) |>
  plot_ly()

## waterfall 트레이스 생성
fig |> add_trace(type = 'waterfall', 
                 name = "등락", orientation = "v",
                 x = ~date, y = ~lag,
                 increasing = list(marker = list(color = 'red')),
                 decreasing = list(marker = list(color = 'blue')
                 )
) |>
  layout(xaxis = list(rangeslider = list(visible = FALSE),
                      rangebreaks = list(
                        list(bounds=c("sat", "mon")), 
                        list(values=c("2022-09-09", "2022-09-12", "2022-10-03", "2022-10-10"))  
                      )), 
         yaxis = list(title_text="주가 등락(원)"),
         title = list(text = "삼성전자 주가 Waterfall Chart", x = 0.5),
         showlegend = FALSE, margin = margins_R)




################################################################################
##  퍼널 차트를 위한 데이터 전처리
df_funnel <- df_취업률 |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')

##  funnel 트레이스 생성
df_funnel |>
  plot_ly() |>
  add_trace(type = 'funnel', x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, textinfo = "text+percent initial") |>
  layout(title = '유지취업자 Funnel Chart', 
         yaxis = list(categoryorder = "total descending"), 
         margin = margins_R)



################################################################################
##  stack funnel 트레이스를 위한 전문대학 데이터 전처리
df_funnel_전문대학 <- df_취업률 |>
  filter(과정구분 == '전문대학과정') |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')

##  stack funnel 트레이스를 위한 대학 데이터 전처리
df_funnel_대학 <- df_취업률 |>
  filter(과정구분 == '대학과정') |>
  summarise(전체취업자 = sum(`취업자_교외취업자_계` + `취업자_교내취업자_계`), 
            유지취업자_1차 = sum(`1차 유지취업자_계`), 
            유지취업자_2차 = sum(`2차 유지취업자_계`), 
            유지취업자_3차 = sum(`3차 유지취업자_계`), 
            유지취업자_4차 = sum(`4차 유지취업자_계`), 
  ) |>
  pivot_longer(1:5, names_to = '구분', values_to = '유지취업자')


## stack funnel 트레이스 생성
df_funnel_전문대학 |> 
  plot_ly() |>
  add_trace(type = 'funnel', name = '전문대학', 
            x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, texttemplate = '%{text:,.0f}') |>
  add_trace(data = df_funnel_대학, type = 'funnel', name = '대학',
            x = ~유지취업자, y = ~구분, 
            text = ~유지취업자, texttemplate = '%{text:,.0f}') |>
  layout(title = '유지취업자 Funnel Chart', 
         yaxis = list(categoryorder = "total descending"), 
         margin = margins_R)



################################################################################
##  funnelarea 트레이스를 위한 데이터 전처리
df_funnelarea <- df_covid19_100_wide |>
  summarise(아프리카 = sum(확진자_아프리카), 
            아시아 = sum(확진자_아시아), 
            유럽 = sum(확진자_유럽), 
            북미 = sum(확진자_북미), 
            남미 = sum(확진자_남미), 
            오세아니아 = sum(확진자_오세아니아)) |>
  pivot_longer(1:6, names_to = '대륙', values_to = '전체확진자')

##  funnelarea 트레이스 생성
df_funnelarea |>
  plot_ly() |>
  add_trace(type = 'funnelarea', text = ~대륙, values = ~전체확진자, 
            textinfo = "text+value+percent") |>
  layout(title = '최근 100일간 대륙별 확진자수 Funnelarea 차트', 
         margin = margins_R)



################################################################################
df_sankey <- df_취업률 |> 
  ## 열 중에서 3열(과정구분, 왼쪽 노드로 사용)과 12열, 21열부터 26열(오른쪽 노드로 사용)까지를 선택
  select(3, 12, 21:26) |> 
  ## 과정구분 열을 사용하여 그룹화
  group_by(과정구분) |>
  ## 전체 열에 대해 `sum`을 적용(summarise_all은 전체 열에 동일한 요약함수를 적영하는 함수임)
  summarise_all(sum) |>
  ## 열이름을 적절히 변경
  rename(c('취업' = '취업자_합계_계', '진학' = '진학자_계', '취업불가' = '취업불가능자_계', '외국인' = '외국인유학생_계', '제외인정' = '제외인정자_계', '기타' = '기타_계', '미상' = '미상_계')) |>
  ##  첫번째 열을 제외하고 나머지 열들에 긴 형태의 데이터로 변환, 열 이름이 들어간 열은 '구분'으로 데이터 값이 들어간 열은 '학생수'열로 설정
  pivot_longer(cols = 2:8, names_to = '졸업구분', values_to = '학생수') |>
  ## 과정구분 열과 구분 열의 순서설정을 위해 팩터 레벨 설정
  mutate(과정구분_node = case_when(
    과정구분 == '전문대학과정' ~ 0, 
    과정구분 == '대학과정' ~ 1,
    과정구분 == '대학원과정' ~ 2),
    졸업구분_node = case_when(
      졸업구분 == '취업' ~ 3, 
      졸업구분 == '진학' ~ 4, 
      졸업구분 == '취업불가' ~ 5, 
      졸업구분 == '외국인' ~ 6, 
      졸업구분 == '제외인정' ~ 7, 
      졸업구분 == '기타' ~ 8, 
      졸업구분 == '미상' ~ 9)
  ) |>
  arrange(과정구분_node, 졸업구분_node)

head(df_sankey, 10)



################################################################################
## 왼쪽 노드로 사용할 변량을 from에 저장
from <- unique(as.character(df_sankey$과정구분))

## 오른쪽 노드로 사용할 변량을 to에 저장
to <- unique(as.character(df_sankey$졸업구분))

## 전체 노드 벡터 생성
node <- c(from, to)




################################################################################
##  sankey 트레이스 생성
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
    value =  ~학생수)) |>
  layout(title = '대학과정별 졸업자의 졸업 후 진로', 
         margin = margins_R)



################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




