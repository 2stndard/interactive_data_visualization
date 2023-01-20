################################################################################
df_covid19 <- read_csv(file = "D:/R/git/datavisualization/plotly/RnPy/owid-covid-data_221203.csv",
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

## 5개국 데이터로 전처리
total_deaths_5_nations_by_day <- df_covid19 |> 
  filter((iso_code %in% c('KOR', 'USA', 'JPN', 'GBR', 'FRA'))) |>
  filter(!is.na(total_deaths_per_million))



################################################################################
## indicator 트레이스를 위한 데이터 전처리
number_KOR <- total_deaths_5_nations_by_day |>
  filter(date == max(date), iso_code == 'KOR') |>
  select(total_deaths_per_million) |> pull()

fig <- total_deaths_5_nations_by_day |>
  plot_ly() |>
  ## scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  ## layout의 제목, 축제목, 여백 속성 설정
  layout(title = list(text = '코로나 19 사망자수 추세', pad = list(b = 5)), 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계', domain = c(0, 0.8)), 
         margin = margins_R)

## number 모드의 indicator 트레이스 추가
fig |> add_trace(type = 'indicator', mode = 'number', value = number_KOR,  
                 title = list(text = paste0('<b>한국 코로나 사망자(10만명당)</b>\n', year(today()), '년', month(today()), '월', day(today()), '일'),
                              font = list(family = '나눔고딕', size = 15)
                 ),
                 ## number 속성 설정
                 number = list(font = list(family = '나눔고딕', size = 15), 
                               suffix = '명'),
                 domain = list(x = c(0.4, 0.6), y = c(0.8, 0.9)))



################################################################################
number1_KOR <- total_deaths_5_nations_by_day |>
  filter(date == max(date)-1, iso_code == 'KOR') |>
  select(total_deaths_per_million) |> pull()

fig <- total_deaths_5_nations_by_day |> plot_ly() |>
  ## scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'lines', 
            x = ~date, y = ~total_deaths_per_million , 
            linetype = ~location, connectgaps = T) |>
  ## layout의 제목, 축제목, 여백 속성 설정
  layout(title = list(text = '코로나 19 사망자수 추세', pad = list(b = 5)), 
         xaxis = list(title = ''), 
         yaxis = list(title = '10만명당 사망자수 누계', domain = c(0, 0.8)), 
         margin = margins_R)

## number+delta 모드의 indicator 트레이스 추가
fig |> add_trace(type = 'indicator', mode = 'number+delta', value = number_KOR,  
                 title = list(text = paste0('<b>한국 코로나 사망자(10만명당)</b>\n', year(today()), '년', month(today()), '월', day(today()), '일'),
                              font = list(family = '나눔고딕', size = 15)),
                 number = list(font = list(family = '나눔고딕', size = 15), 
                               suffix = '명'),
                 ## delta 속성 설정
                 delta = list(reference = number1_KOR, position = 'right', 
                              increasing = list(color = 'red'),
                              decreasing = list(color = 'blue'),
                              font = list(family = '나눔고딕', size = 10)),
                 domain = list(x = c(0.4, 0.6), y = c(0.8, 0.9)))



################################################################################
## 게이지 indicator 트레이스를 위한 데이터 전처리
max_deaths_per_million_by_day <- total_deaths_5_nations_by_day |> group_by(location) |>
  summarise(최대사망자 = max(new_deaths_per_million, na.rm = TRUE))

deaths_per_million_in_lateast <- total_deaths_5_nations_by_day |> group_by(location) |>
  filter(is.na(new_deaths_per_million) == FALSE) |>
  filter(date == max(date)) |>
  select(iso_code, date, new_deaths_per_million)

df_gauge <- left_join(max_deaths_per_million_by_day, deaths_per_million_in_lateast, by = 'location') |> arrange(location)

## 한국 게이지 인디케이터 생성
fig_gauge <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
            domain = list(row = 1, column = 1), value = pull(df_gauge[3, 5]), 
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
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
            domain = list(row = 0, column = 0), value = pull(df_gauge[1, 5]), 
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
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
            domain = list(row = 0, column = 2), value = pull(df_gauge[2, 5]), 
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
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
            domain = list(row = 2, column = 0), value = pull(df_gauge[4, 5]), 
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
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
            domain = list(row = 2, column = 2), value = pull(df_gauge[5, 5]), 
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

fig_gauge |> layout(grid=list(rows=3, columns=3), 
                    margin = margins_R, 
                    title = '10만명당 사망자수(최근 공식발표 기준)')



################################################################################
## 한국 불릿 인디케이터 생성
fig_gauge <- df_gauge |> plot_ly() |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[3, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.82, 0.9)),
            value = pull(df_gauge[3, 5]), 
            gauge = list(axis = list(
              range = list(NULL, pull(df_gauge[3, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[3, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.5, pull(df_gauge[3, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[3, 2])*1.2*0.75, pull(df_gauge[3, 2])*1.2), color = "gray")),
              shape = "bullet",
              threshold = list(
                line = list(color = 'white'), value = pull(df_gauge[3, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 프랑스 불릿 인디케이터 생성
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[1, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.62, 0.7)),
            value = pull(df_gauge[1, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[1, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[1, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.5, pull(df_gauge[1, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[1, 2])*1.2*0.75, pull(df_gauge[1, 2])*1.2), color = "gray")),
              shape = "bullet",
              threshold = list(
                line = list(color = 'white'), value = pull(df_gauge[1, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 일본 불릿 인디케이터 생성
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[2, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.42, 0.5)),
            value = pull(df_gauge[2, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[2, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[2, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.5, pull(df_gauge[2, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[2, 2])*1.2*0.75, pull(df_gauge[2, 2])*1.2), color = "gray")),
              shape = "bullet",
              threshold = list(
                line = list(color = 'white'), value = pull(df_gauge[2, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 영국 불릿 인디케이터 생성
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[4, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.22, 0.3)),
            value = pull(df_gauge[4, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[4, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[4, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.5, pull(df_gauge[4, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[4, 2])*1.2*0.75, pull(df_gauge[4, 2])*1.2), color = "gray")),
              shape = "bullet",
              threshold = list(
                line = list(color = 'white'), value = pull(df_gauge[4, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

## 미국 불릿 인디케이터 생성
fig_gauge <- fig_gauge |>
  add_trace(type = 'indicator', mode = "gauge+number", title = pull(df_gauge[5, 1]),
            domain = list(x = c(0.3,0.8), y = c(0.02, 0.1)),
            value = pull(df_gauge[5, 5]), 
            gauge = list(axis = list(
              shape = "bullet",
              range = list(NULL, pull(df_gauge[5, 2])*1.2)),
              steps = list(
                list(range = c(0, pull(df_gauge[5, 2])*1.2*0.5), color = "lightgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.5, pull(df_gauge[5, 2])*1.2*0.75), color = "darkgray"),
                list(range = c(pull(df_gauge[5, 2])*1.2*0.75, pull(df_gauge[5, 2])*1.2), color = "gray")),
              shape = "bullet",
              threshold = list(
                line = list(color = 'white'), value = pull(df_gauge[5, 2])), 
              bar = list(color = "darkblue")), 
            number = list(suffix = '명'))

fig_gauge |> layout(margin = margins_R, 
                    title = '10만명당 사망자수(최근 공식발표 기준)')



################################################################################
plot_ly() |>
  ##   scattergoe 트레이스 생성
  add_trace(type = 'scattergeo') |>
  layout(geo = list(resolution=50, 
                    showcoastlines=TRUE, coastlinecolor='RebeccaPurple',
                    showland=TRUE, landcolor='LightGreen',
                    showocean=TRUE, oceancolor='LightBlue',
                    showlakes=TRUE, lakecolor='white',
                    showrivers=TRUE, rivercolor='Blue'), 
         margin = list(r = 0, l = 0, t = 0, b = 0))




################################################################################
plot_ly() |>
  add_trace(type = 'scattergeo') |>
  layout(geo = list(resolution=50, scope = 'asia', 
                    showcountries=TRUE, countrycolor="black"), 
         margin = list(r = 0, l = 0, t = 0, b = 0))




################################################################################
## 
if(!require(raster)) {
  install.packages('raster')
  library(raster)
}

if(!require(sf)) {
  install.packages('sf')
  library(sf)
}

## 한국 지형 데이터를 불러옴
map_data <- getData("GADM", country = "KOR", level = 1, type = "sf")
st_crs(map_data) <- st_crs(map_data)  ## old map 데이터 경고를 없애기 위해 사용

subplot(nrows = 2,
        ggplot(map_data) + geom_sf(),
        plot_ly(map_data, color = I('skyBlue')),
        plot_geo(map_data, color = I('skyBlue')),
        plot_mapbox(map_data, color = I('skyBlue')), margin = 0.05) |> 
  hide_legend()




################################################################################
##  지도를 위한 패키지 로딩
if (!require(raster)) {
  install.packages('raster')
  library(raster) }

if (!require(sf)) {
  install.packages('sf')
  library(sf) }

####  충원율 데이터
df_충원율 <- read_excel('D:/R/git/datavisualization/plotly/RnPy/chap6/고등 주요 01-시도별 신입생 충원율(2010-2022)_220825y.xlsx', 
                     sheet = 'Sheet1', skip = 7, col_names = FALSE, 
                     col_types = c(rep('text', 2), rep('numeric', 12)))

df_충원율 <- df_충원율 |> dplyr::select(1, 2, 3, 4, 5)

## df_입학자의 열이름을 적절한 이름으로 설정
colnames(df_충원율) <- c('연도', '지역', '정원내모집인원', '정원내입학생수', '신입생충원율')

##  지형 데이터와 매칭을 위한 열 생성
df_충원율 <- df_충원율 |> filter(연도 == '2022', 지역 != '전국') |> 
  mutate(id = case_when(
    지역 == '강원' ~ 'KR.KW', 지역 == '경기' ~ 'KR.KG',
    지역 == '경남' ~ 'KR.KN', 지역 == '경북' ~ 'KR.KB',
    지역 == '광주' ~ 'KR.KJ', 지역 == '대구' ~ 'KR.TG',
    지역 == '대전' ~ 'KR.TJ', 지역 == '부산' ~ 'KR.PU',
    지역 == '서울' ~ 'KR.SO', 지역 == '세종' ~ 'KR.SJ',
    지역 == '울산' ~ 'KR.UL', 지역 == '인천' ~ 'KR.IN',
    지역 == '전남' ~ 'KR.CN', 지역 == '전북' ~ 'KR.CB',
    지역 == '제주' ~ 'KR.CJ', 지역 == '충남' ~ 'KR.GN',
    지역 == '충북' ~ 'KR.GB'))

##  sf 포맷으로 한국 지형 데이터를 가져옴 
map_data <- getData("GADM", country = "KOR", level = 1, type = "sf")
##  old crs 경고를 없애기 위해 사용
st_crs(map_data) <- st_crs(map_data)

plot_dat <- left_join(map_data, df_충원율, by = c("HASC_1" = "id")) %>%
  st_as_sf()

plot_ly(plot_dat) %>%
  add_sf(type = "scatter", 
         split = ~지역, color = ~신입생충원율,
         showlegend = F, colors = "Blues",
         text = ~paste0(지역, "\n", round(신입생충원율, 2), '%'),
         hoveron = "fills", hoverinfo = "text") %>%
  layout(title = '22년 전국 대학 신입생 충원율', 
         margin = margins_R) 



################################################################################
##  맵박스 토큰 설정
Sys.setenv("MAPBOX_TOKEN" = 'pk.eyJ1IjoiMnN0bmRhcmQiLCJhIjoiY2xicnZjczloMDEzajNvbnk2d255ZHp6ayJ9.A35ianZRE4hWac0ekBDhKg')




################################################################################
##  대학의 위경도 데이터 불러들임
df_univ <- read_excel("D:/R/git/datavisualization/plotly/RnPy/chap6/university.xlsx", 
                      col_types = c('text', 'numeric', 'numeric'))

plot_dat_seoul <- plot_dat |> filter(GID_1 == 'KOR.16_1')

plot_mapbox(plot_dat_seoul) |>
  add_trace(data = df_univ, type = 'scattermapbox', mode = 'markers+text', 
            x = ~lon, y = ~lat,
            marker = list(size = 10, symbol = 'marker'),
            text = ~학교명, textposition = 'top center', 
            textfont = list(color = 'blue')) |>
  layout(title = '서울지역 주요 대학',
         autosize=TRUE, hovermode='closest',
         mapbox=list(
           bearing=0, center=list(lon=126.98, lat=37.56),
           pitch=0, zoom=10, style="light"),
         margin = margins_R, 
         showlegend = FALSE)



