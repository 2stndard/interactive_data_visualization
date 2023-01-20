library(tidyverse)
library(readxl)
library(geojsonio)


df_입학자 <- read_excel('d:/R/git/datavisualization/chap3/2021_연도별 입학자수.xlsx', 
                     ## 'data' 시트의 데이터를 불러오는데,
                     sheet = 'Sheet0',
                     ## 앞의 10행을 제외하고
                     skip = 3, 
                     ## 첫번째 행은 열 이름을 설정
                     col_names = FALSE, 
                     ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                     col_types = c(rep('text', 2), rep('numeric', 30)))
df_입학자 <- df_입학자 |> select(1, 2, 5, 7, 9, 11, 13, 19, 29, 31)

## df_입학자의 열이름을 적절한 이름으로 설정
colnames(df_입학자) <- c('연도', '지역', '전문대학', '교육대학', '일반대학', '방송통신대학', '산업대학', '원격및사이버대학', '석사', '박사')

df_입학자 <- df_입학자 |> filter(!is.na(지역))

library(plotly)
library(rjson)

df_입학자_join <- df_입학자 |> filter(연도 == '2021', 지역 != '전체') |> 
  mutate(id = case_when(
    지역 == '강원' ~ '42', 
    지역 == '경기' ~ '41',
    지역 == '경남' ~ '48',
    지역 == '경북' ~ '47',
    지역 == '광주' ~ '29',
    지역 == '대구' ~ '27',
    지역 == '대전' ~ '30',
    지역 == '부산' ~ '26',
    지역 == '서울' ~ '11',
    지역 == '세종' ~ '36',
    지역 == '울산' ~ '31',
    지역 == '인천' ~ '28',
    지역 == '전남' ~ '46',
    지역 == '전북' ~ '45',
    지역 == '제주' ~ '50',
    지역 == '충남' ~ '44',
    지역 == '충북' ~ '43'
  ))

plotly_spdf <- rjson::fromJSON(file = "D:/R/git/datavisualization/chap10/TL_SCCO_CTPRVN.json")

fig <- plot_ly() %>% add_trace(
  type="choropleth",
  geojson=plotly_spdf, 
  featureidkey = 'properties.CTPRVN_CD', 
  locations = df_입학자_join$id, 
  z = df_입학자_join$일반대학, 
  colorscale="Blues", 
  text = df_입학자_join$지역,
  marker=list(line=list(
    width=1), opacity = 0.5
  ),
  colorbar = list(y = 0.5, yanchor = 'middle')
  )

fig <- fig %>% layout(
  title = '한국지도',
  geo=list(
    style="carto-positron",
    domain = list(x = c(0, 1), y = c(0, 1)),
    showframe = TRUE,
    fitbounds = "locations",
    visible = FALSE,
    center=list(lon=126.98, lat=37.56)
  ),
  autosize=FALSE,
  margin = list(t = 50, b = 25, l = 25, r = 25,
    pad=4,
    autoexpand=TRUE
  ))


fig

spdf_shp <- sf::st_read('D:/R/git/datavisualization/chap10/TL_SCCO_CTPRVN.shp', options = 'ENCODING=CP949')

inner_join(spdf_shp, df_입학자_join, by = c('CTPRVN_CD' = 'id')) |>
  plot_ly(split=~CTP_KOR_NM, color = ~일반대학, colors = "Blues",
          showlegend = FALSE) 



  plot_geo() |>
  add_trace(z = ~일반대학)


fig <- 

map_data("world", "South Korea")

map_data("world", "South Korea") %>%
  group_by(group) %>%
  plot_geo(x = ~long, y = ~lat) %>%
  add_markers(size = I(1))

sf_korea <- raster::getData('GADM', country='KOR', level = 1, type = "sf")

View(sf_korea)

department <- map %>% as.data.frame() %>% .[, 13]
