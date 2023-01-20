library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(plotly)
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
df_취업률 |> group_by(대계열) |>
  summarise(취업률 = mean(취업률_계)) |> 
  plot_ly() |>
  ## bar 트레이스 추가
  add_trace(type = 'bar', x = ~대계열, y = ~취업률,
            ## text와 textposition 설정
            text = ~취업률, textposition = 'inside', 
            ## textemplate 설정
            texttemplate = '%{y:.1f}') |>
  layout(title = '계열별 취업률 평균', 
         ## 눈금 접미어 설정
         yaxis = list(ticksuffix = '%'),
         margin = margins_R)




################################################################################
## 인구수가 백만명 이상의 국가중에 인구백명당접종완료율 top 10 필터링
vaccine_top10 <- df_covid19_stat |>
  filter(인구수 > 10000000) |>
  top_n(10, 인구백명당백신접종완료율)

vaccine_top10 |> 
  plot_ly() |>
  add_trace(type = 'bar', 
            x = ~location, y = ~인구백명당백신접종완료율,
            color = ~continent, text = ~인구백명당백신접종완료율, 
            textposition = 'outside', texttemplate = '%{text}%', 
            textfont = list(color = 'black')) |>
  layout(title = '완전 백신 접종률 상위 top 10 국가', 
         xaxis = list(title = '국가명', categoryorder = 'total descending'), 
         yaxis = list(title = '백신접종완료율', ticksuffix = '%'), 
         margin = margins_R)




################################################################################
## 대륙별 백신접종완료율 top 5 필터링
vaccine_top5_by_continent <- df_covid19_stat |>
  filter(인구수 > 10000000, !is.na(continent)) |>
  group_by(continent) |>
  top_n(5, 인구백명당백신접종완료율) |>
  arrange(continent, desc(인구백명당백신접종완료율)) |>
  ungroup() |>
  mutate(seq = as.factor(seq(1:n())))

vaccine_top5_by_continent |> 
  plot_ly() |>
  add_trace(type = 'bar', 
            y = ~seq, x = ~인구백명당백신접종완료율,
            color = ~continent, 
            text = ~인구백명당백신접종완료율, textposition = 'outside', 
            texttemplate = '%{text}%', 
            textfont = list(color = 'black'), 
            orientation = 'v'
  ) |>
  layout(title = '대륙별 완전 백신 접종률 상위 top 5 국가', 
         xaxis = list(title = '백신접종완료율', 
                      ticksuffix = '%', range = c(0, 105)), 
         yaxis = list(title = '', autorange = 'reversed', 
                      tickvals = ~seq, ticktext = ~location), 
         margin = margins_R, size = list(height = 900)
  )



################################################################################
## 계열별 취업률을 넓은 데이터 형태로 전처리
취업률_by_계열 <- df_취업률 |>
  group_by(과정구분, 대계열) |>
  summarise(취업률 = mean(취업률_계)) |>
  pivot_wider(names_from = 과정구분, values_from = 취업률) 

취업률_by_계열 |> plot_ly() |> 
  ## 과정별로 bar 트레이스 추가
  add_trace(type = 'bar', x = ~대계열, y = ~ 전문대학과정, name = '전문대학과정') |> 
  add_trace(type = 'bar', x = ~대계열, y = ~ 대학과정, name = '대학과정') |>
  add_trace(type = 'bar', x = ~대계열, y = ~대학원과정, name = '대학원과정') |>
  ## barmode, bargroupgap 설정
  layout(barmode = 'group', bargroupgap = 0.2,
         title = '계열별 교육과정별 취업률 평균',
         margin = margins_R)



################################################################################
vaccine_top10 |> 
  plot_ly() |>
  ## bar 트레이스 추가
  add_trace(type = 'bar',
            x = ~location, y = ~인구백명당백신접종완료율,
            color = ~continent, text = ~인구백명당백신접종완료율, 
            textposition = 'outside', texttemplate = '%{text}%',
            textfont = list(color = 'black')) |>
  ## markers+text 모드인 scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'markers+text', 
            ## yaxis를 "y2"로 설정
            name = '10만명당 사망자수', yaxis = "y2",
            x = ~location, 
            y = ~십만명당사망자수, text = ~round(십만명당사망자수, 1), 
            textposition = 'top'
  )|>
  layout(title = '완전 백신 접종률 상위 top 10 국가', 
         xaxis = list(title = '국가명', categoryorder = 'total descending'), 
         yaxis = list(title = '백신접종완료율', 
                      ticksuffix = '%'),
         ## y2 축의 설정
         yaxis2 = list(title = '인구10만명당 사망자수', 
                       side = "right", overlaying = "y",
                       range = c(0, 300), ticksuffix = '명'),
         margin = margins_R,  legend = list(x = 1.1))




################################################################################
vaccine_top5_by_continent |> 
  plot_ly() |>
  ## bar 트레이스 추가
  add_trace(type = 'bar', 
            y = ~seq, x = ~인구백명당백신접종완료율, color = ~continent, 
            text = ~인구백명당백신접종완료율, textposition = 'outside', 
            texttemplate = '%{text}%', 
            textfont = list(color = 'black'), orientation = 'v'
  ) |>
  ## markers+text 모드인 scatter 트레이스 생성
  add_trace(type = 'scatter', mode = 'markers+text', 
            ## xaxis를 "x2"로 설정
            name = '사망자수', xaxis = "x2",
            y = ~seq, x = ~십만명당사망자수, color = I('black'),
            text = ~round(십만명당사망자수, 1), 
            textposition = 'middle right')|>
  layout(barmode = 'group', 
         title = list(text = '대륙별 완전 백신 접종률 상위 top 5 국가', 
                      y = 0.97, yref = 'container'), 
         xaxis = list(title = '백신접종완료율', range = c(0, 105), 
                      ticksuffix = '%'), 
         yaxis = list(title = '', autorange = 'reversed', 
                      tickvals = ~seq, ticktext = ~location), 
         ## xaxis2 축의 설정
         xaxis2 = list(title = list(text = '인구10만명당 사망자수', 
                                    standoff = 1), 
                       side = "top", overlaying = "x",
                       range = c(0, 700), ticksuffix = '명'),
         margin = list(r = 100, t = 80), 
         size = list(height = 900))



################################################################################
## 롤리팝 그래프를 위한 데이터 전처리
df_lolipop <- df_covid19_stat |>
  filter(인구수 > 5000000, continent == 'Asia') |>
  arrange(desc(인구백명당백신접종완료율))

df_lolipop |> 
  plot_ly(x = ~reorder(location, desc(인구백명당백신접종완료율))) |>
  ## 세그먼트 레이어 추가 
  add_segments(xend = ~reorder(location, desc(인구백명당백신접종완료율)),
               y = ~인구백명당백신접종완료율, 
               yend = 0, color = I('gray'), 
               showlegend = FALSE) |>
  ## markers 모드인 scatter 트레이스 추가
  add_trace(type = 'scatter', mode = 'markers', name = '접종완료율',
            y = ~인구백명당백신접종완료율, color = I('darkblue')) |>
  add_trace(type = 'scatter', mode = 'markers', 
            symbol = I('circle-open'),
            name = '사망자수', yaxis = "y2",
            y = ~십만명당사망자수, color = I('black'),
            text = ~round(십만명당사망자수, 1), 
            textposition = 'right')|>
  layout(barmode = 'group', 
         title = list(text = '아시아 국가의 백신접종율', 
                      y = 0.97, yref = 'container'), 
         yaxis = list(title = '백신접종완료율', range = c(0, 105), 
                      ticksuffix = '%'), 
         xaxis = list(title = ''),
         ## 두 번째 Y축의 설정
         yaxis2 = list(title = list(text = '인구10만명당 사망자수', 
                                    standoff = 10), 
                       side = "right", overlaying = "y",
                       range = c(0, 200), ticksuffix = '명'),
         margin = margins_R, 
         legend = list(orientation = 'h', y = -0.5, x = 0.5, 
                       yref = 'container', xanchor = 'center'),
         showlegend = T)



################################################################################
## 레이더 차트를 위한 데이터 전처리
df_radar_veccine <- df_covid19_stat |> 
  filter(iso_code %in% c('OWID_AFR', 'OWID_ASI', 'OWID_EUR', 'OWID_NAM', 'OWID_OCE', 'OWID_SAM')) |>
  select(continent, location, 인구백명당백신접종완료율)

df_radar_veccine |>
  plot_ly() |>
  ## scatterpolar 트레이스 추가
  add_trace(type = 'scatterpolar',  
            theta = ~location, r = ~인구백명당백신접종완료율, fill = 'toself') |>
  ## polar 속성 설정
  layout(polar = list(
    ##  angularaxis 속성 설정
    angularaxis = list(ticktext = c('아프리카', '아시아', '유럽', '북미', '오세아니아', '남미'), 
                       tickvals = c('Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America'), 
                       linewidth = 2, linecolor = 'black', gridcolor = 'gray'),
    ##  radialaxis 속성 설정
    radialaxis = list(linewidth = 2, linecolor = 'dodgerblue', gridcolor = 'skyblue',
                      nticks = 5, ticksuffix = '%', title = '백신 접종률')),
    title = list(text = '대륙별 백신 접종률', x = 0.5), 
    margin = margins_R)



################################################################################
##  덤벨 차트를 위한 데이터 전처리
df_covid19_stat |>
  filter(!is.na(continent), 인구수 > 10000000) |>
  group_by(continent) |>
  summarise(min = min(십만명당사망자수), max = max(십만명당사망자수)) |>
  plot_ly() |>
  ## 덤벨 차트용 세그먼트 추가
  add_segments( 
    x = ~min, xend = ~max, y = ~continent, yend = ~continent,
    showlegend = FALSE, 
    color = I('gray')) |>
  ## 최소값 트레이스 추가
  add_trace(type = 'scatter', mode = 'markers+text', 
            x = ~min, y = ~continent, name = '최소', 
            text = ~round(min, 1), textposition = 'bottom center', 
            color = I('#1f77b4')) |>
  ## 최대값 트레이스 추가
  add_trace(type = 'scatter', mode = 'markers+text', 
            x = ~max, y = ~continent, name = '최대',  
            text = ~round(max, 1), textposition = 'bottom center', 
            color = I('darkblue'), symbol = I('circle-open')) |>
  layout(title = '대륙별 10만명당 사망자수 차이', 
         xaxis = list(title = '10만명당 사망자수'), 
         yaxis = list(title = '', autorange = 'reversed'), 
         margin = margins_R)



################################################################################
##  비율 막대 그래프를 위한 데이터 전처리
df_covid19_stat |>
  filter(iso_code %in% c('OWID_HIC', 'OWID_LIC', 'OWID_LMC', 'OWID_UMC')) |>
  select(3, 5, 6, 7) |>
  pivot_longer(cols = c(2, 3, 4)) |>
  pivot_wider(names_from = location) |>
  group_by(name) |>
  mutate(sum = (`High income`+`Low income`+`Lower middle income`+`Upper middle income`)) |>
  mutate(`High income` = `High income` / sum, 
         `Low income` = `Low income` / sum, 
         `Lower middle income` = `Lower middle income` / sum, 
         `Upper middle income` = `Upper middle income` / sum) |>
  plot_ly() |>
  ##  'High income'을 위한 bar 트레이스 추가
  add_trace(type = 'bar', x = ~`High income`, y = ~name, 
            name = 'High income', orientation = 'h',
            marker = list(line = list(color = 'white', width = 2))) |>
  ##  'Upper middle income'을 위한 bar 트레이스 추가
  add_trace(type = 'bar', x = ~`Upper middle income`, y = ~name, 
            name = 'Upper middle income', orientation = 'h',
            marker = list(line = list(color = 'white', width = 2))) |>
  ##  'Lower middle income'을 위한 bar 트레이스 추가
  add_trace(type = 'bar', x = ~`Lower middle income`, y = ~name, 
            name = 'Lower middle income', orientation = 'h',
            marker = list(line = list(color = 'white', width = 2))) |>
  ##  'Low income'을 위한 bar 트레이스 추가
  add_trace(type = 'bar', x = ~`Low income`, y = ~name, 
            name = 'Low income', orientation = 'h',
            marker = list(line = list(color = 'white', width = 2))) |>
  ##  'High income' 값 표시를 위한 주석 레이어 추가
  add_annotations(xref = 'x', yref = 'y',
                  x = ~`High income` / 2, y = ~name,
                  text = ~paste(round(`High income`*100, 1), '%'),
                  font = list(color = 'white'),
                  showarrow = FALSE) |>
  ##  'High income' 값 표시를 위한 주석 레이어 추가
  add_annotations(xref = 'x', yref = 'y',
                  x = ~`High income` + `Upper middle income` / 2, y = ~name,
                  text = ~paste(round(`Upper middle income`*100, 1), '%'),
                  font = list(color = 'white'),
                  showarrow = FALSE) |>
  ##  'High income' 값 표시를 위한 주석 레이어 추가
  add_annotations(xref = 'x', yref = 'y',
                  x = ~`High income` + `Upper middle income` + `Lower middle income` / 2, 
                  y = ~name,
                  text = ~paste(round(`Lower middle income`*100, 1), '%'),
                  font = list(color = 'white'),
                  showarrow = FALSE) |>
  add_annotations(xref = 'x', yref = 'y',
                  x = ~`High income` + `Upper middle income` + `Lower middle income` + `Low income` / 2, y = ~name,
                  text = ~paste(round(`Lower middle income`*100, 1), '%'),
                  font = list(color = 'white'), 
                  showarrow = FALSE) |>
  layout(barmode = 'stack', 
         title = '국가 소득급간별 코로나19 현황',
         xaxis = list(title = '', tickformat = '.0%'), 
         yaxis = list(title = ''), 
         legend = list(orientation = 'h', traceorder = 'normal'), 
         margin = margins_R) 



################################################################################
df_취업률_500 |> group_by(대계열) |>
  summarise(졸업자수 = sum(졸업자수)) |> 
  plot_ly() |>
  ## value와 labels를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise') |> 
  layout(title = list(text = '대학 계열별 졸업생 분포'), 
         margin = margins_R)



################################################################################
p_pie <- df_취업률_500 |> group_by(대계열) |>
  summarise(졸업자수 = sum(졸업자수)) |> 
  plot_ly()

p_pie |>
  ## value를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            textinfo = 'value') |> 
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = margins_R)

p_pie |>
  ## value와 percent를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            textinfo = 'value + percent') |> 
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = margins_R)

p_pie |>
  ## value와 labels를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            textinfo = 'label+value') |> 
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = margins_R)

p_pie |>
  ## value와 percent를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            textinfo = 'label+percent') |> 
  layout(title = list(text = '대학 계열별 취업률 분포'), 
         margin = margins_R)



################################################################################
p_pie |>
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            ## hole을 사용한 도넛 차트
            textinfo = 'value', hole = 0.3) |> 
  add_annotations(x = 0.5, y = 0.5, text = '<b>졸업생수</b>', 
                  showarrow = FALSE, xanchor = 'center', 
                  font = list(size = 20)) |> 
  layout(title = list(text = '대학 계열별 졸업생 분포'), 
         margin = margins_R)





################################################################################
p_pie |>
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            textinfo = 'value', hole = 0.3, 
            ## 파이 차트 강조 설정
            pull = c(0, 0.2, 0, 0, 0, 0, 0)) |> 
  add_annotations(x = 0.5, y = 0.5, text = '<b>졸업생수</b>', 
                  showarrow = FALSE, xanchor = 'center', 
                  font = list(size = 20)) |> 
  layout(title = list(text = '대학 계열별 졸업생 분포'), 
         margin = margins_R)




################################################################################
## 선버스트 차트를 위한 데이터 전처리
df_sunburst <- df_취업률_500 |> group_by(대계열, 중계열) |>
  summarise(졸업자수 = sum(졸업자수))

all_sum <- sum(df_sunburst$졸업자수)
계열_sum <- df_sunburst |> group_by(대계열) |>
  summarise(sum = sum(졸업자수)) |>
  select(sum) |> pull()

df_sunburst |> plot_ly() |>
  add_trace(type = 'sunburst', 
            ## sunburst 트레이스의 labels 설정
            labels = c('전체', unique(df_sunburst$대계열), df_sunburst$중계열), 
            ## sunburst 트레이스의 parents 설정
            parents = c('', rep('전체', 7), df_sunburst$대계열), 
            ## sunburst 트레이스의 values 설정
            values = c(all_sum, 계열_sum, df_sunburst$졸업자수), 
            branchvalues = 'total')



################################################################################
df_sunburst <- df_취업률_500 |> group_by(대계열, 중계열) |>
  summarise(졸업자수 = sum(졸업자수))

all_sum <- sum(df_sunburst$졸업자수)
계열_sum <- df_sunburst |> group_by(대계열) |>
  summarise(sum = sum(졸업자수)) |>
  select(sum) |> pull()

df_sunburst |> plot_ly() |>
  add_trace(type = 'sunburst', 
            labels = c('전체', unique(df_sunburst$대계열), df_sunburst$중계열), 
            parents = c('', rep('전체', 7), df_sunburst$대계열), 
            values = c(all_sum, 계열_sum, df_sunburst$졸업자수), 
            branchvalues = 'total', maxdepth = 3, domain = list(x = c(0, 0.45))) |>
  add_trace(type = 'sunburst', 
            labels = c('전체', unique(df_sunburst$대계열), df_sunburst$중계열), 
            parents = c('', rep('전체', 7), df_sunburst$대계열), 
            values = c(all_sum, 계열_sum, df_sunburst$졸업자수), 
            branchvalues = 'reminder', maxdepth = 3, domain = list(x = c(0.55, 1))) |>
  add_annotations(x = 0.25, y = 1, text = "branchvalues = 'total'", 
                  showarrow = F, xanchor = 'center')  |>
  add_annotations(x = 0.75, y = 1, text = "branchvalues = 'reminder'", 
                  showarrow = F, xanchor = 'center')




################################################################################
df_sunburst <- df_취업률_500 |> group_by(대계열, 중계열) |>
  summarise(졸업자수 = sum(졸업자수))

all_sum <- sum(df_sunburst$졸업자수)
계열_sum <- df_sunburst |> group_by(대계열) |>
  summarise(sum = sum(졸업자수)) |>
  select(sum) |> pull()

df_sunburst |> plot_ly() |>
  add_trace(type = 'sunburst', 
            labels = c('전체', unique(df_sunburst$대계열), df_sunburst$중계열), 
            parents = c('', rep('전체', 7), df_sunburst$대계열), 
            values = c(all_sum, 계열_sum, df_sunburst$졸업자수), 
            branchvalues = 'total', insidetextorientation = 'radial', maxdepth = 3, domain = list(x = c(0, 0.45))) |>
  add_trace(type = 'sunburst', 
            labels = c('전체', unique(df_sunburst$대계열), df_sunburst$중계열), 
            parents = c('', rep('전체', 7), df_sunburst$대계열), 
            values = c(all_sum, 계열_sum, df_sunburst$졸업자수), 
            branchvalues = 'total', insidetextorientation = 'horizontal',maxdepth = 3, domain = list(x = c(0.55, 1))) |>
  add_annotations(x = 0.25, y = 1, text = "insidetextorientation = 'radial'", 
                  showarrow = F, xanchor = 'center')  |>
  add_annotations(x = 0.75, y = 1, text = "insidetextorientation = 'horizontal'", 
                  showarrow = F, xanchor = 'center')




################################################################################
plot_ly() |>
  add_trace(type = 'treemap',  
            ## treemap 트레이스의 labels 설정
            labels = c('전체', unique(df_sunburst$대계열), df_sunburst$중계열), 
            ## treemap 트레이스의 parents 설정
            parents = c('', rep('전체', 7), df_sunburst$대계열), 
            ## treemap 트레이스의 values 설정
            values = c(all_sum, 계열_sum, df_sunburst$졸업자수),
            textinfo = 'label+value+percent parent+percent entry')



################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




################################################################################




