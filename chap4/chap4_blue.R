################################################################################
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
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-1'))



################################################################################
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
            color = ~continent, colors = RColorBrewer::brewer.pal(6, 'Blues')[6:2], 
            text = ~인구백명당백신접종완료율, textposition = 'outside', texttemplate = '%{text}%', 
            textfont = list(color = 'black'), 
            orientation = 'v'
  ) |>
  layout(barmode = 'group', title = '대륙별 완전 백신 접종률 상위 top 5 국가', 
         xaxis = list(title = '백신접종완료율', 
                      ticksuffix = '%', range = c(0, 105)), 
         yaxis = list(title = '', autorange = 'reversed', 
                      tickvals = ~seq,
                      ticktext = ~location), 
         margin = list(r = 100, t = 80), 
         size = list(height = 900)
  ) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-3'))


################################################################################
vaccine_top10 <- df_covid19_stat |>
  filter(인구수 > 10000000) |>
  top_n(10, 인구백명당백신접종완료율)

vaccine_top10 |> 
  plot_ly() |>
  add_trace(type = 'bar',
            x = ~location, y = ~인구백명당백신접종완료율,
            color = ~continent, colors = RColorBrewer::brewer.pal(6, 'Blues')[6:2], 
            text = ~인구백명당백신접종완료율, textposition = 'outside', texttemplate = '%{text}%',
            textfont = list(color = 'black')) |>
  add_trace(type = 'scatter', mode = 'markers+text', 
            name = '인구10만명당 사망자수', yaxis = "y2", color = I('black'), 
            x = ~location, 
            y = ~십만명당사망자수, text = ~round(십만명당사망자수, 1), 
            textposition = 'top'
  )|>
  layout(title = '완전 백신 접종률 상위 top 10 국가', 
         xaxis = list(title = '국가명', categoryorder = 'total descending'), 
         yaxis = list(title = '백신접종완료율', 
                      ticksuffix = '%'), 
         yaxis2 = list(title = '인구10만명당 사망자수', 
                       side = "right", overlaying = "y",
                       range = c(0, 300), ticksuffix = '명'),
         margin = list(r = 100, t = 50), 
         legend = list(x = 1.1)) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-5'))



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
         margin = list(t = 50, b = 25, l = 25, r = 75), 
         legend = list(orientation = 'h', y = -0.5, x = 0.5, 
                       yref = 'container', xanchor = 'center'),
         showlegend = T) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-7'))


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
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-9'))


################################################################################
df_취업률_500 |> group_by(대계열) |>
  summarise(졸업자수 = sum(졸업자수)) |> 
  plot_ly() |>
  ## value와 labels를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            marker = list(colors =  RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 3, by = -1)])) |> 
  layout(title = list(text = '대학 계열별 졸업생 분포'), 
         margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-11'))



################################################################################
p_pie <- df_취업률_500 |> group_by(대계열) |>
  summarise(졸업자수 = sum(졸업자수)) |> 
  plot_ly()

p_pie |>
  ## value와 labels를 매핑한 파이 trace 추가
  add_trace(type = 'pie', values = ~졸업자수, labels = ~대계열, direction = 'clockwise', 
            textinfo = 'value', hole = 0.3) |> 
  add_annotations(x = 0.5, y = 0.5, text = '<b>졸업생수</b>', 
                  showarrow = FALSE, xanchor = 'center', 
                  font = list(size = 20)) |> 
  layout(title = list(text = '대학 계열별 졸업생 분포'), 
         margin = margins_R, 
         colorway = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 3, by = -1)]) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-13'))


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
            branchvalues = 'total', maxdepth = 3) |>
  layout(sunburstcolorway = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 3, by = -1)]) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-15'))


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
                  showarrow = F, xanchor = 'center') |>
  layout(sunburstcolorway = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 3, by = -1)]) |>
  layout(sunburstcolorway = RColorBrewer::brewer.pal(9, 'Blues')[seq(from = 9, to = 3, by = -1)]) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap4/fig/vector/4-17'))



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



