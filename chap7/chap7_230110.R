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



################################################################################
## 연도별 충원율 데이터를 불러들이고 전처리
df_충원율_botton <- read_excel('D:/R/git/datavisualization/plotly/RnPy/chap6/고등 주요 01-시도별 신입생 충원율(2010-2022)_220825y.xlsx', 
                            sheet = 'Sheet1', skip = 7, col_names = FALSE, 
                            col_types = c(rep('text', 2), rep('numeric', 12)))

df_충원율_botton <- df_충원율_botton |> dplyr::select(1, 2, 5)

colnames(df_충원율_botton) <- c('연도', '지역', '신입생충원율')

df_충원율_botton <- df_충원율_botton |> pivot_wider(names_from = '연도', values_from = '신입생충원율')

df_충원율_botton <- as.data.frame(df_충원율_botton)



################################################################################
fig <- df_충원율_botton |>
  plot_ly() |>
  ## 데이터가 표시되는 bar 트레이스 생성
  add_trace(type = 'bar', x = ~지역, 
            y = ~`2022`, text = ~`2022`, 
            texttemplate = '%{text:.1f}%', textposition = 'outside')

##  버튼 제목이 표시되는 주석 레이어 생성
fig <- fig |> add_annotations(x = -0.1, y = 0.85, text = '<b>연도</b>', 
                              xanchor = 'center', yanchor = 'middle', 
                              yref='paper', xref='paper', showarrow=FALSE )

##  버튼 생성
fig <- fig %>% layout(
  title = "2022년 지역별 충원율",
  xaxis = list(domain = c(0.1, 1), categoryorder = "total descending"),
  yaxis = list(title = "충원율(%)"),
  updatemenus = list(
    list(
      type = "buttons", y = 0.8,
      buttons = list(
        list(method = "restyle",
             args = list(list(y = list(df_충원율_botton$`2018`), 
                              text = list(df_충원율_botton$`2018`))),
             label = "2018년"),
        list(method = "restyle",
             args = list(list(y = list(df_충원율_botton$`2019`), 
                              text = list(df_충원율_botton$`2019`))),
             label = "2019년"),
        list(method = "restyle",
             args = list(list(y = list(df_충원율_botton$`2020`), 
                              text = list(df_충원율_botton$`2020`))),
             label = "2020년"),
        list(method = "restyle",
             args = list(list(y = list(df_충원율_botton$`2021`), 
                              text = list(df_충원율_botton$`2021`))),
             label = "2021년"),
        list(method = "restyle",
             args = list(list(y = list(df_충원율_botton$`2022`), 
                              text = list(df_충원율_botton$`2022`))),
             label = "2022년")))
  ),
  margin = margins_R)

fig


################################################################################
fig <- df_충원율_botton |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~지역, y = ~`2022`, text = ~`2022`, 
            texttemplate = '%{text:.1f}%', textposition = 'outside')

fig <- fig |> add_annotations(x = -0.1, y = 0.85, text = '<b>연도</b>', 
                              xanchor = 'center', yanchor = 'middle', 
                              yref='paper', xref='paper', showarrow=FALSE )

fig <- fig %>% layout(
  title = "2022년 지역별 충원율",
  xaxis = list(domain = c(0.1, 1), categoryorder = "total descending"),
  yaxis = list(title = "충원율(%)"),
  updatemenus = list(
    list(
      type = "buttons",
      y = 0.8,
      buttons = list(
        list(method = "relayout",
             args = list(list(title.text='2018년 지역별 충원율')),
             label = "2018년"),
        list(method = "relayout",
             args = list(list(title.text='2019년 지역별 충원율')),
             label = "2019년"),
        list(method = "relayout",
             args = list(list(title.text='2020년 지역별 충원율')),
             label = "2020년"),
        list(method = "relayout",
             args = list(list(title.text='2021년 지역별 충원율')),
             label = "2021년"),
        list(method = "relayout",
             args = list(list(title.text='2022년 지역별 충원율')),
             label = "2022년")))
  ),
  margin = margins_R)

fig


################################################################################
fig <- df_충원율_botton |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~지역, y = ~`2022`, text = ~`2022`, 
            texttemplate = '%{text:.1f}%', textposition = 'outside')

fig <- fig |> add_annotations(x = -0.1, y = 0.85, text = '<b>연도</b>', 
                              xanchor = 'center', yanchor = 'middle', 
                              yref='paper', xref='paper', showarrow=FALSE )

fig <- fig %>% layout(
  title = '2022년 지역별 충원율',
  xaxis = list(domain = c(0.1, 1), categoryorder = "total descending"),
  yaxis = list(title = "충원율(%)"),
  updatemenus = list(
    list(
      type = "buttons",
      y = 0.8,
      buttons = list(
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2018`), 
                              text = list(df_충원율_botton$`2018`)),
                         list(title.text='2018년 지역별 충원율')),
             label = "2018년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2019`), 
                              text = list(df_충원율_botton$`2019`)),
                         list(title.text='2019년지역별 충원율')),
             label = "2019년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2020`), 
                              text = list(df_충원율_botton$`2020`)),
                         list(title.text='2020년 지역별 충원율')),
             label = "2020년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2021`), 
                              text = list(df_충원율_botton$`2021`)),
                         list(title.text='2021년 지역별 충원율')),
             label = "2021년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2022`), 
                              text = list(df_충원율_botton$`2022`)),
                         list(title.text='2022년 지역별 충원율')),
             label = "2022년")))
  ),
  margin = margins_R)

fig



################################################################################
fig <- df_충원율_botton |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~지역, y = ~`2022`, text = ~`2022`, 
            texttemplate = '%{text:.1f}%', textposition = 'outside')

fig <- fig |> add_annotations(x = -0.1, y = 0.85, text = '<b>연도</b>', 
                              xanchor = 'center', yanchor = 'middle', 
                              yref='paper', xref='paper', showarrow=FALSE )

fig <- fig %>% layout(
  title = '2022년 지역별 충원율',
  xaxis = list(domain = c(0.1, 1), categoryorder = "total descending"),
  yaxis = list(title = "충원율(%)"),
  updatemenus = list(
    list(
      type = 'dropdown',
      y = 0.8,
      buttons = list(
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2018`), 
                              text = list(df_충원율_botton$`2018`)),
                         list(title.text='2018년 지역별 충원율')),
             label = "2018년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2019`), 
                              text = list(df_충원율_botton$`2018`)),
                         list(title.text='2019년지역별 충원율')),
             label = "2019년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2020`), 
                              text = list(df_충원율_botton$`2018`)),
                         list(title.text='2020년 지역별 충원율')),
             label = "2020년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2021`), 
                              text = list(df_충원율_botton$`2018`)),
                         list(title.text='2021년 지역별 충원율')),
             label = "2021년"),
        list(method = "update",
             args = list(list(y = list(df_충원율_botton$`2022`), 
                              text = list(df_충원율_botton$`2018`)),
                         list(title.text='2022년 지역별 충원율')),
             label = "2022년")))
  ),
  margin = margins_R)

fig


################################################################################
fig <- df_충원율_botton |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~지역, 
            y = ~`2022`, text = ~paste0(sprintf('%.1f', df_충원율_botton$`2022`), '%'), 
            textposition = 'outside')

## 슬라이더 설정을 위한 steps 속성 설정
steps <- list(
  list(method = "update",
       args = list(list(y = list(df_충원율_botton$`2018`), 
                        text = list(paste0(sprintf('%.1f', df_충원율_botton$`2018`), '%'))),
                   list(title.text='2018년 지역별 충원율')),
       label = "2018년", value = "1"),
  list(method = "update",
       args = list(list(y = list(df_충원율_botton$`2019`), 
                        text = list(paste0(sprintf('%.1f', df_충원율_botton$`2019`), '%'))),
                   list(title.text='2019년지역별 충원율')),
       label = "2019년", value = "2"),
  list(method = "update",
       args = list(list(y = list(df_충원율_botton$`2020`), 
                        text = list(paste0(sprintf('%.1f', df_충원율_botton$`2020`), '%'))),
                   list(title.text='2020년 지역별 충원율')),
       label = "2020년", value = "3"),
  list(method = "update",
       args = list(list(y = list(df_충원율_botton$`2021`), 
                        text = list(paste0(sprintf('%.1f', df_충원율_botton$`2021`), '%'))),
                   list(title.text='2021년 지역별 충원율')),
       label = "2021년", value = "4"),
  list(method = "update",
       args = list(list(y = list(df_충원율_botton$`2022`), 
                        text = list(paste0(sprintf('%.1f', df_충원율_botton$`2022`), '%'))),
                   list(title.text='2022년 지역별 충원율')),
       label = "2022년", value = "5")
)

fig <- fig %>% layout(
  title = '2022년 지역별 충원율',
  xaxis = list(categoryorder = "total descending"),
  yaxis = list(title = "충원율(%)"),
  sliders = list(
    list(
      active = 6, currentvalue = list(prefix = "연도: "), 
      pad = list(t = 60), 
      steps = steps)), 
  margin = margins_R)

fig





