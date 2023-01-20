################################################################################
## 연도별 충원율 데이터를 불러들이고 전처리
df_충원율_botton <- read_excel('D:/R/data/Rnpy/고등 주요 01-시도별 신입생 충원율(2010-2022)_220825y.xlsx', 
                            sheet = 'Sheet1', skip = 7, col_names = FALSE, 
                            col_types = c(rep('text', 2), rep('numeric', 12)))

df_충원율_botton <- df_충원율_botton |> dplyr::select(1, 2, 5)

colnames(df_충원율_botton) <- c('연도', '지역', '신입생충원율')

df_충원율_botton <- df_충원율_botton |> pivot_wider(names_from = '연도', values_from = '신입생충원율')

df_충원율_botton <- as.data.frame(df_충원율_botton)

margins_R <- list(t = 50, b = 25, l = 25, r = 25)


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
  margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap7/fig/vector/7-1'))


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
  margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap7/fig/vector/7-3'))

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
  margin = margins_R) |>
  config(toImageButtonOptions = list(format = 'svg', filename = 'D:/R/git/datavisualization/plotly/RnPy/chap7/fig/vector/7-5'))

fig



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





