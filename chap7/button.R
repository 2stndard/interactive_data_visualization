library(showtext)
showtext_auto()
library(tidyverse)
library(readxl)
library(patchwork)
library(plotly)

df_충원율_botton <- read_excel('D:/R/git/datavisualization/plotly/RnPy/chap6/고등 주요 01-시도별 신입생 충원율(2010-2022)_220825y.xlsx', 
                     ## 'data' 시트의 데이터를 불러오는데,
                     sheet = 'Sheet1',
                     ## 앞의 10행을 제외하고
                     skip = 7, 
                     ## 첫번째 행은 열 이름을 설정
                     col_names = FALSE, 
                     ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                     col_types = c(rep('text', 2), rep('numeric', 12)))

df_충원율_botton <- df_충원율_botton |> dplyr::select(1, 2, 5)

colnames(df_충원율_botton) <- c('연도', '지역', '신입생충원율')

df_충원율_botton <- df_충원율_botton |> pivot_wider(names_from = '연도', values_from = '신입생충원율')

df_충원율_botton <- as.data.frame(df_충원율_botton)

typeof(df_충원율_botton)

fig <- df_충원율_botton |>
  plot_ly() |>
  add_trace(type = 'bar', x = ~지역, 
            y = ~`2022`
            )

fig <- fig %>% layout(
  title = "연도별 충원율",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "충원율(%)"),
  updatemenus = list(
    list(
      type = "buttons",
      y = 0.8,
      buttons = list(
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2010`)),
             label = "2010년"),
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2012`)),
             label = "2012년"),
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2014`)),
             label = "2014년"),
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2016`)),
             label = "2016년"),
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2018`)),
             label = "2018년"),
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2020`)),
             label = "2020년"),
        list(method = "restyle",
             args = list("y", list(df_충원율_botton$`2022`)),
             label = "2022년")))
  ), 
  margin = margins_R)

fig

