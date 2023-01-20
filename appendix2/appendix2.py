import pandas as pd
from datetime import datetime, timedelta
from pandas.api.types import CategoricalDtype
from matplotlib import pyplot as plt
import plotly.graph_objects as go

df_covid19 = pd.read_csv("D:/R/data/Rnpy/owid-covid-data.csv")

df_covid19['date'] = pd.to_datetime(df_covid19['date'], format="%Y-%m-%d")

df_covid19_100 = df_covid19[(df_covid19['iso_code'].isin(['KOR', 'OWID_ASI', 'OWID_EUR', 'OWID_OCE', 'OWID_NAM', 'OWID_SAM', 'OWID_AFR'])) & (df_covid19['date'] >= (max(df_covid19['date']) - timedelta(days = 100)))]


df_covid19_100.loc[df_covid19_100['location'] == 'South Korea', "location"] = '한국'
df_covid19_100.loc[df_covid19_100['location'] == 'Asia', "location"] = '아시아'
df_covid19_100.loc[df_covid19_100['location'] == 'Europe', "location"] = '유럽'
df_covid19_100.loc[df_covid19_100['location'] == 'Oceania', "location"] = '오세아니아'
df_covid19_100.loc[df_covid19_100['location'] == 'North America', "location"] = '북미'
df_covid19_100.loc[df_covid19_100['location'] == 'South America', "location"] = '남미'
df_covid19_100.loc[df_covid19_100['location'] == 'Africa', "location"] = '아프리카'

ord = CategoricalDtype(categories = ['한국', '아시아', '유럽', '북미', '남미', '아프리카', '오세아니아'], ordered = True)

df_covid19_100['location'] = df_covid19_100['location'].astype(ord)

df_covid19_100 = df_covid19_100.sort_values(by = 'date')

df_covid19_100_wide = df_covid19_100.loc[:,['date', 'location', 'new_cases', 'people_fully_vaccinated_per_hundred']].rename(columns={'new_cases':'확진자', 'people_fully_vaccinated_per_hundred':'백신접종완료자'})

df_covid19_100_wide = df_covid19_100_wide.pivot(index='date', columns='location', values=['확진자', '백신접종완료자']).sort_values(by = 'date')

df_covid19_100_wide.columns = ['확진자_한국', '확진자_아시아', '확진자_유럽', '확진자_북미', '확진자_남미', '확진자_아프리카','확진자_오세아니아',
                              '백신접종완료자_한국', '백신접종완료자_아시아', '백신접종완료자_유럽', '백신접종완료자_북미', '백신접종완료자_남미', '백신접종완료자_아프리카','백신접종완료자_오세아니아']
                              
df_covid19_stat = df_covid19.groupby(['iso_code', 'continent', 'location'], dropna=False).agg(
    인구수 = ('population', 'max'),
    전체확진자수 = ('new_cases', 'sum'),
    전체사망자수 = ('new_deaths', 'sum'), 
    백신접종자완료자수 = ('people_fully_vaccinated', 'max'),
    인구백명당백신접종완료율 = ('people_fully_vaccinated_per_hundred', 'max'),
    인구백명당부스터접종자수 = ('total_boosters_per_hundred', 'max')
).reset_index()

df_covid19_stat['십만명당사망자수'] = round(df_covid19_stat['전체사망자수'] / df_covid19_stat['인구수'] *100000, 5)

df_covid19_stat['백신접종완료율'] = df_covid19_stat['백신접종자완료자수'] / df_covid19_stat['인구수']

######################################   
## python 코드
## 대학 학과 취업률 데이터 셋

df_취업률 = pd.read_excel("d:/R/data/2020년 학과별 고등교육기관 취업통계.xlsx", 
                           sheet_name = '학과별',
                           skiprows=(13), 
                           header = 0)

df_취업률 = pd.concat([df_취업률.iloc[:, 0:8], 
                    df_취업률.loc[:, df_취업률.columns.str.endswith('계')], 
                    df_취업률.loc[:, '입대자']], 
                   axis = 1
                   )

df_취업률_500 = df_취업률.loc[(df_취업률['졸업자_계'] < 500)]

df_취업률_500 = df_취업률_500.iloc[range(0, len(df_취업률_500.index) , 4)]

df_취업률_500 = df_취업률_500.rename(columns = {'졸업자_계':'졸업자수', '취업률_계':'취업률', '취업자_합계_계':'취업자수'})

total_deaths_5_nations_by_day = df_covid19.copy()
total_deaths_5_nations_by_day = total_deaths_5_nations_by_day[(total_deaths_5_nations_by_day['iso_code'].isin(['KOR', 'USA', 'JPN', 'GBR', 'FRA']))].dropna(subset = ['total_deaths_per_million'])

nations = {'France':'0', 'Japan':'1', 'South Korea':'2', 'United Kingdom':'3', 'United States':'4'}

fig = go.Figure()
for location, group in total_deaths_5_nations_by_day.groupby('location'):
    fig.add_trace(go.Scatter(
        mode = 'lines', 
        x = group['date'], 
        y = group['total_deaths_per_million'], 
        line = dict(dash = nations[location]), 
        name = location,
        connectgaps = True, showlegend = False
    ))
    fig.add_trace(go.Scatter(
        mode = 'text',
        x = group.loc[group['date'] == group['date'].max(), 'date'], 
        y = group.loc[group['date'] == group['date'].max(), 'total_deaths_per_million'],
        text = group.loc[group['date'] == group['date'].max(), 'location'], 
        showlegend = False, 
        textposition = 'middle right'
    ))    

fig.update_layout(title = dict(text = '코로나 19 사망자수 추세', x = 0.5), 
                  xaxis = dict(title = '', 
                               range = [total_deaths_5_nations_by_day['date'].min(), total_deaths_5_nations_by_day['date'].max() + timedelta(days=150)]), 
                  yaxis = dict(title = '10만명당 사망자수 누계'))   

deaths_per_million_in_lateast = total_deaths_5_nations_by_day[total_deaths_5_nations_by_day['new_deaths_per_million'].isna() == False]
deaths_per_million_in_lateast = pd.merge(deaths_per_million_in_lateast.groupby('location')['date'].max(), deaths_per_million_in_lateast, on = ("location", 'date'))[['iso_code', 'location', 'date', 'new_deaths_per_million']]
df_gauge = pd.merge(deaths_per_million_in_lateast, total_deaths_5_nations_by_day.groupby('location')['new_deaths_per_million'].max().reset_index(), on = 'location').sort_values('location')
df_gauge.columns = ('iso_code', 'location', 'date', '최근사망자', '최대사망자')

## 한국 게이지 인디케이터 생성
fig_gauge_kor = go.Figure()
fig_gauge_kor.add_trace(go.Indicator(
    type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[2, 1],
            value = df_gauge.iloc[2, 3], 
            gauge = dict(axis = dict(
              range = (0, df_gauge.iloc[2, 4]*1.2)),
              steps = [
                dict(range = (0, (df_gauge.iloc[2, 4])*1.2*0.5), color = "lightgray"),
                dict(range = ((df_gauge.iloc[2, 4])*1.2*0.5, (df_gauge.iloc[2, 4])*1.2*0.75), color = "darkgray"), 
                dict(range = ((df_gauge.iloc[2, 4])*1.2*0.75, (df_gauge.iloc[2, 4])*1.2), color = "gray")],
              threshold = dict(
                line = dict(color = 'white'),
                value = df_gauge.iloc[2, 4]), 
              bar = dict(color = "darkblue")), 
            number = dict(suffix = '명')
))

## 프랑스 게이지 인디케이터 생성
fig_gauge_fra = go.Figure()
fig_gauge_fra.add_trace(go.Indicator(
    type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[0, 1],
            value = df_gauge.iloc[0, 3], 
            gauge = dict(axis = dict(
              range = (0, df_gauge.iloc[0, 4]*1.2)),
              steps = [
                dict(range = (0, (df_gauge.iloc[0, 4])*1.2*0.5), color = "lightgray"),
                dict(range = ((df_gauge.iloc[0, 4])*1.2*0.5, (df_gauge.iloc[0, 4])*1.2*0.75), color = "darkgray"),
                dict(range = ((df_gauge.iloc[0, 4])*1.2*0.75, (df_gauge.iloc[0, 4])*1.2), color = "gray")],
              threshold = dict(
                line = dict(color = 'white'),
                value = df_gauge.iloc[0, 4]), 
              bar = dict(color = "darkblue")), 
            number = dict(suffix = '명')))

## 일본 게이지 인디케이터 생성
fig_gauge_jpn = go.Figure()
fig_gauge_jpn.add_trace(go.Indicator(
    type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[1, 1],
            value = df_gauge.iloc[1, 3], 
            gauge = dict(axis = dict(
              range = (0, df_gauge.iloc[1, 4]*1.2)),
              steps = [
                dict(range = (0, (df_gauge.iloc[1, 4])*1.2*0.5), color = "lightgray"),
                dict(range = ((df_gauge.iloc[1, 4])*1.2*0.5, (df_gauge.iloc[1, 4])*1.2*0.75), color = "darkgray"),
                dict(range = ((df_gauge.iloc[1, 4])*1.2*0.75, (df_gauge.iloc[1, 4])*1.2), color = "gray")],
              threshold = dict(
                line = dict(color = 'white'),
                value = df_gauge.iloc[1, 4]), 
              bar = dict(color = "darkblue")), 
            number = dict(suffix = '명')))

## 영국 게이지 인디케이터 생성
fig_gauge_gbr = go.Figure()
fig_gauge_gbr.add_trace(go.Indicator(
    type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[3, 1],
            value = df_gauge.iloc[3, 3], 
            gauge = dict(axis = dict(
              range = (0, df_gauge.iloc[3, 4]*1.2)),
              steps = [
                dict(range = (0, (df_gauge.iloc[3, 4])*1.2*0.5), color = "lightgray"),
                dict(range = ((df_gauge.iloc[3, 4])*1.2*0.5, (df_gauge.iloc[3, 4])*1.2*0.75), color = "darkgray"),
                dict(range = ((df_gauge.iloc[3, 4])*1.2*0.75, (df_gauge.iloc[3, 4])*1.2), color = "gray")],
              threshold = dict(
                line = dict(color = 'white'),
                value = df_gauge.iloc[3, 4]), 
              bar = dict(color = "darkblue")), 
            number = dict(suffix = '명')))

## 미국 게이지 인디케이터 생성
fig_gauge_usa = go.Figure()
fig_gauge_usa.add_trace(go.Indicator(
    type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[4, 1],
            value = df_gauge.iloc[4, 3], 
            gauge = dict(axis = dict(
              range = (0, df_gauge.iloc[4, 4]*1.2)),
              steps = [
                dict(range = (0, (df_gauge.iloc[4, 4])*1.2*0.5), color = "lightgray"),
                dict(range = ((df_gauge.iloc[4, 4])*1.2*0.5, (df_gauge.iloc[4, 4])*1.2*0.75), color = "darkgray"),
                dict(range = ((df_gauge.iloc[4, 4])*1.2*0.75, (df_gauge.iloc[4, 4])*1.2), color = "gray")],
              threshold = dict(
                line = dict(color = 'white'),
                value = df_gauge.iloc[4, 4]), 
              bar = dict(color = "darkblue")), 
            number = dict(suffix = '명')))

from dash import Dash, dcc, html, Input, Output
import plotly.express as px

## Dash 앱의 초기화
app = Dash(__name__)

##  Dash 앱 레이아웃 설정
app.layout = html.Div(children = [
    ##  첫 번째 div 설정
    html.Div(id = 'header', children = [
        html.H1('코로나 19 사망자수 추세', style = dict(textAlign = 'center', color = 'darkblue'))],
            ), 
    ##  두 번째 div 설정
    html.Div(id = 'second block', children = [
        ##  두 번째 좌측 div 설정
        html.Div(children = [
            html.Label('날짜 선택', style = dict(position = 'absolute', top = '10%', left = '5%')),
            dcc.DatePickerSingle(id = 'picker', date = total_deaths_5_nations_by_day['date'].max() , 
                                 placeholder='Select a date', clearable=True,
                                 style = dict(position = 'absolute', top = '15%', left = '3%', width = '10px'))],
            style = dict(width = '20%', display = 'inline-block')),
        ##  두 번째 우측 div 설정
        html.Div(
            dcc.Graph(id = 'line_5_nations', figure=fig),
            style = dict(width = '80%', display = 'inline-block'))
    ]), 
    ##  세 번째 div 설정
    html.Div(id = 'third block', children = [
        dcc.Graph(id = 'indicator_kor',figure = fig_gauge_kor, style = dict(width = '20%', display = 'inline-block')),
        dcc.Graph(id = 'indicator_fra',figure = fig_gauge_fra, style = dict(width = '20%', display = 'inline-block')),
        dcc.Graph(id = 'indicator_jpn',figure = fig_gauge_jpn, style = dict(width = '20%', display = 'inline-block')),
        dcc.Graph(id = 'indicator_gbr',figure = fig_gauge_gbr, style = dict(width = '20%', display = 'inline-block')),
        dcc.Graph(id = 'indicator_usa',figure = fig_gauge_usa, style = dict(width = '20%', display = 'inline-block'))
        ]), 
     html.Div(id='output-container-date-picker-single')
    ])

@app.callback(
    Output(component_id = 'indicator_kor', component_property = 'figure'), 
    Output(component_id = 'indicator_fra', component_property = 'figure'), 
    Output(component_id = 'indicator_jpn', component_property = 'figure'), 
    Output(component_id = 'indicator_gbr', component_property = 'figure'), 
    Output(component_id = 'indicator_usa', component_property = 'figure'), 
    Output(component_id = 'line_5_nations', component_property = 'figure'), 
    Input(component_id = 'picker', component_property = 'date'))    
def update_output(date_value):
    total_deaths_5_nations_update = total_deaths_5_nations_by_day.loc[total_deaths_5_nations_by_day['date'] == date_value, ['date', 'iso_code', 'new_deaths_per_million']].sort_values(by = 'iso_code')
    ## 한국 게이지 인디케이터 생성
    fig_gauge_kor = go.Figure()
    fig_gauge_kor.add_trace(go.Indicator(
        type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[2, 1],
                value = total_deaths_5_nations_update.iloc[2, 2], 
                gauge = dict(axis = dict(
                  range = (0, df_gauge.iloc[2, 4]*1.2)),
                  steps = [
                    dict(range = (0, (df_gauge.iloc[2, 4])*1.2*0.5), color = "lightgray"),
                    dict(range = ((df_gauge.iloc[2, 4])*1.2*0.5, (df_gauge.iloc[2, 4])*1.2*0.75), color = "darkgray"), 
                    dict(range = ((df_gauge.iloc[2, 4])*1.2*0.75, (df_gauge.iloc[2, 4])*1.2), color = "gray")],
                  threshold = dict(
                    line = dict(color = 'white'),
                    value = df_gauge.iloc[2, 4]), 
                  bar = dict(color = "darkblue")), 
                number = dict(suffix = '명')
    ))
        ## 프랑스 게이지 인디케이터 생성
    fig_gauge_fra = go.Figure()
    fig_gauge_fra.add_trace(go.Indicator(
        type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[0, 1],
                value = total_deaths_5_nations_update.iloc[0, 2], 
                gauge = dict(axis = dict(
                  range = (0, df_gauge.iloc[0, 4]*1.2)),
                  steps = [
                    dict(range = (0, (df_gauge.iloc[0, 4])*1.2*0.5), color = "lightgray"),
                    dict(range = ((df_gauge.iloc[0, 4])*1.2*0.5, (df_gauge.iloc[0, 4])*1.2*0.75), color = "darkgray"),
                    dict(range = ((df_gauge.iloc[0, 4])*1.2*0.75, (df_gauge.iloc[0, 4])*1.2), color = "gray")],
                  threshold = dict(
                    line = dict(color = 'white'),
                    value = df_gauge.iloc[0, 4]), 
                  bar = dict(color = "darkblue")), 
                number = dict(suffix = '명')))
        ## 일본 게이지 인디케이터 생성
    fig_gauge_jpn = go.Figure()
    fig_gauge_jpn.add_trace(go.Indicator(
        type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[1, 1],
                value = total_deaths_5_nations_update.iloc[1, 2], 
                gauge = dict(axis = dict(
                  range = (0, df_gauge.iloc[1, 4]*1.2)),
                  steps = [
                    dict(range = (0, (df_gauge.iloc[1, 4])*1.2*0.5), color = "lightgray"),
                    dict(range = ((df_gauge.iloc[1, 4])*1.2*0.5, (df_gauge.iloc[1, 4])*1.2*0.75), color = "darkgray"),
                    dict(range = ((df_gauge.iloc[1, 4])*1.2*0.75, (df_gauge.iloc[1, 4])*1.2), color = "gray")],
                  threshold = dict(
                    line = dict(color = 'white'),
                    value = df_gauge.iloc[1, 4]), 
                  bar = dict(color = "darkblue")), 
                number = dict(suffix = '명')))    
        ## 영국 게이지 인디케이터 생성
    fig_gauge_gbr = go.Figure()
    fig_gauge_gbr.add_trace(go.Indicator(
        type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[3, 1],
                value = total_deaths_5_nations_update.iloc[3, 2], 
                gauge = dict(axis = dict(
                  range = (0, df_gauge.iloc[3, 4]*1.2)),
                  steps = [
                    dict(range = (0, (df_gauge.iloc[3, 4])*1.2*0.5), color = "lightgray"),
                    dict(range = ((df_gauge.iloc[3, 4])*1.2*0.5, (df_gauge.iloc[3, 4])*1.2*0.75), color = "darkgray"),
                    dict(range = ((df_gauge.iloc[3, 4])*1.2*0.75, (df_gauge.iloc[3, 4])*1.2), color = "gray")],
                  threshold = dict(
                    line = dict(color = 'white'),
                    value = df_gauge.iloc[3, 4]), 
                  bar = dict(color = "darkblue")), 
                number = dict(suffix = '명')))
#         ## 미국 게이지 인디케이터 생성
    fig_gauge_usa = go.Figure()
    fig_gauge_usa.add_trace(go.Indicator(
        type = 'indicator', mode = "gauge+number", title = df_gauge.iloc[4, 1],
                value = total_deaths_5_nations_update.iloc[4, 2], 
                gauge = dict(axis = dict(
                  range = (0, df_gauge.iloc[4, 4]*1.2)),
                  steps = [
                    dict(range = (0, (df_gauge.iloc[4, 4])*1.2*0.5), color = "lightgray"),
                    dict(range = ((df_gauge.iloc[4, 4])*1.2*0.5, (df_gauge.iloc[4, 4])*1.2*0.75), color = "darkgray"),
                    dict(range = ((df_gauge.iloc[4, 4])*1.2*0.75, (df_gauge.iloc[4, 4])*1.2), color = "gray")],
                  threshold = dict(
                    line = dict(color = 'white'),
                    value = df_gauge.iloc[4, 4]), 
                  bar = dict(color = "darkblue")), 
                number = dict(suffix = '명')))
    fig_temp = go.Figure(fig)
    fig_temp = fig_temp.add_shape(type = 'line', yref = "y",
                             y0 = 0, y1 = total_deaths_5_nations_by_day['total_deaths_per_million'].max(),
                             x0 = date_value, x1 = date_value,
                             line = dict(color = 'black', dash="dot"))          
    return fig_gauge_kor, fig_gauge_fra, fig_gauge_jpn, fig_gauge_gbr, fig_gauge_usa, fig_temp

if __name__=='__main__':
    app.run_server(debug=True, use_reloader = False)