setwd("D:/Het Project/Premier league")
install.packages("xgboost")
library("xgboost")
raw.data.1 = read.csv('2000.csv')
raw.data.2 = read.csv('2001.csv')
raw.data.3 = read.csv('2002.csv')
raw.data.4 = read.csv('2003.csv')
raw.data.5 = read.csv('2004.csv')
raw.data.6 = read.csv('2005.csv')
raw.data.7 = read.csv('2006.csv')
raw.data.8 = read.csv('2007.csv')
raw.data.9 = read.csv('2008.csv')
raw.data.10 = read.csv('2009.csv')
raw.data.11 = read.csv('2010.csv')
raw.data.12 = read.csv('2011.csv')
raw.data.13 = read.csv('2012.csv')
raw.data.14 = read.csv('2013.csv')
raw.data.15 = read.csv('2014.csv')
raw.data.16 = read.csv('2015.csv')
raw.data.17 = read.csv('2016.csv')
raw.data.18 = read.csv('2017.csv')


parse_date = function(date){
  if (date == ""){
    return("")
  }else{
    return (strptime(date, '%d/%m/%y'))
} }


parse_date_other = function(date){
  if (date == ""){
    return("")
  }else{
  return(strptime(date, '%d/%m/%Y'))
} }

raw.data.1.Date = lapply(raw.data.1$Date, FUN=parse_date)
raw_data_2.Date = raw_data_2.Date.apply(parse_date)
raw_data_3.Date = raw_data_3.Date.apply(parse_date_other)         # The date format for this dataset is different
raw_data_4.Date = raw_data_4.Date.apply(parse_date)
raw_data_5.Date = raw_data_5.Date.apply(parse_date)
raw_data_6.Date = raw_data_6.Date.apply(parse_date)
raw_data_7.Date = raw_data_7.Date.apply(parse_date)
raw_data_8.Date = raw_data_8.Date.apply(parse_date)
raw_data_9.Date = raw_data_9.Date.apply(parse_date)
raw_data_10.Date = raw_data_10.Date.apply(parse_date)
raw_data_11.Date = raw_data_11.Date.apply(parse_date)
raw_data_12.Date = raw_data_12.Date.apply(parse_date)
raw_data_13.Date = raw_data_13.Date.apply(parse_date)
raw_data_14.Date = raw_data_14.Date.apply(parse_date)
raw_data_15.Date = raw_data_15.Date.apply(parse_date)
raw_data_16.Date = raw_data_16.Date.apply(parse_date)
raw_data_17.Date = raw_data_17.Date.apply(parse_date)
raw_data_18.Date = raw_data_18.Date.apply(parse_date)


# columns connected to gameplay
columns_req = c('Date','HomeTeam','AwayTeam','FTHG','FTAG','FTR')

playing_statistics_1 = raw.data.1[columns_req]                      
playing_statistics_2 = raw.data.2[columns_req]
playing_statistics_3 = raw.data.3[columns_req]
playing_statistics_4 = raw.data.4[columns_req]
playing_statistics_5 = raw.data.5[columns_req]
playing_statistics_6 = raw.data.6[columns_req]
playing_statistics_7 = raw.data.7[columns_req]
playing_statistics_8 = raw.data.8[columns_req]
playing_statistics_9 = raw.data.9[columns_req]
playing_statistics_10 = raw.data.10[columns_req]
playing_statistics_11 = raw.data.11[columns_req]   
playing_statistics_12 = raw.data.12[columns_req]
playing_statistics_13 = raw.data.13[columns_req]
playing_statistics_14 = raw.data.14[columns_req]
playing_statistics_15 = raw.data.15[columns_req]
playing_statistics_16 = raw.data.16[columns_req]
playing_statistics_17 = raw.data.17[columns_req]
playing_statistics_18 = raw.data.18[columns_req]

# Gets the goals scored agg arranged by teams and matchweek
get_goals_scored=function(playing_stat){
  # Create a dictionary with team names as keys
  teams = {}
for (i in playing_stat.groupby('HomeTeam').mean().T.columns){
  teams[i] = []
}
# the value corresponding to keys is a list containing the match location.
for i in range(len(playing_stat)):
  HTGS = playing_stat.iloc[i]['FTHG']
ATGS = playing_stat.iloc[i]['FTAG']
teams[playing_stat.iloc[i].HomeTeam].append(HTGS)
teams[playing_stat.iloc[i].AwayTeam].append(ATGS)

# Create a dataframe for goals scored where rows are teams and cols are matchweek.
GoalsScored = pd.DataFrame(data=teams, index = [i for (i in range(1,39)])).T
GoalsScored[0] = 0
# Aggregate to get uptil that point
for (i in range(2,39)){
  GoalsScored[i] = GoalsScored[i] + GoalsScored[i-1]
return(GoalsScored)
}}