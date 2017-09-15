setwd("D:/Het Project/Premier league/Voetbal-voorspellen")
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

delete.spaces=function(rawdata){
  rawdata$HomeTeam=str_replace_all(rawdata$HomeTeam, fixed(" "), "")
  rawdata$AwayTeam=str_replace_all(rawdata$AwayTeam, fixed(" "), "")
  return(rawdata)
}

raw.data.1=delete.spaces(raw.data.1)
raw.data.2=delete.spaces(raw.data.2)
raw.data.3=delete.spaces(raw.data.3)
raw.data.4=delete.spaces(raw.data.4)
raw.data.5=delete.spaces(raw.data.5)
raw.data.6=delete.spaces(raw.data.6)
raw.data.7=delete.spaces(raw.data.7)
raw.data.8=delete.spaces(raw.data.8)
raw.data.9=delete.spaces(raw.data.9)
raw.data.10=delete.spaces(raw.data.10)
raw.data.11=delete.spaces(raw.data.11)
raw.data.12=delete.spaces(raw.data.12)
raw.data.13=delete.spaces(raw.data.13)
raw.data.14=delete.spaces(raw.data.14)
raw.data.15=delete.spaces(raw.data.15)
raw.data.16=delete.spaces(raw.data.16)
raw.data.17=delete.spaces(raw.data.17)
raw.data.18=delete.spaces(raw.data.18)

parse_date = function(date){
  if (date == ""){
    return("")
  }else{
    return(as.Date(date, '%d/%m/%y'))
} }


parse_date_other = function(date){
  if (date == ""){
    return("")
  }else{
  return(as.Date(date, '%d/%m/%Y'))
} }

raw.data.1$Date = lapply(raw.data.1$Date, FUN=parse_date)
raw.data.2$Date = lapply(raw.data.2$Date, FUN=parse_date)
raw.data.3$Date = lapply(raw.data.3$Date, FUN=parse_date_other)         # The date format for this dataset is different
raw.data.4$Date = lapply(raw.data.4$Date, FUN=parse_date)
raw.data.5$Date = lapply(raw.data.5$Date, FUN=parse_date)
raw.data.6$Date = lapply(raw.data.6$Date, FUN=parse_date)
raw.data.7$Date = lapply(raw.data.7$Date, FUN=parse_date)
raw.data.8$Date = lapply(raw.data.8$Date, FUN=parse_date)
raw.data.9$Date = lapply(raw.data.9$Date, FUN=parse_date)
raw.data.10$Date = lapply(raw.data.10$Date, FUN=parse_date)
raw.data.11$Date = lapply(raw.data.11$Date, FUN=parse_date)
raw.data.12$Date = lapply(raw.data.12$Date, FUN=parse_date)
raw.data.13$Date = lapply(raw.data.13$Date, FUN=parse_date)
raw.data.14$Date = lapply(raw.data.14$Date, FUN=parse_date)
raw.data.15$Date = lapply(raw.data.15$Date, FUN=parse_date)
raw.data.16$Date = lapply(raw.data.16$Date, FUN=parse_date)
raw.data.17$Date = lapply(raw.data.17$Date, FUN=parse_date)
raw.data.18$Date = lapply(raw.data.18$Date, FUN=parse_date)


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
  goalsscored = matrix(rep(0,2*(nrow(playing_stat))),ncol=38)
  teamnames=unique(playing_stat$HomeTeam)
  
  # count goals at Home and Away and create cumulative total per matchweek
  for (t in teamnames){
    HTGS=matrix(rep(0,2*19),ncol = 2)
    ATGS=matrix(rep(0,2*19),ncol = 2)
    for (i in 1:sum(playing_stat$HomeTeam==t)){
      HTGS[i,]=t(c(playing_stat[which(playing_stat$HomeTeam==t)[i],c("FTHG")],as.numeric(playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")])))
    }
    for (i in 1:sum(playing_stat$AwayTeam==t)){
      ATGS[i,]=t(c(playing_stat[which(playing_stat$AwayTeam==t)[i],c("FTAG")],as.numeric(playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")])))
    }
    TGS=rbind(HTGS,ATGS,c(0,0))
    TGS=TGS[order(TGS[,2]),]
    TGS=TGS[-nrow(TGS),]
    TGS[,1]=cumsum(TGS[,1])
    assign(t,TGS[,1])
    goalsscored[which(t==teamnames),]=t(TGS[,1])
  }  
  rownames(goalsscored)=teamnames
  return(goalsscored)
}

get_goals_conceded=function(playing_stat){
  # Create a dictionary with team names as keys
  goalsconceded = matrix(rep(0,2*(nrow(playing_stat))),ncol=38)
  teamnames=unique(playing_stat$HomeTeam)
  
  # count goals at Home and Away and create cumulative total per matchweek
  for (t in teamnames){
    HTGC=matrix(rep(0,2*19),ncol = 2)
    ATGC=matrix(rep(0,2*19),ncol = 2)
    for (i in 1:sum(playing_stat$HomeTeam==t)){
      HTGC[i,]=t(c(playing_stat[which(playing_stat$HomeTeam==t)[i],c("FTAG")],as.numeric(playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")])))
    }
    for (i in 1:sum(playing_stat$AwayTeam==t)){
      ATGC[i,]=t(c(playing_stat[which(playing_stat$AwayTeam==t)[i],c("FTHG")],as.numeric(playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")])))
    }
    TGC=rbind(HTGC,ATGC,c(0,0))
    TGC=TGC[order(TGC[,2]),]
    TGC=TGC[-nrow(TGC),]
    TGC[,1]=cumsum(TGC[,1])
    assign(t,TGC[,1])
    goalsconceded[which(t==teamnames),]=t(TGC[,1])
  }  
  rownames(goalsconceded)=teamnames
  return(goalsconceded)
}

get_gss=function(playing_stat){
  GC = get_goals_conceded(playing_stat)
  GS = get_goals_scored(playing_stat)
  
  j = 1
  HTGS = rep(0,nrow(playing_stat))
  ATGS = rep(0,nrow(playing_stat))
  HTGC = rep(0,nrow(playing_stat))
  ATGC = rep(0,nrow(playing_stat))
  
  for (i in 1:nrow(playing_stat)){
    ht = playing_stat$HomeTeam[i]
    at = playing_stat$AwayTeam[i]
    HTGS[i]=GS[ht,j]
    ATGS[i]=GS[at,j]
    HTGC[i]=GC[ht,j]
    ATGC[i]=GC[at,j]
    
    if ((i %% 10) == 0){
      j = j + 1
    }
    
    playing_stat['HTGS'] = HTGS
    playing_stat['ATGS'] = ATGS
    playing_stat['HTGC'] = HTGC
    playing_stat['ATGC'] = ATGC
  }
  return(playing_stat)
}
