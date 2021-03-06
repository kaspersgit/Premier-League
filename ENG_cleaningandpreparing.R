ENG_preparation <- function(n_teams,include_odds){
  #update_db()
  
  all_data = import_data_db(include_odds)[[1]]
  exp_data = import_data_db(include_odds)[[2]]
  
  
  
  #making sure the underneath doesnt include season == NULL
  # raw.data.1 = all_data[which(all_data$season== 20002001),]
  # raw.data.2 = all_data[which(all_data$season== 20012002),]
  # raw.data.3 = all_data[which(all_data$season== 20022003),]
  # raw.data.4 = all_data[which(all_data$season== 20032004),]
  # raw.data.5 = all_data[which(all_data$season== 20042005),]
  # raw.data.6 = all_data[which(all_data$season== 20052006),]
  raw.data.7 = all_data[which(all_data$season== 20062007),]
  raw.data.8 = all_data[which(all_data$season== 20072008),]
  raw.data.9 = all_data[which(all_data$season== 20082009),]
  raw.data.10 = all_data[which(all_data$season== 20092010),]
  raw.data.11 = all_data[which(all_data$season== 20102011),]
  raw.data.12 = all_data[which(all_data$season== 20112012),]
  raw.data.13 = all_data[which(all_data$season== 20122013),]
  raw.data.14 = all_data[which(all_data$season== 20132014),]
  raw.data.15 = all_data[which(all_data$season== 20142015),]
  raw.data.16 = all_data[which(all_data$season== 20152016),]
  raw.data.17 = all_data[which(all_data$season== 20162017),]
  raw.data.18 = all_data[which(all_data$season== 20172018),]
  raw.data.19 = all_data[which(all_data$season== 20182019),]
  
  # get the latest available data
  n.games=nrow(raw.data.19)
  n.teams=n_teams
  
  # using the expected details and lineups saved in ENG_exp_lineups, this should be rbinded to a new season
  next.matches=exp_data
  
  # If lineup was not found team mv will be 11000 by settings, then take last team mv
  for (i in 1:nrow(next.matches)){
    if (next.matches$home_start_mv[i] == 11000) {
      last_mv = all_data$home_start_mv[tail(which(all_data$HomeTeam==next.matches$hometeam[i] & all_data$home_start_mv!=11000),1)]
      if (length(last_mv)!=0){
        next.matches$home_start_mv[i]=last_mv  
      }
    }  
  }
  
  fixtures=as.data.frame(matrix(rep(0,ncol(raw.data.19)*nrow(exp_data)),nrow=nrow(exp_data)))
  names(fixtures)=names(raw.data.19)
  #fixturesv <- merge(next.matches,fixtures, by.x = intersect(names(fixtures), names(next.matches)))
  fixtures[,intersect(names(fixtures), names(next.matches))] <- next.matches[,intersect(names(fixtures), names(next.matches))]

  
  #filling the dataframe with coming weeks matches
  fixtures$mid=c((max(all_data$mid)+1):(max(all_data$mid)+nrow(fixtures)))
  fixtures$Date=next.matches$match_date
  fixtures$Div=rep("E0",nrow(fixtures))
  fixtures$HomeTeam=next.matches$hometeam
  fixtures$AwayTeam=next.matches$awayteam
  fixtures$FTR=rep("D",nrow(fixtures))
  fixtures$HTR=rep("D",nrow(fixtures))
  fixtures$Referee=rep("D",nrow(fixtures))
  
  raw.data.19=rbind(raw.data.19,fixtures)
  
  
  delete.spaces=function(rawdata){
    rawdata$HomeTeam=str_replace_all(rawdata$HomeTeam, fixed(" "), "")
    rawdata$AwayTeam=str_replace_all(rawdata$AwayTeam, fixed(" "), "")
    return(rawdata)
  }
  
  # raw.data.1=delete.spaces(raw.data.1)
  # raw.data.2=delete.spaces(raw.data.2)
  # raw.data.3=delete.spaces(raw.data.3)
  # raw.data.4=delete.spaces(raw.data.4)
  # raw.data.5=delete.spaces(raw.data.5)
  # raw.data.6=delete.spaces(raw.data.6)
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
  raw.data.19=delete.spaces(raw.data.19)
  
  
  if (include_odds){
    ## to include the odds of InterWetten
    columns_req = c('Date','HomeTeam','AwayTeam','FTHG','FTAG','FTR','HS','AS','HST','AST','home_start_mv','away_start_mv','IWH','IWD','IWA')
  }else{
    # columns connected to gameplay and clean data from NA's
    columns_req = c('Date','HomeTeam','AwayTeam','FTHG','FTAG','FTR','HS','AS','HST','AST','home_start_mv','away_start_mv')
  }
  
  
  # playing_statistics_1 = raw.data.1[!is.na(raw.data.1$Date),columns_req]
  # playing_statistics_2 = raw.data.2[!is.na(raw.data.2$Date),columns_req]
  # playing_statistics_3 = raw.data.3[!is.na(raw.data.3$Date),columns_req]
  # playing_statistics_4 = raw.data.4[!is.na(raw.data.4$Date),columns_req]
  # playing_statistics_5 = raw.data.5[!is.na(raw.data.5$Date),columns_req]
  # playing_statistics_6 = raw.data.6[!is.na(raw.data.6$Date),columns_req]
  playing_statistics_7 = raw.data.7[!is.na(raw.data.7$Date),columns_req]
  playing_statistics_8 = raw.data.8[!is.na(raw.data.8$Date),columns_req]
  playing_statistics_9 = raw.data.9[!is.na(raw.data.9$Date),columns_req]
  playing_statistics_10 = raw.data.10[!is.na(raw.data.10$Date),columns_req]
  playing_statistics_11 = raw.data.11[!is.na(raw.data.11$Date),columns_req]
  playing_statistics_12 = raw.data.12[!is.na(raw.data.12$Date),columns_req]
  playing_statistics_13 = raw.data.13[!is.na(raw.data.13$Date),columns_req]
  playing_statistics_14 = raw.data.14[!is.na(raw.data.14$Date),columns_req]
  playing_statistics_15 = raw.data.15[!is.na(raw.data.15$Date),columns_req]
  playing_statistics_16 = raw.data.16[!is.na(raw.data.16$Date),columns_req]
  playing_statistics_17 = raw.data.17[!is.na(raw.data.17$Date),columns_req]
  playing_statistics_18 = raw.data.18[!is.na(raw.data.18$Date),columns_req]
  playing_statistics_19 = raw.data.19[!is.na(raw.data.19$Date),columns_req]
  
  # Gets the goals scored agg arranged by teams and matchweek
  get_goals_scored=function(playing_stat){
    # Create a dictionary with team names as keys
    goalsscored = matrix(rep(0,2*(n.teams*(n.teams-1))),ncol=2*(n.teams-1))
    teamnames=unique(playing_stat$HomeTeam)
    
    # count goals at Home and Away and create cumulative total per matchweek
    for (t in teamnames){
      HTGS=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      ATGS=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      for (i in 1:sum(playing_stat$HomeTeam==t)){
        HTGS[i,]=t(c(playing_stat[which(playing_stat$HomeTeam==t)[i],c("FTHG")],playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")]))
      }
      for (i in 1:sum(playing_stat$AwayTeam==t)){
        ATGS[i,]=t(c(playing_stat[which(playing_stat$AwayTeam==t)[i],c("FTAG")],playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")]))
      }
      TGS=rbind(HTGS,ATGS,c(0,0))
      TGS=TGS[order(TGS[,2]),]
      TGS=TGS[-nrow(TGS),]
      fill.up.length=matrix(rep(0,2*length(which(TGS[,2]==0)[-tail(which(TGS[,2]==0),1)])),ncol = 2)
      if (length(fill.up.length)>0){
        TGS=TGS[-which(TGS[,2]==0)[-tail(which(TGS[,2]==0),1)],]
        TGS=rbind(TGS,fill.up.length)
      }
      TGS[,1]=cumsum(TGS[,1])
      assign(t,TGS[,1])
      goalsscored[which(t==teamnames),]=t(TGS[,1])
    }  
    rownames(goalsscored)=teamnames
    return(goalsscored)
  }
  
  get_goals_conceded=function(playing_stat){
    # Create a dictionary with team names as keys
    goalsconceded = matrix(rep(0,2*(n.teams*(n.teams-1))),ncol=2*(n.teams-1))
    teamnames=unique(playing_stat$HomeTeam)
    
    # count goals at Home and Away and create cumulative total per matchweek
    for (t in teamnames){
      HTGC=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      ATGC=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      for (i in 1:sum(playing_stat$HomeTeam==t)){
        HTGC[i,]=t(c(playing_stat[which(playing_stat$HomeTeam==t)[i],c("FTAG")],as.numeric(as.Date(playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")]))))
      }
      for (i in 1:sum(playing_stat$AwayTeam==t)){
        ATGC[i,]=t(c(playing_stat[which(playing_stat$AwayTeam==t)[i],c("FTHG")],as.numeric(as.Date(playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")]))))
      }
      TGC=rbind(HTGC,ATGC,c(0,0))
      TGC=TGC[order(TGC[,2]),]
      TGC=TGC[-nrow(TGC),]
      fill.up.length=matrix(rep(0,2*length(which(TGC[,2]==0)[-tail(which(TGC[,2]==0),1)])),ncol = 2)
      if(length(fill.up.length)>0){
        TGC=TGC[-which(TGC[,2]==0)[-tail(which(TGC[,2]==0),1)],]
        TGC=rbind(TGC,fill.up.length)
      }
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
      
      if ((i %% (n.teams/2)) == 0){
        j = j + 1
      }
      
      playing_stat['HTGS'] = HTGS
      playing_stat['ATGS'] = ATGS
      playing_stat['HTGC'] = HTGC
      playing_stat['ATGC'] = ATGC
    }
    return(playing_stat)
  }
  
  # Apply to each dataset
  # playing_statistics_1 = get_gss(playing_statistics_1)
  # playing_statistics_2 = get_gss(playing_statistics_2)
  # playing_statistics_3 = get_gss(playing_statistics_3)
  # playing_statistics_4 = get_gss(playing_statistics_4)
  # playing_statistics_5 = get_gss(playing_statistics_5)
  # playing_statistics_6 = get_gss(playing_statistics_6)
  playing_statistics_7 = get_gss(playing_statistics_7)
  playing_statistics_8 = get_gss(playing_statistics_8)
  playing_statistics_9 = get_gss(playing_statistics_9)
  playing_statistics_10 = get_gss(playing_statistics_10)
  playing_statistics_11 = get_gss(playing_statistics_11)
  playing_statistics_12 = get_gss(playing_statistics_12)
  playing_statistics_13 = get_gss(playing_statistics_13)
  playing_statistics_14 = get_gss(playing_statistics_14)
  playing_statistics_15 = get_gss(playing_statistics_15)
  playing_statistics_16 = get_gss(playing_statistics_16)
  playing_statistics_17 = get_gss(playing_statistics_17)
  playing_statistics_18 = get_gss(playing_statistics_18)
  playing_statistics_19 = get_gss(playing_statistics_19)
  
  
  # Gets the shots agg arranged by teams and matchweek
  get_shots=function(playing_stat){
    # Create a dictionary with team names as keys
    shots = matrix(rep(0,2*n.teams*(n.teams-1)),ncol=2*(n.teams-1))
    teamnames=unique(playing_stat$HomeTeam)
    
    # count goals at Home and Away and create cumulative total per matchweek
    for (t in teamnames){
      HTS=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      ATS=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      for (i in 1:sum(playing_stat$HomeTeam==t)){
        HTS[i,]=t(c(playing_stat[which(playing_stat$HomeTeam==t)[i],"HS"],playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")]))
      }
      for (i in 1:sum(playing_stat$AwayTeam==t)){
        ATS[i,]=t(c(playing_stat[which(playing_stat$AwayTeam==t)[i],c("AS")],playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")]))
      }
      TS=rbind(HTS,ATS,c(0,0))
      TS=TS[order(TS[,2]),]
      TS=TS[-nrow(TS),]
      fill.up.length=matrix(rep(0,2*length(which(TS[,2]==0)[-tail(which(TS[,2]==0),1)])),ncol = 2)
      if (length(fill.up.length)>0){
        TS=TS[-which(TS[,2]==0)[-tail(which(TS[,2]==0),1)],]
        TS=rbind(TS,fill.up.length)
      }
      TS[,1]=cumsum(TS[,1])
      assign(t,TS[,1])
      shots[which(t==teamnames),]=t(TS[,1])
    }  
    rownames(shots)=teamnames
    return(shots)
  }
  
  get_shots_on_target=function(playing_stat){
    # Create a dictionary with team names as keys
    shots_on_target = matrix(rep(0,2*n.teams*(n.teams-1)),ncol=2*(n.teams-1))
    teamnames=unique(playing_stat$HomeTeam)
    
    # count goals at Home and Away and create cumulative total per matchweek
    for (t in teamnames){
      HTST=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      ATST=matrix(rep(0,2*(n.teams-1)),ncol = 2)
      for (i in 1:sum(playing_stat$HomeTeam==t)){
        HTST[i,]=t(c(playing_stat[which(playing_stat$HomeTeam==t)[i],c("HST")],as.numeric(as.Date(playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")]))))
      }
      for (i in 1:sum(playing_stat$AwayTeam==t)){
        ATST[i,]=t(c(playing_stat[which(playing_stat$AwayTeam==t)[i],c("AST")],as.numeric(as.Date(playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")]))))
      }
      TST=rbind(HTST,ATST,c(0,0))
      TST=TST[order(TST[,2]),]
      TST=TST[-nrow(TST),]
      fill.up.length=matrix(rep(0,2*length(which(TST[,2]==0)[-tail(which(TST[,2]==0),1)])),ncol = 2)
      if(length(fill.up.length)>0){
        TST=TST[-which(TST[,2]==0)[-tail(which(TST[,2]==0),1)],]
        TST=rbind(TST,fill.up.length)
      }
      TST[,1]=cumsum(TST[,1])
      assign(t,TST[,1])
      shots_on_target[which(t==teamnames),]=t(TST[,1])
    }  
    rownames(shots_on_target)=teamnames
    return(shots_on_target)
  }
  
  get_sst=function(playing_stat){
    shots = get_shots(playing_stat)
    shotstarget = get_shots_on_target(playing_stat)
    
    j = 1
    HTS = rep(0,nrow(playing_stat))
    ATS = rep(0,nrow(playing_stat))
    HTST = rep(0,nrow(playing_stat))
    ATST = rep(0,nrow(playing_stat))
    
    for (i in 1:nrow(playing_stat)){
      ht = playing_stat$HomeTeam[i]
      at = playing_stat$AwayTeam[i]
      HTS[i]=shots[ht,j]
      ATS[i]=shots[at,j]
      HTST[i]=shotstarget[ht,j]
      ATST[i]=shotstarget[at,j]
      
      if ((i %% (n.teams/2)) == 0){
        j = j + 1
      }
      
      playing_stat['HTS'] = HTS
      playing_stat['ATS'] = ATS
      playing_stat['HTST'] = HTST
      playing_stat['ATST'] = ATST
    }
    return(playing_stat)
  }
  
  # Apply to each dataset
  # playing_statistics_1 = get_sst(playing_statistics_1)
  # playing_statistics_2 = get_sst(playing_statistics_2)
  # playing_statistics_3 = get_sst(playing_statistics_3)
  # playing_statistics_4 = get_sst(playing_statistics_4)
  # playing_statistics_5 = get_sst(playing_statistics_5)
  # playing_statistics_6 = get_sst(playing_statistics_6)
  playing_statistics_7 = get_sst(playing_statistics_7)
  playing_statistics_8 = get_sst(playing_statistics_8)
  playing_statistics_9 = get_sst(playing_statistics_9)
  playing_statistics_10 = get_sst(playing_statistics_10)
  playing_statistics_11 = get_sst(playing_statistics_11)
  playing_statistics_12 = get_sst(playing_statistics_12)
  playing_statistics_13 = get_sst(playing_statistics_13)
  playing_statistics_14 = get_sst(playing_statistics_14)
  playing_statistics_15 = get_sst(playing_statistics_15)
  playing_statistics_16 = get_sst(playing_statistics_16)
  playing_statistics_17 = get_sst(playing_statistics_17)
  playing_statistics_18 = get_sst(playing_statistics_18)
  playing_statistics_19 = get_sst(playing_statistics_19)
  
  # get respective points
  get_points_gained=function(playing_stat){
    # Create a dictionary with team names as keys
    teamnames=unique(playing_stat$HomeTeam)
    n_teams=length(teamnames)
    n_rounds=ceiling(nrow(playing_stat)/(n.teams/2))
    pointsgained = matrix(rep(0,n_rounds*n_teams),ncol=n_rounds)
    
    HFTR.point=rep(0,nrow(playing_stat))
    AFTR.point=rep(0,nrow(playing_stat))
    
    for (i in 1:nrow(playing_stat)){
      if(playing_stat$FTR[i]=="H"){
        HFTR.point[i]=3
        AFTR.point[i]=0
      }else if(playing_stat$FTR[i]=="D"){
        HFTR.point[i]=1
        AFTR.point[i]=1
      }else{
        HFTR.point[i]=0
        AFTR.point[i]=3
      }
    }
    
    # Count points home and away, order by date and then calculate a cumulative
    # Becuase all the seasons are aggregated here, a team not being in the PL during a season gets ?? points per round
    # this to keep the last n games form variable working, Determining matchweek is difficult as some games can be 
    # delayed and played in another matchweek. There is no back track to the original matchweek in this script.
    for (t in teamnames){
      
      HTP=matrix(rep(0,2*n_rounds),ncol = 2)
      ATP=matrix(rep(0,2*n_rounds),ncol = 2)
      
      for (i in 1:sum(playing_stat$HomeTeam==t)){

        w_round=ceiling(which(playing_stat$HomeTeam==t)[i]/(n.teams/2))
        HTP[w_round,]=t(c(HFTR.point[which(playing_stat$HomeTeam==t)[i]],as.Date(playing_stat[which(playing_stat$HomeTeam==t)[i],c("Date")])))
      }
      for (i in 1:sum(playing_stat$AwayTeam==t)){
        w_round=ceiling(which(playing_stat$AwayTeam==t)[i]/(n.teams/2))

        ATP[w_round,]=t(c(AFTR.point[which(playing_stat$AwayTeam==t)[i]],as.Date(playing_stat[which(playing_stat$AwayTeam==t)[i],c("Date")])))
      }
      TP=HTP+ATP
      TP=rbind(c(0,0),TP[-nrow(TP),])
      TP[,1]=cumsum(TP[,1])
      assign(t,TP[,1])
      pointsgained[which(t==teamnames),]=t(TP[,1])
    }  
    rownames(pointsgained)=teamnames
    return(pointsgained)
  }
  
  get_agg_points=function(playing_stat){
    PG = get_points_gained(playing_stat)
    
    j = 1
    HTP = rep(0,nrow(playing_stat))
    ATP = rep(0,nrow(playing_stat))
    
    for (i in 1:nrow(playing_stat)){
      ht = playing_stat$HomeTeam[i]
      at = playing_stat$AwayTeam[i]
      HTP[i]=PG[ht,j]
      ATP[i]=PG[at,j]
      
      if ((i %% (n.teams/2)) == 0){
        j = j + 1
      }
      
      playing_stat['HTP'] = as.numeric(unlist(HTP))
      playing_stat['ATP'] = as.numeric(unlist(ATP))
    }
    return(playing_stat)
  }
  
  # Apply to each dataset
  # playing_statistics_1 = get_agg_points(playing_statistics_1)
  # playing_statistics_2 = get_agg_points(playing_statistics_2)
  # playing_statistics_3 = get_agg_points(playing_statistics_3)
  # playing_statistics_4 = get_agg_points(playing_statistics_4)
  # playing_statistics_5 = get_agg_points(playing_statistics_5)
  # playing_statistics_6 = get_agg_points(playing_statistics_6)
  playing_statistics_7 = get_agg_points(playing_statistics_7)
  playing_statistics_8 = get_agg_points(playing_statistics_8)
  playing_statistics_9 = get_agg_points(playing_statistics_9)
  playing_statistics_10 = get_agg_points(playing_statistics_10)
  playing_statistics_11 = get_agg_points(playing_statistics_11)
  playing_statistics_12 = get_agg_points(playing_statistics_12)
  playing_statistics_13 = get_agg_points(playing_statistics_13)
  playing_statistics_14 = get_agg_points(playing_statistics_14)
  playing_statistics_15 = get_agg_points(playing_statistics_15)
  playing_statistics_16 = get_agg_points(playing_statistics_16)
  playing_statistics_17 = get_agg_points(playing_statistics_17)
  playing_statistics_18 = get_agg_points(playing_statistics_18)
  playing_statistics_19 = get_agg_points(playing_statistics_19)
  
  if (include_odds){
    ## to include the odds of InterWetten
    # Rearranging columns with InterWetten odds
    cols = c('Date', 'HomeTeam', 'AwayTeam', 'FTHG', 'FTAG', 'FTR', 'HTGS', 'ATGS', 'HTGC', 'ATGC', 'HTP', 'ATP','HTS','ATS','HTST','ATST','home_start_mv','away_start_mv','IWH', 'IWD', 'IWA')
    
  }else{ 
    # Rearranging columns
    cols = c('Date', 'HomeTeam', 'AwayTeam', 'FTHG', 'FTAG', 'FTR', 'HTGS', 'ATGS', 'HTGC', 'ATGC', 'HTP', 'ATP','HTS','ATS','HTST','ATST','home_start_mv','away_start_mv')
  }
  
  
  
  
  # playing_statistics_1 = playing_statistics_1[cols]
  # playing_statistics_2 = playing_statistics_2[cols]
  # playing_statistics_3 = playing_statistics_3[cols]
  # playing_statistics_4 = playing_statistics_4[cols]
  # playing_statistics_5 = playing_statistics_5[cols]
  # playing_statistics_6 = playing_statistics_6[cols]
  playing_statistics_7 = playing_statistics_7[cols]
  playing_statistics_8 = playing_statistics_8[cols]
  playing_statistics_9 = playing_statistics_9[cols]
  playing_statistics_10 = playing_statistics_10[cols]
  playing_statistics_11 = playing_statistics_11[cols]
  playing_statistics_12 = playing_statistics_12[cols]
  playing_statistics_13 = playing_statistics_13[cols]
  playing_statistics_14 = playing_statistics_14[cols]
  playing_statistics_15 = playing_statistics_15[cols]
  playing_statistics_16 = playing_statistics_16[cols]
  playing_statistics_17 = playing_statistics_17[cols]
  playing_statistics_18 = playing_statistics_18[cols]
  playing_statistics_19 = playing_statistics_19[cols]
  
  #Get Last Year's Position as also an independent variable:
  Standings = read.csv("yearly_updated_data/EPLStandings.csv", sep = ";")
  Standings[,1]=str_replace_all(Standings[,1], fixed(" "), "")
  rownames(Standings)=Standings[,1]
  Standings=Standings[,-1]
  Standings[is.na(Standings)]=(n.teams-2)
  
  get_last=function(playing_stat, Standings, year){
    HomeTeamLP = rep(0,nrow(playing_stat))
    AwayTeamLP = rep(0,nrow(playing_stat))
    for (i in 1:nrow(playing_stat)){
      ht = playing_stat$HomeTeam[i]
      at = playing_stat$AwayTeam[i]
      HomeTeamLP[i]=Standings[ht,year]
      AwayTeamLP[i]=Standings[at,year]
    } 
    playing_stat['HomeTeamLP'] = HomeTeamLP
    playing_stat['AwayTeamLP'] = AwayTeamLP
    return (playing_stat)
  }
  
  # playing_statistics_1 = get_last(playing_statistics_1, Standings, "X2000")
  # playing_statistics_2 = get_last(playing_statistics_2, Standings, "X2001")
  # playing_statistics_3 = get_last(playing_statistics_3, Standings, "X2002")
  # playing_statistics_4 = get_last(playing_statistics_4, Standings, "X2003")
  # playing_statistics_5 = get_last(playing_statistics_5, Standings, "X2004")
  # playing_statistics_6 = get_last(playing_statistics_6, Standings, "X2005")
  playing_statistics_7 = get_last(playing_statistics_7, Standings, "X2006")
  playing_statistics_8 = get_last(playing_statistics_8, Standings, "X2007")
  playing_statistics_9 = get_last(playing_statistics_9, Standings, "X2008")
  playing_statistics_10 = get_last(playing_statistics_10, Standings, "X2009")
  playing_statistics_11 = get_last(playing_statistics_11, Standings, "X2010")
  playing_statistics_12 = get_last(playing_statistics_12, Standings, "X2011")
  playing_statistics_13 = get_last(playing_statistics_13, Standings, "X2012")
  playing_statistics_14 = get_last(playing_statistics_14, Standings, "X2013")
  playing_statistics_15 = get_last(playing_statistics_15, Standings, "X2014")
  playing_statistics_16 = get_last(playing_statistics_16, Standings, "X2015")
  playing_statistics_17 = get_last(playing_statistics_17, Standings, "X2016")
  playing_statistics_18 = get_last(playing_statistics_18, Standings, "X2017")
  playing_statistics_19 = get_last(playing_statistics_19, Standings, "X2018")
  
  #Get average age as also an independent variable:
  AvgAge = read.csv("yearly_updated_data/AvgAge.csv", sep = ",")
  AvgAge[,1]=str_replace_all(AvgAge[,1], fixed(" "), "")
  rownames(AvgAge)=AvgAge[,1]
  AvgAge=AvgAge[,-1]
  
  get_AvgAge=function(playing_stat, AvgAge, year){
    HomeTeamAA = rep(0,nrow(playing_stat))
    AwayTeamAA = rep(0,nrow(playing_stat))
    for (i in 1:nrow(playing_stat)){
      ht = playing_stat$HomeTeam[i]
      at = playing_stat$AwayTeam[i]
      HomeTeamAA[i]=AvgAge[ht,year]
      AwayTeamAA[i]=AvgAge[at,year]
    } 
    playing_stat['HomeAvgAge'] = HomeTeamAA
    playing_stat['AwayAvgAge'] = AwayTeamAA
    return (playing_stat)
  }
  
  # playing_statistics_1 = get_AvgAge(playing_statistics_1, AvgAge, "X2000")
  # playing_statistics_2 = get_AvgAge(playing_statistics_2, AvgAge, "X2001")
  # playing_statistics_3 = get_AvgAge(playing_statistics_3, AvgAge, "X2002")
  # playing_statistics_4 = get_AvgAge(playing_statistics_4, AvgAge, "X2003")
  # playing_statistics_5 = get_AvgAge(playing_statistics_5, AvgAge, "X2004")
  # playing_statistics_6 = get_AvgAge(playing_statistics_6, AvgAge, "X2005")
  playing_statistics_7 = get_AvgAge(playing_statistics_7, AvgAge, "X2006")
  playing_statistics_8 = get_AvgAge(playing_statistics_8, AvgAge, "X2007")
  playing_statistics_9 = get_AvgAge(playing_statistics_9, AvgAge, "X2008")
  playing_statistics_10 = get_AvgAge(playing_statistics_10, AvgAge, "X2009")
  playing_statistics_11 = get_AvgAge(playing_statistics_11, AvgAge, "X2010")
  playing_statistics_12 = get_AvgAge(playing_statistics_12, AvgAge, "X2011")
  playing_statistics_13 = get_AvgAge(playing_statistics_13, AvgAge, "X2012")
  playing_statistics_14 = get_AvgAge(playing_statistics_14, AvgAge, "X2013")
  playing_statistics_15 = get_AvgAge(playing_statistics_15, AvgAge, "X2014")
  playing_statistics_16 = get_AvgAge(playing_statistics_16, AvgAge, "X2015")
  playing_statistics_17 = get_AvgAge(playing_statistics_17, AvgAge, "X2016")
  playing_statistics_18 = get_AvgAge(playing_statistics_18, AvgAge, "X2017")
  playing_statistics_19 = get_AvgAge(playing_statistics_19, AvgAge, "X2018")
  
  #Get average Market Value as also an independent variable:
  AvgMV = read.csv("yearly_updated_data/AvgMV.csv", sep = ",")
  AvgMV[,1]=str_replace_all(AvgMV[,1], fixed(" "), "")
  rownames(AvgMV)=AvgMV[,1]
  AvgMV=AvgMV[,-1]
  
  get_AvgMV=function(playing_stat, AvgMV, year){
    HomeTeamAA = rep(0,nrow(playing_stat))
    AwayTeamAA = rep(0,nrow(playing_stat))
    for (i in 1:nrow(playing_stat)){
      ht = playing_stat$HomeTeam[i]
      at = playing_stat$AwayTeam[i]
      HomeTeamAA[i]=AvgMV[ht,year]
      AwayTeamAA[i]=AvgMV[at,year]
    } 
    playing_stat['HomeAvgMV'] = HomeTeamAA
    playing_stat['AwayAvgMV'] = AwayTeamAA
    return (playing_stat)
  }
  
  # not available until 2005
  # playing_statistics_1 = get_AvgMV(playing_statistics_1, AvgMV, "X2000")
  # playing_statistics_2 = get_AvgMV(playing_statistics_2, AvgMV, "X2001")
  # playing_statistics_3 = get_AvgMV(playing_statistics_3, AvgMV, "X2002")
  # playing_statistics_4 = get_AvgMV(playing_statistics_4, AvgMV, "X2003")
  # playing_statistics_5 = get_AvgMV(playing_statistics_5, AvgMV, "X2004")
  # playing_statistics_6 = get_AvgMV(playing_statistics_6, AvgMV, "X2005")
  playing_statistics_7 = get_AvgMV(playing_statistics_7, AvgMV, "X2006")
  playing_statistics_8 = get_AvgMV(playing_statistics_8, AvgMV, "X2007")
  playing_statistics_9 = get_AvgMV(playing_statistics_9, AvgMV, "X2008")
  playing_statistics_10 = get_AvgMV(playing_statistics_10, AvgMV, "X2009")
  playing_statistics_11 = get_AvgMV(playing_statistics_11, AvgMV, "X2010")
  playing_statistics_12 = get_AvgMV(playing_statistics_12, AvgMV, "X2011")
  playing_statistics_13 = get_AvgMV(playing_statistics_13, AvgMV, "X2012")
  playing_statistics_14 = get_AvgMV(playing_statistics_14, AvgMV, "X2013")
  playing_statistics_15 = get_AvgMV(playing_statistics_15, AvgMV, "X2014")
  playing_statistics_16 = get_AvgMV(playing_statistics_16, AvgMV, "X2015")
  playing_statistics_17 = get_AvgMV(playing_statistics_17, AvgMV, "X2016")
  playing_statistics_18 = get_AvgMV(playing_statistics_18, AvgMV, "X2017")
  playing_statistics_19 = get_AvgMV(playing_statistics_19, AvgMV, "X2018")
  
  #Get MatchWeek
  get_mw=function(playing_stat){
    j = 1
    MatchWeek = rep(0,nrow(playing_stat))
    for (i in 1:nrow(playing_stat)){
      MatchWeek[i]=j
      if ((i %% (n.teams/2)) == 0){
        j = j + 1
      }
    }
    playing_stat['MW'] = MatchWeek
    return(playing_stat)
  }
  
  # playing_statistics_1 = get_mw(playing_statistics_1)
  # playing_statistics_2 = get_mw(playing_statistics_2)
  # playing_statistics_3 = get_mw(playing_statistics_3)
  # playing_statistics_4 = get_mw(playing_statistics_4)
  # playing_statistics_5 = get_mw(playing_statistics_5)
  # playing_statistics_6 = get_mw(playing_statistics_6)
  playing_statistics_7 = get_mw(playing_statistics_7)
  playing_statistics_8 = get_mw(playing_statistics_8)
  playing_statistics_9 = get_mw(playing_statistics_9)
  playing_statistics_10 = get_mw(playing_statistics_10)
  playing_statistics_11 = get_mw(playing_statistics_11)
  playing_statistics_12 = get_mw(playing_statistics_12)
  playing_statistics_13 = get_mw(playing_statistics_13)
  playing_statistics_14 = get_mw(playing_statistics_14)
  playing_statistics_15 = get_mw(playing_statistics_15)
  playing_statistics_16 = get_mw(playing_statistics_16)
  playing_statistics_17 = get_mw(playing_statistics_17)
  playing_statistics_18 = get_mw(playing_statistics_18)
  playing_statistics_19 = get_mw(playing_statistics_19)
  
  # Combining to one dataset, start up mv only available from nr. 7
  playing_stat = rbind(playing_statistics_7,
                       playing_statistics_8,
                       playing_statistics_9,
                       playing_statistics_10,
                       playing_statistics_11,
                       playing_statistics_12,
                       playing_statistics_13,
                       playing_statistics_14,
                       playing_statistics_15,
                       playing_statistics_16,
                       playing_statistics_17,
                       playing_statistics_18,
                       playing_statistics_19)
  
  # Form defined as result of the last 5 games
  get_form=function(playing_stat,num){
    form = get_points_gained(playing_stat)
    form_final = form*0
    n_teams = length(unique(playing_stat$HomeTeam))
    n_rounds = ceiling(nrow(playing_stat)/(n_teams/2))
    for (i in (num+1):(n_rounds)){
      form_final[,i] = form[,i]-form[,i-min(num,i+1)]
    }
    return(form_final)
  }
  
  add_form=function(playing_stat,num){
    form = get_form(playing_stat,num)
    n_teams = length(unique(playing_stat$HomeTeam))
    n_rounds = ceiling(nrow(playing_stat)/(n.teams/2))
    
    h=vector(mode="numeric",nrow(playing_stat))
    a=vector(mode="numeric",nrow(playing_stat))
    for (i in 1:(num*(n.teams/2))){
      h[i] = 0  # since form is not available for n MW (n*10)
      a[i] = 0 
    }
    j = num+1
    for (i in (num*(n.teams/2)+1):nrow(playing_stat)){
      ht = playing_stat$HomeTeam[i]
      at = playing_stat$AwayTeam[i]
      
      past = form[ht,j]               # get past n results
      h[i]=past                   # 1 index is most recent
      
      past = form[at,j]               # get past n results.
      a[i]=past                  # 1 index is most recent
      
      if ((i %% (n.teams/2)) == 0){
        j = j + 1
      }
    }
    
    playing_stat[paste('HM',num,sep = "")] = h                 
    playing_stat[paste('AM',num,sep = "")] = a
    
    
    return(playing_stat)
  }
  
  add_form_df=function(playing_statistics){
    amount.games=nrow(playing_statistics)
    if(amount.games>=2*(n.teams/2)){playing_statistics = add_form(playing_statistics,1)}
    if(amount.games>=3*(n.teams/2)){playing_statistics = add_form(playing_statistics,2)}
    if(amount.games>=4*(n.teams/2)){playing_statistics = add_form(playing_statistics,3)}
    if(amount.games>=5*(n.teams/2)){playing_statistics = add_form(playing_statistics,4)}
    if(amount.games>=6*(n.teams/2)){playing_statistics = add_form(playing_statistics,5)}
    if(amount.games>=11*(n.teams/2)){playing_statistics = add_form(playing_statistics,10)}
    if(amount.games>=21*(n.teams/2)){playing_statistics = add_form(playing_statistics,20)}
    return(playing_statistics)
  }
  
  # adding form of last match in same situation (home/away)
  add_form_same_venue = function(playing_statistics){
    
    hmh1=vector(mode="numeric",nrow(playing_stat))
    ama1=vector(mode="numeric",nrow(playing_stat))
    
    for (i in (n.teams/2+1):nrow(playing_statistics)){
      home=playing_statistics$HomeTeam[i]
      away=playing_statistics$AwayTeam[i]
      
      # subset data until the game, so it is easier to get the last game from this team
      hist_data = playing_statistics[1:i,]
      
      home_last_result=hist_data$FTR[tail(which(hist_data$HomeTeam==home),2)[1]]
      away_last_result=hist_data$FTR[tail(which(hist_data$AwayTeam==away),2)[1]]
      
      hmh1[i]=ifelse(home_last_result=='H',3,ifelse(home_last_result=='D',1,0))
      ama1[i]=ifelse(away_last_result=='A',3,ifelse(away_last_result=='D',1,0))
      
    }
    playing_statistics['HMH1']=hmh1
    playing_statistics['AMA1']=ama1
    
    return(playing_statistics)
    
  }
  
  # Add points from last 1/3/5/10 and 20 games
  playing_stat = add_form_df(playing_stat)
  
  # Add points from last match on same venue situation (home/away)
  playing_stat = add_form_same_venue(playing_stat)
  
  ### Add distance between clubs playing grounds (air distance in km) 
  distances=read.csv("yearly_updated_data/distances.csv",row.names = 1)
  colnames(distances)=str_replace_all(colnames(distances), fixed("."), "")
  rownames(distances)=str_replace_all(rownames(distances), fixed(" "), "")
  
  get_distance=function(playing_statd){
    Distance = rep(0,nrow(playing_statd))
    for (i in 1:nrow(playing_statd)){
      ht = playing_statd$HomeTeam[i]
      at = playing_statd$AwayTeam[i]
      Distance[i]=distances[ht,at]
    } 
    playing_statd["Distance"] = Distance
    return (playing_statd)
  }
  
  playing_stat=get_distance(playing_stat)
  
  # Get Goal Difference
  playing_stat['HTGD'] = as.numeric(unlist(playing_stat['HTGS'])) - as.numeric(unlist(playing_stat['HTGC']))
  playing_stat['ATGD'] = as.numeric(unlist(playing_stat['ATGS'])) - as.numeric(unlist(playing_stat['ATGC']))
  
  # Diff in points
  playing_stat['DiffPts'] = playing_stat['HTP'] - playing_stat['ATP']
  
  # Diff in last year positions
  playing_stat['DiffLP'] = playing_stat['HomeTeamLP'] - playing_stat['AwayTeamLP']
  
  # Scale DiffPts , DiffFormPts, HTGD, ATGD by Matchweek.
  cols = c('HTGD','ATGD','DiffPts','HTP','ATP','HTS','ATS','HTST','ATST')
  
  for (col in cols){
    playing_stat[col] = as.numeric(unlist(playing_stat[col])) / as.numeric(unlist(playing_stat$MW))
  }
  
  write.csv(playing_stat,"ENG_final_dataset.csv")
}
