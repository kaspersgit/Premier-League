import_data_db <- function(include_odds){
  setwd("D:/Het Project/Voetbal predictions/Premier-League")
  # SQLite connection
  library("RSQLite")
  library(DBI)
  # connect to the sqlite file
  con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")
  if (include_odds){
    main_data <- dbGetQuery(con,'WITH step1 AS (
                            SELECT 
                            esl.match_date,
                            esl.hometeam,
                            esl.awayteam,
                            COALESCE(esl.season,epm1.season,epm5.season,epm8.season,epm12.season,epm15.season,epm19.season,epm22.season) AS season,
                            COALESCE(epm1.MV,1000) AS hmv1,
                            COALESCE(epm2.MV,1000) AS hmv2,
                            COALESCE(epm3.MV,1000) AS hmv3,
                            COALESCE(epm4.MV,1000) AS hmv4,
                            COALESCE(epm5.MV,1000) AS hmv5,
                            COALESCE(epm6.MV,1000) AS hmv6,
                            COALESCE(epm7.MV,1000) AS hmv7,
                            COALESCE(epm8.MV,1000) AS hmv8,
                            COALESCE(epm9.MV,1000) AS hmv9,
                            COALESCE(epm10.MV,1000) AS hmv10,
                            COALESCE(epm11.MV,1000) AS hmv11,
                            COALESCE(epm12.MV,1000) AS amv1,
                            COALESCE(epm13.MV,1000) AS amv2,
                            COALESCE(epm14.MV,1000) AS amv3,
                            COALESCE(epm15.MV,1000) AS amv4,
                            COALESCE(epm16.MV,1000) AS amv5,
                            COALESCE(epm17.MV,1000) AS amv6,
                            COALESCE(epm18.MV,1000) AS amv7,
                            COALESCE(epm19.MV,1000) AS amv8,
                            COALESCE(epm20.MV,1000) AS amv9,
                            COALESCE(epm21.MV,1000) AS amv10,
                            COALESCE(epm22.MV,1000) AS amv11
                            FROM 
                            ENG_match_lineup AS esl
                            LEFT JOIN 
                            ENG_players_mv AS epm1 ON epm1.name = esl.h1 AND epm1.season = esl.season AND epm1.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm2 ON epm2.name = esl.h2 AND epm2.season = esl.season AND epm2.team = esl.hometeam 
                            LEFT JOIN 
                            ENG_players_mv AS epm3 ON epm3.name = esl.h3  AND epm3.season = esl.season AND epm3.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm4 ON epm4.name = esl.h4  AND epm4.season = esl.season AND epm4.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm5 ON epm5.name = esl.h5  AND epm5.season = esl.season AND epm5.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm6 ON epm6.name = esl.h6  AND epm6.season = esl.season AND epm6.team = esl.hometeam
                            LEFT JOIN
                            ENG_players_mv AS epm7 ON epm7.name = esl.h7  AND epm7.season = esl.season AND epm7.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm8 ON epm8.name = esl.h8  AND epm8.season = esl.season AND epm8.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm9 ON epm9.name = esl.h9  AND epm9.season = esl.season AND epm9.team = esl.hometeam
                            LEFT JOIN
                            ENG_players_mv AS epm10 ON epm10.name = esl.h10 AND epm10.season = esl.season AND epm10.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm11 ON epm11.name = esl.h11 AND epm11.season = esl.season AND epm11.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm12 ON epm12.name = esl.a1 AND epm12.season = esl.season AND epm12.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm13 ON epm13.name = esl.a2 AND epm13.season = esl.season AND epm13.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm14 ON epm14.name = esl.a3 AND epm14.season = esl.season AND epm14.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm15 ON epm15.name = esl.a4 AND epm15.season = esl.season AND epm15.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm16 ON epm16.name = esl.a5 AND epm16.season = esl.season AND epm16.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm17 ON epm17.name = esl.a6 AND epm17.season = esl.season AND epm17.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm18 ON epm18.name = esl.a7 AND epm18.season = esl.season AND epm18.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm19 ON epm19.name = esl.a8 AND epm19.season = esl.season AND epm19.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm20 ON epm20.name = esl.a9 AND epm20.season = esl.season AND epm20.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm21 ON epm21.name = esl.a10 AND epm21.season = esl.season AND epm21.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm22 ON epm22.name = esl.a11 AND epm22.season = esl.season AND epm22.team = esl.awayteam
    ), 
                            with_duplicates AS (
                            SELECT 
                            *,
                            (hmv1 +
                            hmv2 +
                            hmv3 +
                            hmv4 +
                            hmv5 +
                            hmv6 +
                            hmv7 +
                            hmv8 +
                            hmv9 +
                            hmv10  +
                            hmv11) AS home_start_mv,  
                            (amv1 +
                            amv2 +
                            amv3 +
                            amv4 +
                            amv5 +
                            amv6 +
                            amv7 +
                            amv8 +
                            amv9 +
                            amv10  +
                            amv11) AS away_start_mv 
                            FROM 
                            step1
                            ),
                            step2 AS (
                            SELECT 
                            DISTINCT*
                            FROM 
                            with_duplicates
                            )
                            
                            SELECT 
                            emh.*,
                            emsm.home_start_mv,
                            emsm.away_start_mv,
                            IWH,
                            IWD,
                            IWA,
                            LBH,
                            LBD,
                            LBA,
                            WHH,
                            WHD,
                            WHA
                            FROM 
                            ENG_matches_hist AS emh
                            LEFT JOIN step2 AS emsm 
                            ON emsm.match_date = emh.Date
                            AND emsm.hometeam = emh.HomeTeam
                            AND emsm.awayteam = emh.AwayTeam
                            LEFT JOIN
                            ENG_match_odds AS emo
                            ON emo.Date = emh.Date
                            AND emo.HomeTeam = emh.HomeTeam
                            AND emo.AwayTeam = emh.AwayTeam')
    
  }else{
    main_data <- dbGetQuery(con,'WITH step1 AS (
                            SELECT 
                            esl.match_date,
                            esl.hometeam,
                            esl.awayteam,
                            COALESCE(esl.season,epm1.season,epm5.season,epm8.season,epm12.season,epm15.season,epm19.season,epm22.season) AS season,
                            COALESCE(epm1.MV,1000) AS hmv1,
                            COALESCE(epm2.MV,1000) AS hmv2,
                            COALESCE(epm3.MV,1000) AS hmv3,
                            COALESCE(epm4.MV,1000) AS hmv4,
                            COALESCE(epm5.MV,1000) AS hmv5,
                            COALESCE(epm6.MV,1000) AS hmv6,
                            COALESCE(epm7.MV,1000) AS hmv7,
                            COALESCE(epm8.MV,1000) AS hmv8,
                            COALESCE(epm9.MV,1000) AS hmv9,
                            COALESCE(epm10.MV,1000) AS hmv10,
                            COALESCE(epm11.MV,1000) AS hmv11,
                            COALESCE(epm12.MV,1000) AS amv1,
                            COALESCE(epm13.MV,1000) AS amv2,
                            COALESCE(epm14.MV,1000) AS amv3,
                            COALESCE(epm15.MV,1000) AS amv4,
                            COALESCE(epm16.MV,1000) AS amv5,
                            COALESCE(epm17.MV,1000) AS amv6,
                            COALESCE(epm18.MV,1000) AS amv7,
                            COALESCE(epm19.MV,1000) AS amv8,
                            COALESCE(epm20.MV,1000) AS amv9,
                            COALESCE(epm21.MV,1000) AS amv10,
                            COALESCE(epm22.MV,1000) AS amv11
                            FROM 
                            ENG_match_lineup AS esl
                            LEFT JOIN 
                            ENG_players_mv AS epm1 ON epm1.name = esl.h1 AND epm1.season = esl.season AND epm1.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm2 ON epm2.name = esl.h2 AND epm2.season = esl.season AND epm2.team = esl.hometeam 
                            LEFT JOIN 
                            ENG_players_mv AS epm3 ON epm3.name = esl.h3  AND epm3.season = esl.season AND epm3.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm4 ON epm4.name = esl.h4  AND epm4.season = esl.season AND epm4.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm5 ON epm5.name = esl.h5  AND epm5.season = esl.season AND epm5.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm6 ON epm6.name = esl.h6  AND epm6.season = esl.season AND epm6.team = esl.hometeam
                            LEFT JOIN
                            ENG_players_mv AS epm7 ON epm7.name = esl.h7  AND epm7.season = esl.season AND epm7.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm8 ON epm8.name = esl.h8  AND epm8.season = esl.season AND epm8.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm9 ON epm9.name = esl.h9  AND epm9.season = esl.season AND epm9.team = esl.hometeam
                            LEFT JOIN
                            ENG_players_mv AS epm10 ON epm10.name = esl.h10 AND epm10.season = esl.season AND epm10.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm11 ON epm11.name = esl.h11 AND epm11.season = esl.season AND epm11.team = esl.hometeam
                            LEFT JOIN 
                            ENG_players_mv AS epm12 ON epm12.name = esl.a1 AND epm12.season = esl.season AND epm12.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm13 ON epm13.name = esl.a2 AND epm13.season = esl.season AND epm13.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm14 ON epm14.name = esl.a3 AND epm14.season = esl.season AND epm14.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm15 ON epm15.name = esl.a4 AND epm15.season = esl.season AND epm15.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm16 ON epm16.name = esl.a5 AND epm16.season = esl.season AND epm16.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm17 ON epm17.name = esl.a6 AND epm17.season = esl.season AND epm17.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm18 ON epm18.name = esl.a7 AND epm18.season = esl.season AND epm18.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm19 ON epm19.name = esl.a8 AND epm19.season = esl.season AND epm19.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm20 ON epm20.name = esl.a9 AND epm20.season = esl.season AND epm20.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm21 ON epm21.name = esl.a10 AND epm21.season = esl.season AND epm21.team = esl.awayteam
                            LEFT JOIN 
                            ENG_players_mv AS epm22 ON epm22.name = esl.a11 AND epm22.season = esl.season AND epm22.team = esl.awayteam
    ), 
                            with_duplicates AS (
                            SELECT 
                            *,
                            (hmv1 +
                            hmv2 +
                            hmv3 +
                            hmv4 +
                            hmv5 +
                            hmv6 +
                            hmv7 +
                            hmv8 +
                            hmv9 +
                            hmv10  +
                            hmv11) AS home_start_mv,  
                            (amv1 +
                            amv2 +
                            amv3 +
                            amv4 +
                            amv5 +
                            amv6 +
                            amv7 +
                            amv8 +
                            amv9 +
                            amv10  +
                            amv11) AS away_start_mv 
                            FROM 
                            step1
                            ),
                            step2 AS (
                            SELECT 
                            DISTINCT*
                            FROM 
                            with_duplicates
                            )
                            
                            SELECT 
                            emh.*,
                            emsm.home_start_mv,
                            emsm.away_start_mv
                            FROM 
                            ENG_matches_hist AS emh
                            LEFT JOIN step2 AS emsm 
                            ON emsm.match_date = emh.Date
                            AND emsm.hometeam = emh.HomeTeam
                            AND emsm.awayteam = emh.AwayTeam')
    
    exp_data <- dbGetQuery(con,'WITH step1 AS (
                           SELECT 
                           esl.match_date,
                           esl.hometeam,
                           esl.awayteam,
                           COALESCE(esl.season,epm1.season,epm8.season,epm15.season,epm22.season) AS season,
                           COALESCE(epm1.MV,1000) AS hmv1,
                           COALESCE(epm2.MV,1000) AS hmv2,
                           COALESCE(epm3.MV,1000) AS hmv3,
                           COALESCE(epm4.MV,1000) AS hmv4,
                           COALESCE(epm5.MV,1000) AS hmv5,
                           COALESCE(epm6.MV,1000) AS hmv6,
                           COALESCE(epm7.MV,1000) AS hmv7,
                           COALESCE(epm8.MV,1000) AS hmv8,
                           COALESCE(epm9.MV,1000) AS hmv9,
                           COALESCE(epm10.MV,1000) AS hmv10,
                           COALESCE(epm11.MV,1000) AS hmv11,
                           COALESCE(epm12.MV,1000) AS amv1,
                           COALESCE(epm13.MV,1000) AS amv2,
                           COALESCE(epm14.MV,1000) AS amv3,
                           COALESCE(epm15.MV,1000) AS amv4,
                           COALESCE(epm16.MV,1000) AS amv5,
                           COALESCE(epm17.MV,1000) AS amv6,
                           COALESCE(epm18.MV,1000) AS amv7,
                           COALESCE(epm19.MV,1000) AS amv8,
                           COALESCE(epm20.MV,1000) AS amv9,
                           COALESCE(epm21.MV,1000) AS amv10,
                           COALESCE(epm22.MV,1000) AS amv11
                           FROM 
                           ENG_exp_lineups AS esl
                           LEFT JOIN 
                           ENG_players_mv AS epm1 ON epm1.name = esl.h1 AND epm1.season = esl.season AND epm1.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm2 ON epm2.name = esl.h2 AND epm2.season = esl.season AND epm2.team = esl.hometeam 
                           LEFT JOIN 
                           ENG_players_mv AS epm3 ON epm3.name = esl.h3  AND epm3.season = esl.season AND epm3.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm4 ON epm4.name = esl.h4  AND epm4.season = esl.season AND epm4.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm5 ON epm5.name = esl.h5  AND epm5.season = esl.season AND epm5.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm6 ON epm6.name = esl.h6  AND epm6.season = esl.season AND epm6.team = esl.hometeam
                           LEFT JOIN
                           ENG_players_mv AS epm7 ON epm7.name = esl.h7  AND epm7.season = esl.season AND epm7.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm8 ON epm8.name = esl.h8  AND epm8.season = esl.season AND epm8.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm9 ON epm9.name = esl.h9  AND epm9.season = esl.season AND epm9.team = esl.hometeam
                           LEFT JOIN
                           ENG_players_mv AS epm10 ON epm10.name = esl.h10 AND epm10.season = esl.season AND epm10.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm11 ON epm11.name = esl.h11 AND epm11.season = esl.season AND epm11.team = esl.hometeam
                           LEFT JOIN 
                           ENG_players_mv AS epm12 ON epm12.name = esl.a1 AND epm12.season = esl.season AND epm12.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm13 ON epm13.name = esl.a2 AND epm13.season = esl.season AND epm13.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm14 ON epm14.name = esl.a3 AND epm14.season = esl.season AND epm14.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm15 ON epm15.name = esl.a4 AND epm15.season = esl.season AND epm15.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm16 ON epm16.name = esl.a5 AND epm16.season = esl.season AND epm16.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm17 ON epm17.name = esl.a6 AND epm17.season = esl.season AND epm17.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm18 ON epm18.name = esl.a7 AND epm18.season = esl.season AND epm18.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm19 ON epm19.name = esl.a8 AND epm19.season = esl.season AND epm19.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm20 ON epm20.name = esl.a9 AND epm20.season = esl.season AND epm20.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm21 ON epm21.name = esl.a10 AND epm21.season = esl.season AND epm21.team = esl.awayteam
                           LEFT JOIN 
                           ENG_players_mv AS epm22 ON epm22.name = esl.a11 AND epm22.season = esl.season AND epm22.team = esl.awayteam
                           ), 
                           with_duplicates AS (
                           SELECT 
                           match_date,
                           hometeam,
                           awayteam,
                           season,
                           (hmv1 +
                           hmv2 +
                           hmv3 +
                           hmv4 +
                           hmv5 +
                           hmv6 +
                           hmv7 +
                           hmv8 +
                           hmv9 +
                           hmv10  +
                           hmv11) AS home_start_mv,  
                           (amv1 +
                           amv2 +
                           amv3 +
                           amv4 +
                           amv5 +
                           amv6 +
                           amv7 +
                           amv8 +
                           amv9 +
                           amv10  +
                           amv11) AS away_start_mv 
                           FROM 
                           step1
                           )
                           SELECT 
                           DISTINCT*
                           FROM 
                           with_duplicates
                           ')
  }
  
  dbDisconnect(con)
  return(list(main_data,exp_data))
}
