-- Table which new info has to be added to
INSERT INTO ENG_players_mv (season, team, name, MV)
SELECT season, team , name, MV
FROM (

-- The FROM table is the table having partially new information. 
-- Left join and where clause make sure only the new info is selected
       SELECT pmv1.*
       FROM player_mv_20172018_2 AS pmv1
       LEFT JOIN ENG_players_mv AS pmv_tot 
              ON pmv_tot.season = pmv1.season
              AND pmv_tot.team = pmv1.team
              AND pmv_tot.name = pmv1.name
       WHERE pmv_tot.name IS NULL
       )
       
