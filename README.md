# Predicting Premier League
###What
Predicting next matchweek's 10 matches their outcome given in probablities per home, draw and away result.
###How
Automatic updating of past match details, including scraping lineups. Taking the next matchweek matches and combine this with expected lineups. Based on the past match details a eXtreme Gradient Boosted tree (XGBoost package) is trained and used to predict next matchweek matches.
### Dependencies 
**Continously**

- Historic match details: <http://www.football-data.co.uk>
- Historic line ups: <https://www.11v11.com>
- Expected line ups: <https://www.rotowire.com>

**(Half) Seasonly**

- Market Values: <https://www.transfermarkt.co.uk>
- Distance between venues: <http://www.sportmapworld.com>

**Sqlite database**
 
- `ENG_matches_hist`
	- Match details of historic matches are saved in this table
- `ENG_match_odds`
	- Match odds of historic matches (odds from InterWetten, LB and William Hill on Home, Draw and Away)
- `ENG_match_lineup`
	- Historic starting eleven line ups
- `ENG_players_mv`
	- Market Value of (most) players in the Premier League from 2006 onwards
- `ENG_exp_lineups`
	- Expected starting eleven lineups for the next matchweek games
- `ENG_R_lineup_input`
	- Transfer table for importing historic starting eleven line ups before adding them to `ENG_match_lineup`. Importing to this table is done through `ENG_db_updating.R` and exporting to `ENG_match_lineup`is through the trigger `R_lineup_update`
- `2017`     
