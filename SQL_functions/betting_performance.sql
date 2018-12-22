-- Get which teams were predicted good most of the time
WITH prediction AS (
SELECT 
	*
	, CASE 
		WHEN PredictedOutcome = HomeTeam THEN 'H'
		WHEN PredictedOutcome = AwayTeam THEN 'A'
		ELSE 'D'
	END pred_FTR
from 
	ENG_match_prediction
)

, count_per_team AS (
	SELECT 
	FTR = pred_FTR AS correct_pred
	, FTR = pred_FTR AND best_ratio_outcome = FTR AS correct_bet
	, FTR
	, pred.HomeTeam as team
FROM 
       prediction AS pred
LEFT JOIN 
       ENG_matches_hist AS hist 
       ON date(pred.MatchDate)=hist.`Date` 
       AND pred.HomeTeam = replace(hist.HomeTeam,' ','')
       AND pred.AwayTeam = replace(hist.AwayTeam,' ','')

 UNION ALL 

 	SELECT 
	FTR = pred_FTR AS correct_pred
	, FTR = pred_FTR AND best_ratio_outcome = FTR AS correct_bet
	, FTR
	, pred.AwayTeam as team
FROM 
       prediction AS pred
LEFT JOIN 
       ENG_matches_hist AS hist 
       ON date(pred.MatchDate)=hist.`Date` 
       AND pred.HomeTeam = replace(hist.HomeTeam,' ','')
       AND pred.AwayTeam = replace(hist.AwayTeam,' ','')
)

SELECT 
	team
	, count(1) as counting
FROM 
	count_per_team
WHERE 
	correct_pred
GROUP BY 
	team
ORDER BY 
	counting desc
;


-- Get the prediction performance per H,A and D
WITH prediction AS (
SELECT 
	*
	, CASE 
		WHEN PredictedOutcome = HomeTeam THEN 'H'
		WHEN PredictedOutcome = AwayTeam THEN 'A'
		ELSE 'D'
	END pred_FTR
from 
	ENG_match_prediction
),
bet_stats AS (

	SELECT 
		SUM(CASE WHEN FTR = pred_FTR THEN 1 ELSE 0 END) AS correct_pred
		, SUM(CASE WHEN FTR = pred_FTR AND best_ratio_outcome = FTR THEN 1 ELSE 0 END) AS correct_bet	
		, SUM(CASE WHEN best_ratio_outcome = pred_FTR THEN 1 ELSE 0 END) AS betted
		, SUM(1) AS total
		, pred_FTR
		, FTR 
		, AVG(CASE 
				WHEN pred_FTR = 'H' THEN BF_H_odds
				WHEN pred_FTR = 'A' THEN BF_A_odds
				ELSE BF_D_odds
		END) AS avg_bf_odds
		, AVG(CASE 
				WHEN pred_FTR = 'H' THEN HomeOdd
				WHEN pred_FTR = 'A' THEN AwayOdd
				ELSE DrawOdd
		END) AS pred_odds
		, AVG(CASE 
				WHEN pred_FTR = 'H' AND best_ratio_outcome = pred_FTR THEN BF_H_odds
				WHEN pred_FTR = 'A' AND best_ratio_outcome = pred_FTR THEN BF_A_odds
				WHEN pred_FTR = 'D' AND best_ratio_outcome = pred_FTR THEN BF_D_odds
				ELSE NULL
		END) AS betted_bf_odds
		, AVG(CASE 
				WHEN pred_FTR = 'H' AND best_ratio_outcome = pred_FTR THEN HomeOdd
				WHEN pred_FTR = 'A' AND best_ratio_outcome = pred_FTR THEN AwayOdd
				WHEN pred_FTR = 'D' AND best_ratio_outcome = pred_FTR THEN DrawOdd
				ELSE NULL
		END) AS betted_pred_odds	
	
	

	FROM 
	       prediction AS pred
	INNER JOIN 
	       ENG_matches_hist AS hist 
	       ON date(pred.MatchDate)=hist.`Date` 
	       AND pred.HomeTeam = replace(hist.HomeTeam,' ','')
	       AND pred.AwayTeam = replace(hist.AwayTeam,' ','')
	GROUP BY 
		pred_FTR
		, FTR
)
SELECT 
	*
	, round(betted * (10/betted_bf_odds),2) AS betted_money
	, correct_bet * 10 AS won_money
FROM 
	bet_stats
;


-- Get how the best outcome ratio is performing
WITH prediction AS (
SELECT 
	*
	, 1 AS bet_ratio_treshold_min
	, 2 AS bet_ratio_treshold_max
	, CASE 
		WHEN PredictedOutcome = HomeTeam THEN 'H'
		WHEN PredictedOutcome = AwayTeam THEN 'A'
		ELSE 'D'
	END pred_FTR
from 
	ENG_match_prediction
),
bet_stats AS (

	SELECT 
		SUM(CASE WHEN best_ratio_outcome = FTR THEN 1 ELSE 0 END) AS correct_bet
		, SUM(CASE WHEN best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN 1 ELSE 0 END) AS betted
		, SUM(1) AS total
		, best_ratio_outcome
		, FTR 
		, AVG(best_ratio) AS best_ratio
		, AVG(CASE 
				WHEN pred_FTR = 'H' THEN BF_H_odds
				WHEN pred_FTR = 'A' THEN BF_A_odds
				ELSE BF_D_odds
		END) AS avg_bf_odds
		, AVG(CASE 
				WHEN pred_FTR = 'H' THEN HomeOdd
				WHEN pred_FTR = 'A' THEN AwayOdd
				ELSE DrawOdd
		END) AS pred_odds
		, AVG(CASE 
				WHEN best_ratio_outcome = 'H' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN BF_H_odds
				WHEN best_ratio_outcome = 'A' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN BF_A_odds
				WHEN best_ratio_outcome = 'D' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN BF_D_odds
				ELSE NULL
		END) AS betted_bf_odds
		, AVG(CASE 
				WHEN best_ratio_outcome = 'H' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN HomeOdd
				WHEN best_ratio_outcome = 'A' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN AwayOdd
				WHEN best_ratio_outcome = 'D' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN DrawOdd
				ELSE NULL
		END) AS betted_pred_odds	
		, SUM(10/(CASE 
				WHEN best_ratio_outcome = 'H' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN BF_H_odds
				WHEN best_ratio_outcome = 'A' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN BF_A_odds
				WHEN best_ratio_outcome = 'D' AND best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max THEN BF_D_odds
				ELSE NULL
		END)) AS betted_money 
	
	

	FROM 
	       prediction AS pred
	INNER JOIN 
	       ENG_matches_hist AS hist 
	       ON date(pred.MatchDate)=hist.`Date` 
	       AND pred.HomeTeam = replace(hist.HomeTeam,' ','')
	       AND pred.AwayTeam = replace(hist.AwayTeam,' ','')
	       WHERE best_ratio > bet_ratio_treshold_min AND best_ratio < bet_ratio_treshold_max
	GROUP BY 
		best_ratio_outcome
		, FTR
)
SELECT 
	*
	, correct_bet * 10 AS won_money
FROM 
	bet_stats
ORDER BY 
	best_ratio_outcome
;
