CREATE TRIGGER clean_match_prediction AFTER INSERT ON ENG_match_prediction 
BEGIN 
DELETE
FROM
 ENG_match_prediction
WHERE
  'Date' IS NULL
 OR HomeTeam IS NULL
 OR AwayTeam IS NULL
 OR PredictedOutcome IS NULL
 OR PredictedOutcome = 'NA'
; END