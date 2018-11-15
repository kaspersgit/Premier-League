--  select 380 * 22 = 8360 players --> 804 are unmatched
-- removing accents gives: 587 are unmatched
-- putting names in lower case: 446 are unmatched
WITH step_1 AS (
SELECT 
	sum(case when hmv1 = 1000 then 1 else 0 end) as h1
	, sum(case when hmv2 = 1000 then 1 else 0 end) as h2
	, sum(case when hmv3 = 1000 then 1 else 0 end) as h3
	, sum(case when hmv4 = 1000 then 1 else 0 end) as h4
	, sum(case when hmv5 = 1000 then 1 else 0 end) as h5
	, sum(case when hmv6 = 1000 then 1 else 0 end) as h6
	, sum(case when hmv7 = 1000 then 1 else 0 end) as h7
	, sum(case when hmv8 = 1000 then 1 else 0 end) as h8
	, sum(case when hmv9 = 1000 then 1 else 0 end) as h9
	, sum(case when hmv10 = 1000 then 1 else 0 end) as h10
	, sum(case when hmv11 = 1000 then 1 else 0 end) as h11
	, sum(case when amv1 = 1000 then 1 else 0 end) as a1
	, sum(case when amv2 = 1000 then 1 else 0 end) as a2
	, sum(case when amv3 = 1000 then 1 else 0 end) as a3
	, sum(case when amv4 = 1000 then 1 else 0 end) as a4
	, sum(case when amv5 = 1000 then 1 else 0 end) as a5
	, sum(case when amv6 = 1000 then 1 else 0 end) as a6
	, sum(case when amv7 = 1000 then 1 else 0 end) as a7
	, sum(case when amv8 = 1000 then 1 else 0 end) as a8
	, sum(case when amv9 = 1000 then 1 else 0 end) as a9
	, sum(case when amv10 = 1000 then 1 else 0 end) as a10
	, sum(case when amv11 = 1000 then 1 else 0 end) as a11
FROM 
	ENG_match_mv
WHERE 
	season = 2016017
)

SELECT
	h1+h2+h3+h4+h5+h6+h7+h8+h9+h10+h11+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11
FROM 
	step_1
;


SELECT *
FROM ENG_players_mv_cleaning
WHERE team like '%wolves%'
AND season = 20182019;

SELECT *
FROM ENG_match_lineup_cleaning
WHERE hometeam like '%wolves%'
AND season = 20182019;

SELECT *
FROM ENG_match_mv
WHERE hometeam like '%wolves%'
AND season = 20182019;