CREATE TRIGGER lineup_to_mv_update AFTER INSERT ON ENG_R_lineup_input 
BEGIN 
INSERT INTO ENG_match_mv ( 
 match_date,
 hometeam,
 awayteam,
 season,
 hmv1,
 hmv2,
 hmv3,
 hmv4,
 hmv5,
 hmv6,
 hmv7,
 hmv8,
 hmv9,
 hmv10,
 hmv11,
 amv1,
 amv2,
 amv3,
 amv4,
 amv5,
 amv6,
 amv7,
 amv8,
 amv9,
 amv10,
 amv11,
 home_start_mv,
 away_start_mv) 

WITH step1 AS (
	SELECT 
		esl.match_date,
		esl.hometeam,
		esl.awayteam,
		COALESCE(epm1.season,epm8.season,epm15.season,epm22.season) AS season,
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
		ENG_R_lineup_input AS esl
	LEFT JOIN 
		ENG_players_mv AS epm1 ON epm1.name = NEW.h1 AND epm1.season = NEW.season AND epm1.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm2 ON epm2.name = NEW.h2 AND epm2.season = NEW.season AND epm2.team = NEW.hometeam 
	LEFT JOIN 
		ENG_players_mv AS epm3 ON epm3.name = NEW.h3  AND epm3.season = NEW.season AND epm3.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm4 ON epm4.name = NEW.h4  AND epm4.season = NEW.season AND epm4.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm5 ON epm5.name = NEW.h5  AND epm5.season = NEW.season AND epm5.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm6 ON epm6.name = NEW.h6  AND epm6.season = NEW.season AND epm6.team = NEW.hometeam
	LEFT JOIN
		ENG_players_mv AS epm7 ON epm7.name = NEW.h7  AND epm7.season = NEW.season AND epm7.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm8 ON epm8.name = NEW.h8  AND epm8.season = NEW.season AND epm8.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm9 ON epm9.name = NEW.h9  AND epm9.season = NEW.season AND epm9.team = NEW.hometeam
	LEFT JOIN
		ENG_players_mv AS epm10 ON epm10.name = NEW.h10 AND epm10.season = NEW.season AND epm10.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm11 ON epm11.name = NEW.h11 AND epm11.season = NEW.season AND epm11.team = NEW.hometeam
	LEFT JOIN 
		ENG_players_mv AS epm12 ON epm12.name = NEW.a1 AND epm12.season = NEW.season AND epm12.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm13 ON epm13.name = NEW.a2 AND epm13.season = NEW.season AND epm13.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm14 ON epm14.name = NEW.a3 AND epm14.season = NEW.season AND epm14.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm15 ON epm15.name = NEW.a4 AND epm15.season = NEW.season AND epm15.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm16 ON epm16.name = NEW.a5 AND epm16.season = NEW.season AND epm16.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm17 ON epm17.name = NEW.a6 AND epm17.season = NEW.season AND epm17.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm18 ON epm18.name = NEW.a7 AND epm18.season = NEW.season AND epm18.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm19 ON epm19.name = NEW.a8 AND epm19.season = NEW.season AND epm19.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm20 ON epm20.name = NEW.a9 AND epm20.season = NEW.season AND epm20.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm21 ON epm21.name = NEW.a10 AND epm21.season = NEW.season AND epm21.team = NEW.awayteam
	LEFT JOIN 
		ENG_players_mv AS epm22 ON epm22.name = NEW.a11 AND epm22.season = NEW.season AND epm22.team = NEW.awayteam
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
)
SELECT 
	DISTINCT*
FROM 
	with_duplicates
ORDER BY 
	match_date ASC, hometeam ASC
; END