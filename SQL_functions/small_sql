-- Adding season column to line up table

CREATE TABLE ENG_lineup_hist AS
SELECT 
	*,
	CASE 
		WHEN(CAST(strftime('%W',match_date) AS integer) BETWEEN 0 AND 26)
			THEN (CAST(strftime('%Y',match_date) AS integer) -1)||strftime('%Y',match_date)
		ELSE strftime('%Y',match_date)||(CAST(strftime('%Y',match_date) AS integer) + 1)
	END AS season
FROM ENG_lineups_hist
