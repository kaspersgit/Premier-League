CREATE TRIGGER clean_player_mv 
AFTER INSERT ON ENG_players_mv
BEGIN INSERT INTO ENG_players_mv_clean
 	(eng_pmv_id
 	, season
 	, team 
 	, name 
 	, MV 
 	, clean_name)
 VALUES 
 	(NEW.eng_pmv_id
 	, NEW.season
 	, NEW.team 
 	, NEW.name 
 	, NEW.MV 
 	, replace(replace(replace(replace(replace(replace(replace(replace(
	replace(replace(replace( lower(NEW.name), 'á','a'), 'ã','a'), 'â','a'), 'é','e'), 'ê','e'), 'í','i'),
	'ó','o') ,'õ','o') ,'ô','o'),'ú','u'), 'ç','c'))
; END
