CREATE TRIGGER match_odds_update 
AFTER INSERT ON '20182019' 
BEGIN INSERT INTO ENG_match_odds (
	`Div`
	, `Date`
	, `HomeTeam`
	, `AwayTeam`
	, `IWH`
	, `IWD`
	, `IWA`
	, `LBH`
	, `LBD`
	, `LBA`
	, `WHH`
	, `WHD`
	, `WHA`) VALUES (NEW.`Div`
	, NEW.`Date`
	, NEW.`HomeTeam`
	, NEW.`AwayTeam`
	, NEW.`IWH`
	, NEW.`IWD`
	, NEW.`IWA`
	, NEW.`LBH`
	, NEW.`LBD`
	, NEW.`LBA`
	, NEW.`WHH`
	, NEW.`WHD`
	, NEW.`WHA`); END