# variables ----
current_season <- nflreadr::get_current_season(TRUE)
league_id <- 74675
positions <- c("QB", "RB", "WR", "TE", "PK", "DT", "DE", "LB", "CB", "S")

# data ----
mfl <- ffscrapr::mfl_connect(current_season, league_id)

franchises <- ffscrapr::ff_franchises(mfl)
standing <- ffscrapr::ff_standings(mfl)
rosters <- ffscrapr::ff_rosters(mfl)
starter <- ffscrapr::ff_starters(mfl)
scores_avg <- ffscrapr::ff_playerscores(mfl, current_season, "AVG")
scores_ytd <- ffscrapr::ff_playerscores(mfl, current_season, "YTD")

# helper ----

