# variables ----
current_season <- nflreadr::get_current_season(TRUE) + 1
current_year <- as.numeric(format(Sys.Date(), "%Y"))
current_week <- nflreadr::get_current_week()
league_id <- 74675
positions <- c("QB", "RB", "WR", "TE", "PK", "DT", "DE", "LB", "CB", "S")

# data ----
mfl <- ffscrapr::mfl_connect(current_season, league_id)
mfl_last_season <- ffscrapr::mfl_connect(current_season - 1, league_id)

franchises <- ffscrapr::ff_franchises(mfl)
rosters <- ffscrapr::ff_rosters(mfl)

if ((current_year == current_season & current_week > 17) | current_week < 2) {
  standing <- ffscrapr::ff_standings(mfl_last_season)
  starter <- ffscrapr::ff_starters(mfl_last_season)
  scores_avg <- ffscrapr::ff_playerscores(mfl_last_season, current_season - 1, "AVG")
  scores_ytd <- ffscrapr::ff_playerscores(mfl_last_season, current_season - 1, "YTD")
} else {
  standing <- ffscrapr::ff_standings(mfl)
  starter <- ffscrapr::ff_starters(mfl)
  scores_avg <- ffscrapr::f(mfl, current_season, "AVG")
  scores_ytd <- ffscrapr::ff_playerscores(mfl, current_season, "YTD")
}


# helper ----
f_create_ranks <- function(df, arrange_by, rank_col_name) {
  df %>%
    dplyr::arrange({{arrange_by}}) %>%
    dplyr::mutate({{rank_col_name}} := paste0("#", row_number()))
}

# filter
f_filter_by_pos <- function(df) {
  df %>%
    dplyr::filter(
      if(shiny::isTruthy(input$selectPositions))
        pos %in% input$selectPositions
      else
        TRUE
    )
}
