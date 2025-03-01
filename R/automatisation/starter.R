library(nflreadr)
library(tidyverse)
library(piggyback)

cli::cli_alert_info("Create Data")
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()
last_week <- current_week - 1

mfl <- ffscrapr::mfl_connect(current_season, league_id, rate_limit = FALSE)
starter <- ffscrapr::ff_starters(mfl_last_season, week = last_week:current_week)

if (current_week == 1) {
  cli::cli_alert_info("Write Data")
  readr::write_csv(starter, paste0("gdl_starter_", current_season, ".csv"))

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_starter_", current_season, ".csv"), "bohndesverband/gdl", "starter_data", overwrite = TRUE)
} else if (current_week <= 17) {
  old_data <- readr::read_csv(paste0("https://github.com/bohndesverband/gdl/releases/download/starter_data/gdl_starter_", current_season, ".csv"), col_types = "ccdicidccc") %>%
    dplyr::filter(week < last_week)

  cli::cli_alert_info("Write Data")
  readr::write_csv(rbind(old_data, starter), paste0("gdl_starter_", current_season, ".csv"))

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_starter_", current_season, ".csv"), "bohndesverband/gdl", "starter_data", overwrite = TRUE)
} else {
  cli::cli_alert_info("Season Ended")
}

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/gdl", "starter_data", overwrite = TRUE)
