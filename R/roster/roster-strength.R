library(tidyverse)
library(nflreadr)
library(ffscrapr)
library(gt)
library(gtExtras)

current_season <- nflreadr::get_current_season(TRUE)
league_id <- 74675

mfl <- ffscrapr::mfl_connect(current_season, league_id)

franchises <- ffscrapr::ff_franchises(mfl)
starter <- ffscrapr::ff_starters(mfl)
standing <- ffscrapr::ff_standings(mfl)

pf <- starter %>%
  dplyr::filter(starter_status == "starter") %>%
  dplyr::group_by(franchise_id, franchise_name, pos) %>%
  dplyr::summarise(
    pf_sum = sum(player_score, na.rm = TRUE),
    pf_avg = mean(player_score, na.rm = TRUE),
    .groups = "drop"
  )

pp <- starter %>%
  dplyr::filter(should_start == 1) %>%
  dplyr::group_by(franchise_id, pos) %>%
  dplyr::summarise(
    pp_sum = sum(player_score, na.rm = TRUE),
    pp_avg = mean(player_score, na.rm = TRUE),
    .groups = "drop"
  )

pp_league <- pp %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(
    pp_league_sum = sum(pp_sum),
    pp_league_avg = mean(pp_avg)
  )

data <- pf %>%
  dplyr::left_join(
    pp,
    by = c("franchise_id", "pos")
  ) %>%
  dplyr::left_join(
    pp_league,
    by = "pos"
  ) %>%
  dplyr::select(-dplyr::ends_with("_sum")) %>%
  dplyr::mutate(
    pp_diff = round(pp_avg - pp_league_avg, 2)
  ) %>%
  dplyr::left_join(
    standing %>%
      dplyr::select(franchise_id, h2h_wlt, avg_points_for),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::rename(cap_space = bbidAvailableBalance) %>%
      dplyr::select(franchise_id, cap_space),
    by = "franchise_id"
  ) %>%
  dplyr::mutate(
    franchise_info = paste(avg_points_for, h2h_wlt, paste0(cap_space, "$"), sep = " - ")
  ) %>%
  dplyr::select(-franchise_id, -pp_league_avg, -h2h_wlt, -cap_space)

gt_basics <- function(df) {
  df %>%
    gt::gt() %>%
    gt::cols_hide(avg_points_for) %>%
    gt::tab_options(column_labels.hidden = TRUE) %>%
    gtExtras::gt_plt_bullet(column = pf_avg, target = pp_avg, width = 100,
                            palette = c("lightblue", "black"))
}

# by team
data %>%
  dplyr::group_by(franchise_name) %>%
  dplyr::arrange(dplyr::desc(avg_points_for), dplyr::desc(pp_avg)) %>%
  dplyr::mutate(
    pp_diff = ifelse(pp_diff > 0, paste0("+", pp_diff), pp_diff),
    franchise_name = paste0(franchise_name, " (", franchise_info, ")")
  ) %>%
  gt_basics() %>%
  gt::cols_hide(franchise_info) %>%
  gt::gtsave("charts/strength_by_team.png", expand = 10)

# by position
data %>%
  dplyr::group_by(pos) %>%
  dplyr::arrange(pos, dplyr::desc(avg_points_for)) %>%
  dplyr::mutate(
    pp_diff = ifelse(pp_diff > 0, paste0("+", pp_diff), pp_diff)
  ) %>%
  gt_basics() %>%
  gtExtras::gt_merge_stack(col1 = franchise_name, col2 = franchise_info) %>%
  gt::cols_hide(franchise_info) %>%
  gt::gtsave("charts/strength_by_pos.png", expand = 10)
