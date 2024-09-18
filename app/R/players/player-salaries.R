# pos averages
ranking <- pos_ranks_avg %>%
  dplyr::left_join(
    rosters %>%
      dplyr::select(player_id, salary),
    by = "player_id"
  ) %>%
  dplyr::select(-season:-player_name, -team, -is_available) %>%
  dplyr::group_by(pos, pos_finish) %>%
  dplyr::summarise(
    top_x_avg_salary = round(mean(salary, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  tidyr::spread(pos_finish, top_x_avg_salary) %>%
  dplyr::select(pos, top_5, top_8, top_16, top_32, top_48, top_64, below_64)

top_player_salary_avg <- pos_ranks_ytd %>%
  dplyr::filter(is_top_pos_player == 1) %>%
  dplyr::left_join(
    rosters %>%
      dplyr::select(player_id, salary),
    by = "player_id"
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(top_player_salary = round(mean(salary, na.rm = TRUE), 2))

# rosters
roster_with_salary <- rosters %>%
  dplyr::select(-dplyr::starts_with("draft")) %>%
  dplyr::left_join(
    pos_ranks_avg %>%
      dplyr::select(player_id, points, pos_rank_avg),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    ranking,
    by = "pos"
  ) %>%
  dplyr::mutate(
    closest_avg_diff = dplyr::case_when(
      pos_rank_avg <= 6 ~ salary - top_5,
      pos_rank_avg <= 12 ~ salary - top_8,
      pos_rank_avg <= 24 ~ salary - top_16,
      pos_rank_avg <= 40 ~ salary - top_32,
      pos_rank_avg <= 56 ~ salary - top_48,
      pos_rank_avg <= 72 ~ salary - top_64,
      TRUE ~ salary - below_64
    ),
    points = round(points, 2)
  ) %>%
  dplyr::select(franchise_name, player_id, player_name, pos, team, age, points, pos_rank_avg, contract_years, salary, closest_avg_diff, roster_status, franchise_id)

# outlook
salary_outlook <- roster_with_salary %>%
  dplyr::select(franchise_id, franchise_name, player_id, player_name, pos, team, points, contract_years, salary) %>%
  dplyr::left_join(
    pos_ranks_ytd %>%
      dplyr::select(player_id, pos_rank_ytd, is_top_pos_player),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    top_player_salary_avg,
    by = "pos"
  ) %>%
  dplyr::mutate(
    fa_class = nflreadr::get_current_season() + contract_years,
    holdout = ifelse(contract_years > 1 & is_top_pos_player == 1 & salary < (top_player_salary / 2), 1, 0),
    salary_adding = salary * 0.1,
    ext_adding = ifelse(salary_adding < 5, 5, salary_adding),
    salary_next_season = dplyr::case_when(
      contract_years > 1 & holdout == 1 ~ round(top_player_salary / 2, 2),
      contract_years > 1 & (holdout == 0 | is.na(holdout)) ~ round(salary + salary_adding, 2),
      contract_years <= 1 ~ 0
    ),
    salary_ext = ifelse(contract_years > 1, round(salary + ext_adding, 2), 0),
    cut = round(salary * 0.25 * contract_years, 2),
    category = ifelse(contract_years <= 1, "FA", "EXT")
  ) %>%
  dplyr::select(-dplyr::ends_with("_adding"))
