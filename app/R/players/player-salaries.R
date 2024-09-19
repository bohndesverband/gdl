# pos averages
top_x_salary_values <- pos_ranks_ytd %>%
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

# tags
tag_salaries <- rosters %>%
  dplyr::select(pos, salary) %>%
  dplyr::group_by(pos) %>%
  dplyr::arrange(dplyr::desc(salary)) %>%
  dplyr::mutate(
    tender = dplyr::case_when(
      dplyr::row_number() <= 5 ~ "gft",
      dplyr::row_number() == 10 ~ "tender_1",
      dplyr::row_number() == 20 ~ "tender_2",
      dplyr::row_number() == 30 ~ "tender_3"
    ),
    top_player_salary = dplyr::case_when(
      (pos %in% c("QB", "TE", "DT") & dplyr::row_number() <= 10) |
        (pos %in% c("RB", "DE", "S", "CB") & dplyr::row_number() <= 15) |
        (pos %in% c("WR", "LB") & dplyr::row_number() <= 20) ~ salary
    )
  ) %>%
  dplyr::mutate(top_player_salary = round(mean(top_player_salary, na.rm = TRUE), 2)) %>%
  dplyr::filter(!is.na(tender)) %>%
  dplyr::group_by(pos, tender, top_player_salary) %>%
  dplyr::summarise(salary = round(mean(salary), 2), .groups = "drop") %>%
  tidyr::spread(tender, salary)

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
  dplyr::left_join(
    pos_ranks_avg %>%
      dplyr::select(player_id, points, pos_rank_avg),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    pos_ranks_ytd %>%
      dplyr::select(player_id, points, pos_rank_ytd, is_top_pos_player) %>%
      dplyr::rename(points_ytd = points),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    top_x_salary_values,
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
    contract_value = (points / salary) * points_ytd,
    contract_value_pct_ovrl = contract_value / max(contract_value, na.rm = TRUE)
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::mutate(
    contract_value_pct_pos = contract_value / max(contract_value, na.rm = TRUE),

    # remove NA
    across(c(points_ytd, points), ~ ifelse(is.na(.), 0, round(., 2))),
    #across(c(pos_rank_ytd, pos_rank_avg), ~ ifelse(is.na(.), max(.) + 1, .)),
    across(c(contract_value_pct_ovrl, contract_value_pct_pos), ~ ifelse(is.na(.), 0, .))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(franchise_name, player_id, player_name, pos, team, age, points, pos_rank_avg, contract_years, salary, contract_value_pct_ovrl, contract_value_pct_pos, roster_status, franchise_id, points_ytd, pos_rank_ytd, is_top_pos_player, drafted)

# outlook
salary_outlook <- roster_with_salary %>%
  dplyr::left_join(
    tag_salaries,
    by = "pos"
  ) %>%
  dplyr::mutate(
    category = ifelse(contract_years > 1 | roster_status == "TAXI_SQUAD" | grepl("BB", drafted) | drafted == "FCFS", "Mögliche Vertragsverlängerungen", "Auslaufende Verträge"),
    fa_class = nflreadr::get_current_season() + contract_years,
    holdout = ifelse((contract_years > 1 | roster_status == "TAXI_SQUAD") & is_top_pos_player == 1 & salary < (top_player_salary / 2), 1, 0),
    holdout = ifelse(is.na(holdout), "", holdout),
    salary_adding = salary * 0.1,
    ext_adding = ifelse(salary_adding < 5, 5, salary_adding),
    salary_next_season = dplyr::case_when(
      contract_years > 1 & holdout == 1 ~ round(top_player_salary / 2, 2),
      contract_years > 1 & holdout != 1 ~ round(salary + salary_adding, 2),
      contract_years <= 1 ~ 0
    ),
    salary_ext = dplyr::case_when(
      (contract_years > 1 | roster_status == "TAXI_SQUAD" | grepl("BB", drafted) | drafted == "FCFS") & holdout == 1 ~ round(top_player_salary / 2, 2),
      (contract_years > 1 | roster_status == "TAXI_SQUAD" | grepl("BB", drafted) | drafted == "FCFS") & holdout != 1 ~ round(salary + ext_adding, 2),
      (contract_years <= 1 | roster_status != "TAXI_SQUAD") ~ 0
    ),
    cut = round(salary * 0.25 * contract_years, 2),
    gft = ifelse(gft > salary * 1.2, gft, round(salary * 1.2, 2)),
    dplyr::across(dplyr::starts_with("tender"), ~ ifelse(. > salary * 1.1, ., round(salary * 1.1, 2)))
  ) %>%
  dplyr::group_by(pos) %>%
  f_create_ranks(desc(salary), "salary_rank") %>%
  f_create_ranks(desc(salary_next_season), "salary_next_season_rank") %>%
  f_create_ranks(desc(salary_ext), "salary_ext_rank") %>%
  f_create_ranks(desc(cut), "cut_rank") %>%
  dplyr::ungroup() %>%
  #filter(franchise_id == "0006") %>%
  #filter(player_name == "Woods, Jelani") %>%
  dplyr::select(franchise_id, franchise_name, player_name, pos, team, category, points, pos_rank_ytd, contract_years, salary, holdout, salary_next_season, salary_ext, cut, gft, dplyr::ends_with("_rank"), dplyr::starts_with("tender"), fa_class, roster_status)
