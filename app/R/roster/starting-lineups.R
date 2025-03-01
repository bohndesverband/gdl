starting_lineups <- starter %>%
  dplyr::filter(starter_status == "starter") %>%
  dplyr::group_by(franchise_id, player_id) %>%
  dplyr::summarise(
    starts = n(),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    rosters %>%
      dplyr::select(player_id, player_name, pos, team, contract_years, salary),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    pos_ranks_ytd %>%
      dplyr::select(player_id, points, pos_rank_ytd),
    by = "player_id"
  ) %>%
  dplyr::filter(starts == max(starts)) %>%
  dplyr::ungroup()
