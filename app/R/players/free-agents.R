free_agents <- salary_outlook %>%
  dplyr::group_by(fa_class) %>%
  dplyr::arrange(dplyr::desc(points)) %>%
  dplyr::select(player_name, pos, team, fa_class, franchise_name)
