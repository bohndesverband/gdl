connection <- ffscrapr::ff_connect(platform = "mfl", season = 2023, league_id = 74675)

# load player fpts data ----
league_players <- ffscrapr::mfl_getendpoint(connection, "players")$content$players$player %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1)

player_fpts_2019 <- jsonlite::read_json("https://www45.myfantasyleague.com/2019/export?TYPE=playerScores&L=74675&W=AVG&YEAR=2019&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1") %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(avg2019 = score)

player_fpts_2020 <- jsonlite::read_json("https://www45.myfantasyleague.com/2020/export?TYPE=playerScores&L=74675&W=AVG&YEAR=2020&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1") %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(avg2020 = score)

player_fpts_2021 <- jsonlite::read_json("https://www45.myfantasyleague.com/2021/export?TYPE=playerScores&L=74675&W=AVG&YEAR=2021&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1") %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(avg2021 = score)

player_fpts_2022 <- jsonlite::read_json("https://www45.myfantasyleague.com/2022/export?TYPE=playerScores&L=74675&W=AVG&YEAR=2022&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1") %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(avg2022 = score)

player_fpts_2023 <- jsonlite::read_json("https://www45.myfantasyleague.com/2023/export?TYPE=playerScores&L=74675&W=AVG&YEAR=2022&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1") %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(avg2023 = score)

## combine data ----
all_player_fpts <- league_players %>%
  dplyr::left_join(player_fpts_2019 %>% select(avg2019, id), by = "id") %>%
  dplyr::left_join(player_fpts_2020 %>% select(avg2020, id), by = "id") %>%
  dplyr::left_join(player_fpts_2021 %>% select(avg2021, id), by = "id") %>%
  dplyr::left_join(player_fpts_2022 %>% select(avg2022, id), by = "id") %>%
  dplyr::left_join(player_fpts_2023 %>% select(avg2023, id), by = "id") %>%
  dplyr::select(-status) %>%
  dplyr::filter(!grepl("TM|Def|ST|Off|Coach|XX", position))

## create ranks ----
fpts_ranks <- all_player_fpts %>%
  dplyr::mutate(
    dplyr::across(starts_with("avg"), as.numeric)
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    avg = mean(c(avg2019, avg2020, avg2021, avg2022, avg2022, avg2023), na.rm = TRUE),
    avg = ifelse(position == "QB", avg * 0.75, avg)
  ) %>%
  dplyr::group_by(position) %>%
  dplyr::arrange(desc(avg)) %>%
  dplyr::mutate(
    posRank = dplyr::row_number(),
    posFptsPerc = avg / avg[1]
    ) %>%
  dplyr::select(-avg2019, -avg2020, -avg2021, -avg2022, -avg2023)
