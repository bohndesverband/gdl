library(tidyverse)

# run ../positional-value/documentation.Rmd

auction_budget <- 500

player_values <- fpts_ranks %>%
  # führe WR und TE zusammen
  dplyr::mutate(position = ifelse(position %in% c("WR", "TE"), "WR+TE", position)) %>%
  dplyr::group_by(position) %>%
  dplyr::arrange(desc(avg)) %>%
  dplyr::mutate(
    posRank = dplyr::row_number()
  ) %>%
  dplyr::left_join(rostered %>% select(-Limit), by = c("position" = "Position")) %>%
  dplyr::left_join(rostered_avg %>% select(-Starter), by = "position") %>%
  dplyr::rename(starterAvg = startedAvg, posPoints = avgStarterPoints) %>%

  # filter data
  dplyr::left_join(all_player_fpts %>% dplyr::select(id, avg2021, avg2022), by = "id") %>%
  dplyr::filter(
    !is.na(avg2021) | !is.na(avg2022)
  ) %>%

  # create vbd data
  dplyr::group_by(position) %>%
  dplyr::arrange(posRank) %>%
  dplyr::mutate(
    Rostered = as.numeric(Rostered),
    vorp = avg - avg[Started + 1], # Value Over Replacement Player
    volr = avg - avg[Rostered], # Value Over Last Rostered
    totalPosBudget = auction_budget * ((posValuePerStarter / 100) * Starter), # positional budget berechnet sich aus der anzahl der starter und dem positional value; +1 weil die berechneten werte nicht 50 ergeben
    Roster = dplyr::case_when(
      # optimiere roster
      position %in% c("PK", "PN") ~ 1,
      TRUE ~ Roster
    ),
    basePosPlayerBudget = totalPosBudget / Roster,
    price = round(basePosPlayerBudget + vorp, 2),
    price = ifelse(price <= 1, 1, price)
  ) %>%
  dplyr::select(id, name, position, team, posRank, avg, vorp, volr, price)

write.csv(player_values, "data/player-prices.csv", row.names = FALSE)
