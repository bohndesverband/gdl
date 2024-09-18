create_pos_rank <- function(df) {
  df %>%
    dplyr::group_by(pos) %>%
    dplyr::arrange(dplyr::desc(points)) %>%
    dplyr::mutate(
      pos_rank = dplyr::row_number(),
      pos_finish = dplyr::case_when(
        pos_rank <= 5 ~ "top_5",
        pos_rank <= 8 ~ "top_8",
        pos_rank <= 16 ~ "top_16",
        pos_rank <= 32 ~ "top_32",
        pos_rank <= 48 ~ "top_48",
        pos_rank <= 64 ~ "top_64",
        TRUE ~ "below_64"
      )
    ) %>%
    dplyr::ungroup(pos)
}

pos_ranks_avg <- scores_avg %>%
  create_pos_rank() %>%
  dplyr::rename(pos_rank_avg = pos_rank)

pos_ranks_ytd <- scores_ytd %>%
  create_pos_rank() %>%
  dplyr::mutate(
    is_top_pos_player = dplyr::case_when(
      (pos %in% c("QB", "TE", "DT") & pos_rank <= 10) |
        (pos %in% c("RB", "DE", "S", "CB") & pos_rank <= 15) |
        (pos %in% c("WR", "LB") & pos_rank <= 20) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::rename(pos_rank_ytd = pos_rank)
