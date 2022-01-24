clean_yf <- function(data) {
  data <-
    data %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    janitor::clean_names() %>%
    na.omit() %>%
    dplyr::select(date, close) %>%
    dplyr::mutate(
      close_50_ma = zoo::rollmean(close, k = 50, fill = NA, align = "right"),
      close_200_ma = zoo::rollmean(close, k = 200, fill = NA, align = "right"),
      action_today = ifelse(close_50_ma > close_200_ma, "buy", "sell"),
      action_yesterday = lag(action_today)
    ) %>%
    na.omit() %>%
    dplyr::mutate(
      action_sell = case_when(action_today == "sell" & action_yesterday == "buy" ~ close_50_ma, TRUE ~ NA_real_),
      action_buy = case_when(action_today == "buy" & action_yesterday == "sell" ~ close_50_ma, TRUE ~ NA_real_)
    ) %>%
    as_tibble()

  return(data)
}
