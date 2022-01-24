library(data.table)
library(dplyr)
library(janitor)
library(tidyr)
library(zoo)
library(ggplot2)

yf_reader <- function(fund_id) {
  now_epoch <- as.numeric(as.POSIXct(Sys.time()))
  # five_years_ago_epoch <- now_epoch - 5 * 31556926
  five_years_ago_epoch <- now_epoch - 5 * 31553280
  
  read_yahoo <- fread(paste0(
    "https://query1.finance.yahoo.com/v7/finance/download/",
    fund_id,
    "?period1=",
    gsub("\\..*","",five_years_ago_epoch),
    "&period2=",
    gsub("\\..*","",now_epoch),
    "&interval=",
    "1d",
    "&events=history&includeAdjustedClose=true"
  ), sep = ",")
  
  return(read_yahoo)
}

test_yf <- yf_reader("0P0000RU81.L") #FUNDSMITH
test_yf <- yf_reader("0P00000VC9.L")  #BAILLIE GIFFORD

glimpse(test_yf)

test_yf <-
  test_yf %>%
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




ggplot(test_yf) +
  aes(x = date) +
  geom_line(aes(y = close_50_ma), size = 0.9, colour = "green") +
  geom_line(aes(y = close_200_ma), size = 0.9, colour = "yellow") +
  geom_point(aes(y = action_sell), size = 2.5, colour = "red") +
  geom_point(aes(y = action_buy), size = 2.5, colour = "blue") +
  ggthemes::theme_fivethirtyeight()
