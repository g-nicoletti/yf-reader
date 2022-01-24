library(data.table)
library(dplyr)
library(janitor)
library(tidyr)
library(zoo)
library(ggplot2)

yf_reader <- function(fund_id) {
  # get current timestamp (epoch)
  now_epoch <- as.numeric(as.POSIXct(Sys.time()))

  # get same timestamp, 5 years ago
  # five_years_ago_epoch <- now_epoch - 5 * 31556926
  five_years_ago_epoch <- now_epoch - 5 * 31553280

  # read file from yahoo finance
  read_yahoo <- fread(paste0(
    "https://query1.finance.yahoo.com/v7/finance/download/",
    fund_id,
    "?period1=",
    gsub("\\..*", "", five_years_ago_epoch),
    "&period2=",
    gsub("\\..*", "", now_epoch),
    "&interval=",
    "1d",
    "&events=history&includeAdjustedClose=true"
  ), sep = ",")

  return(read_yahoo)
}
