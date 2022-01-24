plot_yf <- function(fund_yf) {
  plot_this <- ggplot(fund_yf) +
    aes(x = date) +
    geom_line(aes(y = close_50_ma), size = 0.9, colour = "green") +
    geom_line(aes(y = close_200_ma), size = 0.9, colour = "yellow") +
    geom_line(aes(y = close), size = 0.5, colour = "grey") +
    geom_point(aes(y = action_sell), size = 2.5, colour = "red") +
    geom_point(aes(y = action_buy), size = 2.5, colour = "blue") +
    ggthemes::theme_fivethirtyeight()
}
