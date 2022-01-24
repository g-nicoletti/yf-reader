run_yf <- function(fund_id) {
  tbl_yf <- yf_reader(fund_id)
  tbl_yf <- clean_yf(tbl_yf)

  plot_me <- plot_yf(tbl_yf)
  
  return(plot_me)
}
