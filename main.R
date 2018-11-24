library(dplyr)

sym <- "AAPL"

list_0 <- scrape_financials(sym)
df_0 <- prep_financials(list_0)
