library(fcf)
library(dplyr)

syms <- c("AAPL", "MSFT", "GOOG", "AMZN")

for (sym in syms) {
    give_recommendation(sym)
}
