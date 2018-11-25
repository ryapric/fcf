estimate_g <- function(sym) {
    list_0 <- scrape_financials(sym)
    df_0 <- prep_financials(list_0)

    shares <- scrape_shares_outstanding(sym)[[1]]
    fcf_0 <- tail(df_0$fcf, 1)
    rr <- get_rr(sym)
    price_0 <- (as.numeric(tail(quantmod::getSymbols(sym, auto.assign = FALSE)[, 6], 1)) * shares)

    est_g <- (((price_0 * rr) - fcf_0) / (price_0 + fcf_0))

    est_g
}


estimate_price <- function(sym) {
    list_0 <- scrape_financials(sym)
    df_0 <- prep_financials(list_0)

    shares <- scrape_shares_outstanding(sym)[[1]]
    fcf_g <- (((1 + prod(diff(log(df_0$fcf)))) ^ (nrow(df_0) - 1)) - 1)
    fcf_0 <- tail(df_0$fcf, 1)
    rr <- get_rr(sym)

    price_0 <- (((fcf_0 * (1 + fcf_g)) / (rr - fcf_g)) / shares)

    list(fair_price = price_0, mkt_price = (as.numeric(tail(quantmod::getSymbols(sym, auto.assign = FALSE)[, 6], 1))))
}


#' @export
give_recommendation <- function(sym) {
    list_0 <- scrape_financials(sym)
    df_0 <- prep_financials(list_0)

    prices <- estimate_price(sym)
    est_g <- estimate_g(sym)
    actual_g <- (((1 + prod(diff(log(df_0$fcf)))) ^ (nrow(df_0) - 1)) - 1)

    rec <- ifelse(prices$fair_price > prices$mkt_price, "BUY", "SELL / SHORT")

    print(glue::glue("\n{sym}:\n",
               "Expected FCF growth based on current market price of ${round(prices$mkt_price, 2)}: {round(est_g, 4)}\n",
               "Actual recent FCF growth: {round(actual_g, 4)}\n",
               "Fair market price based on actual FCF growth: ${round(prices$fair_price, 2)}\n",
               "Recommendation for {sym}: {rec}\n"))
}
