#' Scrape Financial Statements
#'
#' @param sym The stock ticker symbol of interest, passed as a `string`.
scrape_financials <- function(sym) {
    url_cf <- sprintf("%s/%s/cash-flow", .config_env$url_yahoo, sym)
    list_out <- read_html(url_cf) %>%
        html_nodes(., "table") %>%
        html_table(.)
    list_out$Symbol <- sym

    list_out
}


parse_shares <- function(vec) {
    vec <- gsub("^<.*>(.*)<.*>$", "\\1", vec)
    alpha <- gsub(".*([[:alpha:]])", "\\1", vec)
    vec <- as.numeric(gsub("(.*)[[:alpha:]]", "\\1", vec))
    vec <- ifelse(alpha == "B",
                  vec * 1e9,
                  ifelse(alpha == "M",
                         vec * 1e6,
                         ifelse(alpha == "K",
                                vec * 1000,
                                NULL)))

    vec
}


#' Get latest outstanding share counts
#'
#' @inheritParams scrape_financials
scrape_shares_outstanding <- function(sym) {
    url_shares <- sprintf("%s/%s/key-statistics", .config_env$url_yahoo, sym)
    list_out <- read_html(url_shares) %>%
        html_nodes(., "div.Pstart\\(20px\\) > div:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2)")
    # Not a formal list, so make it as one (coercion doesn't work)
    list_out <- lapply(list_out, function(x) {x})
    list_out[[1]] <- parse_shares(list_out[[1]])
    list_out$Symbol <- sym

    list_out
}


#' Get historical price data
#'
#' @inheritParams scrape_financials
get_prices <- function(sym) {
    df_sym <- quantmod::annualReturn(quantmod::getSymbols(sym, from = Sys.Date() - lubridate::years(5), auto.assign = FALSE))
    colnames(df_sym)[1] <- "rs"

    df_mkt <- quantmod::annualReturn(quantmod::getSymbols("^GSPC", from = Sys.Date() - lubridate::years(5), auto.assign = FALSE))
    colnames(df_mkt)[1] <- "rm"

    df_rf <- quantmod::getSymbols("DGS30", src = "FRED", auto.assign = FALSE)
    colnames(df_rf)[1] <- "rf"
    df_rf$rf <- (df_rf$rf / 100)

    df_out <- as.data.frame(merge(df_sym, df_mkt, df_rf))

    df_out$date <- rownames(df_out)
    df_out <- filter_all(df_out, all_vars(!is.na(.)))
    df_out$rm_rf <- (df_out$rm - df_out$rf)

    df_out
}


get_beta <- function(sym) {
    df_0 <- get_prices(sym)

    fit <- lm(rs ~ rm_rf, data = df_0)
    beta_0 <- fit$coefficients["rm_rf"]

    list(beta = beta_0, prices = df_0)
}


get_rr <- function(sym) {
    list_0 <- get_beta(sym)
    beta_0 <- list_0$beta
    rm_rf <- tail(list_0$prices$rm_rf, 1)
    rf <- tail(list_0$prices$rf, 1)

    rr <- (rf + (beta_0 * rm_rf))

    rr
}
