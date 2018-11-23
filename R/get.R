#' Scrape Financial Statements
#'
#' @param sym The stock ticker symbol of interest, passed as a `string`.
scrape_financials <- function(sym) {
    url_cf <- sprintf("%s/%s/cash-flow", .config_env$stmts_url_yahoo, sym)
    list_out <- read_html(url_cf) %>%
        html_nodes(., "table") %>%
        html_table(.)
    list_out
}
