#' Clean up returned list of financial statement data
#'
#' @param list_in List returned from [scrape_financials()].
prep_financials <- function(list_in) {
    df_0 <- as_tibble(t(list_in[[1]]))
    colnames(df_0) <- df_0[1, ]
    df_0 <- df_0[2:nrow(df_0), ]
    df_0$Symbol <- list_in$Symbol

    df_out <- as_tibble(lapply(df_0, function(x) {
        x <- gsub(",", "", x)
        x <- gsub("^-$", "0", x)
        if (suppressWarnings(!is.na(all(as.numeric(x))))) {
            x <- as.numeric(x)
        }
        x
    }))
    df_out$`Period Ending` <- lubridate::mdy(df_out$`Period Ending`)
    df_out <- arrange(df_out, `Period Ending`)

    df_out
}


#' Calculate Free Cash Flows from Financial Data
#'
#' @param df_in Prepped data frame as returned by [prep_financials()].
get_fcf <- function(df_in) {
    df_out <- df_in
    df_out$fcf <- (df_out$`Total Cash Flow From Operating Activities` -
                       df_out$`Total Cash Flows From Investing Activities`)

    df_out
}
