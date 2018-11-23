library(rvest)
library(dplyr)

symbols <- c("AAPL", "GOOG")

df_main <- tibble()
for (sym in symbols) {
    url <- sprintf("https://finance.yahoo.com/quote/%s/cash-flow", sym)

    df_0 <- read_html(url) %>%
        html_nodes(., "table") %>%
        html_table(.) %>%
        as.data.frame(.)
    df_0 <- as_tibble(t(df_0))
    colnames(df_0) <- df_0[1, ]
    df_0 <- df_0[2:nrow(df_0), ]

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
    df_out$Symbol <- sym

    df_main <- bind_rows(df_main, df_out)

}
