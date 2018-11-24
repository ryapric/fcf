context("Getter funcs")

sym <- "AAPL"

test_that("Financial Statements", {
    list_0 <- scrape_financials(sym)

    expect_equal(class(list_0), "list")
    expect_length(list_0, 2)
    expect_equal(class(list_0[[1]]), "data.frame")
    expect_equal(list_0[[1]][1, 1], "Period Ending")
    expect_equal(list_0[[2]], sym)
})

test_that("Shares Outstanding", {
    list_0 <- scrape_shares_outstanding("AAPL")

    expect_equal(class(list_0), "list")
    expect_length(list_0, 2)
    expect_equal(class(list_0[[1]]), "numeric")
    expect_equal(list_0[[2]], sym)
})
