context("Getter funcs retrieve data as expected")

test_that("Yahoo! Finance Financial Statements", {
    list_0 <- scrape_financials("AAPL")

    expect_equal(class(list_0), "list")
    expect_length(list_0, 1)
    expect_equal(class(list_0[[1]]), "data.frame")
    expect_equal(list_0[[1]][1, 1], "Period Ending")
})
