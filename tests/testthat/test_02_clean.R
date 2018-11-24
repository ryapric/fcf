context("Cleaner funcs")

list_0 <- scrape_financials("AAPL")
df_0 <- prep_financials(list_0)
fcf_0 <- get_fcf(df_0)

test_that("Convert to data.frame and clean up", {
    expect_true("data.frame" %in% class(df_0))
    expect_equal(nrow(df_0), 4)
})

test_that("Get FCFs", {
    expect_true("data.frame" %in% class(df_0))
    expect_equal(nrow(fcf_0), 4)
})
