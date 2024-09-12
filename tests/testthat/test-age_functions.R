test_that("Current Age", {
    expect_equal(calculate_age("2000-01-01", "2001-01-01"), 1)
    expect_equal(calculate_age("2000-01-01", as.Date("2001-01-01")), 1)
    expect_equal(calculate_age("2000-01-01", "2002-01-01"), 2)
})

test_that("days_until_next_birthday", {
    expect_equal(days_until_next_birthday("2000-01-01", "2000-12-31"), 1)
    expect_equal(days_until_next_birthday("2000-01-01", as.Date("2000-12-31")), 1)
    expect_equal(days_until_next_birthday("2000-01-01", as.Date("2001-01-01")), 0)
    expect_equal(days_until_next_birthday("2000-01-01", as.Date("2001-01-02")), 364)
})

test_that("days_since_last_birthday", {
    expect_equal(days_since_last_birthday("2000-01-01", "2000-12-31"), 365)
    expect_equal(days_since_last_birthday("2000-01-01", as.Date("2000-12-31")), 365)
    expect_equal(days_since_last_birthday("2000-01-01", as.Date("2001-01-01")), 0)
    expect_equal(days_since_last_birthday("2000-01-01", as.Date("2001-01-02")), 1)
})

test_that("calculate_age_nearest_birthday", {
    expect_equal(calculate_age_nearest_birthday("2000-01-01", "2000-12-31"), 1)
    expect_equal(calculate_age_nearest_birthday("2000-01-01", as.Date("2000-12-31")), 1)
    expect_equal(calculate_age_nearest_birthday("2000-01-01", "2001-01-31"), 1)
})
