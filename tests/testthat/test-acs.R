library(testthat)
context("test the vaious _acs functions")

test_that('get_acs_data returns a list with data and a map title', {
  expect_is(get_acs_data(variable = 'B19013_001', map = 'county', endyear = 2012, span = 5), 'list')
})

test_that('User can not specify both variable and tableId', {
  expect_error(get_acs_data(variable = 'B19013_001', tableId = 'B00001', map = 'county', endyear = 2012, span = 5))
})

test_that('Table B01001 has more than one variable, so an error should be thrown, unless column_idx is given.', {
  expect_error(get_acs_data(tableId = 'B01001', map = 'county', endyear = 2012, span = 5))
  expect_is(get_acs_data(tableId = 'B01001', column_idx = 1, map = 'county', endyear = 2012, span = 5), 'list')
})

test_that('...Unless a column Idx is specified', {
  expect_error(get_acs_data(tableId = 'B01001', map = 'county', endyear = 2012, span = 5))
})


test_that("state_choropleth_acs returns a ggplot2 with parameters set", {
  expect_is(state_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, zoom=c("new york", "new jersey", "connecticut")), "ggplot") 
})

test_that("county_choropleth_acs returns a ggplot2 with parameters set", {
  expect_is(suppressWarnings(
    county_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, state_zoom=c("new york", "new jersey", "connecticut"))), 
    "ggplot")
})

