# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(maps)
library(magrittr)
library(Fars)
library(testthat)

# Test that 2013 data equals itself - trivial dummy case but should at least ensure file load without error
# Test that correct error thrown if file does not exist
# In reality one would create expected data frames (or other result format) by hand from first principles and compare against result of function
context("fars tests")
test_that("functions throw expected errors", {
expect_identical(fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "Fars")), fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "Fars")))
expect_error(fars_read("filenotexist.csv.bz2"), "file 'filenotexist.csv.bz2' does not exist")
})