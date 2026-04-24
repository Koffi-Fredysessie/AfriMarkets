# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(rvest)
library(purrr)
library(stringr)
library(readr)

library(testthat)
library(AfriMarkets)


test_check("AfriMarkets")

GET_tickers("BRVM")
GET_tickers("BVC")
GET_tickers("GSE")
GET_tickers("NGX")
