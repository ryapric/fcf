#!/usr/bin/env Rscript

library(fcf)
library(dplyr)

syms <- commandArgs(trailingOnly = TRUE)

for (sym in syms) {
    give_recommendation(sym)
}
