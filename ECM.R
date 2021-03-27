rm(list=ls())
library('readxl')
library('here')
library('dplyr')
library('lubridate')
library('zoo')
source(here('src', 'utils.R'))

data = merge_fx_sneer_data()

## ECM
ecm = lm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD + INR + JPY + EUR + AUD + GBP, data)
