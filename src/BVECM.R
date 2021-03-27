library('readxl')
library('here')
library('dplyr')
library('bvartools')

fx_data = read_excel(here('src', 'data', 'SNEER_SGD.xlsx'), sheet = 'FX')
sneer_data = read_excel(here('src', 'data', 'SNEER_SGD.xlsx'), sheet = 'SNEER') %>%
  rename(SNEER=PX_LAST)
merge_data = merge(sneer_data, fx_data)





