library('padr')

data_frame_to_ts_list = function(df,
                                 freq_vec){
  
}

merge_fx_sneer_data = function(){
  fx_data = read.csv(here('src', 'data', 'currencies.csv')) %>% mutate(date=ymd(date)) %>% select(-X)
  
  sneer_data = read.csv(here('src', 'data', 'sneer.csv'))%>% mutate(date=ymd(date)) %>%
    pad() %>% mutate(weekday=weekdays(date, abbreviate = TRUE)) %>% filter(weekday=='Sex') %>% select(-weekday, -X)
  
  merge_data = merge(sneer_data, fx_data) 
  merge_data = na.locf(na.locf(merge_data), fromLast = TRUE)
  colnames(merge_data) = unlist(lapply(colnames(merge_data), function(x){strsplit(x, '.', fixed = TRUE)[[1]][1]}))
  rownames(merge_data) = merge_data$date
  
  return(merge_data)
}