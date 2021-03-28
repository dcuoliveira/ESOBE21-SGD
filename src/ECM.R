rm(list=ls())
library('here')
library('dplyr')
library('lubridate')
source(here('src', 'utils.R'))

data = merge_fx_sneer_data() %>% select(-date)
data_scale = apply(data, 2, function(x) scale(x)) %>% as.data.frame()

## ECM
ecm = lm(SNEER ~ KRW + MYR + CNY + THB + IDR + TWD + INR + JPY + EUR + AUD + GBP, data)
plot(ecm$residuals, type = 'l')

output = data.frame(date=ymd(rownames(data)),
                    sneer=data$SNEER,
                    sneer_fitted=ecm$fitted.values,
                    diff_sneer_fit=data$SNEER-ecm$fitted.values)

plot(output$date, output$sneer_fitted, type = 'l', col = 'blue')
par(new = 'T')
plot(output$date, output$sneer, type = 'l', col = 'red')
PP.test(ecm$residuals)

par(mfrow=c(2,1))
plot(output$date, output$sneer_fitted, type = 'l', col = 'blue')
par(new = 'T')
plot(output$date, output$sneer, type = 'l', col = 'red')
plot(output$date, output$diff_sneer_fit, type = 'l')
