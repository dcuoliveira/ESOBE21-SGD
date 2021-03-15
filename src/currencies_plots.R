rm(list=ls())
library('here')
library('readxl')
library('ggplot2')
library('quantmod')
library('dplyr')
library('gridExtra')
library('reshape2')

df = read_excel(here('src', 'data', 'SNEER_SGD.xlsx'), skip = 3)
df_ret =as.data.frame(apply(df[2:dim(df)[2]], 2, function(x) (x/Lag(x) - 1) * 100))
df_ret$Dates = df$Dates

plt1 = ggplot(data = df, aes(x = Dates, y = `SNEER Index`)) + geom_line()
plt2 = ggplot(data = df_ret, aes(x = Dates, y = `SNEER Index`)) + geom_line()
grid.arrange(plt1, plt2, nrow=2)

plt1 = ggplot(data = df, aes(x = Dates, y = `SNEER Index`)) + geom_line()
plt2 = ggplot(data = df, aes(x = Dates, y = `SGD Curncy`)) + geom_line()
grid.arrange(plt1, plt2, nrow=2)

plt1 = ggplot(data = df_ret, aes(x = Dates, y = `SNEER Index`)) + geom_line()
plt2 = ggplot(data = df_ret, aes(x = Dates, y = `SGD Curncy`)) + geom_line()
grid.arrange(plt1, plt2, nrow=2)

ggplot(df_ret, aes(x=`SGD Curncy`, y=`TWD Curncy`)) + geom_point()

df_melt = df %>% dplyr::select(Dates, `SNEER Index`, `SGD Curncy`) %>% melt('Dates')
ggplot(df_melt, aes(x = Dates, y = value)) + geom_line(aes(color = variable))

