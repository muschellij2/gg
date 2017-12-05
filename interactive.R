
library(tidyr)
library(reshape2)
library(matrixStats)
library(dplyr)
library(ggplot2)
library(plotly)
library(crosstalk)
res = rnorm(5000*200)
res = matrix(res,nrow=5000)
res = colCumsums(res)
df = data.frame(res, n = 1:nrow(res))
long = melt(df,id='n', value.name = "sn")
long$sn_n = long$sn / long$n
long$sn_sqrtn = long$sn / sqrt(long$n)
long$loglog = pmax(1e-7, log(log(long$n)))
long$sn_loglog = long$sn / sqrt(long$n * long$loglog)
long$log_out = abs(long$sn_loglog) > sqrt(2)

long = long[ (long$n %% 25 == 0),]


outside = long %>% group_by(n) %>% summarize(pct = mean(log_out))
within = long %>% 
  group_by(n) %>% 
  summarize(pct = mean(!log_out))

plot(within$n, within$pct, 
     type ="l", ylim = c(0.90, 1))
abline(h = 1)

longer = long %>% 
  select(-sn, -loglog) %>% 
  gather(type, value = value, sn_n, sn_sqrtn, sn_loglog)

shared_longer <- SharedData$new(longer)

g = shared_longer %>% 
  ggplot(aes(x = n, y = value, group = variable)) +
  geom_line(alpha=1) 
gfac = g +  facet_wrap(~ type)
gfac_col  = g +  facet_wrap(~ type, ncol = 1)
gfac_col
# ggplotly(gfac_col)
