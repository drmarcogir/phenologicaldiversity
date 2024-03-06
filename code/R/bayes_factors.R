library(tidyverse)
library(rstanarm)
library(bayestestR)

read_csv("./jeodppfiles/dat1_simple.csv") %>%
  group_by(cat) %>%
  #group_split()->dat2
  nest() %>%
  ungroup()->dat2


dat2[10,]$data[[1]]->dd1

tibble(year=c(2003:2020),
       yearf = c(rep('p1','9'),rep('p2','9'))) %>%
  bind_cols(dd1) %>%
  mutate(yearf =as.factor(yearf))->dd1
mod1<-stan_glm(value~yearf,data=dd1)
loonull<- loo(mod1, cores = 2,k_threshold = 0.7)



start_time <- Sys.time()
mod0<-stan_glm(value~1,data=dd1)
mod1<-stan_glm(value~bio1,data=dd1)
loonull<- loo(mod0, cores = 2,k_threshold = 0.7)
loofull <- loo(mod1, co res = 2, k_threshold = 0.7)
as_tibble(mod1$stan_summary) %>% mutate(varnames = row.names(mod1$stan_summary))->res
end_time <- Sys.time()

fit_mod<-function(x)
{
  mod0<-stan_glm(value~1,data=x)
  mod1<-stan_glm(value~bio1,data=x)
  loonull<- loo(mod0, cores = 2,k_threshold = 0.7)
  loofull <- loo(mod1, cores = 2, k_threshold = 0.7)
  return(as_tibble(mod1$stan_summary) %>% mutate(varnames = row.names(mod1$stan_summary)) %>%
    mutate(looicfull = loofull$estimates[3,1], looicnull= loonull$estimates[3,1],
           p_direction = p_direction(mod1)$pd[2]))
}

system.time(fit_mod(x=dd1))
