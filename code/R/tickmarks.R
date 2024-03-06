# Fit a random forest to the boston housing data
library(randomForest)
library(tidyverse)

data (boston)  # load the boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston)

res<-partial(boston.rf, pred.var = "lstat")

res |>
  filter(lstat %in% quantile(res$lstat, probs = seq(.1, .9, by = .1))) |>
  bind_rows(res |> filter(lstat==max(lstat))) |>
  bind_rows(res |> filter(lstat==min(lstat)))->ticks 


res |>
  ggplot(aes(x=lstat,y=yhat))+geom_line()+theme()


head(partial(boston.rf, pred.var = "lstat"))  # returns a data frame
partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE)->res1
