library(ICEbox)
library(MASS)

#https://towardsdatascience.com/how-to-explain-and-affect-individual-decisions-with-ice-curves-1-2-f39fd751546f

X = Boston
y = X$medv
X$medv = NULL

bhd_rf_mod = randomForest(X, y)
bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age", frac_to_build = 1)


# unique values of predictor of interest i.e. age
bhd.ice$xj[1]
# values predicted using the original dataframe
bhd.ice$actual_prediction[1]
# first ice curve
bhd.ice$ice_curves[1,]

# do it manually find unique values in dataframe (if there are any duplicates!)
# not really necessary if one wants to have an ICE for every observation 
X %>%
  mutate(rown = 1:length(age)) %>%
  unique() %>%
  arrange(age) %>%
  as_tibble()->dd

# prepare dataframe for prediction: variable of interest allowed to change
# values for other variables kept constant using the value for the observation
# of interest
dd %>%
  mutate(crim = dd$crim[1],zn = dd$zn[1],
         indus = dd$indus[1],chas =dd$chas[1],nox = dd$nox[1],
         rm = dd$rm[1],dis = dd$dis[1],
         rad=dd$rad[1], tax = dd$tax[1],ptratio=dd$ptratio[1],
         black = dd$black[1],
         lstat = dd$lstat[1])->pred_df1

predict(bhd_rf_mod,pred_df1)


