
###############################################
# ... Tidymodels workflow ...
###############################################

# split dataset
#phenodat1_split <- initial_split(dd, prop = 1)
#phenodat1_train <- training(phenodat1_split)
#phenodat1_test <- testing(phenodat1_split)

# create model recipe
phenodat1_rec <- recipe(y_2004 ~ ., data = phenodat1_train)
# estimate the required parameters from the training set that can
# be later applied to other datasets
phenodat1_prep <- prep(phenodat1_rec)
# applied operation specificied in recipe
juiced <- juice(phenodat1_prep)

# specifcy model type, engine etc.
tune_spec <- rand_forest(mtry = tune(),trees = 1000,min_n = tune()) %>%
  set_args(importance = 'permutation') %>%
  set_mode("regression") %>%
  set_engine("ranger")

# create model workflow
tune_wf <- workflow() %>%
  add_recipe(phenodat1_rec) %>%
  add_model(tune_spec)



# model training (uses grid search): only tunes for hyperparameters
doParallel::registerDoParallel(cores = 7)
tune_res <- tune_grid(tune_wf,resamples = trees_folds,grid = 10)



# select the best model in terms of performance
best_rsme <- select_best(tune_res, "rmse")

# create final model object with best hyperparameters
final_rf <- finalize_model(tune_spec,best_rsme)

# variable importance stuff


# calculate variable importance
# note the use of data juiced
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(y_2004 ~ .,data = juiced)->imp

#imp %>%
#  vip(geom = "point")

# variable importance plot
imp$fit$variable.importance %>%
  as_tibble() %>%
  mutate(variables = names(imp$fit$variable.importance)) %>%
  inner_join(labs) %>%
  arrange(value) %>%
  mutate(variables_renamed = forcats::as_factor(variables_renamed)) %>% {.->>tmp} %>%
  ggplot()+geom_bar(aes(x=variables_renamed,y=value,fill=group),stat="identity")+coord_flip()+theme_minimal()+
  xlab("Variable")+ylab("Increase in MSE")+theme(axis.text=element_text(colour = "black",size=12),
                                                 axis.title = element_text(size=19,colour="black"),
                                                 )+ylim(0,600)+
  scale_fill_manual(values=c("black","deepskyblue3","brown3","dark green"))->varimprf
ggsave(varimprf,filename = "./figures/July21/varimprf_tidymodels.png",width = 12,height=9)


###########################
# partial dependence plots
###########################
library(DALEXtra)

# select best model
best_model <- select_best(tune_res, "rmse")

# refit final best model
final_res <- tune_wf %>%
  finalize_workflow(best_model) %>%
  last_fit(phenodat1_split)

# get fitted values
final_fitted <- final_res$.workflow[[1]]

# create explainer for model workflow using DALEX

phenodat1_train  %>%
  ungroup() %>%
  dplyr::select(!!c(paste0('bio_0',1:9),paste0('bio_',10:19),'std_elev','aridity','humi'))->X


phenodat1_explainer <- explain_tidymodels(
  final_fitted,
  data = X,
  y = phenodat1_train$y_2004,
  verbose = FALSE
)


# create a series of pdp
tmp %>%
  arrange(desc(value))->tmp1

res<-NULL

for (i in 1:dim(tmp1)[1]){
  print(i)
  tmp1[i,]$variables->varname
  # calculate partial dependence plot
  pdp <- model_profile(
    phenodat1_explainer,
    variables = varname,
    N = NULL
  )
  as_tibble(pdp$agr_profiles) %>%
    rename(vname = `_vname_`,x = `_x_`,phenodiv = `_yhat_`)->tmpres
    bind_rows(tmpres,res)->res
  
}



# this is a plot used for calculating.
res %>%
  rename(variables = vname) %>%
  inner_join(labs) %>%
  ggplot(aes(x, phenodiv)) +geom_line(size = 1.2, alpha = 0.8,color="red") +
  labs(x = "variable",y = "Pheno diversity")+theme_minimal()+
  theme(axis.text = element_text(size=9,colour="black"),
          axis.title = element_text(size=12,colour="black"),
        strip.text = element_text(face = "bold"))+
  facet_wrap(~variables_renamed,labeller = label_wrap_gen(width=10),scales = "free")->pdprf

ggsave(pdprf,filename = "./figures/July21/pdprf_tidymodels.png",width = 12,height=9)


#
