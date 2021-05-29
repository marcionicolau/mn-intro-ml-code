## ----setup, include=FALSE-----------------------------------------------------------------------
options(htmltools.dir.version = FALSE)


## ----libraries, echo=TRUE, message=FALSE, output=FALSE------------------------------------------
library(tidyverse)
library(tidymodels)


## -----------------------------------------------------------------------------------------------
data(ames)


## -----------------------------------------------------------------------------------------------
lm_ames <- lm(Sale_Price ~ Gr_Liv_Area, data = ames)
lm_ames


## ----decision_tree------------------------------------------------------------------------------
decision_tree() |>
  set_engine("C5.0") |>
  set_mode("classification")


## ----knn, eval=FALSE, echo=TRUE-----------------------------------------------------------------
nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression")


## ----lm_spec1-----------------------------------------------------------------------------------
lm_spec = linear_reg(
  mode = "regression", penalty = NULL, mixture = NULL
)


## ----lm_spec2, echo=TRUE, eval=FALSE------------------------------------------------------------
lm_spec = linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") # redundante


## ----lm_train1, echo=TRUE, eval=FALSE-----------------------------------------------------------
lm(Sale_Price ~ Gr_Liv_Area, data = ames)


## ----lm_train2, warning=FALSE-------------------------------------------------------------------
lm_spec %>%
  fit(Sale_Price ~Gr_Liv_Area, data = ames)


## ----lm_predict---------------------------------------------------------------------------------
lm_fit = lm_spec %>% 
  fit(Sale_Price ~ Gr_Liv_Area, data = ames)

predict(lm_fit, new_data = ames) %>% head(n = 3)


## ----pred_graph, fig.height=4, fig.width=6------------------------------------------------------
predict(lm_fit, new_data = ames) %>% 
  mutate(truth = ames$Sale_Price) %>%
  ggplot(aes(x=truth, y = .pred)) + 
  geom_point() + 
  xlab('Original') + ylab('Predito') + theme_bw()


## ----lm_metrics---------------------------------------------------------------------------------
predict(lm_fit, new_data = ames) %>% 
  mutate(price_truth = ames$Sale_Price) %>%
  rmse(truth = price_truth, estimate = .pred)


## ----lm_ames_hodout, warning=FALSE--------------------------------------------------------------
set.seed(2021) # Favor NÃO esquecer!

ames_split = initial_split(ames, prop = 0.8)
ames_train = training(ames_split)
ames_test = testing(ames_split)

lm_spec %>%
  fit(Sale_Price ~ Gr_Liv_Area, data = ames_train) %>%
  predict(new_data = ames_test) %>%
  mutate(price_truth = ames_test$Sale_Price) %>%
  rmse(truth = price_truth, estimate = .pred)


## ----ames_tree, warning=FALSE-------------------------------------------------------------------

rt_spec = decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

set.seed(2021)

rt_spec %>%
  fit(Sale_Price ~ Gr_Liv_Area, data = ames_train) %>%
  predict(new_data = ames_test) %>%
  mutate(price_truth = ames_test$Sale_Price) %>%
  rmse(truth = price_truth, estimate = .pred)


## ----kknn, eval=FALSE, echo=TRUE----------------------------------------------------------------

install.packages('kknn')


## ----ames_knn, warning=FALSE--------------------------------------------------------------------

knn_spec = nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression")

set.seed(2021)

knn_spec %>%
  fit(Sale_Price ~ Gr_Liv_Area, data = ames_train) %>%
  predict(new_data = ames_test) %>%
  mutate(price_truth = ames_test$Sale_Price) %>%
  rmse(truth = price_truth, estimate = .pred)


## ----sales_plot, fig.height=4, fig.width=6------------------------------------------------------

tidymodels_prefer() # conflitos

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50) + theme_bw()


## ----fig.height=4, fig.width=6------------------------------------------------------------------
ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50) +
  scale_x_log10() + theme_bw()


## ----ames_strata--------------------------------------------------------------------------------

set.seed(2021)
ames_split <- initial_split(ames, prop = 0.80, 
                            strata = Sale_Price) # breaks = 4
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)



## ----lm_base, eval=FALSE, echo=TRUE-------------------------------------------------------------
lm(Sale_Price ~ Neighborhood +
     log10(Gr_Liv_Area) + Year_Built + Bldg_Type,
   data = ames)


## ----lm_ames_recipes----------------------------------------------------------------------------

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + 
           Bldg_Type, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% # recipes (step_*)
  step_dummy(all_nominal_predictors()) # dplyr
simple_ames



## ----prep---------------------------------------------------------------------------------------
simple_ames <- prep(simple_ames, training = ames_train)
simple_ames


## -----------------------------------------------------------------------------------------------
test_ex = bake(simple_ames, new_data = ames_test)
names(test_ex) %>% head()


## -----------------------------------------------------------------------------------------------
bake(simple_ames, ames_test, starts_with("Neighborhood_")) %>% 
  names() %>% head()


## ----graph_other, fig.height=5, fig.width=8-----------------------------------------------------
ggplot(ames_train, aes(y = Neighborhood)) + 
  geom_bar() + 
  labs(y = NULL)


## ----neigh_step---------------------------------------------------------------------------------
simple_ames =
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

prep(simple_ames, ames_train)


## ----fig.height=6, fig.width=10, echo=FALSE-----------------------------------------------------
ggplot(ames_train, aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")


## -----------------------------------------------------------------------------------------------
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + 
           Year_Built + Bldg_Type, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )


## -----------------------------------------------------------------------------------------------
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + 
           Year_Built + Bldg_Type + Latitude + Longitude, 
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)


## -----------------------------------------------------------------------------------------------
ames_rec_prepped <- prep(ames_rec)
ames_train_prepped <- bake(ames_rec_prepped, new_data = NULL)
ames_test_prepped <- bake(ames_rec_prepped, ames_test)


## -----------------------------------------------------------------------------------------------
lm_fit <- lm(Sale_Price ~ ., data = ames_train_prepped)
glance(lm_fit) # broom


## -----------------------------------------------------------------------------------------------
predict(lm_fit, ames_test_prepped %>% head())


## -----------------------------------------------------------------------------------------------
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)


## -----------------------------------------------------------------------------------------------
rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger", verbose = TRUE) %>% 
  set_mode("regression") %>% translate()


## -----------------------------------------------------------------------------------------------
lm_form_fit %>% pluck("fit")


## -----------------------------------------------------------------------------------------------
lm_form_fit %>% pluck("fit") %>% vcov()


## -----------------------------------------------------------------------------------------------
ames_test_small = ames_test %>% slice(1:5)
ames_test_small %>%
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, 
                    type = "pred_int")) 


## -----------------------------------------------------------------------------------------------
tree_model = 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit = 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))


## -----------------------------------------------------------------------------------------------
library(usemodels)

use_xgboost(Sale_Price ~ Neighborhood + Gr_Liv_Area + 
              Year_Built + Bldg_Type + 
              Latitude + Longitude, 
            data = ames_train,
            # Don't create the model tuning code:
            tune = FALSE,
            # Add comments explaining some of the code:
            verbose = TRUE)

## Add in
bag_tree_rpart_spec <-
  baguette::bag_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

decision_tree_rpart_spec <-
  decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) %>%
  set_engine('rpart') %>%
  set_mode('regression')

linear_reg_keras_spec <-
  linear_reg(penalty = tune()) %>%
  set_engine('keras')

linear_reg_stan_spec <-
  linear_reg() %>%
  set_engine('stan')



## -----------------------------------------------------------------------------------------------
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow


## -----------------------------------------------------------------------------------------------
lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow


## -----------------------------------------------------------------------------------------------
lm_fit <- fit(lm_wflow, ames_train)
lm_fit


## -----------------------------------------------------------------------------------------------
predict(lm_fit, ames_test %>% slice(1:3))


## -----------------------------------------------------------------------------------------------
lm_fit %>% update_formula(Sale_Price ~ Longitude)


## -----------------------------------------------------------------------------------------------
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_recipe(ames_rec)
lm_wflow


## -----------------------------------------------------------------------------------------------
# Does `prep()`, `bake()`, and `fit()` in one step:
lm_fit <- fit(lm_wflow, ames_train)

# Does `bake()` and `predict()` automatically:
predict(lm_fit, ames_test %>% slice(1:3))


## -----------------------------------------------------------------------------------------------
lm_fit %>% 
  pull_workflow_prepped_recipe() %>% 
  tidy()


## -----------------------------------------------------------------------------------------------
lm_fit %>% 
  # This returns the parsnip object:
  pull_workflow_fit() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)


## -----------------------------------------------------------------------------------------------
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

library(workflowsets)
location_models <- workflow_set(preproc = location, 
                                models = list(lm = lm_model))
location_models


## -----------------------------------------------------------------------------------------------
location_models <-
   location_models %>%
   mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models


## -----------------------------------------------------------------------------------------------
ames_test_res = predict(lm_fit, new_data = ames_test %>% 
                          select(-Sale_Price))

ames_test_res = bind_cols(ames_test_res, ames_test %>% 
                            select(Sale_Price))

ames_metrics = metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)



## -----------------------------------------------------------------------------------------------

set.seed(55)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds


## -----------------------------------------------------------------------------------------------
vfold_cv(ames_train, v = 10, repeats = 5)


## -----------------------------------------------------------------------------------------------
loo_cv(ames_train)


## -----------------------------------------------------------------------------------------------
vfold_cv(ames_train, v = 10, repeats = 5)


## -----------------------------------------------------------------------------------------------
keep_pred = control_resamples(save_pred = TRUE, 
                              save_workflow = TRUE)

rf_model = 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow = 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + 
      Year_Built + Bldg_Type + Latitude + Longitude) %>% 
  add_model(rf_model)


## -----------------------------------------------------------------------------------------------
set.seed(2021)
rf_res = 
  rf_wflow %>% 
  fit_resamples(resamples = ames_folds, control = keep_pred)
rf_res


## -----------------------------------------------------------------------------------------------
collect_metrics(rf_res)


## -----------------------------------------------------------------------------------------------
assess_res <- collect_predictions(rf_res)
assess_res


## -----------------------------------------------------------------------------------------------
basic_rec =
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec =
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 


## -----------------------------------------------------------------------------------------------
spline_rec =
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

preproc =
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )


## -----------------------------------------------------------------------------------------------
lm_models = workflow_set(preproc, 
                         list(lm = lm_model), cross = FALSE)
lm_models


## -----------------------------------------------------------------------------------------------
lm_models =
  lm_models %>% 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               seed = 1101, verbose = TRUE,
               # Options to `fit_resamples()`: 
               resamples = ames_folds, control = keep_pred)


## -----------------------------------------------------------------------------------------------
lm_models


## -----------------------------------------------------------------------------------------------
lm_models


## -----------------------------------------------------------------------------------------------
collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")


## -----------------------------------------------------------------------------------------------
four_models = 
  as_workflow_set(random_forest = rf_res) %>% 
  bind_rows(lm_models)
four_models


## ----fig.height=6, fig.width=8------------------------------------------------------------------
autoplot(four_models, metric = "rsq")


## -----------------------------------------------------------------------------------------------
rsq_indiv_estimates =
  collect_metrics(four_models, summarize = FALSE) %>% 
  filter(.metric == "rsq") 

rsq_wider =
  rsq_indiv_estimates %>% 
  select(wflow_id, .estimate, id) %>% 
  pivot_wider(id_cols = "id", names_from = "wflow_id", 
              values_from = ".estimate")


## ----fig.height=4, fig.width=10-----------------------------------------------------------------
rsq_indiv_estimates %>% 
  mutate(wflow_id = reorder(wflow_id, .estimate)) %>% 
  ggplot(aes(x = wflow_id, y = .estimate, 
             group = id, col = id)) + 
  geom_line(alpha = .5, lwd = 1.25) + 
  theme(legend.position = "none") + 
  labs(x = NULL, y = expression(paste(R^2, "statistics"))) +
  theme_bw()

