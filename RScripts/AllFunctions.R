########################################################################
####################### Laudine Carbuccia ##############################
########################################################################


########################################################################
############# Clean the variables names of the models ##################
########################################################################

# clean the variables names to get a vector of variables
# Create a function to do that
clean_variable_names <- function(vars) {
  # vector to store the names
  unique_base_names <- character()
  
  for(var in vars) {
    # Use the underscore as an indication of the variables to clean
    if(grepl("_", var)) {
      base_name <- strsplit(var, "_")[[1]][1]
    } else {
      base_name <- var
    }
    unique_base_names <- c(unique_base_names, base_name)
  }
  return(unique(unique_base_names))
  
}

####################################################
# Simple regression #########################
################################################
#----- SimpleRegression ---------------
regression_all_table <- function(train, test, model_recipe, coef_names, model_name = "Intention") {
  tidymodels_prefer()
  
  # Specify the linear regression model
  reg_model <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")
  
  # Create workflow
  reg_flow <- workflow() %>%
    add_model(reg_model) %>%
    add_recipe(model_recipe)
  
  # Fit model to training data
  reg_fit <- fit(reg_flow, train)
  
  # Evaluate model on test data
  performance_metrics <- reg_fit %>%
    predict(test) %>%
    bind_cols(test) %>%
    metrics(truth = target, estimate = .pred)
  
  # Sort coefficients by absolute value (drop intercept)
  sorted_coefficients <- tidy(reg_fit) %>%
    arrange(desc(abs(estimate))) %>%
    filter(term != "(Intercept)")
  
  # Robust coef_map ordering:
  # - remove duplicated terms (can happen with duplicated term labels)
  # - keep only terms that exist in coef_names (avoid NA entries)
  sorted_names <- sorted_coefficients$term
  sorted_names <- sorted_names[!duplicated(sorted_names)]
  sorted_names <- intersect(sorted_names, names(coef_names))
  
  # Include intercept if provided in coef_names, then ensure unique names
  ordered_coef_map <- coef_names[c("(Intercept)", sorted_names)]
  ordered_coef_map <- ordered_coef_map[!duplicated(names(ordered_coef_map))]
  
  # Create summary table with the correct model name
  models_list <- list(reg_fit)
  names(models_list) <- model_name
  
  model_summary_table <- modelsummary(
    models_list,
    coef_map = ordered_coef_map,
    statistic = "{std.error} ({p.value})",
    estimate  = "{estimate}{stars}  [{conf.low}, {conf.high}]",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    output = "flextable"
  ) |>
    autofit()
  
  return(list(
    model_summary = model_summary_table,
    coefficients  = tidy(reg_fit),
    performance   = performance_metrics,
    model_fit     = reg_fit
  ))
}

#--------------- Forest Plot -------------------------------
# Function to create a forest plot from regression results
create_forest_plot <- function(reg_fit, var_labels, title = "Regression Coefficients", 
                               subtitle = "Forest plot of coefficient estimates", 
                               color = "turquoise4") {
  # Get coefficient estimates with confidence intervals
  out_conf <- tidy(reg_fit, conf.int = TRUE)
  
  # Round values to 2 decimal places
  lm_model_out <- forestmangr::round_df(out_conf, digits = 2)
  
  # Remove the intercept
  lm_model_out <- lm_model_out[-1, ]
  
  # Add a column with clean labels
  lm_model_out$clean_term <- var_labels[lm_model_out$term]
  
  # Replace any NAs (if some terms are not in our dictionary)
  lm_model_out$clean_term[is.na(lm_model_out$clean_term)] <- lm_model_out$term[is.na(lm_model_out$clean_term)]
  
  # Sort by the magnitude of the coefficients
  lm_model_out <- lm_model_out %>%
    arrange(desc(estimate))
  
  # Convert clean_term to a factor to preserve the order
  lm_model_out$clean_term <- factor(lm_model_out$clean_term, levels = lm_model_out$clean_term)
  
  # Create the forest plot with clean terms
  plot <- ggplot(lm_model_out, aes(x = clean_term, y = estimate)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2, size = 1,
                  position = "dodge", color = color) +
    geom_hline(yintercept = 0, color = "red", size = 1) +
    geom_point(size = 2) + 
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = "Coefficient Estimates"
    )
  
  return(plot)
}

####################################################
###### LASSO REGRESSION #########################
################################################

#----- LassoRegression ---------------
lasso_regression_table <- function(train, test, model_recipe, coef_names, 
                                   model_name = "LASSO", 
                                   grid_size = 20, 
                                   penalty_range = c(-5, 2),
                                   seed = 123,
                                   folds = 10, 
                                   repeats = 5) {
  # Tidymodels setup
  tidymodels_prefer()
  set.seed(seed)
  
  # Create cross-validation folds
  train_folds <- vfold_cv(train, v = folds, repeats = repeats)


  # Specify the LASSO model
  # mixture = 1 for LASSO (pure L1 regularization)
  lasso_model <- linear_reg(penalty = tune(), 
                            mixture = 1) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  # Create workflow
  lasso_flow <- workflow() %>%
    add_model(lasso_model) %>%
    add_recipe(model_recipe)

  
  # Using dials to tune the model: 
  glmnet_param <- extract_parameter_set_dials(lasso_model)# it extracts for me the parameter that 
  # needs to be tuned
  glmnet_param
  
  # Define tuning grid for lambda values
  # penalty_range defines the range of powers of 10 for lambda
  lambda_grid <- grid_max_entropy(glmnet_param,
                                 size = 175, # generate 50 different lambdas
                                 # 50 is a bit low, 100 or 150 is better
                                 original = TRUE, # we are keeoing the original scale, the log
                                 variogram_range = 0.5) # the higher the value, the less likely it is to explore 
  
  # Tune the model using cross-validation
  lasso_tune <- tune_grid(
    lasso_flow,
    resamples = train_folds,
    grid = lambda_grid,
    metrics = metric_set(rmse, rsq, mae)
  )
  
  # Select the best lambda based on RMSE
  lasso_best <- select_best(lasso_tune, metric = "rmse")
  
  # Finalize the workflow with the best lambda
  final_lasso <- finalize_workflow(lasso_flow, lasso_best)
  
  # Fit the final model on the entire training dataset
  lasso_fit <- fit(final_lasso, train)
  
  # Evaluate model on test data
  performance_metrics <- lasso_fit %>%
    predict(test) %>%
    bind_cols(test) %>%
    metrics(truth = target, estimate = .pred)
  
  # Get coefficients from the LASSO model
  # Filter to keep only the coefficients for the best lambda
  lasso_coefs <- pull_workflow_fit(lasso_fit) %>%
    tidy() %>%
    filter(lambda == pull(lasso_best, penalty)) %>%
    arrange(desc(abs(estimate))) %>%
    filter(term != "(Intercept)")
  
  # Create vector with sorted variable names by coefficient magnitude
  sorted_names <- lasso_coefs$term
  
  # Order coefficient map including intercept
  all_terms <- c("(Intercept)", sorted_names)
  ordered_coef_map <- coef_names[all_terms]
  
  # Create summary table with the correct model name
  models_list <- list(lasso_fit)
  names(models_list) <- model_name
  
  model_summary_table <- modelsummary(
    models_list,
    coef_map = ordered_coef_map,
    statistic = "{std.error} ({p.value})",
    estimate = "{estimate}{stars}  [{conf.low}, {conf.high}]",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    output = 'flextable'
  ) |> autofit()
  
  # Create a variable importance plot
  var_importance <- lasso_coefs %>%
    filter(estimate != 0) %>%  # Keep only non-zero variables (selected by LASSO)
    mutate(term = factor(term, levels = term[order(abs(estimate))])) %>%
    ggplot(aes(x = abs(estimate), y = term)) +
    geom_col() +
    labs(
      title = "Variable Importance (LASSO)",
      x = "Coefficient Magnitude",
      y = NULL
    ) +
    theme_minimal()
  
  # Return list of outputs
  return(list(
    model_summary = model_summary_table,  # Summary table of the model
    coefficients = lasso_coefs,           # Coefficient details
    performance = performance_metrics,    # Test set performance metrics
    model_fit = lasso_fit,                # The fitted model object
    lasso_best = lasso_best,              # Best lambda value
    lasso_tune = lasso_tune,              # Full tuning results (for autoplot)
    var_importance = var_importance       # Variable importance plot
  ))
}
# Feature Selection and Random Forest Optimization Functions

########################################################################
#-----  Function 1: Perform Recursive Feature Elimination (RFE) -------
########################################################################

# ------------------------------------------------------
# This function performs recursive feature elimination with cross-validation
# to identify the optimal set of predictors for a given dataset and target variable.

perform_rfe <- function(train_df, target_var = "target", recipe_obj = NULL, 
                        min_features = 5, step_size = 5, cv_folds = 10, 
                        seed_val = 1451) {
  
  # Load required packages if not already loaded
  pacman::p_load(caret, recipes)
  tidymodels_prefer()
  
  # Create a basic preprocessing recipe if none is provided
  if (is.null(recipe_obj)) {
    recipe_obj <- recipe(formula(paste(target_var, "~ .")), data = train_df)
  }
  
  # Prepare the recipe and process the training data
  prep_recipe <- prep(recipe_obj, training = train_df)
  train_processed <- bake(prep_recipe, new_data = train_df)
  
  # Configure RFE control parameters
  set.seed(seed_val)
  
  ctrl <- rfeControl(
    functions = rfFuncs,    # Random forest as base model for feature selection
    method = "cv",          # Cross-validation method
    number = cv_folds,      # Number of CV folds
    verbose = TRUE          # Display progress
  )
  
  # Define feature subsets to test
  n_features <- ncol(train_processed) - 1  # Exclude the target variable
  subsets <- seq(min_features, n_features, by = step_size)
  
  # Execute RFE with cross-validation
  set.seed(seed_val)
  rfe_results <- rfe(
    x = train_processed %>% select(-all_of(target_var)),
    y = train_processed[[target_var]],
    sizes = subsets,
    rfeControl = ctrl,
    metric = "Accuracy"     # Optimization metric (can be changed for regression tasks)
  )
  
  # Extract selected variables
  optimal_vars <- predictors(rfe_results)
  
  # Clean variable names (removing one-hot encoding suffixes)
  clean_variable_names <- function(vars) {
    unique_base_names <- character()
    
    for(var in vars) {
      # If variable contains underscore (from one-hot encoding), extract base name
      if(grepl("_", var)) {
        base_name <- strsplit(var, "_")[[1]][1]
      } else {
        base_name <- var
      }
      unique_base_names <- c(unique_base_names, base_name)
    }
    return(unique(unique_base_names))
  }
  
  cleaned_variables <- clean_variable_names(optimal_vars)
  
  # Create filtered datasets with selected variables
  train_df_filtered <- train_df %>% 
    select(all_of(target_var), all_of(cleaned_variables))
  
  # Return results as a list
  return(list(
    rfe_results = rfe_results,
    optimal_vars = optimal_vars,
    cleaned_variables = cleaned_variables,
    train_filtered = train_df_filtered
  ))
}



#----- Function 2: Create optimized recipe based on RFE results -------

# This function generates an optimized recipe using only the variables 
# selected by RFE while preserving the transformation steps from the original recipe.

create_optimized_recipe <- function(train_df_filtered, target_var = "target",
                                    unknown_vars = c("EntryAgeBeforeOne", 
                                                     "AttitudesTowardsWorkAndFamily", 
                                                     "AttitudesLaborAffectsChildren"),
                                    impute_vars = c( #"AffordCostBaseline", 
                                                    "Taux.de.couv.global",
                                                    "NumberChildren", 
                                                    "PrescriptiveNorm"),
                                    yeojohnson_vars = c(#"AffordCostBaseline", 
                                                        "MotherHealthIndex",
                                                        "BabyHealthIndex", 
                                                        "NumberChildren", 
                                                        "OpportunityCosts", 
                                                       # "MonthlyDaycareFees",
                                                        "PrescriptiveNorm"),
                                    knn_neighbors = 5) {
  
  # Get the selected variables from the filtered dataframe
  selected_vars <- setdiff(names(train_df_filtered), target_var)
  
  # Compute intersections with transformation variables
  unknown_vars_filtered <- intersect(unknown_vars, selected_vars)
  impute_vars_filtered <- intersect(impute_vars, selected_vars)
  yeojohnson_vars_filtered <- intersect(yeojohnson_vars, selected_vars)
  
  # Initialize the recipe with the filtered data
  optimized_recipe <- recipe(formula(paste(target_var, "~ .")), 
                             data = train_df_filtered)
  
  # Add transformation steps conditionally based on selected variables
  # Step 1: Handle unknown values for categorical variables
  if (length(unknown_vars_filtered) > 0) {
    optimized_recipe <- optimized_recipe %>% 
      step_unknown(all_of(unknown_vars_filtered))
  }
  
  # Step 2: Create dummy variables for all nominal predictors
  optimized_recipe <- optimized_recipe %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
  
  # Step 3: Normalize numeric predictors
  optimized_recipe <- optimized_recipe %>%
    step_normalize(all_numeric_predictors())
  
  # Step 4: KNN imputation for variables with missing values
  if (length(impute_vars_filtered) > 0) {
    optimized_recipe <- optimized_recipe %>% 
      step_impute_knn(all_of(impute_vars_filtered), neighbors = knn_neighbors)
  }
  
  # Step 5: Apply Yeo-Johnson transformation for skewed variables
  if (length(yeojohnson_vars_filtered) > 0) {
    optimized_recipe <- optimized_recipe %>% 
      step_YeoJohnson(all_of(yeojohnson_vars_filtered))
  }
  
  return(optimized_recipe)
}





#----- Function 3: Tune random forest model with selected features -------

# This function tunes a random forest model using the optimized recipe
# and returns the best model based on cross-validation results.
# I have added a progress bar because the tuning takes a very long time (several hours)

tune_random_forest <- function(train_df_filtered, test_df = NULL, target_var = "target",
                               recipe_obj, cv_folds = 10, cv_repeats = 5,
                               grid_size = 150, var_range = 0.75, n_cores = NULL, seed_val = 30,
                               engine = "partykit", show_progress = TRUE) {
  
  # Load required packages
  pacman::p_load(doParallel, progressr, future)
  tidymodels_prefer()
  
  # Filter test data if provided
  if (!is.null(test_df)) {
    selected_vars <- setdiff(names(train_df_filtered), target_var)
    test_df_filtered <- test_df %>% 
      select(all_of(target_var), all_of(selected_vars))
  } else {
    test_df_filtered <- NULL
  }
  
  # Define the random forest model specification with tunable parameters
  rf_spec <- rand_forest(
    mtry = tune(),       # Number of variables randomly sampled at each split
    trees = tune(),      # Number of trees in the forest
    min_n = tune()       # Minimum node size
  ) %>%
    set_mode("classification") %>%
    set_engine(engine)
  
  # Create a workflow with the recipe and model
  rf_workflow <- workflow() %>%
    add_recipe(recipe_obj) %>%
    add_model(rf_spec)
  
  # Extract and finalize parameter set for tuning
  rf_params <- extract_parameter_set_dials(rf_spec) %>%
    finalize(train_df_filtered)
  A
  # Create Latin Hypercube sampling grid for efficient parameter space exploration
  set.seed(seed_val)
  rf_grid <- grid_latin_hypercube(
    rf_params,
    size = grid_size,
    variogram_range = var_range,
    original = TRUE
  )
  
  # Define cross-validation strategy
  set.seed(seed_val)
  cv_folds_obj <- vfold_cv(
    train_df_filtered, 
    v = cv_folds, 
    repeats = cv_repeats, 
    strata = sym(target_var)  # Stratified sampling preserves class distribution
  )
  
  # Define performance metrics
  rf_metrics <- metric_set(accuracy)
  
  # Set up parallel processing if specified
  if (!is.null(n_cores)) {
    if (n_cores > 1) {
      cl <- makeCluster(n_cores)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
      
      # Configure parallel backend for progressr
      if (show_progress) {
        plan(cluster, workers = cl)
      }
    }
  } else {
    registerDoParallel()
    
    # Configure sequential backend for progressr
    if (show_progress) {
      plan(sequential)
    }
  }
  
  # Set up progress reporting
  if (show_progress) {
    total_iterations <- nrow(rf_grid) * cv_folds * cv_repeats
    
    with_progress({
      # Create a progress bar
      p <- progressor(steps = total_iterations)
      
      # Set handler for progress bar
      handlers(global = TRUE)
      handlers("progress")
      
      # Configure control parameters to update progress
      ctrl <- control_grid(
        verbose = TRUE,
        allow_par = TRUE,
        extract = function(x) {
          p()  # Update progress bar
          NULL # Don't extract any specific data
        }
      )
      
      # Tune the model with grid search and progress tracking
      rf_tune_results <- rf_workflow %>%
        tune_grid(
          resamples = cv_folds_obj,
          grid = rf_grid,
          metrics = rf_metrics,
          control = ctrl
        )
    })
  } else {
    # Tune the model without progress tracking
    rf_tune_results <- rf_workflow %>%
      tune_grid(
        resamples = cv_folds_obj,
        grid = rf_grid,
        metrics = rf_metrics
      )
  }
  
  # Select the best hyperparameter configuration
  rf_best_params <- select_best(rf_tune_results)
  
  # Finalize the model with best parameters
  rf_final_spec <- finalize_model(rf_spec, rf_best_params)
  
  # Update workflow with the finalized model
  rf_final_workflow <- rf_workflow %>%
    update_model(rf_final_spec)
  
  # Fit the final model on the training set
  rf_final_fit <- rf_final_workflow %>%
    fit(data = train_df_filtered)
  
  # Evaluate on test set if provided
  if (!is.null(test_df_filtered)) {
    test_performance <- rf_final_fit %>%
      predict(test_df_filtered) %>%
      bind_cols(test_df_filtered) %>%
      metrics(truth = !!sym(target_var), estimate = .pred_class)
  } else {
    test_performance <- NULL
  }
  
  # Return results
  return(list(
    tuning_results = rf_tune_results,
    best_params = rf_best_params,
    final_workflow = rf_final_workflow,
    final_fit = rf_final_fit,
    test_performance = test_performance
  ))
}

#----- Function 4: Generate SHAP values and feature importance -------

# This function calculates SHAP (SHapley Additive exPlanations) values
# to interpret the random forest model and visualize feature importance.

calculate_shapley_importance <- function(model_fit, test_df, target_var = "target", 
                                         n_samples = 50, seed_val = 1995) {
  
  # Load required packages
  pacman::p_load(DALEXtra, forcats, ggplot2)
  
  # Create explainer object for the model
  explainer <- explain_tidymodels(
    model = model_fit,
    data = test_df %>% select(-all_of(target_var)),
    y = test_df[[target_var]]
  )
  
  # Calculate SHAP values
  set.seed(seed_val)
  shap_values <- predict_parts(
    explainer = explainer,
    new_observation = test_df %>% select(-all_of(target_var)),
    type = "shap",
    B = n_samples  # Number of samples for approximation
  )
  
  # Calculate average SHAP values per variable
  average_shapley <- shap_values %>% 
    group_by(variable) %>% 
    summarise(contribution = mean(contribution)) %>% 
    arrange(desc(abs(contribution)))
  
  # Prepare data for visualization
  shap_plot_data <- shap_values %>%
    group_by(variable) %>%
    mutate(mean_val = mean(contribution)) %>%
    ungroup() %>%
    mutate(variable = fct_reorder(variable, abs(mean_val)))
  
  # Create feature importance plot
  shap_plot <- shap_plot_data %>%
    ggplot(aes(contribution, variable, fill = mean_val > 0)) +
    geom_col(data = ~distinct(., variable, mean_val), 
             aes(mean_val, variable), 
             alpha = 0.5) +
    geom_boxplot(width = 0.5) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
      y = NULL, 
      x = "SHAP Value (impact on model output)",
      title = "Feature Importance Based on SHAP Values",
      subtitle = "Features are ordered by mean absolute SHAP value"
    )
  
  # Return results
  return(list(
    explainer = explainer,
    shap_values = shap_values,
    average_shapley = average_shapley,
    plot_data = shap_plot_data,
    importance_plot = shap_plot
  ))
}


## --- OAXACA -----

#-------------- Oaxaca Bar Chart --------------

# Function to create a bar chart of the Oaxaca-Blinder decomposition from the oax_decomp object
plot_oaxaca_gap <- function(oax_decomp, weight_index = 6,
                            model = "OLS with all predictors", 
                            outcome = "Intention") {
  # Extract the total gap directly from the oax_decomp object
  y_diff <- oax_decomp$y$y.diff
  
  # Extract the decomposition components
  explained_gap <- oax_decomp$twofold$overall[weight_index, 2]
  unexplained_gap <- oax_decomp$twofold$overall[weight_index, 4]
  
  # Create a dataframe with the values of the total gap and its components
  gap_decomposition <- data.frame(
    component = c("Total SES Gap \nto Explain", "Explained", "Unexplained"),
    value = c(y_diff, explained_gap, unexplained_gap)
  )
  
  # Calculate the percentage of each component relative to the total gap
  gap_decomposition$percentage <- round(abs(gap_decomposition$value) / abs(gap_decomposition$value[1]) * 100, 1)
  gap_decomposition$label <- paste0(round(gap_decomposition$value, 3), " (", gap_decomposition$percentage, "%)")
  
  # For the total gap, just display the value without percentage
  gap_decomposition$label[1] <- as.character(round(gap_decomposition$value[1], 3))
  
  # Create a bar chart to visualize the decomposition
  p <- ggplot(gap_decomposition, aes(x = factor(component, levels = c("Total SES Gap \nto Explain", "Explained", "Unexplained")), 
                                     y = value, fill = component)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = label, y = value - 0.01 * sign(value)), 
              vjust = ifelse(gap_decomposition$value < 0, 1.5, -0.5), 
              fontface = "bold") +
    scale_fill_manual(values = c("Total Gap" =  "#D55E00" , "Explained" = "#0072B2", "Unexplained" = "#CC79A7")) +
    theme_minimal() +
    labs(
      title = paste0("SES gap decomposition in Early Childcare ", outcome),
      subtitle = paste0("Based on Oaxaca-Blinder decomposition. Model: weighted", ", ",
                        model),
      x = "",
      y = "Gap magnitude",
      caption = ""
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0),
      plot.subtitle = element_text(hjust = 0)
    )
  
  return(p)
}

#--------------- Waterfall Chart Top Contributors ---------------
# Function to create a chart of the variables contributing most to the explained gap

plot_top_contributors <- function(oax_decomp, 
                                  outcome = "Intention",
                                  model = "OLS with all predictors",
                                  weight_index = 6, 
                                  top_n = 25, 
                                  var_labels = NULL) {
  # Extract the results with the specific weight
  results_weight <- oax_decomp$twofold$variables[[weight_index]]
  
  # Create a dataframe with the explained component
  explained_df <- data.frame(
    variable = rownames(results_weight),
    explained = results_weight[, "coef(explained)"],
    se_explained = results_weight[, "se(explained)"]
  )
  
  # Remove the intercept
  explained_df <- explained_df[explained_df$variable != "(Intercept)", ]
  
  # Sort by absolute magnitude of the explained component
  explained_df <- explained_df[order(abs(explained_df$explained), decreasing = TRUE), ]
  
  # Select the top N variables
  top_vars <- head(explained_df, top_n)
  
  # If a name dictionary is provided, use it to rename the variables
  if (!is.null(var_labels)) {
    top_vars$clean_name <- var_labels[top_vars$variable]
    # Use the original name if not found in the dictionary
    top_vars$clean_name[is.na(top_vars$clean_name)] <- top_vars$variable[is.na(top_vars$clean_name)]
  } else {
    top_vars$clean_name <- top_vars$variable
  }
  
  # Add a factor column for colors based on the order of variables
  top_vars$var_factor <- factor(seq_len(nrow(top_vars)))
  
  # Create the chart with Dark2 color palette for each bar
  p <- ggplot(top_vars, aes(x = reorder(clean_name, abs(explained)), y = explained, fill = var_factor)) +
    geom_col() +
    geom_errorbar(aes(ymin = explained - 1.96*se_explained, 
                      ymax = explained + 1.96*se_explained), 
                  width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = cbPalette) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste0("Variables explaining the SES gap \nin Early Childcare ", outcome),
      subtitle = paste0(top_n, " Top Endowments component. \nModel: weighted, ", model),
      x = "",
      y = "Contribution to gap"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      plot.title = element_text(hjust = 0, size = 12, face = "bold", ),
      plot.subtitle = element_text(hjust = 0),
      legend.position = "none"  # Hide the legend for var_factor
    )
  
  return(p)
}

# Define a custom color palette with at least 15 distinct colors
# This combines colors from viridis palette, which works well for categorical data
#-------- Uneplained oaxaca --------
plot_top_unexplained_contributors <- function(oax_decomp, 
                                              weight_index = 6,
                                              top_n = 25, 
                                              outcome = "Intention",
                                              model = "OLS with all predictors",
                                              var_labels = NULL) {
  # Extract the results with the specific weight
  results_weight <- oax_decomp$twofold$variables[[weight_index]]
  
  # Create a dataframe with the unexplained component
  unexplained_df <- data.frame(
    variable = rownames(results_weight),
    unexplained = results_weight[, "coef(unexplained)"],
    se_unexplained = results_weight[, "se(unexplained)"]
  )
  
  # Remove the intercept
  unexplained_df <- unexplained_df[unexplained_df$variable != "(Intercept)", ]
  
  # Sort by absolute magnitude of the unexplained component
  unexplained_df <- unexplained_df[order(abs(unexplained_df$unexplained), decreasing = TRUE), ]
  
  # Select the top N variables
  top_vars <- head(unexplained_df, top_n)
  
  # If a name dictionary is provided, use it to rename the variables
  if (!is.null(var_labels)) {
    top_vars$clean_name <- var_labels[top_vars$variable]
    # Use the original name if not found in the dictionary
    top_vars$clean_name[is.na(top_vars$clean_name)] <- top_vars$variable[is.na(top_vars$clean_name)]
  } else {
    top_vars$clean_name <- top_vars$variable
  }
  
  # Add a factor column for colors based on the order of variables
  top_vars$var_factor <- factor(seq_len(nrow(top_vars)))
  
  # Create the chart with Dark2 color palette for each bar
  p <- ggplot(top_vars, aes(x = reorder(clean_name, abs(unexplained)), y = unexplained, fill = var_factor)) +
    geom_col() +
    geom_errorbar(aes(ymin = unexplained - 1.96*se_unexplained, 
                      ymax = unexplained + 1.96*se_unexplained), 
                  width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = cbPalette) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste0("Variables explaining the SES gap \nin Early Childcare ", outcome),
      subtitle = paste0(top_n, " Top Coefficient components. \nModel: weighted, ", model),
      x = "",
      y = "Contribution to gap"
    ) +
    theme(
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "none"  # Hide the legend for var_factor
    )
  
  return(p)
}

#-------------- Oaxaca Pooled Coefficients Table --------------
# Function to create a summary table of pooled coefficients and mean differences
oaxaca_pooled_summary_table <- function(oaxaca_object,
                                        pooled = c("pooled.1", "pooled.2"),
                                        coef_names = NULL,
                                        digits_beta = 3,
                                        digits_mean = 3) {
  pooled <- match.arg(pooled)
  
  get_coef_vec <- function(x) {
    if (is.null(x)) return(setNames(numeric(0), character(0)))
    if (is.numeric(x) && !is.null(names(x))) return(x)
    if (inherits(x, c("lm", "glm"))) return(stats::coef(x))
    if (is.list(x) && !is.null(x$coefficients)) {
      co <- x$coefficients
      if (is.numeric(co) && !is.null(names(co))) return(co)
    }
    if (is.list(x)) {
      nums <- x[vapply(x, function(z) is.numeric(z) && length(z) == 1, logical(1))]
      if (length(nums) > 0) return(unlist(nums, use.names = TRUE))
    }
    stop("Impossible d'extraire des coefficients (format inattendu).")
  }
  
  beta <- get_coef_vec(oaxaca_object$reg[[paste0("reg.", pooled)]])
  mean_diff <- oaxaca_object$x$x.mean.diff
  
  df <- data.frame(
    variable = names(beta),
    beta = as.numeric(beta),
    mean_diff = as.numeric(mean_diff[names(beta)]),
    stringsAsFactors = FALSE
  )
  
  # Drop Educ_bin + intercept
  df <- df[df$variable != "Educ_bin" & df$variable != "(Intercept)", , drop = FALSE]
  
  # Add human-readable labels if provided
  df$label <- if (!is.null(coef_names)) {
    unname(ifelse(df$variable %in% names(coef_names), coef_names[df$variable], df$variable))
  } else {
    df$variable
  }
  
  # Reorder columns + rounding
  df <- df[, c("label", "variable", "beta", "mean_diff")]
  df$beta <- round(df$beta, digits_beta)
  df$mean_diff <- round(df$mean_diff, digits_mean)
  
  # If flextable is available, return a nice Word-ready table
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(
      ft,
      label = "Variable",
      variable = "Code",
      beta = "β (pooled)",
      mean_diff = "Mean(A) − Mean(B)"
    )
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, align = "left", part = "all")
    return(ft)
  }
  
  # Fallback: return data.frame (you can kable it)
  df
}

#-------------- Oaxaca Control-Only Significance Table --------------
# Function to test significance of per-variable contributions (explained or
# unexplained component) from a twofold KOB decomposition, e.g. for the
# control-arm-only robustness checks.
oaxaca_control_significance_table <- function(oaxaca_object,
                                              component = c("explained", "unexplained",
                                                            "unexplained_A", "unexplained_B"),
                                              variables_index = 6,   # verify this matches your group.weight of interest
                                              coef_names = NULL,
                                              digits = 3,
                                              alpha = 0.05) {
  component <- match.arg(component)
  
  col_map <- list(
    explained     = c(coef = "coef(explained)",       se = "se(explained)"),
    unexplained   = c(coef = "coef(unexplained)",      se = "se(unexplained)"),
    unexplained_A = c(coef = "coef(unexplained A)",    se = "se(unexplained A)"),
    unexplained_B = c(coef = "coef(unexplained B)",    se = "se(unexplained B)")
  )
  cc <- col_map[[component]]
  
  raw <- oaxaca_object$twofold$variables[[variables_index]]
  
  df <- as.data.frame(raw) %>%
    tibble::rownames_to_column("variable") %>%
    dplyr::filter(variable != "(Intercept)") %>%
    dplyr::transmute(
      variable,
      coef = .data[[cc["coef"]]],
      se   = .data[[cc["se"]]]
    ) %>%
    dplyr::mutate(
      z           = coef / se,
      p_value     = 2 * (1 - pnorm(abs(z))),
      ci_low      = coef - qnorm(1 - alpha / 2) * se,
      ci_high     = coef + qnorm(1 - alpha / 2) * se,
      stars       = dplyr::case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE            ~ ""
      )
    ) %>%
    dplyr::arrange(dplyr::desc(abs(coef)))
  
  df$label <- if (!is.null(coef_names)) {
    unname(ifelse(df$variable %in% names(coef_names), coef_names[df$variable], df$variable))
  } else {
    df$variable
  }
  
  out <- df %>%
    dplyr::transmute(
      Variable   = label,
      Code       = variable,
      Coefficient = round(coef, digits),
      SE          = round(se, digits),
      `95% CI`    = sprintf("[%.*f, %.*f]", digits, ci_low, digits, ci_high),
      `p-value`   = sprintf("%.3f", p_value),
      Sig         = stars
    )
  
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(out) %>%
      flextable::autofit() %>%
      flextable::align(align = "left", part = "all") %>%
      flextable::add_footer_lines(
        paste0("Component: ", component, ". * p<0.10, ** p<0.05, *** p<0.01. ",
               "Wald-type CIs from bootstrap SEs (oaxaca package).")
      )
    return(ft)
  }
  
  out
}
#-------------- Oaxaca threefold  --------------
plot_oaxaca_threefold <- function(decomp_obj) {
  library(ggplot2)
  library(dplyr)
  library(patchwork) # For combining multiple plots
  
  # Extract data from decomposition object
  overall <- decomp_obj$threefold$overall
  variables <- decomp_obj$threefold$variables
  
  # 1. Create overall decomposition plot
  endow <- overall["coef(endowments)"]
  coef <- overall["coef(coefficients)"]
  inter <- overall["coef(interaction)"]
  total <- endow + coef + inter
  
  # Create dataframe for overall plot
  overall_df <- data.frame(
    component = c("Total Gap", "Endowments", "Coefficients", "Interaction"),
    value = c(total, endow, coef, inter)
  ) %>%
    mutate(
      percentage = (value / abs(total)) * 100,
      label = sprintf("%.3f (%.1f%%)", value, percentage)
    )
  
  # Generate overall decomposition plot
  p1 <- ggplot(overall_df, aes(x = component, y = value, fill = component)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = label), vjust = ifelse(overall_df$value < 0, 1.5, -0.5)) +
    labs(title = "SES gap decomposition",
         subtitle = "Oaxaca-Blinder 3-fold decomposition",
         y = "Gap magnitude", x = "") +
    theme_minimal() +
    scale_fill_manual(values = cbPalette[1:4]) +
    theme(legend.position = "none")
  
  # 2. Function to create individual component plots
  create_component_plot <- function(component_name, fill_color) {
    var_names <- rownames(variables)
    
    # Create dataframe for this component
    component_df <- data.frame(
      variable = var_names,
      contribution = variables[, paste0("coef(", component_name, ")")],
      se = variables[, paste0("se(", component_name, ")")]
    ) %>%
      filter(variable != "(Intercept)") %>%
      # Calculate 95% confidence intervals
      mutate(
        lower = contribution - 1.96 * se,
        upper = contribution + 1.96 * se
      ) %>%
      arrange(contribution)  # Sort by contribution size
    
    # Add a factor variable for color mapping
    component_df$var_factor <- factor(seq_len(nrow(component_df)))
    
    # Generate plot for this component
    ggplot(component_df, aes(x = reorder(variable, contribution), y = contribution, fill = var_factor)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      coord_flip() +  # Horizontal bars for better readability
      labs(title = paste("Variables Contributing to", component_name, "Component"),
           x = "", y = "Contribution to gap") +
      theme_minimal() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
      scale_fill_manual(values = cbPalette) +  # Use cbPalette colors
      theme(legend.position = "none")  # Hide the legend
  }
  
  # Create individual plots for each component
  p2 <- create_component_plot("endowments", cbPalette[2])
  p3 <- create_component_plot("coefficients", cbPalette[3])
  p4 <- create_component_plot("interaction", cbPalette[4])
  
  # Return all plots as a list
  return(list(
    overall = p1,
    endowments = p2,
    coefficients = p3,
    interaction = p4
  ))
}

##############################################
########### SHAP FOR Regularization #########
##################################################
#------------------ SHAP --------------------------------------------------
calculate_shapley_importance_lasso <- function(lasso_model, test_df, target_var = "target", 
                                               n_samples = 50, seed_val = 1995) {
  
  # Load required packages
  pacman::p_load(DALEXtra, forcats, ggplot2, dplyr)
  
  # Create explainer object for the LASSO model
  # For LASSO models from tidymodels workflow, we need to extract the fit
  if(inherits(lasso_model, "workflow")) {
    model_fit <- lasso_model$fit$fit$fit
    
    # Create explainer
    explainer <- DALEX::explain(
      model = model_fit,
      data = test_df %>% select(-all_of(target_var)) %>% as.data.frame(),
      y = test_df[[target_var]],
      type = "regression",
      label = "LASSO"
    )
  } else if(inherits(lasso_model, "lm")) {
    # If it's a standard lm object (post-LASSO model)
    explainer <- DALEX::explain(
      model = lasso_model,
      data = test_df %>% select(-all_of(target_var)) %>% as.data.frame(),
      y = test_df[[target_var]],
      type = "regression", 
      label = "LASSO"
    )
  } else {
    stop("Model type not supported. Please provide a tidymodels workflow or lm object.")
  }
  
  # Calculate SHAP values
  set.seed(seed_val)
  
  # For LASSO models, use a subset of observations to speed up computation
  # and make it more manageable
  n_obs <- min(100, nrow(test_df))  # Use at most 100 observations
  sample_idx <- sample(1:nrow(test_df), n_obs)
  
  # Calculate SHAP values for the sampled observations
  shap_values <- DALEX::predict_parts(
    explainer = explainer,
    new_observation = test_df[sample_idx, ] %>% select(-all_of(target_var)),
    type = "shap",
    B = n_samples  # Number of samples for approximation
  )
  
  # Calculate average SHAP values per variable
  average_shapley <- shap_values %>% 
    group_by(variable) %>% 
    summarise(contribution = mean(abs(contribution))) %>%  # Use absolute values for LASSO
    arrange(desc(contribution))
  
  # Prepare data for visualization
  shap_plot_data <- shap_values %>%
    group_by(variable) %>%
    mutate(mean_val = mean(contribution)) %>%
    ungroup() %>%
    mutate(variable = fct_reorder(variable, abs(mean_val)))
  
  # Create feature importance plot
  shap_plot <- shap_plot_data %>%
    ggplot(aes(contribution, variable, fill = contribution > 0)) +
    geom_col(data = ~distinct(., variable, mean_val), 
             aes(abs(mean_val), variable), 
             alpha = 0.5) +
    geom_boxplot(width = 0.5) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
      y = NULL, 
      x = "SHAP Value (impact on model output)",
      title = "Feature Importance for LASSO Model Based on SHAP Values",
      subtitle = "Features are ordered by mean absolute SHAP value"
    )
  
  # Create a dependence plot function for exploring individual feature effects
  dependence_plot <- function(variable_name) {
    DALEX::model_profile(
      explainer, 
      variables = variable_name
    ) %>% 
      plot() + 
      ggtitle(paste("Effect of", variable_name, "on predictions"))
  }
  
  # Return results
  return(list(
    explainer = explainer,
    shap_values = shap_values,
    average_shapley = average_shapley,
    plot_data = shap_plot_data,
    importance_plot = shap_plot,
    dependence_plot = dependence_plot
  ))
}

# 1. Basic SHAP importance plot 
# -------------------------------------
plot_shap_importance <- function(shap_results) {
  # Extract the average SHAP values
  avg_shap <- shap_results$average_shapley
  
  # Clean variable names for better display
  avg_shap$variable_clean <- gsub(" = .*", "", avg_shap$variable)
  
  # Create the importance plot
  importance_plot <- ggplot(avg_shap, aes(x = reorder(variable_clean, contribution), y = contribution)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      x = "Variable",
      y = "Mean |SHAP Value|",
      title = "Variable Importance Based on SHAP Values",
      subtitle = "Higher values indicate stronger impact on model prediction"
    ) +
    theme_minimal()
  
  return(importance_plot)
}

#--------------- Enhanced SHAP importance plot----------------------
# Shows variables sorted by absolute importance just as above but colored by direction
plot_shap_importance_with_direction <- function(shap_results) {
  # Extract the average SHAP values
  avg_shap <- shap_results$average_shapley
  
  # Clean variable names for better display
  avg_shap$variable_clean <- gsub(" = .*", "", avg_shap$variable)
  
  # Add absolute contribution for sorting
  avg_shap$abs_contribution <- abs(avg_shap$contribution)
  
  # Create the importance plot using explicit colors
  # This avoids relying on the result of contribution > 0
  importance_plot <- ggplot(avg_shap, 
                            aes(x = reorder(variable_clean, abs_contribution), 
                                y = contribution)) +
    geom_col(aes(fill = contribution > 0)) +
    # Force the color scale to include both possibilities
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), 
                      name = "Contribution", 
                      labels = c("FALSE" = "Négative", "TRUE" = "Positive")) +
    coord_flip() +
    labs(
      x = "Variable",
      y = "SHAP Value",
      title = "Variable Importance Based on SHAP Values",
      subtitle = "Variables classées par importance absolue, couleur indiquant la direction"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(importance_plot)
}



# 2. Fixed waterfall plot using average_shapley
# ----------------------------------------------------------------------------------------
plot_shap_waterfall <- function(shap_results, top_n = 10) {
  # Use the average_shapley dataframe
  contributions_df <- shap_results$average_shapley
  
  # Clean variable names
  contributions_df$variable_clean <- gsub(" = .*", "", contributions_df$variable)
  
  # Take the top N most important features
  if (nrow(contributions_df) > top_n) {
    top_vars <- contributions_df[1:top_n, ]
  } else {
    top_vars <- contributions_df
  }
  
  # Create the waterfall plot
  waterfall_plot <- ggplot(top_vars, aes(x = reorder(variable_clean, contribution), y = contribution)) +
    geom_col(aes(fill = contribution > 0)) +
    scale_fill_manual(values = c("firebrick", "steelblue"), 
                      name = "Impact", 
                      labels = c("Negative", "Positive")) +
    coord_flip() +
    labs(
      x = "Variable",
      y = "SHAP Importance Value",
      title = paste("Top", top_n, "Features by SHAP Importance"),
      subtitle = "Higher values indicate stronger impact on predictions"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(waterfall_plot)
}

# 3. SHAP values distribution from the shap_values structure
# ------------------------------------------------------------
plot_shap_values_distribution <- function(shap_results, top_n = 10) {
  # Extract SHAP values and convert to dataframe
  shap_vals <- as.data.frame(shap_results$shap_values)
  
  # Extract the median as a measure of central tendency
  var_importance <- data.frame(
    variable = rownames(shap_vals),
    importance = abs(shap_vals$median), 
    direction = sign(shap_vals$median)
  )
  
  # Clean variable names
  var_importance$variable_clean <- gsub("LASSO: ", "", var_importance$variable)
  var_importance$variable_clean <- gsub(" = .*", "", var_importance$variable_clean)
  
  # Sort by importance and take top N
  var_importance <- var_importance[order(-var_importance$importance), ]
  if (nrow(var_importance) > top_n) {
    var_importance <- var_importance[1:top_n, ]
  }
  
  # Create the plot
  importance_plot <- ggplot(var_importance, 
                            aes(x = reorder(variable_clean, importance), 
                                y = importance, 
                                fill = direction > 0)) +
    geom_col() +
    scale_fill_manual(values = c("firebrick", "steelblue"), 
                      name = "Direction", 
                      labels = c("Negative", "Positive")) +
    coord_flip() +
    labs(
      x = "Variable",
      y = "SHAP Importance (absolute value)",
      title = paste("Top", top_n, "Features by SHAP Importance"),
      subtitle = "Variables with highest impact on model predictions"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(importance_plot)
}

# 4. Improved SHAP dependence plot that works with your data structure
# --------------------------------------------------------------------
plot_shap_dependence <- function(shap_results, variable_name, debug = FALSE) {
  # First try to use the plot_data if available
  if ("plot_data" %in% names(shap_results)) {
    plot_data <- shap_results$plot_data
    
    # Find variable data that contains the specified variable name
    matching_vars <- grep(paste0("^", variable_name), as.character(plot_data$variable), value = TRUE)
    
    if (length(matching_vars) == 0) {
      matching_vars <- grep(variable_name, as.character(plot_data$variable), value = TRUE)
      
      if (length(matching_vars) == 0) {
        warning(paste("Variable", variable_name, "not found in plot_data"))
        return(NULL)
      }
    }
    
    if (debug) {
      message("Matching variables found: ", paste(matching_vars, collapse=", "))
    }
    
    # Filter data for the specified variable
    var_data <- plot_data %>%
      filter(variable %in% matching_vars)
    
    if (nrow(var_data) == 0) {
      warning("No data found for the specified variable")
      return(NULL)
    }
    
    if (debug) {
      message("Number of rows in filtered data: ", nrow(var_data))
      print(head(var_data))
    }
    
    # Try to extract the variable value from the variable string
    var_data$value <- NA
    
    # Extract from the variable string (format: "VariableName = Value")
    for (i in 1:nrow(var_data)) {
      var_string <- as.character(var_data$variable[i])
      if (grepl("=", var_string)) {
        value_str <- trimws(gsub(".*=\\s*", "", var_string))
        var_data$value[i] <- as.numeric(value_str)
      }
    }
    
    # If all values are NA, create an index
    if (all(is.na(var_data$value))) {
      if (debug) message("Using observation index as x-axis")
      var_data$value <- 1:nrow(var_data)
    }
    
    # Create a basic scatter plot
    p <- ggplot(var_data, aes(x = value, y = contribution)) +
      geom_point(alpha = 0.7, color = "steelblue", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
      labs(
        x = variable_name,
        y = "SHAP Value",
        title = paste("SHAP Dependence Plot for", variable_name),
        subtitle = "How variable values affect predictions"
      ) +
      theme_minimal()
    
    # Add a smoother if we have enough points
    if (length(unique(na.omit(var_data$value))) >= 4) {
      p <- p + geom_smooth(method = "loess", se = TRUE, color = "darkblue", formula = y ~ x)
    }
    
    return(p)
  } else {
    # Alternative approach using shap_values
    if (!"shap_values" %in% names(shap_results)) {
      warning("Neither plot_data nor shap_values found in shap_results")
      return(NULL)
    }
    
    # Extract variable name pattern to find in rownames
    pattern <- paste0("^LASSO: ", variable_name)
    
    # Find matching variable
    shap_df <- as.data.frame(shap_results$shap_values)
    row_idx <- grep(pattern, rownames(shap_df))
    
    if (length(row_idx) == 0) {
      warning(paste("Variable", variable_name, "not found in shap_values"))
      return(NULL)
    }
    
    # Extract the variable data
    var_info <- rownames(shap_df)[row_idx]
    var_shap <- shap_df[row_idx, , drop = FALSE]
    
    # Extract value from variable name
    value <- as.numeric(gsub(".*= ", "", var_info))
    
    # Create a simple dot plot
    var_data <- data.frame(
      variable = variable_name,
      value = value,
      min = var_shap$min,
      q1 = var_shap$q1,
      median = var_shap$median,
      mean = var_shap$mean,
      q3 = var_shap$q3,
      max = var_shap$max
    )
    
    # Create plot showing the distribution
    p <- ggplot(var_data) +
      geom_pointrange(aes(x = value, y = median, ymin = min, ymax = max), 
                      color = "steelblue", size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
      labs(
        x = variable_name,
        y = "SHAP Value",
        title = paste("SHAP Value Distribution for", variable_name),
        subtitle = "Range of SHAP values for this variable"
      ) +
      theme_minimal()
    
    return(p)
  }
}

# 5. Compare LASSO coefficients with SHAP importance
# --------------------------------------------------
# Compares LASSO coefficients with SHAP importance
plot_shap_vs_coefficients <- function(shap_results, lasso_model) {
  # Get SHAP importance values
  shap_importance <- shap_results$average_shapley
  
  # Get model coefficients
  if (inherits(lasso_model, "lm")) {
    coefs <- broom::tidy(lasso_model) %>%
      filter(term != "(Intercept)")
  } else if (inherits(lasso_model, "workflow")) {
    coefs <- broom::tidy(lasso_model$fit$fit$fit) %>%
      filter(term != "(Intercept)")
  } else {
    stop("Unsupported model type. Please provide an lm object or tidymodels workflow.")
  }
  
  # Clean variable names in both datasets
  shap_importance$variable_clean <- gsub(" = .*", "", shap_importance$variable)
  coefs$term_clean <- gsub("_[^_]*$", "", coefs$term)
  
  # Merge the datasets
  # This is a bit tricky because the variable names might not match exactly
  comparison_data <- data.frame()
  
  for (shap_var in unique(shap_importance$variable_clean)) {
    # Find the closest matching coefficient
    matching_coef <- coefs[grep(shap_var, coefs$term), ]
    
    if (nrow(matching_coef) > 0) {
      # Take the first match if multiple
      matching_coef <- matching_coef[1, ]
      
      # Get the SHAP value
      shap_val <- shap_importance$contribution[shap_importance$variable_clean == shap_var]
      
      # Add to comparison data
      comparison_data <- rbind(comparison_data, data.frame(
        variable = shap_var,
        shap_importance = shap_val,
        coefficient = abs(matching_coef$estimate),
        coefficient_raw = matching_coef$estimate
      ))
    }
  }
  
  # Create a scatter plot comparing SHAP vs coefficients
  if (nrow(comparison_data) > 0) {
    comparison_plot <- ggplot(comparison_data, aes(x = coefficient, y = shap_importance)) +
      geom_point(aes(color = coefficient_raw > 0), size = 3, alpha = 0.7) +
      geom_text(aes(label = variable), vjust = -0.5, hjust = 0.5, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "darkgray", linetype = "dashed") +
      scale_color_manual(values = c("firebrick", "steelblue"), 
                         name = "Coefficient Sign", 
                         labels = c("Negative", "Positive")) +
      labs(
        x = "Absolute Coefficient Value",
        y = "SHAP Importance",
        title = "SHAP Importance vs LASSO Coefficients",
        subtitle = "Comparing statistical significance with predictive importance"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    return(comparison_plot)
  } else {
    stop("No matching variables found between SHAP results and model coefficients")
  }
}

# 5. Alternative simple SHAP plot - works with limited data
# --------------------------------------------------------
plot_shap_simple <- function(shap_results, top_n = 10) {
  # Use the average_shapley which should always be available
  if (!"average_shapley" %in% names(shap_results)) {
    stop("average_shapley not found in shap_results")
  }
  
  # Get the data
  avg_shap <- shap_results$average_shapley
  
  # Clean variable names
  avg_shap$variable_clean <- gsub(" = .*", "", avg_shap$variable)
  
  # Take top N variables
  if (nrow(avg_shap) > top_n) {
    avg_shap <- avg_shap[1:top_n, ]
  }
  
  # Create a colorful barchart
  p <- ggplot(avg_shap, aes(x = reorder(variable_clean, contribution), y = contribution)) +
    geom_col(aes(fill = contribution), show.legend = FALSE) +
    scale_fill_gradient(low = "steelblue", high = "firebrick") +
    coord_flip() +
    labs(
      x = NULL,
      y = "SHAP Importance",
      title = "Variable Importance from SHAP Analysis",
      subtitle = paste("Top", top_n, "most influential variables")
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(size = 10, face = "bold")
    )
  
  return(p)
}
# 4. Improved SHAP dependence plot (works with plot_data structure)
# --------------------------------------------------------------------
plot_shap_dependence <- function(shap_results, variable_name, debug = FALSE) {
  # First try to use the plot_data if available
  if ("plot_data" %in% names(shap_results)) {
    plot_data <- shap_results$plot_data
    
    # Find variable data that contains the specified variable name
    matching_vars <- grep(paste0("^", variable_name), as.character(plot_data$variable), value = TRUE)
    
    if (length(matching_vars) == 0) {
      matching_vars <- grep(variable_name, as.character(plot_data$variable), value = TRUE)
      
      if (length(matching_vars) == 0) {
        warning(paste("Variable", variable_name, "not found in plot_data"))
        return(NULL)
      }
    }
    
    if (debug) {
      message("Matching variables found: ", paste(matching_vars, collapse=", "))
    }
    
    # Filter data for the specified variable
    var_data <- plot_data %>%
      filter(variable %in% matching_vars)
    
    if (nrow(var_data) == 0) {
      warning("No data found for the specified variable")
      return(NULL)
    }
    
    if (debug) {
      message("Number of rows in filtered data: ", nrow(var_data))
      print(head(var_data))
    }
    
    # Try to extract the variable value from the variable string
    var_data$value <- NA
    
    # Extract from the variable string (format: "VariableName = Value")
    for (i in 1:nrow(var_data)) {
      var_string <- as.character(var_data$variable[i])
      if (grepl("=", var_string)) {
        value_str <- trimws(gsub(".*=\\s*", "", var_string))
        var_data$value[i] <- as.numeric(value_str)
      }
    }
    
    # If all values are NA, try using variable_value if it exists
    if (all(is.na(var_data$value)) && "variable_value" %in% colnames(var_data)) {
      if (debug) message("Using variable_value column")
      var_data$value <- as.numeric(var_data$variable_value)
    }
    
    # If still all values are NA, create an index
    if (all(is.na(var_data$value))) {
      if (debug) message("Using observation index as x-axis")
      var_data$value <- 1:nrow(var_data)
    }
    
    # Create a basic scatter plot
    p <- ggplot(var_data, aes(x = value, y = contribution)) +
      geom_point(alpha = 0.7, color = "steelblue", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
      labs(
        x = variable_name,
        y = "SHAP Value",
        title = paste("SHAP Dependence Plot for", variable_name),
        subtitle = "How variable values affect predictions"
      ) +
      theme_minimal()
    
    # Add a smoother if we have enough points
    if (length(unique(na.omit(var_data$value))) >= 4) {
      p <- p + geom_smooth(method = "loess", se = TRUE, color = "darkblue", formula = y ~ x)
    }
    
    return(p)
  } else {
    warning("No plot_data found in shap_results")
    return(NULL)
  }
}
