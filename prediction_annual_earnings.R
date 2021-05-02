############# LOADING GLOBAL PACKAGES #############
# packages to load
lapply(c("dplyr", "lubridate", "tidyr", "pbapply", "purrr",
         "zoo", "tibble", "xts", "forecast", "season", "broom",
         "bigrquery", "stringr", "odbc", "devtools", "readxl",
         "multidplyr", "parallel", "magrittr", "car", "gtsummary",
         "Hmisc", "gridExtra", "ggplot2"), 
       function(x){require(x, character.only = T)})

############# LOADING TRAINING DATA #############

# Download training dataset
training_data <-  read.table(text = gsub('(\\d)\\n', '\\1 ', 
                               paste(readLines(url(paste0('https://archive.',
                               'ics.uci.edu/ml/machine-learning-databases/adult/adult.data'))), 
                                                         collapse = '\n'))) %>%
  dplyr::rename(., age = 1, workclass = 2, fnlwgt = 3, education = 4, 
                education_num = 5, marital_status = 6, occupation = 7,
                relationship = 8, race = 9, sex = 10, 
                capital_gain = 11, capital_loss = 12, hours_per_week = 13,
                native_country = 14, classifier = 15)

# Cleansing data 
training_data_cleansed <- training_data %>%
  mutate_all(., function(x){str_sub(x, 1, str_length(x)-1)}) %>%
  mutate_at(vars(age, fnlwgt, contains("capital")),
            function(x){as.numeric(x)}) %>%
  mutate_at(vars(education_num, hours_per_week),
            function(x){as.integer(x)}) %>%
  mutate(classifier = paste0(classifier, "K")) %>%
  # Changing classifier variable to factor due to only two levels based
  # on data dictionary
  mutate_at(vars(classifier), function(x){C(as.factor(x))})

# Inspecting number of levels for nominal data
str(training_data_cleansed %>%
      dplyr::select_if(is.character, function(x){x}) %>%
      mutate_all(., function(x){as.factor(x)}))

# Analyzing all summary data
Hmisc::describe(training_data_cleansed)

# Binning age, hours_per_week, education to produce groups
# Cleaning levels for marital status
training_data_cleansed <- training_data_cleansed %>%
  mutate(age_group = case_when(
    age <= 25 ~ "25 and below",
    age >25 & age < 31 ~ "26 - 30",
    age >= 31 & age < 40 ~ "31 - 39",
    age >= 41 & age < 50 ~ "41 - 49",
    age >= 51 & age < 60 ~ "51 - 59",
    age >= 61 & age < 70 ~ "61 - 69",
    TRUE ~ "70 and over"
  ) %>% as.factor()) %>%
  mutate(hours_group = case_when(
    hours_per_week <= 24 ~ "24 and below",
    hours_per_week >24 & hours_per_week <= 45 ~ "24 - 45",
    hours_per_week >= 46 & age <= 60 ~ "46 - 60",
    TRUE ~ "61 and over"
  ) %>% as.factor()) %>%
  mutate(educ_group = case_when(
    education %in% c('9th', '10th', '11th', '12th') ~ "Some High School",
    education %in% grepl("Assoc", education) ~ "Associates",
    education %in% c('1st-4th', '5th-6th', '7th-8th') ~ "Some Primary",
    TRUE ~ education
  ) %>% as.factor()) %>%
  mutate(marital_status = case_when(
    str_detect(marital_status, "Married") ~ "Married",
    TRUE ~ marital_status
  ) %>% as.factor()) %>%
  mutate_at(vars(education_num, sex), function(x){as.factor(x)})

############# DATA EXPLORATION #############

# Loading specific packages for exploration
lapply(c("GGally", "ggplot2"), 
       function(x){require(x, character.only = T)})

# Function to plot pairs of highly correlated continuous variables
sctplot_viz <- function(data, foldername, filename){
  
  # Declare directory where png file will be saved
  png(filename = paste0(getwd(),"/", deparse(substitute(foldername)),"/",
                         deparse(substitute(filename)),".png"))
  
  # Create ggpairs scatterplot
  print(ggpairs(data %>% dplyr::select_if(is.numeric, function(x){x})))
  
  # Shut-down graphical device
  dev.off()
}

# Deploy sctplot_viz function
# Age, fnlwgt, capital gain, capital loss appear to be left-skewed
sctplot_viz(data = training_data_cleansed, 
            foldername = other_projects, 
            filename = sctplot_viz)

# Function to review whether numerical vars are normalized
qqplot_viz <- function(data, folder_name, file_name){
  
  # Coerce name of data into character string
  name <- deparse(substitute(data))
  
  # Create character string of numerical variable names
  num_vars <- data %>%
    dplyr::select_if(is.numeric, function(x){x}) %>% names(.)
  
  # For loop to analyze individual qqplots for numerical vars
  for (i in num_vars) {
    
    # Coerce and evaluate cleansed training data and individual numerical vars
    sub_data <- eval(parse(text = paste0(name,'$',i)))
    
    #Declare directory where png file will be saved
    png(filename = paste0(getwd(),"/", deparse(substitute(folder_name)),"/",
                          deparse(substitute(file_name)),"_by_", i,".png"))
    
    # Deploy qqplot
    qqnorm(sub_data, pch = 1, frame = FALSE)
    qqline(sub_data, col = "steelblue", lwd = 2)
    
    dev.off()
  }
}

# Deploy qqplot function over dataset
qqplot_viz(training_data_cleansed, other_projects, qqplot)

# Review distribution of continuous variables by 
# qualitative/categorical variables via stacked bar chart
bar_viz <- function(data, cat_var_list, fill_list, 
                     folder_name, file_name){
  
  # For loop to perform stacked bar chart over categorical variables
  # via x-axis
  for (i in cat_var_list) {
    
    x_axis <- i
    
    # For loop to define how to fill stacked bar chart
    for (j in fill_list) {
      
      #Declare directory where png file will be saved
      png(filename = paste0(getwd(),"/", deparse(substitute(folder_name)),"/",
                            deparse(substitute(file_name)),"_", x_axis,
                            "_by_", j,".png"))
      
      print(ggplot(data, aes_string(x = x_axis, 
                                    fill = j)) + 
              geom_bar(position = "stack") + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1)))
      
      dev.off()
      
    }
  }
}

# Deploy stacked bar visualizations to produce individual charts
bar_viz(data = training_data_cleansed,
         cat_var_list = c("workclass", "marital_status",
                          "age_group", "hours_group"), 
         fill_list = c("race", "classifier", "sex"), 
         folder_name = other_projects, 
         file_name = hist_viz_plot)

############# PREDICTIVE MODELING #############

# Deploy packages supporting classification modeling approaches
lapply(c("rpart", "rpart.plot", "InformationValue", "ROSE",
         "pscl", "ROCR", "Epi"), 
       function(x){require(x, character.only = T)})

# Function applying classification models
class_models <- function(training_data, response, vars_list){
  
  # Apply classification tree  using control parameter
  model_tree <- rpart(as.formula(paste0(response," ~ .")),
                      data = training_data,
                      control = rpart.control(cp = 0.0001))
  
  # Determine value that minimizes cross-validation error
  bestcp <- model_tree$cptable[which.min(model_tree$cptable[,"xerror"]),"CP"]
  
  # Prune the tree using the best cp
  model_tree_pruned <- prune(model_tree, cp = bestcp)
  
  # Underfitting training data to accomodate specific predictors
  data_tbl <- training_data %>%
    dplyr::select(c(response, vars_list)) %>%
    mutate_if(is.character, function(x){as.factor(x)}) %>%
    mutate_if(is.factor, function(x){C(x)})
  
  # Apply logistic regression model
  model_log <- glm(as.formula(paste0(response, "~ .")), 
                   family = binomial(link = "logit"),
                   data = data_tbl)
  
  # Combining model results into working tibble structure
  model_tbl <- tibble(tree_model = list(model_tree_pruned),
                      log_model = list(model_log)) %>%
    pivot_longer(., cols = everything(), names_to = "model_type",
                 values_to = "model_fit")
  
  # Assigning tibble to global environment
  assign("model_tbl", model_tbl, envir = .GlobalEnv)
  
}

# Deploying classification models
# Global variable exported as "model_tbl"
class_models(training_data_cleansed, "classifier",
             c("age", "workclass", "educ_group",
               "age_group", "hours_group", "marital_status"))

############# LOADING TESTING DATA #############

# Download testing dataset
testing_data <-  read.table(text = gsub('(\\d)\\n', '\\1 ', 
                                         paste(readLines(url(paste0('https://archive.',
                                                                    'ics.uci.edu/ml/machine-learning-databases/adult/adult.test'))), 
                                               collapse = '\n')), fill = TRUE) %>%
  dplyr::rename(., age = 1, workclass = 2, fnlwgt = 3, education = 4, 
                education_num = 5, marital_status = 6, occupation = 7,
                relationship = 8, race = 9, sex = 10, 
                capital_gain = 11, capital_loss = 12, hours_per_week = 13,
                native_country = 14, classifier = 15)

# Cleansing testing data 
testing_data_cleansed <- testing_data %>%
  mutate_all(., function(x){str_sub(x, 1, str_length(x)-1)}) %>%
  mutate_at(vars(age, fnlwgt, contains("capital")),
            function(x){as.numeric(x)}) %>%
  mutate_at(vars(education_num, hours_per_week),
            function(x){as.integer(x)}) %>%
  # Changing classifier variable to factor due to only two levels based
  # on data dictionary
  mutate_at(vars(classifier), function(x){C(as.factor(x))})

# Review missing values in testing data -- Noting first row
summary(testing_data_cleansed)

# Cleansing testing data to account for factor variables
testing_data_cleansed_2 <- testing_data_cleansed %>%
  slice(2:n()) %>%
  # Applying group binning similar to training data
  mutate(age_group = case_when(
    age <= 25 ~ "25 and below",
    age >25 & age < 31 ~ "26 - 30",
    age >= 31 & age < 40 ~ "31 - 39",
    age >= 41 & age < 50 ~ "41 - 49",
    age >= 51 & age < 60 ~ "51 - 59",
    age >= 61 & age < 70 ~ "61 - 69",
    TRUE ~ "70 and over"
  ) %>% as.factor()) %>%
  mutate(hours_group = case_when(
    hours_per_week <= 24 ~ "24 and below",
    hours_per_week >24 & hours_per_week <= 45 ~ "24 - 45",
    hours_per_week >= 46 & age <= 60 ~ "46 - 60",
    TRUE ~ "61 and over"
  ) %>% as.factor()) %>%
  mutate(educ_group = case_when(
    education %in% c('9th', '10th', '11th', '12th') ~ "Some High School",
    education %in% grepl("Assoc", education) ~ "Associates",
    education %in% c('1st-4th', '5th-6th', '7th-8th') ~ "Some Primary",
    TRUE ~ education
  ) %>% as.factor()) %>%
  mutate(marital_status = case_when(
    str_detect(marital_status, "Married") ~ "Married",
    TRUE ~ marital_status
  ) %>% as.factor()) %>%
  mutate_at(vars(education_num, sex), function(x){as.factor(x)}) %>%
  mutate_at(vars(classifier), function(x){C(as.factor(str_squish(x)))}) %>%
  filter(classifier != "")
  
############# APPLYING TESTING DATA TO PRED MODELS #############

# Creating predictions from fitted classification models
pred_tbl <- model_tbl %>%
  bind_cols(., tibble(testing_data = list(testing_data_cleansed_2))) %>%
  mutate(predictions = map2(model_fit, testing_data, function(x, y){
    
    print(paste0("Evaluating Model:", class(x)))
    
    # Initiating predictions
    if(class(x) == "rpart"){
      tibble(predicted = predict(x, newdata = tibble(y), type = "class"))}else{
        tibble(predicted = predict(x, newdata = tibble(y), type = "response"))
      }
    
  })) %>%
  # Combining both testing data with predictions
  mutate(prediction_data = map2(testing_data, predictions, function(x,y){
    
    temp_tbl <- tibble(x) %>% bind_cols(tibble(y))
    
  })) %>% dplyr::select(-c(testing_data, predictions)) %>%
  mutate(prediction_data = map2(model_fit, prediction_data, function(x,y){
    
    print(paste0("Evaluating Model:", class(x)))
    
    # Converting probabilities into class predictions
    if(class(x) == "rpart"){tibble(y) %>%
        mutate(predicted = C(as.factor(predicted)))}else{
      
      optCutOff <- optimalCutoff(tibble(y$classifier), tibble(y$predicted))[1]  
      
      tibble(y) %>%
        mutate(predicted = ifelse(predicted > .5, ">50K", "<=50K")) %>%
        mutate(predicted = C(as.factor(predicted)))
      }
    
  }))

# Deploy analysis of testing data results
pred_tbl_results <- pred_tbl %>%
  mutate(analysis = map2(model_fit, prediction_data, function(x, y){
    
    print(paste0("Evaluating Model:", class(x)))
    
    cm <- table(y$classifier, y$predicted)
    
    # How often classifier is correct
    acc_rate <- round((cm[1,1]+cm[2,2])/nrow(y), digits = 5)
    
    # How often classifier is incorrect
    mis_rate <- round((cm[1,2]+cm[2,1])/nrow(y), digits = 5)
    
    # Reject the null hypothesis when TRUE (false non-disruptions/positives)
    type_1 <- round(cm[1,2]/sum(cm[1,]), digits = 5)
    
    # Fail to reject the null hypothesis when FALSE (false disruptions/negatives)
    type_2 <- round(cm[2,1]/sum(cm[2,]), digits = 5)
    
    # True positive rate
    true_pos_rate <- round(cm[2,2]/sum(cm[2,]), digits = 5)
    
    # True negative rate
    true_neg_rate <- round(cm[1,1]/sum(cm[1,]), digits = 5)
    
    # Declare directory where png file will be saved
    png(filename = paste0(getwd(),"/ROC_", class(x),".png"))
    # Plot ROC Curve
    plot(0,type='n',axes=FALSE,ann=FALSE)
    print(plot(performance(prediction(as.numeric(C(as.factor(y$predicted))), 
                                as.numeric(C(as.factor(y$classifier)))),
                     measure = "tpr", "fpr")))
    dev.off()
    
    # Determine AUC metrics
    auc <- performance(prediction(as.numeric(C(as.factor(y$predicted))), 
                                  as.numeric(C(as.factor(y$classifier)))),
                       measure = "auc")
    
    # Extract AUC score
    auc_score <- auc@y.values[[1]]
    
    # Assigning all metrics to working tibble
    metrics <- tibble(accuracy = acc_rate,
                      misclassification = mis_rate,
                      type_1_error = type_1,
                      type_2_error = type_2,
                      true_positives = true_pos_rate,
                      true_negatives = true_neg_rate,
                      auc = round(auc_score, digits = 5)) %>%
      pivot_longer(., cols = everything(),
                   names_to = "metrics", values_to = "amount")
    
    # Declare directory where png file will be saved
    png(filename = paste0(getwd(),"/metrics_", class(x),".png"))
    
    # Print final metric grid
    print(grid.arrange(tableGrob(metrics)))
    
    dev.off()
    
    return(metrics)
    
  }))


                               