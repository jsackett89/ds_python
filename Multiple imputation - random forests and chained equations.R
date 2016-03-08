library(readr)
library(dplyr)
library(mice)
library(missForest)
library(randomForest)
library(nnet)

# Read in data set with missing values
missing <- read_csv('missing.csv')

# Reduce data set to only those that will be imputed
keeps <- c('vars_to_keep')

missing_reduced <- missing[, (names(missing) %in% keeps)]
str(missing_reduced)

# Convert categorical variables to factors. Remove unnecessary variables.
character_vars <- lapply(missing_reduced, class == 'character')
missing_reduced[, character_vars] <- lapply(missing_reduced[, character_vars], as.factor)

missing_reduced$var_2 <- NULL

# Assign row names to missing_reduced
row.names(missing_reduced) <- missing_reduced$id 
missing_reduced$id <- NULL

# Separate numeric and categorical data
nums <- sapply(missing_reduced, is.numeric)
nums['id'] <- TRUE
missing_num <- missing_reduced[, nums]
row.names(missing_num) <- missing_num$id

facts <- sapply(missing_reduced, is.factor)
facts['id'] <- TRUE
missing_fact <- missing_reduced[, facts]
row.names(missing_fact) <- missing_fact$id

# MissForest imputation
missForest_imp <- function(df, seed=NULL) {
  imps <- list()
  for (i in 1:10) {
    index <- df$id
    # Records to extract on each iteration.
    # N is the number of observations in the original df.
    partition_index <- sample(index, 'n/10')
    # Partition to impute
    partition <- df[df$id %in% partition_index, ]
    row.names(partition) <- partition$id
    # Remaining records to be used in subsequent iterations
    df <- df[!(df$id %in% partition_index), ]
    # Coerce to matrix
    partition_mat <- as.matrix(partition)
    # Drop id column from matrix for imputation
    partition_mat <- partition_mat[, -1]
    # Run missForest imputation, convert result to data frame
    partition_df_imp <- as.data.frame((missForest(partition_mat))[[1]])
    # Append data frame to list
    imps[[i]] <- partition_df_imp
  }
  # Rbind all list elements into final imputed data frame
  num_imp_df <<- do.call('rbind', imps)
}

# Impute all numeric variables
missForest_imp(missing_num)

# Add id var back to imputed data set, merge with categoricals
num_imp_df$id <- as.numeric(row.names(num_imp_df))
merge_1 <- merge(num_imp_df, missing_fact, by = 'id')
row.names(merge_1) <- merge_1$id
merge_1$id <- NULL

# Impute categorical variables, extract data set
merge_1_imp <- mice(merge_1, m = 5)
merge_1_imp_df <- complete(merge_1_imp)

# Sort original both original and imputed data sets by id
missing <- missing[with(missing, order(id)), ]
merge_1_imp_df <- merge_1_imp_df[with(merge_1_imp_df, order(id)), ]

# Add necessary columns back to data set
old_vars <- with(well, data.frame(unimputed_vars))

a <- cbind(merge_1_imp_df, old_vars)

# Write to csv
write_csv(a, 'imputed_data.csv')