test_na_roughfix <- function(impute_vars, impute_values, data){
      if (ncol(impute_values) != length(impute_vars)) {
            stop("Please provide value for each column to be imputed.")
      }
      if (any(!(impute_vars %in% names(data)))){
            stop("Names of all imputed variables must match names of input dataframe.")
      }
      imputed_data <- data
      for (this_var in impute_vars){
            fill_column <- match(this_var, names(data))
            if (!is.na(fill_column)){
                  imputed_data[is.na(imputed_data[, fill_column]), fill_column] <- impute_values[1,this_var]
            }
      }
      imputed_data
}