load(file.path("cache", "loan_mf.RData"))
train_ind <- sample(1:nrow(loan_mf), size = floor(0.7*nrow(loan_mf)))
formula_columns <- names(loan_mf)[!str_detect(names(loan_mf), "state_zip") &
                                        !(names(loan_mf) %in% c("grade", "sub_grade", "X.Intercept."))]
column_centers <- data.frame(loan_mf[train_ind,formula_columns] %>%
                                   summarize_if(is.numeric, funs(median(., na.rm = TRUE))),
                             loan_mf[train_ind,formula_columns] %>%
                                   summarize_if(is.factor, funs(names(table(.))[which.max(table(.))]))) %>%
      as_tibble()
column_centers <- column_centers[,match(names(loan_mf[, formula_columns]),
                                        names(column_centers))]

which_missing <- which(is.na(loan_mf[, formula_columns]), arr.ind = TRUE)
for (this_column in unique(which_missing$col)){
      
}
train_mf <- loan_mf[train_ind,]
test_mf <- loan_mf[-train_ind,]
rm(loan_mf)
train_mf <- na.roughfix(train_mf)
formula_columns <- names(train_mf)[!(names(train_mf) %in%
                                           c("grade", "sub_grade", "X.Intercept."))]
registerDoParallel(cores=detectCores(all.tests=TRUE))
tmp.dir.forest <- '/tmp/Rforest'
unlink(tmp.dir.forest, recursive = TRUE)
grade_rf <- bigrfc(x = train_mf,
                   y = train_mf$grade,
                   ntree=100L,
                   varselect = match(formula_columns,
                                     names(train_mf)),
                   cachepath=tmp.dir.forest,
                   trace = 2)
