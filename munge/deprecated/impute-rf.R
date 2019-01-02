load(file.path("cache", "loan_mf.RData"))
set.seed(666)
annual_inc <- loan_mf$annual_inc
train_ind <- sample(1:nrow(loan_mf),
                    size = floor(0.7*nrow(loan_mf)))
formula_columns <- names(loan_mf)[!(names(loan_mf) %in%
                                          c("grade", "sub_grade"))]
train_mf <- loan_mf[train_ind,]
test_mf <- loan_mf[-train_ind,]
rm(loan_mf)

imputed_unmasked_train_mf <- na.roughfix(train_mf)
column_centers <- data.frame(imputed_unmasked_train_mf[,formula_columns] %>%
                                   summarize_if(is.numeric, funs(median(., na.rm = TRUE))),
                             imputed_unmasked_train_mf[,formula_columns] %>%
                                   summarize_if(is.factor,
                                                funs(names(table(.))[which.max(table(.))]))) %>%
      as_tibble()

column_centers <- column_centers[,match(names(train_mf[, formula_columns]),
                                        names(column_centers))]
imputed_unmasked_test_mf <- test_na_roughfix(formula_columns,
                                             column_centers,
                                             test_mf)

unmasked_grade_rf <- ranger(formula = as.formula(paste0("grade ~ ",
                                   paste(formula_columns, collapse = " + "))),
       data = imputed_unmasked_train_mf,
       num.trees = 500,
       mtry = floor(sqrt(ncol(imputed_unmasked_train_mf) - sum(str_detect(names(imputed_unmasked_train_mf),
                                                                          "zip_code")))),
       importance = "impurity",
       write.forest = TRUE, replace = FALSE,
       class.weights = rep(1/nlevels(train_mf$grade),
                           nlevels(train_mf$grade)),
       keep.inbag = TRUE,
       min.node.size = 5,
       num.threads = 8, seed = 666,
       save.memory = FALSE,
       verbose = TRUE)
cache("unmasked_grade_rf")

importance_df <- data.frame(variable = names(unmasked_grade_rf$variable.importance),
                            importance = -unmasked_grade_rf$variable.importance) %>%
      arrange(desc(importance)) %>% as_tibble() %>%
      mutate(masking_level = "raw data")

importance_df$variable <- factor(importance_df$variable, levels = importance_df$variable)
importance_df %>%
      ggplot(., aes(x = variable, y = importance)) +
      geom_point() +
      geom_linerange(aes(ymin = 0, ymax = importance)) +
      theme_bw() +
      ylab("decrease in node purity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

unmasked_test_predictions <- predict(unmasked_grade_rf,
                                     data = imputed_unmasked_test_mf,
                                     predict.all = FALSE,
                                     seed = 666, num.threads = 8,
                                     verbose = TRUE)

confusion_matrix <- unmasked_grade_rf$confusion.matrix
test_error <- (as.matrix(confusion_matrix) -
                     diag(diag(as.matrix(confusion_matrix)))) %>%
      rowSums
test_error <- test_error/table(test_mf$grade)

gg_df <- data.frame(grade = levels(test_mf$grade),
           `Test Error` = as.vector(test_error),
           `predicted proportion` = data.frame(predictions = unmasked_test_predictions$predictions) %>%
                 group_by(predictions) %>%
                 dplyr::count() %>%
                 ungroup %>%
                 mutate(p = n/sum(n)) %>%
                 .$p,
           `observed proportion` = imputed_unmasked_test_mf %>%
                 group_by(grade) %>%
                 dplyr::count() %>%
                 ungroup %>%
                 mutate(p = n/sum(n)) %>%
                 .$p) %>%
      gather(key = metric, value = value, -grade) 

gg_df$metric <- factor(gg_df$metric,
                       levels = c("Test.Error", "observed.proportion","predicted.proportion" ),
                       labels = c("Test Error",
                                  "Observed proportion of loan",
                                  "Proportion predicted of loans"))
gg_df %>%
      ggplot(., aes(grade, value)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      facet_wrap(~ metric, nrow = 3)
