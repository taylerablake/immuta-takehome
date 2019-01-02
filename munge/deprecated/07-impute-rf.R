load(file.path("cache", "loan_mf.RData"))

set.seed(666)
train_ind <- sample(1:nrow(loan_mf),
                    size = floor(0.7*nrow(loan_mf)))
train_mf <- loan_mf[train_ind,]
test_mf <- loan_mf[-train_ind,]
annual_inc <- loan_mf$annual_inc
formula_columns <- names(loan_mf)[!(names(loan_mf) %in% c("grade", "sub_grade"))]
rm(loan_mf)

imputed_train_mf <- na.roughfix(train_mf)
column_centers <- data.frame(imputed_train_mf[,formula_columns] %>%
                                   summarize_if(is.numeric, funs(median(., na.rm = TRUE))),
                             imputed_train_mf[,formula_columns] %>%
                                   summarize_if(is.factor,
                                                funs(names(table(.))[which.max(table(.))]))) %>%
      as_tibble()
column_centers <- column_centers[,match(names(train_mf[, formula_columns]),
                                        names(column_centers))]
imputed_test_mf <- test_na_roughfix(formula_columns,
                                             column_centers,
                                             test_mf)

grade_rf <- ranger(formula = as.formula(paste0("grade ~ ",
                                               paste(formula_columns,
                                                     collapse = " + "))),
                   data = imputed_train_mf,
                   num.trees = 500,
                   importance = "impurity",
                   write.forest = TRUE,
                   replace = FALSE,
                   keep.inbag = TRUE,
                   min.node.size = 5,
                   num.threads = 8,
                   seed = 666,
                   save.memory = FALSE,
                   verbose = TRUE)
importance_df <- data.frame(variable = names(grade_rf$variable.importance),
                                             importance = grade_rf$variable.importance) %>%
                                        arrange(desc(importance)) %>%
                                        as_tibble() %>%
                                        mutate(masking_level = "raw data")
test_predictions <- data.frame(grade = imputed_test_mf$grade,
                               predicted_grade = predict(grade_rf,
                                                         data = imputed_test_mf,
                                                         predict.all = FALSE,
                                                         seed = 666, num.threads = 8,
                                                         verbose = TRUE)$predictions,
                               masking_level = "raw_data")

for (this_divisor in c(1, 100, 1000, 5000, 10000)){
      imputed_train_mf$annual_inc <- round(annual_inc[train_ind]/this_divisor)*this_divisor 
      imputed_test_mf$annual_inc <- round(annual_inc[-train_ind]/this_divisor)*this_divisor 
      
      grade_rf <- ranger(formula = as.formula(paste0("grade ~ ",
                                                     paste(formula_columns,
                                                           collapse = " + "))),
                         data = imputed_train_mf,
                         num.trees = 500,
                         importance = "impurity",
                         write.forest = TRUE,
                         replace = FALSE,
                         keep.inbag = TRUE,
                         min.node.size = 5,
                         num.threads = 8,
                         seed = 666,
                         save.memory = FALSE,
                         verbose = TRUE)
      
      importance_df <- rbind.data.frame(importance_df,
                                        data.frame(variable = names(grade_rf$variable.importance),
                                                   importance = grade_rf$variable.importance) %>%
                                              arrange(desc(importance)) %>%
                                              as_tibble() %>%
                                              mutate(masking_level = paste0("nearest ",
                                                                            as.character(this_divisor))))
      test_predictions <- rbind.data.frame(test_predictions,
                                           data.frame(grade = imputed_test_mf$grade,
                                     predicted_grade = predict(grade_rf,
                                                               data = imputed_test_mf,
                                                               predict.all = FALSE,
                                                               seed = 666, num.threads = 8,
                                                               verbose = TRUE)$predictions,
                                     masking_level = paste0("nearest ",
                                                            as.character(this_divisor))))

}

importance_df$masking_level <- factor(importance_df$masking_level,
                                      levels = unique(importance_df$masking_level),
                                      labels = c("no rounding",
                                                 "rounded, nearest dollar",
                                                 paste0("rounded, nearest ",
                                                        c(100, 1000, 5000, 10000), " dollars")))
importance_df$variable <- factor(importance_df$variable,
                                 levels = c("annual_inc",
                                            unique(importance_df$variable)[unique(importance_df$variable) != "annual_inc"]))

importance_df %>%
      ggplot(., aes(x = variable, y = importance)) +
      geom_point() +
      geom_linerange(aes(ymin = 0, ymax = importance)) +
      theme_bw() +
      ylab("variable importance") +
      xlab("masking level of annual_inc variable") +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.5,
                                       size = 5)) +
      facet_wrap(~ masking_level, nrow = 3) +
      geom_point(data = importance_df %>%
                       filter(variable == "annual_inc"),
                 color = "red") +
      geom_linerange(data = importance_df %>%
                           filter(variable == "annual_inc"),
                     color = "red",
                     aes(ymin = 0, ymax = importance))
importance_df %>%
      filter(variable == "annual_inc") %>%
      ggplot(., aes(x = masking_level, y = importance)) +
      geom_point() +
      geom_linerange(aes(ymin = 0, ymax = importance)) +
      theme_bw() +
      ylab("variable importance") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      xlab("masking level of annual_inc variable")

test_predictions %>%
      group_by(masking_level, grade) %>%
      dplyr::summarise(err = sum(grade != predicted_grade)/length(grade)) %>%
      ungroup %>%
      print(n = Inf)

test_predictions %>%
      group_by(masking_level) %>%
      dplyr::summarise(err = sum(grade != predicted_grade)/length(grade)) %>%
      ungroup %>%
      print(n = Inf)

confusion <- test_predictions %>%
      group_by(masking_level, grade, predicted_grade) %>%
      dplyr::count() %>%
      ungroup %>%
      rename(N = n, predicted = predicted_grade, observed = grade) %>%
      spread(key = predicted, value = N)
      


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






