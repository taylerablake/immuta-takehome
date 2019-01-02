
load(file.path("cache", "loan.RData"))
load(file.path("cache", "loan_desc.RData"))
set.seed(666)

## remove any columns exhibiting no variability - they won't be of any use for model fitting.
loan <- loan[, -match(names(which(sapply(loan,
                                         function(x) length(unique(x))) == 1)),
                      names(loan))]

## The term column has whitespace on the left side of the entries, so removing any
## whitespace from all character variables.

loan <- loan %>%
      mutate_if(is.character, funs(str_trim(., side = "both")))

## The "joint" variables are missing for non-joint applications, so in order to use them,
## we need to impute the most sensical value. For individual applications, we'll set the joint income
## and joint dti to the individual income and dti of the applicant.
loan <- loan %>%
      mutate(annual_income_joint = case_when(application_type == "Joint App" ~ annual_inc_joint,
                                       application_type == "Individual" ~ annual_inc),
             dti_joint = case_when(application_type == "Joint App" ~ dti_joint,
                             application_type == "Individual" ~ dti)) %>%
      dplyr::select(-annual_inc_joint)
# loan$dti_joint[is.na(loan$dti_joint) & !is.na(loan$dti) & loan$application_type == "Joint App"] <-
#       loan$dti[is.na(loan$dti_joint) & !is.na(loan$dti) & loan$application_type == "Joint App"]

## The length of the loan should be treated as numeric because it is possible that 
## we need to predict the grade of a loan having some other term length. To make use of the
## information in the date columns, we can create a variable capturing the length of time
## between the event date and the date that the loan was issued.

loan <- loan %>%
      mutate(term_length_months = case_when(term == "36 months" ~ 36,
                                            term == "60 months" ~ 60),
             cred_tenure_years = as.numeric(issue_d - earliest_cr_line)/365.5,
             weeks_btwn_credit_pull_loan_issue = as.numeric(issue_d - last_credit_pull_d)/7) %>%
      dplyr::select(-c(last_credit_pull_d,
                       issue_d, 
                       earliest_cr_line,
                       term))

#################################################################################################################
## deal with missing values
#################################################################################################################

loan %>%
      group_by(title, grade) %>%
      dplyr::count() %>%
      ungroup %>%
      group_by(title) %>%
      mutate(p = n/sum(n),
             N = sum(n)) %>%
      ungroup %>%
      ggplot(., aes(x = title, y = p)) +
      geom_bar(aes(color = grade, fill = grade),
               stat = "identity") +
      theme_bw() +
      scale_color_tableau() +
      scale_fill_tableau() +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       vjust = 1),
            panel.grid = element_blank()) +
      geom_text(aes(x = title, y = 1.01, label = paste0("N = ", N)),
                angle = 90, size = 2, hjust = 0, vjust = 0.5) +
      scale_y_continuous(breaks = c(0, 1),
                         limits = c(0, 1.17)) +
      ylab("")

title <- loan$title
title[title == "Prescription Drug and Medical Costs"] <- "Medical expenses"
title[title == "new kitchen for momma!"] <- "Home improvement"
title[title %in% c("DebtC", "Credit Card/Auto Repair",
                   "Paying off higher interest cards & auto")] <- "Debt consolidation"
title[title %in% c("Pay off Lowes Card",
                   "New Baby and New House (CC Consolidate)")] <- "Credit card refinancing"
title[title %in% c("Trying to come back to reality!",
                   "Student Loan",
                   "Simple Loan Until Contract Is Completed",
                   "SAVE",
                   "odymeds",
                   "considerate",
                   "new day",
                   "Learning and training",
                   "Green loan")] <- "Other"
title[is.na(title)] <- "Other"
loan$title <- NULL
loan <- loan %>% mutate(title = title)
rm(title)

loan %>%
      group_by(title, grade) %>%
      dplyr::count() %>%
      ungroup %>%
      group_by(title) %>%
      mutate(p = n/sum(n),
             N = sum(n)) %>%
      ungroup %>%
      ggplot(., aes(x = title, y = p)) +
      geom_bar(aes(color = grade, fill = grade),
               stat = "identity") +
      theme_bw() +
      scale_color_tableau() +
      scale_fill_tableau() +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       vjust = 1),
            panel.grid = element_blank()) +
      geom_text(aes(x = title, y = 1.01, label = paste0("N = ", N)),
                angle = 90, size = 2.7, hjust = 0, vjust = 0.5) +
      scale_y_continuous(breaks = c(0, 1),
                         limits = c(0, 1.17)) +
      ylab("")


########################################################################################
########################################################################################
open_rv_12m <- loan$open_rv_12m
open_rv_24m <- loan$open_rv_24m
open_il_12m <- loan$open_il_12m
open_il_24m <- loan$open_il_24m
open_rv_12m[loan$mo_sin_rcnt_rev_tl_op > 12 &
                  is.na(open_rv_12m)] <- 0
open_rv_24m[loan$mo_sin_rcnt_rev_tl_op > 24 &
                  is.na(open_rv_24m)] <- 0
open_il_12m[loan$mths_since_rcnt_il > 12 &
                  is.na(open_il_12m)] <- 0
open_il_24m[loan$mths_since_rcnt_il > 24 &
                  is.na(open_il_24m)] <- 0
loan$open_rv_12m <- loan$open_rv_24m <- loan$open_il_24m <- loan$open_il_12m <- NULL
loan <- loan %>% mutate(open_rv_12m = open_rv_12m,
                        open_rv_24m = open_rv_24m,
                        open_il_12m = open_il_12m,
                        open_il_24m = open_il_24m)
rm(open_il_12m)
rm(open_il_24m)
rm(open_rv_12m)
rm(open_rv_24m)

########################################################################################
########################################################################################

loan %>%
      dplyr::summarise_all(funs(sum(is.na(.)))) %>%
      gather(key = column, value = N_missing) %>%
      arrange(desc(N_missing))

#################################################################################################################
## variable selection and linear dependencies
#################################################################################################################

## fico range captured by two variables, only one is necessary
loan %>% group_by(fico_range_low, fico_range_high) %>%
      dplyr::count() %>%
      ungroup %>%
      arrange(fico_range_low, fico_range_high) %>%
      mutate(fico_range = fico_range_high - fico_range_low) %>%
      print(n = Inf)
loan %>% group_by(last_fico_range_low, last_fico_range_high) %>%
      dplyr::count() %>%
      ungroup %>%
      arrange(last_fico_range_low, last_fico_range_high) %>%
      mutate(last_fico_range = last_fico_range_high - last_fico_range_low) %>%
      print(n = Inf)
loan <- loan %>%
      dplyr::select(-c(last_fico_range_low, fico_range_low))

complete_numeric_cols <-
      names(which(colSums(is.na(loan[,names(which(sapply(loan, is.numeric)))])) == 0))
complete_numeric_cols <- complete_numeric_cols[complete_numeric_cols != "id"]
linear_combos <-
      caret::findLinearCombos(as.matrix(loan[,complete_numeric_cols]))
linear_combos$linearCombos %>%
      lapply(., function(x){
            names(which(sapply(loan, is.numeric)))[x]            
      })
loan <- loan %>% dplyr::select(-c(funded_amnt, delinq_2yrs))

#################################################################################################################
## categorical variables with levels having very few observations should be
## eliminated or combined
#################################################################################################################

loan %>%
      group_by(purpose, grade) %>%
      dplyr::count() %>%
      ungroup %>%
      group_by(purpose) %>%
      mutate(p = n/sum(n),
             N = sum(n)) %>%
      ungroup %>%
      ggplot(., aes(purpose, p, fill = grade, color = grade)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      scale_color_tableau() +
      scale_fill_tableau() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      geom_text(aes(x = purpose, y = 1.01, label = paste0("N = ", N)),
                angle = 90, size = 2.7, hjust = 0, vjust = 0.5,
                inherit.aes = FALSE) +
      scale_y_continuous(breaks = c(0, 1),
                         limits = c(0, 1.17))

purpose <- loan$purpose
purpose[purpose %in% c("wedding", "educational")] <- "other"
loan$purpose <- NULL
loan <- loan %>% mutate(purpose = purpose)
rm(purpose)

loan$mths_since_last_delinq[loan$pct_tl_nvr_dlq == 100] <- -100
loan$mths_since_recent_revol_delinq[loan$pct_tl_nvr_dlq == 100] <- -100
######################################################################################
## to use zip code information, since 2 digits have been masked, we need to append
## state information to discriminate between two different zip codes which may
## share the same first three digits.
##
## Like other categorical variables, to control dimensionality and prevent
## overfitting, we need to aggregate levels having few observations. (Moreover,
## effects of zip codes with only a few loans may not even be estimable if few enough
## of them are included in the training set and we won't be able to make predictions 
## for loans in zip codes which do not appear in the training set.)
######################################################################################

zip_counts <- loan %>%
      group_by(zip_code) %>%
      dplyr::count() %>% ungroup %>%
      filter(n > 200)
zip_code <- loan$zip_code
zip_code[!(zip_code %in% zip_counts$zip_code)] <- "other"
loan$zip_code <- NULL
loan <- loan %>% mutate(zip_code = zip_code)

#################################################################################################################
## variable selection and linear dependencies
#################################################################################################################

correlationMatrix <- cor(loan[ ,names(which(sapply(loan, is.numeric)))],
                         use = "pairwise.complete.obs") 
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix,
                                    cutoff=0.8, names = TRUE)

melted_corr <- melt(correlationMatrix)
names(melted_corr) <- c("var1", "var2", "R")
melted_corr %>%
      ggplot(data = ., aes(var1, var2, fill = R)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_bw()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed() +
      xlab("") + ylab("") +
      theme(axis.text.x = element_text(size = 7))
melted_corr %>%
      filter((var1 %in% highlyCorrelated |
                    var2 %in% highlyCorrelated) &
                   R > 0.8 &
                   var1 != var2)

loan_mf <- loan %>% dplyr::select(-c(total_pymnt_inv,
                                  out_prncp_inv,
                                  funded_amnt_inv,
                                  total_pymnt,
                                  installment,
                                  url,
                                  id,
                                  avg_cur_bal,
                                  annual_income_joint,
                                  emp_title))
loan_mf <- loan_mf %>% mutate_if(is.character, funs(factor))
cache("loan_mf")





