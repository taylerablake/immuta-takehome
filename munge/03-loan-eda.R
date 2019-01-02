
load(file.path("cache", "loan.RData"))
load(file.path("cache", "loan_desc.RData"))
loan <- loan %>%
      mutate_if(is.character, funs(str_trim(., side = "both")))
set.seed(666)
glimpse(loan)


loan %>%
      mutate_all(is.na) %>%
      summarize_all(sum) %>%
      gather(key = column, value = N_missing) %>%
      filter(N_missing > 0) %>%
      arrange(desc(N_missing)) %>%
      dplyr::select(column_name = column,
                    N_missing) %>%
      inner_join(., loan_desc) 
gg_df <- loan %>%
      mutate_all(is.na) %>%
      summarize_all(mean) %>%
      gather(key = column_name, value = prop_missing) %>%
      arrange(desc(prop_missing))
gg_df$column_name <- factor(gg_df$column_name, levels = gg_df$column_name)
gg_df %>%
      filter(prop_missing > 0) %>%
      ggplot(., aes(column_name, prop_missing)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab("variable") +
      ylab("proportion of missing values") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

loan %>% dplyr::select(c(contains("joint"), application_type)) %>%
      mutate_at(vars(contains("joint")), is.na) %>%
      group_by(annual_inc_joint, dti_joint,
               application_type) %>%
      dplyr::count()

incomplete_columns <- names(which(colSums(is.na(loan)) > 0))
NA_linear_combos <- caret::findLinearCombos(loan %>% dplyr::select(names(which(colSums(is.na(loan)) > 0))) %>%
                                                  mutate_all(is.na) %>%
                                                  as.matrix(.))

knitr::kable(loan %>% dplyr::select(inq_fi, application_type, grade) %>%
                   mutate(missing_inq_fi = is.na(inq_fi)) %>%
                   group_by(missing_inq_fi, application_type, grade) %>%
                   dplyr::count() %>%
                   ungroup %>%
                   arrange(grade, application_type, missing_inq_fi) %>%
                   group_by(grade) %>%
                   mutate(p = n/sum(n)) %>%
                   ungroup)

NA_linear_combos$linearCombos %>%
      lapply(., function(x){
            data.frame(column1 = incomplete_columns[x[1]],
                       column2 = incomplete_columns[x[2]])
      }) %>%
      ldply(., data.frame)

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################

loan <- loan[, -match(names(which(sapply(loan,
                                         function(x) length(unique(x))) == 1)),
                      names(loan))]
loan <- loan %>%
      mutate(annual_income = case_when(application_type == "Joint App" ~ annual_inc_joint,
                                       application_type == "Individual" ~ annual_inc),
             dti = case_when(application_type == "Joint App" ~ dti_joint,
                                       application_type == "Individual" ~ dti),
             max_dti = case_when(application_type == "Joint App" & (dti > dti_joint) ~ dti,
                                 application_type == "Joint App" & (dti <= dti_joint) ~ dti_joint,
                                 application_type == "Individual" ~ dti)) %>%
      mutate(cred_tenure_years = as.numeric(issue_d - earliest_cr_line)/365.5,
             weeks_since_last_credit_pull = as.numeric(issue_d - last_credit_pull_d)/7) %>%
      dplyr::select(-c(last_credit_pull_d, issue_d, 
                       earliest_cr_line, id, annual_inc,
                       dti_joint))

loan <- loan %>% filter(!is.na(bc_util) & !is.na(bc_open_to_buy) &
                      !is.na(percent_bc_gt_75) & !is.na(dti) & !is.na(revol_util) &
                            !is.na(dti) & !is.na(weeks_since_last_credit_pull))

state_zip_counts <- loan %>%
      group_by(addr_state, zip_code) %>%
      dplyr::count() %>% ungroup %>%
      group_by(addr_state) %>%
      mutate(N_zip = length(zip_code)) %>%
      ungroup %>%
      filter(N_zip > 1 & n > 100 & !(addr_state %in% c("RI", "HI", "DE")))
state_zip_counts <- state_zip_counts %>%
      mutate(state_zip = paste0(state_zip_counts$addr_state,
                                state_zip_counts$zip_code))

loan <- state_zip_counts %>%
      dplyr::select(addr_state,
                    zip_code,
                    state_zip) %>%
      full_join(., loan) %>%
      dplyr::select(-zip_code)
loan$state_zip[is.na(loan$state_zip)] <- "other"
loan$title[is.na(loan$title)] <- "not provided"
loan <- loan %>%
      dplyr::select(names(which(colSums(is.na(loan)) == 0)))




loan %>% mutate(bad_grade = (grade %in% c("D", "E", "F", "G")),
                state_zip = paste0(loan$addr_state, loan$zip_code)) %>%
      group_by(bad_grade, addr_state, state_zip) %>%
      dplyr::count() %>% ungroup %>%
      group_by(state_zip) %>% mutate(p = n/sum(n)) %>%
      group_by(addr_state) %>% mutate(N = sum(n)) %>% ungroup %>%
      filter(bad_grade == TRUE) %>%
      ggplot(., aes(addr_state, p)) +
      geom_boxplot() +
      geom_jitter(aes(color = n),alpha = 0.8) +
      theme_bw()+
      scale_color_continuous_tableau()

loan %>% mutate(bad_grade = (grade %in% c("D", "E", "F", "G")),
                state_zip = paste0(loan$addr_state, loan$zip_code)) %>%
      group_by(bad_grade, addr_state, state_zip) %>%
      dplyr::count() %>% ungroup %>%
      group_by(state_zip) %>% mutate(p = n/sum(n)) %>%
      group_by(addr_state) %>% mutate(N = sum(n)) %>% ungroup %>%
      filter(bad_grade == TRUE) %>% group_by(addr_state) %>%
      dplyr::summarise(min_zip_p = min(p),
                       max_zip_p = max(p),
                       range_p = max(p) - min(p),
                       N = max(N),
                       min_n = min(n),
                       max_n = max(n)) %>%
      ungroup %>%
      arrange(desc(range_p))

loan %>%
      group_by(addr_state, grade) %>%
      dplyr::count() %>% ungroup %>%
      group_by(addr_state) %>%
      mutate(N = sum(n), p = n/sum(n)) %>%
      ungroup %>%
      ggplot(., aes(addr_state, p, fill = grade,
                    color = grade)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      scale_color_tableau() +
      scale_fill_tableau()
gg_df <- loan %>%
      group_by(addr_state, zip_code, grade) %>%
      dplyr::count() %>% ungroup %>%
      group_by(addr_state, zip_code) %>%
      mutate(N = sum(n), p = n/sum(n)) %>%
      ungroup 
gg_df %>%
      mutate(state_zip = paste0(gg_df$addr_state, gg_df$zip_code)) %>%
      filter(N > 50) %>%
      ggplot(., aes(state_zip, p, fill = grade,
                    color = grade)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      scale_color_tableau() +
      scale_fill_tableau()

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################

train_ind <- sample(1:nrow(loan), size = floor(0.8*nrow(loan)))
loan_train <- loan[train_ind,]
loan_train %>% filter(application_type == "Joint App") %>%
      dplyr::select(grade, starts_with("annual_inc")) %>% 
      ggplot(., aes(x = annual_inc, annual_inc_joint)) +
      geom_point(aes(color = grade)) +
      theme_bw()

linear_combos <- caret::findLinearCombos(loan_train %>%
                                               dplyr::select(names(which(sapply(loan,
                                                                                is.numeric)))) %>%
                                               as.matrix(.))
linear_combos$linearCombos %>%
      lapply(., function(x){
            names(which(sapply(loan_train, is.numeric)))[x]            
      })
loan_train <- loan_train %>%
      dplyr::select(-funded_amnt)
loan_train <- loan_train %>%
      mutate(last_fico_range = last_fico_range_high - last_fico_range_low,
             fico_range = fico_range_high - fico_range_low,
             delta_fico_high = last_fico_range_high - fico_range_high,
             delta_fico_low = last_fico_range_low - fico_range_low)
fmla <- paste0(" ~ ",
               loan_train[,which(sapply(loan_train, is.character))] %>%
                     sapply(., function(x){
                           length(unique(x))
                     }) %>%
                     magrittr::is_less_than(50) %>%
                     which %>%
                     names %>%
                     paste(., collapse = " + "))
mainEffects <- dummyVars(fmla, data = loan_train) %>%
      predict(., newdata = loan_train)
mainEffects <- model.matrix( ~ addr_state + application_type +
                    grade + home_ownership +
                    initial_list_status + loan_status +
                    purpose + sub_grade +
                    term + verification_status,
              data = loan_train)
factor_linear_combos <- findLinearCombos(as.matrix(mainEffects))
factor_linear_combos$linearCombos %>%
      lapply(., function(x){
            dimnames(mainEffects)[[2]][x]            
      })
all_linear_combos <- findLinearCombos(cbind(mainEffects,
                           loan_train %>%
                                 dplyr::select(names(which(sapply(loan_train,
                                                                  is.numeric)))) %>%
                                 as.matrix(.)))

all_linear_combos$linearCombos %>%
      lapply(., function(x){
      c(names(which(sapply(loan_train,
                           is.numeric))),
        dimnames(mainEffects)[[2]])[x]            
})
c(names(which(sapply(loan_train,
                     is.numeric))),
  dimnames(mainEffects)[[2]])[all_linear_combos$remove]

near_zero_var <- nearZeroVar(loan_train %>%
                                   dplyr::select(names(which(sapply(loan_train,
                                                                    is.numeric)))) %>%
                                   as.matrix(.), freqCut = 200/1, uniqueCut = 2)
names(which(sapply(loan_train, is.numeric)))[near_zero_var]
loan %>%
      ggplot(., aes(acc_now_delinq)) +
      geom_histogram() +
      theme_bw()
loan %>% group_by(acc_now_delinq) %>%
      dplyr::count() %>%
      ungroup
loan %>% group_by(acc_now_delinq > 0, grade) %>%
      dplyr::count() %>%
      ungroup() %>%
      group_by(`acc_now_delinq > 0`) %>%
      mutate(p = n/sum(n)) %>%
      ungroup

loan %>%
      filter(delinq_amnt > 0) %>%
      ggplot(., aes(delinq_amnt)) +
      geom_histogram() +
      theme_bw()
loan$delinq_amnt %>% unique %>% sort
loan %>% group_by(delinq_amnt > 0, grade) %>%
      dplyr::count() %>%
      ungroup() %>%
      group_by(`acc_now_delinq > 0`) %>%
      mutate(p = n/sum(n)) %>%
      ungroup
loan %>%
      ggplot(., aes(chargeoff_within_12_mths)) +
      geom_histogram() +
      theme_bw()
loan %>% group_by(chargeoff_within_12_mths) %>%
      dplyr::count() %>%
      ungroup
loan %>% group_by(chargeoff_within_12_mths > 0, grade) %>%
      dplyr::count() %>%
      ungroup() %>%
      group_by(`chargeoff_within_12_mths > 0`) %>%
      mutate(p = n/sum(n)) %>%
      ungroup

loan %>%
      ggplot(., aes(acc_now_delinq, delinq_amnt, color = grade)) +
      geom_point() + theme_bw() + scale_color_tableau()
loan %>%
      mutate(positive_delinq_amnt = delinq_amnt > 0) %>%
      group_by(positive_delinq_amnt, acc_now_delinq) %>%
      dplyr::count() %>%
      ungroup
loan %>% filter(acc_now_delinq == 0 & delinq_amnt > 0) %>%
      ggplot(., aes(delinq_amnt)) +
      geom_histogram() + theme_bw()

names(which(sapply(loan_train,
                   is.numeric)))[checkConditionalX(x = loan_train %>%
                                                         dplyr::select(names(which(sapply(loan_train,
                                                                                          is.numeric)))),
                                                   y = loan_train$grade)]

correlationMatrix <- cor(loan_train %>%
                               dplyr::select(names(which(sapply(loan_train,
                                                                is.numeric))))) 
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix,
                                    cutoff=0.8,
                                    names = TRUE)

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

loan_train_rf <- loan_train %>%
      dplyr::select(-c(#num_rev_tl_bal_gt_0,
                       out_prncp_inv,
                       #recoveries,
                       #avg_cur_bal,
                       total_pymnt_inv,
                       funded_amnt_inv,
                       #installment,
                       #fico_range_low#,
                       url,
                       sub_grade
                       )) %>%
      mutate_if(is.character,factor)
loan_train_rf <- loan_train_rf %>% dplyr::select(-state_zip) %>%
      data.frame(., model.matrix(~ state_zip - 1, data = loan_train_rf))
loan_train_rf <- loan_train_rf %>%
      group_by(grade) %>%
      do(sample_n(., size = min(nrow(.), 10000))) %>%
      ungroup
grade_rf <- randomForest(as.formula(paste0("grade ~ ",
                                          paste(names(loan_train_rf %>% dplyr::select(-grade)),
                                                collapse = " + "))),
                        data=loan_train_rf,
                        importance=TRUE,
                        do.trace=TRUE,
                        ntree = 200)

importance(grade_rf)
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################

char_columns <- sapply(loan, is.character) %>%
      which %>% names %>%
      sapply(.,
             function(x){
                   length(unique(as.vector(data.frame(loan)[,x])))
             }) %>%
      magrittr::is_less_than(40) %>%
      which %>% names
char_columns
grid.arrange(loan %>%
      dplyr::select(loan_status,
                    purpose,
                    verification_status) %>%
      gather(key = column, value = value) %>%
      ggplot(., aes(x = value)) +
      geom_bar() + theme_bw() +
      facet_wrap(~column, nrow = 1, scales = "free") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1,
                                       vjust = 1, size = 6)) +
      xlab(""),
loan %>%
      dplyr::select(initial_list_status, grade,
                    sub_grade) %>%
      gather(key = column, value = value) %>%
      ggplot(., aes(x = value)) +
      geom_bar() + theme_bw() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1,
                                       size = 5)) +
      facet_wrap(~column, nrow = 1, scales = "free") +
      xlab(""),
loan %>%
      dplyr::select(application_type,
                    home_ownership, term, emp_length) %>%
      gather(key = column, value = value) %>%
      ggplot(., aes(x = value)) +
      geom_bar() + theme_bw() +
      facet_wrap(~column, nrow = 1, scales = "free") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1)) +
      xlab(""),
loan %>%
      ggplot(., aes(x = addr_state)) +
      geom_bar() + theme_bw() +
      xlab("") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1)),
ncol = 1, nrow = 4)
      

loan %>% dplyr::select(starts_with("num"))
loan %>% filter(revol_bal > tot_cur_bal)
loan %>% filter(num_rev_accts < num_op_rev_tl)
loan %>% filter(num_sats < num_bc_sats)
loan %>% filter(open_il_24m < open_il_12m)
loan %>% filter(open_rv_24m < open_rv_12m)
sum(loan$num_tl_op_past_12m > loan$open_acc)

loan %>%
      ggplot(., aes(x = fico_range)) +
      geom_histogram() +
      theme_bw()
loan %>%
      ggplot(., aes(x = fico_range)) +
      geom_histogram() +
      theme_bw()

gglist <- list()
for (this_column in names(which(sapply(loan[,-match("id", names(loan))],
                                       is.numeric)))) {
      gglist <- list.append(gglist,
                            ggplot(loan, aes_string(x = this_column)) +
                                  geom_histogram() +
                                  theme_bw() +
                                  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
}
marrangeGrob(gglist, nrow = 3, ncol = 3)

loan %>%
      mutate(year_earliest_cr_line = year(earliest_cr_line)) %>%
      group_by(grade, year_earliest_cr_line) %>%
      dplyr::count() %>%
      ungroup %>% 
      ggplot(., aes(year_earliest_cr_line, n)) +
      geom_point() +
      theme_bw() +
      facet_wrap(~grade, nrow = 2, scales = "free_y")
            
loan %>%
      ggplot(., aes(issue_d)) +
      geom_bar() +
      theme_bw() #+
      #facet_wrap(~grade, nrow = 2, scales = "free_y")


loan %>% filter(pub_rec > 30) %>% print(width = Inf)
loan %>% filter(tax_liens > 20) %>% print(width = Inf)
num_tl_30dpd
num_tl_90g_dpd_24m
tot_rev_hi_lim
total_il_high_credit_limit
tot_coll_amt
total_bc_limit

loan %>% filter(last_fico_range_low < 100) %>% print(n = Inf)
loan %>% sapply(., is.Date) %>% which

loan %>%
      dplyr::select(c(starts_with("mths"))) %>%
      gather(key = variable, value = value) %>%
      ggplot(., aes(x = value)) +
      geom_histogram() +
      theme_bw() +
      facet_wrap(~ variable, nrow = 4, scales = "free")
loan %>%
      dplyr::select(c(starts_with("mths"),
                      starts_with("mo_"))) %>%
      gather(key = variable, value = value) %>%
      filter(!is.na(value)) %>%
      ggplot(., aes(x = value)) +
      geom_histogram(binwidth = 5) +
      theme_bw() +
      facet_wrap(~ variable, nrow = 4, scales = "free")
loan %>%
      dplyr::select(c(starts_with("total"),
                      starts_with("tot_"))) %>%
      gather(key = variable, value = value) %>%
      filter(!is.na(value)) %>%
      ggplot(., aes(x = value)) +
      geom_histogram(binwidth = 5) +
      theme_bw() +
      facet_wrap(~ variable, nrow = 4, scales = "free")

ggpairs(loan_train[,which(sapply(loan, is.numeric))])


help("randomForest")
grade_rf


