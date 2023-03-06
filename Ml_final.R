library(tidyverse)
library(gmodels)
dim(bank_ds)
attach(bank_ds)
summary(bank_ds)
head(bank_ds)
CrossTable(term_deposit)

#----------conversion of dependent variable into factor------#
bank_ds = bank_ds %>% 
  mutate(term_deposit = factor(if_else(term_deposit == "yes", "1", "0"), 
                    levels = c("0", "1")))
# checking for missing values 
library(Amelia)
missmap(bank_ds)
#--------data says it has unknown values which we need to check-----#
bank_ds %>% 
  summarise_all(list(~sum(. == "unknown"))) %>% 
  gather(key = "variable", value = "nr_unknown") %>% 
  arrange(-nr_unknown)
# Exploratory analysis #
library(corrplot)
#Age
summary(c_age)
library(ggplot2)
ggplot(bank_ds, aes(x=c_age, color=term_deposit)) +
  geom_histogram(fill="white", position="dodge",bins = 60)+
  geom_vline(xintercept = c(30, 60), 
             col = "blue",
             linetype = "dashed")+theme(legend.position="top")

bank_ds %>% 
  mutate(age60 = if_else(c_age >30, "1", "0")) %>% 
  group_by(term_deposit) %>% 
  add_count(nr_term_deposit = n()) %>% 
  group_by(age60, term_deposit) %>% 
  summarise(abs_freq = n(),
            relative_freq = round(100*n()/first(nr_term_deposit), 2))
#-------convert age variable to category --------#
bank_ds = bank_ds %>% 
  mutate(c_age = if_else(c_age > 60, "high", if_else(c_age > 30, "mid", "low")))

CrossTable(bank_ds$c_age,bank_ds$term_deposit)

age_plot <- ggplot(bank_ds, aes(term_deposit))
age_plot + geom_bar(aes(fill=c_age), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Customer age groups by deposit") + scale_fill_manual(values=c('#FFCC00','#3399FF',"#CCFF00"))


#------job------#
table(bank_ds$c_job)
CrossTable(bank_ds$c_job,bank_ds$term_deposit)
bank_ds = bank_ds %>% 
  filter(c_job != "unknown")
job_plot <- ggplot(bank_ds, aes(c_job))
job_plot + geom_bar(aes(fill=term_deposit), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Job profiles by term deposit") + scale_fill_manual(values=c('#FFCC00','#3399FF',"#CCFF00"))

#--------marital status--------#
table(bank_ds$c_marital)
CrossTable(bank_ds$c_marital,bank_ds$term_deposit)
bank_ds = bank_ds %>% 
  filter(c_marital != "unknown")
marital_plot <- ggplot(bank_ds, aes(c_marital))
marital_plot + geom_bar(aes(fill=term_deposit), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="marital by term deposit") + scale_fill_manual(values=c('#999999','#E69F00'))

#--------------Education---------------------#
table(bank_ds$c_education)
CrossTable(bank_ds$c_education,bank_ds$term_deposit)

bank_ds = bank_ds %>% 
  filter(c_education != "illiterate")
bank_ds = bank_ds %>% 
  mutate(c_education = recode(c_education, "unknown" = "university.degree"))

education_plot <- ggplot(bank_ds, aes(c_education))
education_plot + geom_bar(aes(fill=term_deposit), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Education by Term Deposit") + scale_fill_manual(values=c('#999999','#3399FF'))

#--------housing--------#
housing_plot <- ggplot(bank_ds, aes(c_housing))
housing_plot + geom_bar(aes(fill=term_deposit), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Housing Loan by Term Deposit") + scale_fill_manual(values=c('#999999','#3399FF'))

CrossTable(c_housing,term_deposit)
chisq.test(c_housing,term_deposit)

#Point of contact#
CrossTable(mode_of_contact,term_deposit)
mode_plot <- ggplot(bank_ds, aes(mode_of_contact))
mode_plot + geom_bar( width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Mode of contact in the campagin") + scale_fill_manual(values=c('#33FF33'))

#month of contact
CrossTable(month,term_deposit)
month_plot <- ggplot(bank_ds, aes(month))
month_plot + geom_bar(aes(fill=term_deposit), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Month by Term Deposit") + scale_fill_manual(values=c('#999999','#33FF33'))

#------------Campagin-------#

campagin_plot <- ggplot(bank_ds, aes(campaign))
campagin_plot + geom_bar(aes(fill=term_deposit), width = 0.8) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Campagin by Term Deposit") +geom_vline(xintercept = c(6, 15), 
                                                     col = "blue",
                                                     linetype = "dashed") +scale_fill_manual(values=c('#999999','#33FF33'))
bank_ds = bank_ds %>% 
  filter(campaign <= 8)
bank_ds = bank_ds %>% 
  mutate(campaign = as.character(campaign))

#--------pdays-------#
table(pdays)
attach(bank_ds)
bank_ds = bank_ds %>% 
  mutate(pdays_temp = if_else(pdays == 999, "0", "1")) %>% 
  select(-pdays)
CrossTable(pdays_temp,term_deposit)
#---------outcome of term deposit------#
CrossTable(poutcome,term_deposit)
poutcome_plot <- ggplot(bank_ds, aes(poutcome))
poutcome_plot + geom_bar(aes(fill=term_deposit), width = 0.8) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="poutcome by Term Deposit") +geom_vline(xintercept = c(6, 15), 
                                                     col = "blue",
                                                     linetype = "dashed") +scale_fill_manual(values=c('#999999','#ff6600'))
#------Bivariate analysis of social economic variables------#
#correlation of social economic variables
bank_ds %>% 
  select(cons.price.idx, cons.conf.idx, euribor3m, nr.employed) %>% 
  cor() %>% 
  corrplot(method = "number",
           type = "full",
           tl.cex = 0.7,
           tl.srt = 40,
           tl.col = "Blue")

# ML Modelling
#Data preparation
#First we need to reorder the levels of the data 
bank_ds$c_age <- factor(bank_ds$c_age)
bank_ds$c_job <- factor(bank_ds$c_job)
bank_ds$c_marital <- factor(bank_ds$c_marital)
bank_ds$c_education <- factor(bank_ds$c_education)
bank_ds$default <- factor(bank_ds$default)
bank_ds$c_housing <- factor(bank_ds$c_age)
bank_ds$loan<-factor(bank_ds$loan)
bank_ds$mode_of_contact<-factor(bank_ds$mode_of_contact)
bank_ds$month<-factor(bank_ds$month)
bank_ds$day_of_week<-factor(bank_ds$day_of_week)
bank_ds$campaign<-factor(bank_ds$campaign)
bank_ds$poutcome<-factor(bank_ds$poutcome)
bank_ds$pdays_temp<-factor(bank_ds$pdays_temp)
#--------exclude unimportant variables------#
bank_ds = bank_ds %>% select(-duration)
bank_ds = bank_ds %>% select(-emp.var.rate)
bank_ds = bank_ds %>% select(-c_housing)
bank_ds = bank_ds %>% select(-default)
bank_ds = bank_ds %>% select(-loan)

#-------Data Preparation-----#
library(caret)

set.seed(1234)
bank.training.samples <- term_deposit %>% 
  createDataPartition(times = 1, p = 0.8, list = FALSE)
bank.train.data  <- bank_ds[bank.training.samples, ]
bank.test.data <- bank_ds[-bank.training.samples, ]

#------logistic regression models-----#

#Model 1
lr.model1 <- glm( term_deposit ~., data = bank.train.data, family = binomial)
summary(lr.model1)

install.packages("ranger")
library(ranger)
logistic = glm(term_deposit ~ .,
               data = bank_ds,
               family = "binomial")
summary(logistic)

#feature importance function of model

feat_imp_fun_ = function(model){
  if (class(model)[1] == "ranger"){
    imp_df = model$variable.importance %>% 
      data.frame("Overall" = .) %>% 
      rownames_to_column() %>% 
      rename(variable = rowname) %>% 
      arrange(-Overall)
  } else {
    imp_df = varImp(model) %>%
      rownames_to_column() %>% 
      rename(variable = rowname) %>% 
      arrange(-Overall)
  }
  
  # (half most important variables)
  gg1 = imp_df %>% 
    slice(1:floor(nrow(.)/2)) %>% 
    ggplot() +
    aes(x = reorder(variable, Overall), weight = Overall, fill = -Overall) +
    geom_bar() +
    coord_flip() +
    xlab("Variables") +
    ylab("Importance") +
    theme(legend.position = "none")
  
  imp_range = ggplot_build(gg1)[["layout"]][["panel_params"]][[1]][["x.range"]]
  imp_gradient = scale_fill_gradient(limits = c(-imp_range[2], -imp_range[1]),
                                     low = "#132B43", 
                                     high = "#56B1F7")
  
  #  (less important variables)
  gg2 = imp_df %>% 
    slice(floor(nrow(.)/2)+1:nrow(.)) %>% 
    ggplot() +
    aes(x = reorder(variable, Overall), weight = Overall, fill = -Overall) +
    geom_bar() +
    coord_flip() +
    xlab("") +
    ylab("Importance") +
    theme(legend.position = "none") +
    ylim(imp_range) +
    imp_gradient
  
  # combining together
  gg_both = plot_grid(gg1 + imp_gradient,
                      gg2)
  return(gg_both)
}

# plotting important features
library(ggpubr)
library(cowplot)
feat_imp_fun_(lr.model1)
feat_imp_fun_(logistic)

#Predicted scores

lm1_train_score = predict(logistic,
                               newdata = bank.train.data,
                               type = "response")

lm1_test_score = predict(logistic,
                              newdata = bank.test.data,
                              type = "response")

#performance measure function
fun_cut = function(score, obs, measure1, measure2) 
  {predictions = prediction(score, obs)
  performance1 = performance(predictions, measure1)
  performance2 = performance(predictions, measure2)
  
  df1 = data.frame(x = performance1@x.values[[1]],
                   y = performance1@y.values[[1]],
                   measure = measure1,
                   stringsAsFactors = F) %>% 
    drop_na()
  df2 = data.frame(x = performance2@x.values[[1]],
                   y = performance2@y.values[[1]],
                   measure = measure2,
                   stringsAsFactors = F) %>% 
    drop_na()
  
  # df contains all the data needed to plot both curves
  df = df1 %>% 
    bind_rows(df2)
  
  # extracting best cut for each measure
  y_max_measure1 = max(df1$y, na.rm = T)
  x_max_measure1 = df1[df1$y == y_max_measure1, "x"][1]
  
  y_max_measure2 = max(df2$y, na.rm = T)
  x_max_measure2 = df2[df2$y == y_max_measure2, "x"][1]
  
  txt_measure1 = paste("Best cut for", measure1, ": x =", round(x_max_measure1, 3))
  txt_measure2 = paste("Best cut for", measure2, ": x =", round(x_max_measure2, 3))
  txt_tot = paste(txt_measure1, "\n", txt_measure2, sep = "")
  
  # plotting both measures in the same plot, with some detail around.
  gg = df %>% 
    ggplot() +
    aes(x = x,
        y = y,
        colour = measure) +
    geom_line() +
    geom_vline(xintercept = c(x_max_measure1, x_max_measure2), linetype = "dashed", color = "gray") +
    geom_hline(yintercept = c(y_max_measure1, y_max_measure2), linetype = "dashed", color = "gray") +
    labs(caption = txt_tot) +
    theme(plot.caption = element_text(hjust = 0)) +
    xlim(c(0, 1)) +
    ylab("") +
    xlab("Threshold")
  
  return(gg)
}

fun_cut_pr_score = function(score, cut) {
  # score: predicted scores
  # cut: threshold for classification
  
  classes = score
  classes[classes > cut] = 1
  classes[classes <= cut] = 0
  classes = as.factor(classes)
  
  return(classes)  
}

# plotting predicting scores and measure cut
install.packages("ROCR")
library(ROCR)
ms_cut_train = fun_cut(lm1_train_score, bank.train.data$term_deposit, 
                              "acc", "f")
ms_cut_train +
  geom_vline(xintercept = c(0.2, 0.5), 
             linetype = "dashed")

# confusion matrix and statistics
logistic_train_cut = 0.2
logistic_train_class = fun_cut_pr_score(lm1_train_score, logistic_train_cut)
# matrix
logistic_train_confm = confusionMatrix(logistic_train_class, bank.train.data$term_deposit, 
                                       positive = "1",
                                       mode = "everything")
logistic_train_confm

# logistic regression model 2 (after removing non related variables)




