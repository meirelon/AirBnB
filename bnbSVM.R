library(readr)
library(stringr)
library(caret)
library(car)
library(dplyr)
library(tidyr)
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
     
     labs <- c(paste(seq(lower, upper - by, by = by),
                     seq(lower + by - 1, upper - 1, by = by),
                     sep = sep),
               paste(upper, above.char, sep = ""))
     
     cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
         right = FALSE, labels = labs)
}

df_train = read_csv("train_users_2.csv")
df_train <- filter(df_train, country_destination == 'NDF' | country_destination == 'US')
labels = df_train['country_destination']
session_grouped <- read_csv("session_grouped.csv")
session_grouped <- session_grouped[,-c(1)]
df_train <- left_join(df_train, session_grouped, by = 'id')
df_train = df_train[-grep('country_destination', colnames(df_train))]

df_test = read_csv("test_users.csv")
df_test <- left_join(df_test, session_grouped , by = 'id')

df_all = rbind.data.frame(df_train,df_test)
# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]
# Create age bucket by decade
df_all$age_decade <- as.character(age.cat(as.numeric(df_all$age), lower = 0, upper = 100, by = 10))
df_all$age_decade[is.na(df_all$age_decade)] <- "30-39"
df_all$age <- NULL


df_all <- df_all %>% 
     unite(col = "device_loyalty", signup_app, first_device_type, first_browser) %>% 
     mutate(device_loyalty = as.factor(device_loyalty)) %>% 
     unite(col = "affiliate", affiliate_provider, first_affiliate_tracked) %>% 
     mutate(affiliate = as.factor(affiliate))

# replace missing values
df_all[is.na(df_all)] <- -1
# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = dac[,1]
df_all['dac_month'] = dac[,2]
df_all['dac_day'] = dac[,3]
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]

# split timestamp_first_active in year, month and day
df_all[,'tfa_year'] = substring(as.character(df_all[,'timestamp_first_active']), 1, 4)
df_all['tfa_month'] = substring(as.character(df_all['timestamp_first_active']), 5, 6)
df_all['tfa_day'] = substring(as.character(df_all['timestamp_first_active']), 7, 8)
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]

# clean Age by removing values
#df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1


#provide rudimentary feature selection
library(pbapply)
system.time(distnt <- pbapply(df_all, 2, n_distinct))
df_all <- df_all[, names(distnt[distnt != 1])]
system.time(remove_me <- nearZeroVar(df_all))
df_all <- df_all[, -remove_me]

train_set = df_all[df_all$id %in% df_train$id,]
y <- as.factor(labels$country_destination)
levels(y) <- c(0,1)
for(i in 1:ncol(train_set)){
     train_set[[i]] <- as.factor(train_set[[i]])
     }
train_set <- cbind.data.frame(train_set, y)
train_set$count <- as.numeric(train_set$count)
train_set$avg_sec_elapsed <- as.numeric(train_set$avg_sec_elapsed)
train_set$dac_year <- NULL
test_set = df_all[df_all$id %in% df_test$id,]
test_set$count <- as.numeric(test_set$count)
test_set$avg_sec_elapsed <- as.numeric(test_set$avg_sec_elapsed)
test_set <- as.data.frame(test_set)



library(gbm)
library(randomForest)
system.time(model <- gbm(y ~., data = train_set[,-1]))#, shrinkage = 0.05, interaction.depth = 3, n.minobsinnode = 11))
summary(model)


predictions <- t(as.data.frame(predict(model, newdata = test_set, n.trees = 100)))
rownames(predictions) <- c('AU','CA','DE','ES','FR','GB','IT', 'NDF','NL','other','PT', 'US')
library(pbapply)
predictions_top5 <- as.vector(pbapply(predictions, 2, function(x) names(sort(x)[12:8])))


ids <- NULL
ids <- as.character(sapply(test_set$id, function(x) rep(x, 5)))

submission <- NULL
submission$id <- ids
submission$country <- predictions_top5
submission <- as.data.frame(submission)
write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)