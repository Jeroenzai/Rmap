# AUTHOR
# Jeroen van Buren
# Build on R version: 3.4.3

## Loading packages:
library(dplyr)
library(ggplot2)
library(caret)

## Novel pacakges:
#install.packages('ggmap')
library(ggmap) 
#install.packages('FSelector')
library(FSelector) #information.gain()
#install.packages('e1071')
library(e1071) #Naive Bayes
#install.packages('RColorBrewer')
library(RColorBrewer) #brewer.pal()

## Loading in raw dataset
terror_original <- read.csv("globalterrorismdb_0617dist.csv", 
  stringsAsFactors = FALSE, na.strings = c(-99, -9, "Unknown", "NA", ''))
## keep the orignal dataset and make a copy which will be used and adjusted
terror <- terror_original

## EXPLORATIVE BASICS
dim(terror)
head(terror)
str(terror)
summary(terror)
colnames(terror)

## CLEANING
## Removing variables for various reasons (irrelevant to categorizing/plotting,
## textual elaborate descriptions, information leakage, too specific, etc.) 

## First stage: removing all obvious unnecessary variables etc.

##Following the structure of the codebook, we have deleted the following 
## variables for the specific category of variables
## I. GTD_ID and Date
terror <- select(terror, -c(approxdate:resolution)) %>%
  ## II. Incident Information
  select(-c(summary, contains('crit'), alternative:multiple, related)) %>% 
  ## III. Incident location
  select(-c(provstate:location)) %>%
  ## IV. Attack Information
  select(-c(suicide, success)) %>%
  ## V. Weapon information 
  select(-c(contains('weap'))) %>%
  ## VI. Target/Victim information
  select(-c(contains('target'), contains('corp'), contains('subtype'))) %>%
  ## VII. Perpetrator Information
  select(-c(motive, nperpcap, contains('subname'), contains('claim'))) %>%
  ## VIII. Casualties and consequences
  select(-c(nkill:nreleased)) %>%
  ## IX. Additional Information Sources
  select(-c(addnotes:dbsource, INT_ANY)) 

## For more information about why and which variables were removed, see the
## readme file or the appendix of the word file.

## clear example of information leakage, suicide is not known untill 
## after the attack but gives alot of information on attacktype
ggplot(terror_original, aes(x = as.factor(suicide), fill = attacktype1_txt)) +
  geom_bar() +
  labs(fill = "Attack Type")

## Second stage: replacing all 'no _txt' variables for '_txt' and remove latter

## replace all columns with no '_txt' for '_txt' columns 
text_columns <- colnames(terror)[grep('txt', colnames(terror))]
for (column in text_columns) {
  terror[substr(column, 1, nchar(column) - nchar('_txt'))] <- terror[column] 
}
## afterwards remove the '_txt', this leaves only the colname of the 
## no '_txt' with the 'character' content of the '_txt' columns
terror <- select(terror, -c(contains('_txt')))

## Third stage: prepare the predictor

## The target Y is attacktype1, to avoid unclear cases with more
## than one attack types are cleaned and removed, also all NA's are filtered

##Attack types, count for every row the amount of attack_types
single_attack <- function(row_df) {
  bool_attack <- row_df[grep('attack', colnames(terror))] != ""
  sum(bool_attack, na.rm = TRUE)
}

## apply function on all rows of the dataframe
count_attacks <- apply(terror, MARGIN = 1, FUN = single_attack)

#filter all rows that anything else than 1 attack type, so greater than 1 or NA
terror <- filter(terror, count_attacks == 1) %>%
  select(-c(attacktype2:attacktype3))


## Fourth stage, general cleaning of content of columns
terror <- mutate(terror, imonth = ifelse(imonth == 0, NA, imonth),
  iday = ifelse(iday == 0, NA, iday)) %>%
    ## pick only cases where there is no doubt of a terrorist attack
    filter(doubtterr == 0) %>%
    select(-doubtterr)


## Fifth stage, specific cleaning to prepare naive bayes

## since naive bayes needs only NON-NA rows our goal is to remove al columns
## with alot of NA's and aftewards omit al rows with NA's. 

#function to get the percentage of non_na per column
perc_non_NA <- function(col_df) {
  sum(!is.na(col_df))/length(col_df)
}

#get the percentage available (non_na) per column
perc_available <- apply(terror, MARGIN = 2, perc_non_NA)

## remove al columns with a with a lot of NA's and thus a low percentage
## of availability (scale 0 to 1), remove all with less than 0.75 available 
## instances.
## removes: targtype2 and 3, natlty2 and 3, gname2 and 3, guncertain2 and 3
## nperps, INT_LOG, INT_IDEO
terror <- select(terror, which(perc_available > 0.5))

## last step: after all columns have been removed, remove all the rows with NA's
## which leaves us a full database ready for naive bayes analysis.
terror <- na.omit(terror)


## DESCRIPTIVE STATISTICS 

## Attack type 
## in absolute numbers
table(terror$attacktype1)
## in percentages
perc_attacktype <- round(prop.table(table(terror$attacktype1)), digits = 3)
perc_attacktype

## bombing/explions: 47.80% is the baseline for predictions using accuracy.
baseline <- perc_attacktype[perc_attacktype == max(perc_attacktype)]
baseline

## plot attack type 
sorted_attacks <- names(sort(perc_attacktype, decreasing = TRUE))

ggplot(terror, aes(x = attacktype1, fill = attacktype1)) +
  geom_bar() +
  scale_x_discrete(limits = sorted_attacks) +
  scale_fill_discrete(breaks = sorted_attacks) +
  theme(axis.text.x = element_blank()) + 
  labs(x = "Attack type", fill = "Attack type")


## most informative variables given information gain on attacktype 
## targettype, country and groupnames are the most important variables 
## followed by nationality
information.gain(attacktype1 ~ ., terror)

## Most informative variables on attacktype

## 1. Group name on attacktype

## Remove all groupnames under a certain amount of terror attacks, to clean data
## for ggmap and descriptive plots and tables
terror_groups <- group_by(terror, gname) %>%
  summarise(count = n(), perc = count/nrow(terror)*100) %>%
  arrange(desc(count))
terror_groups

## pick the biggest six and filter them on these
big_groups <- terror_groups$gname[1:6]
terror_big_groups <- filter(terror, gname %in% big_groups)

##Plot the biggest terrorist groups on attacktype
sorted_groups <- names(sort(table(terror_big_groups$gname)))

ggplot(terror_big_groups, aes(x = gname, fill = attacktype1)) +
  geom_bar() +
  scale_x_discrete(limits = sorted_groups, 
    labels = c("ETA", "FMLN", "IRA", "ISIL", "SL", "Taliban")) +
  labs(x= "Group name", fill = "Attack type", y = "Amount of attacks")
  
## proportion table margined by column, every group counts up to 100%
round(prop.table(table(terror_big_groups$attacktype1, terror_big_groups$gname), 
  margin = 2), digits = 2)


## 2. Target type on attacktype
terror_targets <- group_by(terror, targtype1) %>%
  summarise(count = n(), perc = count/nrow(terror)*100) %>%
  arrange(desc(count))
terror_targets

## pick the biggest five and filter them on these
big_targets <- terror_targets$targtype1[1:6]
terror_big_targets <- filter(terror, targtype1 %in% big_targets)

##Plot the biggest target groups on attacktype
sorted_groups <- names(sort(table(terror_big_targets$targtype1)))

ggplot(terror_big_targets, aes(x = targtype1, fill = attacktype1)) +
  geom_bar() +
  scale_x_discrete(limits = sorted_groups) +
  labs(x= "Target types", fill = "Attack type", y = "Amount of attacks") 

## proportion table margined by column, every group counts up to 100%
round(prop.table(table(terror_big_targets$attacktype1, 
  terror_big_targets$targtype1), margin = 2), digits = 2)


## 3. Nationality on attacktype
terror_nationality <- group_by(terror, natlty1) %>%
  summarise(count = n(), perc = count/nrow(terror)*100) %>%
  arrange(desc(count))
terror_nationality

## pick the biggest five and filter them on these
big_nationalities <- terror_nationality$natlty1[1:6]
terror_big_nationalities <- filter(terror, natlty1 %in% big_nationalities)

##Plot the biggest target nationalities on attacktype
sorted_groups <- names(sort(table(terror_big_nationalities$natlty1)))

ggplot(terror_big_nationalities, aes(x = natlty1, fill = attacktype1)) +
  geom_bar() +
  scale_x_discrete(limits = sorted_groups) +
  labs(x= "Nationality of targets", fill = "Attack type", y = 
         "Amount of attacks") 

## proportion table margined by column, every group counts up to 100%
round(prop.table(table(terror_big_nationalities$attacktype1, 
  terror_big_nationalities$natlty1), margin = 2), digits = 2)


## PREDICTION

## converting all variables to factors for naive
terror <- mutate_at(terror, vars(-c(eventid)), funs(factor))

## Final version of dataset before predicting
dim(terror)
head(terror)
str(terror)
summary(terror)
colnames(terror)


## SPLITTING (HOLD-OUT METHOD) (SUB)TRAIN/VAL/TEST

## Splitting the dataset in a train and test part (hold-out method)
set.seed(256)
train_index = createDataPartition(y = terror$attacktype1, p = 0.80,
                                  list = FALSE)
train_attack = terror[train_index, ]
test_attack = terror[-train_index, ]

## Splitting the trainset for optimisation into a subtrain and validation set
## create a validation set for optimizing, cv is not possbile, could not utilize
## caret with nb because of speed issues etc... (e1071 is alot faster) 
## cv method doesn't work on e1071 in combination with naive bayes
set.seed(128)
sub_index = createDataPartition(y = train_attack$attacktype1, p = 0.80,
                                list = FALSE)

subtrain_attack = train_attack[sub_index, ]
val_attack = train_attack[-sub_index, ]


## OPTIMISATION PROCESS

## baseline to beat: 47,78%
baseline

#see what variables give alot of information of the trainset
information.gain(attacktype1 ~ ., subtrain_attack)

## create a function and loop for this: (!) model specifying than 
## Fitting a naive bayes model (by hand, because no cv could be used
## predictors specified on the dependent variable attacktype (y)
## First stage of tuning; selecting optimal variables to include in NB model.
mdl1 <- attacktype1 ~ . - eventid
mdl2 <- attacktype1 ~ country
mdl3 <- attacktype1 ~ targtype1 
mdl4 <- attacktype1 ~ natlty1 
mdl5 <- attacktype1 ~ country + targtype1 + natlty1 
mdl6 <- attacktype1 ~ country + targtype1
mdl7 <- attacktype1 ~ country + natlty1
mdl8 <- attacktype1 ~ targtype1 + natlty1
mdl9 <- attacktype1 ~ country + targtype1 + region + iyear
mdl10 <- attacktype1 ~ targtype1 + region + iyear
mdl11 <- attacktype1 ~ country + targtype1 + iyear
mdl12 <- attacktype1 ~ country + targtype1 + region
mdl13 <- attacktype1 ~ country + targtype1 + iyear + natlty1
mdl14 <- attacktype1 ~ gname 
mdl15 <- attacktype1 ~ gname + country
mdl16 <- attacktype1 ~ gname + targtype1
mdl17 <- attacktype1 ~ gname + natlty1
mdl18 <- attacktype1 ~ gname + country + targtype1
mdl19 <- attacktype1 ~ gname + country + natlty1
mdl20 <- attacktype1 ~ gname + natlty1 + targtype1
mdl21 <- attacktype1 ~ gname + natlty1 + targtype1 + country
models <- paste0('mdl', 1:21)

# laplace smooting is used to smooth probabilities so none equals exactly zero.
# we are extracting accuracy, as this is not a binary classifier, but y has 8
# categories, that would result in a confusing false positive/negative table.
testModel <- function(mdlx, train_set, test_set) {
  bayes_attack <- naiveBayes(mdlx, data=train_set, laplace = 1)
  predicted_attack <- predict(bayes_attack, test_set)
  confusionMatrix(predicted_attack, test_set$attacktype1)
}

## test the different models on the validation set
## testModel(subtrain_attack, test_attack)
rslt_models_acc <- c() #create empty vector to bind accuracies of al models
rslt_models <- list() #create empty list to create a list with all confMatrices.

set.seed(256)
for(mdl in models) {
  confMat <- testModel(mdlx = as.formula(mdl), train_set = subtrain_attack, 
    test_set = val_attack)
  rslt_models[mdl] <- list(confMat)
  #get Accuracy of the model
  mdl_acc <- rslt_models[[mdl]]$overall['Accuracy']
  rslt_models_acc <- rbind(rslt_models_acc, mdl_acc)
}

rownames(rslt_models_acc) <- models
## review results
rslt_models
rslt_models_acc
rslt_models_acc <- as.data.frame(rslt_models_acc)
ggplot(rslt_models_acc, aes(x = models, y = Accuracy, fill = Accuracy)) + 
  geom_bar(stat = 'identity') +
  scale_x_discrete(limits = models)

## pick best performing model and train as whole on trainset (not just subtrain)
max(rslt_models_acc)
which(rslt_models_acc == max(rslt_models_acc))
rslt_models$mdl16 #model 16 


## FINAL MODEL

## train best performing model on whole train attack and use this model 
## to predict this on the test set
set.seed(256)
rslt_final <- testModel(mdl16, train_attack, test_attack)

##Final
rslt_final
rslt_final$overall['Accuracy'] # an overall accuracy of 56.26%. 


## GGMAPS: Plotting the results on a map


## Preparing the 'ggmap' datset 

## Combine predicted values with 'terror_ggmap' columns (including coordinates)
test_rslt <- naiveBayes(mdl16, data=train_attack, laplace = 1)

## Add the predicted y (attacktype) to the test_attack dataset and whether
## Prediction is accurate (accurate_prediction, yes or no)
test_attack$predicted_attacktype <-  predict(test_rslt, test_attack)
test_attack$accurate_prediction <- ifelse(test_attack$attacktype1 == 
  test_attack$predicted_attacktype, "Yes", "No")

## select the needed variables from test_attack dataset and create a new one
test_attack_join <- select(test_attack, eventid, attacktype1, 
  predicted_attacktype, accurate_prediction) 

## now join the location variables with 'test_attack_join' and only keep the 
## rows of 'test_attack_join'
terror_ggmap <- select(terror_original, c(eventid, provstate:vicinity)) %>%
  inner_join(test_attack_join, by="eventid") %>%
  ## note: specificity is a variable that indicates certainty of the longitude/
  ## latitude variables. We have chosen only certain coordinates
  filter(specificity == 1)


## Plotting the (specific) results 

## Get distinctive colors for plotting
display.brewer.pal(8, 'Set1')
legend_colors <- brewer.pal(8, name =  'Set1')

#get the map
AttackMap <- qmap("Europe", zoom = 4, color = 'bw', legend = 'topleft')

## Predicted attacks visualized
AttackMap +
  geom_point(data = terror_ggmap, aes(x = longitude, y = latitude, 
    colour = predicted_attacktype, shape = accurate_prediction), 
     size = 2.75, alpha = 0.85) + 
  scale_color_manual(name = "Predicted Attacktype", values = legend_colors) + 
  scale_shape_discrete(breaks = c("Yes", "No")) + 
  labs(shape="Accurate prediction")

## Actual attacks visualized
AttackMap +
  geom_point(data = terror_ggmap, aes(x = longitude, y = latitude, 
    colour = attacktype1, shape = accurate_prediction), 
    size = 2.75, alpha = 0.85) + 
  scale_color_manual(name = "True attacktype", values = legend_colors) + 
  scale_shape_discrete(breaks = c("Yes", "No")) + 
  labs(shape="Accurate prediction")
