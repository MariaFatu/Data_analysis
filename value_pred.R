library(rsample)
library(tidyverse)
library(caret)
library(modelr)
library(ISLR)
library(DataExplorer)
library(stringr)
library(AICcmodavg)
library(Metrics)

install.packages('DataExplorer')
install.packages("AICcmodavg")

#fifa 20 football players stats
players = read.csv("players_20.csv")

head(players)

#dataset too big for table
players
#dimensions of the table, 18278 obs and 104 vars, too much unneeded info
dim(players)
mean(subset(players$value_eur, players$international_reputation == 5))
mean(subset(players$value_eur, players$international_reputation == 4))
mean(subset(players$value_eur, players$international_reputation == 3))
mean(subset(players$value_eur, players$international_reputation == 2))
mean(subset(players$value_eur, players$international_reputation == 1))
#vars values and types, clean needed
glimpse(players)

#summary of the dataset
summary(players)

#explore data with a complete report containing
#basic statistics, structure, missing data, distribution visualizations,
#correlation matrix and principal component analysis
DataExplorer::create_report(players)

dim(players)
summary(players)

#get main position of each player
positions = reshape2::colsplit(players$player_positions,",", c('main_position','others'))
players = players %>% 
  mutate(main_position = positions$main_position, .after = wage_eur)

#eliminate vars that do not help with the analysis
players = select(players, age:goalkeeping_reflexes)

#eliminate vars that do not help with the analysis
players = select(players, -c( 
  nationality, 
  player_tags,
  player_positions,
  team_position,
  team_jersey_number,
  dob,
  nation_position,
  real_face,
  contract_valid_until,
  release_clause_eur, 
  weight_kg,
  preferred_foot,
  work_rate,
  body_type,
  height_cm,
  player_traits,
  joined, 
  loaned_from,
  club, 
  contract_valid_until,
  nation_jersey_number,
  body_type))


#map international_reputation to factor, because it represents the level of being worldwide known
players$international_reputation =factor(players$international_reputation, labels = c('Unknown','Little known', 'Known', 'Well known','Best known'))

labels(players$international_reputation)

#map skill_moves to factor, because it represents the level of being technically gifted
players$skill_moves =factor(players$skill_moves, labels = c('Skilless','Little skillful', 'Skillful', 'More skillful','Most skillful'))

#map weak_foot to factor, because it represents how well you can use your weaker foot compared to preferred one
players$weak_foot =factor(players$weak_foot, labels = c('Unuseable','Little useable', 'Useable', 'More useable','Most useable'))

#map main_position to factor, because it represents player positions; no order needed
players$main_position =factor(players$main_position)

#check 
levels(players$main_position)

#eliminate goalies; main set is too heterogeneous
players = subset(players, main_position != 'GK')

#remove goalies attributes for a homogeneous set of data
players = select(players, -starts_with("gk"))

#refactor to eliminate GK
players$main_position =factor(players$main_position)

#check
levels(players$main_position)

#check data analysis
DataExplorer::create_report(players)

#check on age histogram
length(unique(players$age))

#?? why gap as there are values there too; 27 diff values 
ggplot(players, aes(age)) + geom_histogram(bins = 27)

#correlation might exist; linear
players %>%
  ggplot(aes(wage_eur, value_eur)) + geom_point() + geom_smooth()

#might exist correlation; not a clear linear
players %>%
  ggplot(aes(overall, value_eur)) + geom_point() + geom_smooth()

set.seed(123)
split = initial_split(players, prop = 0.7, strata = "value_eur")
train = training(split)
test = testing(split)
mean(train$value_eur)
mean(test$value_eur)

table(players$main_position)

#all positions

#best model


fmla <- as.formula(paste("value_eur ~ ", paste(colnames(select(players,
                                                               c(attacking_crossing:defending_sliding_tackle,
                                                                 age,
                                                                 wage_eur,
                                                               ))), collapse= "+")))

modelt = lm(data = train, value_eur ~ .)
summary(modelt)

modelWage = lm(data = train, value_eur ~ wage_eur)
summary(modelWage)

modelOVR = lm(data = train, value_eur ~ overall)
summary(modelOVR)

modelPotential = lm(data = train, value_eur ~ potential)
summary(modelPotential)

modelAge = lm(data = train, value_eur ~ age)
summary(modelAge)

modelWO = lm(data = train, value_eur ~ wage_eur + overall)
summary(modelWO)

modelWOP = lm(data = train, value_eur ~ wage_eur+ overall + potential)
summary(modelWOP)

modelWOPA = lm(data = train, value_eur ~ wage_eur+ overall + potential + age)
summary(modelWOPA)

#best model
modelWOA = lm(data = train, value_eur ~ wage_eur+ overall + age)
summary(modelWOA)

#should not put int_rep, cause it is already used in overall
modelWOPAI = lm(data = train, value_eur ~ overall + wage_eur + age + international_reputation )
summary(modelWOPAI)

#results make no sense
modelWOPAIS = lm(data = train, value_eur ~ overall + wage_eur + potential  + age  + skill_moves)
summary(modelWOPAIS)

#using substitutes for ovr
model2 = lm(data = train, value_eur ~ shooting + defending + dribbling + physic + age + wage_eur + international_reputation + skill_moves )
summary(model2)

#verify which added variable makes more sense
anovaResults = data.frame()
anovaResults = data.frame(paste(names(modelWO$model), collapse =" "), paste(names(modelWOP$model), collapse = " "), anova(modelWO, modelWOP)$`Pr(>F)`[2])
names(anovaResults) = c("model1","model2","p-value")
intermediar = data.frame(paste(names(modelWO$model), collapse =" "), paste(names(modelWOA$model), collapse = " "), anova(modelWO, modelWOA)$`Pr(>F)`[2])
names(intermediar) = c("model1","model2","p-value")
anovaResults = rbind(anovaResults, intermediar)
intermediar = data.frame(paste(names(modelWOP$model), collapse =" "), paste(names(modelWOPA$model), collapse = " "), anova(modelWOP, modelWOPA)$`Pr(>F)`[2])
names(intermediar) = c("model1","model2","p-value")
anovaResults = rbind(anovaResults, intermediar)

#check results
anovaResults

models = list(modelWage, modelOVR, modelPotential, modelAge, modelWO, modelWOP, modelWOPA, modelWOA, model2)
#check which model explains its data better, using AIC
aictab(models, modnames = c('modelWage', 'modelOVR', 'modelPotential', 'modelAge', 'modelWO', 'modelWOP', 'modelWOPA','modelWOA','model2'))

#makes no sense
model3 = lm(data = train, fmla)
summary(model3)

predictionAll = predict(modelWOA, newdata = test)
#2.383.658
RMSE(predictionAll, test$value_eur)

#0.2040698
rse(test$value_eur, predictionAll)

