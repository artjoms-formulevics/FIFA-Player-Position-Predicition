# Author: Artjoms Formulevics
# Capstone: Your Own Project

#-----Data loading/initializing-----

# Installing libraries, if not already present

if (!require(caret))
  install.packages("caret")
if (!require(data.table))
  install.packages("data.table")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(forcats))
  install.packages("forcats")
if (!require(ggplot2))
  install.packages("ggplot2")
if (!require(reshape2))
  install.packages("reshape2")
if (!require(kableExtra))
  install.packages("kableExtra")
if (!require(knitr))
  install.packages("knitr")
if (!require(stringr))
  install.packages("stringr")
if (!require(tidyr))
  install.packages("tidyr")
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(magrittr))
  install.packages("magrittr")
if (!require(ggrepel))
  install.packages("ggrepel")
if (!require(caTools))
  install.packages("caTools")
if (!require(MLmetrics))
  install.packages("MLmetrics")
if (!require(xgboost))
  install.packages("xgboost")
if (!require(rpart))
  install.packages("rpart")
if (!require(C50))
  install.packages("C50")
if (!require(klaR))
  install.packages("klaR")
if (!require(MASS))
  install.packages("MASS")
if (!require(gbm))
  install.packages("gbm")
if (!require(glmnet))
  install.packages("glmnet")
if (!require(Matrix))
  install.packages("Matrix")


# Loading libraries

library(caret)
library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(knitr)
library(stringr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggrepel)
library(caTools)
library(MLmetrics)
library(xgboost)
library(rpart)
library(C50)
library(klaR)
library(MASS)
library(gbm)
library(glmnet)
library(Matrix)

# Setting Knitr format

options(knitr.table.format = "html")

# Loading Data

fifa <-
  read.csv(
    "https://raw.githubusercontent.com/artjoms-formulevics/HarvardX/master/data.csv",
    header = T,
    stringsAsFactors = F
  )

# Saving original dataset

fifa_original <- fifa

#-----Tidying Data----

## Taking a look at the dataset structure
# Getting dimensions (size) of the dataset

data.frame("Rows" = nrow(fifa), "Columns" = ncol(fifa)) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Getting information about variables classes and values

data.frame(
  variable_name = names(fifa),
  class = sapply(fifa, typeof),
  first_values = sapply(fifa, function(x)
    paste0(head(x, n = 3),  collapse = ", ")),
  row.names = NULL
) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Converting player Value, Wage and Release Clause to numeric

fifa$ValueLast <-
  sapply(strsplit(as.character(fifa$Value), ""), tail, 1)
fifa$WageLast <-
  sapply(strsplit(as.character(fifa$Wage), ""), tail, 1)
fifa$Release.Clause.Last <-
  sapply(strsplit(as.character(fifa$Release.Clause), ""), tail, 1)

extract <- function(x) {
  regexp <- "[[:digit:]]+"
  str_extract(x, regexp)
}

temp <- sapply(fifa$Value, extract)
fifa$Value <- as.numeric(temp)
fifa$Value <-
  ifelse(fifa$ValueLast == "M", fifa$Value * 1000000, fifa$Value * 1000)

temp <- sapply(fifa$Wage, extract)
fifa$Wage <- as.numeric(temp)
fifa$Wage <-
  ifelse(fifa$WageLast == "M", fifa$Wage * 1000000, fifa$Wage * 1000)

temp <- sapply(fifa$Release.Clause, extract)
fifa$Release.Clause <- as.numeric(temp)
fifa$Release.Clause <- ifelse(
  fifa$Release.Clause.Last == "M",
  fifa$Release.Clause * 1000000,
  fifa$Release.Clause * 1000
)

# Converting Height to numeric & metric system

temp <- str_split(fifa$Height, "'")
for (i in 1:length(temp)) {
  temp[[i]] <- as.numeric(temp[[i]])
  temp[[i]] <- (temp[[i]][1] * 12) + temp[[i]][2]
}
temp <- as.numeric(unlist(temp)) * 2.54
fifa$Height <- temp

# Converting Weight to numeric & metric system

temp <- sapply(fifa$Weight, extract)
temp <- as.numeric(temp) * 0.453592
fifa$Weight <- temp

# Filtering out Goalkeepers

fifa <- filter(fifa, fifa$Position != "GK")
fifa <- filter(fifa, fifa$Position != "")

# Removing Poistion scores that won't be used

fifa[, 29:54] <- NULL

# Removing not meaningful variables

fifa <-
  subset(
    fifa,
    select = -c(
      ID,
      X,
      Photo,
      Flag,
      Club.Logo,
      Real.Face,
      Joined,
      Loaned.From,
      Contract.Valid.Until,
      GKDiving,
      GKHandling,
      GKKicking,
      GKPositioning,
      GKReflexes,
      ValueLast,
      WageLast,
      Release.Clause.Last,
      Special
    )
  )

# Show Position groups

temp <- data.frame(Original = unique(fifa$Position))
temp$New <- fct_collapse(
  temp$Original,
  Def = c("RCB", "CB", "LCB", "LB", "RB", "RWB", "LWB"),
  Mid = c(
    "RCM",
    "LCM",
    "LDM",
    "CAM",
    "CDM",
    "RM",
    "LAM",
    "LM",
    "RDM",
    "CM",
    "RAM"
  ),
  Att = c("RF", "ST", "LF", "RS", "LS", "RW", "CF", "LW")
)
temp %>%
  arrange(New, Original) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Group all positions into three categories

fifa$Position_gr <- fct_collapse(
  fifa$Position,
  Def = c("RCB", "CB", "LCB", "LB", "RB", "RWB", "LWB"),
  Mid = c(
    "RCM",
    "LCM",
    "LDM",
    "CAM",
    "CDM",
    "RM",
    "LAM",
    "LM",
    "RDM",
    "CM",
    "RAM"
  ),
  Att = c("RF", "ST", "LF", "RS", "LS", "RW", "CF", "LW")
)

fifa$Position_gr <-
  factor(fifa$Position_gr, levels = c("Def", "Mid", "Att"))

# Remove unwanted variables and temporarty variables

rm(temp, i, extract)

#-----Manipulations/Visualizations-----

# Getting dimensions (size) of the dataset

data.frame("Rows" = nrow(fifa), "Columns" = ncol(fifa)) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Getting information about variables classes and values

data.frame(
  variable_name = names(fifa),
  class = sapply(fifa, typeof),
  first_values = sapply(fifa, function(x)
    paste0(head(x, n = 3),  collapse = ", ")),
  row.names = NULL
) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Showing missing values

sapply(fifa, function(x)
  sum(is.na(x))) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Removing Release Clause from analysis

fifa$Release.Clause <- NULL

# Number of players by position in the dataset

fifa %>% group_by(Position) %>% summarise(count = n()) %>%
  ggplot(aes(reorder(Position, -count), count)) +
  theme_light()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Players by Position",
       x = "Position",
       y = "Players")

# Number of players by position after grouping in the dataset

fifa %>% group_by(Position = Position_gr) %>% summarise(count = n()) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Distribution of Overall Rating by Position

fifa %>%
  ggplot(aes(Overall)) +
  theme_light()  +
  geom_histogram(bins =  14) +
  labs(title = "Overall Rating Distribution by Position",
       x = "Overall Rating",
       y = "Frequency") +
  facet_grid(Position_gr ~ .)

# Top 15 players by overall rating

fifa %>%
  group_by(Position_gr, Name, Overall, Potential) %>%
  summarise() %>%
  arrange(desc(Overall)) %>%
  head(n = 15) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Top 15 players by Value

fifa %>%
  group_by(Position_gr, Name, Value, Wage) %>%
  summarise() %>%
  arrange(desc(Value)) %>%
  head(n = 15) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Finding the most valuable teams

fifa %>%
  group_by(Club) %>%
  summarise(Club.Squad.Value = round(sum(Value) / 1000000)) %>%
  arrange(-Club.Squad.Value) %>%
  head(10) %>%
  ggplot(aes(
    x = as.factor(Club) %>%
      fct_reorder(Club.Squad.Value),
    y = Club.Squad.Value,
    label = Club.Squad.Value
  )) +
  geom_text(hjust = 0.01,
            inherit.aes = T,
            position = "identity") +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Club") +
  ylab("Squad Value in Million") +
  ggtitle("Most valuable teams")

# Correlation between player rating and jersey number

fifa %>%
  group_by(Jersey.Number) %>%
  summarise(
    Avg.Overall = sum(Overall) / length(Jersey.Number),
    Player.Count = sum(Jersey.Number)
  ) %>%
  arrange(-Avg.Overall) %>%
  ggplot(aes(
    x = Jersey.Number,
    y = Avg.Overall,
    col = ifelse(Avg.Overall < 70, "darkgrey", "Red")
  )) +
  geom_point(position = "jitter") +
  theme(legend.position = "none") +
  geom_text_repel(aes(label = ifelse(Avg.Overall >= 70, Jersey.Number, ""))) +
  ggtitle("Correlation between player rating and jersey number")

# Most popular Player Jersey by Position Group

bind_rows(
  fifa[fifa$Position_gr == "Def",] %>%
    group_by(Position_gr, Jersey.Number) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    head(5),
  fifa[fifa$Position_gr == "Mid",] %>%
    group_by(Position_gr, Jersey.Number) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    head(5),
  fifa[fifa$Position_gr == "Att",] %>%
    group_by(Position_gr, Jersey.Number) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    head(5)
) %>%
  kable() %>% collapse_rows(columns = 1) %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

#----Preparing for Modelling-----

# Creating dataset with names for testing

fifa_names <-
  subset(fifa, select = c(Name, Overall, Position, Position_gr))

# Removing Names and ungroped positions and unwanted variables

fifa$Position <- fifa$Position_gr
fifa <-
  subset(fifa, select = -c(Name, Position_gr, Club, Nationality))

# Leaving position pout of preprocessing

fifa_pos <- fifa$Position
fifa$Position <- NULL

# Converting characters to factors

fifa[sapply(fifa, is.character)] <-
  lapply(fifa[sapply(fifa, is.character)],
         as.factor)

# Centering and scaling

preProc <- preProcess(fifa, method = c("center", "scale"))
fifa <- predict(preProc, fifa)

# Convering factors to numbers (dummy variables)

dmy <- dummyVars("~ .", data = fifa, fullRank = F)
fifa <- data.frame(predict(dmy, newdata = fifa))

# Putting Position back as the first variable

fifa$Position <- fifa_pos
fifa <- fifa[, c(61, 1:60)]

# Setting seed for reproducability

seed = 1
set.seed(seed)

# Train Control function for repeated cross-validation

train.control <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 3)

# Splitting into train and test sets 0.9 vs 0.1 (also for names)

sample <- sample.split(fifa, SplitRatio = 0.9)
train <- subset(fifa, sample == TRUE)
test <- subset(fifa, sample == FALSE)

train_names <- subset(fifa_names, sample == TRUE)
test_names <- subset(fifa_names, sample == FALSE)

#----Modelling-----

# Setting list and names

fit <- list()
pred <- list()
pred.prob <- list()

models <-
  c("lda",
    "rda",
    "nb",
    "knn",
    "rpart",
    "C5.0",
    "rf",
    "glmnet",
    "gbm",
    "xgbLinear")

# Creating each model and predictions & probabilities for each class

for (i in c(1:length(models))) {
  set.seed(seed)
  if(models[i] == "gbm") {
    fit[[i]] <-
      train(Position ~ .,
            data = train,
            method = models[i],
            trControl = train.control,
            verbose = F) # supress iterations output for gbm model
  } else {
    fit[[i]] <-
      train(Position ~ .,
            data = train,
            method = models[i],
            trControl = train.control)
  }
  pred[[i]] <- predict(fit[[i]], test, type = "raw")
  pred.prob[[i]] <- predict(fit[[i]], test, type = "prob")
}

# Adding names to models in list

names(fit) <- models
names(pred) <- models
names(pred.prob) <- models

#------Testing Results-----

# Testing what players were guessed incorrectly
# Setting lists

test.model <- list()
n_miss <- list()

# For each model creating a table with actual vs predicted outcome & probabilities of classes
# Also creating a table with overall number of incorrect predictions
# For each algorithm showing 10 top players (by rating) which were incorrectly classified

for (i in c(1:length(models))) {
  test.model[[i]] <-
    cbind(test_names, pred = pred[[i]], round(pred.prob[[i]], 2))
  test.model[[i]]$correct <-
    ifelse(test.model[[i]]$Position_gr == test.model[[i]]$pred, 1, 0)
  test.model[[i]]$method <- models[i]
  
  n_miss[[i]] <-
    test.model[[i]] %>% filter(test.model[[i]]$correct == 0) %>% summarise(n())
  
  head(test.model[[i]][test.model[[i]]$correct == 0,], 10) %>%
    kable() %>%
    kable_styling(
      bootstrap_options = c("striped", "bordered", "hover", "responsive"),
      full_width = FALSE,
      position = "center",
      font_size = 11
    ) %>% print()
}

# Adding model names

names(test.model) <- models
names(n_miss) <- models

# Binding all results together

testing_results <- rbindlist(test.model)
n_missed <- rbindlist(n_miss, idcol = models)
colnames(n_missed) <- c("Model", "Missed")

# Showing how top players were incorrectly classified

testing_results %>% filter(correct == "0") %>% group_by(Name, Overall, Position, Position_gr, pred) %>%
  summarise(count = n()) %>%
  arrange(desc(Overall)) %>% head(20) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Showing some examples

testing_results[Name == "L. Modriæ" & correct == 0, ] %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

testing_results[Name == "L. Bonucci" & correct == 0, ] %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Showing which players been wrongly classified by most algorithms

testing_results %>% filter(correct == "0") %>% group_by(Name, Overall, Position, Position_gr) %>%
  summarise(count = n()) %>%
  arrange(desc(Overall)) %>% arrange(desc(count)) %>% head(20) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Showing some examples

testing_results[Name == "Iniesta" & correct == 0, ] %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

temp <- data.frame(fifa_original[fifa_original$Name == "Iniesta", 29:54])  %>% t() %>% data.frame()
sort(temp[,1], decreasing = T) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

rm(temp)

# Showing total number of missed predictions for each model in descending order

n_missed %>% arrange(desc(Missed)) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Creating summary for all resamples for all models with Accuracy and Kappa metrics

results_resamples <- resamples(fit)
summary(results_resamples, metric = c("Accuracy", "Kappa"))

# Selecting Accuracy metric to plot with Box-Plot chart

results_acc <-
  results_resamples$values[, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)]
colnames(results_acc) <-
  gsub("~Accuracy", "", colnames(results_acc))

ggplot(data = melt(results_acc),
       aes(x = variable, y = value)) + geom_boxplot() + theme_light()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of Accuracy of Models",
       x = "Model",
       y = "Accuracy")

# Preparing tables for summary function (selecting observations, predictions and probabilities)
# Creating Multi-Class summary stats from caret package

summary_tabl <- list()
summary_stats <- list()

for (i in c(1:length(models))) {
  summary_tabl[[i]] <- test.model[[i]][, 4:8]
  colnames(summary_tabl[[i]]) <-
    c("obs", "pred", "Def", "Mid", "Att")
  
  summary_stats[[i]] <- multiClassSummary(data = summary_tabl[[i]],
                                          lev = levels(test$Position),
                                          model = fit[[i]])
  
}

# Adding model names

names(summary_tabl) <- models
names(summary_stats) <- models

# Creating data frame with stats for models

stats <-
  data.frame(model = models, round(t(data.frame(summary_stats[1:10])), 4))

# Creating table with ranks for each statistic

r <- data.frame(stats)
r[, 2] <- rank(-stats[, 2])
for (i in c(3:length(stats))) {
  r[, i] <- rank(stats[, i])
}

# Showing top algorithms sorted by mean of their rank

left_join(data.frame(model = models, score = round(rowMeans(
  subset(r, select = -1)
), 2)),
stats, by = "model") %>%
  arrange(desc(score)) %>% t() %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  )

# Confusion Matrices for top algorithm
confusionMatrix(data = pred[["glmnet"]], reference = test$Position)
confusionMatrix(data = pred[["glmnet"]],
                reference = test$Position,
                mode = "prec_recall")

# Examples for GLMNET
testing_results[method == "glmnet" & Position_gr == "Att" & pred == "Def",] %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered", "hover", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 11
  ) 

# Best model parameters

fit[["glmnet"]]$bestTune
fit[["xgbLinear"]]$bestTune
fit[["rf"]]$bestTune
