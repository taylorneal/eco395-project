library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample) 
library(randomForest)
library(glmnet)
library(gamlr)
library(here)
library(kableExtra)

win_pct <- function(record) {
  game_numbers = as.integer(strsplit(as.character(record), "-")[[1]])
  
  return(game_numbers[1] / sum(game_numbers))
} 

game_results = read.csv("C:/Users/tneal/OneDrive/Desktop/Spring_22/python/final_project_data/game_results.csv", header = TRUE)

team_abbreviations = read.csv("C:/Users/tneal/OneDrive/Desktop/Spring_22/python/final_project_data/team_abbreviations.csv", header = TRUE)

game_results = rename(game_results, Team = home_team)
game_results = merge(game_results, team_abbreviations, by = "Team")
game_results = game_results %>% mutate(Home_TeamID = paste(Abbreviation, 
                                                          year, sep="-"))
game_results = select(game_results, -c(Team, Abbreviation))


game_results = rename(game_results, Team = away_team)
game_results = merge(game_results, team_abbreviations, by = "Team")
game_results = game_results %>% mutate(Away_TeamID = paste(Abbreviation, 
                                                           year, sep="-"))
game_results = select(game_results, -c(Team, Abbreviation))

write.csv(game_results, paste(here(), "data", "clean", "game_results.csv", sep = "/"), row.names = FALSE)

#####
### Advanced Stats Merging/Cleaning
#####

advanced_stats = data.frame()
for (year in seq(2007,2022)){
  temp_advanced_stats = read.csv(paste("C:/Users/tneal/OneDrive/Desktop/Spring_22/python/final_project_data/advanced_stats/", 
                                        year,".csv", sep = ""), header = TRUE)
  temp_advanced_stats$year = year
  advanced_stats = rbind(advanced_stats, temp_advanced_stats)
  
}
rm(temp_advanced_stats, year)

advanced_stats = advanced_stats %>% mutate(Team = gsub("\\*", "", Team),
                                           luck = W - PW)

advanced_stats = merge(advanced_stats, team_abbreviations, by = "Team")

advanced_stats = advanced_stats %>% mutate(Home_TeamID = paste(Abbreviation, 
                                                          year, sep="-"))
advanced_stats$Away_TeamID = advanced_stats$Home_TeamID
advanced_stats <- select(advanced_stats, -c(Rk, X, X.1, X.2, Abbreviation))

advanced_stats = rename(advanced_stats, Off_eFG = eFG., Off_TOV = TOV.,
                        Off_FT_FGA = FT.FGA, Def_eFG = eFG..1, 
                        Def_TOV = TOV..1, Def_FT_FGA = FT.FGA.1)

write.csv(advanced_stats, paste(here(), "data", "clean", "advanced_stats.csv", sep = "/"), row.names = FALSE)


#####
### model building / constructing X
#####

X = game_results

X = X %>% mutate(Home_win = as.numeric(home_score > away_score)) %>%
  filter(year > 2010, year != 2020)

X = select(X, -c(home_score, away_score))

X1 = merge(X, advanced_stats[c("Age","MOV","SOS","SRS","ORtg","DRtg","Pace",
                              "FTr","X3PAr","TS.","Off_eFG","Off_TOV",
                              "ORB.","Off_FT_FGA","Def_eFG","Def_TOV",
                              "DRB.","Def_FT_FGA","luck","Home_TeamID")], 
          by = "Home_TeamID")

X1 = merge(X1, advanced_stats[c("Age","MOV","SOS","SRS","ORtg","DRtg","Pace",
                              "FTr","X3PAr","TS.","Off_eFG","Off_TOV",
                              "ORB.","Off_FT_FGA","Def_eFG","Def_TOV",
                              "DRB.","Def_FT_FGA","luck","Away_TeamID")], 
          by = "Away_TeamID")

X1 = merge(X1, expanded_standings[c("Home","Road","Pre","Post","MOV_under_3",
                                  "MOV_over_10","Home_TeamID")], 
          by = "Home_TeamID")

X1 = merge(X1, expanded_standings[c("Home","Road","Pre","Post","MOV_under_3",
                                  "MOV_over_10","Away_TeamID")], 
          by = "Away_TeamID")



X2 = merge(X, advanced_stats[c("SRS", "ORtg", "DRtg", "TS.", "Age",
                               "MOV", "Home_TeamID")], by = "Home_TeamID")

X2 = merge(X2, advanced_stats[c("SRS", "ORtg", "DRtg", "TS.",
                                "Age",
                                "MOV","Away_TeamID")], 
          by = "Away_TeamID")

X2 = X2 %>% mutate(age_diff = Age.x - Age.y) %>% select(-c(Age.x, Age.y))

X2[7:17] = scale(X2[7:17], center = TRUE, scale = TRUE)

#X2 = merge(X2, expanded_standings[c("Trend", "Home_TeamID")], 
#          by = "Home_TeamID")
#
#X2 = merge(X2, expanded_standings[c("Trend", "Away_TeamID")], 
#          by = "Away_TeamID")

#####
### Model building (Random Forest)
#####

load.forest = randomForest(Home_win ~ ., data = X[-c(1,2)], importance = TRUE)

plot(load.forest)

varImpPlot(load.forest, type=1)

#modelr::rmse(load.forest, load_test)
#partialPlot(load.forest, load_test, 'temp', las=1)

test = sample(2007:2021, 3, replace = FALSE)
X_train = X[!(X$year %in% test),]
X_test = X[X$year %in% test,]


### Lasso
lasso = glmnet(as.matrix(X2[7:17]), X2$Home_win, 
                     family = "binomial", relax = FALSE)

cv.lasso = cv.glmnet(as.matrix(X2[7:17]), X2$Home_win, 
                  family = "binomial", type.measure = "auc", keep = TRUE, relax = FALSE)

rocs <- roc.glmnet(cv.lasso$fit.preval, newy = X2$Home_win)
best.lasso = cv.lasso$index["min",]

plot(rocs[[best.lasso]], type = "l")
invisible(sapply(rocs, lines, col="grey"))
lines(rocs[[best.lasso]], lwd = 2,col = "red")

plot(cv.lasso)
plot(lasso)
print(cv.lasso)
coef(cv.lasso, s = "lambda.min")
#coef(lasso)[,72]
kable(data.frame(as.matrix(cbind(coef(cv.lasso, s = "lambda.min"), coef(cv.lasso, s = "lambda.1se")))), format = "simple", 
      col.names = c("Max AUC", "1se"), digits = 3)


assess.glmnet(cv.lasso, newx = as.matrix(X2[7:17]), 
              newy = X2$Home_win, s = "lambda.min")

pred = predict(cv.lasso, newx = as.matrix(X_test[7:56]), s = "lambda.min", 
               type = "response")


## simple
simple = glm(Home_win ~ SRS.x + TS..y + MOV.y + age_diff, data = X2, family = binomial)
sim_pred = predict(simple, type = "response")
sqrt(sum((sim_pred - X2$Home_win)^2) / length(X2$Home_win))
rmse(simple, X2$Home_win)
#paste(colnames(team_vs_team)[2:31],"-2022", sep = "")

TeamIDs_2022 = c("ATL-2022", "BOS-2022", "CHI-2022", "DAL-2022", "DEN-2022", 
                 "GSW-2022","MEM-2022", "MIA-2022","MIL-2022", "MIN-2022", 
                 "BKN-2022", "NOP-2022","PHI-2022", "PHX-2022","TOR-2022", 
                 "UTA-2022")

TeamIDs_2021 = c("UTA-2021", "MEM-2021", "LAC-2021", "DAL-2021", "DEN-2021", 
                 "POR-2021","LAL-2021", "PHI-2021","WAS-2021", "NYK-2021", 
                 "ATL-2021", "MIL-2021","MIA-2021", "BKN-2021","BOS-2021", 
                 "PHX-2021")

all_matchups = data.frame()
for (team in TeamIDs_2022) {
  temp_matchups = data.frame(rep(team, 15), TeamIDs_2022[-match(team, TeamIDs_2022)])
  
  all_matchups = rbind(all_matchups, temp_matchups)
}
rm(temp_matchups, team)
colnames(all_matchups) = c("Home_TeamID", "Away_TeamID")

all_matchups = merge(all_matchups, advanced_stats[c("SRS", "ORtg", "DRtg", "TS.", "Age",
                                                    "MOV", "Home_TeamID")], by = "Home_TeamID")

all_matchups = merge(all_matchups, advanced_stats[c("SRS", "ORtg", "DRtg", "TS.",
                                                    "Age",
                                                    "MOV","Away_TeamID")], 
                     by = "Away_TeamID")


all_matchups = all_matchups %>% mutate(age_diff = Age.x - Age.y) %>% select(-c(Age.x, Age.y))

all_matchups = merge(all_matchups, expanded_standings[c("Overall","Home_TeamID")], 
                     by = "Home_TeamID")

all_matchups = merge(all_matchups, expanded_standings[c("Overall","Away_TeamID")], 
                     by = "Away_TeamID")

all_matchups[3:13] = scale(all_matchups[3:13], center = TRUE, scale = TRUE)


pred = predict(cv.lasso, newx = as.matrix(all_matchups[3:13]), s = "lambda.min", 
               type = "response")

all_matchups$Home_win_prob = pred

all_matchups = all_matchups %>% select(Home_TeamID, Away_TeamID, Home_win_prob, Overall.x, Overall.y) %>%
  arrange(Home_TeamID, Away_TeamID)

write.csv(all_matchups,"C:/Users/tneal/OneDrive/Desktop/Spring_22/python/NBA_playoff_bracket/artifacts/all_2022_matchups", row.names = FALSE)


####
####
#### CV
####
####

test_results = data.frame()
test_coef = matrix(nrow = 12)
for (i in 1:20) {
  test_set = sample(c(2011:2019,2021), 2, replace = FALSE)
  X1_train = X1[!(X1$year %in% test_set),]
  X1_test = X1[X1$year %in% test_set,]
  X2_train = X2[!(X2$year %in% test_set),]
  X2_test = X2[X2$year %in% test_set,]
  
  cv.lasso1 = cv.glmnet(as.matrix(X1_train[7:56]), X1_train$Home_win, 
                       family = "binomial", type.measure = "mse", keep = TRUE)
  lasso1 = assess.glmnet(cv.lasso1, newx = as.matrix(X1_test[7:56]), 
                newy = X1_test$Home_win, s = "lambda.min", family = "binomial")
  #print(coef(cv.lasso1, s = "lambda.min"))
  cv.lasso2 = cv.glmnet(as.matrix(X2_train[7:17]), X2_train$Home_win, 
                       family = "binomial", type.measure = "class", keep = TRUE)
  lasso2 = assess.glmnet(cv.lasso2, newx = as.matrix(X2_test[7:17]), 
                        newy = X2_test$Home_win, s = "lambda.1se", family = "binomial")
  test_coef = cbind(test_coef, coef(cv.lasso2, s = "lambda.1se"))
  
  simple = glm(Home_win ~ SRS.x + ORtg.x + DRtg.x + TS..x + MOV.x + SRS.y + ORtg.y + DRtg.y + TS..y + MOV.y + age_diff, data = X2_train, family = binomial)
  sim_pred = predict(simple, X2_test, type = "response")
  
  load.forest = randomForest(as.factor(Home_win) ~ ., data = X2_train[-c(1:5)], importance = TRUE, mtry = 6)
  varImpPlot(load.forest, type = 1)
  
  #modelr::rmse(load.forest, X_test)
  forest = predict(load.forest, newdata = X2_test[-c(1:5)], type = "prob")
  
  temp_test_results = data.frame(test_set[1], test_set[2], test_set[3], lasso1$class, lasso1$deviance, 
                                 lasso1$auc, lasso1$mse, lasso1$mae, lasso2$class, lasso2$deviance, 
                                 lasso2$auc, lasso2$mse, lasso2$mae, 
                                 sqrt(sum((forest[,2] - X1_test$Home_win)^2) / length(X1_test$Home_win)),
                                 sqrt(sum((sim_pred - X2_test$Home_win)^2) / length(X2_test$Home_win)))
  
  test_results = rbind(test_results, temp_test_results)
}
rm(i, X1_train, X1_test, X2_train, X2_test, test_set, temp_test_results, cv.lasso1, lasso1, cv.lasso2, lasso2)

mean(test_results[,7])
mean(test_results[,12])
mean(test_results[,14])
mean(test_results[,15])

simple = glm(Home_win ~ SRS.x + SRS.y + TS..x + TS..y, data = X2, family = binomial)
sim_pred = predict(simple, X2, type = "response")
sqrt(sum((sim_pred - X2$Home_win)^2) / length(X2$Home_win))





##### Bracket

series_winner <- function(team1, team2, all_matchups) {
  team1_winpct = filter(all_matchups, Home_TeamID == team1)[1,4]
  team2_winpct = filter(all_matchups, Home_TeamID == team2)[1,4]
  if (team1_winpct >= team2_winpct) {
    high_seed = team1
    low_seed = team2
    }
  else {
    low_seed = team1
    high_seed = team2
  }
  #print(c(team1,team2))
  prob_1257 = filter(all_matchups, Home_TeamID == high_seed, Away_TeamID == low_seed)[3]
  prob_346 = filter(all_matchups, Home_TeamID == low_seed, Away_TeamID == high_seed)[3]
  series = c(prob_1257, prob_1257, 1 - prob_346, 1 - prob_346, prob_1257, 1 - prob_346, prob_1257)
  if (sum(series >= runif(7, 0, 1)) >= 4) {return(high_seed)}
  else {return(low_seed)}
}

series_winner("PHX-2022", "NOP-2022", all_matchups)

playoff_bracket = data.frame(c("PHX-2022", "DAL-2022", "GSW-2022", "MEM-2022", "MIA-2022", "PHI-2022", 
                               "MIL-2022", "BOS-2022"), c("NOP-2022", "UTA-2022", "DEN-2022", "MIN-2022", "ATL-2022", "TOR-2022", 
                                                          "CHI-2022", "BKN-2022"), rep("",8))

playoff_bracket = data.frame(c("UTA-2021", "LAC-2021", "DEN-2021", "PHX-2021", "PHI-2021", "NYK-2021", 
                               "MIL-2021", "BKN-2021"), c("MEM-2021", "DAL-2021", "POR-2021", "LAL-2021", "WAS-2021", "ATL-2021", 
                                                          "MIA-2021", "BOS-2021"), rep("",8))

colnames(playoff_bracket) = c("Team1", "Team2", "Winner")

round_sim <- function(playoff_bracket, all_matchups){
  current_round = playoff_bracket %>% filter(Winner == "")

  for (i in 1:length(current_round$Team1)) {
    current_round[current_round$Team1 == current_round[i,1] & current_round$Team2 == current_round[i,2],3] = series_winner(current_round[i,1], current_round[i,2], all_matchups)
  }
  for (i in seq(1,length(current_round$Team1),2)) {
    current_round = rbind(current_round, c(current_round[i,3],current_round[i+1,3],""))
  }
  playoff_bracket = filter(playoff_bracket, Winner != "")
  playoff_bracket = rbind(playoff_bracket, current_round)
  return(playoff_bracket)
}

playoffs_sim <- function(playoff_bracket, all_matchups){
  playoff_result = round_sim(playoff_bracket, all_matchups)
  playoff_result = round_sim(playoff_result, all_matchups)
  playoff_result = round_sim(playoff_result, all_matchups)
  playoff_result[15,3] = series_winner(playoff_result[15,1], playoff_result[15,2],all_matchups)
  return(playoff_result)
}

playoffs_sim(playoff_bracket, all_matchups)

monte_carlo <- function(playoff_bracket, all_matchups, simulations) {
  output = data.frame()
  for (i in 1:simulations) {
    playoff_result = playoffs_sim(playoff_bracket, all_matchups)
    temp_output = c(playoff_result[15,3], playoff_result[14,3], playoff_result[13,3])
    output = rbind(output, temp_output)
    #print(i)
  }
  colnames(output) = c("NBA_Champion", "East_Champion", "West_Champion")
  return(output)
}

large_sim = monte_carlo(playoff_bracket, all_matchups, 100)

tab = apply(large_sim, 2, table)
right_join(data.frame(tab$NBA_Champion),data.frame(tab$East_Champion))

ggplot(data.frame(apply(large_sim, 2, table))) + geom_bar()

champ = large_sim %>% count(NBA_Champion, sort = TRUE) %>% mutate(Win_percentage = n / 100) %>% select(NBA_Champion, Win_percentage)
east = large_sim %>% count(East_Champion, sort = TRUE) %>% mutate(Win_percentage = n / 100) %>% select(East_Champion, Win_percentage)
west = large_sim %>% count(West_Champion, sort = TRUE) %>% mutate(Win_percentage = n / 100) %>% select(West_Champion, Win_percentage)
knitr::kable(list(champ,east,west))

large_sim_2021 = monte_carlo(playoff_bracket, all_matchups, 1000)

large_sim_2021 %>% count(NBA_Champion, sort = TRUE)


## Additional data utilized for testing but not included in final report

#####
### Expanded Standings Merging/Cleaning
#####

expanded_standings = data.frame()

for (year in seq(2007,2022)){
  temp_expanded_standings = read.csv(paste("C:/Users/tneal/OneDrive/Desktop/Spring_22/python/final_project_data/expanded_standings/", 
                                           year,".csv", sep = ""), header = TRUE)
  
  if (!("Oct" %in% colnames(temp_expanded_standings))) {
    temp_expanded_standings$Oct = ""
  }
  if (!("Nov" %in% colnames(temp_expanded_standings))) {
    temp_expanded_standings$Nov = ""
  }
  if (!("Apr" %in% colnames(temp_expanded_standings))) {
    temp_expanded_standings$Apr = ""
  }
  if (!("May" %in% colnames(temp_expanded_standings))) {
    temp_expanded_standings$May = ""
  }
  if (!("Jul" %in% colnames(temp_expanded_standings))) {
    temp_expanded_standings$Jul = ""
  }
  if (!("Aug" %in% colnames(temp_expanded_standings))) {
    temp_expanded_standings$Aug = ""
  }
  
  temp_expanded_standings$year = year
  
  expanded_standings = rbind(expanded_standings, temp_expanded_standings)
  
}
rm(temp_expanded_standings, year)

expanded_standings = merge(expanded_standings, team_abbreviations, by = "Team")

expanded_standings = expanded_standings %>% mutate(Home_TeamID = paste(Abbreviation, 
                                                                       year, sep="-"))
expanded_standings$Away_TeamID = expanded_standings$Home_TeamID
expanded_standings <- select(expanded_standings, -c(Rk, Abbreviation))

expanded_standings[c("Overall","Home","Road","Pre","Post","MOV_under_3","MOV_over_10")] <- 
  apply(expanded_standings[c("Overall","Home","Road","Pre","Post","MOV_under_3","MOV_over_10")], 
        c(1,2), win_pct)

expanded_standings = expanded_standings %>% mutate(Home_adv = Home / Road,
                                                   Trend = Post / Pre)
write.csv(expanded_standings, paste(here(), "data", "clean", "expanded_standings.csv", sep = "/"), row.names = FALSE)

#####
### team vs team Merging/Cleaning
#####

team_vs_team = data.frame()
old_abbr = c("NJN", "NOK", "NOH", "SEA", "PHO", "BRK", "CHO")
new_abbr = c("BKN", "NOP", "NOP", "OKC", "PHX", "BKN", "CHA")

for (year in seq(2007,2022)){
  temp_team_vs_team = read.csv(paste("C:/Users/tneal/OneDrive/Desktop/Spring_22/python/final_project_data/team_vs_team/", 
                                     year,".csv", sep = ""), header = TRUE)
  temp_replace = old_abbr %in% colnames(temp_team_vs_team)
  temp_team_vs_team = temp_team_vs_team %>% 
    rename_with(~ new_abbr[temp_replace], all_of(old_abbr[temp_replace]))
  temp_team_vs_team$year = year
  team_vs_team = rbind(team_vs_team, temp_team_vs_team)
  
}
rm(temp_team_vs_team, year, old_abbr, new_abbr, temp_replace)

team_vs_team = merge(team_vs_team, team_abbreviations, by = "Team")

team_vs_team = team_vs_team %>% mutate(Home_TeamID = paste(Abbreviation, 
                                                           year, sep="-"))

team_vs_team[3:32] <- apply(team_vs_team[,3:32], c(1,2), win_pct)

team_vs_team <- select(team_vs_team, -c(Rk, Abbreviation))


```{r pct-table-2022, echo = FALSE, fig.align = 'center'}

TeamIDs_2022 = c("ATL-2022", "BOS-2022", "CHI-2022", "DAL-2022", "DEN-2022", 
                 "GSW-2022","MEM-2022", "MIA-2022","MIL-2022", "MIN-2022", 
                 "BKN-2022", "NOP-2022","PHI-2022", "PHX-2022","TOR-2022", 
                 "UTA-2022")

all_matchups = data.frame()
for (team in TeamIDs_2022) {
  temp_matchups = data.frame(rep(team, 15), TeamIDs_2022[-match(team, TeamIDs_2022)])
  
  all_matchups = rbind(all_matchups, temp_matchups)
}
rm(temp_matchups, team)
colnames(all_matchups) = c("Home_TeamID", "Away_TeamID")

all_matchups = merge(all_matchups, advanced_stats[c("SRS", "ORtg", "DRtg", "TS.", "Age",
                                                    "MOV", "Home_TeamID")], by = "Home_TeamID")

all_matchups = merge(all_matchups, advanced_stats[c("SRS", "ORtg", "DRtg", "TS.",
                                                    "Age",
                                                    "MOV","Away_TeamID")], 
                     by = "Away_TeamID")


all_matchups = all_matchups %>% mutate(age_diff = Age.x - Age.y) %>% select(-c(Age.x, Age.y))

all_matchups = merge(all_matchups, expanded_standings[c("Overall","Home_TeamID")], 
                     by = "Home_TeamID")

all_matchups = merge(all_matchups, expanded_standings[c("Overall","Away_TeamID")], 
                     by = "Away_TeamID")

all_matchups[3:13] = scale(all_matchups[3:13], center = TRUE, scale = TRUE)


pred = predict(cv.lasso, newx = as.matrix(all_matchups[3:13]), s = "lambda.min", 
               type = "response")

all_matchups$Home_win_prob = pred

all_matchups = all_matchups %>% select(Home_TeamID, Away_TeamID, Home_win_prob, Overall.x, Overall.y) %>%
  arrange(Home_TeamID, Away_TeamID)

playoff_bracket = data.frame(c("PHX-2022", "DAL-2022", "GSW-2022", "MEM-2022", "MIA-2022", "PHI-2022", 
                               "MIL-2022", "BOS-2022"), c("NOP-2022", "UTA-2022", "DEN-2022", "MIN-2022", "ATL-2022", "TOR-2022", 
                                                          "CHI-2022", "BKN-2022"), rep("",8))

colnames(playoff_bracket) = c("Team1", "Team2", "Winner")

large_sim = monte_carlo(playoff_bracket, all_matchups, 100)

champ = large_sim %>% count(NBA_Champion, sort = TRUE) %>% mutate(Win_percentage = n / 100) %>% select(NBA_Champion, Win_percentage)
east = large_sim %>% count(East_Champion, sort = TRUE) %>% mutate(Win_percentage = n / 100) %>% select(East_Champion, Win_percentage)
west = large_sim %>% count(West_Champion, sort = TRUE) %>% mutate(Win_percentage = n / 100) %>% select(West_Champion, Win_percentage)
knitr::kable(list(champ,east,west))

```