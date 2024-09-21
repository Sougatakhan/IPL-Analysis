library(dplyr)

balls<- IPL_Ball_by_Ball_2008_2020
match<- IPL_Matches_2008_2020

# Keep only the first innings of each match
balls <- balls %>% 
  filter(inning == 1)

# Predictor 1: Total runs scored in the innings (continuous variable)
runs_total <- balls %>% 
  group_by(id) %>% 
  summarise(runs_tot = sum(total_runs))

# Predictor 2: Number of runs scored in the first six overs of the innings (the powerplay, when there are fielding restrictions; continuous variable)
runs_pplay <- balls %>% 
  filter(over < 7) %>%
  group_by(id) %>% 
  summarise(runs_pp = sum(total_runs))

# Predictor 3: Number of dot balls in the innings (continuous variable)
zeros <- balls %>% 
  filter(total_runs == 0) %>% 
  group_by(id) %>% 
  summarise(zeros = n())

# Predictor 4: Number of singles in the innings (balls where 1 run is scored; continuous variable)
ones <- balls %>% 
  filter(batsman_runs == 1) %>% 
  group_by(id) %>% 
  summarise(ones = n())

# Predictor 5: Number of twos in the innings (continuous variable)
twos <- balls %>% 
  filter(batsman_runs == 2) %>% 
  group_by(id) %>% 
  summarise(twos = n())

# Predictor 6: Number of threes in the innings (continuous variable)
threes <- balls %>% 
  filter(batsman_runs == 3) %>% 
  group_by(id) %>% 
  summarise(threes = n())

#Predictor 7: Number of fours in the innings (continuous variables)
fours <- balls %>% 
  filter(batsman_runs == 4) %>% 
  group_by(id) %>% 
  summarise(fours = n())

# Predictor 8: Number of sixes in the innings (continuous variables)
sixes <- balls %>% 
  filter(batsman_runs == 6) %>% 
  group_by(id) %>% 
  summarise(sixes = n())

# Predictor 9: Number of wickets in the innings (continuous variable)
wks_total <- balls %>% 
  filter(is_wicket == 1) %>% 
  group_by(id) %>% 
  summarise(wks_tot = n())

# Predictor 10: Number of wickets in the powerplay (continuous variable)
wks_pplay <- balls %>% 
  filter(over < 7 & is_wicket == 1) %>% 
  group_by(id) %>% 
  summarise(wks_pp = n())


# Predictor 11: Home ground advantage
home <- match %>% 
  mutate(batting_team = if_else(toss_decision == "bat", toss_winner, 
                                if_else(toss_winner == team1, team2, team1)),
         home = if_else(neutral_venue == 1, "Away", if_else(batting_team == team1, "Home", "Away")),
         home = factor(home, levels = c("Away", "Home"))) %>% 
  dplyr::select(id, home)  

# Merge all predictors into one data frame

new_data <- full_join(runs_total,
                      full_join(runs_pplay,
                                full_join(zeros,
                                          full_join(ones,
                                                    full_join(twos,
                                                              full_join(threes,
                                                                        full_join(fours,
                                                                                  full_join(sixes, 
                                                                                            full_join(wks_total, 
                                                                                                                full_join(home,wks_pplay, by = "id"),
                                                                                                      by = "id"),
                                                                                            by = "id"),
                                                                                  by = "id"),
                                                                        by = "id"),
                                                              by = "id"),
                                                    by = "id"),
                                          by = "id"),
                                by = "id"),
                      by = "id")
View(new_data)


# Make a column named Result, and make results(win or loose with respect to bat 1st) categorical
    new_data$Result<- if_else(match$result=="runs",1,0)
#Only keep matches that weren't ties, abandoned, or affected by rain
    
    
    match<-    match%>%
      filter(result == "runs" | result == "wickets")
    View(match)   
    View(new_data)       
 View(new_data)

# What is a winning score when batting first in the IPL?
summary(new_data$runs_tot)

model1<-glm(Result~runs_tot,family = "binomial",data = new_data)

summary(model1)

# Regression formula, based on the results of the summary coefficients
p <- exp(-5.581237 + 0.032513*new_data$runs_tot)/(1 + exp(-5.531237 + 0.032513*new_data$runs_tot))
win_prop <- cbind(new_data$runs_tot, p)
plot(win_prop,xlab = "Total Innnings runs",ylab = "Proportion of matches won",main = "Total Runs Vs Proportion of Matches Won")
  