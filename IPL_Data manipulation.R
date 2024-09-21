library(dplyr)

balls<- IPL_Ball_by_Ball_2008_2020
match<- IPL_Matches_2008_2020

# Keep only the first innings of each match
balls_in1 <- balls %>% 
  filter(inning == 1) 

# Predictor 1: Total runs scored in the innings (continuous variable)
runs_total <- balls_in1 %>% 
  group_by(id) %>% 
  summarise(runs_tot = sum(total_runs))

# Predictor 2: Number of runs scored in the first six overs of the innings (the powerplay, when there are fielding restrictions; continuous variable)
runs_pplay <- balls_in1 %>% 
  filter(over < 7) %>%
  group_by(id) %>% 
  summarise(runs_pp = sum(total_runs))

# Predictor 3: Number of dot balls in the innings (continuous variable)
zeros <- balls_in1 %>% 
  filter(total_runs == 0) %>% 
  group_by(id) %>% 
  summarise(zeros = n())

# Predictor 4: Number of singles in the innings (balls where 1 run is scored; continuous variable)
ones <- balls_in1 %>% 
  filter(batsman_runs == 1) %>% 
  group_by(id) %>% 
  summarise(ones = n())

# Predictor 5: Number of twos in the innings (continuous variable)
twos <- balls_in1 %>% 
  filter(batsman_runs == 2) %>% 
  group_by(id) %>% 
  summarise(twos = n())

# Predictor 6: Number of threes in the innings (continuous variable)
threes <- balls_in1 %>% 
  filter(batsman_runs == 3) %>% 
  group_by(id) %>% 
  summarise(threes = n())

#Predictor 7: Number of fours in the innings (continuous variables)
fours <- balls_in1 %>% 
  filter(batsman_runs == 4) %>% 
  group_by(id) %>% 
  summarise(fours = n())

# Predictor 8: Number of sixes in the innings (continuous variables)
sixes <- balls_in1 %>% 
  filter(batsman_runs == 6) %>% 
  group_by(id) %>% 
  summarise(sixes = n())

# Predictor 9: Number of wickets in the innings (continuous variable)
wks_total <- balls_in1 %>% 
  filter(is_wicket == 1) %>% 
  group_by(id) %>% 
  summarise(wks_tot = n())

# Predictor 10: Number of wickets in the powerplay (continuous variable)
wks_pplay <- balls_in1 %>% 
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
                                                                                                      full_join(wks_pplay,
                                                                                                                full_join(home, by = "id"),
                                                                                                                by = "id"),
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

# Analysis on teams
data_1<-match[,c("team1","team2","toss_winner","toss_decision","winner","neutral_venue")]
View(data_1)

# Analysis on RCB
RCB_data<-filter(data_1,team1=="Royal Challengers Bangalore"|team2=="Royal Challengers Bangalore")
View(RCB_data)
table(RCB_data$winner)

rcb_winboth<-filter(RCB_data,toss_winner=="Royal Challengers Bangalore" & winner=="Royal Challengers Bangalore")
View(rcb_winboth)
nrow(rcb_winboth)
# 43 times RCB wins both in toss and match


rcb_wintossloosematch<- filter(RCB_data,toss_winner=="Royal Challengers Bangalore" & winner!="Royal Challengers Bangalore")
View(rcb_wintossloosematch)


rcb_loosetosswinmatch<-filter(RCB_data,toss_winner!="Royal Challengers Bangalore" & winner=="Royal Challengers Bangalore")
View(rcb_loosetosswinmatch)

rcb_looseboth<-filter(RCB_data,toss_winner!="Royal Challengers Bangalore" & winner!="Royal Challengers Bangalore")
View(rcb_looseboth)

analysis<-c(rcb_win_both=nrow(rcb_winboth),
                    rcb_win_toss_loose_match=nrow(rcb_wintossloosematch),
                     rcb_loose_toss_win_match=nrow(rcb_loosetosswinmatch),
                      rcb_loose_both=nrow(rcb_looseboth))
barplot(analysis,col = c("red","green","yellow","blue"),main = "Analysis on RCB",ylab="No of matches RCB played")


# Analysis on KKR
KKR_data<-filter(data_1,team1=="Kolkata Knight Riders"|team2=="Kolkata Knight Riders")
View(KKR_data)
table(KKR_data$winner)

kkr_winboth<-filter(KKR_data,toss_winner=="Kolkata Knight Riders" & winner=="Kolkata Knight Riders")
View(kkr_winboth)
nrow(kkr_winboth)
# 55 times KKR wins both in toss and match


kkr_wintossloosematch<- filter(KKR_data,toss_winner=="Kolkata Knight Riders" & winner!="Kolkata Knight Riders")
View(kkr_wintossloosematch)


kkr_loosetosswinmatch<-filter(KKR_data,toss_winner!="Kolkata Knight Riders" & winner=="Kolkata Knight Riders")
View(kkr_loosetosswinmatch)

kkr_looseboth<-filter(KKR_data,toss_winner!="Kolkata Knight Riders" & winner!="Kolkata Knight Riders")
View(kkr_looseboth)

analysis<-c(kkr_win_both=nrow(kkr_winboth),
            kkr_win_toss_loose_match=nrow(kkr_wintossloosematch),
            kkr_loose_toss_win_match=nrow(kkr_loosetosswinmatch),
            kkr_loose_both=nrow(kkr_looseboth))
barplot(analysis,col = c("violet","pink","blue","yellow"),main = "Analysis on KKR",ylab="No of matches KKR played")



# Analysis on CSK
CSK_data<-filter(data_1,team1=="Chennai Super Kings"|team2=="Chennai Super Kings")
View(CSK_data)
table(CSK_data$winner)

csk_winboth<-filter(CSK_data,toss_winner=="Chennai Super Kings" & winner=="Chennai Super Kings")
View(csk_winboth)
nrow(csk_winboth)

# 61 times CSK wins both in toss and match

csk_wintossloosematch<- filter(CSK_data,toss_winner=="Chennai Super Kings" & winner!="Chennai Super Kings")
View(csk_wintossloosematch)


csk_loosetosswinmatch<-filter(CSK_data,toss_winner!="Chennai Super Kings" & winner=="Chennai Super Kings")
View(csk_loosetosswinmatch)

csk_looseboth<-filter(CSK_data,toss_winner!="Chennai Super Kings" & winner!="Chennai Super Kings")
View(csk_looseboth)

analysis<-c(csk_win_both=nrow(csk_winboth),
            csk_win_toss_loose_match=nrow(csk_wintossloosematch),
            csk_loose_toss_win_match=nrow(csk_loosetosswinmatch),
            csk_loose_both=nrow(csk_looseboth))
barplot(analysis,col = c("black","green","skyblue","red"),main = "Analysis on CSK",ylab="No of matches CSK played")



# Analysis on KXIP
KXIP_data<-filter(data_1,team1=="Kings XI Punjab"|team2=="Kings XI Punjab")
View(KXIP_data)
table(KXIP_data$winner)

kxip_winboth<-filter(KXIP_data,toss_winner=="Kings XI Punjab" & winner=="Kings XI Punjab")
View(kxip_winboth)
nrow(kxip_winboth)
# 36 times KXIP wins both in toss and match


kxip_wintossloosematch<- filter(KXIP_data,toss_winner=="Kings XI Punjab" & winner!="Kings XI Punjab")
View(kxip_wintossloosematch)


kxip_loosetosswinmatch<-filter(KXIP_data,toss_winner!="Kings XI Punjab" & winner=="Kings XI Punjab")
View(kxip_loosetosswinmatch)

kxip_looseboth<-filter(KXIP_data,toss_winner!="Kings XI Punjab" & winner!="Kings XI Punjab")
View(kxip_looseboth)

analysis<-c(kxip_win_both=nrow(kkr_winboth),
            kxip_win_toss_loose_match=nrow(kkr_wintossloosematch),
            kxip_loose_toss_win_match=nrow(kkr_loosetosswinmatch),
            kxip_loose_both=nrow(kkr_looseboth))
barplot(analysis,col = c("violet","green","blue","yellow"),main = "Analysis on KXIP",ylab="No of matches KXIP played")




# Analysis on MI
MI_data<-filter(data_1,team1=="Mumbai Indians"|team2=="Mumbai Indians")
View(MI_data)
table(MI_data$winner)

mi_winboth<-filter(MI_data,toss_winner=="Mumbai Indians" & winner=="Mumbai Indians")
View(mi_winboth)
nrow(mi_winboth)
# 61 times MI wins both in toss and match


mi_wintossloosematch<- filter(MI_data,toss_winner=="Mumbai Indians" & winner!="Mumbai Indians")
View(mi_wintossloosematch)


mi_loosetosswinmatch<-filter(MI_data,toss_winner!="Mumbai Indians" & winner=="Mumbai Indians")
View(mi_loosetosswinmatch)

mi_looseboth<-filter(MI_data,toss_winner!="Mumbai Indians" & winner!="Mumbai Indians")
View(mi_looseboth)

analysis<-c(mi_win_both=nrow(mi_winboth),
            mi_win_toss_loose_match=nrow(mi_wintossloosematch),
            mi_loose_toss_win_match=nrow(mi_loosetosswinmatch),
            mi_loose_both=nrow(mi_looseboth))
barplot(analysis,col = c("yellow","blue","orange","green"),main = "Analysis on MI",ylab="No of matches MI played")


# Analysis on RR
RR_data<-filter(data_1,team1=="Rajasthan Royals"|team2=="Rajasthan Royals")
View(RR_data)
table(RR_data$winner)

rr_winboth<-filter(RR_data,toss_winner=="Rajasthan Royals" & winner=="Rajasthan Royals")
View(rr_winboth)
nrow(rr_winboth)
# 44 times RR wins both in toss and match


rr_wintossloosematch<- filter(RR_data,toss_winner=="Rajasthan Royals" & winner!="Rajasthan Royals")
View(rr_wintossloosematch)


rr_loosetosswinmatch<-filter(RR_data,toss_winner!="Rajasthan Royals" & winner=="Rajasthan Royals")
View(rr_loosetosswinmatch)

rr_looseboth<-filter(RR_data,toss_winner!="Rajasthan Royals" & winner!="Rajasthan Royals")
View(rr_looseboth)

analysis<-c(rr_win_both=nrow(rr_winboth),
            rr_win_toss_loose_match=nrow(rr_wintossloosematch),
            rr_loose_toss_win_match=nrow(rr_loosetosswinmatch),
            rr_loose_both=nrow(rr_looseboth))
barplot(analysis,col = c("yellow","maroon","blue","green"),main = "Analysis on RR",ylab="No of matches RR played")


# Analysis on DC
DC_data<-filter(data_1,team1=="Deccan Chargers"|team2=="Deccan Chargers")
View(DC_data)
table(DC_data$winner)

dc_winboth<-filter(DC_data,toss_winner=="Deccan Chargers" & winner=="Deccan Chargers")
View(dc_winboth)
nrow(dc_winboth)
# 19 times DC wins both in toss and match


dc_wintossloosematch<- filter(DC_data,toss_winner=="Deccan Chargers" & winner!="Deccan Chargers")
View(dc_wintossloosematch)


dc_loosetosswinmatch<-filter(DC_data,toss_winner!="Deccan Chargers" & winner=="Deccan Chargers")
View(dc_loosetosswinmatch)

dc_looseboth<-filter(DC_data,toss_winner!="Deccan Chargers" & winner!="Deccan Chargers")
View(dc_looseboth)

analysis<-c(dc_win_both=nrow(dc_winboth),
            dc_win_toss_loose_match=nrow(dc_wintossloosematch),
            dc_loose_toss_win_match=nrow(dc_loosetosswinmatch),
            dc_loose_both=nrow(dc_looseboth))
barplot(analysis,col = c("orange","green","yellow","blue"),main = "Analysis on DC",ylab="No of matches DC played")





# Analysis on SH
SH_data<-filter(data_1,team1=="Sunrisers Hyderabad"|team2=="Sunrisers Hyderabad")
View(SH_data)
table(SH_data$winner)

sh_winboth<-filter(SH_data,toss_winner=="Sunrisers Hyderabad" & winner=="Sunrisers Hyderabad")
View(sh_winboth)
nrow(sh_winboth)
# 29 times SH wins both in toss and match


sh_wintossloosematch<- filter(SH_data,toss_winner=="Sunrisers Hyderabad" & winner!="Sunrisers Hyderabad")
View(sh_wintossloosematch)


sh_loosetosswinmatch<-filter(SH_data,toss_winner!="Sunrisers Hyderabad" & winner=="Sunrisers Hyderabad")
View(sh_loosetosswinmatch)

sh_looseboth<-filter(SH_data,toss_winner!="Sunrisers Hyderabad" & winner!="Sunrisers Hyderabad")
View(sh_looseboth)

analysis<-c(sh_win_both=nrow(sh_winboth),
            sh_win_toss_loose_match=nrow(sh_wintossloosematch),
            sh_loose_toss_win_match=nrow(sh_loosetosswinmatch),
            sh_loose_both=nrow(sh_looseboth))
barplot(analysis,col = c("orange","green","pink","gray"),main = "Analysis on SH",ylab="No of matches SH played")


#Analysis on DD / DC
match$team1[match$team1 =="Delhi Daredevils"]<-'DD'
match$team2[match$team2 =="Delhi Daredevils"]<-'DD'
match$team1[match$team1 =="Delhi Capitals"]<-'DD'
match$team2[match$team2 =="Delhi Capitals"]<-'DD'

match$toss_winner[match$toss_winner =="Delhi Daredevils"]<-'DD'
match$toss_winner[match$toss_winner =="Delhi Capitals"]<-'DD'
match$winner[match$winner =="Delhi Daredevils"]<-'DD'
match$winner[match$winner =="Delhi Capitals"]<-'DD'
View(match)
DD_data<-filter(match,team1=="DD"|team2=="DD")
View(DD_data)
table(DD_data$winner)

dd_winboth<-filter(DD_data,toss_winner=="DD" & winner=="DD")
View(dd_winboth)
nrow(dd_winboth)
# 45 times DD wins both in toss and match


dd_wintossloosematch<- filter(DD_data,toss_winner=="DD" & winner!="DD")
View(dd_wintossloosematch)


dd_loosetosswinmatch<-filter(DD_data,toss_winner!="DD" & winner=="DD")
View(dd_loosetosswinmatch)

dd_looseboth<-filter(DD_data,toss_winner!="DD" & winner!="DD")
View(dd_looseboth)

analysis<-c(dd_win_both=nrow(dd_winboth),
            dd_win_toss_loose_match=nrow(dd_wintossloosematch),
            dd_loose_toss_win_match=nrow(dd_loosetosswinmatch),
            dd_loose_both=nrow(dd_looseboth))
barplot(analysis,col = c("black","yellow","red","green"),main = "Analysis on DD",ylab="No of matches DD played")


#Team with most win / Most successful team

match%>%
  group_by(winner)%>%summarise(winner_cnt=n())%>%filter(winner_cnt==max(winner_cnt))

#So, MI is most sussessful team

#Piechart for team who won most matches
match%>%
  filter(result!='no result')%>% group_by(winner)%>%
  summarise(win=n())%>%arrange(desc(win))
x<- table(match$winner)  
y<-sort(x)
pie(y,main = "Match winner")  

#Best batsman still
match%>% 
  filter(result != 'no result') %>% group_by(player_of_match) %>% 
  summarise(win = n()) %>% arrange(desc(win))
a<- table(match$player_of_match)
b<-sort(a,decreasing = TRUE)
c<- head(b,10)
c
barplot(c,main = "Best Batsman",xlab = "Name of Batsmans",ylab ="No of times a player selected as a player of the match" )

#Best bowler Still

s<-balls%>%
  group_by(bowler) %>% 
  filter(is_wicket==1)%>%
  summarise(wickets=n())%>%arrange(desc(wickets))
s
t<-head(s,10)
t

#Top 10 batsman with most number of 6s in IPL
m<-balls%>%
    group_by(batsman)%>%
    filter(batsman_runs=="6")%>%
    summarise(sixs=n())
m
n<-arrange(m,desc(sixs))
o<-head(n,10)
o


#Which team is dominating in a certain location ?
home_ground_advantage_1<-match%>% 
  filter(result != 'no result') %>% group_by(winner,city) %>% 
  summarise(win = n()) %>% arrange(desc(win))
View(home_ground_advantage_1)
home_ground_advantage<- head(home_ground_advantage_1,15)
home_ground_advantage

#Batsman with most runs
batting_leaders<- summarise(group_by(balls,batsman),batting_leaders=sum(batsman_runs))
batting_leaders
batting_leaders1<-arrange(batting_leaders,desc(batting_leaders))
top_ten_batsmans<-head(batting_leaders1,10)
top_ten_batsmans


































