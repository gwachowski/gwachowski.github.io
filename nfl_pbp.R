install.packages("dplyr", verbose=T)
library(dplyr)
install.packages("rvest")
install.packages("tidyverse")
library(rvest)
library(tidyverse)
install.packages("ggimage")
library(ggimage)
install.packages('RCurl')
library(RCurl)
library(ggplot2)
install.packages("rsvg")
library(rsvg)

setwd("~/Desktop/Personal Analysis")
#read in dataset
nfl_pbp<-read.csv("reg_pbp_2019.csv")
#filter data by team and when they pass on first down
teams<-nfl_pbp%>%group_by(posteam)%>%
  filter(first_down_pass==1)%>%
  tally()
#filter data by team just for when they are on first down
teams2<-nfl_pbp%>%group_by(posteam)%>%
  filter(down==1)%>%
  tally()
#2nd down
teams2nd<-nfl_pbp%>%group_by(posteam)%>%
  filter(down==2)%>%
  tally()
#3rd down
teams3rd<-nfl_pbp%>%group_by(posteam)%>%
  filter(down==3)%>%
  tally()
#merge the datasets
teams_total<-merge(teams,teams2, by="posteam")
#rename columns to make sense
library(plyr)
teams_total<-rename(teams_total, c("n.x"="First Down Pass", "n.y"="Total First Downs"))
#create column for percent of first downs throwing
teams_total$perc<-teams_total$`First Down Pass`/teams_total$`Total First Downs`
#find average percent teams throw on first down
mean(teams_total$perc)
#create a new data set for teams that throw more than 3% higher than average
above<-teams_total%>%filter(perc>0.43)
#filter down to only scoring plays separately
score_fg<-nfl_pbp%>%group_by(posteam,field_goal_result)%>%
  filter(field_goal_result=='made')%>%
  tally()%>%
  mutate(fg_pts=n*3)%>%
  select(-c(field_goal_result,n))
score_pat<-nfl_pbp%>%group_by(posteam,extra_point_result)%>%
  filter(extra_point_result=='good')%>%
  tally()%>%
  select(-c(extra_point_result))
score_2pt<-nfl_pbp%>%group_by(posteam,two_point_conv_result)%>%
  filter(two_point_conv_result=='success')%>%
  tally()%>%
  mutate(twopt_pts=n*2)%>%
  select(-c(two_point_conv_result,n))
score_td<-nfl_pbp%>%group_by(posteam,touchdown)%>%
  filter(touchdown==1)%>%
  filter(fumble!=1)%>%
  filter(interception!=1)%>%
  filter(punt_blocked!=1)%>%
  tally()%>%
  mutate(td_pts=n*6)%>%
  select(-c(touchdown,n))
score_sfty<-nfl_pbp%>%group_by(defteam,safety)%>%
  filter(safety==1)%>%
  tally()%>%
  mutate(safety_pts=n*2)%>%
  select(-c(safety,n))
score_kick_punt<-nfl_pbp%>%group_by(posteam,return_touchdown)%>%
  filter(return_touchdown==1)%>%
  filter(play_type!='pass' & play_type!='no_play')%>%
  tally()%>%
  mutate(kick_punt_pts=n*6)%>%
  select(-c(return_touchdown,n))
score_pick6<-nfl_pbp%>%group_by(defteam,return_touchdown)%>%
  filter(return_touchdown==1)%>%
  filter(play_type=='pass'| play_type=='no_play')%>%
  tally()%>%
  mutate(pick6_pts=n*6)%>%
  select(-c(return_touchdown,n))
score_fmbl<-nfl_pbp%>%group_by(defteam, fumble)%>%
  filter(fumble==1)%>%
  filter(touchdown==1)%>%
  filter(play_type!='punt')%>%
  filter(punt_blocked!=1)%>%
  tally()%>%
  mutate(fmble_pts=n*6)%>%
  select(-c(fumble,n))
score_pnt_blk<-nfl_pbp%>%group_by(defteam,punt_blocked)%>%
  filter(punt_blocked==1)%>%
  filter(touchdown==1)%>%
  tally()%>%
  mutate(pntblk_pts=n*6)%>%
  select(-c(punt_blocked,n))
scoring<-merge(score_pat,score_fg, by='posteam')
scoring<-merge(scoring, score_td, by='posteam')
scoring<-merge(scoring, score_2pt, by='posteam', all=TRUE)
scoring<-merge(scoring, score_kick_punt, by='posteam', all=TRUE)
scoring<-merge(scoring, score_sfty, by.x = 'posteam', by.y='defteam', all=TRUE)
scoring<-merge(scoring, score_pick6, by.x = 'posteam', by.y='defteam', all=TRUE)
scoring<-merge(scoring, score_fmbl, by.x='posteam', by.y='defteam', all=TRUE)
scoring<-merge(scoring, score_pnt_blk, by.x='posteam', by.y='defteam', all=TRUE)
scoring[is.na(scoring)] <- 0
scoring<-scoring%>%
  mutate(total_points=n+fg_pts+td_pts+twopt_pts+kick_punt_pts+safety_pts+pick6_pts+fmble_pts+pntblk_pts)
#combine scoring by percentage of pass on first down and total first downs
scoring_total<-scoring%>%
  select(-c(n,fg_pts,td_pts,twopt_pts,kick_punt_pts,safety_pts,pick6_pts,fmble_pts,pntblk_pts))
everything<-merge(scoring_total,teams_total, by='posteam')
#Team offensive possessions
possessions<-nfl_pbp%>%group_by(posteam,game_id,drive)%>%
  distinct(posteam,game_id,drive)%>%
  group_by(posteam)%>%
  tally()%>%
  mutate(Possessions_total=n)%>%
  select(-c(n))
detach(package:plyr)
rushing<-nfl_pbp%>% group_by(posteam)%>%
  filter(play_type=='run')%>%
  select(posteam, yards_gained)%>%
  summarise(rushing_yrds=sum(yards_gained))

#Combine everything
everything<-merge(everything,possessions, by='posteam')
everything$POP<-everything$total_points/everything$Possessions_total
everything<-merge(everything, rushing, by='posteam')
mean(everything$POP)
#use NFL logo as points on graph with some updated urls
url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)
everything <- everything %>% 
  left_join(df.logos, by = c("posteam" = "team_code"))
everything[32,9]<-"https://sportsfly.cbsistatic.com/fly-827/bundles/sportsmediacss/images/team-logos/nfl/light/WAS.svg"
everything[25,8]<-"Las Vegas Raiders"
everything[25,9]<-"https://justblogbaby.com/files/2013/07/Raider-logo-4.png"
everything[31,9]<-"https://sportslogohistory.com/wp-content/uploads/2017/12/tennessee_titans_1999-pres.png"
everything[29,9]<-"https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/San_Francisco_49ers_logo.svg/800px-San_Francisco_49ers_logo.svg.png"
everything[3,9]<-"https://sportslogohistory.com/wp-content/uploads/2017/12/baltimore_ravens_1999-pres.png"
#plot
ggplot(everything, aes(POP, perc)) + 
  geom_image(aes(image = url), size = 0.055) + 
  theme(text = element_text(size=12))+
  labs(title = "First Down Pass Percentage and Offensive Efficiency (POP) in the Red Zone 2019", 
       subtitle = "Data courtesy: nflscrapR                 Dotted red lines: Means") + 
  xlab("Points per Offensive Possession, Mean=2.0086") + 
  ylab("Percent of First Down Possessions Passing in the Red Zone, Mean=.05") + 
  xlim(1.25, 3.25)+
  ylim(0.025,0.085)+
  geom_hline(aes(yintercept = .052), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = 2.0086), lty = 2, col = "red", alpha = 0.5)
#Find correlation, also removing outlier
cor(everything$perc,everything$POP)
everything_notBAL<-everything[-c(3),]
cor(everything_notBAL$perc,everything_notBAL$POP)

#improt QBR data for the season
qbr<-read.csv("NFL2019_QBR.csv")
qbr<-qbr%>%
  select(-c(RK, NAME, X, X.1, X.2, X.3, X.4))
qbr <- qbr[-c(33:61), ]
sup<-merge(everything, qbr, by.x="posteam", by.y="Team", all=TRUE)


ggplot(sup, aes(QBR, perc)) + 
  geom_image(aes(image = url), size = 0.055) + 
  theme(text = element_text(size=17))+
  labs(title = "First Down Pass Percentage and QBR 2019", 
       subtitle = "Data courtesy: nflscrapR     Red Dashed Line: Mean") + 
  xlab("QBR, Mean= 55.5281") + 
  ylab("Percent of First Down Possessions Passing, Mean=.3991") + 
  xlim(25,95)+
  ylim(0.325,0.485)+
  geom_hline(aes(yintercept = .3991), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept =55.5281), lty = 2, col = "red", alpha = 0.5)


cor(sup$QBR,sup$perc)
mean(sup$QBR)


everything$wins<-c(5,7,14,10,5,8,2,6,8,7,3,13,10,7,6,12,5,9,5,10,12,13,4,7,7,9,8,11,13,7,9,3)


cor(everything$wins,everything$POP)
mean(everything$wins)
range(everything$wins)
mean(everything$POP)
range(everything$POP)

ggplot(everything, aes(wins, POP)) + 
  geom_image(aes(image = url), size = 0.055) + 
  theme(text = element_text(size=17))+
  labs(title = "Wins and Offensive Efficiency", 
       subtitle = "Data courtesy: nflscrapR    Red Dashed Line: Mean") + 
  xlab("Wins, Mean= 7.9688") + 
  ylab("Offensive Efficiency (POP), Mean=2.0086") + 
  xlim(0,16)+
  ylim(1,3.5)+
  geom_hline(aes(yintercept = 2.0086), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept =7.9688), lty = 2, col = "red", alpha = 0.5)






cor(everything$POP,everything$rushing_yrds)
mean(everything$rushing_yrds)
range(everything$rushing_yrds)

ggplot(everything, aes(rushing_yrds, POP)) + 
  geom_image(aes(image = url), size = 0.055) + 
  theme(text = element_text(size=17))+
  labs(title = "Total Rushing Yards and Offensive Efficiency (POP)", 
       subtitle = "Data courtesy: nflscrapR    Red Dashed Line: Mean") + 
  xlab("Total Rushing Yards, Mean=1819.844") + 
  ylab("Offensive Efficiency (POP), Mean=2.0086") + 
  xlim(1150,3350)+
  ylim(1,3.5)+
  geom_hline(aes(yintercept = 2.0086), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = 1819.844), lty = 2, col = "red", alpha = 0.5)



plot(everything$r)

#offensive line versus total team rushing yards
off_rushing<-read.csv("offline_rushing.csv")
ggplot(off_rushing, aes(Rushing.Yards,Rank)) + 
  geom_image(aes(image = X), size = 0.055) + 
  theme(text = element_text(size=17))+
  labs(title = "Total Rushing Yards and Offensive Line Ranking", 
       subtitle = "Data courtesy: 4for4 and ESPN    Red Dashed Line: Mean") + 
  xlab("Total Rushing Yards, Mean=1806.438") + 
  ylab("Offensive Line Rank") + 
  xlim(1150,3350)+
  ylim(1,32)+
  geom_vline(aes(xintercept = 1806.438), lty = 2, col = "red", alpha = 0.5)


#rushers dyar versus pop
rusher_pop<-read.csv("rushers_pop.csv")
ggplot(rusher_pop, aes(DYAR,POP)) + 
  geom_image(aes(image = URL), size = 0.055) + 
  theme(text = element_text(size=17))+
  labs(title = "DYAR for Team's Top Rusher versus Offensive Efficiency", 
       subtitle = "Data courtesy: Football Outsiders and ESPN    Red Dashed Line: Mean") + 
  xlab("Defense-adjusted Yards Above Replacement, Mean=93.40625") + 
  ylab("Offensive Efficiency (POP), Mean=2.0086") + 
  xlim(-80,325)+
  ylim(1,3.5)+
  geom_hline(aes(yintercept = 2.0086), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = 93.40625), lty = 2, col = "red", alpha = 0.5)

def_wins<-read.csv("def_wins.csv")
ggplot(def_wins, aes(DEFENSE.DVOA,Wins)) + 
  geom_image(aes(image = URL), size = 0.055) + 
  theme(text = element_text(size=17))+
  labs(title = "Defense DVOA versus Season Win Total 2019", 
       subtitle = "Data courtesy: Football Outsiders and ESPN    Red Dashed Line: Mean") + 
  xlab("Defense-adjusted Value Over Average (%), Mean=-0.125%") + 
  ylab("Season Win Total, Mean=7.96875") + 
  xlim(-30,25)+
  ylim(0,16)+
  geom_hline(aes(yintercept = 7.96875), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = -0.125), lty = 2, col = "red", alpha = 0.5)



