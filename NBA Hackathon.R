##2a
#import data
setwd("/Users/Sonya/Desktop")
winprobdata = read.csv("win_prob.csv")
winprobdata = as.numeric(unlist(winprobdata))
winprobdata = matrix(winprobdata, ncol = 120, byrow = TRUE)
winprobdata = t(winprobdata)
winprobdata[,2] = winprobdata[,2] + 1

#list all possible permutations of win/lose when having exactly 4,5,6,7 games
#no matter which team win
perall = expand.grid(rep(list(0:1),7))
per = subset(perall,rowSums(perall)==4)
win_4 = subset(per[1:4],rowSums(per[1:4])==4)
win_5 = subset(per[1:5],rowSums(per[1:5])==4&per[5]==1)
win_6 = subset(per[1:6],rowSums(per[1:6])==4&per[6]==1)
win_7 = subset(per,rowSums(per[1:7])==4&per[7]==1)

#define function to transfer the win/lose pertumitations to possibilities
#for any team (wp for the team win; lp for the team lose)
#calculate probabilities of win/lose with exactly 4,5,6,7 games
#(exawp for the team win; exalp for the team lose)
wp = function(home,away,winper) {
#to specify situations of equal seeds and west teams ranks higher than east teams  
  if ( home - away >= 4 & home - away <= 8 ) {
    wpindex = which(winprobdata[,1] == away & winprobdata[,2] == home)
    p_h = winprobdata[wpindex,4]
    p_a = winprobdata[wpindex,3]
  } else {
    wpindex = which(winprobdata[,1] == home & winprobdata[,2] == away)
    p_h = winprobdata[wpindex,3]
    p_a = winprobdata[wpindex,4]
  }
  
  series_prob = c(p_h,p_h,p_a,p_a,p_h,p_a,p_h)
  reser_prob = 1-series_prob
  
  winno = ncol(winper)
  for (i in 1:winno) {
    winper[,i] = replace(winper[,i],winper[,i]==1,series_prob[i])
    winper[,i] = replace(winper[,i],winper[,i]==0,reser_prob[i])
  }
  return(winper)
}

exawp = function(home, away) {
  win_E1E8_4 = wp(home,away,win_4)
  wp_E1E8_4 = sum(apply(win_E1E8_4,1,prod))
  win_E1E8_5 = wp(home,away,win_5)
  wp_E1E8_5 = sum(apply(win_E1E8_5,1,prod))
  win_E1E8_6 = wp(home,away,win_6)
  wp_E1E8_6 = sum(apply(win_E1E8_6,1,prod))
  win_E1E8_7 = wp(home,away,win_7)
  wp_E1E8_7 = sum(apply(win_E1E8_7,1,prod))
  exactw = c(wp_E1E8_4,wp_E1E8_5,wp_E1E8_6,wp_E1E8_7)
  return(exactw)
}

perall = expand.grid(rep(list(0:1),7))
per = subset(perall,rowSums(perall)==4)
win_4 = subset(per[1:4],rowSums(per[1:4])==4)
win_5 = subset(per[1:5],rowSums(per[1:5])==4&per[5]==1)
win_6 = subset(per[1:6],rowSums(per[1:6])==4&per[6]==1)
win_7 = subset(per,rowSums(per[1:7])==4&per[7]==1)

lp = function(home,away,winper) {

  if ( home - away >= 4 & home - away <= 8) {
    wpindex = which(winprobdata[,1] == away & winprobdata[,2] == home)
    p_h = winprobdata[wpindex,4]
    p_a = winprobdata[wpindex,3]
  } else {
    wpindex = which(winprobdata[,1] == home & winprobdata[,2] == away)
    p_h = winprobdata[wpindex,3]
    p_a = winprobdata[wpindex,4]
  }
  
  series_prob = c(p_h,p_h,p_a,p_a,p_h,p_a,p_h)
  reser_prob = 1-series_prob
  
  winno = ncol(winper)
  for (i in(1:winno)) {
    winper[,i] = replace(winper[,i],winper[,i]==0,series_prob[i])
    winper[,i] = replace(winper[,i],winper[,i]==1,1-series_prob[i])
  }
  return(winper)
}


exalp = function(home, away){
  lose_E1E8_4 = lp(home,away,win_4)
  lp_E1E8_4 = sum(apply(lose_E1E8_4,1,prod))
  lose_E1E8_5 = lp(home,away,win_5)
  lp_E1E8_5 = sum(apply(lose_E1E8_5,1,prod))
  lose_E1E8_6 = lp(home,away,win_6)
  lp_E1E8_6 = sum(apply(lose_E1E8_6,1,prod))
  lose_E1E8_7 = lp(home,away,win_7)
  lp_E1E8_7 = sum(apply(lose_E1E8_7,1,prod))
  exactl = c(lp_E1E8_4,lp_E1E8_5,lp_E1E8_6,lp_E1E8_7)
  return(exactl)
}

#To answer question 2a
exawp(1,8) + exalp(1,8)
#0.848751096 0.139566872 0.010671220 0.001010811


##2b
#import data
#transform the team info to numbers
#1-8 denotes East1 to East8; 9-17 denotes West1 to West8
#for equal seeds, just enter West1(9) as home while East1(1) as away
setwd("/Users/Sonya/Desktop")
busdata = read.csv("Business-Track.csv")
busdata = unlist(busdata)
busdata = as.numeric(busdata)
busdata = matrix(busdata, ncol = 16, byrow = TRUE)
busdata = t(busdata)
Revenue = busdata[1:16,2:5]

#install.packages("LaplacesDemon")
library(LaplacesDemon)

#Simulator for the Gate_Revenue for any round
simR = function(home,away,round){
  homeR = Revenue[home,round]
  awayR = Revenue[away,round]
  totalR = c(2 * homeR + 2 * awayR,
             3 * homeR + 2 * awayR,
             3 * homeR + 3 * awayR,
             4 * homeR + 3 * awayR)
  
  wexpR = sum(totalR * exawp(home,away))
  lexpR = sum(totalR * exalp(home,away))
  berp = sum(exawp(home,away))
  worl = rbern(1,berp)
  if (worl==1) {
    return(wexpR)
  }else{
    return(lexpR)
  }
}
#for example, East1(1) against East8(8) for first round
#simR(1,8,1)
#[1] 8823256
# for another example, East1(1) against West1(9) for the fourth round
#simR(9,1,4)
#[1] 32173774


##2d





