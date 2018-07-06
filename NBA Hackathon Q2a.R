##2a
#list all possible permutations of win/lose when having exactly 4,5,6,7 games
#no matter which team win
perall = expand.grid(rep(list(0:1),7))
per = subset(perall,rowSums(perall)==4)
win_4 = subset(per[1:4],rowSums(per[1:4])==4)
win_5 = subset(per[1:5],rowSums(per[1:5])==4&per[5]==1)
win_6 = subset(per[1:6],rowSums(per[1:6])==4&per[6]==1)
win_7 = subset(per,rowSums(per[1:7])==4&per[7]==1)

#define function to transfer the win/lose pertumitations to possibilities
#for any team
#wpm for the team win
#lpm for the team lose
wpm = function(p_h,p_a,winper){
  winno = ncol(winper)
  series_prob = c(p_h,p_h,p_a,p_a,p_h,p_a,p_h)
  series_prob = as.vector(series_prob)
  reser_prob = 1-series_prob
  for (i in(1:winno)) {
    winper[,i] = replace(winper[,i],winper[,i]==1,series_prob[i])
    winper[,i] = replace(winper[,i],winper[,i]==0,1-series_prob[i])
  }
  return(winper)
}

lpm = function(p_h,p_a,winper){
  winno = ncol(winper)
  series_prob = c(p_h,p_h,p_a,p_a,p_h,p_a,p_h)
  series_prob = as.vector(series_prob)
  reser_prob = 1-series_prob
  for (i in(1:winno)) {
    winper[,i] = replace(winper[,i],winper[,i]==0,series_prob[i])
    winper[,i] = replace(winper[,i],winper[,i]==1,1-series_prob[i])
  }
  return(winper)
}

#calculate probabilities of win/lose
#with exactly 4,5,6,7 games
win_E1_E8_4 = wpm(0.978,0.942,win_4)
prob_E1_E8_4 = sum(apply(win_E1_E8_4,1,prod))
win_E1_E8_5 = wpm(0.978,0.942,win_5)
prob_E1_E8_5 = sum(apply(win_E1_E8_5,1,prod))
win_E1_E8_6 = wpm(0.978,0.942,win_6)
prob_E1_E8_6 = sum(apply(win_E1_E8_6,1,prod))
win_E1_E8_7 = wpm(0.978,0.942,win_7)
prob_E1_E8_7 = sum(apply(win_E1_E8_7,1,prod))


win_E8_E1_4 = lpm(0.978,0.942,win_4)
prob_E8_E1_4 = sum(apply(win_E8_E1_4,1,prod))
win_E8_E1_5 = lpm(0.978,0.942,win_5)
prob_E8_E1_5 = sum(apply(win_E8_E1_5,1,prod))
win_E8_E1_6 = lpm(0.978,0.942,win_6)
prob_E8_E1_6 = sum(apply(win_E8_E1_6,1,prod))
win_E8_E1_7 = lpm(0.978,0.942,win_7)
prob_E8_E1_7 = sum(apply(win_E8_E1_7,1,prod))

#probabilities of exactly 4,5,6,7 games
#no matter which team win/lose
E1_E8_4 = sum(prob_E8_E1_4,prob_E1_E8_4)
E1_E8_5 = sum(prob_E8_E1_5,prob_E1_E8_5)
E1_E8_6 = sum(prob_E8_E1_6,prob_E1_E8_6)
E1_E8_7 = sum(prob_E8_E1_7,prob_E1_E8_7)
c(E1_E8_4,E1_E8_5,E1_E8_6,E1_E8_7)
#0.848751096 0.139566872 0.010671220 0.001010811