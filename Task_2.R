#Question 2
#(a)
port_adelaide <- read.csv("port.adelaide.csv", header = TRUE)
winwin = 0
winlose = 0
losewin = 0
loselose = 0
rows <- c(0:nrow(port_adelaide))
for(i in rows){
  if(isTRUE(port_adelaide[i+1,1] == 1 && port_adelaide[i,1] == 1)){
    winwin = winwin + 1
  }
  else if (isTRUE(port_adelaide[i+1,1] == 0 & port_adelaide[i,1] == 1)){
    winlose = winlose + 1
  }
  else if (isTRUE(port_adelaide[i+1,1] == 1 && port_adelaide[i,1] == 0)){
    losewin = losewin + 1
  }
  else if (isTRUE(port_adelaide[i+1,1] == 0 && port_adelaide[i,1] == 0)){
    loselose = loselose + 1
  }
}
cat("The number of wins after wins =", winwin, "\nThe number of wins after loses =", losewin,"\nThe number of loses after wins =", winlose,"\nThe number of loses after loses =", loselose)

jointWinWin = winwin/67
jointWinLose = winlose/67
jointLoseWin = losewin/67
jointLoseLose = loselose/67


cat("The estimate of the joint probabilities of a win being followed by a win =", jointWinWin,"\n") #the number of winwin divided by the total number of events
cat("The estimate of the joint probabilities of a win being followed by a lose =", jointWinLose,"\n") #the number of winlose divided by the total number of events
cat("The estimate of the joint probabilities of a lose being followed by a win =", jointLoseWin,"\n") #the number of losewin divided by the total number of events
cat("The estimate of the joint probabilities of a lose being followed by a lose =", jointLoseLose,"\n") #the number of loselose divided by the total number of events

#(b)
marginalWin = jointWinWin + jointLoseWin
cat("The marginal probability of PA winning a game irrespective of whether they won or lost their previous game, i.e., P(Wt = 1) is", marginalWin)

#(c)
#P(Wt = 1|Wt-1 = 1) = (P(Wt=1 ^ Wt-1=1)/P(Wt-1=1)), this is used to find the probability that PA will win a game given that they won their previous game
ProWin = (jointWinWin/(jointWinWin + jointWinLose))
cat("The probability that PA will win a game given that they won their previous game is", ProWin)

#(d)
#P(Wt = 1|Wt-1 = 0) = (P(Wt=1 ^ Wt-1=0)/P(Wt-1=0)), this is used to find the probability that PA will win a game given that they lost their previous game
ProWiLo = (jointLoseWin/(jointLoseLose + jointLoseWin))
cat("The probability that PA will win a game given that they lost their previous game is", ProWiLo)

#(e)
#to check the independency of the two events, we have to prove that the conditional probability of the two events is not equal to its marginal probability
#first we count the conditional probability:
#P(Wt-1=1 ^ Wt=1)/P(Wt-1 = 1)
independencyLeft = jointWinWin/(jointWinWin + jointLoseWin)
independencyRight = (jointWinWin + jointWinLose)
cat("The two events are independent to each other :", independencyLeft == independencyRight)

#(f)



