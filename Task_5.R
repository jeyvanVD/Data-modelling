#question 5
#5(a)
#lambda in maximum likelihood of Poisson distribution is  equal to the mean of the given dataframe.
dog_bites <- read.csv("dogbites.total.csv", header = TRUE)
rows <- c(1:nrow(dog_bites))
total = 0
for (i in rows){
  total = total + dog_bites[i,1]
}
mean_lambda = total/ nrow(dog_bites)

#5(b)
#5(b1)
freq_less_than_1 = ppois(1,mean_lambda)

#5(b2)
#the number of admissions is most likely to occur on a given day would be rather similar to the mean, as the mean is how much dog bites in total over the days
#however, as the data is a discrete data, we could not round the given number up. So we have to round down the given number.
admissions_per_day <- round(floor(mean_lambda))

#5(b3)
#the number of admissions over the interval of 28 days could be acquired by multiplying the mean (average number of person admitted per day) with the number
#of days, which in this case is 28.
admissions_28 = mean_lambda * 28

#5(b4)
#to find six or more dog-bite admissions for at least 8 of the days in a 28 day period, we must first look at how much admissions should be expected upon the 
#8 days as follows:
expected_8_days = mean_lambda * 8 
#to find six or more dog bites, we could use;
six_bites = 1 - ppois(5,mean_lambda)
#and to find the six or more dog bites within the 8 days, we could do ;
six_bites_eight_days = 1 - dbinom(0,28,six_bites) - dbinom(1,28,six_bites) - dbinom(2,28,six_bites) - dbinom(3,28,six_bites) - dbinom(4,28,six_bites) - 
  dbinom(5,28,six_bites) - dbinom(6,28,six_bites) - dbinom(7,28,six_bites)

#5(c)
i = seq(0,24, length = 25 )
j = dpois(i, mean_lambda)
hist(dog_bites$daily.dogbites, ylim = c(0,0.2), breaks = 24, main = "Dog bites per day", xlab = "number of dogbites", prob = TRUE)
lines(i,j, lwd =2.5, col = "red")