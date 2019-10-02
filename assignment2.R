dogbites_fullmoon = read.csv("dogbites.fullmoon.csv", header = TRUE)
full_moon = dogbites_fullmoon[dogbites_fullmoon$is.full.moon==1,]
head(full_moon)
nrow(full_moon)
sum(full_moon$daily.dogbites)
t_val = qt(0.975,12)
sdev = sqrt(var(full_moon$daily.dogbites))

var_a = var(full_moon$daily.dogbites)
uhead = sum(full_moon$daily.dogbites)/13

#1b
no_moon = dogbites_fullmoon[dogbites_fullmoon$is.full.moon==0,]

uhead_no_moon = sum(no_moon$daily.dogbites)/365
var_b = var(no_moon$daily.dogbites)
z_val = qnorm(0.975)
limits = 4.2308-4.5151-(1.96*(sqrt((6.5256/13)+(12.723/365))))
limits2 = 4.2308-4.5151+(1.96*(sqrt((6.5256/13)+(12.723/365))))

z_value_combined = (uhead - uhead_no_moon)/sqrt((var_a/13)+(var_b/365))



a = (80/124)+(1.96*sqrt(((80/124)*(44/124))/124))

binom.test(80,124,0.5)


#no.4
#1.
fuel = read.csv("fuel2017-20.csv", header = TRUE)
fit = lm(Comb.FE~., fuel)
summary(fit)
fit.sw.bic = step(fit, k = log(length(fuel$Comb.FE)))

fuel.test = read.csv("fuel2017-20.test.csv")
first_row = fuel.test[1,] #to take the first row of the list
prediction = predict(fit.sw.bic, first_row, interval="confidence")

