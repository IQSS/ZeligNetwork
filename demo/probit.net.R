library(ZeligNetwork)

data(friendship)

# fit model
z.out <- zelig(friends ~ advice + prestige + perpower, model="probit.net", data=friendship)
summary(z.out)

user.prompt()

# set explanatory variables
x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

user.prompt()

# simulate quantities of interest
s.out <- sim(z.out, x = x.high, x1 = x.low)
summary(s.out)

user.prompt()

# plot quantities of interest
plot(s.out)
