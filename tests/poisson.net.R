library(ZeligNetwork)

data(friendship)
z.out <- zelig(count ~ advice + prestige + perpower, model="poisson.net", data=friendship)

summary(z.out)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

s.out <- sim(z.out, x = x.high, x1 = x.low)
summary(s.out)
#plot(s.out)
