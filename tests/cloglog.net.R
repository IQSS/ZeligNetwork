library(ZeligNetwork)

data(friendship)

z.out <- zelig(
               friends ~ advice + prestige + perpower, 
               model = "cloglog.net", 
               data = friendship
               )

summary(z.out)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

s.out <- sim(z.out, x = x.high)

summary(s.out)
class(s.out)

plot(s.out)
