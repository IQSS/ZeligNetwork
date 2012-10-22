library(ZeligNetwork)

data(sna.ex)

# fit model
z.out <- zelig(Var1 ~ Var2 + Var3 + Var4, model = "ls.net", data=sna.ex)
summary(z.out)

user.prompt()

# set explanatory variables
x.high <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.8))
x.low <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.2))

user.prompt()

# simulate quantities of interest
s.out <- sim(z.out, x=x.high, x1=x.low)
summary(s.out)

user.prompt()

# plot quantities of interest
plot(s.out)
