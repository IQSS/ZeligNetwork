library(ZeligNetwork)

data(friendship)
data(sna.ex)

# CLOGLOG.NET
# CLOGLOG.NET
# CLOGLOG.NET

z.out <- zelig(friends ~ perpower, model = "cloglog.net", data = friendship)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))

s.out <- sim(z.out, x = x.high)

summary(s.out)

plot(s.out)

q()

# GAMMA.NET
# GAMMA.NET
# GAMMA.NET

z.out <- zelig(per ~ perpower, LF="inverse", model="gamma.net", data=friendship)

x.low <- setx(z.out)
x.high <- setx(z.out)

s.out <- sim(z.out, x = x.low, x1 = x.high)

summary(s.out)

plot(s.out)

# LOGIT.NET
# LOGIT.NET
# LOGIT.NET

z.out <- zelig(friends ~ perpower, model="logit.net", data=friendship)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

s.out <- sim(z.out, x = x.high)

summary(s.out)

plot(s.out)

# LS.NET
# LS.NET
# LS.NET

z.out <- zelig(Var1 ~ Var3, model = "ls.net", data=sna.ex)

summary(z.out)

x.high <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.8))
x.low <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.2))

s.out <- sim(z.out, x=x.high, x1=x.low)

summary(s.out)

plot(s.out)

# NORMAL.NET
# NORMAL.NET
# NORMAL.NET

z.out <- zelig(perpower ~ prestige, LF="identity", model="normal.net", data=friendship)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

s.out <- sim(z.out, x = x.high, x1 = x.low)

# POISSON.NET
# POISSON.NET
# POISSON.NET

z.out <- zelig(count ~ perpower, model="poisson.net", data=friendship)

summary(z.out)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

s.out <- sim(z.out, x = x.high, x1 = x.low)

summary(s.out)

plot(s.out)

# PROBIT.NET
# PROBIT.NET
# PROBIT.NET

z.out <- zelig(friends ~ perpower, model="probit.net", data=friendship)

summary(z.out)

x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

s.out <- sim(z.out, x = x.high, x1 = x.low)

summary(s.out)

plot(s.out)
