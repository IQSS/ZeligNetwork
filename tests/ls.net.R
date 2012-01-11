library(ZeligNetwork)

data(sna.ex)

z.out <- zelig(Var1 ~ Var2 + Var3 + Var4, model = "ls.net", data=sna.ex)

summary(z.out)

x.high <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.8))
x.low <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.2))

s.out <- sim(z.out, x=x.high, x1=x.low)

summary(s.out)

#plot(s.out)
