library(ZeligNetwork)

data(friendship)

# Fit the model
z.out <- zelig(per ~ perpower, LF="inverse", model="gamma.net", data=friendship)

#Summarize fitted model

# Set explanatory variables
x.low <- setx(z.out)
x.high <- setx(z.out)

# Simulate quantities of interest
s.out <- sim(z.out, x = x.low, x1 = x.high)

# Summarize simulations
summary(s.out)
