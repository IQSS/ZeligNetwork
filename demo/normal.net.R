library(ZeligNetwork)

data(friendship)

# fit model
z.out <- zelig(
               perpower ~ friends + advice + prestige, 
               LF="identity", 
               model="normal.net", 
               data=friendship
               )
summary(z.out)

user.prompt()

# set explanatory variables
x <- setx(z.out)

user.prompt()

# simulate quantities of interest
s.out <- sim(z.out, x = x)
summary(s.out)

user.prompt()

# plot quantities of interest
plot(s.out)
