# We use the J1.bugs model to run the mcmc iterations

C <- 5 # Number of chains
N <- 10000 # Number of iterations
B = 500 # Burn-ins
I = 1 # Thinning parameter

# INITIAL STARTING VALUES

inits<-function()
list(a1 = 3, b1 = 7, a2 = 7, b2 = 3, p=0.5)

# Jags model
p.jags<-jags.model("J1.bug",data=X,inits=inits, n.chain = C)
vars <- c("a1", "a2", "b1", "b2", "p")
p1.sims <- coda.samples(p.jags, vars, n.iter = N, thin=I, n.burnin=B)
traceplot(p1.sims)
