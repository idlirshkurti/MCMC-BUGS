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

####################### PLOTTING ############################

densityplot(p1.sims)
# Density plots for each chain and each parameter
#Converting posterior samples for
#each parameter into a data frame
a1 = as.matrix(p1.sims[,1], nrow = (N*C), byrow = TRUE)
a1sds <- data.frame(a1)
a1sds$iteration <- 1:(N*C)
names(a1sds) <- c("a1", "iteration")
a2 = as.matrix(p1.sims[,2], nrow = (N*C), byrow = TRUE)
a2sds <- data.frame(a2)
a2sds$iteration <- 1:(N*C)
names(a2sds) <- c("a2", "iteration")
b1 = as.matrix(p1.sims[,3], nrow = (N*C), byrow = TRUE)
b1sds <- data.frame(b1)
b1sds$iteration <- 1:(N*C)
names(b1sds) <- c("b1", "iteration")
b2 = as.matrix(p1.sims[,4], nrow = (N*C), byrow = TRUE)
b2sds <- data.frame(b2)
b2sds$iteration <- 1:(N*C)
names(b2sds) <- c("b2", "iteration")
p = as.matrix(p1.sims[,5], nrow = (N*C), byrow = TRUE)
psds <- data.frame(p)
psds$iteration <- 1:(N*C)
names(psds) <- c("p", "iteration")

######## TRACE PLOTS AND HISTOGRAMS #######################
theme_set(theme_bw())
theme_update(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
axis.title.x = element_text(size = 20),
axis.title.y = element_text(size = 20,
angle = 90), legend.text = element_text(size = 20),
legend.title = element_text(size = 20))

# random samples from normal distributions with 
# mean = sample mean of parameter samples and 
# varaince = sample variance of parameter samples 
NORa1<-rnorm(50000,mean(a1[,1]),sd(a1[,1])) 
NORa2<-rnorm(50000,mean(a2[,1]),sd(a2[,1])) 
NORb1<-rnorm(50000,mean(b1[,1]),sd(b1[,1])) 
NORb2<-rnorm(50000,mean(b2[,1]),sd(b2[,1])) 
NORp<-rnorm(50000,mean(p[,1]),sd(p[,1]))

# Separate histograms for each parameter compared to normal distributions

data1 <- data.frame(xx = a1[,1],yy = NORa1)
data1$iteration <- 1:(N*C)
ga1 <- ggplot(data1,aes(x=xx,y=..density..)) + 
geom_histogram(binwidth = 0.03,alpha = 0.3,fill="blue") +
geom_density(aes(color="density"), size=1.4, linetype="dashed") +
scale_colour_manual("Lines", values=c("blue")) +
geom_histogram(data=subset(data1,yy<10),fill = "red",
alpha = 0.3,binwidth = 0.03)+ xlab(expression(alpha[1]))

data2 <- data.frame(xx = a2[,1],yy = NORa2)
data2$iteration <- 1:(N*C)
ga2 <- ggplot(data2,aes(x=xx,y=..density..)) +
geom_histogram(binwidth = 0.03,alpha = 0.3,fill="blue")+
geom_density(aes(color="density"), size=1.4, linetype="dashed")+
scale_colour_manual("Lines", values=c("blue")) +
geom_histogram(data=subset(data2,yy<10),fill = "red",
alpha = 0.3,binwidth = 0.03)+ xlab(expression(alpha[2]))

datb1 <- data.frame(xx= b1[,1],yy = NORb1)
datb1$iteration <- 1:(N*C)
gb1 <- ggplot(datb1,aes(x=xx,y=..density..)) +
geom_histogram(binwidth = 0.03,alpha = 0.3,fill="blue")+
geom_density(aes(color="density"), size=1.4, linetype="dashed")+
scale_colour_manual("Lines", values=c("blue")) +
geom_histogram(data=subset(datb1,yy<10),fill = "red",
alpha = 0.3,binwidth = 0.03)+ xlab(expression(beta[1]))

datb2 <- data.frame(xx = b2[,1],yy = NORb2)
datb2$iteration <- 1:(N*C)
gb2 <- ggplot(datb2,aes(x=xx,y=..density..)) +
geom_histogram(binwidth = 0.03,alpha = 0.3,fill="blue")
+geom_density(aes(color="density"), size=1.4, linetype="dashed")+
scale_colour_manual("Lines", values=c("blue")) +
geom_histogram(data=subset(datb2,yy<10),fill = "red",
alpha = 0.3,binwidth = 0.03)+ xlab(expression(beta[2]))

datp <- data.frame(xx = p[,1],yy = NORp)
datp$iteration <- 1:(N*C)
gp <- ggplot(datp,aes(x=xx,y=..density..)) +
geom_histogram(binwidth = 0.03,alpha = 0.3,fill="blue")+
geom_density(aes(color="density"), size=1.4, linetype="dashed")+
scale_colour_manual("Lines", values=c("blue")) +
geom_histogram(data=subset(datp,yy<10),fill = "red",
alpha = 0.3,binwidth = 0.003)+ xlab(expression(p))
grid.arrange(ga1, ga2, gb1, gb2, gp)

# ----- Trace plot for all 5 parameters ------ #

datam <- cbind(a1,a2,b1,b2,p)
datasds <- data.frame(datam)
datasds$iteration <- 1:(N*C)
names(datasds) <- c("a1","a2","b1","b2","p", "iteration")
datadf <- melt(datasds, id = "iteration")
g <- ggplot(datadf, aes(x = iteration, y = value, colour = variable)) + geom_line()
g <- g + theme(legend.position = "bottom")
print(g)

###### AUTOCORRELATION ##########
# Separate autocorrelation plots
acf(a1,main=’Autocorrelation for a1’)
acf(a2,main=’Autocorrelation for a2’)
acf(b1,main=’Autocorrelation for b1’)
acf(b2,main=’Autocorrelation for b2’)
acf(p,main=’Autocorrelation for p’)

### ESS
effectiveSize(p1.sims)

######## Autocorrelation plots for all chains
par(mfrow=c(3,2))
acf(a1,main="a1")
acf(a2,main="a2")
acf(b1,main="b1")
acf(b2,main="b2")
acf(p,main="p")

##### Convergence diagnostics
gelman.diag(p1.sims)
gelman.plot(p1.sims)
geweke.diag(p1.sims)
geweke.plot(p1.sims)
rmeanplot(p1.sims)

############ Final histogram ###############
par(mfrow=c(2,1))
a=rbinom((N*C),1,mean(0.5))
alpha=a1*a+a2*(1-a)
beta=b1*a+b2*(1-a)
Z=rgamma((N*C),alpha,beta)
Zsds <- data.frame(Z)
Zsds$iterations<-1:N*C
d1 <- ggplot(data = Xsds ,aes(x = Y))
d1 <- d1 + geom_histogram(binwidth = 0.2,
aes(y = ..density.., fill=..density..), alpha=0.9)
d1 <- d1 + scale_fill_gradient("Density scale")
d1 <- d1 + geom_density(data= Zsds,
aes(color="Posterior Denisty", x=Z), size=1.1, linetype="dashed")
d1 <- d1 + xlim(0, 6)
d1 <- d1 + scale_colour_manual("Lines", values=c("black", "black")) +
scale_fill_gradient("Density scale", low = "aquamarine", high = "aquamarine4")
d1 <- d1 + xlab(expression("Observation values")) + ylab(expression("Density"))
grid.arrange(d1)
# ----------------------- THE END ----------------------------- #
