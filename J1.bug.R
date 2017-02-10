model{
#Likelihood:
for(i in 1:200){
a[i]~dbern(p)
V1[i]~dgamma(a1*a[i]+a2*(1-a[i]),b1*a[i]+b2*(1-a[i]))
}
# Priors:
a1~dunif(0,10)
b1~dunif(0,10)
a2~dunif(0,10)
b2~dunif(0,10)
p~dunif(0,1)}