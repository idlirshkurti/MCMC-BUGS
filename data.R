X <- read.table(’http://www.stats.ox.ac.uk/~nicholls/MScMCMC15/GammaMixtureData.txt’)

# Histogram of data
Xsds <- data.frame(X)
Xsds$iteration <- 1:(nrow(X))
names(Xsds) <- c("Y", "iteration")

d1 <- ggplot(data = Xsds ,aes(x = Y))
d1 <- d1 + geom_histogram(binwidth = 0.3, aes(y = ..density.., fill=..density..), alpha=0.9)
d1 <- d1 + xlim(0, 6)
d1 <- d1 + geom_density(aes(color="Density"), size=1.1, linetype="dashed")
d1 <- d1 + scale_fill_gradient("Density scale", low = "aquamarine", high = "aquamarine4")
d1 <- d1 + xlab(expression(observations))

pdf("Histogram1.pdf",width=9,height=9,pointsize=10)
grid.arrange(d1)
dev.off()
