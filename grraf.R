## Plots effect size
##Alpha diversity
x=c(1.2,1.8)
avg=c(0.0118, -0.0450)
lower=c(-0.0311, -0.0842)
upper=c(0.0546,-0.0057)
plot(1, type="n", xlab="", xaxt="n", ylab="Effect size (95%IC)", xlim=c(1, 2), ylim=c(-.1, .1))
points(x,avg, pch=16,cex=1.5)
arrows(x, lower, x, upper, length=0.05, angle=90, code=3,lwd=2.5)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Richness", "ENS.pie"),cex.lab=14, las=1)
par(mfrow=c(1,2))

##Beta diversity
x=c(1,2,3,4)
avg=c(-0.0043,0.0563,0.0481,-0.0157)
lower=c(-0.0716, 0.0155 , -0.0229, -0.0466)
upper=c(0.0631,0.0970,  0.1191,  0.0153)
plot(1, type="n", xlab="", xaxt="n", ylab="Effect size (95%IC)", xlim=c(0.5, 4.5), ylim=c(-.1, .15))
points(x,avg, pch=16,cex=1.5)
arrows(x, lower, x, upper, length=0.05, angle=90, code=3,lwd=2.5)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Nestedness"," Turnover"," Gradient abundance","Balanced abundance"), cex.lab=14, las=1)

