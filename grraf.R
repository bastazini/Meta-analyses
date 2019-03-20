##Plots for effect sizes(random models
                       
##Alpha diversity
 x=c(1.2,1.8)
avg=c(0.0118, -0.0522)
lower=c(-0.0311,-0.0929)
upper=c(0.0546,-0.0115)
plot(1, type="n", xlab="", xaxt="n", ylab="Effect size (95%IC)", xlim=c(1, 2), ylim=c(-.2, .2))
points(x,avg, pch=16)
arrows(x, lower, x, upper, length=0.05, angle=90, code=3)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Richness", "ENS.pie"), las=1)
par(mfrow=c(1,2))

##Beta diversity
x=c(1,2,3,4)

avg=c()
lower=c()
upper=c()
plot(1, type="n", xlab="", xaxt="n", ylab="Effect size (95%IC)", xlim=c(0.5, 4.5), ylim=c(-.2, .2))
points(x,avg, pch=16)
arrows(x, lower, x, upper, length=0.05, angle=90, code=3)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Nestedness"," Turnover"," Gradient abundance","Balanced abundance"), las=1)


