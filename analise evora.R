
###calculating effect size (Standardize mean differences)

##Incidence based indices
#nestdness
effect.size.nest=escalc(measure = "SMD", n1i = samplecontrol, n2i = samplewarming, m1i = mean_nest_control, 
                        m2i = mean_nest_warming, sd1i = sd_nest_control, sd2i = sd_nest_warming, data=dados)

#turnover
effect.size.turn=escalc(measure = "SMD", n1i = samplecontrol, n2i = samplewarming, m1i = mean_turn_control, 
                        m2i = mean_turn_warming, sd1i =  sd_turn_control, sd2i =sd_turn_warming, data=dados)

##Abundance based indices
#gradient in abundance (nest)

effect.size.grad=escalc(measure = "SMD", n1i = samplecontrol, n2i = samplewarming, m1i = mean_grad_control , 
                        m2i = mean_grad_warming, sd1i = sd_grad_control, sd2i = sd_grad_warming, data=dados)
#balance abundance (turnover)
effect.size.bal=escalc(measure = "SMD", n1i = samplecontrol, n2i = samplewarming, m1i = mean_abund_control, 
                       m2i = mean_bal_warming, sd1i = sd_abund_control, sd2i = sd_bal_warming, data=dados)


#######Multimodelmeta-regression
##function
rma.glmulti <- function(formula, data, ...)
  rma(formula, vi, data=data, method="ML", ...)

#a general model function
res = glmulti(yi ~ Openness+ Initial.homogeneous+system +Study.duration+Delta.temperature+Natural.temp..range+Precipitation,random = ~ 1 | study, na.rm = T,data=effect.size.turn,
               level=1, fitfunction=rma.glmulti, crit="bic", methods="h",confsetsize=500,na.rm=T)

#results of the model
round(coef(res), 4)

print(res)

#best  models
tmp <- weightable(res)
tmp <- tmp[tmp$bic <= min(tmp$bic) + 2,]
tmp

#best model
summary(res@objects[[1]])

#importance
plot(res, type="s")


##multimodel inference
"methods/html/setOldClass.html">setOldClass("rma.uni")
"methods/html/setMethod.html">setMethod("getfit", "rma.uni", function(object, ...) {
  if (object$test=="z") {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=Inf)
  } else {
    cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=object$k-object$p)
  }
})

round(coef(res), 4)
teste=round(coef(res), 4)
teste2=teste[,4]
teste2=edit(teste2)
barplot(teste2, horiz=T,xlab="Relative importance",las=2, col="black",las=2)

##predicted values

x <- c("length"=15, "openness" = open,"initial_homogeneous"= y,"system"= "Marine")
preds <- list()

for (j in 1:res@nbmods) {
  
  model <- res@objects[[j]]
  vars <- names(coef(model))[-1]
  
  if (length(vars) == 0) {
    preds[[j]] <- predict(model)
  } else {
    preds[[j]] <- predict(model, newmods=x[vars])
  }
  
}

#multimodel predicted value
weights <- weightable(res)$weights
yhat <- sum(weights * sapply(preds, function(x) x$pred))
round(yhat, 3)
se <- sqrt(sum(weights * sapply(preds, function(x) x$se^2 + (x$pred - yhat)^2)))
round(yhat + c(-1,1)*qnorm(.975)*se, 3)



####Models
#nestdness
res.nest = glmulti(yi ~ Openness+ Initial.homogeneous+system +Study.duration+Delta.temperature+Natural.temp..range+Precipitation,random = ~ 1 | study, na.rm = T,data=effect.size.nest,
              level=1, fitfunction=rma.glmulti, crit="bic", methods="h",confsetsize=500,na.rm=T)
round(coef(res.nest), 4)
teste=round(coef(res.nest), 4)
#importance
plot(res.nest, type="s")


teste2=teste[,4]
teste2=edit(teste2)
barplot(teste2, horiz=T,xlab="Relative importance",las=2, col="black",las=2)


#turnover
res.turn = glmulti(yi ~ Openness+system +Study.duration+system+Delta.temperature+Natural.temp..range,random = ~ 1 | study, na.rm = T,data=effect.size.turn,
                   level=1, fitfunction=rma.glmulti, crit="bic", methods="h",confsetsize=500,na.rm=T)
round(coef(res.turn), 4)
#importance
plot(res.turn1, type="s")

teste=round(coef(res.turn), 4)
teste2=teste[,4]
teste2=edit(teste2)
barplot(teste2, horiz=T,xlab="Relative importance",las=2, col="black",las=2)




#gradient in abundance (nest)
res.grad = glmulti(yi ~ Openness+ Initial.homogeneous+system +Study.duration+Local.Temperature+Natural.temp..range+Precipitation,random = ~ 1 | study, na.rm = T,data=effect.size.grad,
                   level=1, fitfunction=rma.glmulti, crit="bic", methods="h",confsetsize=500,na.rm=T)
round(coef(res.grad), 4)
#importance
plot(res.grad, type="s")

teste=round(coef(res.grad), 4)
teste2=teste[,4]
teste2=edit(teste2)
barplot(teste2, horiz=T,xlab="Relative moderator importance",las=2, col="black",las=2)



#balance abundance (turnover)

res.bal = glmulti(yi ~ Openness+ Initial.homogeneous+system +Study.duration+Delta.temperature+Natural.temp..range+Precipitation,random = ~ 1 | study, na.rm = T,data=effect.size.bal,
                   level=1, fitfunction=rma.glmulti, crit="bic", methods="h",confsetsize=500,na.rm=T)
round(coef(res.bal), 4)
#importance
plot(res.bal, type="s")

teste=round(coef(res.bal), 4)
teste2=teste[,4]
teste2=edit(teste2)
barplot(teste2, horiz=T,xlab="Relative importance",las=2, col="black",las=2)


##Graf average Intercept (95%IC)
x=c(1,1.5,2,2.5)

avg=c(-0.2802,0.6098,-0.1244,-0.0283)

ic=c(0.9551,0.7936, 1.2876,0.8220)

par(mar=c(3,4,2,2))
plot(1, type="n", xlab="", xaxt="n", ylab="Intercep (95%IC)", xlim=c(0.9, 2.6), ylim=c(-2.5, 2.5))
points(x,avg, pch=16, col="darkred", cex=1.9)
arrows(x, avg-ic, x, avg+ic, length=0.05, angle=90, code=3, lwd=2.5)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Nestedness"," Turnover"," Gradient abundance","Balanced abundance"), las=1)


#Imporntace of predictors using bic
x=c("Study length", "Nat Temp range", "Precipitation", "Openness", "Delta Temp","Initial homogeneous", "System",
    "Study length", "Nat Temp range",  "Openness","Precipitation", "Initial homogeneous","Delta Temp", "System",
    "Study length", "Precipitation","Nat Temp range",  "Openness","Delta Temp", "Initial homogeneous", "System",
    "Study length", "Nat Temp range","Precipitation",  "Openness","Initial homogeneous","Delta Temp", "System" 
)
y=c(1,0.58,0.52,0.5,0.24,0.18,0.07,
    1,0.63,0.5,0.48,0.17,0.16,0.07,
    0.97,0.59,0.51,0.5, 0.28,0.21,0.1,
    0.99,0.55,0.53,0.49,0.25,0.16,0.1)

teste=cbind(x,y)
teste2=teste[order(teste[,1])]
par(mar=c(5,7,2,2))

boxplot(y ~x,
        ylab = " ", xlab = "Average  relative importance", main = "",
        las = 1,
        col = "black",
        border = "white",
        whiskcol = "black",
        staplecol = "black",
        outcol = "black",
        medcol = "black",   las=2, cex.axis=1, horizontal=T)


x=c(1,1.5,2,2.5)

avg=c(0.0034,0.1877 ,0.0227,-0.0859)


ic=c(0.0681,0.07, 0.0692,0.0680)
ic=1.96*ic

par(mar=c(2,4,2,1))
plot(1, type="n", xlab="", xaxt="n", ylab="Overall mean (95%IC)", xlim=c(0.9, 2.6), ylim=c(-0.5, 0.5))
points(x,avg, pch=16, col="darkred", cex=1.9)
arrows(x, avg-ic, x, avg+ic, length=0.05, angle=90, code=3, lwd=2.5)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Nestedness"," Turnover"," Gradient abundance","Balanced abundance"), las=1)



