################################################
#Code for Multi model inference meta-analysis
#Some of the code is modified from: http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti
################################################
## packages
require(glmulti)
require(metafor)

##Handling data
#removing rows with missing data before handling data
dados=dados[!apply(dat[,c("openness", "initial_homogeneous", "system", ...)], 1, anyNA),]


##Function for Multimodel inference
rma.glmulti <- function(formula, data, ...)
  rma(formula, vi, data=data, method="ML", ...)

res <- glmulti(yi ~ Openness+ Initial.homogeneous+system +Study.duration+Delta.temperature+Natural.temp..range+Precipitation,random = ~ 1 | study, na.rm = T,data=effect.size.turn,
               level=1, fitfunction=rma.glmulti, crit="bic", methods="h",confsetsize=500,na.rm=T)
round(coef(res), 4)

print(res)

#best  models
tmp <- weightable(res)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp

#best model
summary(res@objects[[1]])

#ploting relative variable importance
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

## Extracting predicted values
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

#Extracting predicted value from multimodel inference 
weights <- weightable(res)$weights
yhat <- sum(weights * sapply(preds, function(x) x$pred))
round(yhat, 3)
se <- sqrt(sum(weights * sapply(preds, function(x) x$se^2 + (x$pred - yhat)^2)))
round(yhat + c(-1,1)*qnorm(.975)*se, 3)




##graph
x=c(1,2,3,4)

avg=c(0.3977, -0.3052,-0.2343,-0.7366)
ic=c(0.3340,1.0558,0.5839,0.0002)

plot(1, type="n", xlab="Response", xaxt="n", ylab="Average effect size (95%IC)", xlim=c(1, 4), ylim=c(-1.5, 1.5))
points(x,avg, pch=16)
arrows(x, avg-ic, x, avg+ic, length=0.05, angle=90, code=3)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("lnRR Nest"," lnCVR Nest"," lnRR Turn"," lnCVR Turn"))



##Meta-regression
metareg = rma(yi = yi, vi = vi, mod = ~  Openness, method = "REML", 
              data = effect.size.turn)
summary(metareg)

metareg = rma(yi = yi, vi = vi, mod = ~  openness+ initial_homogeneous+system +study_duration+temp_change+Temperatura+range+precipitacao, method = "REML", 
              data = effect.size.alfa2)
summary(metareg)
forest.rma(metareg)
plot(metareg)


##Meta-regression Nestdness
metareg.grad=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous +study_duration+Temperatura+range+precipitacao, method = "REML", 
                 data = effect.size.grad)
summary(metareg.grad)
forest.rma(metareg.grad)
plot(metareg.grad)
forest.rma(metareg.grad,atransf=exp,addcred=TRUE)
###variability
metareg.grad2=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change+Temperatura+range+precipitacao, method = "REML", 
                  data = effect.size.grad2)
summary(metareg.grad2)
forest.rma(metareg.grad2)
plot(metareg.grad2)
forest.rma(metareg.grad2,atransf=exp,addcred=TRUE)

##Meta-regression turnover
metareg.bal=rma(yi = yi, vi = vi, mod = ~  openness+ initial_homogeneous+system +study_duration+temp_change+Temperatura+range+precipitacao, method = "REML", 
                data = effect.size.bal)
summary(metareg.bal)

forest.rma(metareg.bal)
plot(metareg.bal)
forest.rma(metareg.bal,atransf=exp,addcred=TRUE)
#variability
metareg.bal2=rma(yi = yi, vi = vi, mod = ~  openness+ initial_homogeneous+system +study_duration+temp_change+Temperatura+range+precipitacao, method = "REML", 
                 data = effect.size.bal2)
summary(metareg.bal2)

forest.rma(metareg.bal2)
plot(metareg.bal2)
forest.rma(metareg.bal2,atransf=exp,addcred=TRUE)
par(mfrow=c(1,2))



##effect size nestd
effect.size.nest=escalc(measure = "ROM", n1i = samplecontrol, n2i = samplewarming, m1i = mean_nest_control, 
                        m2i = mean_nest_warming, sd1i = sd_nest_control, sd2i = sd_nest_warming, data=dados)

effect.size.nest2=escalc(measure = "CVR", n1i = samplecontrol, n2i = samplewarming, m1i = mean_nest_control, 
                         m2i = mean_nest_warming, sd1i = sd_nest_control, sd2i = sd_nest_warming, data=dados)

#model nestd
metareg.nest=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change, method = "REML", 
                 data = effect.size.nest)
summary(metareg.nest)
forest.rma(metareg.nest,atransf=exp,addcred=TRUE)

metareg.nest2=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change, method = "REML", 
                  data = effect.size.nest2)
summary(metareg.nest2)

forest.rma(metareg.nest2,atransf=exp,addcred=TRUE)


##efectsize turn
effect.size.turn=escalc(measure = "ROM", n1i = samplecontrol, n2i = samplewarming, m1i = dados1$mean_nest_control, 
                        m2i = mean_turn_warming, sd1i =  sd_turn_control, sd2i =sd_turn_warming, data=dados)

effect.size.turn2=escalc(measure = "CVR",n1i = samplecontrol, n2i = samplewarming, m1i = mean_turn_control, 
                         m2i = mean_turn_warming, sd1i =  sd_turn_control, sd2i =sd_turn_warming, data=dados)

#model turn
metareg.turn=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change, method = "REML", 
                 data = effect.size.turn)
summary(metareg.turn)
forest.rma(metareg.turn,atransf=exp,addcred=TRUE)

metareg.turn2=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change, method = "REML", 
                  data = effect.size.turn2)
summary(metareg.turn2)


