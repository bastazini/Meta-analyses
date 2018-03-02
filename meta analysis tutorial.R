#installing and pulling packages
install.packages("meta", "metafor")
require(meta)
require(metafor)

####For dichotomous data
###Creating some data
study = c("Doe_a","Doe_b","Doe_c","Doe_d", "Doe_e")
year= c(2017,2016, 2015,2014, 2013)
##Intervevntion group
#number of events
ei=c(20,10,15,20,18)
#Total smaple size 
si=c(50,45,60,48,50)
##Control group
#number of events
ec=c(10,14,12,15,28)
#Total smaple size 
sc=c(50,50,55,46,50)

#creat a data frame
data.ma =data.frame(study, year,ei,si,ec,sc);data


##simple meta-analysis of binary outcome data
meta.anala=metabin(ei,si,ec,sc,study, data=data.ma);meta.anala
#forest plot
forest (meta.anala, comb.fixed=T)


##effect size  and meta-regression 
effect.size.nest=escalc(measure = "ROM", n1i = samplewarming, n2i = samplecontrol, m1i = mean_nest_warming, 
                        m2i = mean_nest_control, sd1i = sd_nest_warming, sd2i = sd_nest_control, data=dados)

effect.size.nest2=escalc(measure = "CVR", n1i = samplewarming, n2i = samplecontrol, m1i = mean_nest_warming, 
                        m2i = mean_nest_control, sd1i = sd_nest_warming, sd2i = sd_nest_control, data=dados)

#model nestd
metareg.nest=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change, method = "REML", 
                data = effect.size.nest)
summary(metareg.nest)
forest.rma(metareg.nest,atransf=exp,addcred=TRUE)

metareg.nest2=rma(yi = yi, vi = vi, mod = ~   openness+ initial_homogeneous+system +study_duration+temp_change, method = "REML", 
                 data = effect.size.nest2)
summary(metareg.nest2)

forest.rma(metareg.nest2,atransf=exp,addcred=TRUE)
