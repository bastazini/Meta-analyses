#installing and pulling packages
install.packages("meta")
require(meta)

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
forest (meta.anala, comb.fixed=FALSE)
