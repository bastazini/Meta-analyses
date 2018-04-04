
par(mfrow=c(1,2))
x=c(1,2,3,4)

avg=c(-0.3237,0.6197,0.1255,-0.1700)

ic=c(1.0673, 0.9404,1.3041, 0.9905)

par(mar=c(2,5,2,3))
plot(1, type="n", xlab="", xaxt="n", ylab="Intercep (95%IC)", xlim=c(1, 4), ylim=c(-2.5, 2.5))
points(x,avg, pch=16)
arrows(x, avg-ic, x, avg+ic, length=0.05, angle=90, code=3)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Nestedness"," Turnover"," Gradient abundance","Balanced abundance"), las=1)


x=c("Open", "Terrestrial", "Marine","Initial homogenous", "Delta Temp", "Precipitation", "Nat Temp range", "Study duration",
    "Open", "Terrestrial", "Marine","Delta Temp","Initial homogenous",  "Precipitation", "Nat Temp range", "Study duration",
    "Open", "Terrestrial", "Marine","Delta Temp","Initial homogenous",  "Precipitation", "Nat Temp range", "Study duration",
    "Open", "Terrestrial", "Marine","Delta Temp","Nat Temp range","Precipitation","Initial homogenous", "Study duration"
    
    )
y=c(0.0006,0.0851,0.1703,0.2749, 0.3251, 0.5178, 0.6326,0.99,
  0,0.682,0.1365,0.2294,0.2858,0.5047,0.6520,0.9997,
  0.043,0.0513,0.1027,0.2359,0.2644,0.5356,0.5791,0.9814,
  0.042,0.1272,0.2549,0.2880,0.5539,0.5581,0.5998,0.9925)
teste=cbind(x,y)
teste2=teste[order(teste[,1])]
par(mar=c(5,8,2,2))

boxplot(y ~x,
        ylab = " ", xlab = "Average  moderator importance", main = "",
        las = 1,
        col = "black",
        border = "white",
        whiskcol = "black",
        staplecol = "black",
        outcol = "black",
        medcol = "black",   las=2, cex.axis=0.97, horizontal=T)



#for bic
avg=c(-0.2153,0.5913,0.1406, -0.1669 )

ic=c(0.9071,0.8457,1.2287,0.9017)

plot(1, type="n", xlab="", xaxt="n", ylab="Intercep (95%IC)", xlim=c(1, 4), ylim=c(-2.5, 2.5))
points(x,avg, pch=16)
arrows(x, avg-ic, x, avg+ic, length=0.05, angle=90, code=3)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Nestedness"," Turnover"," Gradient abundance","balanced abundance"), las=2)


x=c("Open", "Terrestrial", "Marine","Initial homogenous", "Delta Temp", "Precipitation", "Nat Temp range", "Study duration",
    "Open", "Terrestrial", "Marine","Delta Temp","Initial homogenous",  "Precipitation", "Nat Temp range", "Study duration",
    "Open", "Terrestrial", "Marine","Delta Temp","Initial homogenous",  "Precipitation", "Nat Temp range", "Study duration",
    "Open", "Terrestrial", "Marine","Delta Temp","Initial homogenous",  "Precipitation", "Nat Temp range", "Study duration"
    
)
y=c(0.0002,0.0388, 0.0776,0.1727,0.2208,0.4985,0.5924,0.9994,
    0,0.0292,0.0585,0.1428,0.1814,0.4590,0.6410,0.9997,
    0.0015,0.0232,0.0464,0.1546,0.1703,0.5115,0.5633,0.9797,
    0.0013,0.753,0.1509,0.2259,0.5280,0.5368,0.5372,0.9919
    
    
    
  )

boxplot(y ~x,
        ylab = "Moderator ", xlab = "Average importance", main = "",
        las = 1,
        col = "black",
        border = "white",
        whiskcol = "black",
        staplecol = "black",
        outcol = "black",
        medcol = "black",   las=2, cex.axis=0.9,horizontal=T)


#Imporntace of predictors using bic
x=c("Openness", "System","Initial homogenous", "Delta Temp", "Precipitation", "Nat Temp range", "Study duration",
    
    
)
y=c(0.0006,0.0851,0.1703,0.2749, 0.3251, 0.5178, 0.6326,0.99,
    0,0.682,0.1365,0.2294,0.2858,0.5047,0.6520,0.9997,
    0.043,0.0513,0.1027,0.2359,0.2644,0.5356,0.5791,0.9814,
    0.042,0.1272,0.2549,0.2880,0.5539,0.5581,0.5998,0.9925)
teste=cbind(x,y)
teste2=teste[order(teste[,1])]
par(mar=c(5,8,2,2))

boxplot(y ~x,
        ylab = " ", xlab = "Average  moderator importance", main = "",
        las = 1,
        col = "black",
        border = "white",
        whiskcol = "black",
        staplecol = "black",
        outcol = "black",
        medcol = "black",   las=2, cex.axis=0.97, horizontal=T)

