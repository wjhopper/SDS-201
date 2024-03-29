
png(file="hex.png", width = 750, height = 1050, units = "px", res = 300)
    
par(mar=c(0,0,0,0))

# setting the plotting area
plot(NULL, xlim=c(-1,1), ylim=c(-1,1), axes=FALSE)

#drawing the border
x <- c(sqrt(.75), 0, -(sqrt(.75)), -(sqrt(.75)), 0, sqrt(.75)) 
y <- c(.5,        1, .5,           -.5,         -1, -.5) * 750/1050
polygon(x,y, lwd=4, border="#002855")

# fill color
polygon(x=x*.93, y=y*.93, col="#F2A900", border="white")

#plotting the scatterplot and regression line
set.seed(3)
x.var <- c(rnorm(20, 0, .2), -1, 1)
y.var <- x.var + rnorm(22, 0, .25) * 750/1050
points(x.var, y.var, col="white", pch=16, cex=.5)
segments(x0=-.656, y0=-.656, x1=.656, y1=.656, lwd=1.5, col="white")

#plotting the prediction intervals
fit<-lm(y.var~x.var)
pred.1 <- predict(fit,interval="prediction", newdata=data.frame(seq(-1, 1, length=32)))
lower.b<-pred.1[,2][order(x.var)]
upper.b<-pred.1[,3][order(x.var)]
lines(x.var[order(x.var)], lower.b, lty=2, lwd=1.5, col="white")
lines(x.var[order(x.var)], upper.b, lty=2, lwd=1.5, col="white")

#replotting the border
polygon(x,y, lwd=4, border="#002855", col=NULL)

#text
text(x=0, y=.31, "Smith College", col=	"#002855", cex=.85)
text(x=0, y=0, "SDS", col="#002855", cex=3.5)
text(x=0, y=-.4, "Statistical & \n Data Sciences", col="#002855", cex=.85)


dev.off()
