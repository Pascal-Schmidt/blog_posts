x <- seq(-5, 5, length=1000)
student <- dt(x, df=4)
lines(x,student,type="l",ylab="Density",col="blue",lty=1,lwd=3)

x<-seq(150,250)
y<-dnorm(x,mean=200, sd=15)
plot(x,y, type = "l", lwd = 2, xlim = c(150, 250), main = 'Two-Tailed Critical Region for a Significance level of 0.05
     T, df=24', ylab = "Density", xlab = "Energy Cost")
abline(v = 195, col = "red", lwd=2, lty=3)
mtext("195",side=3,line=-16,at=par("usr")[1]+0.62*diff(par("usr")[1:2]),cex=0.8, col = "red")

## LEFT SIDED CRITICAL REGION ##
x1<-seq(147,qt(0.025,mean = 201.5,df = 19),length=50)
y1<-qt(x1,mean=201.5,df = 19)
polygon(c(147,x1,dt(0.025,200,15)),c(0,y1,0),col="red")

## RIGHT SIDED CRITICAL REGION ##
x2<-seq(qnorm(0.975,200,15),250,length=50)
y2<-dnorm(x2,mean=200,sd=15)
polygon(c(qnorm(0.975,200,15),x2,250),c(0,y2,0),col="red")

## ADDING THE CRITICAL VALUE LINE ; CRITICAL VALUE = 210.2794 ##
abline(v=qnorm(0.975,200,15),col = "BLUE",lwd=2,lty=3)

## ADDING TEXTS TO THE PLOT ##
text(qnorm(0.975,200,15),0.010,"Critical Value = 210.2794 ",col="blue",cex=0.8)
text(120,0.015,"alpha = 0.05")
text(235,0.003,"0.025",cex=1)
text(115,0.003,"0.025",cex=1)


curve(dt(x, df=5), xlim=c(-6,6),
      main=paste("Student t-Distribution Probability Density Function, df = ", 5, ")", sep=""),
      type="n", las=1, ylab="probability", xlab="t")

Shade(FUN="dt(x, df=5)", xlim=c(-6, qt(0.025, df=5)), col="red")
Shade(FUN="dt(x, df=5)", xlim=c(qt(0.025, df=5), qt(0.975, df=5)), col="green")
Shade(FUN="dt(x, df=5)", xlim=c(qt(0.975, df=5), 6), col="red")