## ------------------------------------------------------------------------

methods(plot)

## ------------------------------------------------------------------------
plot(x=1,y=1)

## ------------------------------------------------------------------------
pdf(file="samplescatterplot.pdf")
plot(x=runif(100),y=runif(100))
dev.off()

getwd()


## ------------------------------------------------------------------------
p1<-par()
dev.off() #close the default device

class(p1)
length(p1)

p1[40]

## ------------------------------------------------------------------------
layout(matrix(1:4,nrow=2))
layout.show(4)

## ------------------------------------------------------------------------
m1<-c(1,2,3,3,3,3)
m1<-
matrix(m1,nrow=2,
  byrow=FALSE)
m1

## ------------------------------------------------------------------------
layout(m1)
layout.show(3)

## ---- echo=FALSE---------------------------------------------------------
m1<-matrix(c(1,1,1,2,3,3,3,4,3,3,3,4,3,3,3,4),
           nrow=4,byrow=TRUE)
layout(m1)
layout.show(4)

## ------------------------------------------------------------------------
m1<-matrix(c(1,1,1,2,3,3,3,4,3,3,3,4,3,3,3,4),
           nrow=4,byrow=TRUE)
layout(m1)
layout.show(4)

## ------------------------------------------------------------------------
m1<-matrix(1:4,2,2)
x1<-rnorm(100)
layout(m1)

plot(x1)
hist(x1)
boxplot(x1)
qqnorm(x1)

## ------------------------------------------------------------------------
mysummaryPlot<-function(x){
  m1<-matrix(1:4,2)
	layout(m1)
	plot(x)
	hist(x)
	boxplot(x)
	qqnorm(x)
	}

## ------------------------------------------------------------------------

mysummaryPlot(x1)

## ------------------------------------------------------------------------
palette()
length(colors())
head(colors())

## ------------------------------------------------------------------------
set.seed(6345789)
numColors<-10
n1<-sample(1:length(colors()),replace=FALSE,
           size=numColors)
pie(rep(1,numColors),col=colors()[n1],
    labels=colors()[n1])
title(main="A Color Wheel")

## ------------------------------------------------------------------------
set.seed(6345789)
numColors<-10
n1<-sample(1:length(colors()),replace=FALSE,
           size=numColors)
pie(rep(1,numColors),col=colors()[n1],
    labels=colors()[n1])
title(main="A Color Wheel")

## ------------------------------------------------------------------------
numColors<-8
pie(rep(1,numColors),col=rainbow(numColors),
    labels=rainbow(numColors))
title(main=paste("rainbow(",numColors,")",sep=""))
title(sub="Note that the names are in RGB")

## ------------------------------------------------------------------------
library(RColorBrewer)
display.brewer.pal(6,"RdBu")

## ------------------------------------------------------------------------
X1<-rep(1:5,5)
Y1<-c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
plot(x=X1,y=Y1,pch=1:25,col="blue",bg="red",cex=2,bty="n", xlim=c(0.5,5), 
     ann=FALSE,axes=FALSE)
text(x=X1-.25,y=Y1,1:25)
title(main="Plotting Characters pch=1:25")

## ------------------------------------------------------------------------
btyopts<-c("n","l","7","c","u","]")
m1<-matrix(1:6,nc=2)
layout(m1)
for (i in 1:6){
plot(x=1,y=1,bty=btyopts[i])
  title(main=paste0('bty = ',btyopts[i]))}

## ------------------------------------------------------------------------
plot(x=1,y=1,xlab="Here's where xlab goes",
  ylab="Here's where ylab goes",type="n",bty="n")
title(main="Here's the main title",sub="Here's the subtitle")

## ------------------------------------------------------------------------
plot(x=1,y=1,
	xlim=c(-2,2),
	ylim=c(0,4)
)

## ------------------------------------------------------------------------

plot(x=1,y=1,type="n",xlim=c(0,2),ylim=c(0,2),
     ann=FALSE,bty="n",
      xaxt="n",yaxt="n")
text(x=0,y=1.9,labels="Here's font=1 (default) ",
     font=0.5,adj=c(0,NA))
text(x=0,y=1.7,labels="Here's font=2 (bold)",
     font=2,adj=c(0,NA))
text(x=0,y=1.5,labels="Here's font=3 (italics)",
     font=3,adj=c(0,NA))
text(x=0,y=1.3,labels="Here's font=4 (bold italics)",
     font=4,adj=c(0,NA))
text(x=0,y=1.1,labels="Here's font=5 (symbols)",
     font=5,adj=c(0,NA))

## ------------------------------------------------------------------------
plot(x=1,y=1,type="n",xlim=c(1,2),ylim=c(1,2))
text(x=1.2,y=1.9,labels="Here's cex=.5",cex=.5)
text(x=1.2,y=1.7,labels="Here's cex=.75",cex=.75)
text(x=1.2,y=1.5,labels="Here's cex=1 (default)",cex=1)
text(x=1.2,y=1.3,labels="Here's cex=1.25",cex=1.25)
text(x=1.2,y=1.1,labels="Here's cex=1.5",cex=1.5)

## ------------------------------------------------------------------------
plot(x=3,y=3,type="n",bty="n",
     ylab="lty",ylim=c(0,6),xaxt="n",xlab="",
     main="Line Type (lty=)")
abline(h=0:6,lty=0:6)

## ------------------------------------------------------------------------
plot(x=c(1,2),y=c(1,2),bty="n",col=c("red","blue"),pch=19)
legend("right",legend=c("Red Dot","Blue Dot"),
       fill=c("red","blue"))

## ------------------------------------------------------------------------
c1<-c(10,-2,5)
t1<-c("A","B","C")
col1<-c("blue","yellow","green")
barplot(c1,names.arg=t1,col=col1)

## ------------------------------------------------------------------------
f1<-function(x) x*(x^2-1)
x1<-seq(from=-5,to=5,length=100)
y1<-f1(x1)

plot(x=x1,y=y1,type="l")

## ------------------------------------------------------------------------
library(datasets)
data(chickwts)
head(chickwts)
boxplot(weight~feed,
data=chickwts)

## ------------------------------------------------------------------------
curve(f1,xlim=c(-5,5))

## ------------------------------------------------------------------------
t1<-rnorm(1000)
hist(t1)

## ------------------------------------------------------------------------
main1<-'Histogram of 1,000 random  \n standard normal variables'

hist(t1,main=main1)

## ------------------------------------------------------------------------
sub1<-paste('Mean is', round(mean(t1),3),sep=' ')
hist(t1,main=main1,sub=sub1)

## ------------------------------------------------------------------------
hist(t1,main=main1,
	sub=sub1,
	freq=FALSE,
	xlab="Value", 
	ylab=
	"Probability",
	xlim=c(-5,5),
	ylim=c(0,.5))

## ------------------------------------------------------------------------
hist(t1,main=main1,
	sub=sub1,
	freq=FALSE,
	xlab="Value", 
	ylab=
	"Probability",
	xlim=c(-5,5),
	ylim=c(0,.5))
curve(dnorm,col="red",add=TRUE)

## ------------------------------------------------------------------------
hist(t1,main=main1,
	sub=sub1,
	freq=FALSE,
	xlab="Value", 
	ylab=
	"Probability",
	xlim=c(-5,5),
	ylim=c(0,.5))
curve(dnorm,col="red",add=TRUE)
text(x=3,y=0.4,
	expression(frac(1,sqrt(2*pi))* e^-frac(x^2,2)),
	cex=2)

## ------------------------------------------------------------------------
library(ChainLadder)
data(RAA)
class(RAA)
head(RAA)

## ------------------------------------------------------------------------
plot(RAA)

## ------------------------------------------------------------------------
ChainLadder:::plot.triangle

## ----echo=FALSE----------------------------------------------------------
main2<-"Reinsurance Association of America \n Loss Development"
plot(RAA,pch=19,main=main2,xlab="Development Period (years)",bty="l",
	ylab="Loss Amount",xlim=c(1,11),cex.main=2,lty=1,
	xaxp=c(0,10,10))
legend("bottomright",legend=rownames(RAA),bty="n",fill=1:6,title="Accident Year")

## ----echo=TRUE,eval=FALSE------------------------------------------------
## main2<-"Reinsurance Association of America \n Loss Development"
## plot(RAA,pch=19,main=main2,xlab="Development Period (years)",bty="l",
## 	ylab="Loss Amount",xlim=c(1,11),cex.main=2,lty=1,
## 	xaxp=c(0,10,10))
## legend("bottomright",legend=rownames(RAA),bty="n",fill=1:6,title="Accident Year")

