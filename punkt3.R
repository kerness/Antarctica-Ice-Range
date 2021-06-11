library(rsm)
dane <- read.csv("data/daily_ice_edge.csv")

im<-as.matrix(dane[1:183,2:361])
gr<-expand.grid(1:183,1:360)
df<-data.frame(z=as.vector(im),x=gr[,1],y=gr[,2])

fit <- lm(z ~ poly(x, y, degree = 25), data = df)
pred<-predict(fit)
res<-matrix(nrow=183,ncol=360,pred)
par(mfrow=c(2,2))
 
image(im,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))
 
image(res,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))