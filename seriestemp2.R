############################
don <- readRDS("mat")
plot(don[1,],type="l")
############################
TT <- ncol(don)
vecT <- 1:TT
DATA <- data.frame(C1=cos(2*pi/TT*vecT),S1=sin(2*pi/TT*vecT))
###########################
reg1 <- lm(Z~1,data=data.frame(Z=don[1,],DATA))
lines(reg1$fitted,col=1,lwd=2)
#######
lines(vecT,don[220,],col=4)
reg220 <- lm(Z~1,data=data.frame(Z=don[220,],DATA))
lines(reg220$fitted,col=4,lwd=2)
### UNE BASE
dim <- 100
DATA <- NULL
for(ii in 1:dim){
    DATA <- cbind(DATA,cos(2*pi/TT*vecT*ii),sin(2*pi/TT*vecT*ii))
}
names(DATA)[1:(2*dim)] <- paste(c("C","S"),rep(1:dim,each=2),sep="")

plot(vecT,vecT*0,ylim=c(0,100),col=0)
reg4 <- lm(Z~.,data=data.frame(Z=don[1,],DATA))
lines(reg4$fitted,col=2)
reg4 <- lm(Z~.,data=data.frame(Z=don[220,],DATA))
lines(reg4$fitted,col=3)

par(mfrow=c(3,1))
plot(don[1,],type="l",col=2)
lines(don[220,],col=3)     

plot(vecT,vecT*0,ylim=c(0,100),col=0)
reg4 <- lm(Z~.,data=data.frame(Z=don[1,],DATA[,1:20]))
lines(reg4$fitted,col=2)
reg4 <- lm(Z~.,data=data.frame(Z=don[220,],DATA[,1:20]))
lines(reg4$fitted,col=3)

plot(1:21,(1:21)*0,ylim=c(-10,100),col=0)
reg4 <- lm(Z~.,data=data.frame(Z=don[1,],DATA[,1:20]))
lines(reg4$coeff,col=2)
reg4 <- lm(Z~.,data=data.frame(Z=don[220,],DATA[,1:20]))
lines(reg4$coeff,col=3)

