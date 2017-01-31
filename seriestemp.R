############################
don <- readRDS("mat")
plot(don[1,],type="l")
############################
TT <- ncol(don)
vecT <- 1:TT
DATA <- data.frame(Z=don[1,],C1=cos(2*pi/TT*vecT),S1=sin(2*pi/TT*vecT))
###########################
reg1 <- lm(Z~1,data=DATA)
lines(reg1$fitted,col=2)
###
reg2 <- lm(Z~C1+S1,data=DATA)
lines(reg2$fitted,col=3)
### UNE BASE
dim <- 10
for(ii in 2:dim){
    DATA <- cbind(DATA,cos(2*pi/TT*vecT*ii),sin(2*pi/TT*vecT*ii))
}
names(DATA)[2:(2*dim+1)] <- paste(c("C","S"),rep(1:dim,each=2),sep="")
reg3 <- lm(Z~.,data=DATA)
lines(reg3$fitted,col=4)
### UNE BASE
dim <- 100
for(ii in 2:dim){
    DATA <- cbind(DATA,cos(pi/TT*vecT*ii),sin(pi/TT*vecT*ii))
}
names(DATA)[2:(2*dim+1)] <- paste(c("C","S"),rep(1:dim,each=2),sep="")
reg4 <- lm(Z~.,data=DATA)
lines(reg4$fitted,col=4)

