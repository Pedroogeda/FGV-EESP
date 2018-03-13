###################################################################
################## Econometria 1 - Problem Set 2 ##################
################### José Parreiras Antunes Neto ###################
###################################################################


# Problem 4 ---------------------------------------------------------------

# Equation: Y=10+0.1X+u
# u~N(0,1)
# X~N(2,1)

set.seed(40028922)

rep<-10000
obs<-300
b<-matrix(NA, ncol=2, nrow=rep)
var_b<-list(NA)

for (i in 1:rep){
x<-cbind(rep(1,obs),rnorm(obs, mean = 2, sd = 1))
u<-rnorm(obs, mean = 0, sd = 1)
y<-10*x[,1]+0.1*x[,2]+u

b[i,]<-t(solve(t(x)%*%x)%*%t(x)%*%y)
var_b[[i]]<-solve(t(x)%*%x)
}

hist(b[,2], col='gray40')
mean(b[,2])
sd(b[,2])

test.t<-matrix(NA, ncol=4, nrow=rep)
colnames(test.t)<-c('0.1','0.95','0.5','0')

for (i in 1:rep){
  test.t[i,1]<-(b[i,2]-0.1)/sqrt(var_b[[i]][2,2])
  test.t[i,2]<-(b[i,2]-0.95)/sqrt(var_b[[i]][2,2])
  test.t[i,3]<-(b[i,2]-0.5)/sqrt(var_b[[i]][2,2])
  test.t[i,4]<-(b[i,2]-0)/sqrt(var_b[[i]][2,2])
}

test.p<-pnorm(test.t)
rejection<-colSums(test.p<0.05)
