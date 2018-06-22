library(AER)

set.seed(40028922)

sample<-300
rep<-10000

a<-c()
b<-0
c<-c(0,0,0)

for(i in 1:rep){
 z1<-rnorm(sample)
 z2<-rnorm(sample)
 z3<-rnorm(sample)
 v<-rnorm(sample)
 e<-rnorm(sample)
 x<-z1+z2+z3+v
 y<-x+z2+e
 
 # Question a)
 a<-lm(y~-1+x)
 
 # Question b)
 resid<-as.matrix(resid(ivreg(y~x | z1+z2+z3)))
 z<-cbind(z1,z2,z3)
 stat<-2*sample*x%*%z%*%solve(t(z)%*%z)%*%t(z)%*%resid
 if(pchisq(stat,2)>=0.95){b<-b+1/rep}
 
 # Question c)
 resid<-as.matrix(resid(ivreg(y~x | z1+z2)))
 z<-cbind(z1,z2)
 stat<-2*sample*x%*%z%*%solve(t(z)%*%z)%*%t(z)%*%resid
 if(pchisq(stat,1)>=0.95){c[1]<-c[1]+1/rep}
 
 resid<-as.matrix(resid(ivreg(y~x | z1+z3)))
 z<-cbind(z1,z3)
 stat<-2*sample*x%*%z%*%solve(t(z)%*%z)%*%t(z)%*%resid
 if(pchisq(stat,1)>=0.95){c[2]<-c[2]+1/rep}
 
 resid<-as.matrix(resid(ivreg(y~x | z2+z3)))
 z<-cbind(z2,z3)
 stat<-2*sample*x%*%z%*%solve(t(z)%*%z)%*%t(z)%*%resid
 if(pchisq(stat,1)>=0.95){c[3]<-c[3]+1/rep}
}


