###################################################################
################## Econometria 1 - Problem Set 5 ##################
################### Jos? Parreiras Antunes Neto ###################
###################################################################

library(AER)
library(stargazer)
set.seed(40028922)

# Question 3 --------------------------------------------------------------

reps<-1000
obs<-150
b_ols<-list(coef=c(), var=c())
b_iv<-list(coef=c(), var=c())
b_2sls<-list(coef=c(), var=c())
b_2sls_2<-list(coef=c(), var=c())
b_2sls_3<-list(coef=c(), var=c())
for(i in 1:reps){
  # Variáveis #
  v1<-rnorm(obs)
  v2<-rnorm(obs)
  v3<-rnorm(obs)
  z1<-rnorm(obs)
  z2<-rnorm(obs)
  z3<-rnorm(obs)                      # letra (d)
  zk<-matrix(ncol=100, nrow=obs)      # letra (e)
      zk[,1]<-z1                      #
      zk[,2]<-z2                      #
      zk[,3]<-z3                      #
      for(j in 4:100){                #
        zk[,j]<-rnorm(obs)            #
      }                               #
  x<-z1+z2+v1+v3
  y<-1+x+v1+v2
  
  # Modelos #
  ols<-lm(y~x)
  iv<-ivreg(y~x|z1) 
  twostage<-ivreg(y~x|z1+z2)
  twostage2<-ivreg(y~x|z1+z2+z3)
  twostage3<-ivreg(y~x|zk)
  
  # Resgatando Coeficientes #
  b_ols[[1]][i]<-coef(ols)[2]
  b_ols[[2]][i]<-vcov(ols)[2,2]
  b_iv[[1]][i]<-coef(iv)[2]
  b_iv[[2]][i]<-vcov(iv)[2,2]
  b_2sls[[1]][i]<-coef(twostage)[2]
  b_2sls[[2]][i]<-vcov(twostage)[2,2]
  b_2sls_2[[1]][i]<-coef(twostage2)[2]
  b_2sls_2[[2]][i]<-vcov(twostage2)[2,2]
  b_2sls_3[[1]][i]<-coef(twostage3)[2]
  b_2sls_3[[2]][i]<-vcov(twostage3)[2,2]
  
  # Limpando variáveis do environment #
  rm(v1,v2,v3,z1,z2,z3,zk,x,y,ols,iv, twostage,twostage2,twostage3)
}

# Matriz de estatísticas descritivas #
stat<-matrix(ncol=2, nrow=5)
stat[1,]<-c(mean(b_ols[[1]]), sd(b_ols[[1]]))
stat[2,]<-c(mean(b_iv[[1]]), sd(b_iv[[1]]))
stat[3,]<-c(mean(b_2sls[[1]]), sd(b_2sls[[1]]))
stat[4,]<-c(mean(b_2sls_2[[1]]), sd(b_2sls_2[[1]]))
stat[5,]<-c(mean(b_2sls_3[[1]]), sd(b_2sls_3[[1]]))
rownames(stat)<-c('OLS','IV','2SLS','2SLS (d)', '2SLS (e)')
colnames(stat)<-c('Mean','SD')
stargazer(stat)
