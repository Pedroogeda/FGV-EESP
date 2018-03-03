###### Monte Carlo simulation for Bernoulli mean ######
### José Parreiras Antunes Neto ###

## Instructions
# 1. Install each of the following packages before running
# 2. The seed is fixed. So for different results, try another seed
# 3. Verify your working directory, because the images and text will 
# be saved there.

# Packages
library(ggplot2)
library(moments)
library(stargazer)

set.seed(40028922)
rep<-10000                         # Number of repetitions in the MC sim
sample<-c(100,1000,10000)         # Sample size for the Bernoulli
parameter<-c(0.1,0.5,0.75)        # Bernoulli parameter (theta)

MCsim<-vector(mode = 'numeric',length = rep)
JB<-matrix(NA, nrow = length(parameter), ncol = length(sample))
colnames(JB)<-sample
rownames(JB)<-parameter

for (i in 1:length(sample)){
  n<-sample[i]
for (j in 1:length(parameter)){
  theta<-parameter[j]

  for (l in 1:rep) {
  (x<-rbinom(n, 1, theta))    # Repeating #rep times the randomize
  mean<-mean(x)               # and assigning values of sample mean
  MCsim[l]<-mean              # for the dataframe
  }

### Normality Test ###
# Here we perform a Jarque-Bera test for the sample of means
  kurtosis(MCsim)-3
  skewness(MCsim)
  jb<-rep/6*(skewness(MCsim)^2+1/4*(kurtosis(MCsim)-3)^2)
  p<-pchisq(jb,2, lower.tail=F)
  JB[j,i]<-p
  
### Q-Q plot ###
# A quantile-quantile plot helps us compare the sample with a theoretical
# Normal distribution. The more linear the plot, the closer we are from
# a Normal
  plot<-ggplot(data.frame(MCsim), aes(sample=MCsim))+
    geom_qq(distribution=qnorm, colour='darkblue')+
    xlab('Normal Distribution')+
    ylab('Monte Carlo Simulation')+
    ggtitle('Q-Q Plot',paste('p=',theta,'     rep=',rep,'     n=',n, sep=''))
  ggsave(paste('QQplot-',i,j,'.png', sep=''), plot, device = 'jpeg')
}}

table<-file('JB-Test.txt')
writeLines(stargazer(round(JB,3), title = 'Jarque-Bera test for $X_n$',
                     covariate.labels = 'p/n', style = 'aer'),
           'JB-Test.txt')
