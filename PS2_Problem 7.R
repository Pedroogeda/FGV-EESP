library(foreign)
library(plm)
library(sandwich)
library(lmtest)

db<-read.dta('base_clima_datasus2010.dta')
time<-paste(db$ano, db$mes)
db<-cbind(time, db)
db<-db[-which(is.na(db$dyang_last12) | is.na(db$k_obt_nasc)),]
# db<-pdata.frame(db, index=c('código_6','time'))


# Estimation --------------------------------------------------------------

model<-lm(k_obt_nasc~dyang_last12, data=db)

# variances ---------------------------------------------------------------

v1<-vcov(model)
v3<-vcovHC(model)
v4<-vcovCL(model, cluster = db$código_7)

# Hypotesis Testing -------------------------------------------------------

t1<-coeftest(model, vcov=v1)
t3<-coeftest(model, vcov=v3)
t4<-coeftest(model, vcov=v4)
