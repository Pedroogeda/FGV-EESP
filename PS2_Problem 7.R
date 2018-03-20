###################################################################
################## Econometria 1 - Problem Set 2 ##################
################### José Parreiras Antunes Neto ###################
###################################################################

library(foreign)
library(plm)
library(sandwich)
library(lmtest)

db<-read.dta('DB/base_clima_datasus2010.dta')
time<-paste(db$ano, db$mes)
db<-cbind(time, db)
db<-db[-which(is.na(db$dyang_last12) | is.na(db$k_obt_nasc)),]
# db<-pdata.frame(db, index=c('código_6','time'))


# Estimation --------------------------------------------------------------

model<-lm(k_obt_nasc~dyang_last12, data=db)

# Variances ---------------------------------------------------------------

v1<-vcov(model) # Homoskedastic
v3<-vcovHC(model) # Heteroskedastic
v4<-vcovCL(model, cluster = db$código_7)  # Clustered

# Hypotesis Testing -------------------------------------------------------

t1<-coeftest(model, vcov=v1)
t3<-coeftest(model, vcov=v3)
t4<-coeftest(model, vcov=v4)

# Enriched Model ----------------------------------------------------------

model2<-lm(k_obt_nasc~dyang_last12+share_com_luz+share_sem_sanit+peso_nasc+share_sem_aguacanal, data=db)
round(coeftest(model2),2)
