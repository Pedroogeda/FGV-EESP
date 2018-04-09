# Question 5 --------------------------------------------------------------

library(foreign)
library(lmtest)
library(AER)

jec<-read.dta('JEC.dta')

model<-lm(log(quantity)~log(price), data = jec)
iv.model<-ivreg(log(quantity)~log(price) | cartel , data = jec)

# t.test = -1
t<-(coef(iv.model)[2]+1)/vcov(iv.model)[2,2]
p<-pt(t, 328-1)
q<-qt(0.025, 328-2)

iv.model2<-ivreg(log(quantity)~log(price) | ice, data=jec)

# Question 6 --------------------------------------------------------------

library(dplyr)
library(AER)

CSHP<-read.csv('temp_CSHomePrice.mat.csv') %>%
  group_by(year, circuit)  %>%
  mutate(CSIndex=mean(logpriceindex))
CSHP<-CSHP[!duplicated(CSHP[,-1]),]

reg<-lm(CSIndex~numpro_casecat_12, data=CSHP)
summary(reg)

probs<-read.table(file = 'probs.txt', header = T) %>%
  group_by(Syear, Scircuit) %>%
  filter(Syear %in% unique(CSHP$year) & Scircuit %in% unique(CSHP$circuit)) %>%
  rename(year=Syear, circuit=Scircuit)

db<-merge(CSHP,probs, by=c('year','circuit'))

stage1<-lm(numpro_casecat_12~numpanels1x_dem, data=db)

controls <- c("missing_cy_12","numcasecat_12","prob_1x_dem","prob_2x_dem","prob_3x_dem","prob_1x_female","prob_2x_female","prob_3x_female","prob_1x_nonwhite","prob_2x_nonwhite","prob_3x_nonwhite","prob_1x_black","prob_2x_black","prob_3x_black","prob_1x_jewish","prob_2x_jewish","prob_3x_jewish","prob_1x_catholic","prob_2x_catholic","prob_3x_catholic","prob_1x_noreligion","prob_2x_noreligion","prob_3x_noreligion","prob_1x_instate_ba","prob_2x_instate_ba","prob_3x_instate_ba","prob_1x_ba_public","prob_2x_ba_public","prob_3x_ba_public","prob_1x_jd_public","prob_2x_jd_public","prob_3x_jd_public","prob_1x_elev","prob_2x_elev","prob_3x_elev","prob_1x_female_black","prob_2x_female_black","prob_3x_female_black","prob_1x_female_noreligion","prob_2x_female_noreligion","prob_3x_female_noreligion","prob_1x_black_noreligion","prob_2x_black_noreligion","prob_3x_black_noreligion","prob_1x_female_jd_public","prob_2x_female_jd_public","prob_3x_female_jd_public","prob_1x_black_jd_public","prob_2x_black_jd_public","prob_3x_black_jd_public","prob_1x_noreligion_jd_public","prob_2x_noreligion_jd_public","prob_3x_noreligion_jd_public","prob_1x_mainline","prob_2x_mainline","prob_3x_mainline","prob_1x_evangelical","prob_2x_evangelical","prob_3x_evangelical","prob_1x_protestant","prob_2x_protestant","prob_3x_protestant")
controls <- as.matrix(db[,which(colnames(db)%in%controls)])

ivreg<-ivreg(CSIndex~numpro_casecat_12+controls | numpanels1x_dem+controls , data=db)
coeftest(ivreg)
