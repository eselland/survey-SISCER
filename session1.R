#### session 1 
load(file = "nhis_data.rda")
library(survey)
nhis<-svydesign(data=nhis_data, ids = ~psu_p,strata = ~strat_p,weights = ~wtfa_sa,nest=TRUE)
nhis

table(nhis_data$pdsicka) #how is everything coded
#make new variables (like mutate with tidyverse)
nhis<-update(nhis,sickleave=factor(ifelse(pdsicka=="1 Yes","yes","no"))) #recodes everything to either yes or no, which is probably not the proper way to do it 
nhis<-update(nhis,backpain=factor(ifelse(painlb=="1 Yes","yes","no")))
nhis<-update(nhis,neckpain=factor(ifelse(paineck=="1 Yes","yes","no")))

dim(nhis) #number of observations and number of variables

svytotal(~sickleave,nhis,na.rm=TRUE) #how many people have sick leave and how many dont
svytotal(~backpain,nhis) #how many people report back pain
svytotal(~neckpain,nhis)
svymean(~neckpain,nhis) #means and standard error


###using graphics to explore how hours of sleep varies with age and sex
svyhist(~sleep,design=nhis)
svytable(~sleep+sex,design=nhis)
byagesex<-svyby(~sleep,~sex+age_p,svymean,design=nhis,na.rm=TRUE)
m<-svysmooth(sleep~age_p,design=subset(nhis,sex=="1 Male"))
f<-svysmooth(sleep~age_p,design=subset(nhis,sex=="2 Female"))
plot(rep(18:85,2),coef(byagesex),pch=c(1,19),ylim=c(5,20))
lines(m,lty=2)
lines(f,lty=1)


#logistic regression

agemodel<-svyglm(sleep~age_p,design=nhis)
summary(agemodel)
library(lme4) #how does this compare to linear model on the dataset
summary(lm(sleep~age_p,data=nhis_data)) #survey package is accounting for clustering and weights
#we'd have to account for (1|psu_p) aka the clustering which we put already when we made the survey design with svydesign
#so like lmer(sleep~age_p+(1|psu_p),data=nhis_data) would get us close with estimates and standard errors, but not completely the same because it isn't fully accounting for the weights
#versus svyglm is accounting for all of that wihtout us even having to think about it
    #sounds like that is the power part

agemodel2<-svyglm(sleep~(pmin(age_p,70)+pmax(age_p,70)),design=nhis)
summary(agemodel2)
summary(svyglm(sleep~sex,design = nhis))
summary(svyglm(sleep~sex*age_p, design=nhis))

svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
summary(nhis_data$sleep)
with(nhis_data, table(sleep[sleep>24]))
nhis<-update(nhis, sleep=ifelse(sleep>24,NA,sleep))    #gets rid of any sleeping more than 24 hours (Which doesnt make sense) 
svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
svyplot(sleep~age_p,design=nhis, style="hex") #hexagon binning

svyplot(sleep~age_p,design=nhis, style="trans", pch=19)
age_smth<-svysmooth(sleep~age_p, design=nhis)
lines(age_smth,col="orange",lwd=2)



nhis2<-update(nhis, #categorizing the data
             agelo=pmin(30,age_p),
             agemid=pmin(55,pmax(30,age_p)),
             agehi=pmax(55,age_p)
)
summary(svyglm(sleep~sex*(agelo+agemid+agehi), design=nhis2))


#some t tests, design based
svyttest(sleep~sex, design=nhis) #sleep is diff between sexes
#highly statistically signif but also very small difference
svyranktest(sleep~sex, design=nhis) #means are diff, kruskalwallis test
svyranktest(sleep~sex, design=nhis,test="median")
#^^^all agree that there is a difference for sleep between sexes but it is small

svyttest(sleep~backpain, design=nhis) 
svyranktest(sleep~backpain, design=nhis)
svyranktest(sleep~backpain, design=nhis,test="median")
svyboxplot(sleep~backpain, design=nhis,all.outliers=TRUE)

#on a logit scale
svyciprop(~backpain, design=nhis) #gives proportion with back pain and confidence intervals, which he said can be helpful when the proportions are close to 0??
#if we want it symetric 
svyciprop(~backpain, design=nhis,method="mean")
svyciprop(~I(backpain=="yes"), design=nhis,method="mean") #but large enough sample size that you cant tell a diff between different confidence interval methods
?svyciprop #will show the differences in methods for confidence intervals

## http://gdfe.co/srvyr/