## Code by Shravan Vasishth
## date: 7th October 2019

library(lme4)
library(lmerTest)

priming=read.csv("E1_RTAll.csv",header=TRUE)
head(priming)

m_orig<-lmer(RT~Type*Position+(1|Sub)+(1|ID),
           priming)
summary(m_orig)

acf(residuals(m_orig))

## Type is subject vs object RC probably, not sure which is which:
xtabs(~Sub+Type,priming)

## ID seems to be item:
xtabs(~Sub+ID,priming)

## checks out:
length(unique(priming$Sub))
length(unique(priming$ID))

## some imbalance, probably due to data removal:
xtabs(~ID+Type,priming)


summary(priming)

## head noun:
## We aim to reproduce this analysis:
## "the ORC sentence was found to read significantly faster than the SRC sentence at the head noun region (W4, β = −0.03, SE = 0.01, t = −2.68, p = 0.007)"
headnoun<-subset(priming,Position==4)
summary(headnoun)

headnoun$ID<-factor(headnoun$ID)
headnoun$Sub<-factor(headnoun$Sub)
## Type==1 is SR I think:
headnoun$SO<-ifelse(headnoun$Type==1,0.5,-0.5)
## a positive slope will mean SRs are harder

#model1=lmer(RT~Type*Position+(1|Sub)+(1|ID),data=priming)
#summary(model1)

## some missing data, not serious:
xtabs(~Sub+ID,headnoun)

xtabs(~Sub+SO,headnoun)

boxplot(RT~Type,headnoun)

## ms:
1000*with(headnoun,tapply(RT,
                          Type,mean,
                          na.rm=TRUE))

m_hn<-lmer(RT~SO+(1+SO||Sub)+(1|ID),
           headnoun,control = lmerControl(optimizer ="Nelder_Mead"),REML=FALSE)
summary(m_hn)

m_hn0<-lmer(RT~1+(1+SO||Sub)+(1|ID),
            headnoun,control = lmerControl(optimizer ="Nelder_Mead"),REML=FALSE)

anova(m_hn0,m_hn)

summary(m_hn)

m_hnminimal<-lmer(RT~SO+(1|Sub)+(1|ID),
           headnoun,control = lmerControl(optimizer ="Nelder_Mead"),REML=FALSE)

summary(m_hnminimal)

## Expt 2
#"Chinese SRCs were more difficult to comprehend than ORCs, significantly reflected in W5 (β = −0.08, SE = 0.03, t = −2.93, p = 0.004"

e2dat=read.csv("E2_RTAll.csv",header=TRUE)
head(e2dat)

W5<-subset(e2dat,Position==5)

e2dat$Sub<-factor(e2dat$Sub)
e2dat$ID<-factor(e2dat$ID)

e2dat$SO<-ifelse(e2dat$Type==1,0.5,-0.5)

m_e2<-lmer(RT~SO+(1+SO||Sub)+(1+SO||ID),e2dat,REML=FALSE,control = lmerControl(optimizer ="Nelder_Mead"))
summary(m_e2)

m_e2_0<-lmer(RT~1+(1+SO||Sub)+(1+SO||ID),e2dat,
             REML=FALSE,control = lmerControl(optimizer ="Nelder_Mead"))

anova(m_e2,m_e2_0)

## significant but wrong:
m_e2minimal<-lmer(RT~SO+(1|Sub)+(1|ID),e2dat,control = lmerControl(optimizer ="Nelder_Mead"))

summary(m_e2minimal)
