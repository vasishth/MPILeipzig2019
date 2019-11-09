
library(lme4)
library(lmerTest)

priming=read.csv("RTAll.csv",header=TRUE)
head(priming)

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

m_hn0<-lmer(RT~1+(1+SO||Sub)+(1|ID),
            headnoun,control = lmerControl(optimizer ="Nelder_Mead"),REML=FALSE)

anova(m_hn0,m_hn)

summary(m_hn)
