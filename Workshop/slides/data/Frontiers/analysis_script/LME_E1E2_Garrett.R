## Code by Shravan Vasishth
## date: 7th October 2019
## Modified by Garrett Smith, 8th October 2019

library(lme4)
library(lmerTest)

priming=read.csv("E1_RTAll.csv",header=TRUE)
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

# Garrett mv:
#W5<-subset(e2dat,Position==5)

e2dat$Sub<-factor(e2dat$Sub)
e2dat$ID<-factor(e2dat$ID)

e2dat$SO<-ifelse(e2dat$Type==1,0.5,-0.5)
# Garrett:
W5<-subset(e2dat,Position==5)

m_e2<-lmer(RT~SO+(1+SO||Sub)+(1+SO||ID),W5,REML=FALSE,control = lmerControl(optimizer ="Nelder_Mead"))
summary(m_e2)

m_e2_0<-lmer(RT~1+(1+SO||Sub)+(1+SO||ID),W5,
             REML=FALSE,control = lmerControl(optimizer ="Nelder_Mead"))

anova(m_e2,m_e2_0)

# Garrett: this is now significant:
mw5 <- lmer(RT~SO+(1+SO||Sub)+(1+SO||ID),W5,REML=FALSE,control = lmerControl(optimizer ="Nelder_Mead"))
summary(mw5)

mw5_0 <- lmer(RT~1+(1+SO||Sub)+(1+SO||ID), W5,
              REML=FALSE,control = lmerControl(optimizer ="Nelder_Mead"))
## significant:
anova(mw5, mw5_0)

## significant but wrong:
m_e2minimal<-lmer(RT~SO+(1|Sub)+(1|ID),e2dat,control = lmerControl(optimizer ="Nelder_Mead"))

summary(m_e2minimal)

# Garrett, brms, Exp. 1
library(brms)
library(bayesplot)
headnoun$LogRTms <- log(1000*headnoun$RT)
get_prior(LogRTms ~ SO + (1 + SO | Sub) + (1 + SO | ID), data=headnoun)

priors <- c(set_prior('normal(0, 10)', class='Intercept'),
            set_prior('normal(0, 1)', class='b'),
            set_prior('normal(0, 1)', class='sd'),
            set_prior('normal(0, 1)', class='sigma'),
            set_prior('lkj(2)', class='cor'))


## Bayes factors by SV:
b0 <- brm(LogRTms ~ SO + (1 + SO | Sub) + (1 + SO | ID), data=headnoun,
      save_all_pars = TRUE,
      warmup = 4000,
      iter=20000,
          chains=4, prior=priors)

b0

priorsNULL <- c(set_prior('normal(0, 10)', class='Intercept'),
            set_prior('normal(0, 1)', class='sd'),
            set_prior('normal(0, 1)', class='sigma'),
            set_prior('lkj(2)', class='cor'))



b0_null <- brm(LogRTms ~ 1 + (1 + SO | Sub) + (1 + SO | ID), data=headnoun,
          save_all_pars = TRUE,
          warmup = 4000,
          iter=20000,
          chains=4, prior=priorsNULL)

## a value larger than 3 would be some evidence for alternative hypothesis: 
bayes_factor(b0,b0_null)
## Result: 0.45, weak support for null: BF 2 for null

mcmc_areas(b0, pars=c('b_SO', 'sigma'), prob=0.8, prob_outer=0.95)

# Exp. 2
W5$LogRTms <- log(1000*W5$RT)

b1 <- brm(LogRTms ~ SO + (1 + SO | Sub) + (1 + SO | ID), data=W5,
          warmup= 4000,
          iter = 20000,
          save_all_pars = TRUE,
          cores=4, prior=priors)

b1_null <- brm(LogRTms ~ 1 + (1 + SO | Sub) + (1 + SO | ID), data=W5,
          warmup= 4000,
          iter = 20000,
          save_all_pars = TRUE,
          cores=4, prior=priorsNULL)


summary(b1)

## a value larger than 3 would be some evidence for alternative hypothesis: 
bayes_factor(b1,b1_null)
## .45 -> favors null by a factor of 2.

mcmc_areas(b1, pars=c('b_SO', 'sigma'), prob=0.8, prob_outer=0.95)

## Informative priors analysis
## Now we assume the effect is a priori small


priorsINF <- c(set_prior('normal(0, 10)', class='Intercept'),
            set_prior('normal(0, 0.02)', class='b'),
            set_prior('normal(0, 1)', class='sd'),
            set_prior('normal(0, 1)', class='sigma'),
            set_prior('lkj(2)', class='cor'))


## Bayes factors by SV:
b0INF <- brm(LogRTms ~ SO + (1 + SO | Sub) + (1 + SO | ID), data=headnoun,
          save_all_pars = TRUE,
          warmup = 4000,
          iter=20000,
          chains=4, prior=priorsINF)

bayes_factor(b0INF,b0_null)
## weak support for alternative: 4

b1INF <- brm(LogRTms ~ SO + (1 + SO | Sub) + (1 + SO | ID), data=W5,
          warmup= 4000,
          iter = 20000,
          save_all_pars = TRUE,
          cores=4, prior=priorsINF)

bayes_factor(b1INF,b1_null)
## BF 1.5, equivocal


## Summary
## E1
## frequentist: n.s.
## Bayesian:
## uninformative N(0,1) prior on beta: BF 1/2 
##  favors null weakly
## informative N(0,0.02) prior on beta: BF 4 
##  favors alternative weakly   

## E2
## frequentist: p<0.05 OR advantage
## Bayesian:
## uninformative N(0,1) prior on beta: BF 1/2 
##  favors null weakly
## informative N(0,0.02) prior on beta: BF 1.5 
##  inconclusive   
