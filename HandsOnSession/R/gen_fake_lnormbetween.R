library(MASS)
gen_fake_lnormbetween <- function(nitem=16,nsubj=42,
                           alpha=NULL,beta=NULL,
                           sigma_e=NULL){
  ## prepare data frame for two condition in a latin square design:
  SR<-data.frame(subj=rep(1:(nsubj/2),each=nitem),item=rep(1:nitem,nsubj/2),
                 cond="SR")
  OR<-data.frame(subj=rep(((nsubj/2)+1):nsubj,each=nitem),item=rep(1:nitem,nsubj/2),
                 cond="OR")
  
  fakedat<-rbind(SR,OR)
  fakedat$so<-ifelse(fakedat$cond=="OR",1,-1)
  
  ## generate data row by row:
  N<-dim(fakedat)[1]
  rt<-rep(NA,N)
  for(i in 1:N){
    rt[i] <- rlnorm(1,alpha +
                      beta*fakedat$so[i],
                    sigma_e)}
  fakedat$rt<-rt
  fakedat$subj<-factor(fakedat$subj); fakedat$item<-factor(fakedat$item)
  fakedat
  }