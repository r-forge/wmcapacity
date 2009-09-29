`SimDat` <-
function(Nsub=23,Ntrl=60,SetSizes= c(2,5,8), muK=3.1,sdK=1,muZ = 2.9,sdZ = .46,muIntG=.77,sdIntG=.39,muSlpG=1,sdSlpG=.44,prch = qlogis(c(.3,.5,.7)),KEffect = c(0,-2),GEffect = c(0,-.7))
{

Ksub = rnorm(Nsub,muK,sdK)
Zsub = rnorm(Nsub,muZ,sdZ)

IntGsub = rnorm(Nsub,muIntG,sdIntG)
SlpGsub = rnorm(Nsub,muSlpG,sdSlpG)

Gsub = outer(SlpGsub,prch,'*') + IntGsub

change.v=inverse.rle(list(
lengths=c(plogis(prch)*Ntrl,plogis(-prch)*Ntrl),
values=rep(c(1,0),each=length(prch))
))


prch.v=inverse.rle(list(
lengths=c(plogis(prch)*Ntrl,plogis(-prch)*Ntrl),
values=rep(plogis(prch),2)
))

SetSizes.v = rep(SetSizes,each=length(prch.v))
change.v = rep(change.v,length(SetSizes))
prch.v = rep(prch.v,length(SetSizes))

KCondition.v = rep(KEffect,each=length(prch.v))
GCondition.v = rep(GEffect,each=length(prch.v))
SetSizes.v = rep(SetSizes.v,length(KEffect))
change.v = rep(change.v,length(KEffect))
prch.v = rep(prch.v,length(KEffect))


Subject.v = rep(1:Nsub,each=length(prch.v))
KCondition.v = rep(KCondition.v,Nsub)
GCondition.v = rep(GCondition.v,Nsub)
SetSizes.v = rep(SetSizes.v,Nsub)
change.v = rep(change.v,Nsub)
prch.v = rep(prch.v,Nsub)

goodN = SetSizes>4

dat=data.frame(Subject.v,GCondition.v,KCondition.v,change.v,prch.v,SetSizes.v)

dat$K = rep(Ksub,each=Ntrl*length(SetSizes)*length(prch)*length(KEffect)) + dat$KCondition.v
dat$Z = plogis(rep(Zsub,each=Ntrl*length(SetSizes)*length(prch)*length(KEffect)))
dat$IntG =  rep(IntGsub,each=Ntrl*length(SetSizes)*length(prch)*length(KEffect))
dat$SlpG =  rep(SlpGsub,each=Ntrl*length(SetSizes)*length(prch)*length(KEffect))

dat$G=plogis(dat$IntG+qlogis(dat$prch.v)*dat$SlpG+dat$GCondition.v)
dat$Condition = as.integer(as.factor(dat$GCondition.v))


dat$d  = pmax(pmin(dat$K/dat$SetSizes.v,1),0)
dat$optGuessPashler = (1-dat$d)/(1/dat$prch.v-dat$d)
dat$Pashler.G = dat$Pashler.Gp = plogis(qlogis(dat$optGuessPashler) + dat$GCondition.v)
dat$Pashler.Gp[dat$d==1]=0

dat$PH.Cowan = (1-dat$Z)*dat$G + pmax(pmin(dat$K/dat$SetSizes.v,1),0)*dat$Z + dat$Z*(1-pmax(pmin(dat$K/dat$SetSizes.v,1),0))*dat$G
dat$PH.Pashler = (1-dat$Z)*dat$Pashler.G + pmax(pmin(dat$K/dat$SetSizes.v,1),0)*dat$Z + dat$Z*(1-pmax(pmin(dat$K/dat$SetSizes.v,1),0))*dat$Pashler.Gp

dat$PF.Pashler = (1-dat$Z)*dat$Pashler.G + dat$Z*dat$Pashler.Gp
dat$PF.Cowan = (1-dat$Z)*dat$G + dat$Z*(1-pmax(pmin(dat$K/dat$SetSizes.v,1),0))*dat$G

dat$resp.Cowan = rbinom(dat$change,1,dat$PH.Cowan*dat$change.v + dat$PF.Cowan*(1-dat$change.v))
dat$resp.Pashler = rbinom(dat$change,1,dat$PH.Pashler*dat$change.v + dat$PF.Pashler*(1-dat$change.v))

# What's the estimate of the effect?

CowanEst = tapply(dat$resp.Cowan,list(dat$change,dat$SetSizes.v,dat$Subject.v,dat$Condition),mean)
CowanEst = (CowanEst[2,,,]-CowanEst[1,,,])*SetSizes
CowanEst2 = apply(CowanEst[goodN,,],c(2,3),mean)
diffCowanEst = mean(CowanEst2[,2]-CowanEst2[,1])

CowanEst.nocollapse = tapply(dat$resp.Cowan,list(dat$change,dat$SetSizes.v,dat$Subject.v,dat$prch.v,dat$Condition),mean)
CowanEst.nocollapse = (CowanEst.nocollapse[2,,,,]-CowanEst.nocollapse[1,,,,])*SetSizes
CowanEst2.nocollapse = apply(CowanEst.nocollapse[goodN,,,],c(2,3,4),mean)
diffCowanEst.nocollapse = mean(CowanEst2.nocollapse[,,2]-CowanEst2.nocollapse[,,1])

PashlerEst = tapply(dat$resp.Pashler,list(dat$change,dat$SetSizes.v,dat$Subject.v,dat$Condition),mean)
PashlerEst = (PashlerEst[2,,,]-PashlerEst[1,,,])/(1-PashlerEst[1,,,])*SetSizes
PashlerEst2 = apply(PashlerEst[goodN,,],c(2,3),mean)
diffPashlerEst = mean(PashlerEst2[,2]-PashlerEst2[,1])

PashlerEst.nocollapse = tapply(dat$resp.Pashler,list(dat$change,dat$SetSizes.v,dat$Subject.v,dat$prch.v,dat$Condition),mean)
PashlerEst.nocollapse = (PashlerEst.nocollapse[2,,,,]-PashlerEst.nocollapse[1,,,,])/(1-PashlerEst.nocollapse[1,,,,])*SetSizes
PashlerEst2.nocollapse = apply(PashlerEst.nocollapse[goodN,,,],c(2,3,4),mean)
diffPashlerEst.nocollapse = mean(PashlerEst2.nocollapse[,,2]-PashlerEst2.nocollapse[,,1])

x=table(dat$resp.Pashler,dat$change,dat$SetSizes.v,dat$Subject.v,dat$prch.v,dat$Condition)+1
PashlerEst.nocollapse = x[2,,,,,]/(x[2,,,,,]+x[1,,,,,])
PashlerEst.nocollapse = (PashlerEst.nocollapse[2,,,,]-PashlerEst.nocollapse[1,,,,])/(1-PashlerEst.nocollapse[1,,,,])*SetSizes
PashlerEst2.nocollapse = apply(PashlerEst.nocollapse[goodN,,,],c(2,3,4),mean)
diffPashlerEst.nocollapse = mean(PashlerEst2.nocollapse[,,2]-PashlerEst2.nocollapse[,,1])

# What's average capacity?

CowanEst = tapply(dat$resp.Cowan,list(dat$change,dat$SetSizes.v,dat$Subject.v),mean)
CowanEst = (CowanEst[2,,]-CowanEst[1,,])*SetSizes
CowanEst2 = apply(CowanEst[goodN,],2,mean)


CowanEst.nocollapse = tapply(dat$resp.Cowan,list(dat$change,dat$SetSizes.v,dat$Subject.v,dat$prch.v),mean)
CowanEst.nocollapse = (CowanEst.nocollapse[2,,,]-CowanEst.nocollapse[1,,,])*SetSizes
CowanEst2.nocollapse = apply(CowanEst.nocollapse[goodN,,],2,mean)


PashlerEst = tapply(dat$resp.Pashler,list(dat$change,dat$SetSizes.v,dat$Subject.v),mean)
PashlerEst = (PashlerEst[2,,]-PashlerEst[1,,])/(1-PashlerEst[1,,])*SetSizes
PashlerEst2 = apply(PashlerEst[goodN,],2,mean)

#PashlerEst.nocollapse = tapply(dat$resp.Pashler,list(dat$change,dat$SetSizes.v,dat$Subject.v,dat$prch.v),mean)
#PashlerEst.nocollapse = (PashlerEst.nocollapse[2,,,]-PashlerEst.nocollapse[1,,,])/(1-PashlerEst.nocollapse[1,,,])*SetSizes
#PashlerEst2.nocollapse = apply(PashlerEst.nocollapse[goodN,,],2,mean)

x=table(dat$resp.Pashler,dat$change,dat$SetSizes.v,dat$Subject.v,dat$prch.v)+1
PashlerEst.nocollapse = x[2,,,,]/(x[2,,,,]+x[1,,,,])
PashlerEst.nocollapse = (PashlerEst.nocollapse[2,,,]-PashlerEst.nocollapse[1,,,])/(1-PashlerEst.nocollapse[1,,,])*SetSizes
PashlerEst2.nocollapse = apply(PashlerEst.nocollapse[goodN,,],c(2),mean)

trueAvgK=rowMeans(outer(Ksub,KEffect,"+"))

trueDiff=diff(rev(KEffect))

list(Ksub=Ksub,data = dat,AvgKPashler = PashlerEst2, AvgKCowan = CowanEst2, trueAvgK = trueAvgK, dEstPashler=diffPashlerEst, dEstCowan = diffCowanEst, trued = trueDiff, dEstPashler.nc=diffPashlerEst.nocollapse, dEstCowan.nc = diffCowanEst.nocollapse,AvgKPashler.nc = PashlerEst2.nocollapse, AvgKCowan.nc = CowanEst2.nocollapse)
}

