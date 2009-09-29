

getCovLevels=function(CovSetup,names,startPars)
{
out=list()

for(i in 1:length(CovSetup))
  {
    out[[i]] = matrix("",ncol=as.integer(as.character(CovSetup[[i]][[1]])),nrow=length(CovSetup[[i]][[2]]))
    for(j in 1:dim(out[[i]])[1])
      {
        out[[i]][j,]=names[startPars[i,j]+1:dim(out[[i]])[2]]
      }
  }
return(out)
}


RlogPosterior = function(x,setup) {
.Call("RLogPosteriorNoCov",x,as.integer(setup@settings@newDat2Cat[,1]),
as.integer(setup@settings@newDat2Cat[,2]),
as.integer(setup@settings@newDat2Cat[,3]),
as.integer(setup@settings@newDat2Cat[,4]),
as.integer(as.character(setup@settings@newDat2Cat[,5])),
int.matrix(setup@settings@newDat2Cat[,-(1:5)]),
as.matrix(setup@settings@newDat2Cont[,-(1:5)]),
as.integer(setup@settings@effects[1,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@effects[1,]*0),
as.integer(setup@settings@effects[2,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@effects[2,]*0),
as.integer(setup@settings@effects[3,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@effects[3,]*0),
as.numeric(setup@settings@PriorSetup$IGa0),
as.numeric(setup@settings@PriorSetup$IGb0),
as.numeric(setup@settings@PriorSetup$meanMuK),
as.numeric(setup@settings@PriorSetup$sdMuK)^2,
as.numeric(setup@settings@PriorSetup$meanMuA),
as.numeric(setup@settings@PriorSetup$sdMuA)^2,
as.numeric(setup@settings@PriorSetup$meanMuG),
as.numeric(setup@settings@PriorSetup$sdMuG)^2, 1,
as.integer(setup@settings@Ktype),package="WMCapacity") }

RgradLogPosterior = function(x,setup){
	   .Call("RgradLogPosteriorNoCov",x,
	   as.integer(setup@settings@newDat2Cat[,1]),
	   as.integer(setup@settings@newDat2Cat[,2]),
	   as.integer(setup@settings@newDat2Cat[,3]),
	   as.integer(setup@settings@newDat2Cat[,4]),
	   as.integer(as.character(setup@settings@newDat2Cat[,5])),
	   int.matrix(setup@settings@newDat2Cat[,-(1:5)]),
	   as.matrix(setup@settings@newDat2Cont[,-(1:5)]),
	   as.integer(setup@settings@effects[1,]),
	   as.integer(setup@settings@incCont),
	   as.integer(setup@settings@effects[1,]*0),
	   as.integer(setup@settings@effects[2,]),
	   as.integer(setup@settings@incCont),
	   as.integer(setup@settings@effects[2,]*0),
	   as.integer(setup@settings@effects[3,]),
	   as.integer(setup@settings@incCont),
	   as.integer(setup@settings@effects[3,]*0),
	   as.numeric(setup@settings@PriorSetup$IGa0),
	   as.numeric(setup@settings@PriorSetup$IGb0),
	   as.numeric(setup@settings@PriorSetup$meanMuK),
	   as.numeric(setup@settings@PriorSetup$sdMuK)^2,
	   as.numeric(setup@settings@PriorSetup$meanMuA),
	   as.numeric(setup@settings@PriorSetup$sdMuA)^2,
	   as.numeric(setup@settings@PriorSetup$meanMuG),
	   as.numeric(setup@settings@PriorSetup$sdMuG)^2, 1,
	   as.integer(setup@settings@Ktype),package="WMCapacity") }


RlogPosteriorWithCov = function(x,setup,precList,means) {
.Call("RLogPosteriorWithCov",x,
as.integer(setup@settings@newDat2Cat[,1]),
as.integer(setup@settings@newDat2Cat[,2]),
as.integer(setup@settings@newDat2Cat[,3]),
as.integer(setup@settings@newDat2Cat[,4]),
as.integer(as.character(setup@settings@newDat2Cat[,5])),
int.matrix(setup@settings@newDat2Cat[,-(1:5)]),
as.matrix(setup@settings@newDat2Cont[,-(1:5)]),
as.integer(setup@settings@effects[1,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@inCovMat[1,]),
as.integer(setup@settings@effects[2,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@inCovMat[2,]),
as.integer(setup@settings@effects[3,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@inCovMat[3,]),
as.numeric(setup@settings@PriorSetup$IGa0),
as.numeric(setup@settings@PriorSetup$IGb0),
as.numeric(setup@settings@PriorSetup$meanMuK),
as.numeric(setup@settings@PriorSetup$sdMuK)^2,
as.numeric(setup@settings@PriorSetup$meanMuA),
as.numeric(setup@settings@PriorSetup$sdMuA)^2,
as.numeric(setup@settings@PriorSetup$meanMuG),
as.numeric(setup@settings@PriorSetup$sdMuG)^2, 1,
as.integer(setup@settings@Ktype), precList, means,
as.integer(setup@settings@obsCovMat),
as.integer(setup@settings@sizeCovMat),
as.integer(setup@settings@parStart),
as.integer(setup@settings@covEffSlope), package="WMCapacity") }

RgradLogPosteriorWithCov = function(x,setup,precList,means) {
.Call("RgradLogPosteriorWithCov",x,
as.integer(setup@settings@newDat2Cat[,1]),
as.integer(setup@settings@newDat2Cat[,2]),
as.integer(setup@settings@newDat2Cat[,3]),
as.integer(setup@settings@newDat2Cat[,4]),
as.integer(as.character(setup@settings@newDat2Cat[,5])),
int.matrix(setup@settings@newDat2Cat[,-(1:5)]),
as.matrix(setup@settings@newDat2Cont[,-(1:5)]),
as.integer(setup@settings@effects[1,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@inCovMat[1,]),
as.integer(setup@settings@effects[2,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@inCovMat[2,]),
as.integer(setup@settings@effects[3,]),
as.integer(setup@settings@incCont),
as.integer(setup@settings@inCovMat[3,]),
as.numeric(setup@settings@PriorSetup$IGa0),
as.numeric(setup@settings@PriorSetup$IGb0),
as.numeric(setup@settings@PriorSetup$meanMuK),
as.numeric(setup@settings@PriorSetup$sdMuK)^2,
as.numeric(setup@settings@PriorSetup$meanMuA),
as.numeric(setup@settings@PriorSetup$sdMuA)^2,
as.numeric(setup@settings@PriorSetup$meanMuG),
as.numeric(setup@settings@PriorSetup$sdMuG)^2, 1,
as.integer(setup@settings@Ktype), precList, means,
as.integer(setup@settings@obsCovMat),
as.integer(setup@settings@sizeCovMat),
as.integer(setup@settings@parStart),
as.integer(setup@settings@covEffSlope),package="WMCapacity") 
}



RlogLikelihood = function(x,setup){ 
.Call("RLogLikelihood",x, as.integer(setup@settings@newDat2Cat[,1]),
      as.integer(setup@settings@newDat2Cat[,2]),
      as.integer(setup@settings@newDat2Cat[,3]),
      as.integer(setup@settings@newDat2Cat[,4]),
      as.integer(as.character(setup@settings@newDat2Cat[,5])),
      int.matrix(setup@settings@newDat2Cat[,-(1:5)]),
      as.matrix(setup@settings@newDat2Cont[,-(1:5)]),
      as.integer(setup@settings@effects[1,]),
      as.integer(setup@settings@effects[2,]),
      as.integer(setup@settings@effects[3,]), 1,
      as.integer(setup@settings@Ktype),package="WMCapacity") }

RPredVals = function(x,setup){ 
.Call("RPredictedProbabilities", x,
      as.integer(setup@settings@newDat2Cat[,1]),
      as.integer(setup@settings@newDat2Cat[,2]),
      as.integer(setup@settings@newDat2Cat[,3]),
      as.integer(setup@settings@newDat2Cat[,4]),
      as.integer(as.character(setup@settings@newDat2Cat[,5])),
      int.matrix(setup@settings@newDat2Cat[,-(1:5)]),
      as.matrix(setup@settings@newDat2Cont[,-(1:5)]),
      as.integer(setup@settings@effects[1,]),
      as.integer(setup@settings@effects[2,]),
      as.integer(setup@settings@effects[3,]), 1,
      as.integer(setup@settings@Ktype),package="WMCapacity") }




parseCovMatSettings = function(myCovList,CovSetup,incCont,effects,useA=T)
{
  parnames=c("K","Z","G")

  n = length(CovSetup)
  obs=1:n*NA
  size=1:n*NA
  pstart=cumsum(c(0,effects))[1:length(effects)]
  inCovMat = effects*0
  
  dim(pstart)=dim(effects)
  for(i in 1:n){
    obs[i] = as.integer(as.character(CovSetup[[i]][[1]]))
    size[i] = length(CovSetup[[i]][[2]])
  }

  parStart = matrix(nrow=n,ncol=max(size))
  effSlope = parStart
  
  for(i in 1:n)
    for(j in 1:size[i])
    {
      par=match(myCovList[CovSetup[[i]][[2]][j],1],parnames)
      eff=myCovList[CovSetup[[i]][[2]][j],4]
      parStart[i,j] = pstart[par,eff]
      effSlope[i,j] = incCont[eff]
      inCovMat[par,eff] = 1
    }
if(useA){ parStart = parStart + 3 }else{ parStart = parStart + 2 }
list(obs=obs,size=size,parStart=parStart,effSlope=effSlope,inCovMat=inCovMat)
}

computeDIC=function(likeVec,likePostMean){
  Dbar = -2*mean(likeVec)
  Dthetabar = -2*likePostMean
  pD = Dbar - Dthetabar
  DIC = pD + Dbar
  x=array(c(DIC,pD))
  names(x)=c("DIC","pD")
  return(x)
}

int.matrix<-function(m){
  dims=dim(m)
  m2=as.integer(as.matrix(m))
  if(is.null(dims)){
    dim(m2)=c(length(m),1)
  }else{  
    dim(m2)=dims
  }
  return(m2)
}


niceParVec=function(par,newDat2Cat,newDat2Cont,namedDat2,effects,incCont,useA){
  
  newPars=data.frame(array(par,dim=c(length(par),5)))
  colnames(newPars)=c("Parameter","Effect","Level","Type","Estimate")
  newPars[,5]=par
  newPars[1,1:4]=c("k","grand mean","-","-")
  intcolnames1=colnames(newDat2Cat)[-(1:5)]
  intcolnames2=colnames(newDat2Cont)[-(1:5)]
  intcolnames=paste(intcolnames1,intcolnames2,sep=".x.")
  intcolnames[incCont==0]=intcolnames1[incCont==0]
  parnames=c("k","z","g")
  
  if(useA){
    newPars[2,1:4]=c("z","grand mean","-","-")
    newPars[3,1:4]=c("g","grand mean","-","-")
    counter=4
    for(i in 1:dim(effects)[2])
  	for(j in 1:3){
	      if(effects[j,i]>0){
		for(k in 1:effects[j,i]){
		      cellname=namedDat2[(k==newDat2Cat[,i+5]),i+5][1]
		      newPars[counter,1:4]=c(parnames[j],intcolnames[i],as.character(cellname),c("Random Effect","Slope")[incCont[i]+1])
		      counter=counter+1
		}
	      }
		      	      
	}
  }else{
    newPars[2,1:4]=c("g","grand mean","-","-")
    counter=3
       for(i in 1:dim(effects)[2])
  	for(j in 1:3){
	      if(effects[j,i]>0){
		for(k in 1:effects[j,i]){
		      cellname=namedDat2[(k==newDat2Cat[,i+5]),i+5][1]
		      newPars[counter,1:4]=c(parnames[j],intcolnames[i],as.character(cellname),c("Random Effect","Slope")[incCont[i]+1])
		      counter=counter+1
		}
	      }
		      	      
	}
  }
newPars
}

createDat <- function(data,respColumn,changeColumn,setSizeColumn,interestColumns)
{

	names = colnames(data)
	respColumnNum = match(respColumn,names)
	changeColumnNum = match(changeColumn,names)
	setSizeColumnNum = match(setSizeColumn,names)
	interestColumnsNum = match(interestColumns[,1],names)

	newData = data[,c(respColumnNum,changeColumnNum,setSizeColumnNum,interestColumnsNum)]

	maxLen=prod(unlist(lapply(apply(newData[,-(1:2)],2,unique),length)))
	if(maxLen<length(newData[,1])){

		a=as.vector(table(newData))
		dim(a)=c(4,length(a)/4)
		a=t(a[4:1,])	

		
		colnames(a)=c("Hits","Misses","FalseAlarms","CorrectRej")

		names=dimnames(table(newData))[-(1:2)]
		newData=data.frame(a,expand.grid(names))
		newData=newData[rowSums(newData[,1:4])!=0,]
	}else{
		a=as.integer(cbind(newData[,1] & newData[,2],
			!newData[,1] & newData[,2],
			newData[,1] & !newData[,2],
			!newData[,1] & !newData[,2]))	
		dim(a)=c(length(a)/4,4)
		colnames(a)=c("Hits","Misses","FalseAlarms","CorrectRej")
		newData=data.frame(a,newData[,-(1:2)])
		newData=newData[rowSums(newData[,1:4])!=0,]
	}

	for(i in 1:length(interestColumns[,1])){
		if(interestColumns[i,2]=="CATEG"){
			#newData[,i+5] = as.integer(newData[,i+5])			
		}else if(interestColumns[i,2]=="CONTIN"){
			newData[,i+5] = as.numeric(as.character(newData[,i+5]))
		}
	}

	newData
}

factMax = function(v){
	if(is.integer(v)){
		max(v)
	}else{
		1
	}
}

listModels <- function(newDat,interestColumns){
	nFactors=dim(newDat)[2]-5
	names=colnames(newDat)[-(1:5)]
	levels=1:nFactors
	for(i in 1:nFactors){
	  if(interestColumns[i,2]=="CONTIN") levels[i]=1
	  if(interestColumns[i,2]=="CATEG") levels[i]=length(unique(newDat[,5+i])) 
	#levels[i] = factMax(newDat[,5+i])
	}
	modsInt=list()

	for(i in 1:nFactors){
		modsInt[[i]]=combinations(nFactors,i)
	}

	list(modsInt,names,levels)
}

niceListEffects = function(mods){
	ret=NULL
	for(i in 1:length(mods[[1]])){
		for(j in 1:dim(mods[[1]][[i]])[1]){
			cols=mods[[1]][[i]][j,]
			if(length(cols)==1){
				ret=rbind(ret,c(i,j,mods[[3]][mods[[1]][[i]][j]],mods[[2]][mods[[1]][[i]][j]]))
			}else{
				ret=rbind(ret,c(i,j,prod(mods[[3]][mods[[1]][[i]][j,]]),paste(mods[[2]][mods[[1]][[i]][j,]],collapse=" by ")))
			}

		}
	}
	ret=data.frame(ret)
	ret[,1]=as.numeric(as.character(ret[,1]))
	ret[,2]=as.numeric(as.character(ret[,2]))
	ret[,3]=as.numeric(as.character(ret[,3]))
	ret=data.frame(ret,ret[,1]*0,ret[,1]*0,ret[,1]*0)
	colnames(ret)=c("way","effnum","nlevels","name","K","Z","G")
	ret
}

CatOrCont0=function(name,SelCols)
SelCols[match(name,SelCols[,1]),2]

CatOrCont=Vectorize(CatOrCont0,"name")

createModColsCat <- function(newDat,allMods,intMods,SelCols){

	if(is.null(dim(intMods))){
	  nNewCols = length(intMods)
	}else{
	  nNewCols = dim(intMods)[1]
	}
	#nNewCols = dim(intMods)[1]
	newDat2  = newDat[,1:5]

	names=c("Hits","Misses","FalseAlarms","CorrectRej","setSize")

	for(i in 1:nNewCols){
      		myCols=allMods[[1]][[intMods[i,1]]][intMods[i,2],]
      		nLvls=allMods[[3]][myCols]
      		cc=CatOrCont(colnames(newDat)[myCols+5],SelCols)
      		if(length(myCols)>1){
			if(sum(cc=="CATEG")==0){
				newDat2=data.frame(newDat2,newDat[,1]*0+1)
	      			names=c(names,paste("column",i,"nocat",sep=""))
			}else if(sum(cc=="CATEG")==1){
				newDat2=data.frame(newDat2,as.integer(as.factor(newDat[,which(cc=="CATEG")+5])))
	      			names=c(names,paste(colnames(newDat)[(myCols+5)[cc=="CATEG"]],collapse=".x."))				      				      		
			}else{
				newDat2=data.frame(newDat2,as.integer(as.factor(apply(cbind(newDat[,(myCols+5)[cc=="CATEG"]]),1,paste,collapse="x"))))
	      			names=c(names,paste(colnames(newDat)[(myCols+5)[cc=="CATEG"]],collapse=".x."))				      			
	      		}
		}else{
			if(any(cc=="CATEG")){
				newDat2=data.frame(newDat2,as.integer(as.factor(newDat[,myCols+5])))
      				names=c(names,colnames(newDat)[myCols+5])
      			}else{
				newDat2=data.frame(newDat2,newDat[,1]*0+1)
      				names=c(names,paste("column",i,"nocat",sep=""))      			
      			}
      		}
	}
colnames(newDat2)=names
return(newDat2)
}

createModColsCont <- function(newDat,allMods,intMods,SelCols){

	if(is.null(dim(intMods))){
	  nNewCols = length(intMods)
	}else{
	  nNewCols = dim(intMods)[1]
	}
	#nNewCols = dim(intMods)[1]
	newDat2  = newDat[,1:5]

	names=c("Hits","Misses","FalseAlarms","CorrectRej","setSize")

	for(i in 1:nNewCols){
      		myCols=allMods[[1]][[intMods[i,1]]][intMods[i,2],]
      		nLvls=allMods[[3]][myCols]
      		cc=CatOrCont(colnames(newDat)[myCols+5],SelCols)
      		if(length(myCols)>1){
			if(any(cc=="CONTIN")){
				newDat2=data.frame(newDat2,apply(cbind(newDat[,(myCols+5)[cc=="CONTIN"]]),1,prod))
	      			names=c(names,paste(colnames(newDat)[(myCols+5)[cc=="CONTIN"]],collapse=".x."))
	      		}else{
				newDat2=data.frame(newDat2,newDat[,1]*0+1)
	      			names=c(names,paste("column",i,"nocont",sep=""))      			
	      		}
		}else{
			if(any(cc=="CONTIN")){
				newDat2=data.frame(newDat2,newDat[,myCols+5])
      				names=c(names,colnames(newDat)[myCols+5])
      			}else{
				newDat2=data.frame(newDat2,newDat[,1]*0+1)
      				names=c(names,paste("column",i,"nocont",sep=""))      			
      			}
      		}
	}
colnames(newDat2)=names
return(newDat2)
}


createMeaningfulCols <- function(newDat,allMods,intMods,SelCols)#,LevelNames)
{

	if(is.null(dim(intMods))){
	  nNewCols = length(intMods)
	}else{
	  nNewCols = dim(intMods)[1]
	}
	#nNewCols = dim(intMods)[1]
	#nNewCols = dim(newDat)[2]-5
	newDat2  = newDat[,1:5]
	namedCols=data.frame(newDat[,-(1:5)])

	#for(i in 1:nNewCols){
	#	if(SelCols[i,2]=="CATEG")
	#	{
	#		#namedCols[,i]=LevelNames[[i]][namedCols[,i]]
	#	}else{
	#		namedCols[,i]=rep("(CATEGORICAL)",length(newDat2[,1]))		
	#	}
	#}


	names=c("Hits","Misses","FalseAlarms","CorrectRej","setSize")

	for(i in 1:nNewCols){
      		myCols=allMods[[1]][[intMods[i,1]]][intMods[i,2],]
      		nLvls=allMods[[3]][myCols]
      		cc=CatOrCont(colnames(newDat)[myCols+5],SelCols)
      		if(length(myCols)>1){
				if(any(cc=="CATEG")){
					newDat2=data.frame(newDat2,apply(cbind(namedCols[,myCols[cc=="CATEG"]]),1,paste,collapse=".x."))
				}else{
					newDat2=data.frame(newDat2,rep("(CATEGORICAL)",length(newDat2[,1])))	      			
				}
				names=c(names,paste(colnames(newDat)[myCols+5],collapse=".x."))      			
			}else{
				if(any(cc=="CATEG")){
					newDat2=data.frame(newDat2,namedCols[,myCols])
      			}else{
					newDat2=data.frame(newDat2,rep("(CATEGORICAL)",length(newDat2[,1])))
      			}
      			names=c(names,colnames(newDat)[myCols+5]) 
      		}
	}
colnames(newDat2)=names
return(newDat2)
}


getLevels = function(data,SelCols)
{
	colnums=match(SelCols[,1],colnames(data))
	myLevels=list()
	for(i in 1:length(colnums))
	{
		if(SelCols[i,2]=="CATEG"){
			myLevels[[i]]=levels(as.factor(data[,colnums[i]]))
		}else{
			myLevels[[i]]=""
		}
	}
myLevels
}

getNFactorLevels <-function(allMods,intMods)
{
	nNewCols = dim(intMods)[1]

	FactorLevels=array(1:nNewCols*NA)
	names=NULL

	for(i in 1:nNewCols){
      		myMod=allMods[[1]][[intMods[i,1]]][intMods[i,2],]
      		if(length(myMod)>1){
			FactorLevels[i]=prod(allMods[[3]][myMod])
      			names=c(names,paste(allMods[[2]][myMod],collapse=".x."))
	      	}else{
			FactorLevels[i]=allMods[[3]][myMod]
      			names=c(names,allMods[[2]][myMod])
      		}
	}

	names(FactorLevels)=names
	FactorLevels
}


includesContinuous <- function(SelCols,SelEffs,mods){
  ret=1:dim(SelEffs)[1]
  for(i in 1:dim(SelEffs)[1]){
    ret[i]=any(CatOrCont(mods[[2]][mods[[1]][[SelEffs[i,1]]][SelEffs[i,2],]],SelCols$selectedcols)=="CONTIN")
  }
  ret
}
