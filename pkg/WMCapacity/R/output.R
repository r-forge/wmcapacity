writeOutputFiles=function(myPack,directory=NULL,outputSettings,storePred=TRUE){
  goodIters = (as.numeric(myPack@settings@MCMCSetup$burnin)+1):as.numeric(myPack@settings@EffectiveIters)
  if(is.null(directory)) directory = getwd()
  capMod=ifelse(myPack@settings@Ktype==0,"Cowan","Pashler")
  useCovMod = ifelse(myPack@settings@useCov,"Yes","No")
  time = gsub('\\s','-',myPack@settings@time,perl=T)
  time = gsub(':','.',time,perl=T)
  name = gsub('[\\s:\\\\]','',myPack@settings@analysisname,perl=T)

  if(as.integer(outputSettings$Robj)) save(myPack,file=paste(directory,"/",name,"-",time,"-fullAnalysis.RData",sep=""),compress=TRUE) 


  ## Model stuff
  onK = paste("K        = ",paste(c("muK",as.character(myPack@settings@SelEffs2[as.logical(myPack@settings@SelEffs2[,5]),4])),collapse=" + "),collapse="")
  onZ = paste("logit(Z) = ",paste(c("muZ",as.character(myPack@settings@SelEffs2[as.logical(myPack@settings@SelEffs2[,6]),4])),collapse=" + "),collapse="")
  onG = paste("logit(G) = ",paste(c("muG",as.character(myPack@settings@SelEffs2[as.logical(myPack@settings@SelEffs2[,7]),4])),collapse=" + "),collapse="")

  columns = paste(paste(myPack@settings@SelCols$selectedcols[,1],myPack@settings@SelCols$selectedcols[,2],sep=": "),collapse="\n")

  covnames=list()
  if(myPack@settings@useCov){
    covs = 1:myPack@settings@nCovMat

    for(i in 1:myPack@settings@nCovMat){
      x = myPack@settings@myCovList[lapply(myPack@settings@CovSetup,function(v) v[[2]])[[i]],c(1,3)]
      covnames[[i]]=paste(x[,1],x[,2],sep=" on ")
      covs[i]=paste(covnames[[i]],collapse=", ")
    }
    covs=paste("  Covariance Matrices:\n",paste(covs,collapse="\n"))
    wishartdf = paste("Wishart df     : ",myPack@settings@PriorSetup$WishartDF,"\n",sep="")

  }else{
    covs=""
    wishartdf = ""
  }

  if(myPack@settings@MCMCSetup$useMH=="1"){
  	MCMCtext = paste(
    		   "\nMCMC Settings\n-------------------\n",
    		   "Type           : Random Walk Metropolis-Hastings\n",
		   "Iterations     : ",myPack@settings@MCMCSetup$nIter,"\n",
    		   "Thin           : ",myPack@settings@MCMCSetup$MHthin,"\n",
    		   "Effective Iters: ",myPack@settings@EffectiveIters,"\n",
    		   "Scale          : ",myPack@settings@MCMCSetup$MHscale,"\n\n",
    sep="")

  }else{
	MCMCtext = paste(
    		   "\nMCMC Settings\n-------------------\n",
    		   "Type           : Hybrid\n",
		   "Iterations     : ",myPack@settings@MCMCSetup$nIter,"\n",
    		   "Burnin         : ",myPack@settings@MCMCSetup$burnin,"\n",
    		   "Epsilon        : (",myPack@settings@MCMCSetup$epsLow,", ",myPack@settings@MCMCSetup$epsUpp,")\n",
    		   "Leapfrog steps : ",myPack@settings@MCMCSetup$leapfrog,"\n\n",
    sep="")
  }

  ## Write info
  outputInfo = paste(
    "WOMMBAT Analysis\n",
    "--------------------\n",
    "Analysis name: ",myPack@settings@analysisname,"\n",
    "Analysis time: ",myPack@settings@time,"\n",
    "Filename     : ",myPack@settings@filename,"\n",
    "Model Type   : ",capMod,"\n",
    "Covariances? : ",useCovMod,"\n",
    "\nPrior Specification\n-------------------\n",
    "Inverse Gamma  : (a=",myPack@settings@PriorSetup$IGa0,", b=",myPack@settings@PriorSetup$IGb0,")\n",
    "muK            : Normal(mu=",myPack@settings@PriorSetup$meanMuK,", sigma=",myPack@settings@PriorSetup$sdMuK,")\n",
    "muZ            : Normal(mu=",myPack@settings@PriorSetup$meanMuA,", sigma=",myPack@settings@PriorSetup$sdMuA,")\n",
    "muG            : Normal(mu=",myPack@settings@PriorSetup$meanMuG,", sigma=",myPack@settings@PriorSetup$sdMuG,")\n",
    wishartdf, MCMCtext,

    "\nModel\n-------------------\n",
    columns,"\n\n",
    onK,"\n",onZ,"\n",onG,"\n",
    covs,"\n\n",


    "DIC            : ",round(myPack@output@DIC[1],1)," (Effective parameters: ",round(myPack@output@DIC[2],1),")\n",
    
    sep="")
  cat(outputInfo,file=paste(directory,"/",name,"-",time,"-Info.txt",sep=""))
  
  ## Write data
  if(as.integer(outputSettings$data)) write.csv(myPack@settings@newDat,file=paste(directory,"/",name,"-",time,"-data.csv",sep=""))


  ## Write Effect parameters
  if(as.integer(outputSettings$ChnEff)) write.csv(myPack@output@Effchains,file=paste(directory,"/",name,"-",time,"-EffectChains.csv",sep=""))
  if(as.integer(outputSettings$PmnEff)) write.csv(myPack@output@par,file=paste(directory,"/",name,"-",time,"-EffectPostMeans.csv",sep=""))
  if(as.integer(outputSettings$QntEff)){
    quants = t(apply(myPack@output@Effchains[goodIters,],2,quantile,p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975)))
    sds =  apply(myPack@output@Effchains[goodIters,],2,sd)
    write.csv(data.frame(quants,sds),file=paste(directory,"/",name,"-",time,"-EffectPostQuantiles.csv",sep=""))
  }
  ## Write predicted probabilities
	if(storePred)
	{
		write.csv(myPack@output@predVals[,1,],file=paste(directory,"/",name,"-",time,"-PredProbChainsHits.csv",sep=""))
		write.csv(apply(myPack@output@predVals[,1,goodIters],1,mean),file=paste(directory,"/",name,"-",time,"-PredProbMeansHits.csv",sep=""))
		write.csv(t(apply(myPack@output@predVals[,1,goodIters], 1, quantile, p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975))), file=paste(directory, "/", name, "-", time, "-PredProbQuantilesHits.csv", sep=""))

		write.csv(myPack@output@predVals[,2,],file=paste(directory,"/",name,"-",time,"-PredProbChainsFA.csv",sep=""))
		write.csv(apply(myPack@output@predVals[,2,goodIters],1,mean),file=paste(directory,"/",name,"-",time,"-PredProbMeansFA.csv",sep=""))
		write.csv(t(apply(myPack@output@predVals[,2,goodIters],1 ,quantile, p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975))), file=paste(directory, "/", name, "-" ,time, "-PredProbQuantilesFA.csv", sep=""))
	}

  # Write PDFs
  if(as.integer(outputSettings$MCMCPDF))
	{
		pdf(file=paste(directory,"/",name,"-",time,"-chains.pdf",sep=""),width=11,height=8.5,ver="1.4")
		plot(myPack)
		dev.off()
	}
  if(as.integer(outputSettings$ACFPDF))
	{
		pdf(file=paste(directory,"/",name,"-",time,"-autocorr.pdf",sep=""),width=11,height=8.5,ver="1.4")
		for(i in 1:length(myPack@output@par[,1])){
			acf(myPack@output@Effchains[,i],main=paste("Autocorrelation: ",colnames(myPack@output@Effchains)[i]))
		}
		dev.off()
	}

  # Write covariance stuff
  if(myPack@settings@useCov){
    covcols=corcols=NULL
    covcolnames=corcolnames=NULL
    meancols=NULL
    meannames=NULL
    
    
		for(i in 1:myPack@settings@nCovMat)
		{
			covcols=cbind(covcols,myPack@output@Covchains[[i]][rep(lower.tri(myPack@output@Covchains[[i]][,,1],dia=T),as.integer(myPack@settings@EffectiveIters))])
			nm=outer(covnames[[i]],covnames[[i]],paste,sep=',')
			covcolnames = c(covcolnames,nm[lower.tri(nm,dia=TRUE)])
			corcols=cbind(corcols,myPack@output@Corchains[[i]][rep(lower.tri(myPack@output@Covchains[[i]][,,1],dia=F),as.integer(myPack@settings@EffectiveIters))])
			nm=outer(covnames[[i]],covnames[[i]],paste,sep=',')
			corcolnames = c(corcolnames,nm[lower.tri(nm,dia=FALSE)])
			meannames=c(meannames,covnames[[i]])
			meancols=rbind(meancols,myPack@output@Meanchains[((1:myPack@settings@sizeCovMat[i])-1)*myPack@settings@nCovMat+i,])
			cat("Covariance Matrix ",i,"\n------\n",file=paste(directory,"/",name,"-",time,"-CovLevels.txt",sep=""),append=TRUE)
			cat(paste(apply(myPack@settings@namedCovLevels[[i]],2,paste,collapse=" and "),collapse="\n"),"\n\n",append=TRUE,file=paste(directory,"/",name,"-",time,"-CovLevels.txt",sep=""))
		}
		dim(covcols) = c(length(covcolnames),as.integer(myPack@settings@EffectiveIters))
		dim(corcols) = c(length(corcolnames),as.integer(myPack@settings@EffectiveIters))
		covcols=t(covcols)
		corcols=t(corcols)
		meancols=t(meancols)
		colnames(covcols)=covcolnames
		colnames(corcols)=corcolnames
		colnames(meancols)=meannames

		if(as.integer(outputSettings$ChnCov)) write.csv(covcols,file=paste(directory,"/",name,"-",time,"-CovChains.csv",sep=""))
		if(as.integer(outputSettings$QntCov)) write.csv(apply(covcols[goodIters,], 2, quantile,p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975)), file=paste(directory, "/",name,"-", time, "-CovQuantiles.csv", sep=""))
		if(as.integer(outputSettings$PmnCov)) write.csv(apply(covcols[goodIters,],2,mean),file=paste(directory,"/",name,"-",time,"-CovPostMeans.csv",sep=""))
	
		if(as.integer(outputSettings$ChnCor)) write.csv(corcols,file=paste(directory,"/",name,"-",time,"-CorChains.csv",sep=""))
		if(as.integer(outputSettings$QntCor)) write.csv(apply(as.matrix(corcols[goodIters,]),2,quantile,p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975)), file=paste(directory, "/", name, "-", time, "-CorQuantiles.csv", sep=""))
		if(as.integer(outputSettings$PmnCor)) write.csv(apply(as.matrix(corcols[goodIters,]),2,mean),file=paste(directory,"/",name,"-",time,"-CorPostMeans.csv",sep=""))

		if(as.integer(outputSettings$ChnMeans)) write.csv(meancols,file=paste(directory,"/",name,"-",time,"-MeanChains.csv",sep=""))
		if(as.integer(outputSettings$QntMeans)) write.csv(apply(meancols[goodIters,], 2, quantile,p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975)), file=paste(directory, "/", name, "-", time, "-MeanQuantiles.csv", sep=""))
		if(as.integer(outputSettings$PmnMeans)) write.csv(apply(meancols[goodIters,],2,mean),file=paste(directory,"/",name,"-",time,"-MeanPostMeans.csv",sep=""))

	}

  
}

