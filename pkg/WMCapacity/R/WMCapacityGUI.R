
WMCapacityGUI <- function(data=NULL,filename=NULL,setup=NULL,skipInstr=FALSE,name="Analysis",interface=TRUE,doOutput=TRUE, storePred=FALSE){
  tclRequire("BWidget")
  noMoreTesting=0
  #WorkingMemInit()
  passedPackage = class(setup)=="WM2Package"
  pack = new("WM2Package")
 
  if(!(skipInstr | !is.null(setup) | !interface)){ 
    a=WorkingMemInitInstructions()
  }else{
    a=0
  }
  if(is.null(a)) {cat("Cancelled by user.\n");return(1)}
  
  # Load in data
  if(is.null(data) & is.null(setup)){
    if(!is.null(filename)){
      data=read.csv(filename)
    }else{
      filename <- tclvalue(tkgetOpenFile(
        filetypes = "{{CSV files} {.csv}} {{All files} *}"))
    	if (filename == "") stop("No file selected.");
    	data <- read.csv(filename)
    }
  }
     
  if(passedPackage){
    pack@settings = setup@settings
    rm(setup)
    pack@settings@time = date()
    pack@settings@filename = "Settings loaded from WM2Package object."
      
  }else{
    pack@settings=new("WM2Settings")
    
    pack@settings@data         = data
    pack@settings@filename     = as.character(filename)
    pack@settings@analysisname = as.character(name)
    pack@settings@time         = date()
    
	 
    pack@settings@SelCols  = WorkingMemSelectColumns(pack@settings@data)
      if(identical(pack@settings@SelCols,list(0)))  {cat("Cancelled by user.\n");return(1)}
	
    #pack@settings@lvlnames = getLevels(pack@settings@data,pack@settings@SelCols$selectedcols)
    
    pack@settings@newDat      = createDat(pack@settings@data,pack@settings@SelCols$response,pack@settings@SelCols$change,pack@settings@SelCols$setsize,pack@settings@SelCols$selectedcols)
    pack@settings@mods        = listModels(pack@settings@newDat,pack@settings@SelCols$selectedcols)
    pack@settings@effs        = niceListEffects(pack@settings@mods)
    
    
    pack@settings@SelEffs     = WorkingMemSelectEffects(pack@settings@effs)
      if(identical(pack@settings@SelEffs,list(0))) {cat("Cancelled by user.\n");return(1)}
    pack@settings@useA        = as.logical(as.numeric(pack@settings@SelEffs$useA))
    
    
    if(!pack@settings@useA) pack@settings@SelEffs[[3]][,6]=pack@settings@SelEffs[[3]][,6]*0
    
    pack@settings@Ktype       = ifelse(pack@settings@SelEffs$Ktype=="Pashler",1,0)
    
    pack@settings@intMods     = pack@settings@SelEffs[[3]][apply(as.matrix(pack@settings@SelEffs[[3]][,5:7])==TRUE,1,any),1:2]
    pack@settings@SelEffs2    = pack@settings@SelEffs[[3]][apply(as.matrix(pack@settings@SelEffs[[3]][,5:7])==TRUE,1,any),]
    pack@settings@newDat2Cat  = createModColsCat(pack@settings@newDat,pack@settings@mods,pack@settings@intMods,pack@settings@SelCols$selectedcols)
    pack@settings@newDat2Cont = createModColsCont(pack@settings@newDat,pack@settings@mods,pack@settings@intMods,pack@settings@SelCols$selectedcols)
    
    pack@settings@namedDat2   = createMeaningfulCols(pack@settings@newDat,pack@settings@mods,pack@settings@intMods,pack@settings@SelCols$selectedcols)#,pack@settings@lvlnames)
    
    pack@settings@Lvls        =  as.integer(pack@settings@SelEffs2[,3])
    			      	 #apply(pack@settings@newDat2Cat,2,function(v) length(unique(v)))[-(1:5)]
                                 #getNFactorLevels(pack@settings@mods,pack@settings@intMods)

    
    effects                   = pack@settings@SelEffs[[3]][apply(as.matrix(pack@settings@SelEffs[[3]][,5:7])==TRUE,1,any),5:7]
    effects                   = t(pack@settings@Lvls*effects)
    
    
    effects=as.integer(effects)
    dim(effects)=c(3,length(effects)/3)
    
    pack@settings@effects=effects
    
    #Check Eligibility for covariance setup
    parNums = table(pack@settings@effects[pack@settings@effects>1])
    
    
    pack@settings@useCov =  any(parNums>1)
    if(pack@settings@useCov){
      eligParNums=as.numeric(names(parNums[parNums>1]))
      covMods=pack@settings@SelEffs2[pack@settings@SelEffs2[,3]%in%eligParNums,]
      pars = rep(c("K","Z","G"),each=dim(covMods)[[1]])
      names = rep(covMods[,4],3)
      nPars = rep(covMods[,3],3)
      rowNum = rep((1:dim(pack@settings@SelEffs2)[1])[pack@settings@SelEffs2[,3]%in%eligParNums],3)
      myCovList=cbind(pars,nPars,as.character(names),rowNum)[as.logical(as.matrix(covMods[,5:7]))]
      dim(myCovList)=c(length(myCovList)/4,4)
      pack@settings@myCovList=data.frame(myCovList)
      pack@settings@CovSetup = WorkingMemCovarianceSetup(pack@settings@myCovList)
      if(identical(pack@settings@CovSetup,list(0))) {cat("Cancelled by user.\n");return(1)}
	  if(length(pack@settings@CovSetup)>0){
		ni=as.numeric(as.character(unlist(lapply(pack@settings@CovSetup, function(v) v[[1]]))))
		p=unlist(lapply(pack@settings@CovSetup,function(v) length(v[[2]])))
		minWishDF = max(c(0,p-ni))
	  }else{
		minWishDF = 0
	  }
	}else{
      pack@settings@CovSetup=list()
	  minWishDF=0
	}

	
    if(identical(pack@settings@CovSetup,list())) pack@settings@useCov=FALSE
    
    pack@settings@incCont=includesContinuous(pack@settings@SelCols,pack@settings@SelEffs[[3]],pack@settings@mods)[apply(as.matrix(pack@settings@SelEffs[[3]][,5:7])==TRUE,1,any)]
    
    pack@settings@PriorSetup = WorkingMemPriorSetup(useA=pack@settings@useA,minWishDF=minWishDF)	
	if(identical(pack@settings@PriorSetup,list(0))) {cat("Cancelled by user.\n");return(1)}
	if(!pack@settings@useA){
			pack@settings@PriorSetup$meanMuA = 3
			pack@settings@PriorSetup$sdMuA   = 0.05 
		}
  }  
  while(noMoreTesting==0)
    {
      
      if(interface){
        if(!identical(pack@settings@MCMCSetup,list())){
          pack@settings@MCMCSetup = WorkingMemMCMCSetup(optimMaxit=pack@settings@MCMCSetup$optimMaxIter,
            nIters=pack@settings@MCMCSetup$nIter,
            burnin=pack@settings@MCMCSetup$burnin,
            testrun=FALSE,
            hybridEpsilon=c(pack@settings@MCMCSetup$epsLow,pack@settings@MCMCSetup$epsUpp),
            hybridLFsteps=pack@settings@MCMCSetup$leapfrog,
            progress=pack@settings@MCMCSetup$progress,MetHastScale=pack@settings@MCMCSetup$MHscale,useMH = pack@settings@MCMCSetup$useMH,MetHastThin=pack@settings@MCMCSetup$MHthin)
        }else{
          pack@settings@MCMCSetup = WorkingMemMCMCSetup(optimMaxit=200,nIters=1000,
            burnin=200,testrun=FALSE,hybridEpsilon=c(.02,.035),
            hybridLFsteps=80,hybridMultipoint=FALSE,
            hybridMultipointSize=1,hybridWeight=FALSE,
            hybridMPWeight=TRUE,progress=10)
        }
      }
      if(identical(pack@settings@MCMCSetup,list(0))) {cat("Cancelled by user.\n");return(1)}
      
      metrop = as.integer(pack@settings@MCMCSetup$useMH)
      metropScale =as.numeric(pack@settings@MCMCSetup$MHscale)       
      metropThin =as.integer(pack@settings@MCMCSetup$MHthin)       

      if(!interface | as.integer(pack@settings@MCMCSetup$testRun)==0){
        noMoreTesting=1
      }
      
      
      
      if(interface & doOutput & noMoreTesting)
        {
            OutputSetup = WorkingMemOutputSetup()
            if(identical(OutputSetup,list(0))) {cat("Cancelled by user.\n");return(1)}
			doOutput=as.integer(OutputSetup$doOutput)
			storePred = as.integer(OutputSetup$Pred)
		}
		
      
      
      ## Create progress bar
      progress=as.integer(pack@settings@MCMCSetup$progress)
      if(progress){ pb = txtProgressBar(min = 0, max = as.integer(pack@settings@MCMCSetup$nIter), style = 3) }else{ pb=NULL }
          pbFun = function(samps){ if(progress) setTxtProgressBar(pb, samps)}
      
        startingvals = jitter((1:(sum(pack@settings@effects)+3))*0)
        startingvals[1] = as.numeric(pack@settings@PriorSetup$meanMuK)
        startingvals[2] = as.numeric(pack@settings@PriorSetup$meanMuA)
        startingvals[3] = as.numeric(pack@settings@PriorSetup$meanMuG)
      
      metropSD = startingvals*0 + metropScale

      optimIters=as.numeric(pack@settings@MCMCSetup$optimMaxIter)
	      if(optimIters>0){
		       cat("Using optim() to find starting values...\n")
		      flush.console()
		      optimOut = optim(startingvals,RlogPosterior,RgradLogPosterior,method="BFGS",control=list(maxit=optimIters),setup=pack,hessian=TRUE) 
	   	      cat("Optim Convergence code: ",optimOut$convergence,"\n")
		      startingvals=optimOut$par
				cat("Inverting Hessian...\n")
				pack@settings@MCMCweights = diag(solve(optimOut$hessian))
			   if(any(pack@settings@MCMCweights<=0)){
				cat("Negative values in MCMC weights! Using 1 for all weights.\n")
				pack@settings@MCMCweights = pack@settings@MCMCweights*0 + 1
			   }
				
			   if(metrop){
					metropSD = sqrt(pack@settings@MCMCweights)*metropScale
			   }else{
					metropSD = 0				
					pack@settings@MCMCweights = pack@settings@MCMCweights*0 + 1
					#pack@settings@MCMCweights = pack@settings@MCMCweights/exp(mean(log(pack@settings@MCMCweights)))
			   }
            }		   
      
      pack@output = new("WM2Output")
      
      cat("Starting MCMC...\n")
      
      if(metrop){
	pack@settings@EffectiveIters = floor(as.numeric(pack@settings@MCMCSetup$nIter)/as.numeric(pack@settings@MCMCSetup$MHthin))
      }else{
	pack@settings@EffectiveIters = as.numeric(pack@settings@MCMCSetup$nIter)
      }

      if(pack@settings@useCov){
        pack@settings@nCovMat = as.integer(length(pack@settings@CovSetup))
        covMatSettings = parseCovMatSettings(pack@settings@myCovList,pack@settings@CovSetup,pack@settings@incCont,pack@settings@effects)
        pack@settings@parStart    = covMatSettings$parStart
        pack@settings@obsCovMat   = covMatSettings$obs
        pack@settings@sizeCovMat  = covMatSettings$size
        pack@settings@covEffSlope = covMatSettings$effSlope
        pack@settings@inCovMat    = covMatSettings$inCovMat
        output = .Call("WM2_GibbsSampler", as.integer(pack@settings@MCMCSetup$nIter), startingvals, 
          as.integer(pack@settings@nCovMat),as.integer(pack@settings@obsCovMat),
          as.integer(pack@settings@sizeCovMat),as.integer(pack@settings@parStart),
          as.integer(pack@settings@covEffSlope),
          as.integer(pack@settings@newDat2Cat[,1]), as.integer(pack@settings@newDat2Cat[,2]), 
          as.integer(pack@settings@newDat2Cat[,3]), as.integer(pack@settings@newDat2Cat[,4]), 
          as.integer(as.character(pack@settings@newDat2Cat[,5])), 
          int.matrix(pack@settings@newDat2Cat[,-(1:5)]), as.matrix(pack@settings@newDat2Cont[,-(1:5)]), 
          as.integer(pack@settings@effects[1,]), as.integer(pack@settings@incCont), as.integer(pack@settings@inCovMat[1,]), 
          as.integer(pack@settings@effects[2,]), as.integer(pack@settings@incCont), as.integer(pack@settings@inCovMat[2,]), 
          as.integer(pack@settings@effects[3,]), as.integer(pack@settings@incCont), as.integer(pack@settings@inCovMat[3,]), 
          as.numeric(pack@settings@PriorSetup$IGa0), as.numeric(pack@settings@PriorSetup$IGb0), as.numeric(pack@settings@PriorSetup$meanMuK), 
          as.numeric(pack@settings@PriorSetup$sdMuK)^2, as.numeric(pack@settings@PriorSetup$meanMuA), as.numeric(pack@settings@PriorSetup$sdMuA)^2, 
          as.numeric(pack@settings@PriorSetup$meanMuG), as.numeric(pack@settings@PriorSetup$sdMuG)^2, 
          1, as.integer(pack@settings@Ktype), as.numeric(pack@settings@MCMCSetup$epsLow), 
          (as.numeric(pack@settings@MCMCSetup$epsUpp) - as.numeric(pack@settings@MCMCSetup$epsLow)), 
          as.integer(pack@settings@MCMCSetup$leapfrog), pack@settings@MCMCweights, as.numeric(pack@settings@PriorSetup$WishartDF),
          progress,pbFun,new.env(),as.integer(storePred),as.integer(metrop),metropSD,metropThin,package = "WMCapacity")
        
        pack@output@Meanchains = output[[3]]
        pack@output@Covchains = list()
        pack@output@Corchains = list()
        if(storePred) pack@output@predVals = output[[5]]
        cat("\nFormatting cov/cor matrices...\n");flush.console()
        for(i in 1:pack@settings@nCovMat)
          {
            pack@output@Covchains[[i]]  = apply(output[[4]][[i]],3,solve)
            dim(pack@output@Covchains[[i]]) = c(pack@settings@sizeCovMat[i],pack@settings@sizeCovMat[i],as.integer(pack@settings@EffectiveIters))
			pack@output@Corchains[[i]]  = apply(pack@output@Covchains[[i]],3,cov2cor)
			dim(pack@output@Corchains[[i]]) = c(pack@settings@sizeCovMat[i],pack@settings@sizeCovMat[i],as.integer(pack@settings@EffectiveIters))
		  }
        
      }else{
        output = .Call("WM2_GibbsSamplerNoCov", as.integer(pack@settings@MCMCSetup$nIter), startingvals, as.integer(pack@settings@newDat2Cat[,1]), 
          as.integer(pack@settings@newDat2Cat[,2]), 
          as.integer(pack@settings@newDat2Cat[,3]), as.integer(pack@settings@newDat2Cat[,4]), 
          as.integer(as.character(pack@settings@newDat2Cat[,5])), 
          int.matrix(pack@settings@newDat2Cat[,-(1:5)]), as.matrix(pack@settings@newDat2Cont[,-(1:5)]), 
          as.integer(pack@settings@effects[1,]), as.integer(pack@settings@incCont), as.integer(pack@settings@effects[1,]*0), 
          as.integer(pack@settings@effects[2,]), as.integer(pack@settings@incCont), as.integer(pack@settings@effects[2,]*0), 
          as.integer(pack@settings@effects[3,]), as.integer(pack@settings@incCont), as.integer(pack@settings@effects[3,]*0), 
          as.numeric(pack@settings@PriorSetup$IGa0), as.numeric(pack@settings@PriorSetup$IGb0), as.numeric(pack@settings@PriorSetup$meanMuK), 
          as.numeric(pack@settings@PriorSetup$sdMuK)^2, as.numeric(pack@settings@PriorSetup$meanMuA), as.numeric(pack@settings@PriorSetup$sdMuA)^2, 
          as.numeric(pack@settings@PriorSetup$meanMuG), as.numeric(pack@settings@PriorSetup$sdMuG)^2, 
          1, as.integer(pack@settings@Ktype), as.numeric(pack@settings@MCMCSetup$epsLow), 
          (as.numeric(pack@settings@MCMCSetup$epsUpp) - as.numeric(pack@settings@MCMCSetup$epsLow)), 
          as.integer(pack@settings@MCMCSetup$leapfrog), pack@settings@MCMCweights, 
          progress,pbFun,new.env(),as.integer(storePred),as.integer(metrop),metropSD,metropThin,package = "WMCapacity")
		if(storePred) pack@output@predVals = output[[3]]   
      }
      
      cat("\nMCMC acceptance rate: ",round(mean(diff(output[[2]])!=0),3),"\n");flush.console()
      
      pack@output@burnin = as.numeric(pack@settings@MCMCSetup$burnin)
      pack@output@par = niceParVec(rowMeans(output[[1]][,(pack@output@burnin+1):as.integer(pack@settings@EffectiveIters)]), pack@settings@newDat2Cat, pack@settings@newDat2Cont, pack@settings@namedDat2, pack@settings@effects, pack@settings@incCont, TRUE,	  apply(output[[1]][,(pack@output@burnin+1):as.integer(pack@settings@EffectiveIters)],1,sd))
      chainnames=paste(pack@output@par[,4],pack@output@par[,2],pack@output@par[,3],sep=" ")
      chainnames=paste(chainnames,pack@output@par[,1],sep=" on ")
      pack@output@Effchains=mcmc(t(output[[1]]))
      dimnames(pack@output@Effchains)[[2]] = chainnames
      
      if(noMoreTesting==0){
        plot(pack@output@Effchains)
      }
    }
  if(pack@settings@useCov) pack@settings@namedCovLevels = getCovLevels(pack@settings@CovSetup,chainnames,pack@settings@parStart)
  likePostMean = RlogLikelihood(pack@output@par[,5],pack)
  
  if(metrop){
	pack@output@likeChain = output[[2]][((1:as.integer(pack@settings@MCMCSetup$nIter))%%as.integer(pack@settings@MCMCSetup$MHthin))==0]
  }else{
	pack@output@likeChain = output[[2]]
  }
  pack@output@DIC = computeDIC(pack@output@likeChain[(pack@output@burnin+1):as.integer(pack@settings@EffectiveIters)],likePostMean)
  pack@output@useCov = pack@settings@useCov

  if(doOutput){
    cat("Writing output files...\n")
    flush.console()
    writeOutputFiles(pack,directory=getwd(),outputSettings=OutputSetup,storePred)
  }
  return(pack)
}

