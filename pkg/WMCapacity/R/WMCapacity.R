
printWM2Package=function(object){
capMod=ifelse(object@settings@Ktype==0,"Cowan","Pashler")
display=paste(
                        "WOMBBAT Analysis\n",
			"-------------------------------------\n",
                        "Analysis Name                  : ",object@settings@analysisname,"\n",
                        "Time and Date                  : ",object@settings@time,"\n",
			"Deviance Information Criterion : ",round(object@output@DIC[1],1)," (Effective parameters: ",round(object@output@DIC[2],1),")\n",
                        "Capacity Model:                : ",capMod,"\n",                       
                        "(view summary for more details)\n",
                        "\n\n",sep="")
cat(noquote(display))
invisible(object)
}


summary.WM2Package=function(object,...){
  capMod=ifelse(object@settings@Ktype==0,"Cowan","Pashler")
  useCovMod = ifelse(object@settings@useCov,"Yes","No")
  time = gsub('\\s','-',object@settings@time,perl=T)
  time = gsub(':','.',time,perl=T)
  name = gsub('[\\s:\\\\]','',object@settings@analysisname,perl=T)

  ## Model stuff
  onK = paste("K        = ",paste(c("muK",as.character(object@settings@SelEffs2[as.logical(object@settings@SelEffs2[,5]),4])),collapse=" + "),collapse="")
  onZ = paste("logit(Z) = ",paste(c("muZ",as.character(object@settings@SelEffs2[as.logical(object@settings@SelEffs2[,6]),4])),collapse=" + "),collapse="")
  onG = paste("logit(G) = ",paste(c("muG",as.character(object@settings@SelEffs2[as.logical(object@settings@SelEffs2[,7]),4])),collapse=" + "),collapse="")

  columns = paste(paste(object@settings@SelCols$selectedcols[,1],object@settings@SelCols$selectedcols[,2],sep=": "),collapse="\n")

  covnames=list()
  if(object@settings@useCov){
    covs = 1:object@settings@nCovMat

    for(i in 1:object@settings@nCovMat){
      x = object@settings@myCovList[lapply(object@settings@CovSetup,function(v) v[[2]])[[i]],c(1,3)]
      covnames[[i]]=paste(x[,1],x[,2],sep=" on ")
      covs[i]=paste(covnames[[i]],collapse=", ")
    }
    covs=paste("  Covariance Matrices:\n",paste(covs,collapse="\n"))
    wishartdf = paste("Wishart df     : ",object@settings@PriorSetup$WishartDF,"\n",sep="")

  }else{
    covs=""
    wishartdf = ""
  }

  if(object@settings@MCMCSetup$useMH=="1"){
  	MCMCtext = paste(
    		   "\nMCMC Settings\n-------------------\n",
    		   "Type           : Random Walk Metropolis-Hastings\n",
		   "Iterations     : ",object@settings@MCMCSetup$nIter,"\n",
    		   "Thin           : ",object@settings@MCMCSetup$MHthin,"\n",
    		   "Effective Iters: ",object@settings@EffectiveIters,"\n",
    		   "Scale          : ",object@settings@MCMCSetup$MHscale,"\n\n",
    sep="")

  }else{
	MCMCtext = paste(
    		   "\nMCMC Settings\n-------------------\n",
    		   "Type           : Hybrid\n",
		   "Iterations     : ",object@settings@MCMCSetup$nIter,"\n",
    		   "Burnin         : ",object@settings@MCMCSetup$burnin,"\n",
    		   "Epsilon        : (",object@settings@MCMCSetup$epsLow,", ",object@settings@MCMCSetup$epsUpp,")\n",
    		   "Leapfrog steps : ",object@settings@MCMCSetup$leapfrog,"\n\n",
    sep="")
  }

  ## Write info
  outputInfo = paste(
    "WOMMBAT Analysis\n",
    "--------------------\n",
    "Analysis name: ",object@settings@analysisname,"\n",
    "Analysis time: ",object@settings@time,"\n",
    "Filename     : ",object@settings@filename,"\n",
    "Model Type   : ",capMod,"\n",
    "Covariances? : ",useCovMod,"\n",
    "\nPrior Specification\n-------------------\n",
    "Inverse Gamma  : (a=",object@settings@PriorSetup$IGa0,", b=",object@settings@PriorSetup$IGb0,")\n",
    "muK            : Normal(mu=",object@settings@PriorSetup$meanMuK,", sigma=",object@settings@PriorSetup$sdMuK,")\n",
    "muZ            : Normal(mu=",object@settings@PriorSetup$meanMuA,", sigma=",object@settings@PriorSetup$sdMuA,")\n",
    "muG            : Normal(mu=",object@settings@PriorSetup$meanMuG,", sigma=",object@settings@PriorSetup$sdMuG,")\n",
    wishartdf, MCMCtext,

    "\nModel\n-------------------\n",
    columns,"\n\n",
    onK,"\n",onZ,"\n",onG,"\n",
    covs,"\n\n",


    "DIC            : ",round(object@output@DIC[1],1)," (Effective parameters: ",round(object@output@DIC[2],1),")\n",
    
    sep="")

cat(noquote(outputInfo))  
invisible(object)
}

plot.WM2Package=function(x,...)
{
  plot(x@output@Effchains,...)
}

#summary.WM2Package=function(x)
#{
#  cat("Posterior Means:\n")
#  print(x@output@par)
#  cat("\n\nDIC : ",round(x@output@DIC[1],1),"\n")
#}

setClass("WM2Settings", 
      representation(
	data = "data.frame",      # the data to be analyzed, in raw form
	filename = "character",	  # source of the data
	analysisname="character", # name of the analysis, for future reference 
	time="character",         # time and date the analysis was started
	SelCols="list",           # Result of column selection query
	lvlnames="list",          # names of the factor levels of the columns of interest
	newDat="data.frame",      # data, restricted to only columns of interest and collapsed
	mods="list",              # available model effects (before selection)
	effs="data.frame",        # a nice matrix of all possible model effects
	SelEffs="list",		  # result of the model building query
	useA="logical",           # use A (zone-out) parameter?
	Ktype="numeric",          # Which model on K to use (Cowan or Pashler)
	intMods="data.frame",     # The effects of interest 
	SelEffs2="data.frame",    # nice matrix giving the model requested
	newDat2Cat="data.frame",  # data including the categorical components
	newDat2Cont="data.frame", # data including the continuous components
	namedDat2="data.frame",   # data with nice names instead of integers for factor levels
	Lvls="integer",             # How many levels do the effects have?
	effects="matrix",         # matrix with factor levels - with the data, specifies the model
	useCov="logical",         # use covariance modeling?
	myCovList="data.frame",   # list of all available effects for modelling covariances
	CovSetup="list",          # the selected covariances to be modelled
	nCovMat="numeric",        # number of requested covariance matrices
	obsCovMat="numeric",      # the number of observations for each cov matrix
	sizeCovMat="numeric",     # the size of the covariance matrices
	parStart="matrix",        # where the corresponding elements for each cov matrix are in the parameter vector
	covEffSlope="matrix",     # is the effect in a cov matrix a slope?
        namedCovLevels="list",
        inCovMat="matrix",
	incCont="numeric",        # is the effect a slope?
	PriorSetup="list",        # result of prior selection query
	MCMCSetup="list",          # result of MCMC setup query
	EffectiveIters="numeric"  # number of iters after thinning
	),
contains="list")


setClass("WM2Output", 
      representation(
	DIC = "array",
	useCov="logical",
	Effchains="mcmc",
	Covchains="list",
	Corchains="list",
	Meanchains="matrix",
	likeChain="numeric",
	par="data.frame",
	burnin="numeric",
        predVals="array"                    
),
contains="list")


setClass("WM2Package",
      representation(
	settings = "list",
	output   = "list"
      ),
contains="list")


setMethod("show", "WM2Package", printWM2Package)
#setMethod("plot", signature(x = "WM2Package", y = "missing"), WM2Plot)



WMCapacityGUI <- function(data=NULL,filename=NULL,setup=NULL,skipInstr=FALSE,name="Analysis",interface=TRUE,doOutput=TRUE, storePred=FALSE){

  noMoreTesting=0
  WorkingMemInit()
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
    
    pack@settings@Lvls        =  apply(pack@settings@newDat2Cat[,-(1:5)],2,function(v) length(unique(v)))
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
            burnin=200,testrun=FALSE,hybridEpsilon=c(.01,.02),
            hybridLFsteps=60,hybridMultipoint=FALSE,
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
      		   if(metrop){
					if(any(diag(optimOut$hessian)<=0)) stop("Negative values in optim() Hessian diagonal!")
				metropSD = sqrt(1/abs(diag(optimOut$hessian)))*metropScale
			   }else{
			metropSD = 0
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
          as.integer(pack@settings@MCMCSetup$leapfrog), startingvals*0+1, as.numeric(pack@settings@PriorSetup$WishartDF),
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
          as.integer(pack@settings@MCMCSetup$leapfrog), startingvals*0+1, 
          progress,pbFun,new.env(),as.integer(storePred),as.integer(metrop),metropSD,metropThin,package = "WMCapacity")
		if(storePred) pack@output@predVals = output[[3]]   
      }
      
      cat("\nMCMC acceptance rate: ",round(mean(diff(output[[2]])!=0),3),"\n");flush.console()
      
      pack@output@burnin = as.numeric(pack@settings@MCMCSetup$burnin)
      pack@output@par = niceParVec(rowMeans(output[[1]][,(pack@output@burnin+1):as.integer(pack@settings@EffectiveIters)]), pack@settings@newDat2Cat, pack@settings@newDat2Cont, pack@settings@namedDat2, pack@settings@effects, pack@settings@incCont, TRUE)
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
  if(as.integer(outputSettings$QntEff))  write.csv(t(apply(myPack@output@Effchains[goodIters,],2,quantile,p=c(.025,.05,.1,.25,.5,.75,.9,.95,.975))),file=paste(directory,"/",name,"-",time,"-EffectPostQuantiles.csv",sep=""))

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

WorkingMemInitInstructions <- function(){	
	thisEnv=environment()
	
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Instructions"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="WOMMBAT 1.0 Instructions",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)
	
	# Instructions frame
	frameInstructions <- tkframe(frameOuter,relief="groove",borderwidth=2)
	Txtscr <- tkscrollbar(tt, repeatinterval=5,
                       command=function(...)tkyview(InstrText,...))
	InstrText <- tktext(frameInstructions,bg="white",font="courier",wrap="word",yscrollcommand=function(...)tkset(Txtscr,...))
	tkgrid(InstrText,Txtscr)
	tkgrid.configure(Txtscr,sticky="ns")

	tkinsert(InstrText,"end",WMCapacityHelp()$InitInstructions)
	tkconfigure(InstrText, state="disabled")
	tkfocus(InstrText)
	
	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Instructions Help",message=WMCapacityHelp()$InstrHelp,icon="info",type="ok")

	OnDone=function() tclvalue(done)<-1	
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	#Put all frames up
	tkgrid(frameOuter)

	tkgrid(frameBottom,row=2,column=0,columnspan=2)
	tkgrid(frameInstructions,row=1,column=0,columnspan=2)
	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal="OK"
	if(doneVal==2) retVal=NULL	

	return(retVal)

}



WorkingMemSelectColumns <- function(data){	
	cols=colnames(data)
	thisEnv=environment()
	SelCols=NULL
	
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Select columns of interest"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Data Column Selection",font=WMCapacityFonts()$fontHeading),row=0)
	

	# Left Frame
	frameLeft <- tkframe(frameOuter,relief="flat",borderwidth=2)
	
	scr.AllCols <- tkscrollbar(frameLeft, repeatinterval=5,
				   command=function(...)tkyview(tl.AllCols,...))
	tl.AllCols<-tklistbox(frameLeft,height=20,selectmode="single",
				   yscrollcommand=function(...)tkset(scr.AllCols,...),background="white")
	tkgrid(tl.AllCols,scr.AllCols)
	tkgrid.configure(scr.AllCols,rowspan=20,sticky="nsw")
	tkgrid(tklabel(frameLeft,text="Available Data Columns"))
	for (i in 1:length(cols))
	{
    		tkinsert(tl.AllCols,"end",cols[i])
	}
	tkselection.set(tl.AllCols,0)

	# Middle Upper Frame
	frameMiddleUpper <- tkframe(frameOuter,relief="flat",borderwidth=2)
	
	OnResp=function()
	{
		ColIndex <- as.integer(tkcurselection(tl.AllCols))  
		if(tclvalue(tkcurselection(tl.AllCols))!=""){
			oldVal=tclvalue(RespCol)
			ColName=cols[as.numeric(tkcurselection(tl.AllCols))+1]
			tclvalue(RespCol)<-ColName
			if(oldVal!="<not set>")
			{
				assign("cols",c(cols,oldVal),thisEnv)
				tkinsert(tl.AllCols,"end",oldVal)
			}
			assign("cols",cols[-match(ColName,cols)],thisEnv)
			tkdelete(tl.AllCols,ColIndex)
			tkselection.set(tl.AllCols,0)
		}
	}
	OnChange=function()
	{
		ColIndex <- as.integer(tkcurselection(tl.AllCols))  
		if(tclvalue(tkcurselection(tl.AllCols))!=""){
			oldVal=tclvalue(ChangeCol)
			ColName=cols[as.numeric(tkcurselection(tl.AllCols))+1]
			tclvalue(ChangeCol)<-ColName
			if(oldVal!="<not set>")
			{
				assign("cols",c(cols,oldVal),thisEnv)
				tkinsert(tl.AllCols,"end",oldVal)
			}
			assign("cols",cols[-match(ColName,cols)],thisEnv)
			tkdelete(tl.AllCols,ColIndex)
			tkselection.set(tl.AllCols,0)
		}
	}
	OnSetSize=function()
	{
		ColIndex <- as.integer(tkcurselection(tl.AllCols))  
		if(tclvalue(tkcurselection(tl.AllCols))!=""){
			oldVal=tclvalue(SetSizeCol)
			ColName=cols[as.numeric(tkcurselection(tl.AllCols))+1]
			tclvalue(SetSizeCol)<-ColName
			if(oldVal!="<not set>")
			{
				assign("cols",c(cols,oldVal),thisEnv)
				tkinsert(tl.AllCols,"end",oldVal)
			}
			assign("cols",cols[-match(ColName,cols)],thisEnv)
			tkdelete(tl.AllCols,ColIndex)
			tkselection.set(tl.AllCols,0)
		}
	}	
	Resp.but <- tkbutton(frameMiddleUpper,text="Response Variable--->",command=OnResp)
	Change.but <- tkbutton(frameMiddleUpper,text="Change Variable--->",command=OnChange)
	SetSize.but <- tkbutton(frameMiddleUpper,text="Set Size Variable--->",command=OnSetSize)
	tkgrid(Resp.but)
	tkgrid(Change.but)
	tkgrid(SetSize.but)

	# Middle Lower Frame
	frameMiddleLower <- tkframe(frameOuter,relief="flat",borderwidth=2)
	
	OnCateg=function()
	{
		ColIndex <- as.integer(tkcurselection(tl.AllCols))
   		if(tclvalue(tkcurselection(tl.AllCols))!=""){
   			ColName <- cols[as.numeric(tkcurselection(tl.AllCols))+1]
  	 		tkdelete(tl.AllCols,ColIndex)
			tkinsert(tl.SelCols,"end",ColName)
			assign("SelCols",rbind(SelCols,c(ColName,"CATEG")),thisEnv)
			assign("cols",cols[-match(ColName,cols)],thisEnv)
			tkselection.set(tl.AllCols,0)
		}
	}

	OnContin=function()
	{
		ColIndex <- as.integer(tkcurselection(tl.AllCols))
   		if(tclvalue(tkcurselection(tl.AllCols))!=""){
   			ColName <- cols[as.numeric(tkcurselection(tl.AllCols))+1]
   			tkdelete(tl.AllCols,ColIndex)
			tkinsert(tl.SelCols,"end",ColName)
			assign("SelCols",rbind(SelCols,c(ColName,"CONTIN")),thisEnv)	
			assign("cols",cols[-match(ColName,cols)],thisEnv)
			tkselection.set(tl.AllCols,0)
		}
	}
	OnRemove=function()
	{
		ColIndex <- as.integer(tkcurselection(tl.SelCols))
   		if(tclvalue(tkcurselection(tl.SelCols))!=""){
   			if(length(SelCols)==2){
   				ColName <- SelCols[1]   		
   			}else{
   				ColName <- SelCols[as.numeric(tkcurselection(tl.SelCols))+1,1]
   			}
   			tkdelete(tl.SelCols,ColIndex)
			tkinsert(tl.AllCols,"end",ColName)
			if(length(SelCols)==2){
				assign("SelCols",NULL,thisEnv)
			}else{
				assign("SelCols",SelCols[-match(ColName,SelCols[,1]),],thisEnv)	
			}
			assign("cols",c(cols,ColName),thisEnv)	
			tkselection.set(tl.SelCols,0)
		}
	}	
	CATEG.but <- tkbutton(frameMiddleLower,text="Categorical Variable --->",command=OnCateg)
	Contin.but <- tkbutton(frameMiddleLower,text="Continuous Variable--->",command=OnContin)
	Remove.but <- tkbutton(frameMiddleLower,text="<---Remove Variable",command=OnRemove)
	tkgrid(CATEG.but)
	tkgrid(Contin.but)
	tkgrid(Remove.but)
	
	# Right Upper Frame
	frameRightUpper <- tkframe(frameOuter,relief="flat",borderwidth=2)
	
	RespCol <- tclVar("<not set>")
	entry.Resp <-tkentry(frameRightUpper,width="15",textvariable=RespCol)
	ChangeCol <- tclVar("<not set>")
	entry.Change <-tkentry(frameRightUpper,width="15",textvariable=ChangeCol)
	SetSizeCol <- tclVar("<not set>")
	entry.SetSize <-tkentry(frameRightUpper,width="15",textvariable=SetSizeCol)

	tkgrid(tklabel(frameRightUpper,text="Response Column:"),entry.Resp)
	tkgrid(tklabel(frameRightUpper,text="Change Column:"),entry.Change)
	tkgrid(tklabel(frameRightUpper,text="Set Size Column:"),entry.SetSize)
	tkconfigure(entry.Resp, state="readonly")
	tkconfigure(entry.Change, state="readonly")
	tkconfigure(entry.SetSize, state="readonly")

	# Right Lower Frame
	frameRightLower <- tkframe(frameOuter,relief="flat",borderwidth=2)

	
	scr.SelCols <- tkscrollbar(frameRightLower, repeatinterval=5,
				   command=function(...)tkyview(tl.SelCols,...))
	tl.SelCols<-tklistbox(frameRightLower,height=10,selectmode="single",
				   yscrollcommand=function(...)tkset(scr.SelCols,...),background="white")
	tkgrid(tl.SelCols,scr.SelCols)
	tkgrid.configure(scr.SelCols,rowspan=10,sticky="nsw")
	tkgrid(tklabel(frameRightLower,text="Selected Data Columns"))
			
	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Column Selection Help",message=WMCapacityHelp()$ColSelHelp,icon="info",type="ok")

	OnDone=function(){ 
		error=""
		## Sanity check on columns
		respcolnum = match(tclvalue(RespCol),colnames(data))
		if(!is.na(respcolnum)){
			if(!identical(sort(as.integer(unique(data[,respcolnum]))),as.integer(c(0,1))))
				error=paste(error,"Invalid response column.\n\n",sep="")
		}else{
			error=paste(error,"No response column specified.\n\n",sep="")
		}
		
		changecolnum = match(tclvalue(ChangeCol),colnames(data))
		if(!is.na(changecolnum)){
			if(!identical(sort(as.integer(unique(data[,changecolnum]))),as.integer(c(0,1))))
				error=paste(error,"Invalid change column.\n\n",sep="")
		}else{
			error=paste(error,"No change column specified.\n\n",sep="")
		}
		
		setsizecolnum = match(tclvalue(SetSizeCol),colnames(data))
		if(!is.na(setsizecolnum)){
			if(!is.integer(data[,setsizecolnum]) | any(data[,setsizecolnum]<1))
				error=paste(error,"Invalid set size column.\n",sep="")
		}else{
			error=paste(error,"No set size column specified.\n\n",sep="")
		}
		
		if(is.null(SelCols))
			error=paste(error,"You must specify at least one column of interest.\n\n",sep="")
		
		if(identical("",error)){ 
			tclvalue(done)<-1	
		}else{
			modalDialog(title="Error",message=error,reFocus=tt)
		}
	}
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")
	
	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	# Put all frames up
	tkgrid(frameOuter)
	HorizSep=ttkseparator(frameOuter,orient="horizontal")
	tkgrid(HorizSep,row=2,column=1,columnspan=2)
	tkgrid(frameLeft,row=1,column=0,rowspan=3)
	tkgrid(frameMiddleUpper,row=1,column=1)
	tkgrid(frameMiddleLower,row=3,column=1)
	tkgrid(frameRightUpper,row=1,column=2)
	tkgrid(frameRightLower,row=3,column=2)
	tkgrid(frameBottom,row=4,columnspan=3)
	
	tkgrid.configure(HorizSep,sticky="we")
	tkgrid.configure(frameBottom,sticky="we")
	
	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal=list(response=tclvalue(RespCol),change=tclvalue(ChangeCol),setsize=tclvalue(SetSizeCol),selectedcols=SelCols)
	if(doneVal==2) retVal=list(0)	

	return(retVal)
}

modalDialog <- function(title,message,reFocus)
{
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,title)
  ReturnVal <- NULL
  label1 <- tklabel(dlg,text=message)
  tkgrid(label1)

  onOK <- function()
  {
    ReturnVal <<- "OK"
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(reFocus)
  }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  tkgrid(OK.but)

  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(reFocus)})
   tkwait.window(dlg)

  return(ReturnVal)

}


WorkingMemSelectEffects <- function(allEffects){	
	thisEnv=environment()

	Keffs=numeric(0)
	Aeffs=numeric(0)
	Geffs=numeric(0)

	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Select effects of interest"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Effect Selection",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)

	# Left frame
	frameLeft <- tkframe(frameOuter,relief="flat",borderwidth=2)
	xScr       <- tkscrollbar(frameLeft,command=function(...)tkxview(treeWidget,...),orient="horizontal")
	yScr       <- tkscrollbar(frameLeft,command=function(...)tkyview(treeWidget,...))
	treeWidget <- tkwidget(frameLeft,"Tree",xscrollcommand=function(...)tkset(xScr,...),
                                 yscrollcommand=function(...)tkset(yScr,...),width=30,height=20)
	tkgrid(treeWidget,yScr)
	tkgrid.configure(yScr,stick="nsw")
	tkgrid(xScr)
	tkgrid.configure(xScr,stick="new")
	tkgrid(tklabel(frameLeft,text="Available Effects"))


	for(i in 1:max(allEffects[,1])){
		if(i==1){
			nodename="Main Effects"
		}else{
			nodename=paste(i,"way")
		}
		tkinsert(treeWidget,"end","root",paste("VisArrayEffectNode.",as.character(i),sep=""),text=nodename)
		}

	for(i in 1:dim(allEffects)[1]){
		tkinsert(treeWidget,"end",paste("VisArrayEffectNode.",as.character(allEffects[i,1]),sep=""),as.character(allEffects[i,4]),text=as.character(allEffects[i,4]))
	}

	# Middle Upper frame
	frameMiddleUpper <- tkframe(frameOuter,relief="flat",borderwidth=2)
	
	OnAddK <- function()
	{
		choice <- tclvalue(tcl(treeWidget,"selection","get"))	
		if(substr(choice,1,1)=="{" & substr(choice,nchar(choice),nchar(choice))=="}") choice=substr(choice,2,nchar(choice)-1)
		if(is.na(pmatch("VisArrayEffectNode",choice)) & !(choice%in%Keffs)){
			tkinsert(tl.SelEffsK,"end",choice)
			Keffs=cbind(Keffs,choice)
			assign("Keffs",Keffs,pos=thisEnv)
		}
	##### add to K
	}

	OnRemK <- function()
	{	
		if(tclvalue(tkcurselection(tl.SelEffsK))!=""){
			Index <- as.integer(tkcurselection(tl.SelEffsK))
			tkdelete(tl.SelEffsK,Index)
			Keffs=Keffs[-(Index+1)]
			assign("Keffs",Keffs,pos=thisEnv)
		}
		##### Rem K
	}

	
	AddK.but <- tkbutton(frameMiddleUpper,text="Affects K--->",command=OnAddK)
	RemK.but <- tkbutton(frameMiddleUpper,text="<---Remove from K",command=OnRemK)
	tkgrid(AddK.but)
	tkgrid(RemK.but)

	# Middle Middle frame
	frameMiddleMiddle <- tkframe(frameOuter,relief="flat",borderwidth=2)

	OnAddA <- function()
	{
		choice <- tclvalue(tcl(treeWidget,"selection","get"))	
		if(substr(choice,1,1)=="{" & substr(choice,nchar(choice),nchar(choice))=="}") choice=substr(choice,2,nchar(choice)-1)
		if(is.na(pmatch("VisArrayEffectNode",choice)) & !(choice%in%Aeffs)){
			tkinsert(tl.SelEffsA,"end",choice)
			Aeffs=cbind(Aeffs,choice)
			assign("Aeffs",Aeffs,pos=thisEnv)
		}
	##### add to A
	}

	OnRemA <- function()
	{
		if(tclvalue(tkcurselection(tl.SelEffsA))!=""){
			Index <- as.integer(tkcurselection(tl.SelEffsA))
			tkdelete(tl.SelEffsA,Index)
			Aeffs=Aeffs[-(Index+1)]
			assign("Aeffs",Aeffs,pos=thisEnv)
		}
	}
	
	AddA.but <- tkbutton(frameMiddleMiddle,text="Affects Z--->",command=OnAddA)
	RemA.but <- tkbutton(frameMiddleMiddle,text="<---Remove from Z",command=OnRemA)
	tkgrid(AddA.but)
	tkgrid(RemA.but)
	
	# Middle Lower frame
	frameMiddleLower <- tkframe(frameOuter,relief="flat",borderwidth=2)

	OnAddG <- function()
	{
		choice <- tclvalue(tcl(treeWidget,"selection","get"))	
		if(substr(choice,1,1)=="{" & substr(choice,nchar(choice),nchar(choice))=="}") choice=substr(choice,2,nchar(choice)-1)
		if(is.na(pmatch("VisArrayEffectNode",choice)) & !(choice%in%Geffs)){
			tkinsert(tl.SelEffsG,"end",choice)
			Geffs=cbind(Geffs,choice)
			assign("Geffs",Geffs,pos=thisEnv)	
		}
		##### add to G
	}
	OnRemG <- function()
	{
		if(tclvalue(tkcurselection(tl.SelEffsG))!=""){
			Index=as.integer(tkcurselection(tl.SelEffsG))
			tkdelete(tl.SelEffsG,Index)
			Geffs=Geffs[-(Index+1)]
			assign("Geffs",Geffs,pos=thisEnv)
		}
	}
	
	AddG.but <- tkbutton(frameMiddleLower,text="Affects G--->",command=OnAddG)
	RemG.but <- tkbutton(frameMiddleLower,text="<---Remove from G",command=OnRemG)
	tkgrid(AddG.but)
	tkgrid(RemG.but)

	# Right Upper frame
	frameRightUpper <- tkframe(frameOuter,relief="flat",borderwidth=2)

	scr.SelEffsK <- tkscrollbar(frameRightUpper, repeatinterval=5,
				   command=function(...)tkyview(tl.SelEffsK,...))
	tl.SelEffsK<-tklistbox(frameRightUpper,height=5,selectmode="single",
				   yscrollcommand=function(...)tkset(scr.SelEffsK,...),background="white")
	tkgrid(tl.SelEffsK,scr.SelEffsK)
	tkgrid.configure(scr.SelEffsK,rowspan=5,sticky="nsw")
	tkgrid(tklabel(frameRightUpper,text="Selected Effects on K"))


	# Right Middle frame
	frameRightMiddle <- tkframe(frameOuter,relief="flat",borderwidth=2)

	scr.SelEffsA <- tkscrollbar(frameRightMiddle, repeatinterval=5,
				   command=function(...)tkyview(tl.SelEffsA,...))
	tl.SelEffsA<-tklistbox(frameRightMiddle,height=5,selectmode="single",
				   yscrollcommand=function(...)tkset(scr.SelEffsA,...),background="white")
	tkgrid(tl.SelEffsA,scr.SelEffsA)
	tkgrid.configure(scr.SelEffsA,rowspan=5,sticky="nsw")
	tkgrid(tklabel(frameRightMiddle,text="Selected Effects on Z"))


	# Right Lower frame
	frameRightLower <- tkframe(frameOuter,relief="flat",borderwidth=2)

	scr.SelEffsG <- tkscrollbar(frameRightLower, repeatinterval=5,
				   command=function(...)tkyview(tl.SelEffsG,...))
	tl.SelEffsG<-tklistbox(frameRightLower,height=5,selectmode="single",
				   yscrollcommand=function(...)tkset(scr.SelEffsG,...),background="white")
	tkgrid(tl.SelEffsG,scr.SelEffsG)
	tkgrid.configure(scr.SelEffsG,rowspan=5,sticky="nsw")
	tkgrid(tklabel(frameRightLower,text="Selected Effects on G"))

	# Model options frame
	frameModOpt <-tkframe(frameOuter,relief="groove",borderwidth=2)
	
	OnUseA=function(){
		if(tclvalue(useAcbVal)=="0")
		{
			tkconfigure(AddA.but,state="disabled")
			tkconfigure(RemA.but,state="disabled")
			tkconfigure(tl.SelEffsA,state="disabled")
			
		}
		if(tclvalue(useAcbVal)=="1")
		{
			tkconfigure(AddA.but,state="normal")
			tkconfigure(RemA.but,state="normal")
			tkconfigure(tl.SelEffsA,state="normal")		
		}
	}
	useAcb <- tkcheckbutton(frameModOpt,command=OnUseA)
	useAcbVal=tclVar("1")
	tkconfigure(useAcb,variable=useAcbVal)

	VertSep1=ttkseparator(frameModOpt,orient="vertical")
	tkgrid(useAcb,column=0,row=1)
	tkgrid(tklabel(frameModOpt,text="Use Zone-out Parameter"),column=0,row=0)
	tkgrid(VertSep1,column=1,row=0,rowspan=3)
	tkgrid.configure(VertSep1,sticky="ns")

	Ktype.rb1 <- tkradiobutton(frameModOpt)
	Ktype.rb2 <- tkradiobutton(frameModOpt)
	Ktype.rbValue <- tclVar("Cowan")
	tkconfigure(Ktype.rb1,variable=Ktype.rbValue,value="Cowan")
	tkconfigure(Ktype.rb2,variable=Ktype.rbValue,value="Pashler")
	tkgrid(tklabel(frameModOpt,text="Which model on K?"),column=2,columnspan=2,row=0)
	tkgrid(tklabel(frameModOpt,text="Cowan (cued) "),column=2,row=1)
	tkgrid(Ktype.rb1,column=3,row=1)
	tkgrid(tklabel(frameModOpt,text="Pashler (uncued) "),Ktype.rb2,column=2,row=2)
	tkgrid(Ktype.rb2,column=3,row=2)
	
	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Effect Selection Help",message=WMCapacityHelp()$EffSelHelp,icon="info",type="ok")

	OnDone=function(){
		error=""
		## Sanity check on effects
		if(identical(Keffs,numeric(0)) & identical(Aeffs,numeric(0)) & identical(Geffs,numeric(0)))
			error=paste(error,"You must specify at least one effect on one parameter.\n\n",sep="")
		
		if(identical("",error)){ 
			tclvalue(done)<-1	
		}else{
			modalDialog(title="Error",message=error,reFocus=tt)
		}
	}	
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)


	#Put all frames up
	tkgrid(frameOuter)
	
	HorizSep1=ttkseparator(frameOuter,orient="horizontal")
	tkgrid(HorizSep1,row=2,column=1,columnspan=2)

	HorizSep2=ttkseparator(frameOuter,orient="horizontal")
	tkgrid(HorizSep2,row=4,column=1,columnspan=2)

	
	tkgrid(frameLeft,row=1,column=0,rowspan=5)
	
	tkgrid(frameMiddleUpper,row=1,column=1)
	tkgrid(frameMiddleMiddle,row=3,column=1)	
	tkgrid(frameMiddleLower,row=5,column=1)
	tkgrid(frameRightUpper,row=1,column=2)
	tkgrid(frameRightMiddle,row=3,column=2)
	tkgrid(frameRightLower,row=5,column=2)
	tkgrid(frameModOpt,row=6,column=2,columnspan=1)
	tkgrid(frameBottom,row=6,column=0,columnspan=2)
	
	tkgrid.configure(HorizSep1,sticky="we")
	tkgrid.configure(HorizSep2,sticky="we")	
	tkgrid.configure(frameBottom,sticky="we")
	tkgrid.configure(frameModOpt,sticky="we")
	
	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(!identical(Keffs,numeric(0))) allEffects[match(Keffs,allEffects[,4]),5]=1
	if(!identical(Aeffs,numeric(0))) allEffects[match(Aeffs,allEffects[,4]),6]=1
	if(!identical(Geffs,numeric(0))) allEffects[match(Geffs,allEffects[,4]),7]=1
	
	if(doneVal==1) retVal=list(Ktype=tclvalue(Ktype.rbValue),useA=tclvalue(useAcbVal),SelEffs=allEffects)
	if(doneVal==2) retVal=list(0)	

	return(retVal)
}

WorkingMemPriorSetup <- function(useA=TRUE,meanMuK=3,sdMuK=10,meanMuA=0,sdMuA=10,meanMuG=0,sdMuG=10,invGamma.a0=1,invGamma.b0=1,WishartDF=2,slopeSD=100,minWishDF=0){	
	thisEnv=environment()
	
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Specify Prior Settings"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Prior Setup",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)


	# Prior on muK frame
	frameMuKprior <- tkframe(frameOuter,relief="groove",borderwidth=2)
	meanMuK.var <- tclVar(as.character(meanMuK))
	meanMuK.Entry <-tkentry(frameMuKprior,width="5",textvariable=meanMuK.var)
	
	sdMuK.var <- tclVar(as.character(sdMuK))
	sdMuK.Entry <-tkentry(frameMuKprior,width="5",textvariable=sdMuK.var)

	tkgrid(tklabel(frameMuKprior,text="grand mean K parameter"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameMuKprior,text="Mean of prior on the grand mean K: "),row=1,column=0)
	tkgrid(meanMuK.Entry,row=1,column=1)
	tkgrid(tklabel(frameMuKprior,text="Standard deviation of prior on the grand mean K: "),row=2,column=0)
	tkgrid(sdMuK.Entry,row=2,column=1)
	tkgrid(tklabel(frameMuKprior,text=">0"),row=2,column=2)	


	# Prior on muA frame
	frameMuAprior <- tkframe(frameOuter,relief="groove",borderwidth=2)
	meanMuA.var <- tclVar(as.character(meanMuA))
	meanMuA.Entry <-tkentry(frameMuAprior,width="5",textvariable=meanMuA.var)
	
	sdMuA.var <- tclVar(as.character(sdMuA))
	sdMuA.Entry <-tkentry(frameMuAprior,width="5",textvariable=sdMuA.var)

	tkgrid(tklabel(frameMuAprior,text="grand mean Z parameter"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameMuAprior,text="Mean of prior on the grand mean Z: "),row=1,column=0)
	tkgrid(meanMuA.Entry,row=1,column=1)
	tkgrid(tklabel(frameMuAprior,text="Standard deviation of prior on the grand mean Z: "),row=2,column=0)
	tkgrid(tklabel(frameMuAprior,text=">0"),row=2,column=2)	
	tkgrid(sdMuA.Entry,row=2,column=1)

	# Prior on muG frame
	frameMuGprior <- tkframe(frameOuter,relief="groove",borderwidth=2)
	meanMuG.var <- tclVar(as.character(meanMuG))
	meanMuG.Entry <-tkentry(frameMuGprior,width="5",textvariable=meanMuG.var)
	
	sdMuG.var <- tclVar(as.character(sdMuG))
	sdMuG.Entry <-tkentry(frameMuGprior,width="5",textvariable=sdMuG.var)

	tkgrid(tklabel(frameMuGprior,text="grand mean G parameter"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameMuGprior,text="Mean of prior on the grand mean G: "),row=1,column=0)
	tkgrid(meanMuG.Entry,row=1,column=1)
	tkgrid(tklabel(frameMuGprior,text="Standard deviation of prior on the grand mean G: "),row=2,column=0)
	tkgrid(tklabel(frameMuGprior,text=">0"),row=2,column=2)	
	tkgrid(sdMuG.Entry,row=2,column=1)

	# Inverse Gamma prior setup
	frameInvGammaPrior <- tkframe(frameOuter,relief="groove",borderwidth=2)
	IGa0.var <- tclVar(as.character(invGamma.a0))
	IGa0.Entry <-tkentry(frameInvGammaPrior,width="5",textvariable=IGa0.var)
	
	IGb0.var <- tclVar(as.character(invGamma.b0))
	IGb0.Entry <-tkentry(frameInvGammaPrior,width="5",textvariable=IGb0.var)

	tkgrid(tklabel(frameInvGammaPrior,text="Inverse Gamma prior on variances"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameInvGammaPrior,text="IG 'a' parameter: "),row=1,column=0)
	tkgrid(IGa0.Entry,row=1,column=1)
	tkgrid(tklabel(frameInvGammaPrior,text=">0"),row=1,column=2)		
	tkgrid(tklabel(frameInvGammaPrior,text="IG 'b' parameter: "),row=2,column=0)
	tkgrid(IGb0.Entry,row=2,column=1)
	tkgrid(tklabel(frameInvGammaPrior,text=">0"),row=2,column=2)		


	# Slope Frame
	#frameSlopePrior <- tkframe(frameOuter,relief="groove",borderwidth=2)
	#slopeSD.var <- tclVar(as.character(slopeSD))
	#slopeSD.Entry <-tkentry(frameSlopePrior,width="5",textvariable=slopeSD.var)
	
	#tkgrid(tklabel(frameSlopePrior,text="Normal prior on regression slopes"),row=0,column=0,columnspan=3)
	#tkgrid(tklabel(frameSlopePrior,text="Prior mean on slopes is 0."),row=1,column=0,columnspan=3)	
	#tkgrid(tklabel(frameSlopePrior,text="Standard deviation on slope prior: "),row=2,column=0)
	#tkgrid(slopeSD.Entry,row=2,column=1)
	#tkgrid(tklabel(frameSlopePrior,text=">0"),row=2,column=2)		
	
	# Covariance Frame
	frameWishartPrior <- tkframe(frameOuter,relief="groove",borderwidth=2)
	WishartDF.var <- tclVar(as.character(WishartDF))
	WishartDF.Entry <-tkentry(frameWishartPrior,width="5",textvariable=WishartDF.var)
	
	tkgrid(tklabel(frameWishartPrior,text="Wishart prior on covariance Matrices"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameWishartPrior,text="Prior scale matrix is the identity matrix."),row=1,column=0,columnspan=3)	
	tkgrid(tklabel(frameWishartPrior,text="Wishart df parameter: "),row=2,column=0)
	tkgrid(WishartDF.Entry,row=2,column=1)
	tkgrid(tklabel(frameWishartPrior,text=paste(">",minWishDF,sep="")),row=2,column=2)		
	

	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Prior Setup Help",message=WMCapacityHelp()$PriorSetHelp,icon="info",type="ok")

	OnDone=function(){
		error=""
		## Sanity check on prior parameters
		if(as.numeric(tclvalue(IGa0.var))<=0)
			error=paste(error,"Inverse Gamma prior must have a>0.\n\n",sep="")
		if(as.numeric(tclvalue(IGb0.var))<=0)
			error=paste(error,"Inverse Gamma prior must have b>0.\n\n",sep="")
		if(as.numeric(tclvalue(sdMuK.var))<=0)
			error=paste(error,"Standard deviation of prior on muK must have sd>0.\n\n",sep="")
		if(as.numeric(tclvalue(sdMuA.var))<=0 & useA)
			error=paste(error,"Standard deviation of prior on muA must have sd>0.\n\n",sep="")
		if(as.numeric(tclvalue(sdMuG.var))<=0)
			error=paste(error,"Standard deviation of prior on muG must have sd>0.\n\n",sep="")
		if(as.numeric(tclvalue(WishartDF.var))<=minWishDF)
			error=paste(error,"For the selected covariance matrices, df must be >",minWishDF,".\n\n",sep="")
		
		if(identical("",error)){ 
			tclvalue(done)<-1	
		}else{
			modalDialog(title="Error",message=error,reFocus=tt)
		}
	}		
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	#Put all frames up
	tkgrid(frameOuter)


	tkgrid(frameMuKprior,row=1,column=1)
	if(useA) tkgrid(frameMuAprior,row=2,column=1)
	tkgrid(frameMuGprior,row=3,column=1)
	tkgrid(frameBottom,row=4,column=0,columnspan=2)
	tkgrid(frameInvGammaPrior,row=1,column=0)
	#tkgrid(frameSlopePrior,row=2,column=0)
	tkgrid(frameWishartPrior,row=3,column=0)
	
	
	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal=list(
				IGa0=tclvalue(IGa0.var),
				IGb0=tclvalue(IGb0.var),
				meanMuK=tclvalue(meanMuK.var),
				sdMuK=tclvalue(sdMuK.var),
				meanMuA=tclvalue(meanMuA.var),
				sdMuA=tclvalue(sdMuA.var),
				meanMuG=tclvalue(meanMuG.var),
				sdMuG=tclvalue(sdMuG.var),
				WishartDF=tclvalue(WishartDF.var)
				#slopeSD=tclvalue(slopeSD.var)
				)
	if(doneVal==2) retVal=list(0)	

	return(retVal)

}

WorkingMemMCMCSetup <- function(optimMaxit=200,nIters=1000,burnin=200,testrun=FALSE,hybridEpsilon=c(.01,.02),hybridLFsteps=60,hybridMultipoint=FALSE,hybridMultipointSize=1,hybridWeight=FALSE,hybridMPWeight=TRUE,progress=10,MetHastScale=.2,useMH="0",MetHastThin=1){	
	thisEnv=environment()
	
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Set MCMC Options"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Set MCMC Options",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)

	# HybridMC frame
	frameHybridMC<-tkframe(frameOuter,relief="groove",borderwidth=2)
	hybridEpsilonLower.var <- tclVar(as.character(hybridEpsilon[1]))
	EpsLowEntry <-tkentry(frameHybridMC,width="6",textvariable=hybridEpsilonLower.var)
	hybridEpsilonUpper.var <- tclVar(as.character(hybridEpsilon[2]))
	EpsUppEntry <-tkentry(frameHybridMC,width="6",textvariable=hybridEpsilonUpper.var)
	hybridLF.var <- tclVar(as.character(hybridLFsteps))
	LFEntry <-tkentry(frameHybridMC,width="3",textvariable=hybridLF.var)

	OnUseMH=function(){
		if(tclvalue(useMHVal)=="0")
		{
			tkconfigure(MHscale.Entry,state="disabled")
			tkconfigure(MHthin.Entry,state="disabled")
			tkconfigure(EpsLowEntry,state="normal")
			tkconfigure(EpsUppEntry,state="normal")
			tkconfigure(LFEntry,state="normal")
			tclvalue(hybridEpsilonLower.var)=as.character(hybridEpsilon[1])
			tclvalue(hybridEpsilonUpper.var)=as.character(hybridEpsilon[2])
			tclvalue(hybridLF.var)=as.character(hybridLFsteps)
		}
		if(tclvalue(useMHVal)=="1")
		{
			tkconfigure(MHscale.Entry,state="normal")		
			tclvalue(MHscale.var)=as.character(MetHastScale)
			tkconfigure(MHthin.Entry,state="normal")		
			tclvalue(MHthin.var)=as.character(MetHastThin)		
			tkconfigure(EpsLowEntry,state="disabled")
			tkconfigure(EpsUppEntry,state="disabled")
			tkconfigure(LFEntry,state="disabled")

		}
	}
	useMHcb <- tkcheckbutton(frameHybridMC,command=OnUseMH)
	useMHVal=tclVar(useMH)
	tkconfigure(useMHcb,variable=useMHVal)		
	
	MHscale.var <- tclVar(as.character(MetHastScale))
	MHscale.Entry <-tkentry(frameHybridMC,width="5",textvariable=MHscale.var)
	
	MHthin.var <- tclVar(as.character(MetHastThin))
	MHthin.Entry <-tkentry(frameHybridMC,width="5",textvariable=MHthin.var)
	
	if(useMH=="0")
		{
			tkconfigure(MHscale.Entry,state="disabled")
			tkconfigure(EpsLowEntry,state="normal")
			tkconfigure(EpsUppEntry,state="normal")
			tkconfigure(LFEntry,state="normal")
			tclvalue(hybridEpsilonLower.var)=as.character(hybridEpsilon[1])
			tclvalue(hybridEpsilonUpper.var)=as.character(hybridEpsilon[2])
			tclvalue(hybridLF.var)=as.character(hybridLFsteps)
			tkconfigure(MHthin.Entry,state="disabled")
		
		}else{
			tkconfigure(MHscale.Entry,state="normal")
			tkconfigure(MHthin.Entry,state="normal")
			tkconfigure(EpsLowEntry,state="disabled")
			tkconfigure(EpsUppEntry,state="disabled")
			tkconfigure(LFEntry,state="disabled")

		}


	
	tkgrid(tklabel(frameHybridMC,text="Hybrid Monte Carlo Settings"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameHybridMC,text="Lower epsilon value: "),row=1,column=0)
	tkgrid(tklabel(frameHybridMC,text="Upper epsilon value: "),row=2,column=0)
	tkgrid(EpsLowEntry,row=1,column=1)
	tkgrid(EpsUppEntry,row=2,column=1)
	tkgrid(tklabel(frameHybridMC,text="Number of Leapfrog steps: "),row=3,column=0)
	tkgrid(LFEntry,row=3,column=1)
	tkgrid(tklabel(frameHybridMC,text="Use Simple Metropolis (no Hybrid)? "),row=4,column=0)
	tkgrid(useMHcb,row=4,column=1)
	tkgrid(tklabel(frameHybridMC,text="Metropolis Candidate scale: "),row=5,column=0)	
	tkgrid(MHscale.Entry,row=5,column=1)
	tkgrid(tklabel(frameHybridMC,text="Thinning Every: "),row=6,column=0)	
	tkgrid(MHthin.Entry,row=6,column=1)


	#tkconfigure(useMPWeightcb,variable=useMPWeightVal,state="disabled")
	#tkconfigure(MPSize.Entry,state="disabled")



	# Starting Values frame	
	frameStartingVals<-tkframe(frameOuter,relief="groove",borderwidth=2)
	optimMaxIter.var <- tclVar(as.character(optimMaxit))
	optimMaxitEntry <-tkentry(frameStartingVals,width="4",textvariable=optimMaxIter.var)

	tkgrid(tklabel(frameStartingVals,text="Starting Value Options"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameStartingVals,text="Maximum Optim Iterations: "),row=1,column=0)
	tkgrid(tklabel(frameStartingVals,text="Set to 0 to disable optim() starting values."),row=2,column=0,columnspan=3)
	tkgrid(optimMaxitEntry,row=1,column=1)

	# General Setup frame	
	frameGeneral<-tkframe(frameOuter,relief="groove",borderwidth=2)
	MCMCIterations.var <- tclVar(as.character(nIters))
	MCMCIterationsEntry <-tkentry(frameGeneral,width="7",textvariable=MCMCIterations.var)
	BurninIterations.var <- tclVar(as.character(burnin))
	BurninIterationsEntry <-tkentry(frameGeneral,width="7",textvariable=BurninIterations.var)
	Progress.var <- tclVar(as.character(progress))
	ProgressEntry <-tkentry(frameGeneral,width="7",textvariable=Progress.var)
	
	isTestcb <- tkcheckbutton(frameGeneral)
	testcb.val=tclVar("0")
	tkconfigure(isTestcb,variable=testcb.val)


	tkgrid(tklabel(frameGeneral,text="General Options"),row=0,column=0,columnspan=3)
	tkgrid(tklabel(frameGeneral,text="MCMC Iterations: "),row=1,column=0)
	tkgrid(MCMCIterationsEntry,row=1,column=1)
	tkgrid(tklabel(frameGeneral,text="Burnin Iterations: "),row=2,column=0)
	tkgrid(BurninIterationsEntry,row=2,column=1)
	tkgrid(tklabel(frameGeneral,text="Report Progress Every: "),row=3,column=0)
	tkgrid(ProgressEntry,row=3,column=1)
	tkgrid(tklabel(frameGeneral,text="Test run? "),row=4,column=0)
	tkgrid(isTestcb,row=4,column=1)
	
	
	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="MCMC Options Help",message=WMCapacityHelp()$MCMCHelp,icon="info",type="ok")

	OnDone=function(){
		error=""
		## Sanity check on MCMC parameters
		if(as.numeric(tclvalue(hybridLF.var))<=0)
			error=paste(error,"Number of leapfrog steps must be greater than 0.\n\n",sep="")
		if(as.numeric(tclvalue(hybridEpsilonLower.var))<=0 | as.numeric(tclvalue(hybridEpsilonLower.var))>as.numeric(tclvalue(hybridEpsilonUpper.var)))
			error=paste(error,"Epsilons must both be greater than 0, and the upper value must be greater than the lower value.\n\n",sep="")
		if(as.numeric(tclvalue(optimMaxIter.var))<0)
			error=paste(error,"Number of optim iterations must be 0 or more.\n\n",sep="")
		if(as.numeric(tclvalue(BurninIterations.var))<0 | as.numeric(tclvalue(BurninIterations.var))>=as.numeric(tclvalue(MCMCIterations.var)))
			error=paste(error,"Number of burnin iterations must be 0 or more, and must be less than total MCMC iterations.\n\n",sep="")
		if(as.numeric(tclvalue(MCMCIterations.var))<=0)
			error=paste(error,"The number of MCMC iterations must be >0.\n\n",sep="")
		
		if(as.numeric(tclvalue(MHthin.var))<0 | as.numeric(tclvalue(MHthin.var))>=as.numeric(tclvalue(MCMCIterations.var)))
			error=paste(error,"Number of thinning iterations must be 0 or more, and must be less than total MCMC iterations.\n\n",sep="")
		if(as.numeric(tclvalue(MHscale.var))<=0)
			error=paste(error,"Metropolis-Hastings scale must be >0.\n\n",sep="")
		
		if(tclvalue(useMHVal)=="1" & as.numeric(tclvalue(BurninIterations.var))>=as.numeric(tclvalue(MCMCIterations.var))/as.numeric(tclvalue(MHthin.var)))
			error=paste(error,"Burnin too high - must be less than the number of samples after thinning.\n\n",sep="")
		

		if(identical("",error)){ 
			tclvalue(done)<-1	
		}else{
			modalDialog(title="Error",message=error,reFocus=tt)
		}
	}	
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	#Put all frames up
	tkgrid(frameOuter)

	tkgrid(frameBottom,row=2,column=0,columnspan=2)
	tkgrid(frameGeneral,row=1,column=0)
	tkgrid(frameHybridMC,row=1,column=2)	
	tkgrid(frameStartingVals,row=1,column=1)
	tkwait.variable(done)


	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal=list(nIter=tclvalue(MCMCIterations.var),
					burnin=tclvalue(BurninIterations.var),
					progress=tclvalue(Progress.var),
					testRun=tclvalue(testcb.val),
					optimMaxIter=tclvalue(optimMaxIter.var),
					epsLow=tclvalue(hybridEpsilonLower.var),
					epsUpp=tclvalue(hybridEpsilonUpper.var),
					leapfrog=tclvalue(hybridLF.var),
					useMH=tclvalue(useMHVal),
					MHscale=tclvalue(MHscale.var),
					MHthin=tclvalue(MHthin.var)				
					#MPsteps=tclvalue(hybridMPsteps.var)
				)
	if(doneVal==2) retVal=list(0)	

	return(retVal)

}



WorkingMemCovarianceSetup <- function(myCovList){	
	thisEnv=environment()
        covGroups=list()

        
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Covariance Structure Setup"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Setup Covariance Models",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)


	# Effects tree frame
	frameEffects <- tkframe(frameOuter, relief="flat",borderwidth=2)
	xScrEff       <- tkscrollbar(frameEffects,command=function(...)tkxview(treeWidgetEff,...),orient="horizontal")
	yScrEff       <- tkscrollbar(frameEffects,command=function(...)tkyview(treeWidgetEff,...))
	treeWidgetEff <- tkwidget(frameEffects,"Tree",xscrollcommand=function(...)tkset(xScrEff,...),
                                 yscrollcommand=function(...)tkset(yScrEff,...),width=30,height=20)
	tkgrid(treeWidgetEff,yScrEff)
	tkgrid.configure(yScrEff,stick="nsw")
	tkgrid(xScrEff)
	tkgrid.configure(xScrEff,stick="new")
	tkgrid(tklabel(frameEffects,text="Selected Effects"))

        effNodes=paste("Node",sort(unique(myCovList[,2])),sep="")
        effNames=paste(sort(unique(myCovList[,2]))," levels",sep=" ")
        for(i in 1:length(effNodes)){
          tkinsert(treeWidgetEff,"end","root",effNodes[i],text=effNames[i])
        }
        
	for(i in 1:dim(myCovList)[1])
          {
            tkinsert(treeWidgetEff,"end",paste("Node",myCovList[i,2],sep=""),
                     i,
                     text=paste(myCovList[i,3]," on ",myCovList[i,1],sep=""))
	  }

        # Buttons frame
 	frameButtons <- tkframe(frameOuter,relief="flat",borderwidth=2)

        OnAddNew=function()
          {
            sel = tclvalue(tcl(treeWidgetEff,"selection","get"))
            if(substr(sel,1,4)!="Node" & sel != ""){
              nPar = myCovList[as.numeric(sel),2]
              newNodeName=round(runif(1),6)
              covGroups[[length(covGroups)+1]]=list(nPar,as.numeric(sel),newNodeName)
              tkinsert(treeWidgetCov,"end","root",paste("Node",newNodeName,sep=""),text=paste("Group:",nPar,"levels"))
              tkinsert(treeWidgetCov,"end",paste("Node",newNodeName,sep=""),sel,
                       text=paste(myCovList[as.numeric(sel),3]," on ",myCovList[as.numeric(sel),1],sep=""))
              tkdelete(treeWidgetEff,sel)
              assign("covGroups",covGroups,pos=thisEnv)
            }
          }
        
        OnAddSel=function(){
           sel = tclvalue(tcl(treeWidgetEff,"selection","get"))
           sel2= tclvalue(tcl(treeWidgetCov,"selection","get"))
           if(substr(sel2,1,4)=="Node" & sel != ""){
              nPar = myCovList[as.numeric(sel),2]
              myNode=as.numeric(substr(sel2,5,nchar(sel2)))
              for(i in 1:length(covGroups)){
                if(covGroups[[i]][[3]]==myNode) index=i
              }
              if(nPar==covGroups[[index]][[1]]){
                covGroups[[index]][[2]]=c(covGroups[[index]][[2]],as.numeric(sel))
                tkinsert(treeWidgetCov,"end",paste("Node",myNode,sep=""),sel,
                       text=paste(myCovList[as.numeric(sel),3]," on ",myCovList[as.numeric(sel),1],sep=""))
                tkdelete(treeWidgetEff,sel)
                assign("covGroups",covGroups,pos=thisEnv)

              }
            }
        }
        OnRem=function()
          {
           
           sel2= tclvalue(tcl(treeWidgetCov,"selection","get"))
           if(substr(sel2,1,4)=="Node"){
             myNode=as.numeric(substr(sel2,5,nchar(sel2)))
             for(i in 1:length(covGroups)){
               if(covGroups[[i]][[3]]==myNode) index=i
             }
             tkdelete(treeWidgetCov,sel2)
             for(i in 1:length(covGroups[[index]][[2]])){
               t=covGroups[[index]][[2]][i]
               tkinsert(treeWidgetEff,"end",paste("Node",myCovList[t,2],sep=""),
                        t,
                        text=paste(myCovList[t,3]," on ",myCovList[t,1],sep=""))
             }
             if(length(covGroups)>1){
               covGroups=covGroups[-index]
             }else{
               covGroups=list()
             }
             assign("covGroups",covGroups,pos=thisEnv)
           }
           if(substr(sel2,1,4)!="Node" & sel2!=""){
             tkdelete(treeWidgetCov,sel2)
             tkinsert(treeWidgetEff,"end",paste("Node",myCovList[as.numeric(sel2),2],sep=""),
                      sel2,
                      text=paste(myCovList[as.numeric(sel2),3]," on ",myCovList[as.numeric(sel2),1],sep=""))
             for(i in 1:length(covGroups))
               {
                 myList=covGroups[[i]][[2]]
                 myList=myList[myList!=as.numeric(sel2)]
                 if(length(myList)==0){
                   tkdelete(treeWidgetCov,paste("Node",covGroups[[i]][[3]],sep=""))
                   covGroups=ifelse(length(covGroups)>1,covGroups[-i],list())
                 }else{
                   covGroups[[i]][[2]]=myList
                 }
               }
             assign("covGroups",covGroups,pos=thisEnv)
           }


          }
	Rem.but <- tkbutton(frameButtons,text=   "<--- Remove group or item ",command=OnRem)
        AddSel.but <- tkbutton(frameButtons,text="Add to SELECTED covariance group --->",command=OnAddSel)
        AddNew.but <- tkbutton(frameButtons,text="   Add to NEW covariance group   --->",command=OnAddNew)
	tkgrid(AddNew.but,column=0,row=0)
	tkgrid(AddSel.but,column=0,row=1)
        tkgrid(Rem.but,column=0,row=2)

	# Cov groups tree frame
	frameCov <- tkframe(frameOuter, relief="flat",borderwidth=2)
	xScrCov       <- tkscrollbar(frameCov,command=function(...)tkxview(treeWidgetCov,...),orient="horizontal")
	yScrCov       <- tkscrollbar(frameCov,command=function(...)tkyview(treeWidgetCov,...))
	treeWidgetCov <- tkwidget(frameCov,"Tree",xscrollcommand=function(...)tkset(xScrCov,...),
                                 yscrollcommand=function(...)tkset(yScrCov,...),width=30,height=20)
	tkgrid(treeWidgetCov,yScrCov)
	tkgrid.configure(yScrCov,stick="nsw")
	tkgrid(xScrCov)
	tkgrid.configure(xScrCov,stick="new")
	tkgrid(tklabel(frameCov,text="Covariance groups"))

        
	               
	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Covariance Model Help",message=WMCapacityHelp()$CovHelp,icon="info",type="ok")

	OnDone=function(){
		error=""
		## Sanity check on Covariance Matrices 
		if(length(covGroups)>0){
			if(any(lapply(covGroups,function(v) length(v[[2]]))<2))
				error=paste(error,"All covariance groups must have at least 2 members.\n\n",sep="")
		}
		
		if(identical("",error)){ 
			tclvalue(done)<-1	
		}else{
			modalDialog(title="Error",message=error,reFocus=tt)
		}
	}
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	#Put all frames up
	tkgrid(frameOuter)

        tkgrid(frameEffects,row=1,column=0,rowspan=3)
        tkgrid(frameButtons,row=2,column=1)
        tkgrid(frameCov,row=1,column=2,rowspan=3)
        tkgrid(frameBottom,row=4,column=0,columnspan=2)

	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal=covGroups
	if(doneVal==2) retVal=list(0)	

	return(retVal)

}


WorkingMemOutputSetup <- function(){	
	thisEnv=environment()
	
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Output Setup"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Specify Output Options",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)

	# Any Output frame	
	frameOutput<-tkframe(frameOuter,relief="groove",borderwidth=2)
	
	Outputcb <- tkcheckbutton(frameOutput)
	Outputcb.val=tclVar("1")
	tkconfigure(Outputcb,variable=Outputcb.val)
	outputlab = tklabel(frameOutput,text="Output files selected below?")
	tkgrid(outputlab,row=0,column=1)
	tkgrid(Outputcb,row=0,column=0)
	tkgrid(tklabel(frameOutput,text=paste("All files will be saved in ",getwd(),sep="")),row=1,column=1)
	tkgrid.configure(outputlab,sticky="w")

	
	# Chains frame	
	frameChains<-tkframe(frameOuter,relief="groove",borderwidth=2)
	
	ChnEffcb <- tkcheckbutton(frameChains)
	ChnEffcb.val=tclVar("1")
	tkconfigure(ChnEffcb,variable=ChnEffcb.val)
	ChnMeanscb <- tkcheckbutton(frameChains)
	ChnMeanscb.val=tclVar("1")
	tkconfigure(ChnMeanscb,variable=ChnMeanscb.val)
	ChnCovcb <- tkcheckbutton(frameChains)
	ChnCovcb.val=tclVar("1")
	tkconfigure(ChnCovcb,variable=ChnCovcb.val)
	ChnCorcb <- tkcheckbutton(frameChains)
	ChnCorcb.val=tclVar("1")
	tkconfigure(ChnCorcb,variable=ChnCorcb.val)

	tkgrid(tklabel(frameChains,text="Full Chains"),row=0,column=0)
	tkgrid(tklabel(frameChains,text="Random Effects/Slopes"),row=1,column=0)
	tkgrid(ChnEffcb,row=1,column=1)
	tkgrid(tklabel(frameChains,text="Effect/Slope Means"),row=2,column=0)
	tkgrid(ChnMeanscb,row=2,column=1)
	tkgrid(tklabel(frameChains,text="Covariance Matrices"),row=3,column=0)
	tkgrid(ChnCovcb,row=3,column=1)
	tkgrid(tklabel(frameChains,text="Correlation Matrices"),row=4,column=0)
	tkgrid(ChnCorcb,row=4,column=1)

	# Quantiles frame	
	frameQuantiles<-tkframe(frameOuter,relief="groove",borderwidth=2)
	
	QntEffcb <- tkcheckbutton(frameQuantiles)
	QntEffcb.val=tclVar("1")
	tkconfigure(QntEffcb,variable=QntEffcb.val)
	QntMeanscb <- tkcheckbutton(frameQuantiles)
	QntMeanscb.val=tclVar("1")
	tkconfigure(QntMeanscb,variable=QntMeanscb.val)
	QntCovcb <- tkcheckbutton(frameQuantiles)
	QntCovcb.val=tclVar("1")
	tkconfigure(QntCovcb,variable=QntCovcb.val)
	QntCorcb <- tkcheckbutton(frameQuantiles)
	QntCorcb.val=tclVar("1")
	tkconfigure(QntCorcb,variable=QntCorcb.val)

	tkgrid(tklabel(frameQuantiles,text="Posterior Quantiles"),row=0,column=0)
	tkgrid(tklabel(frameQuantiles,text="Random Effects/Slopes"),row=1,column=0)
	tkgrid(QntEffcb,row=1,column=1)
	tkgrid(tklabel(frameQuantiles,text="Effect/Slope Means"),row=2,column=0)
	tkgrid(ChnMeanscb,row=2,column=1)
	tkgrid(tklabel(frameQuantiles,text="Covariance Matrices"),row=3,column=0)
	tkgrid(QntCovcb,row=3,column=1)
	tkgrid(tklabel(frameQuantiles,text="Correlation Matrices"),row=4,column=0)
	tkgrid(QntCorcb,row=4,column=1)

	# Quantiles frame	
	framePostMeans<-tkframe(frameOuter,relief="groove",borderwidth=2)
	
	PmnEffcb <- tkcheckbutton(framePostMeans)
	PmnEffcb.val=tclVar("1")
	tkconfigure(PmnEffcb,variable=PmnEffcb.val)
	PmnMeanscb <- tkcheckbutton(framePostMeans)
	PmnMeanscb.val=tclVar("1")
	tkconfigure(PmnMeanscb,variable=PmnMeanscb.val)
	PmnCovcb <- tkcheckbutton(framePostMeans)
	PmnCovcb.val=tclVar("1")
	tkconfigure(PmnCovcb,variable=PmnCovcb.val)
	PmnCorcb <- tkcheckbutton(framePostMeans)
	PmnCorcb.val=tclVar("1")
	tkconfigure(PmnCorcb,variable=PmnCorcb.val)

	tkgrid(tklabel(framePostMeans,text="Posterior Means"),row=0,column=0)
	tkgrid(tklabel(framePostMeans,text="Random Effects/Slopes"),row=1,column=0)
	tkgrid(PmnEffcb,row=1,column=1)
	tkgrid(tklabel(framePostMeans,text="Effect/Slope Means"),row=2,column=0)
	tkgrid(PmnMeanscb,row=2,column=1)
	tkgrid(tklabel(framePostMeans,text="Covariance Matrices"),row=3,column=0)
	tkgrid(PmnCovcb,row=3,column=1)
	tkgrid(tklabel(framePostMeans,text="Correlation Matrices"),row=4,column=0)
	tkgrid(PmnCorcb,row=4,column=1)

	# Misc1 Frame
	frameMisc1<-tkframe(frameOuter,relief="groove",borderwidth=2)
	tkgrid(tklabel(frameMisc1,text="Miscellaneous settings"),row=0,column=1)
	
	outDatacb <- tkcheckbutton(frameMisc1)
	outDatacb.val=tclVar("1")
	tkconfigure(outDatacb,variable=outDatacb.val)
	
	tkgrid(tklabel(frameMisc1,text="Parsed Data"),row=1,column=0)
	tkgrid(outDatacb,row=1,column=1)

	outRobjcb <- tkcheckbutton(frameMisc1)
	outRobjcb.val=tclVar("1")
	tkconfigure(outRobjcb,variable=outRobjcb.val)
	
	tkgrid(tklabel(frameMisc1,text="Full R object"),row=2,column=0)
	tkgrid(outRobjcb,row=2,column=1)

	VertSep1=ttkseparator(frameMisc1,orient="vertical")
	tkgrid(VertSep1,column=2,row=1,rowspan=2)
	tkgrid.configure(VertSep1,sticky="ns")

	
	outMCMCPDFcb <- tkcheckbutton(frameMisc1)
	outMCMCPDFcb.val=tclVar("1")
	tkconfigure(outMCMCPDFcb,variable=outMCMCPDFcb.val)
	
	tkgrid(tklabel(frameMisc1,text="Chains PDF"),row=1,column=3)
	tkgrid(outMCMCPDFcb,row=1,column=4)

	outACFPDFcb <- tkcheckbutton(frameMisc1)
	outACFPDFcb.val=tclVar("1")
	tkconfigure(outACFPDFcb,variable=outACFPDFcb.val)
	
	tkgrid(tklabel(frameMisc1,text="Autocorrelation PDF"),row=2,column=3)
	tkgrid(outACFPDFcb,row=2,column=4)

	VertSep2=ttkseparator(frameMisc1,orient="vertical")
	tkgrid(VertSep2,column=5,row=1,rowspan=2)
	tkgrid.configure(VertSep2,sticky="ns")

	outCovLvlscb <- tkcheckbutton(frameMisc1)
	outCovLvlscb.val=tclVar("1")
	tkconfigure(outCovLvlscb,variable=outCovLvlscb.val)
	
	tkgrid(tklabel(frameMisc1,text="Covariance Levels"),row=1,column=6)
	tkgrid(outCovLvlscb,row=1,column=7)

	outPredcb <- tkcheckbutton(frameMisc1)
	outPredcb.val=tclVar("0")
	tkconfigure(outPredcb,variable=outPredcb.val)
	
	tkgrid(tklabel(frameMisc1,text="Predicted probabilities*"),row=2,column=6)
	tkgrid(tklabel(frameMisc1,text="*(Warning: high memory usage)"),row=3,column=6)
	tkgrid(outPredcb,row=2,column=7)

	
	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Output Options Help",message=WMCapacityHelp()$OutputHelp,icon="info",type="ok")

	OnDone=function() tclvalue(done)<-1	
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	#Put all frames up
	tkgrid(frameOuter)
	tkgrid(frameOutput,row=1,column=0,columnspan=3)
	tkgrid(frameChains,row=2,column=0,columnspan=1)
	tkgrid(frameQuantiles,row=2,column=1,columnspan=1)
	tkgrid(framePostMeans,row=2,column=2,columnspan=1)
	tkgrid(frameMisc1,row=3,column=0,columnspan=3)

	tkgrid(frameBottom,row=4,column=0,columnspan=2)

	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal=list(doOutput=tclvalue(Outputcb.val),
								ChnEff=tclvalue(ChnEffcb.val),
								ChnMeans=tclvalue(ChnMeanscb.val),
								ChnCov=tclvalue(ChnCovcb.val),
								ChnCor=tclvalue(ChnCorcb.val),
								QntEff=tclvalue(QntEffcb.val),
								QntMeans=tclvalue(QntMeanscb.val),
								QntCov=tclvalue(QntCovcb.val),
								QntCor=tclvalue(QntCorcb.val),
								PmnEff=tclvalue(PmnEffcb.val),
								PmnMeans=tclvalue(PmnMeanscb.val),
								PmnCov=tclvalue(PmnCovcb.val),
								PmnCor=tclvalue(PmnCorcb.val),

								data=tclvalue(outDatacb.val),
								Robj=tclvalue(outRobjcb.val),
								MCMCPDF=tclvalue(outMCMCPDFcb.val),
								ACFPDF=tclvalue(outACFPDFcb.val),
								CovLvls=tclvalue(outCovLvlscb.val),
								Pred=tclvalue(outPredcb.val)
	)
	if(doneVal==2) retVal=list(0)	

	return(retVal)

}




WorkingMemGeneric <- function(){	
	thisEnv=environment()
	
	tt<-tktoplevel()
	done <- tclVar(0) # Are we done yet?
	tkgrab.set(tt)
	tktitle(tt) = "Generic Window"
	frameOuter <- tkframe(tt)
	tkgrid(tklabel(frameOuter,text="Generic Window Title",font=WMCapacityFonts()$fontHeading),row=0,columnspan=3)


	# Bottom Frame	
	frameBottom <- tkframe(frameOuter,relief="groove",borderwidth=2)

	OnHelp=function() tkmessageBox(title="Generic Help",message="Generic Help Content",icon="info",type="ok")

	OnDone=function() tclvalue(done)<-1	
	OnCancel=function() tclvalue(done)<-2
	Help.but <- tkbutton(frameBottom,text="      Help     ",command=OnHelp)
	Cancel.but <- tkbutton(frameBottom,text="      Cancel      ",command=OnCancel)
	Done.but <- tkbutton(frameBottom,text="      Next      ",command=OnDone)
	tkgrid(Help.but,column=0,row=0)
	tkgrid(Cancel.but,column=1,row=0)
	tkgrid(Done.but,column=2,row=0)
	tkgrid.configure(Done.but,sticky="w")

	tkbind(tt,"<Destroy>",OnCancel)
	tkbind(tt,"<Escape>",OnCancel)
	tkbind(tt,"<Return>",OnDone)

	#Put all frames up
	tkgrid(frameOuter)

	tkgrid(frameBottom,row=2,column=0,columnspan=2)

	tkwait.variable(done)

	doneVal <- as.integer(tclvalue(done))
      	tkgrab.release(tt)
	tkdestroy(tt)
	
	if(doneVal==1) retVal="OK"
	if(doneVal==2) retVal=list()	

	return(retVal)

}



