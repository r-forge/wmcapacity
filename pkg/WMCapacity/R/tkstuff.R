
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

WorkingMemPriorSetup <- function(useA=TRUE,meanMuK=3,sdMuK=10,meanMuA=0,sdMuA=10,meanMuG=0,sdMuG=10,invGamma.a0=2,invGamma.b0=1,WishartDF=2,slopeSD=100,minWishDF=0){	
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

WorkingMemMCMCSetup <- function(optimMaxit=200,nIters=1000,burnin=200,testrun=FALSE,hybridEpsilon=c(.015,.025),hybridLFsteps=80,hybridMultipoint=FALSE,hybridMultipointSize=1,hybridWeight=FALSE,hybridMPWeight=TRUE,progress=10,MetHastScale=.2,useMH="0",MetHastThin=1){	
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



