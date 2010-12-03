# this stores application (non-project) state information
if (!exists("StateEnv", environment(), inherits=FALSE)) {
	StateEnv <- new.env()
}
# stores all project information
if (!exists("wommbatAnalysis", environment(), inherits=FALSE)) {
	wommbatAnalysis <- new.env()
}

wommbatGUI <- function(project = NULL, projectFile= NULL,CSVfile = NULL, dataFrame = NULL) {

	options("guiToolkit"="RGtk2")

	StateEnv$update <- list()
	wommbatAnalysis$Models <- list()
	
	StateEnv$echo.to.console <- TRUE
	StateEnv$echo.to.log <- TRUE
	StateEnv$Graphics <- list()
	
	#StateEnv$GUI <- gladeXMLNew(getpackagefile("wommbatGlade.glade"),
	#	root="window1")

	StateEnv$GUI <- gtkBuilder()
	filename <- getpackagefile("wommbatMain.ui")
	res <- StateEnv$GUI$addFromFile(filename)
	if (!is.null(res$error))
		stop("ERROR: ", res$error$message)
		
	StateEnv$win <- theWidget("window1")
	StateEnv$win$setTitle("WoMMBAT")
	
	# connect the callbacks (event handlers)
	#gladeXMLSignalAutoconnect(StateEnv$GUI)
	StateEnv$GUI$connectSignals(NULL)
	StateEnv$handlers = list()
	
	# Connect scrollbar signal
	scrollBar <- theWidget("hscrollbar1")
	adjustment <- gtkAdjustmentNew(value = 1, lower = 1, upper = 1, step.inc=1)
	scrollBar$setAdjustment(adjustment)
	gtkAdjustmentSetValue(scrollBar$getAdjustment(), 1)
	StateEnv$handlers$diagnosticScrollBar1 <- gSignalConnect(theWidget("hscrollbar1"), "value-changed", .scrolled_diagnostics_scrollbar)
	
	
	resultsSelection <- theWidget("resultsParEstTreeview")$getSelection()		
	gtkTreeSelectionSetMode(resultsSelection, "GTK_SELECTION_MULTIPLE")
	gSignalConnect(resultsSelection, "changed", .resultsSelectionChanged)

	# This should not be here. It is for testing purposes.
	itersSpace = theWidget('diagnosticItersComboSpace')
	typeSpace = theWidget('diagnosticTypeComboSpace')
	StateEnv$itersCombo = gtkComboBoxNewText()
	StateEnv$typeCombo = gtkComboBoxNewText()
	gtkComboBoxAppendText(StateEnv$itersCombo, "All")
	gtkComboBoxAppendText(StateEnv$typeCombo, "effects")
	itersSpace$packStart(StateEnv$itersCombo,FALSE,FALSE,0)
	typeSpace$packStart(StateEnv$typeCombo,FALSE,FALSE,0)
	gtkComboBoxSetActive(StateEnv$itersCombo,0)
	gtkComboBoxSetActive(StateEnv$typeCombo,0)
	#Connect chain limit signal
	StateEnv$handlers$diagnosticLimitCombo <- gSignalConnect(StateEnv$itersCombo, "changed", .selected_number_chain_iterations)
	# Connect parameter type signal
	StateEnv$handlers$diagnosticTypeCombo <- gSignalConnect(StateEnv$typeCombo, "changed", .selected_diagnostic_parameter_type)
	# Above should not be here. Move to own function!

	
	.womSetInitialSensitivity()
	
	# load files/dataframes
	if(!is.null(dataFrame)){
		if(is.data.frame(dataFrame)){
			theWidget("dataFilenameEntry")$setText("<Loaded from data.frame>")
			.womSetDataForColumnSelection(dataFrame)
		}
	}else if(!is.null(CSVfile))
	{
		.womOpenCSVFile(CSVfile)
	}else if(!is.null(project))
	{
		.womLoadProject(project)
	}else if(!is.null(projectFile))
	{
		.womLoadProjectFile(projectFile)
	}
	
	StateEnv$win$present()
	return(invisible(NULL))
}


wommbatNoGUI <- function(project=NULL, projectFile= NULL, settings)
{
	stop('This function is not yet implemented.')
	return(invisible(NULL))
}

