# Some Rcmdr dialogs for the orloca package

# last modified: 18 Sep 2008 by Fern�ndez-Palac�n&Manuel-Mu�oz


.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }

###
# New loca.p object as data.frame
###
Rcmdr.new.loca.p <- function(){
require(orloca)
initializeDialog(title=gettextRcmdr("New loca.p..."))
nameVar <- tclVar(gettextRcmdr("Data"))
nameEntry <- tkentry(top, width="8", textvariable=nameVar)
onOK <- function(){
	closeDialog()
        name <- tclvalue(nameVar)
        if (!is.valid.name(name)) {
            errorCondition(recall=Rcmdr.new.loca.p, message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
          }
        if (is.element(name, listDataSets()))
          {
          if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Data set"))))
            {
              errorCondition(recall=Rcmdr.new.loca.p, message="Introduce the name (another) for the new data.frame.")
              return()
             }
          }
###
        command <- "edit(data.frame(x=numeric(0), y=numeric(0), w=numeric(0)))"
        assign(name, justDoIt(command), envir=.GlobalEnv)
        logger(paste(name, "<-", command))
        if (nrow(get(name)) == 0){
            errorCondition(recall=Rcmdr.new.loca.p, message=gettextRcmdr("empty data set."))
            return()
            }
###
        activeDataSet(name)
        closeDialog()
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject="loca.p")
tkgrid(tklabel(top, text="Name of new loca.p object"), nameEntry, sticky="e")
tkgrid(buttonsFrame, sticky="w", columnspan=2)
tkgrid.configure(nameEntry, sticky="w")
dialogSuffix(rows=2, columns=2, focus=nameEntry)
}
###
# End of Rcmdr.new.loca.p
###

Rcmdr.rloca.p <- function(){
require(orloca)
initializeDialog(title=gettextRcmdr("New loca.p Random Instance"))
nameVar <- tclVar(gettextRcmdr("Data"))
nameEntry <- tkentry(top, width="8", textvariable=nameVar)
nVar <- tclVar("100")
nEntry <- tkentry(top, width="8", textvariable=nVar)
xminVar <- tclVar("0")
xminEntry <- tkentry(top, width="8", textvariable=xminVar)
xmaxVar <- tclVar("1")
xmaxEntry <- tkentry(top, width="8", textvariable=xmaxVar)
yminVar <- tclVar("0")
yminEntry <- tkentry(top, width="8", textvariable=yminVar)
ymaxVar <- tclVar("1")
ymaxEntry <- tkentry(top, width="8", textvariable=ymaxVar)
groupsVar <- tclVar("1")
groupsEntry <- tkentry(top, width="8", textvariable=groupsVar)
onOK <- function(){
	closeDialog()
        name <- tclvalue(nameVar)
        if (!is.valid.name(name)) {
            errorCondition(recall=Rcmdr.rloca.p, message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
          }
        if (is.element(name, listDataSets()))
          {
          if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Data set"))))
            {
              errorCondition(recall=Rcmdr.rloca.p, message="Introduce the name (another) for the new data.frame.")
              return()
             }
          }
	n <- round(as.numeric(tclvalue(nVar)))
        if (is.na(n) || n <= 0){
            errorCondition(recall=Rcmdr.rloca.p, message="The number of demand points must be a positive integer.")
            return()
            }
	xmin <- as.numeric(tclvalue(xminVar))
        if (is.na(xmin)){
            errorCondition(recall=Rcmdr.rloca.p, message="xmin must be a real number.")
            return()
            }
	xmax <- as.numeric(tclvalue(xmaxVar))
        if (is.na(xmax) || xmax < xmin){
            errorCondition(recall=Rcmdr.rloca.p, message="xmax must be a real number bigger that xmin.")
            return()
            }
	ymin <- as.numeric(tclvalue(yminVar))
        if (is.na(ymin)){
            errorCondition(recall=Rcmdr.rloca.p, message="ymin must be a real number.")
            return()
            }
	ymax <- as.numeric(tclvalue(ymaxVar))
        if (is.na(ymax) || ymax < ymin){
            errorCondition(recall=Rcmdr.rloca.p, message="ymax must be a real number bigger that ymin.")
            return()
            }
	groups <- round(as.numeric(tclvalue(groupsVar)))
        if (is.na(groups) || groups <= 0){
            errorCondition(recall=Rcmdr.rloca.p, message="groups must be a real number bigger that ymin.")
            return()
            }
	command <- paste(name, " <- rloca.p(n = ", n,", xmin = ", xmin,", xmax = ", xmax,", ymin = ", ymin,", ymax = ", ymax,", groups = ", groups,")", sep="")
	doItAndPrint(command)
#        command <- paste("summary(", name, ")")
#        doItAndPrint(command)
	command <- paste(name, " <- as(", name, ", \"data.frame\")")
        doItAndPrint(command)
        activeDataSet(name)
	tkfocus(CommanderWindow())
#falta incluir un cuadro de diálogo para guardar loca.p y convertirlo en fichero activo
	}
OKCancelHelp(helpSubject="rloca.p")
tkgrid(tklabel(top, text="Name of new loca.p object"), nameEntry, sticky="e")
tkgrid(tklabel(top, text="Number of demand points"), nEntry, sticky="e")
tkgrid(tklabel(top, text="x Minimum"), xminEntry, sticky="e")
tkgrid(tklabel(top, text="x Maximum"), xmaxEntry, sticky="e")
tkgrid(tklabel(top, text="y Minimum"), yminEntry, sticky="e")
tkgrid(tklabel(top, text="y Maximum"), ymaxEntry, sticky="e")
tkgrid(tklabel(top, text="Number of groups"), groupsEntry, sticky="e")
tkgrid(buttonsFrame, sticky="w", columnspan=2)
tkgrid.configure(nameEntry, sticky="w")
tkgrid.configure(nEntry, sticky="w")
tkgrid.configure(xminEntry, sticky="w")
tkgrid.configure(xmaxEntry, sticky="w")
tkgrid.configure(yminEntry, sticky="w")
tkgrid.configure(ymaxEntry, sticky="w")
tkgrid.configure(groupsEntry, sticky="w")
dialogSuffix(rows=7, columns=2, focus=nEntry)
}


Rcmdr.zsum <- function(){
require(orloca)
#.activeDataSet <- ActiveDataSet()
#o <- as(.activeDataSet, "loca.p"); habra que comprobar si se puede convertir
initializeDialog(title=gettextRcmdr("Evaluation Objective Function for mini-sum Location Problem"))
xVar <- tclVar("0")
xEntry <- tkentry(top, width="6", textvariable=xVar)
yVar <- tclVar("0")
yEntry <- tkentry(top, width="6", textvariable=yVar)
onOK <- function(){
	closeDialog()
	x <- as.numeric(tclvalue(xVar))
        if (is.na(x)){
            errorCondition(recall=Rcmdr.zsum, message="x-axis must be a number.")
            return()
            }
	y <- as.numeric(tclvalue(yVar))
        if (is.na(y)){
            errorCondition(recall=Rcmdr.zsum, message="y-axis must be a number.")
            return()
            }
        command <- paste("zsum(as(", ActiveDataSet(), ", \"loca.p\") , x = ", x,", y = ", y,")", sep="")
	doItAndPrint(command)
        command <- paste("zsumgra(as(", ActiveDataSet(), ", \"loca.p\") , x = ", x,", y = ", y,")", sep="")
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject="zsum")
tkgrid(tklabel(top, text="x-axis"), xEntry, sticky="e")
tkgrid(tklabel(top, text="y-axis"), yEntry, sticky="e")
tkgrid(buttonsFrame, sticky="w", columnspan=2)
tkgrid.configure(xEntry, sticky="w")
tkgrid.configure(yEntry, sticky="w")
dialogSuffix(rows=2, columns=2, focus=xEntry)
}

Rcmdr.zsummin <- function(){
require(orloca)
#.activeDataSet <- ActiveDataSet()
#o <- as(.activeDataSet, "loca.p"); habra que comprobar si se puede convertir
initializeDialog(title=gettextRcmdr("Solve mini-sum Location Problem"))
xVar <- tclVar("0")
xEntry <- tkentry(top, width="6", textvariable=xVar)
yVar <- tclVar("0")
yEntry <- tkentry(top, width="6", textvariable=yVar)
nVar <- tclVar("100")
nEntry <- tkentry(top, width="6", textvariable=nVar)
epsVar <- tclVar("0.001")
epsEntry <- tkentry(top, width="6", textvariable=epsVar)
radioButtons(name="algorithm", buttons=c("w", "g", "s"), values=c("w", "g", "s"), initialValue="w", labels=gettextRcmdr(c("Weiszfeld", "Gradient", "Search method")), title=gettextRcmdr("Algorithm"))

#tkgrid(labelRcmdr(statisticFrame), sticky="w")
onOK <- function(){
	closeDialog()
	x <- as.numeric(tclvalue(xVar))
        if (is.na(x)){
            errorCondition(recall=Rcmdr.zsummin, message="x-axis must be a number.")
            return()
            }
	y <- as.numeric(tclvalue(yVar))
        if (is.na(y)){
            errorCondition(recall=Rcmdr.zsummin, message="y-axis must be a number.")
            return()
            }
	n <- as.numeric(tclvalue(nVar))
        if (is.na(n) || n <= 0){
            errorCondition(recall=Rcmdr.zsummin, message="The maximum number of iterations must be a positive integer")
            return()
            }
	eps <- as.numeric(tclvalue(epsVar))
        if (is.na(eps) || eps <= 0){
            errorCondition(recall=Rcmdr.zsummin, message="The norm of the gradient must be a positive integer")
            return()
            }
        command <- paste(".sol <- zsummin(as(", ActiveDataSet(), ", \"loca.p\") , x = ", x,", y = ", y,", eps =", eps,")", sep="")
	doItAndPrint(command)
        doItAndPrint(".sol")
        command <- paste("zsum(as(", ActiveDataSet(), ", \"loca.p\") , x =", .sol[1], ", y = ", .sol[2], ")")
        doItAndPrint(command)
        doItAndPrint("remove(.sol)")
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject="zsummin")
tkgrid(tklabel(top, text="Iter. Max."), nEntry, sticky="e")
tkgrid(tklabel(top, text="x-axis"), xEntry, sticky="e")
tkgrid(tklabel(top, text="y-axis"), yEntry, sticky="e")
tkgrid(tklabel(top, text="Grad. norm."), epsEntry, sticky="e")
tkgrid(buttonsFrame, sticky="w", columnspan=2)
tkgrid.configure(nEntry, sticky="w")
tkgrid.configure(xEntry, sticky="w")
tkgrid.configure(yEntry, sticky="w")
tkgrid.configure(epsEntry, sticky="w")
dialogSuffix(rows=4, columns=2, focus=xEntry)
}


Rcmdr.plot.loca.p <- function(){
   require(orloca)
   command <- paste("plot.loca.p(as(", ActiveDataSet(), ", \"loca.p\"), main= \"Plot of demand points of", ActiveDataSet(), "\")")
   doItAndPrint(command)
   invisible(NULL)
}

Rcmdr.contour.zsum <- function(){
   require(orloca)
   command <- paste("contour(as(", ActiveDataSet(), ",\"loca.p\"), main=\"Contour Level plot of min-sum objective for", ActiveDataSet(), "\")")
   doItAndPrint(command)
   invisible(NULL)
}


Rcmdr.persp.zsum <- function(){
   require(orloca)
   command <- paste("persp(as(", ActiveDataSet(), ", \"loca.p\"), main=\"3D plot of min-sum objective for", ActiveDataSet(), "\")")
   doItAndPrint(command)
   invisible(NULL)
}


Rcmdr.plot.contour.loca.p <- function(){
   require(orloca)
   command <- paste("plot.loca.p(as(", ActiveDataSet(), ", \"loca.p\"), main= \"Plot of demand points and contour plot of", ActiveDataSet(), "\")")
   doItAndPrint(command)
   command <- paste("contour(as(", ActiveDataSet(), ",\"loca.p\"), add=T)")
   doItAndPrint(command)
   invisible(NULL)
}


Rcmdr.help.orloca <- function(){
   require(orloca)
   doItAndPrint("help(\"orloca\")")
   invisible(NULL)
}

Rcmdr.help.RcmdrPlugin.orloca <- function(){
   require(orloca)
   doItAndPrint("help(\"RcmdrPlugin.orloca\")")
   invisible(NULL)
}

Rcmdr.summary.loca.p <- function(){
   require(orloca)
   command <- paste("summary(as(", ActiveDataSet(), ", \"loca.p\"))")
   doItAndPrint(command)
   invisible(NULL)
}

activeDataSetLocaP <- function() activeDataSetP() && validObject(new("loca.p",x=get(ActiveDataSet())$x, y=get(ActiveDataSet())$y, w=get(ActiveDataSet())$w))

activeDataSetLocaP <- function()
  {
    if (activeDataSetP())
      {
      .activeDataSet <- get(ActiveDataSet())
      (nrow(.activeDataSet)==length(.activeDataSet$x)) && (nrow(.activeDataSet)==length(.activeDataSet$y)) && (nrow(.activeDataSet)==length(.activeDataSet$w)) && (sum(is.na(.activeDataSet$x))+sum(is.na(.activeDataSet$y)+sum(is.na(.activeDataSet$w)))==0)
      }
    else FALSE
  }
