\name{Rcmdr.Utilities}
\alias{Rcmdr.Utilities}
\alias{activeDataSet}
\alias{activeModel}
\alias{checkActiveDataSet}
\alias{checkActiveModel}
\alias{checkBoxes}
\alias{checkFactors}
\alias{checkNumeric}
\alias{checkReplace}
\alias{checkTwoLevelFactors}
\alias{checkVariables}
\alias{dialogSuffix}
\alias{doItAndPrint}
\alias{errorCondition}
\alias{getFrame}
\alias{getFrame.listbox}
\alias{getSelection}
\alias{getSelection.listbox}
\alias{groupsBox}
\alias{groupsLabel}
\alias{initializeDialog}
\alias{is.valid.name}
\alias{justDoIt}
\alias{listDataSets}
\alias{listGeneralizedLinearModels}
\alias{listFactors}
\alias{listLinearModels}
\alias{listNumeric}
\alias{listTwoLevelFactors}
\alias{listVariables}
\alias{logger}
\alias{modelFormula}
\alias{OKCancelHelp}
\alias{radioButtons}
\alias{subOKCancelHelp}
\alias{subsetBox}
\alias{variableListBox}

\title{Rcmdr Utility Functions}
\description{
  These functions support writing additions to the Rcmdr package. Additional R code
  can be placed in files with file type \code{.R} in the \code{etc} subdirectory
  of the package. Add menus, submenus, and menu items by editing the file
  \code{Rcmdr-menus.txt} in the same directory.
}

\usage{
activeDataSet(dsname, flushModel=TRUE)
activeModel(model)
checkActiveDataSet()
checkActiveModel()
checkBoxes(window=top, frame, boxes, initialValues=NULL, labels)  # macro
checkFactors(n=1)
checkNumeric(n=1)
checkReplace(name, type="Variable")
checkTwoLevelFactors(n=1)
checkVariables(n=1)
dialogSuffix(window=top, onOK=onOK, rows=1, columns=1, focus=top, bindReturn=TRUE, 
    preventGrabFocus=FALSE)  # macro
doItAndPrint(command, log=TRUE)
errorCondition(window=top, recall=NULL, message, model=FALSE)  # macro
\method{getFrame}{listbox}(object)
\method{getSelection}{listbox}(object)
groupsBox(recall=NULL, label="Plot by:", initialLabel="Plot by groups",
    plotLinesByGroup=FALSE, positionLegend=FALSE, 
    plotLinesByGroupsText="Plot lines by group")  # macro
groupsLabel(frame=top, groupsBox=groupsBox, columnspan=1)  # macro
justDoIt(command)
initializeDialog(window=top, title="")  # macro
is.valid.name(x)
listDataSets(envir = .GlobalEnv, ...)
listFactors(dataSet=.activeDataSet)
listGeneralizedLinearModels(envir = .GlobalEnv, ...)
listLinearModels(envir = .GlobalEnv, ...)
listNumeric{dataSet=.activeDataSet}
listTwoLevelFactors(dataSet=.activeDataSet)
listVariables(dataSet=.activeDataSet)
logger(command)
modelFormula(frame=top, hasLhs=TRUE)  # macro
OKCancelHelp(window=top, helpSubject=NULL, model=FALSE)  # macro
radioButtons(window=top, name, buttons, values=NULL, initialValue=..values[1], 
    labels, title)  # macro
subOKCancelHelp(window=subdialog, helpSubject=NULL)  # macro
subsetBox(window=top, model=FALSE)  # macro
variableListBox(parentWindow, variableList=.variables, bg="white",
    selectmode="single", export="FALSE", initialSelection=NULL, 
    listHeight = 4, title)
}

\arguments{
  \item{bg}{background color.}
  \item{bindReturn}{if \code{TRUE}, the \emph{Return} key is bound to the \code{onOK} function in the dialog.}
  \item{boxes}{vector of quoted names for check boxes, used to generate each box and its associated variable.}
  \item{buttons}{vector of quoted names for buttons in a set of related radio buttons.}
  \item{columnspan}{number of dialog-box columns to be spanned by frame.}
  \item{command}{a character string that evaluates to an R command.}
  \item{dataSet, dsname}{the quoted name of a data frame in memory.}
  \item{envir}{the environment to be searched; should generally be left at the default.}
  \item{export}{export selection?}
  \item{flushModel}{set (or reset) the active model to NULL? Should normally be \code{TRUE} when the active 
    data set is changed; an exception is when variables are simply added to, deleted from, or modified in 
    the data set set.}
  \item{focus}{Tk window to get the focus.}
  \item{frame}{frame or quoted name for frame depending upon the function.}
  \item{groupsBox}{listbox object for selecting groups variable.}
  \item{hasLhs}{does the model formula have a left-hand side?}
  \item{helpSubject}{the quoted name of a help subject, to be called as \code{help(helpSubject)}
    when the dialog \emph{Help} button is pressed.}
  \item{initialLabel}{label for groups button before a selection is made.}
  \item{initialSelection}{index of item initially selected, 0-base indexing.}
  \item{initialValue}{for a set of related radio buttons.}
  \item{initialValues}{for a set of related check boxes.}
  \item{label}{label prefix for groups button after a selection is made.}
  \item{labels}{a vector of character strings to label a set of radio buttons or check boxes.}
  \item{listHeight}{Maximum number of elements displayed simultaneously in list box.}
  \item{log}{echo command to the log window, as well as executing it and printing its output.}
  \item{message}{error message.}
  \item{model}{the name of a model, as a character string, or \code{TRUE} or \code{FALSE}, depending
    upon the function.}
  \item{name}{quoted name.}
  \item{n}{number of variables to check for.}
  \item{object}{a \code{listbox} object.}
  \item{onOK}{function to execute when the \emph{OK} button is pressed.}
  \item{plotLinesByGroup}{include a check box for plotting lines by group?}
  \item{plotLinesByGroupsText}{the label for the plot-lines-by-group check box.}
  \item{positionLegend}{include a check box for a legend?}
  \item{preventGrabFocus}{prevent the dialog box from grabbing the focus.}
  \item{recall}{function to call after error --- usually the function that initiates the dialog.}
  \item{rows, columns}{numbers of rows and columns of widgets in the dialog box.}
  \item{values}{vector of quoted values associated with radio buttons or check boxes.}
  \item{selectmode}{\code{"single"} or \code{"multiple"}.}
  \item{title}{Window or dialog-box-element title.}
  \item{type}{quoted type of object to check; used to generate check-replace dialog box.}
  \item{variableList}{a vector of variable names.}
  \item{window, parentWindow}{a Tk window.}
  \item{x}{a putative R object name, as a character string.}
  \item{\dots}{Disregard.}
}

\details{

  There are several groups of functions exported by the \code{Rcmdr} package and documented briefly here.
  To see how these functions work, it is simplest to examine the dialog-generating functions in the \code{Rcmdr} package.

  \emph{Executing and logging commands: }
  The functions \code{doItAndPrint}, \code{justDoIt}, and \code{logger} control the 
  execution, logging, and printing of commands generated by menus and dialogs. 
  \code{logger(command)} adds \code{command} to the log/script window and to the output window. 
  \code{justDoIt(command)} causes \code{command}
  to be executed. \code{doItAndPrint(command)} does both of these operations, and also prints the
  output produced by the command.
  
  \emph{Checking for errors: }
  The function \code{is.valid.name} checks whether a character string specifies a valid name
  for an R object. The functions \code{checkActiveDataSet}, \code{checkActiveModel},
  \code{checkFactors}, \code{checkNumeric}, \code{checkTwoLevelFactors}, and \code{checkVariables}
  check for the existence of objects and bring up an error-information dialog if they are absent
  (or insufficiently numerous, in the case of different kinds of variables). The function 
  \code{checkReplace} opens a dialog to query whether an existing object should be replaced.
  The function \code{errorCondition} reports an error to the user and (optionally) re-starts
  a dialog.
  
  \emph{Information: }
  Several functions return vectors of object names: \code{listDataSets}, \code{listGeneralizedLinearModels}, 
  \code{listFactors}, \code{listLinearModels}, \code{listNumeric}, \code{listTwoLevelFactors}, 
  \code{listVariables}. The functions \code{activeDataSet} and \code{activeModel} respectively report or set the active
  data set and model.
  
  \emph{Building dialog boxes: }
  Several functions simplify the process of constructing Tk dialogs: 
  initializing a dialog box, \code{initializeDialog}, and completing the definition of a dialog box, \code{dialogSuffix};
  a set of check boxes, \code{checkBoxes};
  a set of radio buttons, \code{radioButtons};
  a list box with associated scrollbars and state variable, \code{variableListBox}
  (and the associated functions \code{getFrame} and \code{getSelection});
  a button and subdialog for selecting a "grouping" variable, \code{groupsBox}; 
  displaying the currently defined groups in a dialog, \code{groupsLabel};
  a dialog-box structure for entering a model formula, \code{modelFormula};
  a text box for entering a subsetting expression, \code{subsetBox};
  \emph{OK}, \emph{Cancel}, and \emph{Help} buttons for dialogs, \code{OKCancelHelp}, and subdialogs, \code{subOKCancelHelp}.
  
  Some of these functions, marked \code{# macro} under \emph{Usage}, are "macro-like" in their behaviour, in that they 
  execute in the environment from which they are called. These were defined with an adaptation (used with permission) of
  Thomas Lumley's \code{defmacro} function, described in Lumley (2001).
  
}

\references{
    T. Lumley (2001) Programmer's niche: Macros in R.
    \emph{R News}, \bold{1(3)}, 11--13.
    }

\author{John Fox \email{jfox@mcmaster.ca}}

\keyword{misc}
