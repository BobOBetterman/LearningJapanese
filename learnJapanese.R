library(tcltk)

setwd("C:/cygwin64/home/andrewHill/forFun/learnJapanese")

modalText <- function(title, question, entryWidth = 35, returnValonCancel = 0) {
    tBox <- tktoplevel()
    tkwm.deiconify(tBox)
    tkgrab.set(tBox)
    tkfocus(tBox)
    
    normalFont <- tkfont.create(family="times",size=14)
    
    tkwm.title(tBox, title)
    
    entryVal <- ""
    
    textEntryVarTcl <- tclVar(entryVal)
    textEntryWidget <- tkentry(tBox, width = entryWidth, textvariable = textEntryVarTcl, font = normalFont)
    
    tkgrid(tklabel(tBox, text = ""))
    tkgrid(tklabel(tBox, text = "     "), tklabel(tBox, text = question, font = normalFont), textEntryWidget, tklabel(tBox, text = "     "))
    tkgrid(tklabel(tBox, text = ""))
    
    ReturnVal <- returnValonCancel

    onOK <- function() {
        ReturnVal <<- tclvalue(textEntryVarTcl)
        tkgrab.release(tBox)
        tkdestroy(tBox)
    }
    
    onCancel <- function() {
        ReturnVal <<- returnValonCancel
        tkgrab.release(tBox)
        tkdestroy(tBox)
    }
    
    OK.but <- tkbutton(tBox, text = "    Ok    ", command = onOK, font = normalFont)
    Cancel.but <- tkbutton(tBox, text = "  Cancel  ", command = onCancel, font = normalFont)
    
    tkgrid(tklabel(tBox, text = "     "), OK.but, Cancel.but, tklabel(tBox, text = "     "))
    tkgrid(tklabel(tBox, text = ""))
    
    tkfocus(tBox)
    
    tkbind(tBox, "<Destroy>", function() {tkgrab.release(tBox)})
    
    tkbind(textEntryWidget, "<Return>", onOK)
    
    tkwait.window(tBox)
    
    return(ReturnVal)
}

modeSelect <- function() {
    tBox <- tktoplevel()
    tkwm.deiconify(tBox)
    tkgrab.set(tBox)
    tkfocus(tBox)
    
    tkwm.title(tBox, "Welcome")
    
    tkgrid(tklabel(tBox, text = "Hello."), columnspan = 2)
    tkgrid(tklabel(tBox, text = "Welcome to the amazing Japanese language tutor that you've created through your brilliance."), columnspan = 2)
    tkgrid(tklabel(tBox, text = "Please select a mode below."), columnspan = 2)
    tkgrid(tklabel(tBox, text = "Would you like to add to the database, or practice your skills?"), columnspan = 2)
    tkgrid(tklabel(tBox, text = ""), columnspan = 2)
    
    modeType <- -1
    
    onAdd <- function() {
        modeType <<- 0
        tkgrab.release(tBox)
        tkdestroy(tBox)
    }
    
    onReview <- function() {
        modeType <<- 1
        tkgrab.release(tBox)
        tkdestroy(tBox)
    }
    
    add.but <- tkbutton(tBox, text = "Expand Database", command = onAdd)
    review.but <- tkbutton(tBox, text = "Review Knowledge", command = onReview)
    
    tkgrid(add.but, review.but)
    
    tkgrid(tklabel(tBox, text = ""), columnspan = 2)
    
    tkfocus(tBox)
    
    tkbind(tBox, "<Destroy>", function() {tkgrab.release(tBox)})
    
    tkwait.window(tBox)
    
    return(modeType)
}

checkEntry <- function(japText, engText) {
    tBox <- tktoplevel()
    tkwm.deiconify(tBox)
    tkgrab.set(tBox)
    tkfocus(tBox)
    
    normalFont <- tkfont.create(family="times",size=14)
    checkFont <- tkfont.create(family="times",size=18)
    
    tkwm.title(tBox, "Checking...")
    
    tkgrid(tklabel(tBox, text = "               Just to be sure...               ", font = normalFont), columnspan = 2)
    tkgrid(tklabel(tBox, text = "Is this correct?", font = normalFont), columnspan = 2)
    tkgrid(tklabel(tBox, text = paste(japText, "     :     ", engText), font = checkFont), columnspan = 2)
    tkgrid(tklabel(tBox, text = ""), columnspan = 2)
    
    finalCheck <- 0
    
    onCorrect <- function() {
        finalCheck <<- 1
        tkgrab.release(tBox)
        tkdestroy(tBox)
    }
    
    onCancel <- function() {
        finalCheck <<- 0
        tkgrab.release(tBox)
        tkdestroy(tBox)
    }
    
    check.but <- tkbutton(tBox, text = "Correct", command = onCorrect, font = normalFont)
    cancel.but <- tkbutton(tBox, text = "Cancel", command = onCancel, font = normalFont)
    
    tkgrid(check.but, cancel.but)
    
    tkgrid(tklabel(tBox, text = ""), columnspan = 2)
    
    tkfocus(tBox)
    
    tkbind(tBox, "<Destroy>", function() {tkgrab.release(tBox)})
    
    tkwait.window(tBox)
    
    return(finalCheck)
}

exitProgram <- function() {
    tkmessageBox(message = "Thanks! Please come again soon!", title = "Goodbye")
}

addData <- function() {
    japText <- modalText("Japanese", "Please enter the Japanese term here: ")
    if(japText == 0) {
        exitProgram()
    } else {
        engText <- modalText("English", "Please enter the English term here: ")

        if(engText == 0) {exitProgram()}
        else {
    
            xPar <- parse(text = paste0("'", japText, "'"))
            japText <- xPar[[1]]

            finFlag <- checkEntry(japText, engText)

            if(finFlag == 0) {exitProgram()}
            else {
                fileCon <- file("data/japDict.txt", open = "at")
        
                writeLines(paste(japText, engText), fileCon, useBytes = TRUE)
        
                close(fileCon)
            }
        }
    }
}

mode <- modeSelect()

if(mode == -1) {exitProgram()}

if(mode == 0) {
    addData()
}

if(mode == 1) {
    fileCon <- file("data/japDict.txt", open = "rt")
    japDict <- readLines(fileCon, encoding = "UTF-8")
    close(fileCon)
}