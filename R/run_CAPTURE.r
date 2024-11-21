#' @title Mark-recapture estimates from CAPTURE
#' 
#' @description 
#' Obtain mark-recapture estimates from the CAPTURE program for a given set of 
#' capture histories
#' 
#' @param ch Data frame or 2D array with binomial capture histories
#' @param keep.out If TRUE, copy capture.out to working directory
#' 
#' @details
#' This function runs the CAPTURE program (a compiled executable written in 
#' FORTRAN) to estimate abundance from the provided set of capture histories, 
#' which code individual animals (rows) as sighted (1) or not (0) in each occasion 
#' (cols). The CAPTURE program (Rexstad and Burnham 1992; White et al., 1978) was 
#' developed as a companion to Otis et al.'s (1978) mark-recapture monograph. 
#' 
#' This function, modified from code written by Jay Barlow (Calambokidis and 
#' Barlow 2020), currently only runs the Chao Mth (Chao et al., 1992) and 
#' Darroch Mt models, but can easily be modified to run other estimators available 
#' in CAPTURE (see program manual in the inst/capture directory of this package). 
#' 
#' @return list of estimates, standard errors, and capture probabilities
#' 
#' @references 
#' Calambokidis, J., and J. Barlow. 2020. Updated abundance estimates for blue 
#' and humpback whales along the U.S. West Coast using data through 2018. U.S. 
#' Department of Commerce, NOAA Technical Memorandum NMFS-SWFSC-634.
#' 
#' Chao, A., S.-M. Lee, and S.L. Jeng. 1992 Estimating population size for 
#' capture-recapture data when capture probabilities vary by time and individual 
#' animal. Biometrics 48: 201-216.
#' 
#' Otis, D. L., K. P. Burnham, G. C. White, and D. R. Anderson. 1978. Statistical 
#' inference from capture data on closed animal populations. Wildlife Monographs 
#' 62. 135 pp. 
#' 
#' Rexstad, E., and K.P. Burnham. 1992. Users Guide for Interactive Program CAPTURE.
#' Colorado Cooperative Fish & Wildlife Research Unit, Colorado State University, 
#' Fort Collins, Colorado. 
#' 
#' White, G.C., K.P. Burnham, D.L. Otis, and D.R. Anderson. 1978. Users Manual for 
#' Program CAPTURE, Utah State Univ. Press, Logan, Utah. 
#'
#' @export
run_CAPTURE <- function(ch, keep.out=TRUE){
  
  #substitute zero for missing values in capture histories
  for (i in 1:length(ch)) {ch[[i]][is.na(ch[[i]])]= 0}
  
  ch <- data.frame(ch)
  nocc <- ncol(ch)   # number of occasions
  numcaps <- as.numeric(colSums(ch))
  
  # create temporary working directory
  wd <- getwd()
  td <- tempdir()
  setwd(td)
  if (!(file.exists(paste0(td,"/CAPTURE.EXE"))))
    utils::unzip(zipfile=system.file("capture", "CAPTURE.zip", package = "CMRutils"), 
                 files = "CAPTURE.EXE", exdir = td)
  
  # Create input file for CAPTURE
  filename <- "capture.inp"
  zz <- file(filename,"w")      #specify output file name
  cat("title='Output from run_CAPTURE'\n",file=zz)
  cat(paste("task read captures occasions=",nocc," x matrix","\n",sep=""),file=zz)
  cat(paste("format='(a4,1x,10f1.0)'","\n",sep=""),file=zz)
  cat(paste("read input data","\n"),file=zz)
  for (j in 1:nrow(ch)) {
    cat(1000+j," ",as.numeric(ch[j,]),"\n",sep="",file=zz)
  }
  cat("task population estimate Mth-Chao","\n",file=zz)
  cat("task population estimate Darroch","\n",file=zz)
  # cat("task population estimate all","\n",file=zz)
  # cat("task model selection","\n",file=zz)
  close(zz)
  
  # create and run temporary batch file capture.bat
  fileConn<-file("capture.bat")
  writeLines("capture i=capture.inp o=capture.out", fileConn)
  close(fileConn)
  shell.exec("capture.bat")
  Sys.sleep(1)
  file.remove("capture.bat")
  
  # extract population estimates and capture probabilities from output file
  outputLines <- readLines("capture.out")   # outfile
  for (j in 1:length(outputLines)) {
    line <- outputLines[j]
    if (substr(line,2,23) == "Population estimate is") {
      N.DarrochMt <- as.numeric(substr(line,24,34))
      seN.DarrochMt <- as.numeric(substr(line,55,65))
    } else if (substr(line,2,38) == "Bias-corrected population estimate is") {
      N.ChaoMth <- as.numeric(substr(line,39,49))
      seN.ChaoMth <- as.numeric(substr(line,70,79))
    }
    
    if(j==48) p.ChaoMth <- as.numeric(unlist(stringr::str_extract_all(line, "[0-9]+[.]?[0-9]*")))
    if(j==70) p.DarrochMt <- as.numeric(unlist(stringr::str_extract_all(line, "[0-9]+[.]?[0-9]*")))
    
  }
  
  setwd(wd)
  
  # if keep.out=TRUE, copy capture.out to original working directory 
  if (keep.out)
    file.copy(from=paste0(td,"/capture.out"), to=wd, overwrite=TRUE)
  
  return(list(nocc=nocc, numcaps=numcaps,
              N.DarrochMt=N.DarrochMt, seN.DarrochMt=seN.DarrochMt, p.DarrochMt=p.DarrochMt, 
              N.ChaoMth=N.ChaoMth, seN.ChaoMth=seN.ChaoMth, p.ChaoMth=p.ChaoMth))
}


  