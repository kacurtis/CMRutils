# import data from CRC Sighting databases

import_effort <- function(datdir, effyrs = NULL) {

  # import CRC Sightings databases
  # Note: function assumes effort data files ('CRC Sighting Data YYYY.mdb') all stored in one directory
  # 
  # Arguments
  # datdir    directory path for data files
  # effyrs    years of interest for effort
  # 
  # Returns
  # list with effort, effort events, and event code tables

  # load library
  library(DBI)
  library(dplyr)
  
  if (is.null(effyrs)) 
    stop("Must specify at least one year of interest for effort")
    
  eff <- data.frame(NULL)
  effev <- data.frame(NULL)
  evcode <- data.frame(NULL)
  
  for (y in effyrs) {
    # connect
    driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
    dbq_string <- paste0("DBQ=",datdir,"CRC Sighting Data ",y,".mdb")
    db_connect_string <- paste0(driver_string, dbq_string)
    myconn <- dbConnect(odbc::odbc(), .connection_string = db_connect_string)
    # fetch data
    tempeff <- dbReadTable(myconn, "tDailyEffort")
    tempev <- dbReadTable(myconn, "tDailyEvents")
    tempevcode <- dbReadTable(myconn, "ltEventCodes") %>%   # accepts variation in capitalization of table names in some years
      mutate(yy = y)
    # disconnect
    dbDisconnect(myconn)
    # add to data frames
    eff <- bind_rows(eff, tempeff)
    effev <- bind_rows(effev, tempev)
    evcode <- bind_rows(evcode, tempevcode)
  }
    
  detach("package:DBI")
  
  return(list(eff=eff, effev=effev, evcode=evcode))
}
