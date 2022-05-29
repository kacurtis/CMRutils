# import data from MN ID database

import_mnid <- function(datdir, mnidfile) {

  # import MN ID database
  # 
  # Arguments
  # datdir    directory path for data files
  # mnidfile  file path for MN ID database file
  # 
  # Returns
  # list with sightings, loccode, and biopsy tables

  # load library
  library(DBI)
  
  # get connection to MN ID db
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  dbq_string <- paste0("DBQ=",datdir,mnidfile)
  db_connect_string <- paste0(driver_string, dbq_string)
  myconn <- dbConnect(odbc::odbc(), .connection_string = db_connect_string)
  # fetch data
  sight <- dbReadTable(myconn, "MN ID Sightings")
  loccode <- dbReadTable(myconn, "Loc Code")
  biop <- dbReadTable(myconn, "Mn ID Samples")
  # disconnect
  dbDisconnect(myconn)
  rm(driver_string, dbq_string, db_connect_string, myconn)
  detach("package:DBI")
  
  return(list(sight=sight, loccode=loccode, biop=biop))
}
