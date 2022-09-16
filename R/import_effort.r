#' @title Import effort data
#' 
#' @description 
#' Import data from CRC or SPLASH Sighting databases
#' 
#' @details 
#' Assumes effort data files (e.g., 'CRC Sighting Data YYYY.mdb') all stored in one directory
#' 
#' @param datdir Directory path for effort data files
#' @param filename Years for which to import effort
#' @param effyrs Years for which to import effort
#' 
#' @return list of data frames for with effort, effort events, and event codes
#' 
#' @export
import_effort <- function(datdir, filename = NULL, effyrs = NULL) {

  if (is.null(effyrs) & is.null(filename)) 
    stop("Must specify either a filename or at least one year for which to import effort")
    
  eff <- data.frame(NULL)
  effev <- data.frame(NULL)
  evcode <- data.frame(NULL)
  
  if (!is.null(effyrs)) {
    for (y in effyrs) {
      # connect
      driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
      dbq_string <- paste0("DBQ=", datdir, "CRC Sighting Data ", y, ".mdb")
      db_connect_string <- paste0(driver_string, dbq_string)
      myconn <- DBI::dbConnect(odbc::odbc(), .connection_string = db_connect_string)
      # fetch data
      tempeff <- DBI::dbReadTable(myconn, "tDailyEffort")
      tempev <- DBI::dbReadTable(myconn, "tDailyEvents")
      tempevcode <- DBI::dbReadTable(myconn, "ltEventCodes") %>%   # accepts variation in capitalization of table names in some years
        dplyr::mutate(yy = y)
      # disconnect
      DBI::dbDisconnect(myconn)
      # add to data frames
      eff <- dplyr::bind_rows(eff, tempeff)
      effev <- dplyr::bind_rows(effev, tempev)
      evcode <- dplyr::bind_rows(evcode, tempevcode)
    }
  } else {
    # connect
    driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
    dbq_string <- paste0("DBQ=", datdir, filename)
    db_connect_string <- paste0(driver_string, dbq_string)
    myconn <- DBI::dbConnect(odbc::odbc(), .connection_string = db_connect_string)
    # fetch data
    eff <- DBI::dbReadTable(myconn, "tDailyEffort")
    effev <- DBI::dbReadTable(myconn, "tDailyEvents")
    evcode <- DBI::dbReadTable(myconn, "ltEventCodes")   # accepts variation in capitalization of table names in some years
    # disconnect
    DBI::dbDisconnect(myconn)
  }
    
  return(list(eff=eff, effev=effev, evcode=evcode))
}
