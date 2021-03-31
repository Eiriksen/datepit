#' @title datepit_to_ID
#' @description Function for obtaining the ID of an individual using columns date and pit. It takes in two tables, one to fill in (tb) and one to look up pit-tags from (tb_pit)
#' @param tb table to put ID's into (and get pit tags and dates from) must have columns "date" and "pit". Date should be formated as date-month-year or as a date value. The function's output will be this table plus a new "ID" column.
#' @param tb_pit a table that contains all known pit tags, their date of registration, and their corresponding ID. Date should be formated as data-month-year or as a date value.
#' @examples tb_fish_withID <- datepit_to_ID(tb_fish,tb_pit)
#' @examples tb_fish_withID <- tb_fish %>% datepit_to_ID(tb_pit)
#' @examples tb_fish_withID <- tb_fish %>% datepit_to-ID(read.table("pit date reference.txt",head=T))
#' @export
datepit_to_ID = function(tb, tb_pit){
  options(warn=2)
  require(lubridate)

  # check that pit and date columns exist in the table
  if (!"date" %in% colnames(tb)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb)) stop("pit column missing from tb")
  if (!"date" %in% colnames(tb_pit)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb_pit)) stop("pit column missing from tb")

  # save the old date column in the tb before messing around with it
  .oldDate <- tb$date

  # format dates, if necessary
  tb_pit$date <- ifelse( !is.Date(tb_pit$date), ymd(tb_pit$date), tb_pit$date)
  tb$date     <- ifelse( !is.Date(tb$date), ymd(tb$date), tb$date)

  # create the ID column in tb if it does not exist yet
  if (!"ID" %in% colnames(tb)) {
    tb$ID <- ""
  }

  # the algorithm:
  for (row in 1:nrow(tb))
  {
    # For every row in the table...
    # Get the pit tag and date of this row
    pit_r  <- tb[row,]$pit
    date_r <- tb[row,]$date
    # if any of those are NA, then ID is NA
    if (is.na(pit_r) | is.na(date_r)){
      tb[row,]$ID = NA
      next()
    }
    # get all the ID's that match this PIT tag and have a date
    # - that is registered at the same date or before
    # (excluding ID's that are registered AFTER this pit-tag is read)
    IDs <- tb_pit[tb_pit$pit==pit_r & tb_pit$date <= date_r,]
    if (nrow(IDs)==0){
      # If none found, ID is nA
      tb[row,]$ID <- NA
    }
    else {
      # If some are found, then pick the ID that was registered last
      ID = IDs[IDs$date == max(IDs$date),]$ID %>% unlist()
      if(length(ID)!=1)
        tb[row,]$ID <- NA
      else
        tb[row,]$ID <- ID
    }
  }

  # put the old date column back and return the table
  tb$date <- .oldDate
  return(tb)
}

