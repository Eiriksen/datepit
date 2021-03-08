#' Function for obtaining the ID of an individual using columns date and pit
#' @param tb table to put ID's into (and get pit tags and dates from) must have columns "date" and "pit". Date should be formated as date-month-year
#' @param tb_pit a table that contains all known pit tags, their date of registration, and their corresponding ID
#' @export
datepit_to_ID = function(tb, tb_pit){
  if (!"date" %in% colnames(tb)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb)) stop("pit column missing from tb")

  tb_pit$date <- ymd(tb_pit$date)
  tb$date     <- ymd(tb$date)
  if (!"ID" %in% colnames(tb)) {
    tb$ID <- ""
  }
  for (row in 1:nrow(tb)){
    pit_r <- tb[row,]$pit
    date_r <- tb[row,]$date
    if (is.na(pit_r) | is.na(date_r)){
      tb[row,]$ID = NA
      next()
    }
    IDs <- tb_pit[tb_pit$pit==pit_r & tb_pit$date <= date_r,]
    if (nrow(IDs)==0)
      tb[row,]$ID <- NA
    else {
      ID = IDs[IDs$date == max(IDs$date),]$ID %>% unlist()
      if(length(ID)!=1)
        tb[row,]$ID <- NA
      else
        tb[row,]$ID <- ID
    }
  }
  return(tb)
}
