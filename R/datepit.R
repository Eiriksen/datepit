#' @title datepit_to_ID
#' @description Function for obtaining the ID of an individual using columns date and pit. It takes in two tables, one to fill in (tb) and one to look up pit-tags from (tb_pit)
#' @param tb table to put ID's into (and get pit tags and dates from) must have columns "date" and "pit". Date should be formated as date-month-year or as a date value. The function's output will be this table plus a new "ID" column.
#' @param tb_pit a table that contains all known pit tags, their date of registration, and their corresponding ID. Date should be formated as data-month-year or as a date value.
#' @examples tb_fish_withID <- datepit_to_ID(tb_fish,tb_pit)
#' @examples tb_fish_withID <- tb_fish %>% datepit_to_ID(tb_pit)
#' @examples tb_fish_withID <- tb_fish %>% datepit_to-ID(read.table("pit date reference.txt",head=T))
#' @export
datepit_to_ID = function(tb, tb_pit){
  require(lubridate)

  # check that pit and date columns exist in the table
  if (!"date" %in% colnames(tb)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb)) stop("pit column missing from tb")
  if (!"date" %in% colnames(tb_pit)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb_pit)) stop("pit column missing from tb")

  # save the old date column in the tb before messing around with it
  .oldDate <- tb$date

  # format dates, if necessary
  tb_pit$date <- ymd(tb_pit$date)
  tb$date     <- ymd(tb$date)


  # the algorithm
  tb$ID <- apply(tb,MARGIN = 1,FUN=function(row){
    # For every row in the table...
    # Get the pit tag and date of this row
    rowlist <- as.list(row)
    pit_r  <- rowlist$pit
    date_r <- rowlist$date
    # if any of those are NA, then ID is NA
    if (is.na(pit_r) | is.na(date_r)){
      return(NA)
    }
    # get all the ID's that match this PIT tag and have a date
    # - that is registered at the same date or before
    # (excluding ID's that are registered AFTER this pit-tag is read)
    IDs <- tb_pit[tb_pit$pit==pit_r & tb_pit$date <= date_r,]
    if (nrow(IDs)==0){
      # If none found, ID is nA
      return(NA)
    }
    else {
      # If some are found, then pick the ID that was registered last
      ID = IDs[IDs$date == max(IDs$date),]$ID %>% unlist()
      if(length(ID)!=1)
        return(NA)
      else
        return(ID)
    }
  })

  # put the old date column back and return the table
  tb$date <- .oldDate
  return(tb)
}




#' @title re_dnaID
#' @export
re_dnaID <- function(tb,tb_rednaid){
  require(glue)
  #1: Go through each row in the rednaID table
  #2: for the given pit, and date:
  #3: find the item in the datepit table with:
  #4: the same pit
  #5: that was registered the time before that datepit
  #6 then, rename the ID in that item with the one from the current row in the rednaID table

  for( i_row in 1:nrow(tb_rednaid) )
  {
    row <- tb_rednaid[i_row,]
    pit <- row$pit
    date <- row$date
    dnaid <- row$dnaID

    if(nrow( tb[tb$pit==pit & tb$date < date,])==0) message(glue("re-dnaid: pit {pit} for id {dnaid} and date {date} not found"))

    tb[tb$pit==pit & tb$date < date,][1,][["ID"]] <- dnaid

  }

  return(tb)

}




#' @export
write_datepit_file <- function(tb, finclip_matches){

  # a table with where every record of a pit tag is one row
  # incl. pit, date, i_measurement, dnaID (if any), pit_i
  tf_pit_raw_long <-
    tb %>%
    clean_ID_df(column_name="dnaID",prefix="Offsp",numLength=4,keep_name=T,remove_NA=F) %>%
    # if the first occurance of a PIT does not have a dnaID, give it a fleeter ID
    group_by(pit) %>%
    arrange(date) %>%
    # if first mention of pit is missing dnaID, make fleeter
    filter(pit!="" & !is.na(pit)) %>%
    mutate(
      dnaID = ifelse((1:n())==1 & is.na(dnaID), yes=glue::glue("Fleeter{date}{measOrder}"), no=dnaID)
    ) %>%
    ungroup() %>%
    rename(
      pit.1=pit,
      pit.2=pit_new,
      pit.3=pit_second,
      pit.4=pit_third
    ) %>%
    mutate(i_measurement = row_number()) %>%
    pivot_longer(c(pit.1,pit.2,pit.3,pit.4),names_to="i_pit",values_to="pit") %>%
    select(pit, date, dnaID, i_pit,i_measurement) %>%
    filter(!is.na(pit),nchar(pit)==23)

  # finclip matches
  # keep only rows with a DNAID - and translate DNAID to ID_original
  tf_pit_DNAID <-
    tf_pit_raw_long %>%
    filter(!is.na(dnaID), !is.na(pit)) %>%
    lookup::lookup(
      from = finclip_matches,
      by.x = "dnaID",
      by.y = "ID",
      what = "ID_original",
      overwrite = T,
      overwriteNA = F
    )

  cycle <- function(tb_ID,tb_raw,msg) {
    message(msg)
    tb_ID <-
      tb_raw %>%
      select(-c(dnaID)) %>%
      filter(!is.na(pit),nchar(pit)==23) %>%
      datepit_to_ID(tb_ID) %>%
      pivot_wider(names_from = i_pit, values_from=pit) %>% group_by(i_measurement) %>% summarise_all(function(x){x[!is.na(x)][1]}) %>%
      pivot_longer(-c(date,ID,i_measurement),names_to="i_pit",values_to="pit") %>%
      group_by(pit,i_pit,i_measurement) %>%
      summarise(
        date = ymd(date[1]),
        ID = ID[1]
      ) %>%
      filter(!is.na(ID),!is.na(pit)) %>%
      group_by(ID,pit) %>%
      summarise_all(function(x){x[!is.na(x)][1]}) %>%
      ungroup()
  }


  tf_pit_ID <-
    tf_pit_DNAID %>%
    rename(ID=dnaID) %>%
    cycle(tf_pit_raw_long,"0-32%") %>%
    cycle(tf_pit_raw_long,"33-65%") %>%
    cycle(tf_pit_raw_long,"66-100%") %>%
    select(pit,date,ID)

  return(tf_pit_ID)

}
