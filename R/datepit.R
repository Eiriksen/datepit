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

#' @title datepit_to_ID2
#' @description Function for obtaining the ID of an individual using columns date and pit. It takes in two tables, one to fill in (tb) and one to look up pit-tags from (tb_pit)
#' @param tb table to put ID's into (and get pit tags and dates from) must have columns "date" and "pit". Date should be formated as date-month-year or as a date value. The function's output will be this table plus a new "ID" column.
#' @param tb_pit a table that contains all known pit tags, their date of registration, and their corresponding ID. Date should be formated as data-month-year or as a date value.
#' @examples tb_fish_withID <- datepit_to_ID(tb_fish,tb_pit)
#' @examples tb_fish_withID <- tb_fish %>% datepit_to_ID(tb_pit)
#' @examples tb_fish_withID <- tb_fish %>% datepit_to-ID(read.table("pit date reference.txt",head=T))
#' @export
datepit_to_ID2 = function(tb, tb_pit){
  require(lubridate)

  # check that pit and date columns exist in the table
  if (!"date" %in% colnames(tb)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb)) stop("pit column missing from tb")
  if (!"date" %in% colnames(tb_pit)) stop("date column missing from tb")
  if (!"pit" %in% colnames(tb_pit)) stop("pit column missing from tb")

  # save the old date column in the tb before messing around with it
  .oldDate <- tb$date

  # format dates, if necessary
  if(!is.Date(tb_pit$date)) tb_pit$date <- ymd(tb_pit$date)
  tb_pit <- tb_pit %>% rename(date_pit=date)

  if(!is.Date(tb$date)) tb$date <- ymd(tb$date)


  tb_obs_pit <-
    tb %>%
    mutate(observation = row_number()) %>%
    left_join(tb_pit,by="pit", multiple="all") %>%
    filter(!is.na(date_pit)) %>%
    filter(date_pit <= date) %>%
    arrange(date_pit) %>%
    group_by(observation) %>%
    summarise(ID = ID[n()])

  tb %>%
    mutate(observation = row_number()) %>%
    left_join(tb_obs_pit,by="observation") %>%
    select(-observation)

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

  tb_rednaid <- tb_rednaid %>% clean_ID_df(column_name="dnaID",prefix="Offsp",numLength=4,keep_name=T,remove_NA=F)

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
write_datepit_file <- function(tb, finclip_matches) {

  # Creates an overview of every first record of a pit, and whether tha that pit is "lonely"
  # Uses the longform pit record table (see tf_pit_raw_long below)
  # A lonely pit is missing a dnaID or another pit tag at first mention
  # Thus, it can't be connected to any ID, and the fish is given an Incognito ID
  # Eg: "Incognito-20210204-545"
  # returns the same pit table, but with dnaIDs for he incognito fish.
  identify_incognitos <- function(tb_pits){
    # Checks every single record of a pit and whether is it lonely or not
    # Two columns: i_measuremnt, lonely
    tb_lonely_measurements <-
      tb_pits %>%
      group_by(i_measurement) %>%
      summarise(
        lonely = if_else(sum(!is.na(pit)) == 1 & all(is.na(ID)),T,F)
        ) %>% ungroup()

    # if first mention of pit is missing ID and another pit (is lonely), make incognito
    # alternatively, if a "pit_new" is lonely, also make incognito
    tb_pits %>%
      left_join(tb_lonely_measurements,by="i_measurement") %>%
      group_by(pit) %>%
      arrange(date) %>%
      filter(pit!="" & !is.na(pit)) %>%
      mutate(
        ID = ifelse((1:n())==1 & lonely, yes=glue::glue("Incognito{i_measurement}"), no=ID),
        ID = ifelse(i_pit == 2 & lonely, yes=glue::glue("Incognito{i_measurement}"), no=ID)
      ) %>%
      ungroup() %>%
      select(-lonely)
  }

  # magic woodo I've forgot how this works
  # is the centerpiece for this system
  # I think it basically "connects" IDs between pit-tags
  cycle <- function(tb_ID,tb_raw) {
    tb_IDtemp <-
      tb_raw %>%
      select(-c(ID)) %>%
      filter(!is.na(pit),nchar(pit)==23) %>%
      datepit_to_ID2(tb_ID) %>%
      pivot_wider(id_cols = c(i_measurement,date,ID), names_from = i_pit, values_from=pit) %>% group_by(i_measurement) %>% summarise_all(function(x){x[!is.na(x)][1]}) %>%
      pivot_longer(-c(date,ID,i_measurement),names_to="i_pit",values_to="pit") %>%
      filter(!is.na(ID),!is.na(pit)) %>%
      group_by(ID,pit) %>%
      summarise_all(function(x){x[!is.na(x)][1]}) %>%
      ungroup()
  }
  
  # A long table for each record of a pit tag
  # every record of a pit tag is one row
  # columns: 
  #   pit, 
  #   date,
  #   i_measurement (each measurment of one fish, which may have multiple pit tags, gets a unique running number) 
  #   i_pit (pit nr 1, 2, 3, for a fish, in order of detection)
  #   ID (name of finclip, if any)
  tf_pit_raw_long <-
    tb %>%
    clean_ID_df(column_name="dnaID",prefix="Offsp",numLength=4,keep_name=F,remove_NA=F) %>%
    rename(
      pit.1=pit,
      pit.2=pit_new,
      pit.3=pit_second,
      pit.4=pit_third
    ) %>%
    arrange(date,measOrder) %>%
    mutate(i_measurement = row_number()) %>%
    pivot_longer(c(pit.1,pit.2,pit.3,pit.4),names_to="i_pit",values_to="pit") %>%
    filter(!is.na(pit),nchar(pit)==23) %>%
    #identify_incognitos() %>%
    select(pit, date, ID, i_pit, i_measurement)


  # A table of every record with a ID (know ID)
  # transltes new dnaIDs to their old one using fin clip matches
  # keep only rows with a ID 
  tf_pit_DNAID <-
    tf_pit_raw_long %>%
    filter(!is.na(ID), !is.na(pit)) %>%
    lookup::lookup(
      from = finclip_matches,
      by.x = "ID",
      by.y = "ID",
      what = "ID_original",
      overwrite = T,
      overwriteNA = F
    )


  # here the magic happens
  tf_pit_ID <-
    tf_pit_DNAID %>%
    cycle(tf_pit_raw_long) %>%
    cycle(tf_pit_raw_long) %>%
    cycle(tf_pit_raw_long) %>%
    select(pit,date,ID)

  #clearing non-connected pit tags
  tf_pit_raw_long_incognito <- 
    tf_pit_raw_long %>% filter(! pit %in% tf_pit_ID$pit) %>% 
    identify_incognitos() 
  
  tf_pit_ID_incognito <- 
    tf_pit_raw_long_incognito %>% 
    cycle(tf_pit_raw_long_incognito %>% filter(!is.na(ID))) %>%
    cycle(tf_pit_raw_long_incognito %>% filter(!is.na(ID))) %>%
    cycle(tf_pit_raw_long_incognito %>% filter(!is.na(ID))) %>%
    select(pit,date,ID)

  return(bind_rows(tf_pit_ID,tf_pit_ID_incognito))
         
}
