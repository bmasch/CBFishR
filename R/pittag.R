

parseFallbacks <- function(lines){
  require(lubridate)
  require(dplyr)

  createDataFrame <- function(string_vector) {
    # Ensure the input is a vector and has at least one element
    if (length(string_vector) < 1) {
      stop("Input vector must have at least one element")
    }

    # Split the first element to get the column names
    header <- strsplit(string_vector[1], ",")[[1]]

    # Initialize an empty list to store the data rows
    data_list <- list()

    # Loop through the remaining elements and split by comma
    for (i in 2:length(string_vector)) {
      data_list[[i-1]] <- strsplit(string_vector[i], ",")[[1]]
    }

    # Convert the list to a data frame
    data_frame <- do.call(rbind, data_list)

    # Convert to a data frame and set the column names
    data_frame <- as.data.frame(data_frame, stringsAsFactors = FALSE)
    colnames(data_frame) <- header

    return(data_frame)
  }
  df <- data.frame()
  collect <- F
  data <- ""
  for(i in 1:length(lines)){
    if(collect == T & lines[i]!="proj,obs_site,tag_id,obs_date,coil"){
      if(grepl(",att,",lines[i]) | grepl(",ign,",lines[i])){
        lines[i] <- paste0(lines[i],",,,,")
      }
      data <- c(data,lines[i])
    }
    if(lines[i]=="tag_id, status,entry_ladder,entry_coil,entry_time,exit_ladder,exit_coil,exit_time"){
      data <- lines[i]
      collect <- T
    }
    else if(lines[i]=="proj,obs_site,tag_id,obs_date,coil"){
      collect <- F
      csv <- createDataFrame(data)
      df <- rbind(df,csv)
    }
  }

  return(df)
}

getProjectNames <- function(){
  #' Returns a dataframe with the projects available for DART query
  #'
  #' @return data frame
  #' @export
  #' @examples getProjectNames()
  #'
  df <- data.frame(proj=c("B2A","TDA","JDL","MCA","IHA","LMA","GOA","GRA"),
                   name=c("Bonneville","The Dalles","John Day","McNary","Ice Harbor","Lower Monumental","Little Goose","Lower Granite"),
                   startyear=c(2006, 2013,2018,2006,2006,2014,2014,2016))
  return(df)
}

getSpeciesNames <- function(){
  #' Returns a vector with the species available for DART query.
  #'
  #' @return data frame
  #' @export
  #' @examples getSpeciesNames()
  #'
  species <- c("Chinook","Coho","Steelhead","Sockeye")
  return(species)
}

getRunNames <- function(){
  #' Returns a dataframe with the runs available for DART query
  #'
  #' @return data frame
  #' @export
  #' @examples getRunNames()
  #'

  runs <- data.frame(r=c("1","2","3","4","5","All"),
                     name=c("Spring","Summer","Fall","Winter","Unknown","All"))
}

getDARTAdultMainstem <- function(year=2023,proj="B2A",s=1,r="All",rt="All",relsite="norestrict",trans="norestrict",relregion="norestrict",taginyear="no",datatype="ascents"){
  #' Submits a call to the Fallbacks page on DART.
  #'
  #' @param datatype String,
  #' @param year Number
  #' @param proj String. Project code.
  #' @param s Number. Species code.
  #' @param r Number. Run code.
  #' @param relsite String. norestrict, above, below
  #' @param taginyear String norestrict, no (default), or only
  #' @param datatype
  #' @return list
  #' @export
  #' @examples getDARTAdultMainstem())
  #'

  require(httr)
  require(lubridate)
  require(dplyr)

  url <- paste0("https://www.cbr.washington.edu/dart/cs/php/rpt/pit_fallback.php?year=",year,"&proj=",proj,"&s=",s,"&r=",r,"&rt=",rt,"&relsite=",relsite,"&relregion=norestrict&taginyear=",taginyear,"&trans=",trans,"&period=season")
  print(url)
  resp <- httr::GET(url)
  print(resp$url)
  token <- substring(resp$url,72,nchar(resp$url)-4)
  print(token)

  rel.link <- paste0("https://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php?type=csv&fname=pitfallback_",token,"_rel.csv")
  hist.link <- paste0("https://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php?type=csv&fname=pitfallback_",token,"_hist.csv")
  mevents.link <- paste0("https://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php?type=csv&fname=pitfallback_",token,"_mevents.csv")
  stats.link <- paste0("https://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php?type=csv&fname=pitfallback_",token,"_stats.csv")
  fallback.link <- paste0("https://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php?type=csv&fname=pitfallback_",token,"_fallback.csv")
  print(rel.link)
  rel.csv <- read.csv(rel.link,stringsAsFactors=F) %>%
    mutate(rel_yr=as.numeric(rel_yr)) %>%
    filter(!is.na(rel_yr)) %>%
    mutate(rel_date=ymd(rel_date)) %>%
    mutate(species=getSpeciesNames()[s]) %>%
    mutate(run=getRunNames()$name[run])

  print("releases")

  if(datatype=="ascents"){
    print(mevents.link)
    mevents.csv <- read.csv(mevents.link,stringsAsFactors=F) %>%
      filter(entry_ladder != "") %>%
      rename(tag_id=pit_code) %>%
      mutate(entry_time=ymd_hms(entry_time),exit_time=ymd_hms(exit_time))
    print("ascents")
    return(list(token=token,releases=rel.csv,data=mevents.csv))
  }

  else if(datatype=="histories"){
    print(hist.link)
    hist.csv <- read.csv(hist.link,skip=6,stringsAsFactors=F) %>%
      mutate(obs_date=ymd_hms(obs_date)) %>%
      filter(!is.na(obs_date))
    print("histories")
    return(list(token=token,releases=rel.csv,data=hist.csv))
  }

  else if(datatype=="stats"){
    print(stats.link)
    stats.csv <- read.csv(stats.link,stringsAsFactors=F)
    print("stats")
    return(list(token=token,releases=rel.csv,data=stats.csv))
  }
  else if(datatype=="fallbacks"){
    print(fallback.link)
    fallback.lines <- readLines(fallback.link)
    fallback.csv <- parseFallbacks(fallback.lines) %>%
      mutate(entry_time=ymd_hms(entry_time),exit_time=ymd_hms(exit_time))

    print("fallbacks")
    return(list(token=token,releases=rel.csv,data=fallback.csv))
  }
  else return(list())

}
