#' Prettify files in the WC portal folder
#'
#' Copies WC portal data to a new folder and formats column names and date/time information in the process. All other data analyses will require these column headers.
#' @param WCfolder A file path to the WC portal folder with the tag data.
#' @param archival Is the tag one of the archival tags. Default is FALSE
#' @param extra Some archival tags have extra stuff in file titles. Default is there is nothing extra.
#' @return Once finished, a new folder names "WC-pretty" will be created within the folder specified by the input of 'WCfolder' in which all new data CSV files will be placed.
#' @export
#' @examples prettifyWC("WhaleTag001/WCPortal")

prettifyWC <- function(WCfolder, archival = FALSE, extra = "") {
  #get files in WC-portal folder
  lf <- list.files(WCfolder)

  #determine PTT and deployID if present
  deployID <- NULL
  if (archival) {
    for (f in 1:length(lf)) {
      splitfile <- strsplit(strsplit(lf[f], '/')[[1]], '-')[[1]]
      if (splitfile[length(splitfile)] == 'Locations.csv') {
        PTT <- splitfile[3]
        deployID <- strsplit(paste(splitfile[1:3],collapse="-"), '_')[[1]][1]
      }
    }
  } else {
    for (f in 1:length(lf)) {
      splitfile <- strsplit(strsplit(lf[f], '/')[[1]], '-')[[1]]
      if (splitfile[length(splitfile)] == 'All.csv') {
        PTT <- splitfile[1]
      }
      if (length(splitfile) > 1) {
        if ((splitfile[length(splitfile)] == 'FastGPS.csv') & (splitfile[(length(splitfile) - 1)] %in% c('1', '2', '3', '4'))) {
          deployID <- splitfile[1]
          if (length(splitfile)==6) { #for some of the newer SOCAL LIMPETs
            deployID <- paste(splitfile[1:3], collapse="-")
          }
        }
      }
    }
  }

  #create WC-pretty folder
  newdir <- paste(WCfolder, 'WC-pretty', sep = '/')
  dir.create(newdir)

  #create names of files needing to be prettied
  filetags <- c("All", "Argos", "Behavior", "Corrupt", "Histos",
                "Locations", "MinMaxDepth", "RawArgos", "Series", "SeriesRange",
                "Status", "Summary", "Argos-imade", "SST", "FastGPS")
  if (archival) {
    files2prettify <- paste0(deployID, extra, "-", filetags, ".csv")
    files2prettify <- files2prettify[which(files2prettify %in% lf)]
  } else {
    files2prettify <- paste0(PTT, "-", filetags, ".csv")
    fastGPS1 <- paste0(deployID, "-", PTT, "-", c("1-FastGPS", "1-Locations", "2-FastGPS", "2-Locations", "3-FastGPS", "3-Locations", "4-FastGPS", "4-Locations"), ".csv")
    fastGPS2 <- paste0(PTT, "-", c("1-FastGPS", "1-Locations", "2-FastGPS", "2-Locations", "3-FastGPS", "3-Locations", "4-FastGPS", "4-Locations"), ".csv")
    files2prettify <- c(files2prettify, fastGPS1, fastGPS2)
    files2prettify <- files2prettify[which(files2prettify %in% lf)]
  }

  #process each file that needs to be prettied, if it exists
  for (FILE in files2prettify) {
    ##########################################################
    # ALL
    if (grepl("-All.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        if (names(data)[1] == "row.names") {
          names(data) <- c("DeployID", names(data)[3:ncol(data)])
          data <- data[,1:(ncol(data) - 1)]
        }
        names(data)[which(names(data) == "Platform.ID.No.")] <- "Ptt"
        names(data)[which(names(data) == "Loc..date")] <- "Date"
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # ARGOS
    if (grepl("-Argos.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        if (names(data)[1] == "row.names") {
          names(data) <- c("DeployID", names(data)[3:ncol(data)])
          data <- data[,1:(ncol(data) - 1)]
        }
        data$Date <- time.turner(data$Date)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # BEHAVIOR
    if (grepl("-Behavior.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Start <- time.turner(data$Start)$raw
        data$End <- time.turner(data$End)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # CORRUPT
    if (grepl("-Corrupt.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Date <- time.turner(data$Date)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # HISTOS
    if (grepl("-Histos.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Date <- time.turner(data$Date)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # LOCATIONS
    if (grepl("-Locations.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Date <- time.turner(data$Date)$raw
        data$MsgCount <- data$Satellite <- NA
        names(data)[which(names(data) == "Quality")] <- "LocationQuality"
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # MINMAXDEPTH
    if (grepl("-MinMaxDepth.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Date <- time.turner(data$Date)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # RAWARGOS
    if (grepl("-RawArgos.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data)>0) {
        PassDate <- paste0(data$PassDate, " ", data$PassTime)
        PassDate <- time.turner(PassDate)$raw
        data$PassDate <- PassDate
        data$PassTime <- NULL
        MsgDate <- paste0(data$MsgDate, " ", data$MsgTime)
        MsgDate <- time.turner(MsgDate)$raw
        data$MsgDate <- MsgDate
        data$MsgTime <- NULL
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names =FALSE)
      }
      next
    }

    ##########################################################
    # SERIES
    if (grepl("-Series.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if(nrow(data) > 0) {
        DATE <- paste0(data$Day, " ", data$Time)
        DATE <- time.turner(DATE)$raw
        data$Date <- DATE
        data$Time <- NULL
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }


    ##########################################################
    # SERIESRANGE
    if (grepl("-SeriesRange.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Start <- time.turner(data$Start)$raw
        data$End <- time.turner(data$End)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # STATUS
    if (grepl("-Status.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname,stringsAsFactors=FALSE,row.names=NULL)
      if (nrow(data) > 0) {
        data$RTC <- time.turner(data$RTC)$raw
        data$Received <- time.turner(data$Received)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # SUMMARY
    if (grepl("-Summary.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        if (names(data)[1] == "row.names") {
          names(data) <- c("DeployID", names(data)[3:ncol(data)])
          data <- data[,1:(ncol(data) - 1)]
        }
        data$EarliestXmitTime <- time.turner(data$EarliestXmitTime)$raw
        data$LatestXmitTime <- time.turner(data$LatestXmitTime)$raw
        data$EarliestDataTime <- time.turner(data$EarliestDataTime)$raw
        data$LatestDataTime <- time.turner(data$LatestDataTime)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # ARGOS-imade  # for FaRW031
    if (grepl("-Argos-imade.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data) > 0) {
        data$Date <- time.turner(data$Date)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # SST  # for GgTag018
    if (grepl("-SST.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(data)> 0) {
        data$Date <- time.turner(data$Date)$raw
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    ##########################################################
    # FastGPS
    if (grepl("-FastGPS.csv",FILE)) {
      readname <- paste0(WCfolder, "/", FILE)
      data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
      if (data[1,1] == "") {
        data <- read.table(readname, header = TRUE, sep = ",",
                           skip = 3, stringsAsFactors = FALSE, row.names = NULL)
      }
      if (nrow(data) > 0) {
        names(data)[which(names(data) == "Name")] <- "DeployID"
        data$DeployID <- deployID
        DATE <- paste0(data$Day, " ", data$Time)
        data$Date <- time.turner(DATE)$raw
        data$Time <- NULL
        prettyname <- paste0(newdir, "/", FILE)
        write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
      }
      next
    }

    if (!is.null(deployID)) {
      ##########################################################
      if (FILE %in% c(paste0(deployID, "-", PTT, "-1-FastGPS.csv"), paste0(deployID, "-", PTT, "-2-FastGPS.csv"), paste0(deployID, "-", PTT, "-3-FastGPS.csv"), paste0(deployID, "-", PTT, "-4-FastGPS.csv"))) {
        readname <- paste0(WCfolder, "/", FILE)
        data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
        if (data[1,1] == "") {
          data <- read.table(readname, header = TRUE, sep = ",",
                             skip = 3, stringsAsFactors = FALSE, row.names = NULL)
        }
        if (nrow(data) > 0) {
          names(data)[which(names(data) == "Name")] <- "DeployID"
          DATE <- paste0(data$Day, " ", data$Time)
          data$Date <- time.turner(DATE)$raw
          data$Time <- NULL
          data$InitTime <- time.turner(data$InitTime)$raw
          prettyname <- paste0(newdir, "/", FILE)
          write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
        }
      }
      ##########################################################
      if (FILE %in% c(paste0(deployID, "-", PTT, "-1-Locations.csv"), paste0(deployID, "-", PTT, "-2-Locations.csv"), paste0(deployID, "-", PTT, "-3-Locations.csv"), paste0(deployID, "-", PTT, "-4-Locations.csv"))) {
        readname <- paste0(WCfolder, "/", FILE)
        data <- read.csv(readname, stringsAsFactors = FALSE, row.names = NULL)
        if (nrow(data) > 0) {
          data$Date <- time.turner(data$Date)$raw
          prettyname <- paste0(newdir, "/", FILE)
          write.csv(data, file = prettyname, quote = FALSE, row.names = FALSE)
        }
      }
    }
  }
}
