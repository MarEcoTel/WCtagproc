crawl.itup <- function(data, model.interval) {
  require(crawl)
  require(sf)
  crs <- 2230
  if ('POSIXct' %in% class(data$Date) == FALSE) {
    data$Date <- as.POSIXct(as.character(data$Date), format = '%m/%d/%Y %H:%M:%S', tz='UTC')
  }
  sf_locs <- st_as_sf(data, coords = c("Longitude", "Latitude")) %>% st_set_crs(4326)
  make_unique <- function(x) {
    xts::make.time.unique(x$Date, eps = 1)
  }
  data <- data %>% arrange(DeployID, Date)
  data$Date <- suppressWarnings(make_unique(data))
  sf_locs <- st_as_sf(data, coords = c("Longitude", "Latitude")) %>% st_set_crs(., 4326)
  sf_locs <- st_transform(sf_locs, crs)
  sfc_as_cols <- function(x, names = c("x", "y")) {
    stopifnot(inherits(x, "sf") && inherits(st_geometry(x), "sfc_POINT"))
    ret <- do.call(rbind, st_geometry(x))
    ret <- as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
    ret <- setNames(ret, names)
    bind_cols(x, ret)
  }
  sf_locs <- suppressWarnings(sf_locs %>% sfc_as_cols())
  sf_locs <- sf_locs %>%
    group_by(DeployID) %>%
    arrange(Date) %>%
    nest() %>%
    mutate(data = map(data, st_as_sf))
  ellipse_matrix <- function(x) {
    if (inherits(x, "sf")) {
      st_geometry(x) <- NULL
    }
    ret <- model.matrix(
      ~ Error.Semi.major.axis + Error.Semi.minor.axis +
        Error.Ellipse.orientation,
      model.frame(~ ., x, na.action = na.pass))[,-1]
  }

  sf_locs <- sf_locs %>%
    mutate(diag = map(data, ellipse_matrix),
           diag = map(diag, ~ argosDiag2Cov(
             .x[,1], .x[,2], .x[,3])),
           data = map2(data, diag, bind_cols)
    ) %>% select(-diag)
  fit_crawl <- function(d) {

    priors <- list(NULL,
                   ln_prior = function(par) {dnorm(par[2], 4, 4, log = TRUE)},
                   lap_prior = function(par) {-abs(par[2] - 4) / 5},
                   reg_prior = function(par) {dt(par[2] - 3, df = 1, log = TRUE)}
    )
    #cycle through 4 different prior values. the first being no prior/NULL
    for (pppp in 1:length(priors)) {
      prior <- priors[[pppp]]
      fit <- crwMLE(
        err.model = list(
          x =  ~ ln.sd.x - 1,
          y =  ~ ln.sd.y - 1,
          rho =  ~ error.corr
        ),
        data = d,
        Time.name = "Date",
        fixPar = c(1, 1, NA, NA),
        prior = prior
      )
      if ("crwFit" %in% class(fit)){
        if (!any(is.na(fit$se[3:4]))) {
          print(pppp)
          return(fit)
        }
      }
    }
    for (pppp in 1:length(priors)) {
      prior <- priors[[pppp]]
      fit <- crwMLE(
        err.model = list(
          x =  ~ ln.sd.x - 1,
          y =  ~ ln.sd.y - 1,
          rho =  ~ error.corr
        ),
        data = d,
        Time.name = "Date",
        fixPar = c(1, 1, NA, 4),
        prior = prior
      )
      if ("crwFit" %in% class(fit)){
        if (!is.na(fit$se[3])) {
          print(paste("brown",pppp))
          return(fit)
        }
      }
    }
    stop("bad fit")
  }
  data_fit <- suppressWarnings(sf_locs %>%
                                 mutate(fit = pmap(list(d = data), fit_crawl),
                                        params = map(fit, tidy_crwFit)))
  data_fit <- data_fit %>%
    mutate(predict = map(fit,
                         crwPredict,
                         predTime = model.interval,
                         return.type = "flat"))
  as.sf <- function(p, id, epsg, type, loctype) {
    p <-
      st_as_sf(p, coords = c("mu.x","mu.y")) %>%
      mutate(TimeNum = as_datetime(TimeNum),
             DeployID = id) %>%
      rename(pred_dt = TimeNum) %>%
      filter(locType == loctype) %>%
      st_set_crs(.,epsg)
    return(p)
  }
  data_fit <- data_fit %>%
    mutate(sf_points = map2(predict, DeployID,
                            as.sf,
                            epsg = crs,
                            type = "POINT",
                            loctype = "p"))
  m <- data_fit$sf_points %>%
    lift(rbind)() %>%
    st_set_crs(crs) %>%
    st_transform(4326)
  xys <- as.character(m$geometry)
  x <- y <- vector()
  for (i in 1:length(xys)) {
    xyi <- xys[i]
    xyi <- substr(xyi, 3, nchar(xyi))
    xyi <- substr(xyi, 1, nchar(xyi) - 1)
    xi <- strsplit(xyi, ", ")[[1]][1]
    yi <- strsplit(xyi, ", ")[[1]][2]
    x <- c(x, xi)
    y <- c(y, yi)
  }
  predicted.path <- data.frame(x,y)
  mr <- data_fit$predict[[1]]
  mr <- mr[which(mr$locType == 'p'),]

  mrDate <- mr$Date
  mrDeployID <- c()
  for (i in 1:nrow(data_fit)) {
    mrDeployID<- c(mrDeployID, as.character(rep(data_fit$DeployID[i],nrow(mr[which(mr$Ptt == data_fit$predict[[i]]$Ptt[1]),]))))
  }
  mrPtt <- mr$Ptt
  mrLoc <- mr$LocationQuality
  mrLatitude <- as.numeric(as.character(predicted.path$y))
  mrLongitude <- as.numeric(as.character(predicted.path$x))

  mr <- data.frame(DeployID = mrDeployID, Ptt = mrPtt, Date = mrDate, LocationQuality = mrLoc,
                   Latitude = mrLatitude, Longitude = mrLongitude, Type= mr$Type,
                   mr[,c(8:ncol(mr))])
  names(mr)[which(names(mr) == "msg")] <- "MsgCount"
  return(mr)
}
