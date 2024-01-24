#' Sync bathymetry and behavioral data
#'
#' Pulls bathymetry data from the internet and associates it with tag locations
#' @param data A dataframe of location data to associate with bathymetry data
#' @param rad Distance in km around each location that seafloor depth and slope will be calculated. This can be a single value or a vector of different values the same length as the number of rows of the data input
#' @param res Resolution of the bathymetric grid, in minutes
#' @param names Columns names of longitude and latitude (in that order) in data
#' @param isobaths Different isobath levels to which you'd like distances for each location (can be more than one numeric value here)
#' @param perimeter list of longitudes and latitudes to create box perimeter from which to pull bathymetric data (useful if wanting to plot bathy data)
#' @return A dataframe with all tag data and its associated bathymetry data as new columns in the dataset
#'
fullmarmap.bathy <- function(data, rad = 1, res = 1e-10, names = c("Longitude","Latitude"), isobaths = 0, perimeter = list(top = NA, right = NA, bottom = NA, left = NA)) {
  #pull locations and simplify
  locs <- data[,names] %>% as_tibble()
  colnames(locs) <- c("lon", "lat")

  #make sure locations are in north pacific
  if (all(locs$lat[which(!is.na(locs$lat))]>0) == FALSE) {stop("Locations must have positive lat and negative lon for this code")}
  if (all(locs$lon[which(!is.na(locs$lon))]<0) == FALSE) {stop("Locations must have positive lat and negative lon for this code")}

  #pull bathy data from region of interest
  if (NA %in% unlist(perimeter)) {
    top <- swfscMisc::destination(lat = max(locs$lat, na.rm=TRUE), lon = median(locs$lon, na.rm=TRUE), brng = 0, distance = (rad*1.5), units = "km", type = "vincenty")[1]
    bottom <- swfscMisc::destination(lat = min(locs$lat, na.rm=TRUE), lon = median(locs$lon, na.rm=TRUE), brng = 180, distance = (rad*1.5), units = "km", type = "vincenty")[1]
    left <- swfscMisc::destination(lat = median(locs$lat, na.rm=TRUE), lon = min(locs$lon, na.rm=TRUE), brng = 270, distance = (rad*1.5), units = "km", type = "vincenty")[2]
    right <- swfscMisc::destination(lat = median(locs$lat, na.rm=TRUE), lon = max(locs$lon, na.rm=TRUE), brng = 90, distance = (rad*1.5), units = "km", type = "vincenty")[2]
    bmat_full <- marmap::getNOAA.bathy(lon1 = left, lon2 = right, lat1 = bottom, lat2 = top, resolution = res, keep = FALSE, antimeridian = FALSE)
  } else {
    bmat_full <- marmap::getNOAA.bathy(lon1 = perimeter$left, lon2 = perimeter$right, lat1 = perimeter$bottom, lat2 = perimeter$top, resolution = res, keep = FALSE, antimeridian = FALSE)
  }
  bath_full <- marmap::as.xyz(bmat_full)
  names(bath_full) <- c("lon", "lat", "z")

  #prep data outputs
  data <- data %>% mutate(bathy_resolution_mins = (as.numeric(rownames(bath_full))[2] - as.numeric(rownames(bath_full))[1]) / 0.016667,
                          bathy_radius_km = rad, bathy_key = "negative bathy is below sea level while positive is on land",
                          bathy_nearest = NA, bathy_mean = NA, bathy_median = NA, bathy_sd = NA, bathy_iqr = NA,
                          bathy_slope = NA, bathy_aspect = NA, bathy_min = NA, bathy_max = NA, bathy_n = NA)

  #distance to isobaths
  for (iso in isobaths) {
    data[,paste0("dist2iso",iso)] <- marmap::dist2isobath(bmat_full, locs, isobath = iso)$distance/1000
  }

  #loop through locations and pull stats of interest
  for (i in 1:nrow(data)) {
    print(paste0(i,"/",nrow(data)))
    if (is.na(locs$lat[i]) | is.na(locs$lon[i])) {next}

    #get locations within radius of interest
    top <- swfscMisc::destination(lat = locs$lat[i], lon = locs$lon[i], brng = 0, distance = (rad*2), units = "km", type = "vincenty")[1]
    bottom <- swfscMisc::destination(lat = locs$lat[i], lon = locs$lon[i], brng = 180, distance = (rad*2), units = "km", type = "vincenty")[1]
    left <- swfscMisc::destination(lat = locs$lat[i], lon = locs$lon[i], brng = 270, distance = (rad*2), units = "km", type = "vincenty")[2]
    right <- swfscMisc::destination(lat = locs$lat[i], lon = locs$lon[i], brng = 90, distance = (rad*2), units = "km", type = "vincenty")[2]
    bath <- bath_full %>% filter(lon < right & lon > left,
                                 lat < top & lat > bottom)
    bath$dist <- as.vector(geosphere::distm(c(locs$lon[i], locs$lat[i]), bath[,c(1:2)],
                                            geosphere::distVincentyEllipsoid)/1000)
    bath_rad <- bath %>% filter(dist <= rad)

    #calculate stats
    data$bathy_nearest[i] <- bath_rad$z[which.min(bath_rad$dist)]
    data$bathy_min[i] <- max(bath_rad$z) #positive bathy = above sea level
    data$bathy_max[i] <- min(bath_rad$z) #negative bathy = below sea level
    data$bathy_n[i] <- length(bath_rad$z)
    data$bathy_mean[i] <- mean(bath_rad$z)
    data$bathy_median[i] <- median(bath_rad$z)
    data$bathy_sd[i] <- sd(bath_rad$z)
    data$bathy_iqr[i] <- IQR(bath_rad$z)

    #aspect and slope
    zdiff <- max(bath_rad$z) - min(bath_rad$z)
    lon1 <- bath_rad$lon[which.max(bath_rad$z)] #positive bathy = above sea level
    lon2 <- bath_rad$lon[which.min(bath_rad$z)] #negative bathy = below sea level
    lat1 <- bath_rad$lat[which.max(bath_rad$z)]
    lat2 <- bath_rad$lat[which.min(bath_rad$z)]
    zdist <- geosphere::distVincentyEllipsoid(c(lon1, lat1),c(lon2,lat2))
    data$bathy_slope[i] <- zdiff / zdist
    data$bathy_aspect[i] <- geosphere::bearing(c(lon1,lat1), c(lon2,lat2))
  }

  return(list(data=data, bathy_matrix = bmat_full))
}
