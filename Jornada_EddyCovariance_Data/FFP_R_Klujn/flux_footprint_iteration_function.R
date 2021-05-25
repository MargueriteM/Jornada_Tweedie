#########################################################################################
###               Function to Iterate FFP Function Over Rows in Data Frame            ###
###                                Code by HGR 5/2021                                 ###
#########################################################################################

# Function to fun ffp over each row of input data
calc.ffp.loop <- function(df, tower.loc, raster.brick, contour.range) {
  
  print(paste('Function prep starting at', Sys.time()))
  
  # extract tower location information as numeric 
  tower.x.utm <- st_coordinates(tower.loc)[,1]
  tower.y.utm <- st_coordinates(tower.loc)[,2]
  latitude.wgs84 <- st_coordinates(st_transform(tower.loc, crs = 4326))[,1]
  
  # create raster template onto which output should be projected
  crop.extent <- extent(tower.x.utm - 500,
                        tower.x.utm + 500,
                        tower.y.utm - 500,
                        tower.y.utm + 500)
  raster.brick.crop <- crop(raster.brick, crop.extent)
  
  # create vector of ffp output names
  matrices <- c('x_2d', 'y_2d', 'f_2d')
  
  # create output objects
  karst.pc <- vector()
  sd.mtopo <- vector()
  
  # run ffp model on each row of data in the input (each row is a half hour period)
  for (i in 1:nrow(df)) { 
    
    print(paste('Iteration', i, 'footprint model starting at', Sys.time()))
    
    # run ffp model
    ffp <- calc_footprint_FFP(lat = latitude.wgs84,
                              zm = as.numeric(df[i, "zm"]),
                              z0 = as.numeric(df[i, "z0"]),
                              umean = as.numeric(df[i, "u_mean"]),
                              ol = as.numeric(df[i, "L"]),
                              sigmav = as.numeric(df[i, "sigma_v"]),
                              ustar = as.numeric(df[i, "u_star"]),
                              wind_dir = as.numeric(df[i, "wind_dir"]),
                              r = contour.range)
    
    if (ffp$flag_err == 0) {
      
      print(paste('Iteration', i, 'footprint model done. Reformating starting at', Sys.time()))
      
      # format as data frame (this allows easy alignment of points to ec tower)
      ffp.sf <- map_dfc(matrices,
                        ~ ffp[[.x]] %>% # select x, y, or f matrix
                          as.data.frame() %>%
                          pivot_longer(cols = 1:ncol(.),
                                       names_to = 'remove',
                                       values_to = .x) %>% # make it into a 'tidy' data frame (long format with single column for x, y, or f)
                          select(-matches('remove'))) %>% # we don't need the names column which just contains original column information
        mutate(x_geo = x_2d + tower.x.utm, # add ec tower latitude location to x distance values
               y_geo = y_2d + tower.y.utm, # add ec tower longitude location to y distance values
               f_2d_scaled = f_2d/sum(f_2d, na.rm = TRUE)) %>% # scale the flux measurement for each cell to a percentage of total flux
        st_as_sf(coords = c('x_geo', 'y_geo'), crs = 32606, remove = FALSE)
      
      # reformat contours as sf
      ffp.sa.polygon <- list()
      for (j in 1:length(ffp$xr)) {
        # extract vectors with contour location information
        xr <- ffp$xr[[j]] + tower.x.utm
        yr <- ffp$yr[[j]] + tower.y.utm
        # convert contour information to an sf polygon object
        ffp.sa.polygon[[j]] <- st_polygon(list(matrix(c(xr, yr), ncol = 2, byrow = FALSE)))
      }
      ffp.sa.sfc <- st_as_sfc(ffp.sa.polygon, crs = 32606)
      ffp.sa.sf <- st_sf(geometry = ffp.sa.sfc) %>%
        mutate(interval = contour.range)
      
      # use rasterize to convert points to a raster aligned with thermokarst model
      ffp.raster <- rasterize(as(ffp.sf, 'Spatial'),
                              raster.brick.crop,
                              field = 'f_2d_scaled',
                              fun = 'sum',
                              na.rm = TRUE)
      
      # mask to maximum requested contour polygon
      ffp.raster.mask <- mask(ffp.raster, ffp.sa.sf)
      raster.brick.mask <- mask(raster.brick.crop, ffp.sa.sf)
      # plot(ffp.raster.mask)
      
      # calculate thermokarst percent cover
      karst.pc[i] <- cellStats(raster.brick.mask[[1]]*ffp.raster.mask, sum, na.rm = TRUE)/cellStats(ffp.raster.mask, sum, na.rm = TRUE)
      
      # calculate mean microtopography
      sd.mtopo[i] <- sqrt(cellStats(ffp.raster.mask*(raster.brick.mask[[2]] - cellStats(raster.brick.mask[[2]], mean, na.rm = TRUE))^2, sum, na.rm = TRUE)/((length(ffp.raster.mask[!is.na(ffp.raster.mask)]) - 1)/length(ffp.raster.mask[!is.na(ffp.raster.mask)])*cellStats(ffp.raster.mask, sum, na.rm = TRUE)))
      
    } else { # if the model didn't run for the current time period
      
      print(paste('Iteration', i, 'footprint model failed at', Sys.time()))
      
      # fill in karst.pc and sd.mtopo with NA
      karst.pc[i] <- NA
      sd.mtopo[i] <- NA
      
    }
    
  }
  
  print(paste('Loop completed at', Sys.time()))
  
  output <- df %>%
    mutate(karst.pc = karst.pc,
           sd.mtopo = sd.mtopo)
  
  print(paste('Output being returned at', Sys.time()))
  
  
  return(output)
  
}
