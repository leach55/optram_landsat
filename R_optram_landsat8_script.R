# Install and load packages
pkg_list = c(
  "terra", # read rasters
  "sf",   # vector layers
  "tidyverse"  #working with data 
)

# installs packages that are not installed 
installed_packages = pkg_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg_list[!installed_packages])
}

# Package loading
lapply(pkg_list, library, character.only = TRUE)

# set working directory       
setwd("C:\\Users\\lea5l\\Desktop\\seminar")       

# create folders      
# folder w/ downloaded landsat 
datasets_dir = "./Landsat_datasets"
if (!dir.exists(datasets_dir)) {
  dir.create(datasets_dir)
}


# folder for output
output_dir = "./output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# folder for classified raster's output
NDVI_dir = "./output/NDVI"
if (!dir.exists(NDVI_dir)) {
  dir.create(NDVI_dir)
}


# folder for classified raster's output
STR_dir = "./output/STR"
if (!dir.exists(STR_dir)) {
  dir.create(STR_dir)
}


# function to:
# select wanted bands
# create raster
# crop and mask to study area
# rescale image bands
# calculate NDVI and STR
# return raster

crop_index = function(tif_list, study_area) {
  # Read list of TIF files into stack
  # Crop to extent of testing polygons
  # ---------------------------
  #select wanted bands landsat 8/9
  tif_list_08 = tif_list[grep(pattern="LC08_|LC09", x=tif_list)]
  #tif_list_08 <- tif_list_08[grep(pattern="_SR_", x=tif_list_08)]
  # Do not use B1 (aerosol band)
  tif_list_08 = tif_list_08[grep(pattern = "B2|B3|B4|B5|B6|B7",
                                  x = tif_list_08)]
  tif_stk = rast(tif_list_08)
  
  names(tif_stk) = c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
  cropped = terra::crop(tif_stk, study_area)
  cropped = terra::mask(cropped, study_area) 
  
  #rescale factor for refletance 
  # landsat 8 https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1619_Landsat-8-Collection2_Level-2_Science-Product-Guide-v3.pdf
  cropped = cropped*0.0000275 - 0.2
  
  ndvi = ((cropped$NIR - cropped$red) / (cropped$NIR + cropped$red))
  STR = ((1 -cropped$SWIR2)^2)/(2*cropped$SWIR2) # check witch SWIR band is needed and 
  
  names(ndvi) = "NDVI" 
  names(STR) = "STR"
  
  all = c(cropped, ndvi, STR)        
  
  return(all)
}

# load AOI
aoi = read_sf("migda_perimeter.gpkg")
aoi = aoi %>%
  filter(tillage == "crust")
aoi = st_transform(aoi, crs = st_crs(32636))

aoi = vect(aoi)

#create list of folders with landsat images
img_file_list = list.dirs(datasets_dir, full.names = T, recursive = F)

# run_function = if (length(img_file_list > 0) {
#   
#   lapply(1:length(img_file_list), function(x){
#   
#   tif_list = list.files(x, pattern = "TIF$", full.names = T, recursive = T)
#   
#   if(length(tif_list) > 0) {
#     
#     crop_index_image = crop_index(tif_list, aoi)
#     
#     return(crop_index_image)
#   })
#   }))

#run a "loop" that applys the crop index function to the list of image folders
# and saves the NDVI and STR to respective output folders
run_function = lapply(img_file_list, function(x){
  tif_list = list.files(path = x, pattern = "TIF$", full.names = TRUE, recursive = TRUE)
  crop_index_image = crop_index(tif_list = tif_list, study_area = aoi)
  
  r_split = strsplit(x=tif_list[[4]], split = ".", fixed = TRUE)
  r_split = unlist(r_split)[2]
  r_split = strsplit(x=r_split, split = "/", fixed = TRUE)
  r_split = unlist(r_split)[3]
  r_date = substr(r_split, 18, nchar(r_split)-15)
  
  rastname = paste("NDVI", r_date , sep="_") #get date
  rastpath = file.path(NDVI_dir, paste0(rastname, ".tif")) 
  terra::writeRaster(x= crop_index_image$NDVI, filename = rastpath, overwrite = TRUE)
  
  rastname = paste("STR", r_date , sep="_") #get date
  rastpath = file.path(STR_dir, paste0(rastname, ".tif")) 
  terra::writeRaster(x= crop_index_image$STR, filename = rastpath, overwrite = TRUE)
  
  return(crop_index_image)
  
})

# rastname = paste("NDVI", "get data info..." , sep="_") #get date
# rastpath <- file.path(NDVI_dir, paste0(rastname, ".tif")) # change file path to wanted DIR
# terra::writeRaster(x= crop_index_image$NDVI, filename = rastpath, overwrite = TRUE)
# 
# rastname = paste("STR", "get data info..." , sep="_") #get date
# rastpath <- file.path(NDVI_dir, paste0(rastname, ".tif")) # change file path to wanted DIR
# terra::writeRaster(x= crop_index_image$STR, filename = rastpath, overwrite = TRUE)

# r_split <- strsplit(x=img_file_list[1], split = ".", fixed = TRUE)
# r_split <- unlist(r_split)[2]
# r_date <- substr(r_split, 36, nchar(r_split)-15)
# rastname = paste(r_split, paste0("classified_", landsat), sep="_")
# rastpath <- file.path(classified_full_dir, paste0(rastname, ".tif"))

# pic1 = run_function[[1]]
# plot(pic1)
# 
# pic2 = run_function[[2]]
# 
# df = as.data.frame(pic1$STR)
# df
# 
# 
# df2 = as.data.frame(pic2$STR)
# m = as.matrix(pic1$STR)
# m

#create list of STR images 
STR_tif_list = list.files(STR_dir, full.names = TRUE, recursive = TRUE )

# #make into multiband raster
# STR_rast = rast(STR_tif_list)
# name = substr(STR_tif_list, 14, nchar(STR_tif_list)-5) 
# names(STR_rast) = name #add names with date
# STR_df = as.data.frame(STR_rast) #make into a dataframe
# 
# write.csv(STR_df, "output/STR_df.csv") #save as CSV

# #make into multiband raster
# NDVI_rast = rast(NDVI_tif_list)
# name = substr(NDVI_tif_list, 15, nchar(NDVI_tif_list)-4)
# names(NDVI_rast) = name#add names with date
# NDVI_df = as.data.frame(NDVI_rast)#make into a dataframe
# 
# write.csv(NDVI_df, "output/NDVI_df.csv") #save as CSV

STR_df_list = lapply(STR_tif_list, function(f){
  date_str = unlist(strsplit(basename(f), split = "_", fixed = TRUE))[2]
  date_str = unlist(strsplit(basename(date_str), split = "." ,fixed= TRUE))[1]
  STR = terra::rast(f)
  STR_1_df = as.data.frame(STR, xy=TRUE)
  names(STR_1_df) <- c("x", "y", "STR")
  STR_1_df['Date'] <- as.Date(date_str, format="%Y%m%d")
  return(STR_1_df)
})

STR_df = do.call(rbind, STR_df_list)


#create list of NDVI images
NDVI_tif_list = list.files(NDVI_dir, full.names = TRUE, recursive = TRUE )

NDVI_df_list = lapply(NDVI_tif_list, function(f){
  # Get image date
  date_str = unlist(strsplit(basename(f), split="_", fixed=TRUE))[2]
  date_str = unlist(strsplit(basename(date_str), split = "." ,fixed= TRUE))[1]
  NDVI <- terra::rast(f)
  # Revert to original scale
  #VI <- VI/10000.0
  NDVI_1_df = as.data.frame(NDVI, xy=TRUE)
  names(NDVI_1_df) = c("x", "y", "NDVI")
  NDVI_1_df['Date'] = as.Date(date_str, format="%Y%m%d")
  return(NDVI_1_df)
})

NDVI_df <=do.call(rbind, NDVI_df_list)

# Merge VI and STR pixel data
full_df = dplyr::full_join(STR_df, NDVI_df)
full_df = full_df[stats::complete.cases(full_df),]
df_file = file.path(output_dir, "NDVI_STR_data.rds")
df_file1 = file.path(output_dir, "NDVI_STR_data.csv")
saveRDS(full_df, df_file)
write.csv(full_df, df_file1)
message("Saved: ", nrow(full_df), " rows of NDVI-STR data to: ", df_file)


### code from micha 

#' @title Derive coefficients of slope and intercept
#' @description Derive slope and intercept coefficients
#' for both wet and dry trapezoid lines.
#' Write coefficients to a CSV file (as input to `optram_soilmoisture()` function)
#' @param full_df, data.frame of STR and NDVI values
#' @param output_dir, string, directory to save coefficients CSV file
#' @param step, float
#' @param aoi_file, string, full path to AOI file (used to add title to plot)
#' @param save_plot, boolean, If TRUE (default) save scatterplot to output_dir
#' @return coeffs, list of float, coefficients of wet-dry trapezoid
#' @export
#' @examples print("Running optram_wetdry_coefficients.R")

optram_wetdry_coefficients <- function(full_df,
                                       aoi_file,
                                       output_dir = tempdir(),
                                       step=0.01, 
                                       save_plot = TRUE) {
  # Derive slope and intercept to two sides of trapezoid
  # Based on:
  # https://github.com/teerathrai/OPTRAM
  #
  # Parameters:
  #   full_df: data.frame, table of NDVI and STR values
  # Returns:
  #   coeffs: array of floats, slope & intercept for both wet and dry lines
  
  # Create series of values for NDVI
  # Get min/max values from NDVI data, slightly smaller than full range
  
  # Avoid "no visible binding for global variable" NOTE
  VI_min_max <- VI_series <- VI_STR_list <- VI_STR_df <- NULL
  Qs <- str_max <- str_min <- interval_df <- VI_STR_df1 <- NULL
  
  # Make sure no Inf or NA in full_df
  full_df <- full_df[is.finite(full_df$NDVI), ]
  VI_min_max <- round(stats::quantile(full_df$NDVI, c(0.2, 0.98)), 2)
  VI_series <- seq(VI_min_max[[1]], VI_min_max[[2]], step)
  message("NDVI series length:", length(VI_series))
  VI_STR_list <- lapply(VI_series, function(i){
    # Set NDVI value at midpoint of each interval
    ndvi_val <- i + step/2.0
    
    # Subset the data.frame to include only NDVI values between i and i+step
    interval_df <-  full_df[full_df$NDVI>=i & full_df$NDVI < (i+step),]
    # if too few rows in this interval, skip it, just return NULL
    if (nrow(interval_df) < 4) {
      return(NA)
    }
    # Remove lower than 2% and more than 98% quartile of STR values
    Qs <- stats::quantile(interval_df$STR, c(0.01, 0.99), na.rm=TRUE)
    interval_df <- interval_df[interval_df$STR<=Qs[[2]] &
                                 interval_df$STR>=Qs[[1]],]
    # Now, with outliers removed, find min (dry) and max (wet)
    # Within each interval
    str_max <- max(interval_df$STR, na.rm = TRUE)
    str_min <- min(interval_df$STR, na.rm = TRUE)
    VI_STR_df1 <- data.frame("NDVI" = ndvi_val,
                             "STR_wet" = str_max,
                             "STR_dry" = str_min)
    return(VI_STR_df1)
  })
  # Bind all interval results into one long DF
  VI_STR_list <- VI_STR_list[ !is.na(VI_STR_list) ]
  VI_STR_df <- do.call(rbind, VI_STR_list)
  
  # Save STR and VI values to CSV
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  utils::write.csv(VI_STR_df,
                   file.path(output_dir, "VI_STR_df.csv"),
                   row.names = FALSE)
  # Run linear regression between STR and NDVI
  # to determine the intercept and slope for both wet and dry data
  wet_fit <- stats::lm(STR_wet ~ NDVI, data=VI_STR_df)
  dry_fit <- stats::lm(STR_dry ~ NDVI, data=VI_STR_df)
  i_wet <- wet_fit$coefficients[[1]]
  s_wet <- wet_fit$coefficients[[2]]
  i_dry <- dry_fit$coefficients[[1]]
  s_dry <- dry_fit$coefficients[[2]]
  coeffs <- data.frame("intercept_dry"=i_dry, "slope_dry"=s_dry,
                       "intercept_wet"=i_wet, "slope_wet"=s_wet)
  utils::write.csv(coeffs,
                   file.path(output_dir, "coefficients.csv"),
                   row.names=FALSE)
  
  if (save_plot) {
    rOPTRAM::plot_ndvi_str_cloud(full_df,
                                 coeffs, aoi_file = aoi_file,
                                 output_dir = output_dir)
  }
  return(coeffs)
}

aoi_path = "migda_perimeter.gpkg"

coef = optram_wetdry_coefficients(full_df = full_df, aoi_file = aoi_path, output_dir = output_dir, step = 0.01, save_plot = FALSE)


#' @title Create scatter plot of STR-NDVI point cloud,
#' @description
#' Plot STR-NDVI scatterplot to show dry and wet trapezoid lines
#' over scatterplot of multi-temporal STR and NDVI pixel values
#' @param full_df, data.frame of NDVI and STR pixel values
#' @param coeffs, list of floats, the slope and intercept
#'   of wet and dry regression lines
#' @param aoi_file, string, full path to AOI file
#' @param output_dir, string, directory to save plot png file.
#' @return None
#' @export
#' @import ggplot2
#' @examples
#' print("Running plot_ndvi_str_cloud.R")

#peckage micha
install.packages("remotes")

remotes::install_gitlab("rsl-bidr/roptram")



plot_ndvi_str_cloud <- function(full_df,
                                coeffs,
                                aoi_file,
                                output_dir = tempdir()) {
  # Avoid "no visible binding for global variable" NOTE
  i_dry <- i_wet <- s_dry <- s_wet <- plot_df <- plot_path <- NULL
  x_min <- x_max <- y_min <- y_max <- VI_STR_df1 <- NULL
  NDVI <- STR <- NULL
  
  i_dry <- coeffs$intercept_dry
  s_dry <- coeffs$slope_dry
  i_wet <- coeffs$intercept_wet
  s_wet <- coeffs$slope_wet
  
  # We don't need Millions of points! get a subset
  num_rows <- nrow(full_df)
  if (num_rows < 200000) {
    plot_df <- full_df
  }
  else {
    # This trick drops the num of plotted points 
    # by orders of magnitude, keeping total below 1M
    places <- log10(num_rows)
    divisor <- 10**(places - log10(100000))
    samp_num <- num_rows  / divisor
    sample_idx <- sample(num_rows, samp_num)
    plot_df <- full_df[sample_idx,]
  }
  num_rows_plotted <- nrow(plot_df)
  # NDVI (x) axis limits
  #x_min <- min(plot_df$NDVI)*0.9
  #x_max <- max(plot_df$NDVI)*1.05
  # Set fixed plot limits
  x_min <- 0.0
  x_max <- 0.9
  # STR (y) axis limits
  y_min <- 0.1
  y_max <- 3.6
  #y_max <- max(plot_df$STR)*1.05
  
  aoi_name <- rOPTRAM::aoi_to_name(aoi_file)
  ggplot2::ggplot(plot_df) +
    geom_point(aes(x=NDVI, y=STR),
               color = "#0070000b", alpha = 0.1, size = 0.1) +
    # Wet edge
    geom_abline(intercept = i_wet, slope = s_wet,
                color = "#2E94B9", linewidth = 1.0) +
    # Dry edge
    geom_abline(intercept = i_dry, slope = s_dry,
                color = "#FD5959", linewidth = 1.0) +
    # Set gradient color
    scale_color_gradient(low = "#FD5959",
                         high = "#2E94B9") +
    expand_limits(y=c(y_min, y_max), x=c(x_min, x_max)) +
    labs(x="Vegetation Index", y="SWIR Transformed") +
    ggtitle(paste("Trapezoid Plot - ", aoi_name)) +
    # Set theme
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 18))
  
  plot_path <- file.path(output_dir, paste0("trapezoid_", aoi_name, ".png"))
  ggsave(plot_path, width = 10, height = 7)
  message("Scatterplot of: ", num_rows_plotted, " pixels \n Saved to: ", plot_path)
}

plot_ndvi_str = plot_ndvi_str_cloud(full_df = full_df, coeffs = coef, aoi_file = aoi_path, output_dir = output_dir)











