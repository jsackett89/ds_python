##############################################################################################
# 3-D distance between objects from geological survey data
##############################################################################################

# Read in necessary packages
library(readr)
library(dplyr)
library(fields)
library(scatterplot3d)
library(rgdal)

# Read in location sensor data
well <- read_csv('data.csv')

# Extract records that have more than one well per field
well <- well %>%
  group_by(Field) %>%
  filter(n_distinct(UWI) > 1)

# Arrange survey data by field and descending depth per item
well <- well %>%
  group_by(Field, UWI) %>%
  arrange(desc(Z_Meters))

# Subset out five wells at a time to test progress
well_subset <- well[well$UWI %in% sample(unique(well$UWI), 5), ]

############################################################################################
# Normalize data
############################################################################################

# Function to map all latitude, longitude, and depth values to the range [0,1]
# data_set = data set to normalize
norm <- function() {
  well_norm <<- well %>%
    group_by(UWI) %>%
    mutate(z_norm = (Z_Meters - min(Z_Meters)) / (max(Z_Meters) - min(Z_Meters)))
}

############################################################################################
# Visualizing the underground geometry of wells
############################################################################################

# d = data set
scatter3d <- function(d) {
  for (i in unique(d$UWI)) {
    scatterplot3d(d$Lat_NAD27[d$UWI == i], d$Long_NAD27[d$UWI == i],
                  d$Z_Meters[d$UWI == i], highlight.3d = TRUE,
                  xlab = 'Latitude', ylab = 'Longitude', 
                  zlab = 'Depth', main = paste('Underground geometry of', i))
  }
}

# Test function on subset
scatter3d(well_subset)

############################################################################################
# Converting latitude/longitude to UTM coordinates
############################################################################################

# Add UTM zones
well$zone_UTM <- (floor((well$Long_NAD27 + 180)/6) %% 60) + 1

# Map UTM coordinates to well
well_10 <- subset(well, zone_UTM == 10)
well_10 <- cbind(well_10, project(matrix(c(well_10$Long_NAD27, well_10$Lat_NAD27), ncol = 2),
                                  '+proj=utm +zone=10 ellps=WGS84'))

well_11 <- subset(well, zone_UTM == 11)
well_11 <- cbind(well_11, project(matrix(c(well_11$Long_NAD27, well_11$Lat_NAD27), ncol = 2),
                                  '+proj=utm +zone=11 ellps=WGS84'))

well_12 <- subset(well, zone_UTM == 12)
well_12 <- cbind(well_12, project(matrix(c(well_12$Long_NAD27, well_12$Lat_NAD27), ncol = 2),
                                  '+proj=utm +zone=12 ellps=WGS84'))

well_13 <- subset(well, zone_UTM == 13)
well_13 <- cbind(well_13, project(matrix(c(well_13$Long_NAD27, well_13$Lat_NAD27), ncol = 2),
                                  '+proj=utm +zone=13 ellps=WGS84'))

well_14 <- subset(well, zone_UTM == 14)
well_14 <- cbind(well_14, project(matrix(c(well_14$Long_NAD27, well_14$Lat_NAD27), ncol = 2),
                                  '+proj=utm +zone=14 ellps=WGS84'))

well <- rbind(well_10, well_11, well_12, well_13, well_14)

rm(well_10)
rm(well_11)
rm(well_12)
rm(well_13)
rm(well_14)

names(well)[7] <- 'easting_UTM'
names(well)[8] <- 'northing_UTM'

############################################################################################
# Subsetting observations from the bottom of the object
############################################################################################

# Strategy 1: Extract wells based on convergence of rate of change
## Attach the rate of depth change between subsequent records
well <- well %>%
  group_by(UWI) %>%
  mutate(depth_roc = (lead(Z_Meters) - Z_Meters) / sqrt((lead(easting_UTM) - easting_UTM) ^ 2 + 
                                                        (lead(northing_UTM) - northing_UTM) ^ 2))

## Set rate of change for bottom record of each well to '0'
well$depth_roc[is.na(well$depth_roc) == TRUE] <- 0

# Extract wells where the | rate of change of depth | is <= 0.1
well_bottom_1 <- well[which(abs(well_roc$depth_roc) <= 0.1), ]

# Strategy 2: Extract bottom x% of wells
## Create column of normalized depths per wells
well <- well %>%
  group_by(UWI) %>%
  mutate(z_norm = (Z_Meters - min(Z_Meters)) / (max(Z_Meters) - min(Z_Meters)))

## Extract wells from bottom 1% of depths
well_bottom_2 <- well_norm[well_norm$z_norm <= 0.01, ]

## Extract wells from bottom 5% of depths
well_bottom_3 <- well[well$z_norm <= 0.05, ]

###########################################################################################
# Creating distance measure between wells
###########################################################################################

# Create data set with centroid of each well
centroid_1 <- well_bottom_1 %>%
  group_by(Field, UWI) %>%
  summarize(lat_centroid = mean(Lat_NAD27), long_centroid = mean(Long_NAD27),
            z_centroid = mean(Z_Meters))

centroid_2 <- well_bottom_2 %>%
  group_by(Field, UWI) %>%
  summarize(lat_centroid = mean(Lat_NAD27), long_centroid = mean(Long_NAD27),
            z_centroid = mean(Z_Meters))

centroid_3 <- well_bottom_3 %>%
  group_by(Field, UWI) %>%
  summarize(lat_centroid = mean(Lat_NAD27), long_centroid = mean(Long_NAD27),
            z_centroid = mean(Z_Meters))

# Create lateral distance matrix

## Create unique field set
field <- unique(centroid$Field)

## Initialize new columns. Create unique id for rownames to map back to original data if necessary.
centroid$dist_closest_well <- 0
centroid$UWI_closest_well <- '0'
rownames(centroid) <- centroid$UWI

dist_calc <- function() {
  for (i in field) {
    df <- subset(centroid, Field == i)
    lat_dist_mat <- 1000 * rdist.earth(matrix(c(df$long_centroid, df$lat_centroid), ncol = 2),
                                       miles = F)
    vert_dist_mat <- as.matrix(dist(df$z_centroid, method = 'manhattan', diag = T, upper = T))
    dist_mat <<- sqrt(lat_dist_mat ^ 2 + vert_dist_mat ^ 2)
    diag(dist_mat) <<- NA
    rownames(dist_mat) <<- df$UWI[df$Field == i]
    colnames(dist_mat) <<- df$UWI[df$Field == i]
    for (j in 1:nrow(dist_mat)) {
      centroid[centroid$UWI == rownames(dist_mat)[j], 'dist_closest_well'] <<- 
        min(dist_mat[j, ], na.rm = TRUE)
      range(dist_mat[j, ], na.rm = TRUE)
      centroid[centroid$UWI == rownames(dist_mat)[j], 'UWI_closest_well'] <<- 
        names(which.min(dist_mat[j, ]))
    }
  }
}

well_final <- write_csv(centroid, 'whatever.csv')
