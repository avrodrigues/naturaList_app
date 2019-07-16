library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(raster)
library(rgdal)
#require(mapview)        
#require(mapedit)
require(leaflet.extras)
library(shinyLP)


### Files 
ex.table <- read.csv( "www/ex.table_Fern.csv")
cont <- readOGR("www/Continentes", "level1")
#ocean <- SpatialPolygons(list(Polygons(list(pol), ID = "Ocean")))
source("include.criteria.R")

reduce.df <- function(df){

      institutionsource <- as.character(df$institutionCode)
      collectioncode <- as.character(df$collectionCode)
      catalognumber <- as.character(df$catalogNumber)
      year.event <- df$year
      dateIdentified <- df$dateIdentified
      scientific.name <- as.character(df$species)
      determined.by <- as.character(df$identifiedBy)
      longitude <- df$decimalLongitude
      latitude <- df$decimalLatitude
      basis.of.rec <- df$basisOfRecord
      media.type <- df$mediaType
      occ.id <- df$occurrenceID
      rowID <- rownames(df)
      
      
      data <- data.frame(rowID, occ.id, scientific.name, longitude, latitude,
                         year.event, determined.by, dateIdentified, 
                         institutionsource, collectioncode, 
                         catalognumber,media.type, basis.of.rec, stringsAsFactors = FALSE)
      
      ll.na <- is.na(data$longitude)
      data <- data[!ll.na,]
      lat.na <- is.na(data$latitude)
      final.data <- data[!lat.na,]
      
      final.data
}

## grid.base - character. length = 2. Database and resolution.
#raster.grid <- function(grid.base){
#  option <- grid.base[1]
#  res <- grid.base[2]
#  raster.res <- grid.base[3]
#  
#  if(option == "WorldClim"){
#    res.choosed <- c("10 minutes" = "www/wc_10m.tif", 
#                     "5 minutes" = "www/wc_5m.tif",
#                     "2.5 minutes" = "www/wc_2.5m.tif")
#    gd <- raster(res.choosed[res])
#  }
#  
#  if(option == "CHELSA"){
#    gd <- raster("www/CHELSA_30s.tif")
#  }
#  
#  if(option == "ecoClimate"){
#    gd <- raster("www/bio # CCSM_Modern(1950-1999)_bio1.bil")
#  }
#  
#  if(option == "Define a resolution"){
#    gd <- raster(resolution = as.numeric(raster.res))
#  }
#  gd
#}


continent.selection <- function(continents) {
  cont.list <- list("Europe" = 1,      
                    "Africa" = 2,      
                    "Asia" = c(3,4), 
                    "Oceania" = c(5,6), 
                    "Americas" = c(7,8), 
                    "Antarctic" = 9)     
  
  cont.color <- list("Europe" = "grey70", 
                     "Africa" = "orange", 
                     "Asia" = rep("yellow", 2),
                     "Oceania" = rep("red", 2), 
                     "Americas" = rep("green", 2), 
                     "Antarctic" = "lightblue")
  
  
  
  
  cont.sel <- do.call(c,cont.list[continents])
  color.sel <- do.call(c,cont.color[continents])
  
  list(cont.sel, color.sel)
}

shape.remove <- function(x, cont.sel){
  sptDF <- SpatialPointsDataFrame(x[,c("longitude", "latitude")], x)
  cut <- cont[cont.sel,]
  final.cols <- c(ncol(sptDF)+1,ncol(sptDF)+2)
  as.data.frame(sptDF[cut,])[,-final.cols]
  
}

grid.selector <- function(x, grid.resolution = c(0.5,0.5), session.app = session){
  
  
  require(sp)
  require(raster)
  
  sptDF <- SpatialPointsDataFrame(x[,c("longitude", "latitude")], x)
  
  spt.spp_DF <- sptDF[,]
  
  resolution <- grid.resolution
  
  ext <- extent(spt.spp_DF)[1:4] 
  
  new.ext <- c(ext[1] - resolution[1], 
               ext[2] + resolution[2],
               ext[3] - resolution[1],
               ext[4] + resolution[2])
  
  r <- raster(resolution = resolution, ext = extent(ext))
  
  cell.with.pts <- as.numeric(names(table(cellFromXY(r, spt.spp_DF))))
  
  total <- length(cell.with.pts)
  #pb <- txtProgressBar(min = 0, max = total, style = 3)
  final.cols <- c(ncol(spt.spp_DF)+1,ncol(spt.spp_DF)+2)
  
  df.crit <- vector("list", total)
  idx <- 1
  
  
  
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:total){
                   e <- extentFromCells(r,cell.with.pts[i])
                   crop.df <- crop(spt.spp_DF, extent(e))
                   
                   
                   df.crit[[idx]] <- as.data.frame(crop.df)[1,-final.cols]
                   
                   idx <- idx+1
                  }
  })
  
  df.occ.crit <- do.call(rbind,df.crit)
  
  return(df.occ.crit)
  
}

rm.coord.dup <- function(x){
  
  unique.row <- !duplicated(x[,c("latitude","longitude")])
  res <- x[unique.row,]
  row.names(res) <- 1:nrow(res)
  
  res
}

pol.coords <- function(input.polig){
  
  pol.coords <- data.frame(x=numeric(),y=numeric())
  total <- length(input.polig$geometry$coordinates[[1]])
  long.pol <- numeric()
  lat.pol <- numeric()
  for (i in 1:total){
    long.pol <- input.polig$geometry$coordinates[[1]][[i]][[1]]
    lat.pol <- input.polig$geometry$coordinates[[1]][[i]][[2]]
    
    
    coords <- data.frame(x=long.pol,y=lat.pol)
    
    pol.coords <- rbind(pol.coords, coords)
    
  }
  
  pol.coords 
}

make.polygon <- function(df){
  
  # and then something like this
  sp <- SpatialPolygons(list(Polygons(list(Polygon(df)), 1)))
  sp
  
}