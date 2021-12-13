
#' my first fuction
#' @param lisr list of all months，data from multi_raslas_read function
#'
#' @return  monall_fam

#' @examples matou_1 <- fread('E:/OneDrive - Business/陶学雨/matou_data/matou1.csv')[,-1]
#' @examples matou_1$num <-1: length(matou_1$type)
#' @examples matou_2 <- readRDS('E:/OneDrive - Business/陶学雨/matou_data/labelmatou2.rds')
#' @examples matou_2$num <-1: length(matou_2$type)
#' @examples ####matou 1 raster
#' @examples xy <- as.data.frame(matou_1[,c(7,6)])
#' @examples xy$Lon <- xy$Lon-0.000006
#' @examples xy$Lat <- xy$Lat+0.00001
#' @examples xy <- as.matrix(xy)
#' @examples e <- extent(xy)
#' @examples r <- raster(e, ncol=500, nrow=500)
#' @examples canopy1 <- rasterize(xy, r, matou_1[,12])
#' @examples xy <- as.data.frame(matou_2[,c(7,6)])
#' @examples xy$Lon <- xy$Lon+0.000005
#' @examples xy$Lat <- xy$Lat-0.000002
#' @examples xy <- as.matrix(xy)
#' @examples e <- extent(xy)
#' @examples r <- raster(e, ncol=500, nrow=500)
#' @examples canopy2 <- rasterize(xy, r, matou_2[,12])
#' @examples r_1to11 <- list(r2rmse_all1,r2rmse_all3,r2rmse_all4,r2rmse_all5,
#' @examples                 r2rmse_all6,r2rmse_all7,r2rmse_all8,r2rmse_all9,
#' @examples                 r2rmse_all10,r2rmse_all11)
#' @examples  mon11_fam <- extra_fam(r_1to11 )
#' @examples  factor(mon11_fam2$Fam)
#' @examples  monall_fam <-mon11_fam%>%invoke(rbind,.)
#' @examples  saveRDS(monall_fam,'monall_fam.rds')


extra_fam <- function(lisr){
  lapply(lisr, function(dd){
    library(tidyverse)
    library(raster)
    ## nov data raster
    xy2 <- as.data.frame(dd[,c(2,3,1)])
    canop = rasterFromXYZ(xy2)
    plot(canop)
    # plot(ra)
    plot(canopy1 ,
         add=T,
         col=rainbow(1))
    plot(canopy2 ,
         add=T,
         col=rainbow(1))
    ##site 1
    e <- extent(canopy1)
    new_raster <- raster::crop(canop, e )
    rc1 <- extend(new_raster,canopy1)
    plot(rc1)
    rc1 <- raster::resample(rc1,canopy1, method = 'ngb')
    r3 <- mask(rc1, canopy1 )
    plot(r3 ,
         add=T,
         col=rainbow(1))
    ##site 1
    ee <- extent(canopy2)
    new_raster2 <- raster::crop(canop, ee )
    rc2 <- extend(new_raster2,canopy2)
    plot(rc2)
    rc2 <- raster::resample(rc2,canopy2, method = 'ngb')
    r32 <- mask(rc2, canopy2 )
    plot(r32 ,
         add=T,
         col=rainbow(1))

    sta_sit1 <- stack(r3,canopy1)
    sta_sit2 <- stack(r32,canopy2)

    #### merge site 1
    data_sta_1 <- rasterToPoints(sta_sit1)
    data_sta_1 <- data.frame(data_sta_1)
    combin1 <- data_sta_1%>% inner_join(matou_1, by = c ('layer'= 'num')) %>% as_tibble()
    combin1 <- combin1 %>% dplyr::select(1,2,3,4,5,8,9,10,11)
    names(combin1) <- sapply(strsplit(names(combin),'[.]'), function(x){
      y=x[1]
    })
    names(combin1)[7] <- 'Mapy'
    names(combin1)[5] <- 'Fam'
    trid_fam <- combin1   %>%
      group_by(Fam ) %>%
      summarise_at(vars(c(treeID)), funs(min(., na.rm=TRUE)))
    mon11_fam1 <- dd %>% left_join(trid_fam, by = 'treeID')
    # mon11_fam1 <- mon11_fam1 %>% drop_na(Fam)
    mon11_fam1 <-mon11_fam1[!is.na(mon11_fam1$Fam),]
    #### merge site 2
    data_sta_2 <- rasterToPoints(sta_sit2)
    data_sta_2 <- data.frame(data_sta_2)
    combin2 <- data_sta_2%>% inner_join(matou_2, by = c ('layer'= 'num')) %>% as_tibble()
    combin2 <- combin2 %>% dplyr::select(1,2,3,4,5,8,9,10,11)
    names(combin2) <- sapply(strsplit(names(combin),'[.]'), function(x){
      y=x[1]
    })
    names(combin2)[7] <- 'Mapy'
    names(combin2)[5] <- 'Fam'
    trid_fam2 <- combin2   %>%
      group_by(Fam ) %>%
      summarise_at(vars(c(treeID)), funs(min(., na.rm=TRUE)))
    mon11_fam2 <- dd %>% left_join(trid_fam2, by = 'treeID')
    # mon11_fam2 <- mon11_fam %>% drop_na(Fam)
    mon11_fam2 <-mon11_fam2[!is.na(mon11_fam2$Fam),]
    mon11_fam  <- rbind.data.frame(mon11_fam1,mon11_fam2)
  })
}


