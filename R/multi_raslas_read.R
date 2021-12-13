#' to read and join the multi spectral with the cloud data
#'
#' @param las_list  the list of each month cloud data with names
#' @param dsf_list the list of each month raster image data wihch form ras_band_ali
#'
#' @return a list of each tree in each month with multi spectra and cloud information
#'
#' @examples urll1 <- 'E:/PCGSPRO_1623035349/17366648332/42/map/'
#' @examples dsf1 <- raster_read(urll1)
#' @examples dsf1 <-dsf1 %>% unlist(recursive = F)%>%  unlist(recursive = F)
#' @examples las_01 <- readLAS('E:/PCGSPRO_1623035349/17366648332/43/models/pc/0/terra_las/cloud.las',
#' @examples filter = "-change_classification_from_to 1 2",
#' @examples select = "xyzirc")
#' @examples las_list <- list(las_01,las_03)
#' @examples las_list <- list(las_01,las_03)
#' @examples names(las_list) <- c('Jan', 'Mar')
#' @examples dsf_list <- list(dsf1,dsf3)
#' @examples names(dsf_list) <- c('Jan','Mar')
#' @examples data_all <- multi_raslas_read(las_list,dsf_list)


multi_raslas_read <- function(las_list,dsf_list){
  # las_list <- list(las_list)
  lapply(las_list, function(x){
    # las_list <- names(las_list)
    expr <- tryCatch({
      library("lidR")
      library("rgdal")
      library(raster)
      library(tidyverse)
      # print(names(las_@header@VLR$Extra_Bytes$`Extra Bytes Description`))
      message( paste0(names(x@header@VLR$Extra_Bytes$`Extra Bytes Description`)))
      las = classify_ground(x,csf(cloth_resolution = 0.5, class_threshold = 0.15, rigidness = 1), last_returns = FALSE)
      pologon1 <- readRDS('e:/OneDrive - Business/宋钊颖/pologon1.rds')
      subset = clip_roi(las,pologon1)
      dtm <- grid_terrain(subset, res = 0.5, algorithm = knnidw(k=5,p = 0.5), use_class = c(1L, 2L),keep_lowest = F)
      nlas_dtm  <- subset - dtm
      chm    <- grid_canopy(nlas_dtm , res = 0.5, p2r())
      ttops  <- find_trees(nlas_dtm , lmf(ws=6, hmin=2.6, shape = "circular"))
      algo   <- dalponte2016(chm, ttops )
      lass   <- segment_trees(nlas_dtm, algo, uniqueness ='incremental')
      crown_polo  <- delineate_crowns(lass, func = .stdtreemetrics)
      crowns_data <- as.data.frame(crown_polo)
      crowns <- algo()
      a<-extent(crowns)
      ab<-c(a)
      ab <- map(ab, function(x) {as(x,'SpatialPolygons')}) %>%
        map(.,function(x){x@proj4string <- CRS(" +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs");
        spTransform(x,CRS("+proj=longlat +datum=WGS84 +no_defs"))})
      new_extent <-ab %>% map(extent)
      extent(crowns) <- new_extent[[1]]
      data2    <- rasterToPoints(crowns)
      data2    <- as.data.frame(data2)
      crowns_all <- crowns_data %>% left_join(data2,by = c("treeID"='layer' ) )
      crowns_all <- crowns_all%>% dplyr:: select(-c(2:4))%>%
        dplyr::select(treeID,x,y,everything())
      head(crowns_all)
      library(data.table)
      fd <- dsf_list[names(dsf_list) ==names(x@header@VLR$Extra_Bytes$`Extra Bytes Description`)]
      message(paste(names(fd)))
      lapply(fd, function(x1){
        x2 <-list(unlist(x1))
        lapply(x2, function(ls){
          lapply(ls, function(ls2){
            # if(names(ls2) == names(x)
            tryCatch({
              message( paste0(names(ls2)))
              e <- extent(crowns)
              new_raster <- crop(ls2, e )
              rc1 <- extend(new_raster,crowns)
              # library(viridis)
              ls_da <- raster::resample(rc1,crowns, method = 'ngb')
              plot(ls_da)
              r3 <- mask(ls_da, crowns)
              plot(r3,add=T, alpha=0.6,col=rainbow(1))
              df_r <-rasterToPoints(r3)
              df_r <- as.data.frame(df_r)
              df_crownssds <-as.data.frame(rasterToPoints(crowns))
              names(df_crownssds)[3] <-'treeID'
              library(tidyverse)
              crodata <- crowns_all %>% left_join(df_r,by = c('x','y' ))
              library(beepr)
              beep(4)
              crodata},
              error = function(e) {
                message('Caught an error!')
                cat("ERROR :", conditionMessage(e), "\n")
                print(e)})})
        })
      })

    },error = function(e) {
      message('Caught an error!')
      cat("ERROR :", conditionMessage(e), "\n")
      print(e)},
    print(paste("processing Loop", names(las_list), sep = "_")))
  })
}
