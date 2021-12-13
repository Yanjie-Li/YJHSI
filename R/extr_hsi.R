

#' Title
#'
#' @param listlayer the number of the his raster image layer, 1,2,3,4....
#'
#' @return
#' @export
#'
#' @examples lisddt <- 1:length(hsi@layers)
#' @examples fdfyi <- extr_hsi(listlayer=lisddt )
#' @examples saveRDS(fdfyi,'fdfyi.rds')


extr_hsi <- function(listlayer){
  lapply(listlayer, function(list1){
    message(paste0('hsi','_',list1))
    landsatsub1  <- subset(hsi, list1)
    landsatsub2  <- flip(landsatsub1, "y")
    library(tictoc)
    tic("for loop start")
    dff <- raster::extract(landsatsub2,sf)
    print("finished")
    toc()
    dff
  })}


#' Title
#'
#' @param list each tree number
#'
#' @return the xy, to be combine with the data from extr_hsi function
#'
#' @examples fghfh
extr_xy <- function(list){
  lapply(listlayer, function(list1){
    message(paste0('hsi','_',list1))
    library(tictoc)
    tic("for loop start")
    # plot(landsatsub2)
    plot(sf[list1,],add=T,col='red')
    dff3 <- raster::mask(landsatsub2,sf[list1,])
    dff33 <- rasterToPoints(dff3)
    print("finished")
    toc()
    dff33
  })}


