#' Title to combine all singe multispectral image together fro DJI terra
#'
#' @param url the link folder that contaim multiple singe layer images
#'
#' @return a list of all combined image data
#'
#' @examples urll11 <- 'E:/PCGSPRO_1623035349/17366648332/117/map/'
#' @examples dsf11 <- raster_read(urll11)
#' @examples dsf1 <-dsf1 %>% unlist(recursive = F)%>%  unlist(recursive = F)

ras_band_ali  <- function(url) {
  lapply(url, function(urll){
    imag <- list.files(
      path = urll,
      pattern = '.tif',
      all.files = T,
      full.names = T,
      no.. = T
    )
    imag <- list(imag)
    # imag1 <-imag[-c(2,9)]
    lapply(imag, function(z)
      expr <- tryCatch({
        library(raster)
        p_dsm <-raster:: raster(z[[1]])
        p_blue <-raster:: raster(z[[4]])
        p_green <-raster:: raster(z[[5]])
        p_red <- raster::raster(z[[7]])
        p_redege <- raster::raster(z[[8]])
        p_nir <- raster::raster(z[[6]])
        extent(p_dsm) <- raster::extent(p_blue)
        trainx <- list(p_red,p_blue,p_green,p_redege,p_nir,p_dsm)
        # names(trainx) <- c('red',"blue", "green",'redege','nir','dsm')
        return(trainx)

      },
      error = function(e) {
        message('Caught an error!')
        cat("ERROR :", conditionMessage(e), "\n")
        print(e)
      },
      print(paste("processing Loop", z, sep = "_"))))})}
