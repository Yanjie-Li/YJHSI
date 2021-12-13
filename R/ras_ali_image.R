
#' Title
#'
#' @param monthi the month number,for example: "May"
#' @param fami the family name of each tree,for example:"2_9_5"
#'
#' @return
#' @examples  data <- data
#' @examples  fam <- (unique(data$Fam))
#' @examples  month <- (unique(data$month))
#' @examples  chl_tree <- ras_ali_image(fami=fam,monthi=month)




ras_ali_image <- function(monthi,fami){
  lapply(monthi, function(m){
    m2 <-list(unlist(m))
    lapply(m2, function(m22){
      expr <- tryCatch({ # message( paste0(x,'_',f))
        # lapply(monthi , function(x){
        library(tidyverse)
        library(raster)
        library(EBImage)
        library(tools)
        nir <- filter(data, month == m22[1] )
        lapply(fami, function(f){
          f2 <-list(unlist(f))
          lapply(f2, function(f22){
            nir2 <- dplyr::filter(nir, Fam == f22[1]  )
            nir3 <- nir2[,-c(1:3)]
            chl <- nir2[,3]
            library(tidyverse)
            matou_vis <- nir3 %>% dplyr:: mutate(
              ndvi=  ((result_NIR - result_Red) / (result_NIR + result_Red)),
              osavi = ((result_NIR-result_Red)*(1+0.16)) / (result_NIR + result_Red + 0.16),
              gndvi = (result_NIR-result_Green)/(result_NIR+result_Green),
              savi = ((result_NIR - result_Red)*(1+0.5))/((result_NIR + result_Red+0.5)),
              msavi = (2*result_NIR+1-sqrt((2*result_NIR+1)^2-8*(result_NIR-result_Red)))/2,
              gci = result_NIR/result_Green-1,
              RECI = result_NIR/result_RedEdge-1,
              LCI = (result_NIR-result_RedEdge)/(result_NIR+result_Red),
              GRVI =(result_Green-result_Red)/(result_Green+result_Red),
              MGRVI =(result_Green^2-result_Red^2)/(result_Green^2+result_Red^2 ),
              RGBVI =(result_Green^2-result_Red*result_Blue)/(result_Green^2+result_Red*result_Blue),
              NDRE= (result_NIR-result_RedEdge)/(result_NIR+result_RedEdge),
              MACI= result_NIR/result_Green,
              ARI= result_Green/result_NIR,
              MARI=(result_Green^(-1)-result_RedEdge^(-1))/result_NIR

            )
            # library(raster)
            # # # # set up an 'empty' raster, here via an extent object derived from your data
            # xy <- as.matrix(nir3[,1:2])
            tryCatch({
              library(raster)
              # create spatial points data frame
              spg <- matou_vis
              coordinates(spg) <- ~ x + y
              # coerce to SpatialPixelsDataFrame
              gridded(spg) <- TRUE
              # coerce to raster
              rasterDF <- stack(spg)
              plot(rasterDF)
              imgg <- as.array(rasterDF )
              ds3 <- EBImage::as.Image(imgg)
              ds4 <- EBImage::resize(ds3,30,30)
              f_name  <- list(ds4)
              names(f_name) <- paste0(m22[1],'_',f22[1])
              f_namechl  <- list(chl)
              names(f_namechl) <- paste0(m22[1],'_chl_',f22[1])
              message( paste0(m22[1],'_',f22[1]))
              return(list(f_namechl,f_name))
            },error = function(e) {NULL})
            # e <- extent(xy)

            # # # # you need to provide a function 'fun' for when there are multiple points per cell


          })})},error = function(e) {
            message('Caught an error!')
            cat("ERROR :", conditionMessage(e), "\n")
            print(e)})})})}


