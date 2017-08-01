#' Heightmap creation
#'
#' Create an empty square matrix with size 2^x + 1
#'
#' @param rad An integer value, will be an exponent of 2. It is not recommended to set this higher than 10.
#' @param min A numeric of minimum value for a cell/pixel (default 1)
#' @param min A numeric of maximum value for a cell/pixel (default 255)
#' @return A matrix with random integers between min and max in corners and value -1 in every other field.
#' @export

create_heightmap <- function(rad,min=1,max=255){
  mw <- 2^rad + 1
  heightmap <- matrix(-1,nrow=mw,ncol=mw)
  heightmap[1,1] <- sample(min:max,1)
  heightmap[1,mw] <- sample(min:max,1)
  heightmap[mw,1] <- sample(min:max,1)
  heightmap[mw,mw] <- sample(min:max,1)
  start_step <- paste(2^(rad-1) + 1,2^(rad-1) + 1,rad-1,sep=",")
  heightmap <- square_step(heightmap,c(start_step))
  return(heightmap)
}
