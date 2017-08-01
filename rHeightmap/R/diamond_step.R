#' Diamond Step
#'
#' Execute diamond step of the diamond square algorithm
#'
#' @param HM A matrix containing the heightmap that is being generated
#' @param steps A vector of string containing planned steps for the diamond function in the format "row,column,radius"
#' @return Returns an altered Heightmap entered as parameter
diamond_step <- function(HM,steps) {
  new_squares <- c()
  while (length(steps) > 0) {
    step <- pop(steps)
    step <- as.numeric(strsplit(step,',')[[1]])
    row <- step[1]
    col <- step[2]
    rad <- 2^step[3]
    temp_sum <- 0
    temp_count <- 0
    if (is.element(row+rad,1:length(HM[1,]))) {
      temp_count <- temp_count + 1
      temp_sum <- temp_sum + HM[row+rad,col]
    }
    if (is.element(row-rad,1:length(HM[1,]))) {
      temp_count <- temp_count + 1
      temp_sum <- temp_sum + HM[row-rad,col]
    }
    if (is.element(col+rad,1:length(HM[1,]))) {
      temp_count <- temp_count + 1
      temp_sum <- temp_sum + HM[row,col+rad]
    }
    if (is.element(col-rad,1:length(HM[1,]))) {
      temp_count <- temp_count + 1
      temp_sum <- temp_sum + HM[row,col-rad]
    }
    HM[row,col] <- as.integer(temp_sum/temp_count)  + (sample(1:(4*rad),1) - 2*rad)
    if (step[3] > 0) {
      r <- step[3] - 1
      if (is.element(row + 2^r,1:length(HM[1,])) && is.element(col + 2^r,1:length(HM[1,]))) {
        tmp <- paste(row + 2^r, col + 2^r, r, sep = ",")
        if (!is.element(tmp,new_squares)) {
          push(new_squares,tmp)
        }
      }
      if (is.element(row + 2^r,1:length(HM[1,])) && is.element(col - 2^r,1:length(HM[1,]))) {
        tmp <- paste(row + 2^r, col - 2^r, r, sep = ",")
        if (!is.element(tmp,new_squares)) {
          push(new_squares,tmp)
        }
      }
      if (is.element(row - 2^r,1:length(HM[1,])) && is.element(col + 2^r,1:length(HM[1,]))) {
        tmp <- paste(row - 2^r, col + 2^r, r, sep = ",")
        if (!is.element(tmp,new_squares)) {
          push(new_squares,tmp)
        }
      }
      if (is.element(row - 2^r,1:length(HM[1,])) && is.element(col - 2^r,1:length(HM[1,]))) {
        tmp <- paste(row - 2^r, col - 2^r, r, sep = ",")
        if (!is.element(tmp,new_squares)) {
          push(new_squares,tmp)
        }
      }
    }
  }
  if (step[3] > 0) {
    ret <- square_step(HM,new_squares)
    return(ret)
  } else {
    return(HM)
  }
}
