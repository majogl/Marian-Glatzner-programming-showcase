#' Square Step
#'
#' Execute square step of the diamond square algorithm
#'
#' @param HM A matrix containing the heightmap that is being generated
#' @param steps A vector containing planned steps for the square function in the format "row,column,radius"
#' @return Returns an altered Heightmap entered as parameter
square_step <- function(HM,steps) {
  new_diamonds <- c()
  while (length(steps) > 0) {
    step <- pop(steps)
    step <- as.numeric(strsplit(step,',')[[1]])
    row <- step[1]
    col <- step[2]
    rad <- 2^step[3]
    HM[row,col] <- as.integer(
                    (HM[row+rad,col+rad] + HM[row-rad,col-rad] +
                    HM[row-rad,col+rad] + HM[row+rad,col-rad]) / 4) + (sample(1:(4*rad),1) - 2*rad)
    nd <- c(paste(row + rad,col,step[3],sep=","),
            paste(row - rad,col,step[3],sep=","),
            paste(row,col + rad,step[3],sep=","),
            paste(row,col - rad,step[3],sep=","))
    for (i in 1:length(nd)) {
      if (!is.element(nd[i],new_diamonds)) {
        push(new_diamonds,nd[i])
      }
    }
  }
  #print(new_diamonds)
  ret <- diamond_step(HM,new_diamonds)
  return(ret)
}
