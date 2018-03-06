#' @title Split Lines
#' @description to be filled
#' @param spobj sp object
#' @param dist distance in units of coords
#' @param start start from the start (T) or the end (F)
#' @return spatialLinesDataFrame
#' @import sp
#' @import plyr
#' @export
#'
splitLines<- function(spobj, dist, start = T){
    xydf<-coordBuild(spobj)
    if (start == F){
        xydf<-xydf[rev(rownames(xydf)),]
    }
    spoints <- split(xydf, dist)
    linelist <- list()
    lineslist <- list()
    id <- 1
    j <- 1
    for(i in 1:(nrow(spoints)-1)){
        linelist[j] <- Line(spoints[c(i, i + 1), c(1:2)])
        j = j + 1
        if(spoints[i+1,3] == 1){ 
            lineslist[id]<-Lines(linelist, ID = id)
            id = id+1
            linelist<-list()
            j = 1
        }
    }
    return(SpatialLinesDataFrame(SpatialLines(lineslist), data = data.frame(id = 0:(length(lineslist)-1))))
}