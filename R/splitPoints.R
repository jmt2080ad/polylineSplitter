#' @title Split Points
#' @description to be filled
#' @param spobj sp object
#' @param dist distance in units of coords
#' @param start start from the start (T) or the end (F)
#' @param simplify return simple version x/y points if T else, SpatialPointsDataFrame
#' @return points split in format per simplify
#' @import sp
#' @import plyr
#'
splitPoints<-function(spobj, dist, start = T, simplify = F){
    xydf<-coordBuild(spobj)
    if (start == F){
        xydf <- xydf[rev(rownames(xydf)),]
    }
    spoints <- split(xydf, dist)
    spoints <- spoints[which(spoints$end == 1),]
    spoints$id <- c(0:(nrow(spoints) - 1))
    spoints$distance <- spoints$id * dist
    if(simplify == F){
        return(SpatialPointsDataFrame(SpatialPoints(data.frame(x = spoints$x, y = spoints$y)), data = spoints[,which(names(spoints) %in% c("x","y","id","distance"))]))
    }
    return(spoints[c("x", "y")])
}
