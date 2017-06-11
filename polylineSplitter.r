### requires plyr, rgdal
require(rgdal)
require(plyr)

coordBuild <- function(spobj){
    if(class(spobj) %in% c("SpatialLinesDataFrame",    "SpatialLines")){
        coords <- lapply(spobj@lines, function(x) lapply(x@Lines, function(y) y@coords))
        coords <- ldply(coords, data.frame)
        names(coords) <- c("x","y")
        return(coords)
    }
    if(class(spobj) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")){
        coords <- lapply(spobj@polygons, function(x) lapply(x@Polygons, function(y) y@coords))
        coords <- ldply(coords, data.frame)
        names(coords) <- c("x","y")
        return(coords)
    }
    if(class(spobj) == "data.frame"){
        if(all(c("x", "y") %in% tolower(names(spobj)))){
            return(spobj[c("x", "y")])
        }
        else{
            stop("Dataframe provided does not have x, y columns")
        }
    }
    stop("Class of spatial argument is not supported. Need SpatialLinesDataFrame or SpatialPolygonsDataFrame")
}

# reads in xy data frame of coordinates and a distance value to split line on
split <- function(xydf, dist){
    modck <- function(change, mod){
        if(change<0){
            return(mod*-1)
        }
        else{
            return(mod)
        }
    }
    x <- c()
    y <- c()
    end <- c()
    rem <- 0
    for(i in 1:nrow(xydf)){
        if(i == 1){
            x <- c(x, xydf$x[i])
            y <- c(y, xydf$y[i])
            end <- c(end,1)
        }
        if(i != 1){
            cx <- xydf$x[i] - xydf$x[i-1]
            cy <- xydf$y[i] - xydf$y[i-1]
            if(cx & cy != 0){
                len <- sqrt((cx^2) + (cy^2)) + rem
                segs <- len %/% dist
                if(segs == 0){
                    rem <- len
                }
                else{
                    m <- cx/cy
                    ymod <- dist / (sqrt((m^2)+1))
                    xmod <- ymod * abs(m)
                    yremsub <- rem / (sqrt((m^2)+1))
                    xremsub <- yremsub * abs(m)
                    xmod <- modck(cx, xmod)
                    ymod <- modck(cy, ymod)
                    xremsub <- modck(cx, xremsub)
                    yremsub <- modck(cy, yremsub)
                    xnew<-seq(xydf$x[i-1] - xremsub, xydf$x[i-1] + (xmod * segs), by = xmod)[-1]
                    ynew<-seq(xydf$y[i-1] - yremsub, xydf$y[i-1] + (ymod * segs), by = ymod)[-1]
                    rem<-sqrt((xydf$x[i] - tail(xnew,1))^2 + (xydf$y[i] - tail(ynew,1))^2)
                    x <- c(x, xnew)
                    y <- c(y, ynew)
                    end <- c(end, rep(1, length(xnew)))
                    }
            }
            if(cx != 0 & cy == 0){
                len <- cx + rem
                segs <- len %/% dist
                if(segs == 0){
                    rem <- len
                }
                else{
                    xmod <- dist
                    ymod <- 0
                    xmod <- modck(cx, xmod)
                    xremsub <- modck(cx, xremsub)
                    yremsub <- 0     
                    xnew <- seq(xydf$x[i-1] - rem, xydf$x[i-1] + (xmod * segs), by = xmod)[-1]
                    ynew <- rep(xydf$y[i-1], segs)
                    rem <- xydf$x[i] - tail(xnew,1)
                    x <- c(x, xnew)
                    y <- c(y, ynew)
                    end <- c(end, rep(1, length(xnew)))
                }
            }
            if(cx == 0 & cy != 0){
                len <- cy + rem
                segs <- len %/% dist
                if(segs == 0){
                    rem <- len
                }
                else{
                    xmod <- 0
                    ymod <- dist
                    xmod <- modck(cx, xmod)
                    xremsub <- modck(cx, xremsub)
                    yremsub <- 0    
                    xnew <- rep(xydf$x[i-1], segs) 
                    ynew <- seq(xydf$y[i-1] - rem, xydf$y[i-1] + (ymod * segs), by = ymod)[-1]
                    rem <- xydf$y[i] - tail(ynew,1)
                    x <- c(x, xnew)
                    y <- c(y, ynew)
                    end <- c(end, rep(1, length(ynew)))
                }
            }
            x<-c(x, xydf$x[i])
            y<-c(y, xydf$y[i])
            if(i != nrow(xydf)){
                end <- c(end, 0)    
                }
            else{
                end <- c(end, 1) 
                }
        }
    }
    return(data.frame(x = x, y = y, end = end))
}

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

verticalSplit <- function(spobj, xdist, start = T){
    'for all x coordinates
         if x value == split value
             then split y == y value
         if next x value is greater than split value and current x value is less than y value
             then get slope of line between both two x values
             caculate y split value at the x split value'
}
