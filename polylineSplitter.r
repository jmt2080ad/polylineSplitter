### requires plyr, rgdal

coordBuild<-function(spobj){
  if(class(spobj) == "SpatialLinesDataFrame"){
    coords <- lapply(l@lines, function(x) lapply(x@Lines, function(y) y@coords))
    coods <- ldply(coords, data.frame)  
  }
  else{
    stop("Argument class is not supported. Need SpatialLinesDataFrame")
  }
}

split<-function(spobj, dist = 40){
  xydf<-coordBuild(spobj)
  modck<-function(change, mod){
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
      x<-c(x, xydf$x[i])
      y<-c(y, xydf$y[i])
      end<-c(end,1)
    }
    if(i != 1){
      cx <- xydf$x[i] - xydf$x[i-1]
      cy <- xydf$y[i] - xydf$y[i-1]
      len <- sqrt((cx^2) + (cy^2)) + rem
      segs <- len %/% dist
      if(segs == 0){
        rem <- sqrt((cx^2) + (cy^2)) + rem
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

splitLines<- function(xydf, dist = 40){
  xydf<-coordBuild(spobj)
  spoints<-split(xydf, dist)
  linelist<-list()
  lineslist<-list()
  id = 1
  j = 1
  for(i in 1:(nrow(spoints)-1)){
    linelist[j]<-Line(spoints[c(i,i+1),c(1:2)])
    j = j + 1
    if(spoints[i+1,3] == 1){ 
      lineslist[id]<-Lines(linelist, ID = id)
      id = id+1
      linelist<-list()
      j = 1
      plot(SpatialLines(lineslist[id-1]), add = T, col = 'red')
    }
  }
  return(SpatialLinesDataFrame(SpatialLines(lineslist), data = data.frame(id = 1:length(lineslist))))
}

splitPoints<-function(xydf, dist = 40){
  xydf<-coordBuild(spobj)
  spoints<-split(xydf, dist)
  return(SpatialPointDataFrame(SpatialPoints(data.frame(x=spoints$x, y=spoints$y)), data = spoints[,1:2]))
}
