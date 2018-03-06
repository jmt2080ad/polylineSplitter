#' @title reads in xy data frame of coordinates and a distance value to split line on
#' @param xydf dataframe with x and y coords
#' @param dist distance in units of coords
#' @return split version of xydf
#' @import sp
#' @import plyr
#' @export
#'
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

