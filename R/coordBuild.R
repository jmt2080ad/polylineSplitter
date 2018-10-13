#' @title build x/y coordinates data.frame
#' @description to be filled
#' @param spobj sp or sf object
#' @return x/y data.frame
#' @import sp
#' @import plyr
coordBuild <- function(spobj){
    if("LINESTRING" %in% class(spobj)) {
      return(setNames(data.frame(sf::st_zm(sf::st_coordinates(spobj))), c("x", "y")))
    }
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
