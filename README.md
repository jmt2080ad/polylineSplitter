# polylineSplitter
Polyline/polygon splitter tools for R. These functions will allow a user to pass SpatialLines or SpatialPolygons and split the line or polygon edge by a specific distance returning either points or line segments depending on which functions are used.

Useage can be see in the `test.r` file. These tools were designed for tasks were even measurements along lines or polygons perimeters are required. This might include tasks like defining river miles, defining sample locations along excavation sidewalls, or changing the shapes of lines or polygons based on even distances along those shapes (a form of simplification). 

Here are some examples:

This graphic shows how a polyline can be split into parts that are even length plus a remainder. 
![line_splitting]("./images/line_splitting.png")

This graphic shows how a polygon edge can be sampled at an even distance along its perimeter.
![polygon_sampling]("./images/polygon_sampling.png")

This graphic shows how a polygon can be simplifed.
![polygon_simplification.png]("./images/polygon_simplification.png")


