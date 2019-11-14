library(igraph)
library(WGCNA)
library(rgl)
library(tcltk)

## Saving the tkplot() 
g <- make_star(10, center=10) 
E(g)$width <- sample(1:10, ecount(g), replace=TRUE)
lay <- layout_nicely(g)

id <- tkplot(g, layout=lay)
canvas <- tk_canvas(id)
tcltk::tkpostscript(canvas, file="C:/Users/Jamila/Documents/UTRGV/Fall 2019/Senior Project/output.eps")
tk_close(id)

#3D visualization: with coordinates, no pop-up
rglplot(g)

## Setting the coordinates title
g <- make_ring(10)
id <- tkplot(make_ring(10), canvas.width=450, canvas.height=500)

canvas <- tk_canvas(id)
padding <- 20
coords <- norm_coords(layout_in_circle(g), 0+padding, 450-padding,
                      50+padding, 500-padding)
tk_set_coords(id, coords)

width <- as.numeric(tkcget(canvas, "-width"))
height <- as.numeric(tkcget(canvas, "-height"))
tkcreate(canvas, "text", width/2, 25, text="Modular Visualization",
         justify="center", font=tcltk::tkfont.create(family="helvetica",
                                                     size=20,weight="bold"))



#Undirected small cluster; very plain and no interaction
small_cluster <- make_tree(20, 2, mode="undirected")
plot(small_cluster, layout=layout_with_drl, vertex.size=6, vertex.label=NA)



