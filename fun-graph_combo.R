# Function to plot several graphs together
# By Eliette 2019.02.25

# Input variables
# a, b, ... the plots to combine in a grid
# ygrob : the global y-axis label 
# xgrob : the global x-axis label
# nc : number of columns in the grid

graph_combo = function (a,b, ygrob, xgrob, nc, ...){
  
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(gridExtra)
  
  
  plot = plot_grid(a,b, ..., labels = "AUTO", ncol = nc, align = "h")
  
  
  y.grob <- textGrob(ygrob, 
                     gp=gpar(fontface="bold", fontsize=10),rot=90)
  
  x.grob <- textGrob(xgrob, 
                     gp=gpar(fontface="bold", fontsize=10))
  
  
  z = grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
  
return(z)
  
}

