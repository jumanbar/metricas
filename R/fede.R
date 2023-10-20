library(fmsb)
create_beautiful_radarchart <- function(data, color = "#00AFBB",
                                                                             vlabels = colnames(data), vlcex = 0.7,
                                                                             caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey",
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Define colors and titles
#colors <- c("#00AFBB", "#E7B800", "#FC4E07", "grey", "red")
colors <- c("6","4", "orange", "3", "5")
titles <- c("PMLv2", "PMLv2 (Ec+Ei)", "MODIS", "INTA-SEPA", "INIA-GRAS")

jpeg(file = "Figure 5.jpg", width = 3000, height = 2000, units = "px", res = 300)

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,3), cex=1)

# Create the radar chart
for(i in 1:5){
  create_beautiful_radarchart(
    data = D[c(1, 2, i+2), ], caxislabels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    color = colors[i], title = titles[i]
  )
}
par(op)
dev.off()
