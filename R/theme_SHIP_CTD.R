

theme_SHIP_CTD <- function(base_size=14, base_family="Lucida Sans") {
  library(grid)
  library(ggthemes)
  library(extrafont)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(panel.grid.minor = element_line(colour = "grey", linewidth = 0.5),
            panel.grid.major = element_line(colour = "grey", linewidth = 0.2),
            axis.text = element_text(color = "black"),
            axis.title = element_text()
            #aspect.ratio = 6/5
    ))

}
