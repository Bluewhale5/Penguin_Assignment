
#Function to create interaction plot

plot_mass_figure <- function(penguins_mass){
  penguins_mass %>% 
    ggplot(aes(x = species, color = sex, shape = sex, fill = sex, group = sex, y = body_mass_g)) +
    geom_jitter(alpha = 0.3, size = 2, position = position_jitter(width = 0.3, seed = 0)) +
    scale_x_discrete(labels=c("Adelie","Chinstrap","Gentoo")) +
    stat_summary(fun = mean, geom = "point", size = 2) +
    stat_summary(fun = mean, geom = "line", size = 0.8, alpha = 0.5) +
    stat_summary(fun.data = mean_cl_normal,  
                 geom = "errorbar", alpha = 1, width = 0.3, size = 0.8) + 
    scale_color_manual(values = c("turquoise4","chocolate1"), name = "Mean Body Mass", 
                        labels = c("Female", "Male"), guide = guide_legend(reverse=TRUE)) +
    scale_shape_discrete(name = "Mean Body Mass", 
                         labels = c("Female", "Male"), guide = guide_legend(reverse=TRUE)) +
    scale_fill_manual(values = c("turquoise4","chocolate1"), name = "Mean Body Mass", 
                      labels = c("Female", "Male"), guide = guide_legend(reverse=TRUE)) +
    guides(fill = "none") +
    labs(x = "Species",
         y = "Body Mass (g)",
         title = "Interaction Plot of Body Mass, Species and Sex of \n Penguins in the Palmer Archipelago:",
         caption = "Error bars represent 95% confidence intervals") +
    theme_gray() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.line = element_line(size = 0.5, colour = "black", linetype=1),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.direction= "horizontal")}


#Save the interaction plot figure as a PNG file

save_mass_plot_png <- function(penguins_mass, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  mass_interaction <- plot_mass_figure(penguins_mass)
  print(mass_interaction)
  dev.off()
}


#Save the interaction plot figure as an SVG file

save_mass_plot_svg <- function(penguins_mass, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  mass_interaction <- plot_mass_figure(penguins_mass)
  print(mass_interaction)
  dev.off()
}
