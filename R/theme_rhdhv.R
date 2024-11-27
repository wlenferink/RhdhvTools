#' theme_rhdhv
#'
#' @param x_labels One of 'h' (horizontal), 'v' (vertical), 'd' (diagonal)
#' @param color_scheme A number, currently only 1
#' @param grid_lines One of 'none', 'both', 'x' (vertical), 'y' (horizontal).
#' @param grid_linetype One of GGPlot-supported linetypes (e.g., solid, dotted, etc.).
#' @param text_size One of 's' (small), 'm' (medium), 'l' (large).
#' @param legend_position One of bottom, left, right, top.
#'
#' @return a ggplot theme.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(1, 2, 3)
#' 
#' ggplot(aes(x = x, y = y)) +
#'   geom_point() +
#'   theme_rhdhv()
theme_rhdhv <- function (x_labels = "h", # h, v, d
                         color_scheme = 1,
                         grid_lines = "none", # none, x, y, both
                         grid_linetype = "solid", # ggplot option
                         text_size = "m", # s, m, l
                         legend_position = "bottom" # ggplot option
) {
  
  # Info and reference material -------------------------------------------------------------------------------------

  # A-4 sizes and margins of RHDHV report format in portrait
  # Width = 21 cm
  # Height = 29.7 cm
  # Margins T, L, B, R = 15, 2.2, 1.4, 2.2 cm
  
  # A-4 sizes and margins of RHDHV report format in landscape
  # Width = 21 cm
  # Height = 29.7 cm
  # Margins T, L, B, R = 2.2, 1.4, 2.2, 15 cm
  
  # Text size and type
  # Body Arial 10pt
  # H1 Arial 14 pt
  # H2 Arial 13 pt
  # H3 Arial 12 pt
  # Caption Arial 8 pt


  # General settings (required in all plots) ------------------------------------------------------------------------
  ## Text size ----
  text_size_plot <- case_when(text_size == "s" ~ 8,
                              text_size == "m" ~ 10,
                              text_size == "l" ~ 12)
  
  text_size_caption <- text_size_plot
  text_size_subtitle <- text_size_plot + 1
  text_size_title <- text_size_plot + 3
  
  ## Text font ----
  text_font <- "sans"
  
  ## X label position ----
  x_angle <- case_when(x_labels == "h" ~ 0,
                       x_labels == "v" ~ 90,
                       x_labels == "d" ~ 45)
  x_hjust <- case_when(x_labels == "h" ~ 0.5,
                       x_labels == "v" ~ 1,
                       x_labels == "d" ~ 1)
  x_vjust <- case_when(x_labels == "h" ~ 1,
                       x_labels == "v" ~ 0.5,
                       x_labels == "d" ~ 1)
  

  # Colour settings (data-independent color scheme) -----------------------------------------------------------------
  ## Schemes options ----
  c_1 <- c("#00567D", "#F2F6DB")


  ## Scheme of choice ----
  use_colors <- case_when(color_scheme == 1 ~ c_1,
                          .default = NULL)
  

  # Base theme build-up ---------------------------------------------------------------------------------------------
  returnTheme <- theme(
    
    ## Adjust plot titles ----
    plot.title = element_text(size = text_size_title, 
                              face = "bold",
                              family = text_font,
                              colour = use_colors[1]),
    plot.subtitle = element_text(size = text_size_subtitle,
                                 family = text_font,
                                 colour = use_colors[1]),
    plot.caption = element_text(size = text_size_caption,
                                family = text_font,
                                colour = use_colors[1]),
    
    ## Adjust axis titles ----
    axis.title = element_text(size = text_size_plot, 
                              face = "bold", 
                              family = text_font,
                              colour = use_colors[1]),
    
    ## Adjust axis text ----
    axis.text = element_text(size = text_size_plot,
                             family = text_font,
                             colour = use_colors[1]),
    axis.text.y = element_text(),
    axis.text.x = element_text(angle = x_angle,
                               hjust = x_hjust,
                               vjust = x_vjust),
    
    ## Adjust plot background ----
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, 
                                colour = use_colors[1]),
    axis.line = element_line(colour = NA),
    plot.background = element_rect(fill = "white"),
    
    ## Adjust facet text ----
    strip.text = element_text(size = text_size_plot, 
                              colour = use_colors[1], 
                              face = "bold"),
    
    ## Adjust facet strips ----
    strip.background = element_rect(color = NA, 
                                    fill = use_colors[2], 
                                    linewidth = 0.5, 
                                    linetype="solid")
  )
  

  # Grid lines add-on ------------------------------------------------------------------------------------------------
  if (!grid_lines == "none") {
    
    ## Grid line options ----
    x_grid_width = case_when(grid_lines == "none" ~ 0,
                             grid_lines == "x" ~ 1,
                             grid_lines == "both" ~ 1)
    y_grid_width = case_when(grid_lines == "none" ~ 0,
                             grid_lines == "y" ~ 1,
                             grid_lines == "both" ~ 1)
    
    
    ## Adjust grid lines ----
    returnTheme <- returnTheme + theme(
      panel.grid.major.x = element_line(color = alpha("#00567D", 0.25),
                                        linetype = grid_linetype,
                                        linewidth = x_grid_width),
      panel.grid.minor.x = element_line(color = alpha("#00567D", 0.25),
                                        linetype = grid_linetype,
                                        linewidth = x_grid_width * 0.75),
      panel.grid.major.y = element_line(color = alpha("#00567D", 0.25),
                                        linetype = grid_linetype,
                                        linewidth = y_grid_width),
      panel.grid.minor.y = element_line(color = alpha("#00567D", 0.25),
                                        linetype = grid_linetype,
                                        linewidth = y_grid_width * 0.75)
    )
    
    # Legend add-on ---------------------------------------------------------------------------------------------------
    if(!legend_position == "none") {
      
      ## Legend options ----
      #
      
      ## Adjust legend ----
      returnTheme <- returnTheme + theme(
        legend.text = element_text(size = text_size_plot, colour = use_colors[1]),
        legend.title = element_blank(),
        legend.position = legend_position,
        legend.background = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'), 
        legend.spacing.y = unit(0.1, 'cm'),
        legend.key = element_blank()
      )
    } else {
      returnTheme <- returnTheme + theme(
        legend.position = legend_position
      )
    }
    
  }
  
  return(returnTheme)

}

