#' Use RHDHV colour schemes
#'
#' @param palette The name of a palette. Use `display = T` to view available palettes.
#' @param type 'v', 'nv', 'df'. Returns the HEX codes in a vector, a named vector or a dataframe of 2 columns.
#' @param display Set to TRUE to display available palettes.
#'
#' @return Returns a (named) vector or dataframe.
#' @export
#'
#' @examples
#' rhdhv_color_palette("KRW", "nv")
#' rhdhv_color_palette(display = T)
rhdhv_color_palette <- function(palette = "none",
                                type = "v",
                                display = F){
  
  if (palette == "none" & type == "v" & display == F) {
    stop("Please choose a palette to return. If you want to show which palettes are available, use 'display = T'")
  }
  
  palette_list <- list(
    # KRW kleuren
    KRW = data.frame(
      Name = c("Voldoet niet", "Voldoet", "Slecht", "Ontoereikend", "Matig", "Goed", "Niet toetsbaar", "Geen Oordeel", "n.v.t."),
      HEX = c("#FE0000", "#0070C0", "#FE0000", "#FFC000", "#FFFF01", "#00B050", "grey60", "white", "grey30")
    ),
    
    # Primary corporate colour scheme
    PrimaryCorporate = data.frame(
      Name = c("Primary Dark Blue", "Primary Green", "Primary Light Blue"),
      HEX = c("#00567D", "#a5c100", "#0086A8")
    ),
    
    # Secondary colour scheme
    SecondaryColours = data.frame(
      Name = c("Orange", "Red", "Bright Purple", "Yellow", "Green", "Light Purple", "Grey"),
      HEX = c("#f49600", "#e41f18", "#821066", "#ffd923", "#72981b", "#776db0", "#686867")
    ),
    
    # Combination 1
    Combination1 = data.frame(
      Name = c("Primary Dark Blue", "Primary Green", "Primary Light Blue"),
      HEX = c("#00567D", "#a5c100", "#0086A8")
    ),
    
    # Combination 2
    Combination2 = data.frame(
      Name = c("Primary Dark Blue", "Orange", "Red"),
      HEX = c("#00567D", "#f49600", "#e41f18")
    ),
    
    # Combination 3
    Combination3 = data.frame(
      Name = c("Primary Dark Blue", "Bright Purple", "Yellow"),
      HEX = c("#00567D", "#821066", "#ffd923")
    ),
    
    # Combination 4
    Combination4 = data.frame(
      Name = c("Primary Dark Blue", "Green", "Light Purple"),
      HEX = c("#00567D", "#72981b", "#776db0")
    ),
    
    # Combination 5
    Combination5 = data.frame(
      Name = c("Primary Dark Blue", "Grey", "Primary Green"),
      HEX = c("#00567D", "#686867", "#a5c100")
    ),
    
    # Combination 6
    Combination6 = data.frame(
      Name = c("Primary Green", "Orange", "Red"),
      HEX = c("#a5c100", "#f49600", "#e41f18")
    ),
    
    # Combination 7
    Combination7 = data.frame(
      Name = c("Primary Green", "Bright Purple", "Yellow"),
      HEX = c("#a5c100", "#821066", "#ffd923")
    ))
  
  palette_names <- names(palette_list)
  

  # Stop function ---------------------------------------------------------------------------------------------------
  stopifnot("Palette name not recognized" = palette %in% c("none", palette_names))
  stopifnot("Multiple palette selection not supported" = length(palette) == 1)
  stopifnot("Return type not supported" = type %in% c("df", "v", "nv"))
  stopifnot("Display must be logical" = is.logical(display))
  

  # Display all palettes --------------------------------------------------------------------------------------------
  if (display == TRUE) {
    # Function to create a ggplot for a given palette
    plot_palette <- function(palette_name, palette_df) {
      ggplot(palette_df, aes(x = 1, y = Name, fill = HEX)) +
        geom_tile(color = "white") +
        geom_text(aes(label = HEX), size = 1) +
        scale_fill_identity() +
        labs(title = palette_name, x = NULL, y = NULL) +
        theme_minimal() +
        theme(plot.title = element_text(size = 8, face = "bold"),
              axis.text.y = element_text(size = 8),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              panel.grid = element_blank())
    }
    
    # Create and display plots for each palette
    palette_plots <- lapply(names(palette_list), function(palette_name) {
      plot_palette(palette_name, palette_list[[palette_name]])
    })
    
    # Display the plots
    nflplotR::ggpreview(plot_grid(plotlist = palette_plots, ncol = 1, align = "hv"),
                        height = 400,
                        width = 200,
                        units = "mm")
  }
  
  # Select palettes -------------------------------------------------------------------------------------------------
  if(palette != "none") {
    call_palette <- palette_list[[palette]]
  } else {
    call_palette <- NULL
  }

  # Create type of return -------------------------------------------------------------------------------------------
  if(!is.null(call_palette)) {
    if(type == "v") {
      # Return vector of hex codes
      return_palette <- call_palette$HEX
    } else if(type == "df") {
      # Return dataframe
      return_palette <- call_palette
    } else if(type == "nv") {
      return_palette <- setNames(as.character(call_palette$HEX), call_palette$Name)
    }
    
    return(return_palette)
  }
  
}
