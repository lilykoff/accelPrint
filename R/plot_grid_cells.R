#' Plot Grid Cell Predictors generated from Accelerometry Data
#'
#' This function processes raw accelerometry data and creates predictors
#' using a grid-based approach with optional lagging.
#'
#' @importFrom dplyr mutate select across contains
#' @importFrom ggplot2 ggplot aes geom_rect theme_classic scale_x_continuous scale_y_continuous facet_grid theme
#' @importFrom tidyr pivot_wider separate_wider_delim
#' @importFrom utils tail
#' @importFrom viridis scale_fill_viridis
#'
#' @param grid_cell_predictors A data frame of grid cell predictors generated from `compute_grid_cells` function
#' @return A ggplot2 plot of the grid cell predictors
#' @export
#' @examples
#' # Load example walking bout data
#' data(walking_data)
#'
#' # Run function to get grid cells
#' res <- compute_grid_cells(walking_data, lags = c(0.15, 0.30, 0.45), cell_size = 0.25, max_vm = 3)
#'
#' # Plot the grid cell predictors
#' plot_grid_cells(res)
plot_grid_cells = function(grid_cell_predictors){
  second = name = vm = lag_vm = value = NULL
  rm(list = c("second",
              "name",
              "vm",
              "lag_vm",
              "value"))
  cnames = colnames(grid_cell_predictors %>% dplyr::select(-second))
  cells = sub("\\_.*", "", cnames) %>% unique()
  lags = sub(".*\\_", "", cnames) %>% unique()
  last = tail(cells, 1)
  last_label = sub(".*\\,(.+)\\].*", "\\1", last)
  labs = c(sub(".*[\\(\\[]([^,]+),.*", "\\1", cells), last_label)

  p = grid_cell_predictors %>%
    tidyr::pivot_longer(cols = -second) %>%
    tidyr::separate_wider_delim(name,
                                delim = "_",
                                names = c("vm", "lag_vm", "lag")) %>%
    dplyr::mutate(lag = paste0("Lag = ", lag, " samples")) %>%
    dplyr::mutate(vm = factor(vm, levels = cells),
           lag_vm = factor(lag_vm, levels = cells)) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("vm"), as.numeric)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(
      xmin = vm - 1,
      xmax = vm,
      ymin = lag_vm - 1,
      ymax = lag_vm,
      fill = value
    ),
    color = "black") +
    ggplot2::theme_classic() +
    ggplot2::scale_x_continuous(
      name = "Acceleration (g)",
      breaks = seq(0, length(cells), by = 1),
      labels = labs,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      name = "Lag acceleration (g)",
      breaks = seq(0, length(cells), by = 2),
      labels = labs[seq(1, length(labs), by = 2)],
      expand = c(0, 0)
    ) +
    viridis::scale_fill_viridis(name = "Number of points in cell", option = "C") +
    ggplot2::facet_grid(lag ~ .) +
    ggplot2::theme(legend.position = "bottom")
  return(p)
}


