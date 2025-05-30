% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_grid_cells.R
\name{plot_grid_cells}
\alias{plot_grid_cells}
\title{Plot Grid Cell Predictors generated from Accelerometry Data}
\usage{
plot_grid_cells(grid_cell_predictors)
}
\arguments{
\item{grid_cell_predictors}{A data frame of grid cell predictors generated from \code{compute_grid_cells} function}
}
\value{
A ggplot2 plot of the grid cell predictors
}
\description{
This function processes raw accelerometry data and creates predictors
using a grid-based approach with optional lagging.
}
\examples{
# Load example walking bout data
data(walking_data)

# Run function to get grid cells
res <- compute_grid_cells(walking_data, lags = c(0.15, 0.30, 0.45), cell_size = 0.25, max_vm = 3)

# Plot the grid cell predictors
plot_grid_cells(res)
}
