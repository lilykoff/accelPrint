#' Plot fingerprints from densities in accelerometry data
#'
#' This function takes in walking data and lags and plots "fingerprints"
#'
#' @importFrom dplyr mutate select ungroup group_by lag
#' @importFrom tidyr drop_na
#' @importFrom purrr map_dfr
#' @importFrom ggplot2 ggplot aes geom_point facet_grid labs scale_x_continuous scale_y_continuous theme element_blank
#' @importFrom viridis scale_color_viridis
#'
#' @param data A data frame with accelerometry values (e.g., x, y, z, and time), preferably from walking
#' @param lags vector of lag values in seconds
#' @param max_vm maximum vector magnitude for plotting, defaults to 3
#' @param sample_rate optional specification of sample rate (samples per sec), if not specified will be inferred
#' @return A plot of the fingerprints for each provided lag across all provided seconds
#' @export
#' @examples
#' # Load example walking bout data
#' data(walking_data)
#'
#' # Run the function
#' plot_fingerprints(walking_data, lags = c(0.15, 0.30, 0.45))
plot_fingerprints = function(data, lags, sample_rate = NULL, max_vm = 3){
  time = x = y = z = second = vm = lag_id = lag_vm = density = NULL
  rm(list = c(
    "time",
    "x",
    "y",
    "z",
    "second",
    "vm",
    "lag_id",
    "lag_vm",
    "density"))

  # check that data is a data frame
  assertthat::assert_that(
    is.data.frame(data),
    msg = "Data must be a data frame."
  )

  # check that lags are less than 1
  assertthat::assert_that(all(lags < 1) & all(lags > 0), msg = "Lags must be positive and less than 1 second")

  # make column names lowercase
  colnames(data) <- tolower(colnames(data))

  # check for time column
  time_col <- colnames(data)[grepl("time", colnames(data))]
  assertthat::assert_that(length(time_col) == 1, msg = "Data must have exactly one column containing 'time'.")
  data <- data %>% dplyr::rename(time = !!dplyr::sym(time_col))

  # check for x,y,z columns in data
  assertthat::assert_that(all(c("x", "y", "z") %in% colnames(data)), msg = "Data must contain x, y, and z columns (case-insensitive).")

  # check for vm column, if it doesn't exist, create it
  if (!"vm" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(vm = sqrt(x^2 + y^2 + z^2))
  }

  # add seconds column
  data <- data %>%
    dplyr::mutate(second = lubridate::floor_date(time, unit = "seconds"))


  # infer sample rate if not provided
  # see if there's an attribute (if gt3x file)
  if(is.null(sample_rate)) {
    obs_per_sec =
      data %>%
      dplyr::mutate(second = lubridate::floor_date(time, unit = "seconds")) %>%
      dplyr::count(second) %>%
      dplyr::pull(n)
    sample_rate = Mode(obs_per_sec)
    message(sprintf(
      "Sample rate not provided. Inferred sample rate: %s Hz",
      sample_rate
    ))
  }

  # check that sample rate is integer
  assertthat::assert_that(sample_rate%%1 == 0, msg = "Sample rate must be an integer.")

  # lags are lag (in seconds) * samples per second
  lags_samples = lags * sample_rate

  # check that lags are integers, if not, round them
  if(!all(lags_samples%%1 == 0)){
    lags_samples = round(lags_samples, 0)
    warning(sprintf(
      "Some lags multiplied by sample rate were not integers. Rounded to: %s",
      paste(lags_samples, collapse = ", ")
    ))
  }

  assertthat::assert_that(
    all(lags_samples < sample_rate),
    msg = "Lags mutliplied by sample rate must be less than sample rate."
  )


  lag_df =
    purrr::map_dfr(
      .x = lags_samples,
      .f = function(df, lag) {
        df %>%
          dplyr::mutate(vm = sqrt(x ^ 2 + y ^ 2 + z ^ 2)) %>%
          dplyr::group_by(second) %>%
          dplyr::mutate(lag_vm = dplyr::lag(vm, n = lag)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(lag_id = lag)
      },
      df = data
    )
  dens_df =
    lag_df %>%
    tidyr::drop_na() %>%
    dplyr::group_by(lag_id) %>%
    dplyr::group_modify(~.x %>% dplyr::mutate(density = get_density(vm, lag_vm, n = sample_rate))) %>%
    dplyr::ungroup()

  p = dens_df %>%
    dplyr::mutate(lag = paste0("Lag = ", round(lag_id / sample_rate, 2), " sec")) %>%
    ggplot2::ggplot(ggplot2::aes(x = vm, y = lag_vm, color = density)) +
    ggplot2::geom_point(size = .85) +
    viridis::scale_color_viridis(name = "# points", option = "C") +
    ggplot2::facet_grid(lag ~ .) +
    ggplot2::labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
    ggplot2::scale_x_continuous(limits = c(0, max_vm),
                                breaks = seq(0, max_vm, 0.5)) +
    scale_y_continuous(limits = c(0, max_vm),
                       breaks = seq(0, max_vm, 0.5)) +
    theme(legend.position = "bottom", panel.grid = ggplot2::element_blank())

  return(p)
}
