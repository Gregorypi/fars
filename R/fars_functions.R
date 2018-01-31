

#' loads or "reads" in fars data
#'
#' \code{fars_read} loads data from csv.
#'
#'  This function loads data from a comma separated file taken from the  US
#'  National Highway Traffic Safetyc Administration's Fatality Analysis
#'  Reporting System (FARS).
#'
#' @import dplyr
#'
#' @note If the file or permission to it does not exist, an error is produced.
#'
#' @param The name of the file to be loaded.
#' @return dataframe containing the selected file's data.
#'
#' @examples
#' full_filename <- system.file('extdata', 'accident_2014.csv.bz2',
#'                              package = 'farsdata')
#' fars_read(filename = full_filename)
#'
#' \dontrun{
#' fars_read(filename = 'filedoesnotexist')
#' }
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' produces a string represetation of a valid fars file
#'
#' \code{make_filename} produces a string containing a valid filename
#'
#' This function requires a year as input and returns a full FARS filename.
#'
#' @note If the year cannot be coerced to integer, NA is used.
#'
#' @param year An integer, or a string value representing a year
#' @return a string that appends the input year to a filename
#' to provide the full name of a valid fars file
#' @examples
#'#' make_filename(year = 2016)
#'
#' \dontrun{
#' make_filename(year = 'two thousand sixteen') #  error
#' }
#'
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' reads fars files for one or more years.
#'
#' \code{fars_read_years} produces a list of FARS data containing months and years.
#'
#' for the given years fars file produces a dataframe of year and month observations.
#'
#' @import dplyr
#'
#' @note If the file for a specified year, or permission to it, does not exist, an error is produced.
#'
#' @param one or more years as integer or string
#' @importFrom magrittr "%>%"
#' @return a list of data frames
#' @examples
#' fars_read_years(years = c(2013, 2014, 2015))
#' fars_read_years(years = 2013)
#'
#' \dontrun{
#' fars_read_years(years = 1066) # error
#' }
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' summarise of contents of one or more fars Files.
#'
#' \code{fars_summarize_years} create a summary of FARS years and
#'   months
#'
#' This function takes an input years, and produces a summary table that
#' contains the number of observations for each month/year
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @note If the file for a specified year, or permission to it, does not exist, an error is produced.
#'
#' @param one or more years as integer or string
#' @return This function returns a data frame containing summary information
#' @examples
#' fars_summarize_years(years = c(2013, 2014, 2015))
#' fars_summarize_years(years = 2013)
#'
#' \dontrun{
#' fars_summarize_years(years = 1066)
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' plot fars data on map.
#'
#' \code{fars_map_state} maps fars data for each year and to a state
#'
#' takes a state number and a year, and creates a map image plotting
#' the fars data point by long and lat, require mapdata package.
#'
#' @import dplyr
#' @import maps
#' @importFrom graphics points
#'
#' @note If the \code{state.num} provided is not in the FARS file, an error is produced.
#' @note If there are no fatal crashes for the state and year, a message is provided.
#' @note LONGITUD values > 900 are set to NA, LATITUDE values > 90 are set to NA
#'
#' @param state.num integer referencing a state.
#' @param year  An integer, or a string
#' @return NULL
#' @examples
#' fars_map_state(22, 2015)
#'
#' \dontrun{
#' fars_map_state(1066, 0)   # error
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
