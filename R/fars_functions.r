#' @title fars_read
#'
#' @description
#' This function reads data from a file in csv format.
#' The function validates if the file exists but id doesn't show the progress when the data is
#' loaded from the file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string giving the file name (and full path) with data to be read
#'
#' @return if the file exists, the function returns a data frame with the data read
#'
#' @examples
#' \dontrun{
#' df <- fars_read("./data/data2015.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' @title make_filename
#'
#' @description
#' This create a the filename to be assigned to.
#' It doesn't validate if the parameter is a valid integer or even a valida number of year.
#'
#' @param year The year to be used as part of the name. It could be a string but it should be converted to a numeric.
#'
#' @return string containing the constructed filename
#'
#' @examples
#' fn <- make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' @title fars_read_years
#'
#' @description
#' This function load and summarize data from the file that corresponds to every year especified in the parameter.
#' It validates if every file exists and shows an error if it doesn't exists.
#' It uses mutate and select functions from dplyr package
#'
#' @importFrom dplyr mutate
#'
#' @param years A vector with the years to be processed.
#'
#' @return A data frame with the
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014,2015))
#' }
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}
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

#' @title fars_summarize_years
#'
#' @description
#' This function returns the quantity of incidets per month/year for every year especified in the parameter.
#' It uses bind_rows, group_by and summarize functions from dplyr package.
#' It uses spread function from tidyr package.
#'
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
#' @param years A vector with the years to be processed.
#'
#' @return A data frame with the summary
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014,2015))
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


#' @title fars_map_state
#'
#' @description
#' This function plots accidents in a specific year for a state in a map.
#' If the state code doesn't exists for the specified year, the function stops with an error.
#' It this functions from other packages:
#' 		dplyr::filter
#' 		maps::map
#'		graphics::points
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num State code to be searched
#' @param year Year to be loaded
#'
#' @return A map with points of every accident for the state.num indicated for the year specified.
#'
#' @examples
#' \dontrun{
#' fars_map_state(37,2015)
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
