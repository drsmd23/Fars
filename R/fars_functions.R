#' Read motor vehicle crash csv file 
#'
#' This function (fars_read) validates that an input file exists.
#' An error message is returned if the file does not exist.
#' If the file exists then the file is read using the read_csv function from the readr package (\code{\link[readr]{read_csv}}).
#' A data frame table is created using the tbl_df function from the dplyr package (\code{\link[dplyr]{tbl_df}}).
#'
#' @param filename A character string giving the path, name, and extension of a file to read in.
#' 
#' @return This function returns a data frame table containing data for motor vehicle crashes for a particular year if the input csv file exists.
#' 		An error message is returned if the file does not exist.
#'
#' @examples
#' \dontrun{
#' fars_read("D:/Data/accident_2013.csv.bz2")
#' fars_read(filename="D:/Data/accident_2013.csv.bz2")
#' }
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generate filename for motor vehicle crash year
#'
#' This function (make_filename) creates a string containing a filename.
#' The user input year is converted to an integer.
#' The year is then incorporated into a string with prefix "accident_" and suffix ".csv.bz2".
#'
#' @param year A string or integer giving the year to use in the filename
#' 
#' @return This function returns a string containing the filename for motor vehicle crashes in a particular year.
#' 		An warning message is returned if the input value cannot be coerced to an integer value.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename("2013")
#' make_filename(year=2013)
#' }
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read multiple years of motor vehicle crash data 
#'
#' This function (fars_read_years) accepts a vector of years or a single year as input.
#' Each element of the input vector is treated separately and a list of data frames is returned.
#' The year is converted to a filename using the make_filename function from this package (\code{\link{make_filename}}).
#' The file is then read using the fars_read function from this package (\code{\link{fars_read}}).
#' A column containing year is set to the year value contained in the filename using the mutate function from the dplyr package (\code{\link[dplyr]{mutate}}).
#' Only the MONTH and year columns for each file are returned by using the select function from the dplyr package (\code{\link[dplyr]{select}}).
#' A warning message is returned if a file does not exist.
#'
#' @param years A string or integer vector giving the years of motor vehicle crash files to read in.
#' 
#' @return This function returns a list of data frames if the motor vehicle crash year file exists.
#' 		An warning message is returned if a file does not exist in the current directory.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2014))
#' fars_read_years("2013")
#' fars_read_years(c("2013", "2014"))
#' fars_read_years(years=2013)
#' }
#'
#' @export
#' @import magrittr
#' @importFrom dplyr mutate select

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~ year) %>% 
                                dplyr::select_(.dots = c('MONTH', 'year'))
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize multiple years of motor vehicle crash data 
#'
#' This function (fars_summarize_years) accepts a vector of years or a single year as input.
#' Motor vehicle crash data for each year in the input vector is read using the fars_read_years function from this package (\code{\link{fars_read_years}}).
#' The list of dataframes are then merged using the bind_rows function from the dplyr package (\code{\link[dplyr]{bind_rows}}).
#' The resulting dataframe is grouped by the year and MONTH columns using the group_by function from the dplyr package (\code{\link[dplyr]{group_by}}).
#' The total number of records relating to each group is then calculated using the summarize function from the dplyr package (\code{\link[dplyr]{summarize}}).
#' The results are represented with months displayed along the rows and years along the columns by using the spread function from the tidyr package (\code{\link[tidyr]{spread}}).
#' An error message occurs if the there is no data file for a corresponding input year.
#'
#' @param years A string or integer vector giving the years of motor vehicle crash files to read in.
#' 
#' @return This function returns a data frame containing the number of motor vehicle crashes for each year and month.
#' 	An error message occurs if the there is no data file for a corresponding input year.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013, 2014))
#' fars_summarize_years("2013")
#' fars_summarize_years(c("2013", "2014"))
#' fars_summarize_years(years=2013)
#' }
#'
#' @export
#' @import magrittr
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread 

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by_(~ year, ~ MONTH) %>% 
                dplyr::summarize_(n = ~ n()) %>%
                tidyr::spread_(key_col = 'year', value_col = 'n')
}

#' Plot locations of motor vehicle crashes in a state for a given year
#'
#' This function (fars_map_state) accepts a single state number and a single year as input.
#' The year is converted to a filename using the make_filename function from this package (\code{\link{make_filename}}).
#' Motor vehicle crash data for the year is read using the fars_read function from this package (\code{\link{fars_read}}).
#' The user input state number is converted to an integer.
#' The user input state number is checked to see if it is in the list of unique state numbers detected in the data.
#' An error message occurs if the user input state number is not detected in the data.
#' A subset of the data with only the user input state number values is created using the filter function from the dplyr package (\code{\link[dplyr]{filter}}).
#' A message is given if there are no motor vehicle crashes to plot.
#' The value for the column LONGITUD is set to NA if it is greater than 900.
#' The value for the column LATITUDE is set to NA if it is greater than 90.
#' The motor vehicle accident locations based on the LONGITUD and LATITUDE columns are then plotted on a map of the state.
#' The graphs are created using the map function from the maps package (\code{\link[maps]{map}}) and the points function from the graphics package (\code{\link[graphics]{points}}).
#'
#' @param state.num A string or integer giving the number of the state to plot motor vehicle crashes for.
#' @param year A string or integer giving the year to plot motor vehicle crashes for.
#' 
#' @return This function returns a map with the locations of of motor vehicle crashes in a state for a particular year.
#' 		An error message occurs if the state or year input values are not valid
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state("1", "2013")
#' fars_map_state(state.num=1,year=2013)
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
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