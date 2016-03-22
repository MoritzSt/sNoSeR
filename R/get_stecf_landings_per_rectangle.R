#' Get and convert STECF landings and effort per rectangle data
#'
#' \code{get_stecf_landings_per_rectangle} takes the downloaded STEFC data and
#' converts it to long format. Uppon request, it can subset data for the North
#' Sea only and remove data collected under the 'DEEP' specification.
#'
#' @param file Define location where STECF landings per rectangle data is stored
#' as a standard csv file, as downloaded from http://stecf.jrc.ec.europa.eu/c/
#' document_library/get_file?uuid=015650e2-8d27-4c69-922c-2a15ba4d8b5a&groupId=43805
#' @param nose_only A logical indicating if you want all data or data from the
#' North Sea (ICES area IV) only (which is STECF reg gear code 3B in annex IIA).
#' @param deep A logical indicating if you want data of SPECON 'DEEP' to be
#' included. Standard value should be 'F' to avoid doubling data occurence.
#' @param fdf A logical indicating if you want data of 'fully documented
#' fisheries' to be included, such as 'FDFBAL', 'FDFIIA' and 'FDFIIC'.Standard
#' value should be 'F' to avoid doubling data occurence.
#' @param format_long A logical defining if you want the data to be transposed
#' to long format. Is do, specify \code{format_long <- 'T'}.


get_stecf_landings_per_rectangle <- function(file, nose_only, deep, fdf, format_long) {
  dat <- read.csv(file = file, stringsAsFactors = FALSE)  # load landings data from file
  dat$X <- NULL
  names(dat) <- tolower(names(dat))
  if(nose_only == T) {  # subset data for the greater North Sea, if intended
    dat <- dat[dat$annex == 'IIA',]
    dat <- dat[dat$reg_area_cod == '3B2',]
  }

  # remove various specons from dataset ot avoid doubled data
  no_dat <- c()
  if(deep == F) {
    no_dat <- c(no_dat, 'DEEP')
  }

  if(fdf == F) {
    no_dat <- c(no_dat, paste(unique(dat$specon)[grep(pattern = 'FDF', x = unique(dat$specon))]))
  }
  dat <- dat[is.element(el = dat$specon, set = no_dat)==FALSE,]


  # if intended, make data long formatted
  if(format_long == T) {
    #devtools::use_package("reshape2")  # load required package 'reshape2'
    if (!requireNamespace("reshape2", quietly = TRUE)) { # ...or return an error if not installed
      stop("reshape2 needed for this function to work. Please install it.",
           call. = FALSE)
    }
    names(dat)[grep(pattern = '2', x = names(dat), ignore.case = FALSE)] <-
      as.character(seq(from = 2003, length.out = length(names(dat)[grep(pattern = '2',
      x = names(dat), ignore.case = FALSE)])))  # replace year names

    dat2 <- melt(data = dat, id.vars = c(names(dat)[-grep(pattern = '2', x = names(dat))]) )
    names(dat2)[which(names(dat2)=='variable')]  <- 'year'
    names(dat2)[which(names(dat2)=='value')]  <- 'landings'

  }
  landings <- as.data.frame(dat2)  # return landings
}
