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
#' included. Default value is \code{fdf = FALSE} to avoid doubling data occurence.
#' @param fdf A logical indicating if you want data of 'fully documented
#' fisheries' to be included, such as 'FDFBAL', 'FDFIIA' and 'FDFIIC'. Default
#' value is \code{deep = FALSE} to avoid doubling data occurence.
#' @param format_long A logical defining if you want the data to be transposed
#' to long format. This is the case by default, thus \code{format_long <- TRUE}.
#' @export

# Do a test change here.

get_stecf_landings_per_rectangle <- function(file, nose_only, deep = FALSE, fdf = FALSE, format_long = TRUE) {
  dat <- read.csv(file = file, stringsAsFactors = FALSE)  # load landings data from file
  dat$X <- NULL
  names(dat) <- tolower(names(dat))
  names(dat) <- gsub(x = names(dat),
                          pattern = "\\.",
                          replacement = "_")

  if(nose_only == T) {  # subset data for the greater North Sea, if intended
    if(is.element('IIA', unique(dat$annex)) ) { dat <- dat[dat$annex == 'IIA',]}
    if(is.element('IIa', unique(dat$annex)) ) { dat <- dat[dat$annex == 'IIa',]}

    if(is.element('3b', unique(dat$reg_area_cod)) ) { dat <- dat[dat$reg_area_cod == '3b',]}
    if(is.element('3B', unique(dat$reg_area_cod)) ) { dat <- dat[dat$reg_area_cod == '3B',]}
    if(is.element('3b2', unique(dat$reg_area_cod)) ) { dat <- dat[dat$reg_area_cod == '3b2',]}
    if(is.element('3B2', unique(dat$reg_area_cod)) ) { dat <- dat[dat$reg_area_cod == '3B2',]}

    if(!is.element('IIA', unique(dat$annex)) &
    !is.element('IIa', unique(dat$annex)) &
    !is.element('3b', unique(dat$reg_area_cod)) &
    !is.element('3B', unique(dat$reg_area_cod))  &
    !is.element('3b2', unique(dat$reg_area_cod)) &
    !is.element('3B2', unique(dat$reg_area_cod)) ) {
      warnings('Could not identify North Sea data from Annex and Reg Gear Code!')
    }
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

    dat2 <- reshape2::melt(data = dat, id.vars = c(names(dat)[-grep(pattern = '2', x = names(dat))]) )
    names(dat2)[which(names(dat2)=='variable')]  <- 'year'
    dat2$year <- as.integer(as.character(dat2$year))
  }
  landings <- as.data.frame(dat2)  # return landings
}
