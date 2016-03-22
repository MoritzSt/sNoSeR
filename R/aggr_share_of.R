#' Calculates the share of something in an entirety of a dataframe.
#'
#' \code{aggr_share_of} calculate the share of a group of entries in the
#' entirety of entries one level above, such as the share of plaice in
#' landings of a particular fishing fleet per year.
#'
#' @param data Dataset to use.
#' @param id.vars A list of the variables to split data frame by, as as.quoted variables,
#' a formula or character vector (similar ddply in package plyr).
#' @param col Variable name that defines the case for which share is
#' to be calculated.
#' @param case Name of the case for which share in the entirety is to be calculated.
#' @param value What to aggregate, i.e. calculate the share of?
#' @import dplyr
#' @importFrom magrittr %>%
#' @export

aggr_share_of <- function(data, id.vars, col, case, value) {

    # calculate total value
    dots <- list()
    for (i in seq_along(id.vars)) {
      dots[[i]] <- {as.formula(paste0('~', id.vars[i]), env = new.env())}
    }
    result <- as.data.frame(data) %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise_(total = lazyeval::interp(~sum(var), var = as.name(value)))

    totals <- result


    # calculate subset value
    data <- data[data[,col] == case,]
    dots <- list()
    for (i in seq_along(id.vars)) {
      dots[[i]] <- {as.formula(paste0('~', id.vars[i]), env = new.env())}
    }
    result <- as.data.frame(data) %>%
      dplyr::group_by_(.dots = dots) %>%
      dplyr::summarise_(subset_total = lazyeval::interp(~sum(var), var = as.name(value)))

    subsets <- result

    # merge datasets and calculate share
    dat <- merge(totals, subsets, all.x = TRUE, all.y = TRUE)
    dat$share <- dat$subset_total / dat$total
    dat$subset_total <- NULL
    dat$total <- NULL

    dat  # return dat
}
