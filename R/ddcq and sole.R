# This EXECUTE script screens the sole fishery of the North Sea for density-dependent
# changes in FPUE (finshing mortality per unit effort, here called catchability,
# or q). If q or FPUE changes with fish abundance, we speak of density dependent
# changes in catchability (ddcq).

# It starts by establishing a time series of FPUE by dividing assessment F by
# efforts of various fleets from STECF. It seeks to establish if these time
# series are rather constant, if the rise with time (indicating technological
# creep) or are dependent on abundance (i.e. SSB or TSB, which indicates ddcq)

# Add a new line here for testing Git.

# (1) Is FPUE of sole constant or does it change with time and/or abundance?----

  # check catch composition of sole in STECF landings. Get data at
      # https://stecf.jrc.ec.europa.eu/ewg1413
      # use snose package to load and convert STECF file to long format
  library(reshape2)
  #library(sNoSeR)
  file_location_stecf_landings <- 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\input data\\STECF 2014\\Landings_by_ICES_rectangle.csv'
  landings <- get_stecf_landings_per_rectangle(file = file_location_stecf_landings, nose_only = T, deep = F, fdf = F, format_long = T)

    # calculate contribution of sole to total catch per fleet, rectangle and year
  library(dplyr)
  library(magrittr)

        #  ## For testing, specify input values for sNoSeR function aggr_share_of():
        #  data <- landings
        #  id.vars <- c('annex', 'reg_area_cod', 'reg_gear_cod', 'specon', 'vessel_length', 'rectangle', 'country', 'year')
        #  col <- 'species'
        #  case <- 'SOL'
        #  value <- 'landings'
  shares <- aggr_share_of(data = landings, id.vars = c('annex', 'reg_area_cod', 'reg_gear_cod', 'specon', 'vessel_length', 'rectangle', 'country', 'year'),
                value = 'landings', col = 'species', case = 'SOL')
  if(max(shares$share, na.rm = TRUE) != 1)  {warnings('Shares calculation failed!')}

  # plot share of sole in total catch and seek a pattern
  library(ggplot2)
  qplot(data = shares[shares$share != 0,], share) + geom_histogram() + facet_wrap(~ reg_gear_cod, scales = 'free')
  qplot(data = shares, y = share, x = reg_gear_cod) + geom_boxplot()
  qplot(data = shares, y = share,x = rectangle) + geom_point() + facet_wrap( ~ reg_gear_cod)
  # [!!! Consider plotting share of sole as colour in a map.]




  # get effort
  source(file = 'P:\\Offlineordner\\Promotion III -- Technological Creep\\03--Severe stats--JUL2015\\Script\\get Effort.r')
  effort2 <- get_stecf_landings_per_rectangle()

  # Read B and F (total and per age class) from sole assessment (WGNSSK 2015).

  sole_total <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\03--Severe stats--JUL2015\\Input\\Stock Assessment Data\\Sole in Subarea IV.csv')
  names(sole_total) <- tolower(names(sole_total))
  sole_total$year <- as.integer(as.character(sole_total$year))

  sole_F_at_age <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\WGNSSK15-Table 10.3.1. North Sea sole. Harvest (F).csv')
  sole_weight_at_age <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\WGNSSK15-Table 10.2.5. North Sea sole. Stock weight at age (kg).csv')
  sole_number_at_age <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\WGNSSK15-Table 10.4.1. North Sea sole. Stock numbers (thousands).csv')
