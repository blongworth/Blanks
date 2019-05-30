# functions for pulling blank data from the DB


### load libraries
library(amstools)
library(odbc)
library(dplyr)



#' getRaw
#'
#' @param from 
#' @param recs A vector of rec_nums
#'
#' @return A data table of raw blank data
#' @export
#' @importFrom magrittr "%>%"
#'
getRaw <- function(from = as.Date('2014-09-01'),
                   recs = c(83028, 53804, 2138, 140548, 36168, 55101,
                            1081, 39246, 32490, 32491, 32492, 36947, 148820)) {

  #Open DB connection
  db <- amstools::conNOSAMS()
  
  #Get raw blank data
  
   query <- glue::glue_sql(
	    "SELECT runtime, target.tp_date_pressed, target.rec_num, 
               sample_name, target.tp_num, gf_co2_qty, 
               he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
             FROM snics_raw
             JOIN target ON target.tp_num = snics_raw.tp_num
             LEFT JOIN graphite
             ON target.osg_num = graphite.osg_num
             WHERE tp_date_pressed > {from}
             AND target.rec_num IN ({recs*})",
    from = from,
    recs = recs,
    .con = db)
  raw <- odbc::dbGetQuery(db, query)
  
  #average by target and filter
  raw %>%
    dplyr::group_by(tp_num) %>%
    dplyr::summarize(
      he12c = mean(ifelse(ok_calc == 1, he12c, NA), na.rm = TRUE),
      he1412 = mean(ifelse(ok_calc == 1, he14_12, NA), na.rm = TRUE),
      flagged = ( (length(ok_calc) - sum(ok_calc == 1)) / length(ok_calc)) # fraction of runs flagged
    ) %>%
    dplyr::mutate(c1412x = he1412 * 1e16) 

}

#' getNorm
#'
#' @param from A date object 
#' @param recs A vector of rec_nums
#'
#' @return A data table of normalized blank data
#' @export
#'
getNorm <- function(from = as.Date('2014-09-01'),
                   recs = c(83028, 53804, 2138, 140548, 36168, 55101,
                            1081, 39246, 32490, 32491, 32492, 36947, 148820)) {

  #Open DB connection
  db <- amstools::conNOSAMS()
  
  query <- glue::glue_sql(
	     "SELECT runtime, wheel, target.tp_date_pressed, sample_name,
                target.rec_num, target.tp_num, gf_co2_qty, 
                norm_ratio, int_err, ext_err, 
                blk_corr_method, fm_corr, sig_fm_corr, ss
              FROM snics_results
              JOIN target
              ON snics_results.tp_num = target.tp_num
              LEFT JOIN graphite
              ON target.osg_num = graphite.osg_num
                 WHERE tp_date_pressed > {from}
                 AND target.rec_num IN ({recs*})",
              from = from,
              recs = recs,
              .con = db)

  odbc::dbGetQuery(db, query)

}

#' combineBlanks
#'
#' @param raw a raw blank datatable from getRaw
#' @param norm A normalized blank datatable from getNorm
#'
#' @return A data table of combined blank data
#' @export
#' @importFrom magrittr "%>%"
#'
combineBlanks <- function(raw, norm) {

  dplyr::left_join(raw, norm, by="tp_num") %>%
    dplyr::filter(norm_ratio > -99) %>%
    dplyr::mutate(tp_date_pressed = as.Date(tp_date_pressed),
           type = ordered(dplyr::recode(as.character(rec_num), 
                                 "1081" = "C1", 
                                 "2138" = "TIRI-F", 
                                 "32490" = "JME",
                                 "32491" = "JME",
                                 "32492" = "JME",
                                 "36168" = "Acet", 
                                 "39246" = "C1", 
                                 "53804" = "C1", 
                                 "55101" = "Acet", 
                                 "83028" = "C1", 
                                 "140548" = "Acet",
                                 "36947" = "Old Ceylon",
                                 "148820" = "Ceylon"),
                          levels = c("Acet", "C1", "TIRI-F", "JME", 
  				   "Old Ceylon", "Ceylon")),
           merr = pmax(int_err, ext_err),
           system = toupper(substring(wheel, 1, 5)),
           age = amstools::rcage(norm_ratio))

}

#' getBlankData
#'
#' @param from A date object 
#'
#' @return A data table of combined blank data
#' @export
#'
getBlankData <- function(from = '2014-09-01') {
	raw <- blanks::getRaw(from)
	norm <- blanks::getNorm(from)
	blanks::combineBlanks(raw, norm)
}
