# functions for pulling blank data from the DB


### load libraries
library(amstools)
library(odbc)
library(dplyr)



#' getRaw
#'
#' @param from 
#'
#' @return A data table of raw blank data
#' @export
#'
#' @examples
getRaw <- function(from = '2014-09-01') {

  #Open DB connection
  db <- amstools::conNOSAMS()
  
  #Get raw blank data
  raw <- odbc::dbGetQuery(db, paste("
    SELECT runtime, target.tp_date_pressed, target.rec_num, 
      sample_name, target.tp_num, gf_co2_qty, 
      he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
    FROM snics_raw
    JOIN target ON target.tp_num = snics_raw.tp_num
    LEFT JOIN graphite
    ON target.osg_num = graphite.osg_num
    WHERE tp_date_pressed > '", from, "'
    AND target.rec_num IN (83028, 53804, 2138, 140548, 36168, 55101, 1081, 39246, 32490, 32491, 32492, 36947, 148820)
    "))
  
  #average by target and filter
  raw %>%
    group_by(tp_num) %>%
    summarize(
      he12c = mean(ifelse(ok_calc == 1, he12c, NA), na.rm = TRUE),
      he1412 = mean(ifelse(ok_calc == 1, he14_12, NA), na.rm = TRUE),
      flagged = ( (n() - sum(ok_calc == 1)) / n()) # fraction of runs flagged
    ) %>%
    mutate(c1412x = he1412 * 1e16) 

}

#' getNorm
#'
#' @param from 
#'
#' @return A data table of normalized blank data
#' @export
#'
#' @examples
getNorm <- function(from = '2014-09-01') {

  #Open DB connection
  db <- amstools::conNOSAMS()
  
  blanks.n =  dbGetQuery(db, paste("
        SELECT runtime, wheel, target.tp_date_pressed, sample_name,
            target.rec_num, target.tp_num, gf_co2_qty, 
            norm_ratio, int_err, ext_err, 
            blk_corr_method, fm_corr, sig_fm_corr, ss
          FROM snics_results
          JOIN target
          ON snics_results.tp_num = target.tp_num
          LEFT JOIN graphite
          ON target.osg_num = graphite.osg_num
          WHERE tp_date_pressed > '", from, "'
          AND target.rec_num IN (83028, 53804, 2138, 140548, 36168, 55101, 1081, 39246, 32490, 32491, 32492, 36947, 148820)
          "))
  
  blanks.n

}

#' combineBlanks
#'
#' @param raw 
#' @param norm 
#'
#' @return A data table of combined blank data
#' @export
#'
#' @examples
combineBlanks <- function(raw, norm) {

  blanks <- left_join(raw, norm, by="tp_num") %>%
    filter(norm_ratio > -99) %>%
    mutate(tp_date_pressed = as.Date(tp_date_pressed),
           type = ordered(recode(as.character(rec_num), 
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
           age = rcage(norm_ratio))
  blanks

}

#' getBlankData
#'
#' @param from 
#'
#' @return A data table of combined blank data
#' @export
#'
#' @examples
getBlankData <- function(from = '2014-09-01') {
	raw <- getRaw(from)
	norm <- getNorm(from)
	combineBlanks(raw, norm)
}