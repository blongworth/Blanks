# shiny app to get blanks from db, summarize and plot

library(amstools)
library(RODBC)
library(dplyr)
library(ggplot2)
library(knitr)
library(shiny)


#Open DB connection
db <- conNOSAMS()

#Get raw blank data
c1r =  sqlQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, sample_name, target.tp_num, gf_co2_qty, 
          he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
        FROM snics_raw, target, graphite
      	WHERE target.tp_num = snics_raw.tp_num
        AND target.osg_num = graphite.osg_num
        AND tp_date_pressed > '2014-09-01'
        AND target.rec_num = 83028
        AND graphite.gf_co2_qty > 40
        AND graphite_lab = 1
        "))

acetr =  sqlQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, sample_name, target.tp_num, gf_co2_qty, 
          he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
        FROM snics_raw, target, graphite
      	WHERE target.tp_num = snics_raw.tp_num
        AND target.osg_num = graphite.osg_num
        AND sample_name LIKE 'Acet%'
        AND tp_date_pressed > '2014-09-01'
        AND graphite.gf_co2_qty > 40
        AND graphite_lab = 1
      "))

jmer =  sqlQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, sample_name, target.tp_num, 
          he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
        FROM snics_raw, target
      	WHERE target.tp_num = snics_raw.tp_num
        AND target.rec_num = 32491
        AND tp_date_pressed > '2014-09-01'"))

#add type columns, combine data frames
c1r$type <- "C1"
acetr$type <- "Acet"
jmer$type <- "JME"
jmer$gf_co2_qty <- NA
blanks.r <- rbind(c1r, acetr, jmer)
blanks.r$type <- ordered(blanks.r$type, levels = c("Acet", "C1", "JME"))

#average by target and filter
blanks.a <- blanks.r %>%
  mutate(system = ifelse(grepl("CFAMS", wheel), "CFAMS", "USAMS")) %>%
  group_by(tp_num, tp_date_pressed, system, type) %>%
  summarize(
    he12c = mean(ifelse(ok_calc == 1, he12c, NA), na.rm = TRUE),
    he1412 = mean(ifelse(ok_calc == 1, he14_12, NA), na.rm = TRUE),
    flagged = ( (n() - sum(ok_calc == 1)) / n()) # fraction of runs flagged
  ) %>%
  mutate(c1412x = he1412 * 1e16) %>%
  filter(c1412x < 250) 

###
#Get normalized blank data
###

c1 =  sqlQuery(db, paste("
      SELECT runtime, wheel, target.tp_date_pressed, sample_name, target.rec_num, target.tp_num, gf_co2_qty, 
          norm_ratio, int_err, ext_err, blk_corr_method, fm_corr, sig_fm_corr, ss
        FROM snics_results, target, graphite
        WHERE target.tp_num = snics_results.tp_num
        AND target.osg_num = graphite.osg_num
        AND tp_date_pressed > '2014-09-01'
        AND target.rec_num = 83028
        AND graphite.gf_co2_qty > 40
        AND graphite_lab = 1
        "))

acet =  sqlQuery(db, paste("
      SELECT runtime, wheel, target.tp_date_pressed, sample_name, target.rec_num, target.tp_num, gf_co2_qty, 
          norm_ratio, int_err, ext_err, blk_corr_method, fm_corr, sig_fm_corr, ss
        FROM snics_results, target, graphite
        WHERE target.tp_num = snics_results.tp_num
        AND target.osg_num = graphite.osg_num
        AND sample_name LIKE 'Acet%'
        AND tp_date_pressed > '2014-09-01'
        AND graphite.gf_co2_qty > 40
        AND graphite_lab = 1
      "))


#Close DB
odbcClose(db)

c1$type <- "C1"
acet$type <- "Acet"
blanks.n <- rbind(c1, acet)
blanks.n$type <- ordered(blanks.n$type, levels = c("Acet", "C1"))
blanks.n <- mutate(blanks.n, system = ifelse(grepl("CFAMS", wheel), "CFAMS", "USAMS"))


#combine data

blanks.as <- select(as.data.frame(blanks.a), -tp_date_pressed, -system, -type)
blanks <- left_join(blanks.n, blanks.as, by="tp_num")

blanks <- blanks %>% 
  filter(norm_ratio < .05, norm_ratio > -99, fm_corr <.05) %>% 
  mutate(merr = pmax(int_err, ext_err))


server <- function(input, output) {
  
  blankdata <- reactive({
    
    # Apply filters
    blanks <- blanks %>%
      filter(
        tp_date_pressed >= input$date[1],
        tp_date_pressed <= input$date[2],
        gf_co2_qty >= input$size[1],
        gf_co2_qty <= input$size[2],
        norm_ratio > -99)
    if (input$filtqc) {
      blanks %>% filter(norm_ratio < input$nfm)
    } else {
      blanks
    }
  })
  
  output$blankTable <- renderTable({       
    blanksum <- blanks %>%
      group_by(system, type) %>%
      summarize(
        Raw1412 = mean(c1412x, na.rm = TRUE),
        Raw1412.sd = sd(c1412x, na.rm = TRUE),
        normFm = mean(norm_ratio),
        normFm.sd = sd(norm_ratio),
        normFm.err = mean(merr),
        BlkCorrFm = mean(fm_corr),
        #fm_corr.sd = sd(fm_corr, na.rm = TRUE),
        N = n())
    kable(blanksum, digits = c(0,0,2,3,4,5,5,6,0))
    
  })
  
  
  
  output$blankPlot <- renderPlot({
    ggplot(blanks.a, aes(x = system, y = c1412x)) + 
      geom_boxplot() + facet_grid(. ~ type) + 
      xlab("System") + ylab("Average Raw 14/12 ratio") +
      ylab(expression(paste("Raw 14/12 ratio ( X", 10^{-16},")"))) +
      theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  })
  
  output$blanktimePlot <- renderPlot({
    qplot(tp_date_pressed, c1412x, color = system, data = blanks.a) +
      scale_y_log10() + geom_smooth() + facet_grid(type ~ ., scale = "free") +  theme_bw() + 
      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  })
  
}


ui <- fluidPage(
  # Application title
  titlePanel("NOSAMS Blanks"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('date',
                     label = 'Date Range',
                     start = (Sys.Date() - 90),
                     end = Sys.Date(),
                     max = Sys.Date()),
      sliderInput("size", "Graphite Size (umol)",
                  1, 500, value = c(40,300)),
      checkboxInput("filtqc", "Filter by Fm?"),
      sliderInput("nfm", "Max Fm",
                  0, 0.1, value = 0.02)
    ),
    mainPanel(plotOutput("blankPlot"),
              plotOutput("blanktimePlot"),
              tableOutput("blankTable")
    )
    
  )
)