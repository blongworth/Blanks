### shiny app to get blanks from db, summarize and plot

### load libraries
library(amstools)
library(RODBC)
library(dplyr)
library(ggplot2)
library(shiny)

# TODO: sort by size0

### Get blank data from DB
from <- '2014-09-01'

#Open DB connection
db <- conNOSAMS()

#Get raw blank data
raw =  sqlQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, target.rec_num, 
          sample_name, target.tp_num, gf_co2_qty, 
          he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
        FROM snics_raw, target, graphite
      	WHERE target.tp_num = snics_raw.tp_num
        AND target.osg_num = graphite.osg_num
        AND tp_date_pressed > '", from, "'
        AND target.rec_num IN (83028, 53804, 2138, 140548, 36168)
        "))

jmer =  sqlQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, target.rec_num, 
          sample_name, target.tp_num, he12c, he13c, d13c, he14_12, 
          he13_12, wheel, ok_calc
        FROM snics_raw, target
      	WHERE target.tp_num = snics_raw.tp_num
        AND tp_date_pressed > '", from, "'
        AND target.rec_num = 32491
        "))

#add type columns, combine data frames
jmer$gf_co2_qty <- NA
blanks.r <- rbind(raw, jmer)

#average by target and filter
blanks.a <- blanks.r %>%
  group_by(tp_num) %>%
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

blanks.n =  sqlQuery(db, paste("
      SELECT runtime, wheel, target.tp_date_pressed, sample_name,
          target.rec_num, target.tp_num, gf_co2_qty, 
          norm_ratio, int_err, ext_err, 
          blk_corr_method, fm_corr, sig_fm_corr, ss
        FROM snics_results, target, graphite
        WHERE target.tp_num = snics_results.tp_num
        AND target.osg_num = graphite.osg_num
        AND tp_date_pressed > '", from, "'
        AND target.rec_num IN (83028, 53804, 2138, 140548, 36168)
        "))


jme =  sqlQuery(db, paste("
       SELECT runtime, wheel, target.tp_date_pressed, sample_name, 
           target.rec_num, target.tp_num,
           norm_ratio, int_err, ext_err,
           blk_corr_method, fm_corr, sig_fm_corr, ss
         FROM snics_results, target
         WHERE target.tp_num = snics_results.tp_num
         AND target.rec_num = 32491
        AND tp_date_pressed > '", from, "'
         "))

#Close DB
odbcClose(db)

jme$gf_co2_qty <- NA
blanks.n <- rbind(blanks.n, jme)


#combine data
blanks <- left_join(blanks.a, blanks.n, by="tp_num") %>%
  mutate(tp_date_pressed = as.Date(tp_date_pressed),
         type = ordered(recode(as.character(rec_num), 
                               "83028" = "C1", 
                               "53804" = "C1", 
                               "2138" = "TIRI-F", 
                               "36168" = "Acet", 
                               "140548" = "Acet",
                               "32491" = "JME"),
                        levels = c("Acet", "C1", "TIRI-F", "JME")),
         merr = pmax(int_err, ext_err),
         system = substring(wheel, 1, 5),
         age = rcage(norm_ratio))


## Define UI for application 
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
      checkboxInput("raw", "Plot raw 14/12"),
      sliderInput("nfm", "Max Fm",
                  0, 0.1, value = 0.02)
    ),
    mainPanel(plotOutput("blankPlot"),
              plotOutput("blanktimePlot"),
              tableOutput("blankTable")
    )
    
  )
)

# define server logic
server <- function(input, output) {
  
  blankdata <- reactive({
    
    # Apply filters
    blanks <- blanks %>%
      filter(
        tp_date_pressed >= input$date[1],
        tp_date_pressed <= input$date[2],
        (is.na(gf_co2_qty) |
        gf_co2_qty >= input$size[1] &
        gf_co2_qty <= input$size[2]),
        norm_ratio > -99)
    if (input$filtqc) {
      blanks %>% filter(norm_ratio < input$nfm)
    } else {
      blanks
    }
  })
  
  output$blankTable <- renderTable({       
    blankdata() %>%
      group_by(system, type) %>%
      summarize(
        Raw1412 = mean(c1412x, na.rm = TRUE),
        Raw1412.sd = sd(c1412x, na.rm = TRUE),
        RCAge  = mean(age),
        RCAge.sd = sd(age),
        N = n())
  }, digits = 0)
  
  output$blankPlot <- renderPlot({
    if (input$raw == TRUE) {
    ggplot(blankdata(), aes(x = type, y = c1412x)) + 
      geom_boxplot() + facet_grid(. ~ system) + 
      xlab("Blank type") + ylab("Average Raw 14/12 ratio") +
      ylab(expression(paste("Raw 14/12 ratio ( X", 10^{-16},")"))) +
      ggtitle("Blanks by system") + theme_bw() + 
      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
    } else {
    ggplot(blankdata(), aes(x = type, y = age)) + 
      geom_boxplot() + facet_grid(. ~ system) + 
      xlab("Blank type") +
      ylab("Radiocarbon age") +
      ggtitle("Blanks by system") + theme_bw() + 
      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  }
  })
  
  output$blanktimePlot <- renderPlot({
    if (input$raw == TRUE) {
      ggplot(blankdata(), aes(tp_date_pressed, c1412x, color = system)) + 
        geom_point() +
        facet_grid(type ~ ., scale = "free") +  theme_bw() + 
        ggtitle("Blanks over time") +
        ylab(expression(paste("Raw 14/12 ratio ( X", 10^{-16},")"))) 
    } else {
      ggplot(blankdata(), aes(tp_date_pressed, age, color = system)) + 
        geom_point() +
        facet_grid(type ~ ., scale = "free") +  theme_bw() + 
        ggtitle("Blanks over time") +
        ylab("Radiocarbon age") 
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

