### shiny app to get blanks from db, summarize and plot

### load libraries
library(amstools)
library(odbc)
library(dplyr)
library(ggplot2)
library(shiny)

# TODO: sort by size0

### Get blank data from DB
from <- '2014-09-01'

#Open DB connection
db <- conNOSAMS()

#Get raw blank data
raw =  dbGetQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, target.rec_num, 
          sample_name, target.tp_num, gf_co2_qty, 
          he12c, he13c, d13c, he14_12, he13_12, wheel, ok_calc
        FROM snics_raw, target, graphite
      	WHERE target.tp_num = snics_raw.tp_num
        AND target.osg_num = graphite.osg_num
        AND tp_date_pressed > '", from, "'
        AND target.rec_num IN (83028, 53804, 2138, 140548, 36168, 55101, 1081, 39246)
        "))

jmer =  dbGetQuery(db, paste("
      SELECT runtime, target.tp_date_pressed, target.rec_num, 
          sample_name, target.tp_num, he12c, he13c, d13c, he14_12, 
          he13_12, wheel, ok_calc
        FROM snics_raw, target
      	WHERE target.tp_num = snics_raw.tp_num
        AND tp_date_pressed > '", from, "'
        AND target.rec_num IN (32490, 32491, 32492, 36947, 148820)
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
  mutate(c1412x = he1412 * 1e16) 

###
#Get normalized blank data
###

blanks.n =  dbGetQuery(db, paste("
      SELECT runtime, wheel, target.tp_date_pressed, sample_name,
          target.rec_num, target.tp_num, gf_co2_qty, 
          norm_ratio, int_err, ext_err, 
          blk_corr_method, fm_corr, sig_fm_corr, ss
        FROM snics_results, target, graphite
        WHERE target.tp_num = snics_results.tp_num
        AND target.osg_num = graphite.osg_num
        AND tp_date_pressed > '", from, "'
        AND target.rec_num IN (83028, 53804, 2138, 140548, 36168, 55101, 1081, 39246)
        "))


jme =  dbGetQuery(db, paste("
       SELECT runtime, wheel, target.tp_date_pressed, sample_name, 
           target.rec_num, target.tp_num,
           norm_ratio, int_err, ext_err,
           blk_corr_method, fm_corr, sig_fm_corr, ss
         FROM snics_results, target
         WHERE target.tp_num = snics_results.tp_num
         AND target.rec_num IN (32490, 32491, 32492, 36947, 148820)
         AND tp_date_pressed > '", from, "'
         "))

jme$gf_co2_qty <- NA
blanks.n <- rbind(blanks.n, jme)


#combine data
blanks <- left_join(blanks.a, blanks.n, by="tp_num") %>%
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
                        levels = c("Acet", "C1", "TIRI-F", "JME", "Old Ceylon", "Ceylon")),
         merr = pmax(int_err, ext_err),
         system = toupper(substring(wheel, 1, 5)),
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
      checkboxInput("raw", "Plot raw 14/12"),
      checkboxInput("filtqc", "Filter by Age?"),
      sliderInput("age", "Min age",
                  0, 60000, value = 40000)
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
        gf_co2_qty <= input$size[2]))
    if (input$filtqc) {
      blanks %>% filter(age > input$age)
    } else {
      blanks
    }
  })
  
  output$blankTable <- renderTable({       
    blankdata() %>%
      group_by(system, type) %>%
      summarize(
        #Raw1412 = as.integer(mean(c1412x, na.rm = TRUE)),
        #Raw1412.sd = as.integer(sd(c1412x, na.rm = TRUE)),
        Norm1412 = mean(norm_ratio, na.rm = TRUE),
        Norm1412.sd = sd(norm_ratio, na.rm = TRUE),
        RCAge  = as.integer(round(mean(age), -3)),
        RCAge.sd = as.integer(round(sd(age), -2)),
        N = n())
  }, digits = 4)
  
  output$blankPlot <- renderPlot({
    if (input$raw == TRUE) {
    ggplot(blankdata(), aes(x = type, y = norm_ratio)) + 
      geom_boxplot() + facet_grid(. ~ system) + 
      xlab("Blank type") +
      ylab("Normalized Fm") +
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
      ggplot(blankdata(), aes(tp_date_pressed, norm_ratio, color = system)) + 
        geom_point() +
        facet_grid(type ~ ., scale = "free") +  theme_bw() + 
        ggtitle("Blanks over time") +
        ylab("Normalized Fm") 
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

