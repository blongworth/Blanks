### shiny app to get blanks from db, summarize and plot

### load libraries
library(blanks)
library(dplyr)
library(ggplot2)
library(shiny)

# TODO: sort by size0

### Get blank data from DB
from <- as.Date('2014-09-01')
blanks <- getBlankData(from)

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
        facet_grid(process ~ ., scale = "free") +  theme_bw() + 
        ggtitle("Blanks over time") +
        ylab("Normalized Fm") 
    } else {
      ggplot(blankdata(), aes(tp_date_pressed, age, color = system)) + 
        geom_point() +
        facet_grid(process ~ ., scale = "free") +  theme_bw() + 
        ggtitle("Blanks over time") +
        ylab("Radiocarbon age") 
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

