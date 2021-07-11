#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("To Be or Not to Be: The choose your own adventure shiny app"),

    # Sidebar with a slider input for number of bins 
        # Show a plot of the generated distribution
        mainPanel(width = 16,
           plotOutput("distPlot")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    max_pages <- 368
    
    df <- data.frame(date = "07/11/21",
                     person = "skg",
                     path_id = "1",
                     nar = c("Beginning",
                            "Hamlet",
                            "Hamlet",
                            "Hamlet",
                            "Hamlet",
                            "Hamlet",
                            "Ophelia",
                            "Ophelia",
                            "Ophelia",
                            "Ophelia",
                            "Ophelia",
                            "Ophelia",
                            "End"
                            ),
                     page = c(9,
                              17,
                              360,
                              31,
                              54,
                              114,
                              29,
                              245,
                              221,
                              248,
                              259,
                              266,
                              max_pages),
                     percent = c(1,
                                 4,
                                 96,
                                 8,
                                 14,
                                 30,
                                 7,
                                 64,
                                 58,
                                 65,
                                 68,
                                 70,
                                 100)
                  )
    
  
    
    df <- df %>%
        mutate(x = page / max_pages,
               y = factor(nar, levels = c("Hamlet",
                                         "Ophelia",
                                         "Hamlet Sr.",
                                         "Beginning",
                                         "End")),
                xto =  lead(page) / max_pages,
               yto = lead(y),
               id = paste(date, sep = "-", 
                          person, path_id),
               ) 
    
    g <- ggplot(data = df,
                group = id) +
        geom_curve(aes(x = x,
                      y = as.numeric(y),
                      xend = xto,
                      yend = as.numeric(yto)),
                   arrow = arrow(length = unit(0.05, "npc")),
                    curvature = .2)+
        geom_point(aes(x = x,
                       y = as.numeric(y),
                       fill = y),
                   shape= 21,
                   size = 7) +
        theme_bw(base_size = 20) +
        labs(x = "Percent of book",
              y = "") +
        scale_fill_brewer(palette = "Set1",
                          name = "Narrator") + 
        theme(legend.position = "bottom",
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        ylim(-1, 5)

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        print(g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
