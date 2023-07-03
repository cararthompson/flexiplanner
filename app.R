
library(tidyverse)
library(plyr)

# Set up fonts
dir.create('~/.fonts')
file.copy("www/Sigmar-Regular.ttf", "~/.fonts")
file.copy("www/noah-regular.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Define UI
ui <- fluidPage(
  
  tags$head(
    # Define UI css
    tags$style(HTML("
body {
  background-color: #fafafa;
  color: #2C3D4F;
}
     
h2 {
  font-family: 'Sigmar', sans-serif;
}

h3 {
  font-family: 'Sigmar', sans-serif;
  font-size: 30pt;
  margin-top: 24pt;
  margin-bottom: 6pt;
}
    
h4 {
  font-family: 'Noah', sans-serif;
  margin-top: 6pt;
  margin-bottom: 12pt;
}

.shiny-input-container {
 font-family: 'Noah';
 background-color: #fafafa;
 color: #808A95;
}
      
.irs {
  font-family: 'Noah';
  color: #2C3D4F;
}

.well {
  background-color: #fafafa;
  border: 1px solid #808A95;
  border-radius: 5px;
}

.irs--shiny .irs-bar {
  top: 25px;
  height: 8px;
  border-top: 1px solid #28A569;
  border-bottom: 1px solid #28A569;
  background: #28A569;
}

.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background-color: #28A569;
  border-radius: 5px;
  font-size: 12px;
}

.irs--shiny .irs-min, .irs--shiny .irs-max {
  background-color: #fafafa;
  color:  #2C3D4F;
}

.form-control {
  border-color: #808A95;
}

.form-control:focus {
  border-color: #808A95;
  color: 2C3D4F;
  -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075),0 0 8px #808A95;
  box-shadow: inset 0 1px 1px rgba(0,0,0,.075),0 0 8px #808A95;
}

.irs--shiny .irs-line {
  top: 25px;
  height: 8px;
  background: linear-gradient(to bottom, #808A95 -50%, #fff 150%);
    background-color: rgba(0, 0, 0, 0);
  background-color: #808A95;
  border: 1px solid #cccccc;
  border-radius: 8px;
}

.fa-classic, .fa-regular, .fa-solid, .far, .fas {
  color: #28A569;
}

.irs--shiny .irs-handle {
  top: 23px;
  width: 12px;
  height: 12px;
  border: 1px solid #2C3D4F;
  background-color: #28A569;
  box-shadow: 1px 1px 3px #808A95;
  border-radius: 5px;
}

.btn-default {
    color: #2C3D4F;
    font-family: 'Noah';
    background-color: #fff;
    border-color: #808A95;
}
"))
  ),

# Application title
titlePanel(windowTitle = "FlexiPlanner", 
           title = div(h3('FlexiPlanner'), 
                       h4('Planning for the predictably unpreditable'))),

# Sidebar with a slider input for number of days 
sidebarLayout(
  sidebarPanel(
    textInput("title",
              "Month / Title"),
    textInput("subtitle",
              "Theme"),
    sliderInput("days",
                "How many days / time chunks?",
                min = 1, ticks = FALSE,
                max = 100,
                value = 28),
    sliderInput("rows",
                "How many columns?",
                value = 7, ticks = FALSE,
                min = 3, max = 10),
    downloadButton("save", "Save image")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("planner")
  )
)
)

# Define server logic
server <- function(input, output) {
  
  make_waffle <- function(input) {
    ggplot() +
    waffle::geom_waffle(aes(fill = "empty", values = input$days),
                        colour = "#2C3D4F",
                        n_rows = input$rows,
                        flip = TRUE,
                        height = 0.8,
                        width = 0.8,
                        size = 0.8,
                        radius = unit(5, "pt"),
                        show.legend = FALSE) +
    scale_fill_manual(values = c("empty" = "#FFFFFF")) +
    labs(title = input$title,
         subtitle = input$subtitle,
         caption = "www.cararthompson.com\nBuilding Stories with Data") +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(family = "Sigmar", size = rel(2.4),
                                    hjust = 0.5, 
                                    colour = "#2C3D4F",
                                    margin = margin(t = 12, b = 12)),
          plot.subtitle = element_text(family = "Noah", 
                                       colour = "#1A242F",
                                       size = rel(1.8), hjust = 0.5,
                                       margin = margin(t = 0, b = 12)),
          plot.caption = element_text(margin = margin(t = 12), 
                                      family = "Noah", colour = "#808A95"),
          plot.margin = margin(t = 12, r = 12, b = 12, l = 12))
  }
  
  output$planner <- renderPlot({
    make_waffle(input)
  })
  
  output$save <- downloadHandler(
      filename = function(){
        paste("flexiPlanner_", janitor::make_clean_names(input$title), ".png", sep = "")
        },
      content = function(file){
        ggsave(file, make_waffle(input), dpi = 300, units = "in",
               bg = "#FFFFFF")
      }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)

