library(shiny)
library(shinythemes)

# which fields get saved 
# define the fields we want to save from the form
fieldsAll <- c("name", "studentid", "course", "module", "supervisors")


saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

loadSupervisors <- function() {
  if (exists("supervisors")) {
    supervisors
  }
}
  
# directory where responses get stored
responsesDir <- file.path("responses")

# directory where responses get stored
supervisorsDir <- file.path("supervisors")

# # Load supervisor names and collate in vector
# setwd("shiny-server/apps/project-pref/supervisors")
# sname = 'supervisors.csv' # supervisor names
# supervisors = read.csv(sname,header=FALSE)
# names = supervisors
# names <-names[[1]]
# 



shinyApp(
  ui = fluidPage(
    
    # Shiny theme
    theme = shinytheme("united"),
    
    # Title of ouput
    titlePanel("Third Year Project Supervisor Preferences"),
    
    # Creates a layout with a side bar and main area
    sidebarLayout(
      
      # Input into side bar on the left (in this  case)
      sidebarPanel(width = 4,
                   
                   textInput("name", "Name", "Nobody"),
                   tags$head(tags$style("#student{color: black;
                                        font-size: 16px;
                                        font-name: Arial;
                                        }")),
               textInput("studentid", "Student ID", "12345"), # Mandatory fields 
               tags$head(tags$style("#studentid{color: black;
                                    font-size: 16px;
                                    font-name: Arial;
                                    }")),
               
               radioButtons("course", label = h3("Course:"),
                            choices = list("C800" = "C800", 
                                           "C850" = "C850")),
               tags$head(tags$style("#course{color: black;
                                    font-size: 16px;
                                    font-name: Arial;
                                    }")),               
               
               radioButtons("module", label = h3("Module:"),
                            choices = list("Full Project (C83MPR)" = "C83MPR", 
                                           "Mini-Project (C83MUA)" = "C83MUA", 
                                           "Mini-Project (C83MUB)" = "C83MUB")),
               tags$head(tags$style("#module{color: black;
                                    font-size: 16px;
                                    font-name: Arial;
                                    }")),                   
               selectInput("supervisors", "Choose your 10 preferred supervisors (Delete and move cursor to reorder)",
                           c("Harriet Allen", "Ben Webb", "Denis Schluppeck"), selected = "Harriet Allen",
                           selectize = TRUE, multiple = FALSE)),
      
      mainPanel(
        DT::dataTableOutput("responses", width = 300), tags$hr(),
        actionButton("submit", "Submit", class = "btn-primary"))
      
               )
    ),
  
  
  server = function(input, output, session) {
    
    
    
    # Whenever a field is filled, x§x§x
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data
    })

    # When a supervisor is chosen, update the form
    observeEvent(input$supervisors, {
      saveData(formData())
    })

    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$supervisors
      loadData()
    })
  }
  )