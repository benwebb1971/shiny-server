library(shiny)
library(shinythemes)

# responses directory
responsesDir <- file.path("responses")

saveData <- function(data) {
  
  # Create a unique file name
  fileName <-paste(data[1,1], data[1,2], sep = "-")
  fileName <-paste(fileName, Sys.Date(), sep = "-")
  fileName <-paste(fileName, "csv", sep=".")
  
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(responsesDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}


# Mandatory fields for form
fieldsMandatory <- c("name", "student_id")  # Mandatory fields 

# Fileds we want to save to file
fields <- c("name", "student_id", "course", "module", "supervisors")

# Star for Mandatory fields 
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Red star and error message
appCSS <-
  ".mandatory_star { color: red; }
#error { color: red; }"


shinyApp(
  ui <- fluidPage(
    
    #use shiny js to disable the ID field and CSS for red star
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!")
      )),
    
    # Shiny theme
    theme = shinytheme("united"),
    div(id="form",
        
        # Title of ouput
        titlePanel("Third Year Project Supervisor Preferences"),
        
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(textInput("name", labelMandatory("Name")),
                       textInput("student_id",  labelMandatory("Student_ID")),
                       radioButtons("course", label = h3("Course:"),
                                    choices = list("C800" = "C800", 
                                                   "C850" = "C850")),
                       radioButtons("module", label = h3("Module:"),
                                    choices = list("Full Project (C83MPR)" = "C83MPR", 
                                                   "Mini-Project (C83MUA)" = "C83MUA", 
                                                   "Mini-Project (C83MUB)" = "C83MUB"))),
          
          
          
          
          
          mainPanel(DT::dataTableOutput("table"),
                    uiOutput("supervisors"),
                    actionButton("select", "Select Supervisor"),
                    actionButton("delete", "Delete Supervisor"),
                    actionButton("submit", "Submit Choices")
                    
          )
        ))),
  
  # Define server logic 
  server <- function(input, output, session) {
    
    # # Load supervisor names and collate in vector
    sname = 'supervisors.csv' # supervisor names
    sups = read.csv(sname,header=FALSE)
    names = c(sups)
    supervisors <-names[[1]]
    
    output$supervisors <- renderUI({
      selectInput("supervisors", "Choose your 10 preferred supervisors",
                  supervisors, selected = "",
                  selectize = TRUE, multiple = FALSE)})
    
    
    # v$choices will represent the supervisors that have been chosen
    v <- reactiveValues(choices = c())
    
    # When the select button is clicked save the form data, add the chosen supervisor to v$choices
    # and update the selector
    observeEvent(input$select, {
      v$choices <- append(v$choices, input$supervisors)
      updateSelectInput(session, "supervisors",
                        choices = supervisors[!(supervisors %in% v$choices)])
    })
    
    
    tablevalues <- reactiveValues()
    tablevalues$df <- data.frame(Name = numeric(), 
                                 StudentID = numeric(), 
                                 Course = character(),
                                 Module = character(),
                                 Supervisors = character(),
                                 stringsAsFactors = FALSE)
    newEntry <- observe({
      if(input$select > 0) {
        isolate(tablevalues$df[nrow(tablevalues$df)+1,] <- c(input$name, 
                                                             input$student_id, 
                                                             input$course, 
                                                             input$module, 
                                                             input$supervisors))
        
      }
    })
    
    deleteEntry <- observe({
      if(input$delete > 0) {
        isolate(tablevalues$df <- tablevalues$df[-nrow(tablevalues$df),])
      }
    })
    
    
    output$table <- DT::renderDataTable({tablevalues$df}, options = list(paging = FALSE, 
                                                                         searching = FALSE))
    
    
    # Ensure mandatory fields have input before enabling the submit button
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    
    observeEvent(input$submit, {
      saveData(tablevalues$df)
      
      
      # reset and hide shiny form 
      shinyjs::reset("form")
      shinyjs::hide("form")
      
      # show thank you message
      shinyjs::show("thankyou_msg")
      
      
      
      
    })
    
  }
)

