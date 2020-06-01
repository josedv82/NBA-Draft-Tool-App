
#DRAFTTOOL
#Jose FErnandez
#2020

###################################################

#loading required libraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(formattable)
library(dashboardthemes)

#####################################################

#load 2017 draft data using Pelton's model. This data is directly extracted from the website.

draft <- read.csv("2017Draft.csv")

###################################################

#User Interface

##################################################


  
  ui <- dashboardPagePlus(
    
    header = dashboardHeaderPlus(
      
      enable_rightsidebar = FALSE,
      title = "DraftTool"),

#the following code crates funtionality on the left side menu, providing filtering options for users.    
        
    sidebar = dashboardSidebar(
      
        uiOutput("trade_input"),
       
        tags$hr(),
        
        uiOutput("trade_input2")
      
    ),
  
#the following code provides functionality for the main body of the dashboard
    body = dashboardBody(
      
    shinyDashboardThemes(theme = "purple_gradient"), #adds purple layout to the app
    
    column(width = 7,
 
           
#the tabs populating the tables showing picks and value selected by the user              
    tabBox(
      
    title = "",
    id =  "tab1",
    height = '500px',
    
#tab for picks to give away
    tabPanel("Picks Given Away", DT::dataTableOutput("trade_away"),
             tags$hr(),
             tags$br(),
             tags$br(),
             tags$h1("Total Given Value", style = "color:white"),
             tags$br(),
             h2(span(textOutput("value_out"), style = "color:lightblue")),
             h4(span(textOutput("value_out2"), style = "color:springgreen"))),
    
#tab for picks to receive
    tabPanel("Picks Received", DT::dataTableOutput("trade_away2"),
             tags$hr(),
             tags$br(),
             tags$br(),
             tags$h1("Total Received Value", style = "color:white"),
             tags$br(),
             h2(span(textOutput("value_in"), style = "color:lightblue")),
             h4(span(textOutput("value_in2"), style = "color:springgreen")))
             
    )   
       ),
  
    
  
#this part add the advice functionality and whether to go for the trade or not
    column(width = 5,
           
           h1("Overall Trade Value:"),
           h2(span(uiOutput("overall"))),
           tags$br(),
           tags$hr(),
           h3("Overall Normalized Value:"),
           h2(span(uiOutput("overall.2"))),
           tags$hr()
           )
    
   
  ),

#this codes add a footer to the dashboard
footer = dashboardFooter(
  left_text = p("Based on Kevin Pelton's 2017 Draft Model"),
  right_text = ""
)

)
  

###################################################
  
#Server Logic. Adds functionality to the user experience elements
  
##################################################
  
  
server <- function(input, output, session) {
    
# Code to create the filter for users to choose picks to give away
    
output$trade_input <- renderUI({
    
    pickerInput(
      inputId = "trade.input",
      label = "Select Picks to Trade:", 
      choices = unique(draft$Pick),
      multiple = TRUE)
      
    })


#creates a reactive object filtered by what the users selects on the first filter

draft.1 <- reactive(
  
  draft %>%
    filter(Pick %in% input$trade.input)
  
)

#Code to render the table of trades to give away

output$trade_away <- DT::renderDataTable({
  
  draft.1()  %>%
    formattable() %>%
    as.datatable(rownames = FALSE, options = list(dom = 't')) %>%
    formatStyle('Pick', color = 'white', fontWeight = 'bold') %>%
    formatStyle('Value', color = 'white') %>%
    formatStyle('Normalized', color = 'white')
  
})


#Reactive object showing the selections of the filter for picks to receive

draft.2 <- reactive(
  
  draft %>%
    mutate(ops = ifelse(Pick %in% input$trade.input, 1, 0)) %>%
    filter(ops != 1) %>%
    select(-ops)
  
)


#Code to create the second filter for picks to receive

output$trade_input2 <- renderUI({
  
  pickerInput(
    inputId = "trade.input2",
    label = "Select Picks to Receive:", 
    choices = draft.2()$Pick %>% unique(),
    multiple = TRUE)
  
})


#This code renders the table with picks to receive as selected by the user.

output$trade_away2 <- DT::renderDataTable({
  
  draft.2() %>%
    filter(Pick %in% input$trade.input2) %>%
    formattable() %>%
    as.datatable(rownames = FALSE, options = list(dom = 't')) %>%
    formatStyle('Pick', color = 'white', fontWeight = 'bold') %>%
    formatStyle('Value', color = 'white') %>%
    formatStyle('Normalized', color = 'white')
  
})



#Code to summarise the value given away account for all picks to give selected.

value.out <- reactive(

draft.1() %>% 
  select(-Pick) %>%
  summarise_all(list(sum)) %>%
  as.tibble()

)

output$value_out <- renderText({ 
  paste("", value.out()$Value)
})

output$value_out2 <- renderText({ 
  paste("Normalized:", value.out()$Normalized)
})


#Code to summarise the value for picks to receive. it accounts for all picks to give selected.

value.in <- reactive(

  draft.2() %>%
  filter(Pick %in% input$trade.input2) %>%
  select(-Pick) %>%
  summarise_all(list(sum)) %>%
  as.tibble()
  
)


output$value_in <- renderText({ 
  paste("", value.in()$Value)
})

output$value_in2 <- renderText({ 
  paste("Normalized:", value.in()$Normalized)
})

#This code calculates the difference between the total value out vs total value in. I created a quick ...if this then that... logic 
#to play with colors and messages based on the outcome of the pick

overall.1 <- reactive({
  
  a <- c(value.out()$Value, value.in()$Value)
  
diff(a)

  
})

output$overall <- renderUI({ 
  
  if(overall.1() == 0){
    
    a <- paste("<span style=color:gray>", "Use the filters on the left to start trading picks.", "</span>")
  
  
  }else if(overall.1() > 0){
  
    a <- paste("<span style=color:springgreen>", "+" ,overall.1(), "<br/>","Go for it!", icon = icon("thumbs-up"), "</span>")
  
  }else{
    
    a <- paste("<span style=color:pink>", overall.1(), "<br/>", "Don't do it!", icon = icon("thumbs-down"), "</span>")
    
  }
  
  HTML(a)
  
})

#same as the above if else logic but for normalized value

overall.2 <- reactive({
  
  a <- c(value.out()$Normalized, value.in()$Normalized)
  
  diff(a)
  
  
})


output$overall.2 <- renderUI({ 
  
  if(overall.1() == 0){
    
    a <- paste("<span style=color:gray>", "", "</span>")
    
    
  }else if(overall.2() > 0){
    
    a <- paste("<span style=color:springgreen>", "+" ,overall.2(), "</span>")
    
  }else{
    
    a <- paste("<span style=color:pink>", overall.2(), "</span>")
    
  }
  
  HTML(a)
  
})

    
  }
  
  
    
##################################################
    
#line to create the app
shinyApp(ui, server)


###################################################

