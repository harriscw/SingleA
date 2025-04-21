
library(shiny)
library(DT)
library(dplyr)

password <- readLines("password.txt")


# Define UI for application 
ui <- fluidPage(
  
  uiOutput("main_ui")

)

# Define server logic 
server <- function(input, output) {
  
  access_granted <- reactiveVal(FALSE)

  # read data
  df = readxl::read_excel("data/A Ball Scouting Reports.xlsx")
  
  # set filter appropriately
  thenum=reactive({
    if(input$radio=="all"){
      0
    }else if(input$radio=="second half"){
      8
    }else if(input$radio=="last 3"){
      max(df$Game)-3
    }else if(input$radio=="last 6"){
      max(df$Game)-6
    }
    
  })
  
  # display for aggregate data
  output$agg = renderDT({
    
    df_agg=df %>% 
      filter(Game>thenum()) %>% 
      group_by(Name) %>% 
      summarize(games=n(),order=round(mean(Order),2),Hits=sum(Hits),AB=sum(`At Bats`),XBH=sum(XBH),SO=sum(SO)) %>% 
      ungroup() %>% 
      mutate(AVG=round(Hits/AB,3)) %>% 
      arrange(desc(AVG))
    
  }, options = list(lengthChange = FALSE,paging = FALSE)
  )
  
  #display for raw data
  output$raw = renderDT(
    df, options = list(lengthChange = FALSE)
  )
  
  
  ###
  # Render UI
  ###
  
  
  output$main_ui <- renderUI({
    if (!access_granted()) {
      tagList(
        passwordInput("pwd", "Enter Password"),
        actionButton("go", "Submit"),
        textOutput("wrong_pwd")
      )
    } else {
      tabsetPanel(
        tabPanel("Aggregate",
                 
                 radioButtons("radio", label = "Filter",
                              choices = list("All Games" = "all", 
                                             "Second Half" = "second half", 
                                             "Last 3" = "last 3",
                                             "Last 6" = "last 6"
                              ), 
                              selected = "all"),
                 DTOutput('agg')
                 
        ),
        tabPanel("Raw",DTOutput('raw'))
      )
    }
  })
  
  observeEvent(input$go, {
    if (input$pwd == password) {
      access_granted(TRUE)
    } else {
      output$wrong_pwd <- renderText("❌ Incorrect password.")
    }
  })
}
  

  


# Run the application 
shinyApp(ui = ui, server = server)
