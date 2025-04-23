
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
  df_=readxl::read_excel("data/A Ball Scouting Reports.xlsx")
  

  df = reactive({

    # reactively filter to team
    if(input$team != "All"){
      df_=df_ %>% filter(Team==input$team)
    }
    
    # reactively filter games
    if(input$radio=="second half"){
      df_ = df_ %>% filter(Game>8)
    }else if(input$radio=="last 3"){
      df_ = df_ %>% 
        group_by(Name) %>% 
        dplyr::slice(tail(row_number(),3)) %>%
        ungroup()
    }else if(input$radio=="last 6"){
      df_ = df_ %>% 
        group_by(Name) %>% 
        dplyr::slice(tail(row_number(),6)) %>% 
        ungroup()
    }
    
    df_
    
    })
  

  
  # display for aggregate data
  output$agg = renderDT({
    
    # df=df_ %>% filter(Team=="Lugnuts")
    
    df() %>% 
      group_by(Team,Name) %>% 
      summarize(games=n(),
                order=round(mean(Order),2),
                Hits=sum(Hits),
                AB=sum(`At Bats`),
                XBH=sum(XBH),
                SO=sum(SO)
                ) %>% 
      ungroup() %>% 
      mutate(AVG=round(Hits/AB,3),
             `SO/G`=round(SO/games,2)
             ) %>% 
      arrange(desc(AVG)) %>% 
      mutate(`Percentile (AVG)`=round(100*(nrow(.)-row_number())/nrow(.),2))
    
  }, options = list(lengthChange = FALSE,paging = FALSE)
  )
  
  #display for raw data
  output$raw = renderDT(
    {
      
      df()

      }, options = list(lengthChange = FALSE)
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
      
      sidebarLayout(
        sidebarPanel(
          
          selectInput("team", 
                      label = "Team", 
                      choices = c("All",unique(df_$Team)), 
                      selected = "All"),
          radioButtons("radio", label = "Filter",
                       choices = list("All Games" = "all", 
                                      "Second Half" = "second half", 
                                      "Last 3" = "last 3",
                                      "Last 6" = "last 6"
                       ), 
                       selected = "all")
          
        ),
        mainPanel(
        tabsetPanel(
          tabPanel("Aggregate",
                   DTOutput('agg')
                   
          ),
          tabPanel("Raw",DTOutput('raw'))
        )#end tabsetpanel
        )#end mainpanel
      )#end sidebarlayout
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
