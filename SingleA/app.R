
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

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
    if(input$radio=="first half"){
      df_ = df_ %>% filter(Game<=8 & Game<=18)
    }else if(input$radio=="second half"){
      df_ = df_ %>% filter(Game>8 & Game<=18)
    }else if(input$radio=="second plus"){
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
    }else if(input$radio=="regular season"){
      df_ = df_ %>% filter(Game<=18)
    }else if(input$radio=="playoffs"){
      df_ = df_ %>% filter(Game>18)
    }
    
    df_
    
    })
  
  
  ######
  # display for team data
  ######

  
  output$team = renderDT({
    
    # df=df_ %>% filter(Team=="Lugnuts")
    
    df=df() %>% 
      group_by(Team) %>% 
      summarize(
        games=case_when(
          input$radio=="last 3"~3,
          input$radio=="last 6"~6,
          TRUE~length(unique(Game))
      ),
                Hits=sum(Hits),
                AB=sum(`At Bats`),
                XBH=sum(XBH),
                SO=sum(SO)
      ) %>% 
      ungroup() %>% 
      mutate(AVG=round(Hits/AB,3),
             `SO/G`=round(SO/games,2)
      ) %>% 
      arrange(desc(AVG))
    
    df
    
  }, options = list(lengthChange = FALSE,paging = FALSE)
  )
  

  ######
  # display for aggregate player data
  ######
  
  aggdata=reactive({
    # df=df_ %>% filter(Team=="Lugnuts")
    
    aggdata=df() %>% 
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
    
    aggdata
    
  })
  
  output$agg = renderDT({
    
    aggdata()
    
  }, options = list(lengthChange = FALSE,paging = FALSE)
  )
  
  output$boxplot <- renderPlot( 
    { 
      
      plotdata=aggdata() %>% 
        mutate(x_jittered=runif(n()))
        
      ggplot() +
        geom_boxplot(data = plotdata, aes(x = .5, y = AVG), outlier.shape = NA) +
        geom_point(data = (plotdata %>% filter(Name=="Otis H")),
                   aes(x = x_jittered, y = AVG),
                   shape=15, size=5, alpha = 0.7, color="black") +
        geom_point(data = (plotdata %>% filter(Team=="Lugnuts")),
                   aes(x = x_jittered, y = AVG),
                   shape=20, size=5, alpha = 0.7, color="black") +
        geom_point(data = plotdata, aes(x = x_jittered, y = AVG, color = Team),
                   size = 2, alpha = 0.7) +
        xlab("")
        
    } 
  ) 
  
  ######
  #display for raw data
  ######
  
  output$raw = renderDT(
    {
      
      df()

      }
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
                                      "First Half (Reg Season)" = "first half", 
                                      "Second Half (Reg Season)" = "second half", 
                                      "Second Half (Reg+Playoffs)" = "second plus", 
                                      "Last 3" = "last 3",
                                      "Last 6" = "last 6",
                                      "Regular Season" = "regular season",
                                      "Playoffs"="playoffs"
                       ), 
                       selected = "all")
          
        ),
        mainPanel(
        tabsetPanel(
          tabPanel("Team",
                   DTOutput('team')
                   
          ),
          tabPanel("Player",
                   DTOutput('agg'),
                   plotOutput("boxplot",height = "600px", width = "50%")
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
