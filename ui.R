
shinyUI(fluidPage(theme=shinytheme("united"),
  
  # Application title
  titlePanel("Drug Shortage Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput('select_ing','Select an Active Ingredient',active_ing),
      selectInput('select_company','Select a Company',c('Select a company name'='')),
      br(),
      h4("About"),
      HTML(paste0(
        "<p>",
        "This app is a prototype to show the drug shortage reports in Canada between 2002 and 2018.",
        "The data was obtained from <a href =\"https://www.drugshortagescanada.ca\" target='_blank'>",
        "https://www.drugshortagescanada.ca</a>. ",
        "At this development stage, our team welcome any feedbacks for improvement in contents and application performance",br()
      )),
      br(),
      h4("Authors: "),
      HTML(paste0(
        "Ted Kim",br(),
        "Regulatory Project Manager, HFPB", br(),
        "Health Canada / Government of Canada", br(),
        "taehyun.kim@canada.ca", br(),
        br(),
        "Nanqing Zhu, MSc", br(),
        "Data Scientist, HFPB", br(),
        "Health Canada / Government of Canada", br(),
        "nanqing.zhu@canada.ca"
      )),
      br(),
      br(),
      actionLink("remove", "Remove detail tabs")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='tabs',
        tabPanel(title='Main',
                 
        fluidRow(
          column(12,
                 htmlOutput('drugplot_title'),
                 plotlyOutput("drugplot")),
          column(12,div(style = "height:100px"),
                 htmlOutput('freqplot_title'),
                 p('Click on the bar to see detailed information on drugshortage'),
                 plotlyOutput("freqplot"))
                
      )
    )
  )
)

)))
