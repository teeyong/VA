packages = c('tidyverse','shiny','shinydashboard','plotly')
for (p in packages){
    if (!require(p, character.only= T)){
        install.packages(p)
    }
    library(p,character.only= T)
}


exam <- read_csv("data/Exam_data.csv")
vg<-read_csv("data/vgsales.csv")
#interface with interactivity
if (interactive()) {
    header <- dashboardHeader(title=img(src = "nogamenolife.png",height='40',width='100'))
    
    sidebar <- dashboardSidebar(
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem("About Us", tabName = "aboutus", icon = icon("info")),
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem("Details", icon = icon("dashboard"),
                     menuSubItem("Sub-item 1", tabName = "subitem1",  icon = icon("dashboard")),
                     menuSubItem("Sub-item 2", tabName = "subitem2",  icon = icon("dashboard"))
            )
        ),collapsed = FALSE
    )
    
    body <- dashboardBody(
        tabItems(
            tabItem("aboutus",
                    tags$h1("Problem and Motivation"),
                    tags$h4("The video game industry has grown massively over the years, with $2.3 billion in sales in the US alone in 2019. However, the industry has also shown signs of stagnation, with a 19% decline in sales from the previous year. One issue game developers face is the disconnect between console and game sales. Even though there are plenty of visualisations on console sales, there is a lack of visualisation and insight on games sales themselves, especially across genres and platforms. This is crucial as console sales figures are meaningless without any insights on the games played on those consoles."),
                    tags$h4("With an increasingly saturated market and disruption caused by mobile and VR games, we aim to create an interactive platform to understand game sales trends in order to validate console sales trends and to identify the best performing games, regions and genres across the different consoles."),
                    tags$h1("Our Objective"),
                    h4(tags$ol(
                        tags$li("Identify game sales trends within each console since 1980. We will also identify the top performing modern console (PS4, XBOX1, PC or Wii U)"),
                        tags$li("Find relationships between consoles and genres, regions and publishers"),
                        tags$li("If possible, extrapolate current games sales trends into the future for each console"))
                       ),
            ),
            tabItem("overview",
                    mainPanel(plotOutput("timeseries"))
                    
            ),
            tabItem("subitem1",
                    "Sub-item 1 tab content",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(inputId="variable",
                                        label="Subject:",
                                        choices=c("English"="ENGLISH",
                                                  "Maths"="MATHS",
                                                  "Science"="SCIENCE"),
                                        selected="ENGLISH"),
                            sliderInput(inputId = "bin",
                                        label="Number of Bins",
                                        min=5,
                                        max=20,
                                        value=10),
                            checkboxInput(inputId = "show_data",
                                          label="Show data table",
                                          value=TRUE)
                        ),
                        
                        mainPanel(
                            plotOutput("distPlot"),
                            DT:: dataTableOutput(outputId = "examtable")
                        )
                    )
            ),
            tabItem("subitem2",
                    "Sub-item 2 tab content"
            )
        )
    )
    
}



ui <- dashboardPage(skin = "black",header, sidebar, body)

server <- function(input, output){
    output$distPlot <-renderPlot({
        x<- unlist(exam[,input$variable])
        ggplot(exam,aes(x))+
            geom_histogram(bins = input$bin,
                           color="black",
                           fill="lightblue")
    })
    
    output$examtable<-DT::renderDataTable({
        if (input$show_data){
            DT::datatable(data=exam %>% select(1:7),
                          options =list(pageLength=10),
                          rownames = FALSE)
        }
    })
    
    output$timeseries<- renderPlot({
        xmin<-min(vg$Year)
        xmax<-max(vg$Year)
        ggplot(data=vg,aes(x=Year,y=Global_Sales))+
            geom_line()+scale_x_date(breaks='5 years')
    })
}

shinyApp(ui=ui,server=server)