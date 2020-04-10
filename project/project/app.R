packages = c('tidyverse','shiny','shinydashboard','plotly','ggplot2','ggrepel')
for (p in packages){
    if (!require(p, character.only= T)){
        install.packages(p)
    }
    library(p,character.only= T)
}


exam <- read_csv("data/Exam_data.csv")
vg<-read_csv("data/vgsales.csv")
datacsv<-read_csv("data/Failure_pareto.csv")
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
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput(inputId = "slider1",
                                label="Years",
                                min=1980,
                                max=2020,
                                value = c(1980, 2020)
                                )
                            ),
                        mainPanel(plotlyOutput("timeseries"))
                    )
                    
            ),
            tabItem("subitem1",
                    "Sub-item 1 tab content",
                    sidebarLayout(
                        sidebarPanel(
                        ),
                        
                        mainPanel(
                            box(width = 12, plotOutput("paretoChart", height = 800))
                        )
                    )
            ),
            tabItem("subitem2",
                    "Sub-item 2 tab content",
                    sidebarLayout(
                        sidebarPanel(
                        ),
                        
                        mainPanel(
                            fileInput(
                                "file1","Upload an xlsx file",accept = ".xlsx"
                            ),
                            p("After uploading your CSV file, click on the 'Inspect the Data' tab"),
                            box(width = 12, plotOutput("plot1", height = 250))
                        )
                    )
            )
        )
    )
    
}



ui <- dashboardPage(skin = "black",header, sidebar, body)

server <- function(input, output){
    output$paretoChart <-renderPlot({
        dat <-vg[order(vg$Global_Sales,decreasing = TRUE),]
        myDf <-data.frame(sales = dat$Global_Sales,publisher= dat$Publisher, stringsAsFactors = FALSE)
        
        myDf<- myDf %>% as_tibble() %>% mutate(
            cumulative = cumsum(sales),
            freq = round(sales / sum(sales),3)*100,
            cum_freq = cumsum(freq)
        )
        myDf[dim(myDf)[1],dim(myDf)[2]] <- 100 #force last level
        
        pareto <- ggplot(myDf, aes(x=reorder(publisher,-sales),y=sales)) +
            geom_bar(aes(y=myDf$sales), fill='blue', stat="identity") +
            geom_point(aes(y=myDf$cumulative), color = rgb(0, 1, 0), pch=16, size=3) +
            geom_path(aes(y=myDf$cumulative, group=1)) +
            theme(axis.text.x = element_text(size = 12,angle=90, vjust=0.6)) +
            labs(title = "publisher Pareto Plot", 
                 subtitle = "Produced by David Arteta", 
                 x = 'Publishers', y = 'Cumulative Sales')
        
        pareto 
    })
    
    output$timeseries<- renderPlotly({
        minYear=99999
        maxYear=0
        for (r in 1:length(vg$Year)) {
            if (vg$Year[r]!="N/A"){ 
                if (vg$Year[r]<minYear){
                    minYear=vg$Year[r]
                }
                if (vg$Year[r]>maxYear){
                    maxYear=vg$Year[r]
                }
            }
        }
        YearRange <- c(minYear:maxYear)
        
        desired_length<-length(as.numeric(YearRange))
        v2600<-c()
        v3DO<-c()
        v3DS<-c()
        DC<-c()
        DS<-c()
        GB<-c()
        GBA<-c()
        GC<-c()
        Gen<-c()
        GG<-c()
        N64<-c()
        NES<-c()
        NG<-c()
        PC<-c()
        PCFX<-c()
        PS<-c()
        PS2<-c()
        PS3<-c()
        PS4<-c()
        PSP<-c()
        PSV<-c()
        SAT<-c()
        SCD<-c()
        SNES<-c()
        TG16<-c()
        Wii<-c()
        WiiU<-c()
        WS<-c()
        X360<-c()
        XB<-c()
        XOne<-c()
        
        for (r in 1:desired_length) {
            v2600<-c(v2600,NA)
            v3DO<-c(v3DO,NA)
            v3DS<-c(v3DS,NA)
            DC<-c(DC,NA)
            DS<-c(DS,NA)
            GB<-c(GB,NA)
            GBA<-c(GBA,NA)
            GC<-c(GC,NA)
            Gen<-c(Gen,NA)
            GG<-c(GG,NA)
            N64<-c(N64,NA)
            NES<-c(NES,NA)
            NG<-c(NG,NA)
            PC<-c(PC,NA)
            PCFX<-c(PCFX,NA)
            PS<-c(PS,NA)
            PS2<-c(PS2,NA)
            PS3<-c(PS3,NA)
            PS4<-c(PS4,NA)
            PSP<-c(PSP,NA)
            PSV<-c(PSV,NA)
            SAT<-c(SAT,NA)
            SCD<-c(SCD,NA)
            SNES<-c(SNES,NA)
            TG16<-c(TG16,NA)
            Wii<-c(Wii,NA)
            WiiU<-c(WiiU,NA)
            WS<-c(WS,NA)
            X360<-c(X360,NA)
            XB<-c(XB,NA)
            XOne<-c(XOne,NA)
        }
        for (r in 1:length(vg$Year)) {
            if (vg$Year[r]!="N/A"){
                index<-(as.numeric(vg$Year[r])-as.numeric(minYear))+1
                if (vg$Platform[r]=='2600'){
                    if (!is.na(v2600[index])){
                        v2600[index]<-v2600[index]+as.double(vg$Global_Sales[r])
                    }else{
                        v2600[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='3DO'){
                    if (!is.na(v3DO[index])){
                        v3DO[index]<-v3DO[index]+as.double(vg$Global_Sales[r])
                    }else{
                        v3DO[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='3DS'){
                    if (!is.na(v3DS[index])){
                        v3DS[index]<-v3DS[index]+as.double(vg$Global_Sales[r])
                    }else{
                        v3DS[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='DC'){
                    if (!is.na(DC[index])){
                        DC[index]<-DC[index]+as.double(vg$Global_Sales[r])
                    }else{
                        DC[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='DS'){
                    if (!is.na(DS[index])){
                        DS[index]<-DS[index]+as.double(vg$Global_Sales[r])
                    }else{
                        DS[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='GB'){
                    if (!is.na(GB[index])){
                        GB[index]<-GB[index]+as.double(vg$Global_Sales[r])
                    }else{
                        GB[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='GBA'){
                    if (!is.na(GBA[index])){
                        GBA[index]<-GBA[index]+as.double(vg$Global_Sales[r])
                    }else{
                        GBA[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='GC'){
                    if (!is.na(GC[index])){
                        GC[index]<-GC[index]+as.double(vg$Global_Sales[r])
                    }else{
                        GC[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='Gen'){
                    if (!is.na(Gen[index])){
                        Gen[index]<-Gen[index]+as.double(vg$Global_Sales[r])
                    }else{
                        Gen[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='GG'){
                    if (!is.na(GG[index])){
                        GG[index]<-GG[index]+as.double(vg$Global_Sales[r])
                    }else{
                        GG[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='N64'){
                    if (!is.na(N64[index])){
                        N64[index]<-N64[index]+as.double(vg$Global_Sales[r])
                    }else{
                        N64[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='NES'){
                    if (!is.na(v3DS[index])){
                        NES[index]<-NES[index]+as.double(vg$Global_Sales[r])
                    }else{
                        NES[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='NG'){
                    if (!is.na(NG[index])){
                        NG[index]<-NG[index]+as.double(vg$Global_Sales[r])
                    }else{
                        NG[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PC'){
                    if (!is.na(PC[index])){
                        PC[index]<-PC[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PC[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PCFX'){
                    if (!is.na(PCFX[index])){
                        PCFX[index]<-PCFX[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PCFX[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PS'){
                    if (!is.na(PS[index])){
                        PS[index]<-PS[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PS[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PS2'){
                    if (!is.na(PS2[index])){
                        PS2[index]<-PS2[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PS2[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PS3'){
                    if (!is.na(PS3[index])){
                        PS3[index]<-PS3[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PS3[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PS4'){
                    if (!is.na(PS4[index])){
                        PS4[index]<-PS4[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PS4[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PSP'){
                    if (!is.na(PSP[index])){
                        PSP[index]<-PSP[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PSP[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='PSV'){
                    if (!is.na(PSV[index])){
                        PSV[index]<-PSV[index]+as.double(vg$Global_Sales[r])
                    }else{
                        PSV[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='SAT'){
                    if (!is.na(SAT[index])){
                        SAT[index]<-SAT[index]+as.double(vg$Global_Sales[r])
                    }else{
                        SAT[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='SCD'){
                    if (!is.na(SCD[index])){
                        SCD[index]<-SCD[index]+as.double(vg$Global_Sales[r])
                    }else{
                        SCD[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='SNES'){
                    if (!is.na(SNES[index])){
                        SNES[index]<-SNES[index]+as.double(vg$Global_Sales[r])
                    }else{
                        SNES[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='TG16'){
                    if (!is.na(TG16[index])){
                        TG16[index]<-TG16[index]+as.double(vg$Global_Sales[r])
                    }else{
                        TG16[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='Wii'){
                    if (!is.na(Wii[index])){
                        Wii[index]<-Wii[index]+as.double(vg$Global_Sales[r])
                    }else{
                        Wii[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='WiiU'){
                    if (!is.na(WiiU[index])){
                        WiiU[index]<-WiiU[index]+as.double(vg$Global_Sales[r])
                    }else{
                        WiiU[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='WS'){
                    if (!is.na(WS[index])){
                        WS[index]<-WS[index]+as.double(vg$Global_Sales[r])
                    }else{
                        WS[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='X360'){
                    if (!is.na(X360[index])){
                        X360[index]<-X360[index]+as.double(vg$Global_Sales[r])
                    }else{
                        X360[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='XB'){
                    if (!is.na(XB[index])){
                        XB[index]<-XB[index]+as.double(vg$Global_Sales[r])
                    }else{
                        XB[index]<-as.double(vg$Global_Sales[r])
                    }
                }else if (vg$Platform[r]=='XOne'){
                    if (!is.na(XOne[index])){
                        XOne[index]<-XOne[index]+as.double(vg$Global_Sales[r])
                    }else{
                        XOne[index]<-as.double(vg$Global_Sales[r])
                    }
                }
            }
        }
        data <- data.frame(YearRange, v2600,v3DO,v3DS, DC,DS, GB, GBA,GC, Gen,GG, N64,NES,NG, PC, PCFX, PS, PS2, PS3, PS4,PSP,PSV,SAT,SCD,SNES, TG16, Wii,WiiU, WS, X360, XB,  XOne)
               
        p <- plot_ly(data, x = ~YearRange, y = ~v2600, name = 'v2600', type = 'scatter', mode = 'lines+markers',
                     line = list(color = 'rgb(205, 12, 24)', width = 4), hovertemplate = paste('<i>2600</b>',
                                                                                               '<br><i>%{x}</i><br>',
                                                                                               '<i>Total Sales: $%{y:.2f}</i>'))
        p <- p %>% add_trace(y = ~v3DO, name = 'v3DO', mode = 'lines+markers' ) 
        p <- p %>% add_trace(y = ~v3DS, name = 'v3DS', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~DC, name = 'DC', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~DS, name = 'DS', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~GB, name = 'GB', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~GBA, name = 'GBA', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~GC, name = 'GC', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~Gen, name = 'Gen', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~GG, name = 'GG', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~N64, name = 'N64', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~NES, name = 'NES', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~NG, name = 'NG', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PC, name = 'PC', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PCFX, name = 'PCFX', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PS, name = 'PS', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PS2, name = 'PS2', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PS3, name = 'PS3', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PS4, name = 'PS4', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PSP, name = 'PSP', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~PSV, name = 'PSV', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~SAT, name = 'SAT', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~SCD, name = 'SCD', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~SNES, name = 'SNES', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~TG16, name = 'TG16', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~Wii, name = 'Wii', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~WiiU, name = 'WiiU', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~WS, name = 'WS', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~X360, name = 'X360', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~XB, name = 'XB', mode = 'lines+markers') 
        p <- p %>% add_trace(y = ~XOne, name = 'XOne', mode = 'lines+markers') 
        
        # style the xaxis
        layout(p, xaxis = list(title = "Time", range = c(input$slider1[1], input$slider1[2]), autorange = F,
                               autotick = F, tick0 = minYear, dtick = 5), yaxis=list(title = "Sales"),title = "Sale over the years")
    })
    
    the_data_fn <- reactive({
        
        dat <- datacsv
        return(dat)
    })# end of reading file
    
    #display a pareto plot
    output$plot1 <- renderPlot({
        dat <- the_data_fn()
        dat <-dat[order(dat$N,decreasing = TRUE),]
        myDf <-data.frame(count = dat$N,failure= dat$failure, stringsAsFactors = FALSE)
        
        
        myDf<- myDf %>% as_tibble() %>% mutate(
            cumulative = cumsum(count),
            freq = round(count / sum(count),3)*100,
            cum_freq = cumsum(freq)
        )
        myDf[dim(myDf)[1],dim(myDf)[2]] <- 100 #force last level
        
        pareto <- ggplot(myDf, aes(x=reorder(failure,-count),y=count)) +
            geom_bar(aes(y=myDf$count), fill='blue', stat="identity") +
            geom_point(aes(y=myDf$cumulative), color = rgb(0, 1, 0), pch=16, size=3) +
            geom_path(aes(y=myDf$cumulative, group=1)) +
            theme(axis.text.x = element_text(size = 12,angle=90, vjust=0.6)) +
            labs(title = "Failure Pareto Plot", 
                 subtitle = "Produced by David Arteta", 
                 x = 'Failures', y = 'Cumulative Count')
        
        pareto + geom_label_repel(aes(label= myDf$cum_freq, y = myDf$cumulative),
                                  box.padding = 0.35,
                                  point.padding = 0.5,
                                  segment.color = 'grey50')
        
    })# end of renderPlot
    
    # display a table with the cumulative values
    output$summary <-renderPrint({
        dat <- the_data_fn()
        dat <-dat[order(dat$N,decreasing = TRUE),]
        myDf <-data.frame(count = dat$N,failure= dat$failure, stringsAsFactors = FALSE)
        
        myDf<- myDf %>% as_tibble() %>% mutate(
            cumsum = cumsum(count),
            freq = round(count / sum(count),3)*100,
            cum_freq = cumsum(freq)
        )
        myDf[dim(myDf)[1],dim(myDf)[2]] <- 100 #force last level
        return(myDf)
    })# end of renderPrint
}

shinyApp(ui=ui,server=server)