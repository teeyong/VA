#packages = c('tidyverse','shiny','shinydashboard','plotly','ggplot2','ggrepel','treemap')
#for (p in packages){
   # if (!require(p, character.only= T)){
  #      install.packages(p)
    #}
 #   library(p,character.only= T)
#}

library(shiny)
library(shinydashboard, warn.conflicts=FALSE)
library(tidyverse, warn.conflicts=FALSE)
library(plotly, warn.conflicts=FALSE)
library(ggplot2, warn.conflicts=FALSE)
library(ggrepel, warn.conflicts=FALSE)
library(treemap, warn.conflicts=FALSE)


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
                     menuSubItem("Sub-item 1", tabName = "heatmap1",  icon = icon("dashboard")),
                     menuSubItem("Sub-item 2", tabName = "heatmap2",  icon = icon("dashboard"))
            ),
            menuItem("Publisher", tabName = "Graph", icon = icon("dashboard"))
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
            tabItem("heatmap1",
                    mainPanel(
                        fluidRow(
                            column(4,plotOutput("heatmapNA", width=500,height=350)),
                            column(4,offset = 4,plotOutput("heatmapEU", width=500,height=350))
                        ),
                        fluidRow(
                            column(4,plotOutput("heatmapJP", width=500,height=350)),
                            column(4,offset = 4,plotOutput("heatmapOther", width=500,height=350))
                        )
                    )

                    
            ),
            tabItem("heatmap2",
                    mainPanel(
                        plotOutput("heatmapGlobal", width=800,height=600)
                    )
            ),
            tabItem("Graph",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(inputId="variable",
                                        label="Publisher:",
                                        choices=c("All",
                                                  "10TACLE Studios",
                                                  "1C Company",
                                                  "20th Century Fox Video Games",
                                                  "2D Boy",
                                                  "3DO",
                                                  "49Games",
                                                  "505 Games",
                                                  "5pb",
                                                  "7G//AMES",
                                                  "989 Sports",
                                                  "989 Studios",
                                                  "Abylight",
                                                  "Acclaim Entertainment",
                                                  "Accolade",
                                                  "Ackkstudios",
                                                  "Acquire",
                                                  "Activision",
                                                  "Activision Blizzard",
                                                  "Activision Value",
                                                  "Adeline Software",
                                                  "Aerosoft",
                                                  "Agatsuma Entertainment",
                                                  "Agetec",
                                                  "Aksys Games",
                                                  "Alawar Entertainment",
                                                  "Alchemist",
                                                  "Alternative Software",
                                                  "Altron",
                                                  "Alvion",
                                                  "American Softworks",
                                                  "Angel Studios",
                                                  "Answer Software",
                                                  "AQ Interactive",
                                                  "Aqua Plus",
                                                  "Aques",
                                                  "Arc System Works",
                                                  "Arena Entertainment",
                                                  "Aria",
                                                  "Arika",
                                                  "ArtDink",
                                                  "Aruze Corp",
                                                  "ASC Games",
                                                  "Ascaron Entertainment",
                                                  "Ascaron Entertainment GmbH",
                                                  "ASCII Entertainment",
                                                  "ASCII Media Works",
                                                  "Asgard",
                                                  "ASK",
                                                  "Asmik Ace Entertainment",
                                                  "Asmik Corp",
                                                  "Aspyr",
                                                  "Astragon",
                                                  "Asylum Entertainment",
                                                  "Atari",
                                                  "Athena",
                                                  "Atlus",
                                                  "Avalon Interactive",
                                                  "Avanquest",
                                                  "Avanquest Software",
                                                  "Axela",
                                                  "BAM! Entertainment",
                                                  "Banpresto",
                                                  "Benesse",
                                                  "Berkeley",
                                                  "Bethesda Softworks",
                                                  "Big Ben Interactive",
                                                  "Big Fish Games",
                                                  "Bigben Interactive",
                                                  "bitComposer Games",
                                                  "Black Bean Games",
                                                  "Black Label Games",
                                                  "Blast! Entertainment Ltd",
                                                  "Blue Byte",
                                                  "BMG Interactive Entertainment",
                                                  "Bohemia Interactive",
                                                  "Bomb",
                                                  "Boost On",
                                                  "BPS",
                                                  "Brash Entertainment",
                                                  "Broccoli",
                                                  "BushiRoad",
                                                  "Capcom",
                                                  "Cave",
                                                  "CBS Electronics",
                                                  "CCP",
                                                  "CDV Software Entertainment",
                                                  "ChunSoft",
                                                  "City Interactive",
                                                  "Cloud Imperium Games Corporation",
                                                  "Coconuts Japan",
                                                  "Codemasters",
                                                  "Codemasters Online",
                                                  "CokeM Interactive",
                                                  "Coleco",
                                                  "Comfort",
                                                  "Commseed",
                                                  "Compile",
                                                  "Compile Heart",
                                                  "Conspiracy Entertainment",
                                                  "Core Design Ltd.",
                                                  "CPG Products",
                                                  "Crave Entertainment",
                                                  "Creative Core",
                                                  "Crimson Cow",
                                                  "Crystal Dynamics",
                                                  "CTO SpA",
                                                  "Culture Brain",
                                                  "Culture Publishers",
                                                  "CyberFront",
                                                  "Cygames",
                                                  "D3Publisher",
                                                  "Daedalic",
                                                  "Daedalic Entertainment",
                                                  "Daito",
                                                  "Data Age",
                                                  "Data Design Interactive",
                                                  "Data East",
                                                  "Datam Polystar",
                                                  "Deep Silver",
                                                  "Destination Software, Inc",
                                                  "Destineer",
                                                  "Detn8 Games",
                                                  "Devolver Digital",
                                                  "DHM Interactive",
                                                  "DigiCube",
                                                  "Disney Interactive Studios",
                                                  "Dorart",
                                                  "dramatic create",
                                                  "DreamCatcher Interactive",
                                                  "DreamWorks Interactive",
                                                  "DSI Games",
                                                  "DTP Entertainment",
                                                  "Dusenberry Martin Racing",
                                                  "EA Games",
                                                  "Easy Interactive",
                                                  "Ecole",
                                                  "Edia",
                                                  "Eidos Interactive",
                                                  "Electronic Arts",
                                                  "Electronic Arts Victor",
                                                  "Elf",
                                                  "Elite",
                                                  "Empire Interactive",
                                                  "Encore",
                                                  "Enix Corporation",
                                                  "Enjoy Gaming ltd.",
                                                  "Enterbrain",
                                                  "EON Digital Entertainment",
                                                  "Epic Games",
                                                  "Epoch",
                                                  "Ertain",
                                                  "ESP",
                                                  "Essential Games",
                                                  "Evolution Games",
                                                  "Evolved Games",
                                                  "Excalibur Publishing",
                                                  "Experience Inc.",
                                                  "Extreme Entertainment Group",
                                                  "Falcom Corporation",
                                                  "Fields",
                                                  "Flashpoint Games",
                                                  "Flight-Plan",
                                                  "Focus Home Interactive",
                                                  "Focus Multimedia",
                                                  "fonfun",
                                                  "Foreign Media Games",
                                                  "Fortyfive",
                                                  "Fox Interactive",
                                                  "From Software",
                                                  "Fuji",
                                                  "Funbox Media",
                                                  "Funcom",
                                                  "FunSoft",
                                                  "Funsta",
                                                  "FuRyu",
                                                  "FuRyu Corporation",
                                                  "G.Rev",
                                                  "Gaga",
                                                  "Gainax Network Systems",
                                                  "Gakken",
                                                  "Game Arts",
                                                  "Game Factory",
                                                  "Game Life",
                                                  "Gamebridge",
                                                  "Gamecock",
                                                  "Gameloft",
                                                  "GameMill Entertainment",
                                                  "GameTek",
                                                  "Gathering of Developers",
                                                  "General Entertainment",
                                                  "Genki",
                                                  "Genterprise",
                                                  "Ghostlight",
                                                  "Giga",
                                                  "Giza10",
                                                  "Glams",
                                                  "Global A Entertainment",
                                                  "Global Star",
                                                  "GN Software",
                                                  "GOA",
                                                  "Gotham Games",
                                                  "Graffiti",
                                                  "Grand Prix Games",
                                                  "Graphsim Entertainment",
                                                  "Gremlin Interactive Ltd",
                                                  "Griffin International",
                                                  "Groove Games",
                                                  "GSP",
                                                  "GT Interactive",
                                                  "GungHo",
                                                  "Gust",
                                                  "Hackberry",
                                                  "HAL Laboratory",
                                                  "Hamster Corporation",
                                                  "Happinet",
                                                  "Harmonix Music Systems",
                                                  "Hasbro Interactive",
                                                  "Havas Interactive",
                                                  "Headup Games",
                                                  "Hearty Robin",
                                                  "Hect",
                                                  "Hello Games",
                                                  "Her Interactive",
                                                  "Hip Interactive",
                                                  "HMH Interactive",
                                                  "Home Entertainment Suppliers",
                                                  "Hudson Entertainment",
                                                  "Hudson Soft",
                                                  "Human Entertainment",
                                                  "HuneX",
                                                  "Iceberg Interactive",
                                                  "id Software",
                                                  "Idea Factory",
                                                  "Idea Factory International",
                                                  "IE Institute",
                                                  "Ignition Entertainment",
                                                  "Illusion Softworks",
                                                  "Imadio",
                                                  "Image Epoch",
                                                  "imageepoch Inc.",
                                                  "Imageworks",
                                                  "Imagic",
                                                  "Imagineer",
                                                  "Imax",
                                                  "Indie Games",
                                                  "Infogrames",
                                                  "Insomniac Games",
                                                  "Interchannel",
                                                  "Interchannel-Holon",
                                                  "Intergrow",
                                                  "Interplay",
                                                  "Interplay Productions",
                                                  "Interworks Unlimited, Inc.",
                                                  "Inti Creates",
                                                  "Introversion Software",
                                                  "inXile Entertainment",
                                                  "Irem Software Engineering",
                                                  "ITT Family Games",
                                                  "Ivolgamus",
                                                  "iWin",
                                                  "Jack of All Games",
                                                  "Jaleco",
                                                  "Jester Interactive",
                                                  "Jorudan",
                                                  "JoWood Productions",
                                                  "Just Flight",
                                                  "JVC",
                                                  "Kadokawa Games",
                                                  "Kadokawa Shoten",
                                                  "Kaga Create",
                                                  "Kalypso Media",
                                                  "Kamui",
                                                  "Kando Games",
                                                  "Karin Entertainment",
                                                  "Kemco",
                                                  "KID",
                                                  "Kids Station",
                                                  "King Records",
                                                  "Knowledge Adventure",
                                                  "Koch Media",
                                                  "Kokopeli Digital Studios",
                                                  "Konami Digital Entertainment",
                                                  "Kool Kizz",
                                                  "KSS",
                                                  "Laguna",
                                                  "Legacy Interactive",
                                                  "LEGO Media",
                                                  "Level 5",
                                                  "Lexicon Entertainment",
                                                  "Licensed 4U",
                                                  "Lighthouse Interactive",
                                                  "Liquid Games",
                                                  "Little Orbit",
                                                  "Locus",
                                                  "LSP Games",
                                                  "LucasArts",
                                                  "Mad Catz",
                                                  "Magical Company",
                                                  "Magix",
                                                  "Majesco Entertainment",
                                                  "Mamba Games",
                                                  "Marvel Entertainment",
                                                  "Marvelous Entertainment",
                                                  "Marvelous Games",
                                                  "Marvelous Interactive",
                                                  "Masque Publishing",
                                                  "Mastertronic",
                                                  "Mastiff",
                                                  "Mattel Interactive",
                                                  "Max Five",
                                                  "Maximum Family Games",
                                                  "Maxis",
                                                  "MC2 Entertainment",
                                                  "Media Entertainment",
                                                  "Media Factory",
                                                  "Media Rings",
                                                  "Media Works",
                                                  "MediaQuest",
                                                  "Men-A-Vision",
                                                  "Mentor Interactive",
                                                  "Mercury Games",
                                                  "Merscom LLC",
                                                  "Metro 3D",
                                                  "Michaelsoft",
                                                  "Micro Cabin",
                                                  "Microids",
                                                  "Microprose",
                                                  "Microsoft Game Studios",
                                                  "Midas Interactive Entertainment",
                                                  "Midway Games",
                                                  "Milestone",
                                                  "Milestone S.r.l",
                                                  "Milestone S.r.l.",
                                                  "Minato Station",
                                                  "Mindscape",
                                                  "Mirai Shounen",
                                                  "Misawa",
                                                  "Mitsui",
                                                  "mixi, Inc",
                                                  "MLB.com",
                                                  "Mojang",
                                                  "Monte Christo Multimedia",
                                                  "Moss",
                                                  "MTO",
                                                  "MTV Games",
                                                  "Mud Duck Productions",
                                                  "Mumbo Jumbo",
                                                  "Mycom",
                                                  "Myelin Media",
                                                  "Mystique",
                                                  "N/A",
                                                  "Namco Bandai Games",
                                                  "Natsume",
                                                  "Navarre Corp",
                                                  "Naxat Soft",
                                                  "NCS",
                                                  "NCSoft",
                                                  "NDA Productions",
                                                  "NEC",
                                                  "NEC Interchannel",
                                                  "Neko Entertainment",
                                                  "NetRevo",
                                                  "New",
                                                  "New World Computing",
                                                  "NewKidCo",
                                                  "Nexon",
                                                  "Nichibutsu",
                                                  "Nihon Falcom Corporation",
                                                  "Nintendo",
                                                  "Nippon Amuse",
                                                  "Nippon Columbia",
                                                  "Nippon Ichi Software",
                                                  "Nippon Telenet",
                                                  "Nitroplus",
                                                  "Nobilis",
                                                  "Nordcurrent",
                                                  "Nordic Games",
                                                  "NovaLogic",
                                                  "Number None",
                                                  "O3 Entertainment",
                                                  "Ocean",
                                                  "Office Create",
                                                  "O-Games",
                                                  "On Demand",
                                                  "Ongakukan",
                                                  "Origin Systems",
                                                  "Otomate",
                                                  "Oxygen Interactive",
                                                  "P2 Games",
                                                  "Pacific Century Cyber Works",
                                                  "Pack In Soft",
                                                  "Pack-In-Video",
                                                  "Palcom",
                                                  "Panther Software",
                                                  "Paon",
                                                  "Paon Corporation",
                                                  "Paradox Development",
                                                  "Paradox Interactive",
                                                  "Parker Bros.",
                                                  "Performance Designed Products",
                                                  "Phantagram",
                                                  "Phantom EFX",
                                                  "Phenomedia",
                                                  "Phoenix Games",
                                                  "Piacci",
                                                  "Pinnacle",
                                                  "Pioneer LDC",
                                                  "Play It",
                                                  "Playlogic Game Factory",
                                                  "Playmates",
                                                  "Playmore",
                                                  "PlayV",
                                                  "Plenty",
                                                  "PM Studios",
                                                  "Pony Canyon",
                                                  "PopCap Games",
                                                  "Popcorn Arcade",
                                                  "PopTop Software",
                                                  "Pow",
                                                  "PQube",
                                                  "Princess Soft",
                                                  "Prototype",
                                                  "Psygnosis",
                                                  "Quelle",
                                                  "Quest",
                                                  "Quinrose",
                                                  "Quintet",
                                                  "Rage Software",
                                                  "Rain Games",
                                                  "Rebellion",
                                                  "Rebellion Developments",
                                                  "RED Entertainment",
                                                  "Red Orb",
                                                  "Red Storm Entertainment",
                                                  "RedOctane",
                                                  "Reef Entertainment",
                                                  "responDESIGN",
                                                  "Revolution (Japan)",
                                                  "Revolution Software",
                                                  "Rising Star Games",
                                                  "Riverhillsoft",
                                                  "Rocket Company",
                                                  "Rondomedia",
                                                  "RTL",
                                                  "Russel",
                                                  "Sammy Corporation",
                                                  "Saurus",
                                                  "Scholastic Inc.",
                                                  "SCi",
                                                  "Screenlife",
                                                  "SCS Software",
                                                  "Sears",
                                                  "Sega",
                                                  "Seta Corporation",
                                                  "Seventh Chord",
                                                  "Shogakukan",
                                                  "Simon & Schuster Interactive",
                                                  "Slightly Mad Studios",
                                                  "Slitherine Software",
                                                  "SNK",
                                                  "SNK Playmore",
                                                  "Societa",
                                                  "Sold Out",
                                                  "Sonnet",
                                                  "Sony Computer Entertainment",
                                                  "Sony Computer Entertainment America",
                                                  "Sony Computer Entertainment Europe",
                                                  "Sony Music Entertainment",
                                                  "Sony Online Entertainment",
                                                  "SouthPeak Games",
                                                  "Spike",
                                                  "SPS",
                                                  "Square",
                                                  "Square EA",
                                                  "Square Enix",
                                                  "SquareSoft",
                                                  "SSI",
                                                  "Stainless Games",
                                                  "Starfish",
                                                  "Starpath Corp.",
                                                  "Sting",
                                                  "Storm City Games",
                                                  "Strategy First",
                                                  "Success",
                                                  "Summitsoft",
                                                  "Sunflowers",
                                                  "Sunrise Interactive",
                                                  "Sunsoft",
                                                  "Sweets",
                                                  "Swing! Entertainment",
                                                  "Syscom",
                                                  "System 3",
                                                  "System 3 Arcade Software",
                                                  "System Soft",
                                                  "T&E Soft",
                                                  "Taito",
                                                  "Takara",
                                                  "Takara Tomy",
                                                  "Take-Two Interactive",
                                                  "Takuyo",
                                                  "TalonSoft",
                                                  "TDK Core",
                                                  "TDK Mediactive",
                                                  "Team17 Software",
                                                  "Technos Japan Corporation",
                                                  "TechnoSoft",
                                                  "Tecmo Koei",
                                                  "Telegames",
                                                  "Telltale Games",
                                                  "Telstar",
                                                  "Tetris Online",
                                                  "TGL",
                                                  "The Adventure Company",
                                                  "The Learning Company",
                                                  "THQ",
                                                  "Tigervision",
                                                  "Time Warner Interactive",
                                                  "Titus",
                                                  "Tivola",
                                                  "TOHO",
                                                  "Tommo",
                                                  "Tomy Corporation",
                                                  "TopWare Interactive",
                                                  "Touchstone",
                                                  "Tradewest",
                                                  "Trion Worlds",
                                                  "Tripwire Interactive",
                                                  "Tru Blu Entertainment",
                                                  "Tryfirst",
                                                  "TYO",
                                                  "Type-Moon",
                                                  "U.S. Gold",
                                                  "Ubisoft",
                                                  "Ubisoft Annecy",
                                                  "UEP Systems",
                                                  "UFO Interactive",
                                                  "UIG Entertainment",
                                                  "Ultravision",
                                                  "Universal Gamex",
                                                  "Universal Interactive",
                                                  "Unknown",
                                                  "Valcon Games",
                                                  "ValuSoft",
                                                  "Valve",
                                                  "Valve Software",
                                                  "Vap",
                                                  "Vatical Entertainment",
                                                  "Vic Tokai",
                                                  "Victor Interactive",
                                                  "Video System",
                                                  "Views",
                                                  "Vir2L Studios",
                                                  "Virgin Interactive",
                                                  "Virtual Play Games",
                                                  "Visco",
                                                  "Vivendi Games",
                                                  "Wanadoo",
                                                  "Warashi",
                                                  "Wargaming.net",
                                                  "Warner Bros. Interactive Entertainment",
                                                  "Warp",
                                                  "WayForward Technologies",
                                                  "Westwood Studios",
                                                  "White Park Bay Software",
                                                  "Wizard Video Games",
                                                  "Xicat Interactive",
                                                  "Xing Entertainment",
                                                  "Xplosiv",
                                                  "XS Games",
                                                  "Xseed Games",
                                                  "Yacht Club Games",
                                                  "Yamasa Entertainment",
                                                  "Yeti",
                                                  "Yuke's",
                                                  "Yumedia",
                                                  "Zenrin",
                                                  "Zoo Digital Publishing",
                                                  "Zoo Games",
                                                  "Zushi Games"
                                                  ),
                                        selected="All")
                            
                        ),
                        mainPanel(
                            plotlyOutput("barchart1", width=1000,height=600)
                        )
                    )
            )

        )
    )
    
}



ui <- dashboardPage(skin = "black",header, sidebar, body)

server <- function(input, output){

    #timeseries
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
               
        p <- plot_ly(data, x = ~YearRange, y = ~v2600, name = '2600', type = 'scatter', mode = 'lines+markers',
                     line = list(color = 'rgb(205, 12, 24)', width = 4), hovertemplate = paste('<i>2600</b>',
                                                                                               '<br><i>%{x}</i><br>',
                                                                                               '<i>Total Sales: $%{y:.2f} million</i>'))
        p <- p %>% add_trace(y = ~v3DO, name = '3DO', mode = 'lines+markers' ,hovertemplate = paste('<i>3DO</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>')) 
        p <- p %>% add_trace(y = ~v3DS, name = '3DS', mode = 'lines+markers',hovertemplate = paste('<i>3DS</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~DC, name = 'DC', mode = 'lines+markers',hovertemplate = paste('<i>DC</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~DS, name = 'DS', mode = 'lines+markers',hovertemplate = paste('<i>DS</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~GB, name = 'GB', mode = 'lines+markers',hovertemplate = paste('<i>GB</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~GBA, name = 'GBA', mode = 'lines+markers',hovertemplate = paste('<i>GBA</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~GC, name = 'GC', mode = 'lines+markers',hovertemplate = paste('<i>GC</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~Gen, name = 'Gen', mode = 'lines+markers',hovertemplate = paste('<i>Gen</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~GG, name = 'GG', mode = 'lines+markers',hovertemplate = paste('<i>GG</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~N64, name = 'N64', mode = 'lines+markers',hovertemplate = paste('<i>N64</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million million</i>'))  
        p <- p %>% add_trace(y = ~NES, name = 'NES', mode = 'lines+markers',hovertemplate = paste('<i>NES</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~NG, name = 'NG', mode = 'lines+markers',hovertemplate = paste('<i>NG</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PC, name = 'PC', mode = 'lines+markers',hovertemplate = paste('<i>PC</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PCFX, name = 'PCFX', mode = 'lines+markers',hovertemplate = paste('<i>PCFX</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PS, name = 'PS', mode = 'lines+markers',hovertemplate = paste('<i>PS</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PS2, name = 'PS2', mode = 'lines+markers',hovertemplate = paste('<i>PS2</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PS3, name = 'PS3', mode = 'lines+markers',hovertemplate = paste('<i>PS3</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PS4, name = 'PS4', mode = 'lines+markers',hovertemplate = paste('<i>PS4</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PSP, name = 'PSP', mode = 'lines+markers',hovertemplate = paste('<i>PSP</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~PSV, name = 'PSV', mode = 'lines+markers',hovertemplate = paste('<i>PSV</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~SAT, name = 'SAT', mode = 'lines+markers',hovertemplate = paste('<i>SAT</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~SCD, name = 'SCD', mode = 'lines+markers',hovertemplate = paste('<i>SCD</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~SNES, name = 'SNES', mode = 'lines+markers',hovertemplate = paste('<i>SNES</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~TG16, name = 'TG16', mode = 'lines+markers',hovertemplate = paste('<i>TG16</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~Wii, name = 'Wii', mode = 'lines+markers',hovertemplate = paste('<i>Wii</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~WiiU, name = 'WiiU', mode = 'lines+markers',hovertemplate = paste('<i>WiiU</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~WS, name = 'WS', mode = 'lines+markers',hovertemplate = paste('<i>WS</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~X360, name = 'X360', mode = 'lines+markers',hovertemplate = paste('<i>X360</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~XB, name = 'XB', mode = 'lines+markers',hovertemplate = paste('<i>XB</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        p <- p %>% add_trace(y = ~XOne, name = 'XOne', mode = 'lines+markers',hovertemplate = paste('<i>XOne</b>','<br><i>%{x}</i><br>','<i>Total Sales: $%{y:.2f} million</i>'))  
        
        # style the xaxis
        layout(p, xaxis = list(title = "Time", range = c(input$slider1[1], input$slider1[2]), autorange = F,
                               autotick = F, tick0 = minYear, dtick = 5), yaxis=list(title = "Sales in million"),title = "Sale over the years")
    })
    
    #heatmap
    output$heatmapNA <-renderPlot({
        vg_grouped <- group_by(vg, `Genre`)
        vg_summarised <- summarise(vg_grouped, 
                                            `Total NA_Sales` = sum(`NA_Sales`, na.rm = TRUE),
                                            `Total EU_Sales` = sum(`EU_Sales`, na.rm = TRUE),
                                            `Total JP_Sales` = sum(`JP_Sales`, na.rm = TRUE), 
                                            `Total Other_Sales` = sum(`Other_Sales`, na.rm = TRUE),
                                            `Total Global_Sales` = sum(`Global_Sales`, na.rm = TRUE))
        treemap(vg_summarised,
                index=c("Genre"),
                vSize="Total NA_Sales",
                vColor="Total NA_Sales",
                type = "value",
                title="Treemap for NA",
                title.legend = "Total NA_Sales in million"
        )
    })
    
    output$heatmapEU <-renderPlot({
        vg_grouped <- group_by(vg, `Genre`)
        vg_summarised <- summarise(vg_grouped, 
                                   `Total NA_Sales` = sum(`NA_Sales`, na.rm = TRUE),
                                   `Total EU_Sales` = sum(`EU_Sales`, na.rm = TRUE),
                                   `Total JP_Sales` = sum(`JP_Sales`, na.rm = TRUE), 
                                   `Total Other_Sales` = sum(`Other_Sales`, na.rm = TRUE),
                                   `Total Global_Sales` = sum(`Global_Sales`, na.rm = TRUE))
        treemap(vg_summarised,
                index=c("Genre"),
                vSize="Total EU_Sales",
                vColor="Total EU_Sales",
                type = "value",
                title="Treemap for EU",
                title.legend = "Total EU_Sales in million"
        )
    })
    
    output$heatmapJP <-renderPlot({
        vg_grouped <- group_by(vg, `Genre`)
        vg_summarised <- summarise(vg_grouped, 
                                   `Total NA_Sales` = sum(`NA_Sales`, na.rm = TRUE),
                                   `Total EU_Sales` = sum(`EU_Sales`, na.rm = TRUE),
                                   `Total JP_Sales` = sum(`JP_Sales`, na.rm = TRUE), 
                                   `Total Other_Sales` = sum(`Other_Sales`, na.rm = TRUE),
                                   `Total Global_Sales` = sum(`Global_Sales`, na.rm = TRUE))
        treemap(vg_summarised,
                index=c("Genre"),
                vSize="Total JP_Sales",
                vColor="Total JP_Sales",
                type = "value",
                title="Treemap for JP",
                title.legend = "Total JP_Sales in million"
        )
    })
    
    output$heatmapOther <-renderPlot({
        vg_grouped <- group_by(vg, `Genre`)
        vg_summarised <- summarise(vg_grouped, 
                                   `Total NA_Sales` = sum(`NA_Sales`, na.rm = TRUE),
                                   `Total EU_Sales` = sum(`EU_Sales`, na.rm = TRUE),
                                   `Total JP_Sales` = sum(`JP_Sales`, na.rm = TRUE), 
                                   `Total Other_Sales` = sum(`Other_Sales`, na.rm = TRUE),
                                   `Total Global_Sales` = sum(`Global_Sales`, na.rm = TRUE))
        treemap(vg_summarised,
                index=c("Genre"),
                vSize="Total Other_Sales",
                vColor="Total Other_Sales",
                type = "value",
                title="Treemap for Other",
                title.legend = "Total Other_Sales in million"
        )
    })
    
    output$heatmapGlobal <-renderPlot({
        vg_grouped <- group_by(vg, `Genre`)
        vg_summarised <- summarise(vg_grouped, 
                                   `Total NA_Sales` = sum(`NA_Sales`, na.rm = TRUE),
                                   `Total EU_Sales` = sum(`EU_Sales`, na.rm = TRUE),
                                   `Total JP_Sales` = sum(`JP_Sales`, na.rm = TRUE), 
                                   `Total Other_Sales` = sum(`Other_Sales`, na.rm = TRUE),
                                   `Total Global_Sales` = sum(`Global_Sales`, na.rm = TRUE))
        treemap(vg_summarised,
                index=c("Genre"),
                vSize="Total Global_Sales",
                vColor="Total Global_Sales",
                type = "value",
                title="Treemap for Global Sales",
                title.legend = "Total Global_Sales in million"
        )
    })
    
    #barchart
    output$barchart1 <-renderPlotly({
        vg_grouped <- group_by(vg, `Publisher`)
        vg_summarised <- summarise(vg_grouped, 
                                   `Total NA_Sales` = sum(`NA_Sales`, na.rm = TRUE),
                                   `Total EU_Sales` = sum(`EU_Sales`, na.rm = TRUE),
                                   `Total JP_Sales` = sum(`JP_Sales`, na.rm = TRUE), 
                                   `Total Other_Sales` = sum(`Other_Sales`, na.rm = TRUE),
                                   `Total Global_Sales` = sum(`Global_Sales`, na.rm = TRUE))
        if (input$variable=='All'){
            vg_summarised <-vg_summarised[order(vg_summarised$`Total Global_Sales`,decreasing = TRUE),]
            vg_summarised$Publisher<-factor(vg_summarised$Publisher, levels = unique(vg_summarised$Publisher)[order(vg_summarised$`Total Global_Sales`, decreasing = TRUE)])
        }else{
            vg_summarised <- vg_summarised %>%
                filter(`Publisher` ==input$variable)
            vg_summarised <-vg_summarised[order(vg_summarised$`Total Global_Sales`,decreasing = TRUE),]
            vg_summarised$Publisher<-factor(vg_summarised$Publisher, levels = unique(vg_summarised$Publisher)[order(vg_summarised$`Total Global_Sales`, decreasing = TRUE)])
        }

        p <- plot_ly(vg_summarised, x = ~`Publisher`, y = ~`Total NA_Sales`, name = 'Total NA_Sales', type = 'bar',text = ~`Total NA_Sales`, textposition = 'auto')
        p <- p %>% add_trace(y = ~`Total EU_Sales`, name = 'Total EU_Sales',text = ~`Total EU_Sales`, textposition = 'auto')
        p <- p %>% add_trace(y = ~`Total JP_Sales`, name = 'Total JP_Sales',text = ~`Total JP_Sales`, textposition = 'auto')
        p <- p %>% add_trace(y = ~`Total Other_Sales`, name = 'Total Other_Sales',text = ~`Total Other_Sales`, textposition = 'auto')
        
        # style the xaxis
        layout(p, xaxis = list(title = "Publisher"), yaxis=list(title = "Sales in million"),title = "Sale over the years")
        
    })
    
}

shinyApp(ui=ui,server=server)