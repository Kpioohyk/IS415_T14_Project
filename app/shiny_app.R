# R packages to be loaded
packages = c('sf','spdep','rgdal','tmap','tidyverse', 'shinycssloaders', 'shinythemes', 'DT', 'markdown')
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Import Data
# Geospatial data (shapefile)
area <- st_read(dsn = "data/geospatial/StudyArea.shp", layer = "StudyArea")
area <- area %>%
    rename(
        PROVINCE_ID = PROVNO,
        CITY_ID = KABKOTNO,
        DISTRICT_ID = KECNO,
        VILLAGE_ID = DESANO,
        PROVINCE = PROVINSI,
        CITY = KABKOT,
        DISTRICT =KECAMATAN,
        VILLAGE = DESA,
        REGION_CODE = IDDESA,
        YEAR = TAHUN,
        SOURCE = SUMBER,
        GEOMETRY = geometry
    )

# Aspatial data (CSV file)
podes_final <- read_csv("data/aspatial/podes_final.csv") #, sep = ",")
podes_final$PROVINCE_ID <- as.character(podes_final$PROVINCE_ID)
podes_final$CITY_ID <- as.character(podes_final$CITY_ID)
podes_final$DISTRICT_ID <- as.character(podes_final$DISTRICT_ID)
podes_final$VILLAGE_ID <- as.character(podes_final$VILLAGE_ID)
#print(ncol(podes_final))
#print(nrow(podes_final))

# Join spatial & aspatial data
eastkalimantan_podes <- dplyr::inner_join(area, podes_final, by = c("PROVINCE_ID", "CITY_ID", "DISTRICT_ID", "VILLAGE_ID", "PROVINCE", "CITY", "DISTRICT", "VILLAGE")) 
#print(head(eastkalimantan_podes))
print(nrow(eastkalimantan_podes))

# EDA Categorical Variables
var_lod <- c("City" = "CITY",
             "District" = "DISTRICT",
             "Village" = "VILLAGE")

cat_var <- c("Topography" = "VILLAGE/SUBDISTRICT_BY_TOPOGRAPHY",
             "Location to Forest" = "VILLAGE_LOCATION_TO_FOREST",
             "Forest function" = "FOREST_FUNCTION",
             "Main income" = "VILLAGE_MAIN_INCOME")

# EDA Continuous variables
con_var <- c("No. of Slums" = "NUM_OF_SLUMS",
             "Early childhood centre" = "EARLYCHILDHOOD_PUBLIC_INSTITUTION_NUM",
             "Kindergarten" = "KINDERGARTEN_PUBLIC_INSTITUTION_NUM",
             "Primary school" = "PRIMARY_PUBLIC_INSTITUTION_NUM",
             "Middle school" = "MIDDLE_PUBLIC_INSTITUTION_NUM",
             "Polytechnic" = "POLYTECHNIC_PUBLIC_INSTITUTION_NUM", 
             "College" = "COLLEGE_PUBLIC_INSTITUTION_NUM")


# Define UI
ui <- fluidPage(theme = shinytheme("simplex"),
                # Navigation Bar
                navbarPage(
                    title = div(img(src = 'logo_john.jpg', style = "margin-top: 0px; padding-right:4px;padding-bottom:20px", height = 70)),
                    windowTitle = "East Kalimantan as The New J-Town",
                    collapsible = TRUE,
                    
                    # About Panel
                    tabPanel("About", 
                             # h2(tags$strong("East Kalimantan as the New J-Town"), style = "color: #3f7300;"),
                             # tags$hr(),
                             sidebarLayout(
                                 sidebarPanel(h3(tags$strong("East Kalimantan as the New J-Town"), style = "color: #3f7300;"),
                                              #h4("IS415 Geospatial Analytics and Application"),
                                              tags$br(),
                                              h4("A Group Project by:"),
                                              tags$ul(
                                                  tags$li(tags$a(href = "https://www.linkedin.com/in/erikaaldisa/", "Erika Aldisa Gunawan"), style = "font-size: 18px;"),
                                                  tags$li(tags$a(href = "http://www.linkedin.com/in/xunjieng53b81m", "Ng Xun Jie"), style = "font-size: 18px;"),
                                                  tags$li("Sean Lee Jun Hui", style = "font-size: 18px;")
                                                  ),
                                              tags$br(),
                                              h4("Guided by Professor Kam Tin Seong for IS415 Geospatial Analytics and Application"),
                                              #div(img(src = 'smu_logo.png', style = "margin-top:0px; padding-right:0px;padding-bottom:0px", height = 130)),
                                              imageOutput("smu_logo"),
                                              width = 3
                                              #tags$style(
                                              #    HTML('.well {background-color: #ffffff;}'))
                                              ),
                                 mainPanel(h3("Project Motivation"),
                                           tags$hr(),
                                           h4("On 26th August 2019, Indonesia announced new capital city site would be moved to East Kalimantan"),
                                           fluidRow(
                                               # column(6,
                                               #        imageOutput("news_map"),
                                               #        tags$a(href = "https://www.theguardian.com/world/2019/aug/26/indonesia-new-capital-city-borneo-forests-jakarta", "Source: The Guardian"
                                               #        )),
                                               column(5,
                                               h4("The intention of the Indonesian government was to move away from the heavily populated Java island to slow down the 
                                               environmental degradation of Jakarta and to unify the archipelago by developing Kalimantan 
                                                  which is geographically the central of Indonesia.", style = "margin-top:5px;"),
                                               tags$br(),
                                               h4("The move garnered criticisms from urban planning experts and environmentalists 
                                               because the move was not strategic, potentially causing more environmental damage and it 
                                                  does not mean that Jakarta’s problems will be significantly resolved."),
                                               tags$br(),
                                               h4("Our project aims to conduct geospatial analysis on the 
                                               proposed area of the new capital city in East Kalimantan to learn more 
                                               about the area’s socio-economical potential to be considered as the 
                                                  new capital city of Indonesia.")
                                               ),
                                               column(7,
                                                      imageOutput("news_map"),
                                                      tags$a(href = "https://www.theguardian.com/world/2019/aug/26/indonesia-new-capital-city-borneo-forests-jakarta", "Source: The Guardian"
                                                             ))
                                               ),
                                           h3("Project Objectives"),
                                           tags$hr(),
                                           tags$ul(
                                               tags$li("To explore the proposed area of the new capital city site by conducting Exploratory Data Analysis on East Kalimantan PODES 2018 data", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("To conduct geographical segmentation and profiling to learn about different villages' suitability to be the new capital city site", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("To analyse and recommend the most suitable site/area for the new capital city based on chosen variables", style = "font-size: 17px; font-weight: 500;")
                                               ),
                                           h3("Application"),
                                           tags$hr(),
                                           tags$ol(
                                               tags$li("Exploratory Data Analysis (EDA), to visualize the boxplot & choropleth map of the chosen categorical/continuous variable of PODES 2018 data", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("Spatially Constrained Clustering using SKATER method, to visualize the clusters formed & correlation analysis graph by choosing the input variables, number of clusters and distance method used", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("Spatially Constrained Clustering using ClustGeo method, to visualize the clusters formed & correlation analysis graph by choosing the input variables, number of clusters, hierarchical cluster agglomeration method and clustgeo alpha value", style = "font-size: 17px; font-weight: 500;")
                                               ),
                                           h3("R Blogdown Page"),
                                           tags$hr(),
                                           uiOutput("blogdown"),
                                           tags$br(),
                                           width = 9
                                           )
                                 )),
                    tags$br(),
                    
                    # EDA Panel
                    tabPanel("EDA",
                             #h3(tags$strong("Geographical Profiling & Segmentation of the new proposed capital city in East Kalimantan"), style = "color: #3f7300;"),
                             #tags$hr(),
                             #h5("Create Choropleth Maps based on Indonesia's village potential (podes) data collected in 2018."),
                             #tags$br(),
                             sidebarLayout(
                                 sidebarPanel(fluid = TRUE, width = 3,
                                              conditionalPanel(
                                                  'input.EDA_var === "Categorical Var."',
                                                  tags$strong("Categorical Variable Inputs"),
                                                  #helpText("Click the Categorical Var. Tab to see the changes"),
                                                  selectInput(inputId = "var_cat",
                                                              label = "Choose a categorical var to display:",
                                                              choices = cat_var,
                                                              selected = "Topography"),
                                                  selectInput(inputId = "palette_cat",
                                                              label = "Palette:",
                                                              choices = c("Accent" = "Accent",
                                                                          "Pastel 1" = "Pastel1", 
                                                                          "Pastel 2" = "Pastel2",
                                                                          "Set 3" = "Set3"),
                                                              selected = "Accent")
                                                  ),
                                              
                                              conditionalPanel(
                                                  'input.EDA_var === "Administrative Areas"',
                                                  tags$strong("Administrative Areas in East Kalimantan Province"),
                                                  selectInput(inputId = "var_lod",
                                                              label = "Choose the Level of Detail:",
                                                              choices = var_lod,
                                                              selected = "City"),
                                                  selectInput(inputId = "palette_lod",
                                                              label = "Palette:",
                                                              choices = c("Accent" = "Accent",
                                                                          "Pastel 1" = "Pastel1", 
                                                                          "Pastel 2" = "Pastel2",
                                                                          "Set 3" = "Set3"),
                                                              selected = "Accent")
                                                  ),
                                            
                                              conditionalPanel(
                                                  'input.EDA_var === "Continuous Var."',
                                                  tags$strong("Continuous Variable Inputs"),
                                                  #helpText("Click the Continuous Var. Tab to see the changes"),
                                                  selectInput(inputId = "var_con", 
                                                              label = "Choose a continuous var to display:",
                                                              choices = con_var,
                                                              selected = "Slums"),
                                                  radioButtons(inputId = "style",
                                                               label = "Style for continuous variable:",
                                                               choices = c("Pretty" = "pretty",
                                                                           "Quantile" = "quantile", 
                                                                           "Jenks" = "jenks",
                                                                           "Equal" = "equal"),
                                                               selected = "pretty"),
                                                  sliderInput(inputId = "bin",
                                                              label = "Number of classes:",
                                                              min = 1,
                                                              max = 10,
                                                              step = 1,
                                                              value = 3),
                                                  selectInput(inputId = "palette_con",
                                                              label = "Palette:",
                                                              choices = c("Blues" = "Blues",
                                                                          "Greens" = "Greens", 
                                                                          "Reds" = "Reds",
                                                                          "Yellow & Green" = "YlGn",
                                                                          "Red & Blue" = "RdBu",
                                                                          "Blue & Red" = "-RdBu",
                                                                          "Purple & Blue" = "PuBu"),
                                                              selected = "Blues")
                                                  )
                                              ),
                                              
                                 mainPanel(
                                     tabsetPanel(
                                         id = "EDA_var",
                                         tabPanel("Categorical Var.",
                                                  column(7,
                                                         tmapOutput("podes_cat_map")
                                                         ),
                                                  column(5,
                                                         h2("Box plot here")
                                                         )
                                                  ),
                                         tabPanel("Continuous Var.", 
                                                  column(7,
                                                         tmapOutput("podes_con_map")
                                                         ),
                                                  column(5,
                                                         h2("Box plot here")
                                                         )
                                                  ),
                                         tabPanel("Administrative Areas",
                                                  column(7,
                                                         tmapOutput("podes_cat_lod"))
                                                  )
                                         )
                                     )
                                 )
                             ),
                    
                    #SKATER Clustering Panel
                    tabPanel("Clustering - SKATER",
                             h3(tags$strong("SKATER method"), style = 'color: #3f7300;')
                             ),
                    
                    #ClustGeo Clustering Panel
                    tabPanel("Clustering - ClustGeo",
                             h3(tags$strong("ClustGeo method"), style = 'color: #3f7300;')
                             ),
                    
                    # Data table Panel
                    tabPanel("Data",
                             h3(tags$strong("Data frame of PODES 2018 Data"), style = "color: #3f7300;"),
                             tags$hr(),
                             h5("Some of the variables of PODES data, Potensi Desa (village potential data), are displayed in the data table below."),
                             h5("Village is Indonesia's smallest administrative area and there are two types of villages, Desa (rural village) and Kelurahan (urban village),"),
                             h5("According to the 2019 report by the Ministry of Home Affairs, there are 8,488 urban villages and 74,953 rural villages in Indonesia."),
                             h5("PODES data is obtained from Professor Kam Tin Seong & produced by Badan Pusat Statistik, Central Statistics Body Indonesia."),
                             tags$br(),
                             tags$br(),
                             DT::dataTableOutput(outputId = "podes")),
                             tags$head(tags$style(HTML('.navbar-brand {width: 130px;
                             text-align:center;
                             padding-top: 0px;
                             padding-bottom: -10px;}',
                             '.navbar { background-color: #ffffff; 
                             font-family: Arial;
                             font-size: 18px;
                             min-height: 65px;
                             padding-top: 10px;
                             padding-bottom: 0px;
                             color: #FF0000; }',
                             '.navbar-dropdown { 
                             font-family: Arial;
                             font-size: 15px;
                             color: #FF0000; }')))
                    )
                    
                )
    


# Define server 
# server <- function(input, output, session) {
#     output$podes_cat_map <- renderTmap({
#         input$button
#         
#         x <- input$var_cat
#         tm_shape(eastkalimantan_podes)+
#             tm_fill(col = x, 
#                     #style = input$style,
#                     #breaks = input$breaks,
#                     palette = input$palette)+
#             tm_borders(alpha = 0.5, lwd = 0.5)+
#             tm_layout(legend.height = 0.50, 
#                       legend.width = 0.44, 
#                       legend.outside = TRUE,
#                       legend.position = c("right", "bottom"),
#                       frame = FALSE) +
#             tm_compass(type="8star", size = 2) +
#             tm_scale_bar(width = 0.15)
#     })
#     output$podes_con_map <- renderTmap({
#         input$button
#         
#         y <- input$var_con
#         tm_shape(eastkalimantan_podes)+
#             tm_fill(col = y, 
#                     style = input$style,
#                     breaks = input$breaks,
#                     palette = input$palette)+
#             tm_borders(alpha = 0.5, lwd = 0.5)+
#             tm_layout(legend.height = 0.50, 
#                       legend.width = 0.44, 
#                       legend.outside = TRUE,
#                       legend.position = c("right", "bottom"),
#                       frame = FALSE) +
#             tm_compass(type="8star", size = 2) +
#             tm_scale_bar(width = 0.15)
#     })
#     output$podes <- DT::renderDataTable({
#         DT::datatable(data = eastkalimantan_podes %>% select(1:10),
#                       options = list(pageLength = 10),
#                       rownames = FALSE)
#     })
#     output$news_map <- renderImage({
#         width <- session$clientData$output_news_map_width
#         height <- session$clientData$output_news_map_height
#         
#         list(
#             src = "www/news.jpg",
#             contentType = "image/png",
#             width = width,
#             height = height,
#             alt = "New Proposed Capital City of Indonesia"
#         )
#     })
#     output$logo <- renderImage({
#         width <- session$clientData$output_news_map_width
#         height <- session$clientData$output_news_map_height
#         
#         list(
#             src = "www/logo.png",
#             contentType = "image/png",
#             width = width,
#             height = height,
#             alt = "Logo"
#         ) 
#     })
# }

server <- function(input, output, session) {
    #observeEvent(input$controller, {
    #    updateTabsetPanel(session, "inTabset", selected = paste0("panel", input$controller))
    #})
    output$podes_cat_map <- renderTmap({
        #input$button
        
        x <- input$var_cat
        tm_shape(eastkalimantan_podes)+
            tm_fill(col = x, 
                    style = "cat",
                    #breaks = input$breaks,
                    palette = input$palette_cat,
                    id = "VILLAGE",
                    legend.show = TRUE)
            #tm_borders(alpha = 0.5, lwd = 0.5)+
            #tm_layout(legend.height = 0.50, 
            #          legend.width = 0.44, 
            #          legend.outside = TRUE,
            #          legend.position = c("right", "bottom"),
            #          frame = FALSE) +
            #tm_compass(type="8star", size = 2) +
            #tm_scale_bar(width = 0.15)
    })
    output$podes_cat_lod <- renderTmap({
        x2 <- input$var_lod
        tm_shape(eastkalimantan_podes)+
            tm_fill(col = x2, 
                    style = "cat",
                    #breaks = input$breaks,
                    palette = input$palette_lod,
                    id = x2,
                    legend.show = FALSE)
        
    })
    output$podes_con_map <- renderTmap({
        
        y <- input$var_con
        tm_shape(eastkalimantan_podes)+
            tm_fill(col = y, 
                    title = as.character(y),
                    style = input$style,
                    breaks = input$breaks,
                    n = input$bin,
                    palette = input$palette_con,
                    id = "VILLAGE",
                    legend.show = TRUE)
            #tm_borders(alpha = 0.5, lwd = 0.5)+
            #tm_layout(legend.height = 0.50, 
            #          legend.width = 0.44, 
            #          legend.outside = TRUE,
            #          legend.position = c("right", "bottom"),
            #          frame = FALSE) +
            #tm_compass(type="8star", size = 2) +
            #tm_scale_bar(width = 0.15)
    })
    output$boxplot <- plotly::renderPlotly({
        
        
    })
    output$podes <- DT::renderDataTable({
        data <- eastkalimantan_podes %>%
            select(PROVINCE, CITY, DISTRICT, VILLAGE, 
                   STATUS_HEAD_OFFICE, CONDITION_HEAD_OFFICE, LOCATION_HEAD_OFFICE, 
                   VILLAGE_ADJACENT_TO_SEA,
                   PRESENCE_MANGROVE_TREES, VILLAGE_LOCATION_TO_FOREST, FOREST_FUNCTION,
                   VILLAGE_MAIN_INCOME, TYPE_OF_COMMODITIES, STREET_LIGHTING,
                   ELECTRICTY_PROVIDER, FUEL_USED_OFTEN, TRASH_DUMP_USED_OFTEN,
                   TOILET_FACILITIES, TOILET_DISPOSAL_SITE, SOURCE_DRINKING_WATER,
                   PRESENCE_POWER_LINES, NUM_OF_SLUMS, SOURCE_WATER_POLLUTION,
                   SOURCE_LAND_POLLUTION, SOURCE_AIR_POLLUTION, PLANTING_ENVIRONMENTAL_CONSERVATION,
                   RECYCLE_ENVIRONMENTAL_CONSERVATION, SLASH_AND_BURN
                   )
        DT::datatable(data = data,
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
    output$news_map <- renderImage({
        width <- session$clientData$output_news_map_width
        height <- session$clientData$output_news_map_height
        
        list(
            src = "www/news.jpg",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "New Proposed Capital City of Indonesia"
        )
    })
    output$blogdown <- renderUI({
        div(
            h4("Please check out our R blogdown page for the full report", style = "display: inline;"),
            h4(tags$a(href = "https://www.google.com/", "here."), style = "display: inline;")
        )
    })
    output$logo <- renderImage({
        width <- (session$clientData$output_news_map_width)
        height <- (session$clientData$output_news_map_height)
        
        list(
            src = "www/logo_john.jpg",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "Logo"
        )
    })
    output$smu_logo <- renderImage({
        width <- 0.38*(session$clientData$output_news_map_width)
        height <- 0.32*(session$clientData$output_news_map_height)
        
        list(
            src = "www/smu_logo.png",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "SMU Logo"
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()
