# R packages needed
packages = c('sf','spdep','rgdal','tmap','tidyverse', 'shinycssloaders', 'shinythemes', 'DT')
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Imported Data
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
podes_final <- read.csv("data/aspatial/podes_final.csv", sep = ",")
podes_final$PROVINCE_ID <- as.character(podes$PROVINCE_ID)
podes_final$CITY_ID <- as.character(podes$CITY_ID)
podes_final$DISTRICT_ID <- as.character(podes$DISTRICT_ID)
podes_final$VILLAGE_ID <- as.character(podes$VILLAGE_ID)
# print(head(podes_final))

eastkalimantan.podes <- dplyr::inner_join(area, podes, by = c("PROVINCE_ID", "CITY_ID", "DISTRICT_ID", "VILLAGE_ID", "PROVINCE", "CITY", "DISTRICT", "VILLAGE")) 
print(nrow(eastkalimantan.podes))


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"


# Define UI
ui <- fluidPage(
    titlePanel("Geographical Profiling & Segmentation of the new proposed capital city in East Kalimantan"),
    sidebarLayout(
        sidebarPanel(
            helpText("Create Choropleth Map based on Indonesia's village potential (podes) data collected in 2018."),
            selectInput(inputId = "var_cat", 
                        label = "Choose a categorical variable to display:",
                        choices = c("City" = "CITY",
                                    "District" = "DISTRICT",
                                    "Village" = "VILLAGE",
                                    "Topography" = "VILLAGE/SUBDISTRICT_BY_TOPOGRAPHY",
                                    "Location to Forest" = "VILLAGE_LOCATION_TO_FOREST",
                                    "Forest function" = "FOREST_FUNCTION", 
                                    "Main income" = "VILLAGE_MAIN_INCOME"),
                        selected = "City"),
            selectInput(inputId = "var_con", 
                        label = "Choose a continuous variable to display:",
                        choices = c("Slums" = "NUM_OF_SLUMS",
                                    "Early childhood centre" = "EARLYCHILDHOOD_PUBLIC_INSTITUTION_NUM",
                                    "Kindergarten" = "KINDERGARTEN_PUBLIC_INSTITUTION_NUM",
                                    "Primary school" = "PRIMARY_PUBLIC_INSTITUTION_NUM",
                                    "Middle school" = "MIDDLE_PUBLIC_INSTITUTION_NUM",
                                    "Polytechnic" = "POLYTECHNIC_PUBLIC_INSTITUTION_NUM", 
                                    "College" = "COLLEGE_PUBLIC_INSTITUTION_NUM"),
                        selected = "Slums"),
            selectInput(inputId = "style",
                        label = "Style for continuous variable:",
                        choices = c("Pretty" = "pretty",
                                    "Quantile" = "quantile", 
                                    #"Fixed" = "fixed",
                                    "Equal" = "equal"),
                        selected = "Pretty"),
           # selectInput(inputId = "breaks",
                       # label = "Breaks:",
                       # choices = c("NULL" = NULL,
                                  #  "1" = c(0,1),
                                  #  "2" = c(0,1,2), 
                                  #  "3" = c(0,1,2,3),
                                  #  "4" = c(0,1,2,3,4),
                                  #  "5" = c(0,1,2,3,4,5)),
                       # selected = "NULL"),
            selectInput(inputId = "palette",
                        label = "Palette:",
                        choices = c("Blues" = "Blues",
                                    "Greens" = "Greens", 
                                    "Reds" = "Reds",
                                    "Red and Blue" = "RdBu"),
                        selected = "Blues")),
        mainPanel(
            "Choropleth Maps",
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), tmapOutput("podes_cat_map"), tmapOutput("podes_con_map"))
            ),
            DT::dataTableOutput(outputId = "podes")
            )),
    actionButton("button", "Apply changes", class = "btn-success")
)



# Define server 
server <- function(input, output) {
    output$podes_cat_map <- renderTmap({
        input$button
        
        x <- input$var_cat
        tm_shape(eastkalimantan.podes)+
            tm_fill(col = x, 
                    #style = input$style,
                    #breaks = input$breaks,
                    palette = input$palette)+
            tm_borders(alpha = 0.5, lwd = 0.5)+
            tm_layout(legend.height = 0.50, 
                      legend.width = 0.44, 
                      legend.outside = TRUE,
                      legend.position = c("right", "bottom"),
                      frame = FALSE) +
            tm_compass(type="8star", size = 2) +
            tm_scale_bar(width = 0.15)
    })
    output$podes_con_map <- renderTmap({
        input$button
        
        y <- input$var_con
        tm_shape(eastkalimantan.podes)+
            tm_fill(col = y, 
                    style = input$style,
                    # breaks = input$breaks,
                    palette = input$palette)+
            tm_borders(alpha = 0.5, lwd = 0.5)+
            tm_layout(legend.height = 0.50, 
                      legend.width = 0.44, 
                      legend.outside = TRUE,
                      legend.position = c("right", "bottom"),
                      frame = FALSE) +
            tm_compass(type="8star", size = 2) +
            tm_scale_bar(width = 0.15)
    })
    output$podes <- DT::renderDataTable({
        DT::datatable(data = eastkalimantan.podes %>% select(1:10),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()
