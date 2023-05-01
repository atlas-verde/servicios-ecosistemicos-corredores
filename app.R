#
# Dashboard for "Servicios ecosistémicos - Corredores biológicos"
#


# PACKAGES
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(colorspace)


# FUNCTIONS

# Get labels for quartiles
get_quartiles_labels <- function(x, unit) {
  # Returns a vector with the labels of the quartiles ranges
  
  r1 <- paste0(as.character(round(x[1], 1)), " - ", as.character(round(x[2], 1)), " ", unit)
  r2 <- paste0(as.character(round(x[2], 1)), " - ", as.character(round(x[3], 1)), " ", unit)
  r3 <- paste0(as.character(round(x[3], 1)), " - ", as.character(round(x[4], 1)), " ", unit)
  r4 <- paste0(as.character(round(x[4], 1)), " - ", as.character(round(x[5], 1)), " ", unit)
  
  return(c(r1, r2, r3, r4))
}

# Create map
create_map <-
  function(indicator_column, indicator_group, indicator_legend_title, indicator_legend_labels, indicator_fillColor, indicator_palette, indicator_unit) {
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa de calles (OpenStreetMap)") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro (CartoDB Dark Matter)") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Mapa claro (Stamen Toner Lite)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes satelitales (ESRI World Imagery)") |>
      addPolygons(
        data = corridors,
        fillOpacity = ifelse(is.na(indicator_column), 0, 0.7),
        stroke = TRUE,
        color = "Black",
        fillColor = indicator_fillColor,
        weight = 1,
        popup = paste(
          paste("<strong>Corredor biológico:</strong>",  corridors[[COLUMN_CORRIDOR_NAME]]),
          paste(
            paste0("<strong>", indicator_group, ":</strong>"),
            paste0(round(indicator_column, 1), " ", indicator_unit)
          ),
          sep = '<br/>'
        ),
        label = paste(
          paste("Corredor biológico:",  corridors[[COLUMN_CORRIDOR_NAME]]),
          paste(paste0(indicator_group, ":"), round(indicator_column, 1), indicator_unit),
          sep = ' - '
        ),
        group = indicator_group
      ) |>
      addLegend(
        position = "bottomright",
        pal = indicator_palette,
        values = indicator_column,
        labFormat = function(type, cuts, p) {
          paste0(indicator_legend_labels)
        },
        group = indicator_group,
        title = indicator_legend_title
      ) |>
      addLayersControl(
        baseGroups = c(
          "Mapa de calles (OpenStreetMap)",
          "Mapa oscuro (CartoDB Dark Matter)",
          "Mapa claro (Stamen Toner Lite)",
          "Imágenes satelitales (ESRI World Imagery)"
        ),
        overlayGroups = c(indicator_group),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  }

# Create barplot
create_barplot <-
  function(indicator_column, indicator_geom_col_label, indicator_y_axis_label, indicator_geom_col_fill, indicator_unit) {
    # Ggplot2 plot
    barplot_recreation_ggplot2 <-
      corridors |>
      ggplot(aes(x = reorder(
        !!sym(COLUMN_CORRIDOR_NAME), -indicator_column
      ), y = indicator_column)) +
      geom_col(aes(
        text = paste0(
          "Corredor biológico: ",
          !!sym(COLUMN_CORRIDOR_NAME),
          "\n",
          indicator_geom_col_label,
          ": ",
          round(indicator_column, 1),
          " ",
          indicator_unit
        )
      ),
      fill = indicator_geom_col_fill) +
      xlab("Corredor biológico") +
      ylab(indicator_y_axis_label) +
      theme_classic() +
      theme(
        axis.text.x = element_text(
          angle = 50,
          vjust = 1,
          hjust = 1
        ),
        legend.position = "none"
      )
    
    # Plotly plot
    barplot_recreation_ggplot2 |>
      ggplotly(tooltip = "text") |>
      config(locale = 'es')    
  }


# CONSTANTS

# Data sources
DSN_CORRIDORS <- "data/metricas-corredores.geojson"

# Corridors dataframe
corridors <- st_read(dsn = DSN_CORRIDORS, quiet = TRUE)

# Data cleaning

# Data columns
COLUMN_CORRIDOR_NAME <- "nombre_cb"

COLUMN_TREECOVER_DENSITY <- corridors$DENS_ARB
COLUMN_WETLANDAREA_AREA <- corridors$SUP_HUMEDA

COLUMN_VEGETATION_PERCENTAGE <- corridors$PORC_VT
COLUMN_NATURALAREA_PERCENTAGE <- corridors$PORC_VN
COLUMN_SEMINATURALAREA_PERCENTAGE <- corridors$POR_VSN

COLUMN_CULTIVATEDLAND_AREA <- corridors$tierra_c_h
COLUMN_CULTIVATEDPASTURES_AREA <- corridors$pastos_ha
COLUMN_SHADECOFFEE_AREA <- corridors$cafe_ha
COLUMN_PERMANENTCROPS_AREA <- corridors$perenne_ha
COLUMN_ANNUALCROPS_AREA <- corridors$anuales_ha

# Indicators
INDICATOR_TREECOVER_DENSITY <- "Densidad de cobertura arbórea"
INDICATOR_WETLANDAREA_AREA <- "Superficie de humedales"

INDICATOR_VEGETATION_PERCENTAGE <- "Porcentaje de vegetación"
INDICATOR_NATURALAREA_PERCENTAGE <- "Porcentaje de área natural"
INDICATOR_SEMINATURALAREA_PERCENTAGE <- "Porcentaje de área seminatural"

INDICATOR_CULTIVATEDLAND_AREA <- "Superficie de tierra cultivada"
INDICATOR_CULTIVATEDPASTURES_AREA <- "Superficie de pastos cultivados"
INDICATOR_SHADECOFFEE_AREA <- "Superficie de café con sombra"
INDICATOR_PERMANENTCROPS_AREA <- "Superficie de cultivos perennes"
INDICATOR_ANNUALCROPS_AREA <- "Superficie de cultivos anuales"

# Units of measurement
UNIT_TREECOVER_DENSITY <- "ha cobertura arbórea/ha"
UNIT_WETLANDAREA_AREA <- "ha"

UNIT_VEGETATION_PERCENTAGE <- "%"
UNIT_NATURALAREA_PERCENTAGE <- "%"
UNIT_SEMINATURALAREA_PERCENTAGE <- "%"

UNIT_CULTIVATEDLAND_AREA <- "ha"
UNIT_CULTIVATEDPASTURES_AREA <- "ha"
UNIT_SHADECOFFEE_AREA <- "ha"
UNIT_PERMANENTCROPS_AREA <- "ha"
UNIT_ANNUALCROPS_AREA <- "ha"

# Classes for simbology in maps
QUANTILES_TREECOVER_DENSITY_VALUES <- fivenum(COLUMN_TREECOVER_DENSITY)
QUANTILES_TREECOVER_DENSITY_LABELS <- get_quartiles_labels(QUANTILES_TREECOVER_DENSITY_VALUES, UNIT_TREECOVER_DENSITY)

QUANTILES_WETLANDAREA_AREA_VALUES <- fivenum(COLUMN_WETLANDAREA_AREA)
QUANTILES_WETLANDAREA_AREA_LABELS <- get_quartiles_labels(QUANTILES_WETLANDAREA_AREA_VALUES, UNIT_WETLANDAREA_AREA)

QUANTILES_VEGETATION_PERCENTAGE_VALUES <- fivenum(COLUMN_VEGETATION_PERCENTAGE)
QUANTILES_VEGETATION_PERCENTAGE_LABELS <- get_quartiles_labels(QUANTILES_VEGETATION_PERCENTAGE_VALUES, UNIT_VEGETATION_PERCENTAGE)

QUANTILES_NATURALAREA_PERCENTAGE_VALUES <- fivenum(COLUMN_NATURALAREA_PERCENTAGE)
QUANTILES_NATURALAREA_PERCENTAGE_LABELS <- get_quartiles_labels(QUANTILES_NATURALAREA_PERCENTAGE_VALUES, UNIT_NATURALAREA_PERCENTAGE)

QUANTILES_SEMINATURALAREA_PERCENTAGE_VALUES <- fivenum(COLUMN_SEMINATURALAREA_PERCENTAGE)
QUANTILES_SEMINATURALAREA_PERCENTAGE_LABELS <- get_quartiles_labels(QUANTILES_SEMINATURALAREA_PERCENTAGE_VALUES, UNIT_SEMINATURALAREA_PERCENTAGE)

QUANTILES_CULTIVATEDLAND_AREA_VALUES <- fivenum(COLUMN_CULTIVATEDLAND_AREA)
QUANTILES_CULTIVATEDLAND_AREA_LABELS <- get_quartiles_labels(QUANTILES_CULTIVATEDLAND_AREA_VALUES, UNIT_CULTIVATEDLAND_AREA)

QUANTILES_CULTIVATEDPASTURES_AREA_VALUES <- fivenum(COLUMN_CULTIVATEDPASTURES_AREA)
QUANTILES_CULTIVATEDPASTURES_AREA_LABELS <- get_quartiles_labels(QUANTILES_CULTIVATEDPASTURES_AREA_VALUES, UNIT_CULTIVATEDPASTURES_AREA)

QUANTILES_SHADECOFFEE_AREA_VALUES <- fivenum(COLUMN_SHADECOFFEE_AREA)
QUANTILES_SHADECOFFEE_AREA_LABELS <- get_quartiles_labels(QUANTILES_SHADECOFFEE_AREA_VALUES, UNIT_SHADECOFFEE_AREA)

QUANTILES_PERMANENTCROPS_AREA_VALUES <- fivenum(COLUMN_PERMANENTCROPS_AREA)
QUANTILES_PERMANENTCROPS_AREA_LABELS <-  get_quartiles_labels(QUANTILES_PERMANENTCROPS_AREA_VALUES, UNIT_PERMANENTCROPS_AREA)
# QUANTILES_PERMANENTCROPS_AREA_VALUES <- c(0, 0.7, 5.25, 99.967)
# QUANTILES_PERMANENTCROPS_AREA_LABELS <-  c("0 - 0.7 ha", "0.7 - 5.25 ha", "5.25 - 99.97 ha")

QUANTILES_ANNUALCROPS_AREA_VALUES <- fivenum(COLUMN_ANNUALCROPS_AREA)
QUANTILES_ANNUALCROPS_AREA_LABELS <- get_quartiles_labels(QUANTILES_ANNUALCROPS_AREA_VALUES, UNIT_ANNUALCROPS_AREA)

# Color palettes
PALETTE_TREECOVER_DENSITY_COLOR <- rgb(60, 170, 0, maxColorValue = 255) # CORINE CR - Bosque secundario
PALETTE_TREECOVER_DENSITY_START_COLOR <- lighten(PALETTE_TREECOVER_DENSITY_COLOR, 0.4)
PALETTE_TREECOVER_DENSITY_END_COLOR <- darken(PALETTE_TREECOVER_DENSITY_COLOR, 0.4)
PALETTE_TREECOVER_DENSITY <- colorBin(
  bins = QUANTILES_TREECOVER_DENSITY_VALUES,
  palette = c(PALETTE_TREECOVER_DENSITY_START_COLOR, PALETTE_TREECOVER_DENSITY_END_COLOR),
  na.color = NA
)
PALETTE_WETLANDAREA_AREA_COLOR <- rgb(180, 230, 250, maxColorValue = 255) # CORINE CR - Embalses
PALETTE_WETLANDAREA_AREA_START_COLOR <- lighten(PALETTE_WETLANDAREA_AREA_COLOR, 0.4)
PALETTE_WETLANDAREA_AREA_END_COLOR <- darken(PALETTE_WETLANDAREA_AREA_COLOR, 0.4)
PALETTE_WETLANDAREA_AREA <- colorBin(
  bins = QUANTILES_WETLANDAREA_AREA_VALUES,
  palette = c(PALETTE_WETLANDAREA_AREA_START_COLOR, PALETTE_WETLANDAREA_AREA_END_COLOR),
  na.color = NA
)

PALETTE_VEGETATION_PERCENTAGE_COLOR <- rgb(167,167, 255, maxColorValue = 255) # CORINE CR - Bosque de galería
PALETTE_VEGETATION_PERCENTAGE_START_COLOR <- lighten(PALETTE_VEGETATION_PERCENTAGE_COLOR, 0.4)
PALETTE_VEGETATION_PERCENTAGE_END_COLOR <- darken(PALETTE_VEGETATION_PERCENTAGE_COLOR, 0.4)
PALETTE_VEGETATION_PERCENTAGE <- colorBin(
  bins = QUANTILES_VEGETATION_PERCENTAGE_VALUES,
  palette = c(PALETTE_VEGETATION_PERCENTAGE_START_COLOR, PALETTE_VEGETATION_PERCENTAGE_END_COLOR),
  na.color = NA
)
PALETTE_NATURALAREA_PERCENTAGE_COLOR <- rgb(255, 210, 125, maxColorValue = 255) # CORINE CR - Mosaico de pastos, cultivos y espacios naturales
PALETTE_NATURALAREA_PERCENTAGE_START_COLOR <- lighten(PALETTE_NATURALAREA_PERCENTAGE_COLOR, 0.4)
PALETTE_NATURALAREA_PERCENTAGE_END_COLOR <- darken(PALETTE_NATURALAREA_PERCENTAGE_COLOR, 0.4)
PALETTE_NATURALAREA_PERCENTAGE <- colorBin(
  bins = QUANTILES_NATURALAREA_PERCENTAGE_VALUES,
  palette = c(PALETTE_NATURALAREA_PERCENTAGE_START_COLOR, PALETTE_NATURALAREA_PERCENTAGE_END_COLOR),
  na.color = NA
)
PALETTE_SEMINATURALAREA_PERCENTAGE_COLOR <- rgb(255, 255, 166, maxColorValue = 255) # CORINE CR - Mosaico de pastos y espacios naturales
PALETTE_SEMINATURALAREA_PERCENTAGE_START_COLOR <- lighten(PALETTE_SEMINATURALAREA_PERCENTAGE_COLOR, 0.4)
PALETTE_SEMINATURALAREA_PERCENTAGE_END_COLOR <- darken(PALETTE_SEMINATURALAREA_PERCENTAGE_COLOR, 0.4)
PALETTE_SEMINATURALAREA_PERCENTAGE <- colorBin(
  bins = QUANTILES_SEMINATURALAREA_PERCENTAGE_VALUES,
  palette = c(PALETTE_SEMINATURALAREA_PERCENTAGE_START_COLOR, PALETTE_SEMINATURALAREA_PERCENTAGE_END_COLOR),
  na.color = NA
)

PALETTE_CULTIVATEDLAND_AREA_COLOR <- rgb(190, 205, 0, maxColorValue = 255) # CORINE CR - Mosaico de cultivos
PALETTE_CULTIVATEDLAND_AREA_START_COLOR <- lighten(PALETTE_CULTIVATEDLAND_AREA_COLOR, 0.4)
PALETTE_CULTIVATEDLAND_AREA_END_COLOR <- darken(PALETTE_CULTIVATEDLAND_AREA_COLOR, 0.4)
PALETTE_CULTIVATEDLAND_AREA <- colorBin(
  bins = QUANTILES_CULTIVATEDLAND_AREA_VALUES,
  palette = c(PALETTE_CULTIVATEDLAND_AREA_START_COLOR, PALETTE_CULTIVATEDLAND_AREA_END_COLOR),
  na.color = NA
)
PALETTE_CULTIVATEDPASTURES_AREA_COLOR <- rgb(255, 255, 166, maxColorValue = 255) # CORINE CR - Pastos limpios
PALETTE_CULTIVATEDPASTURES_AREA_START_COLOR <- lighten(PALETTE_CULTIVATEDPASTURES_AREA_COLOR, 0.4)
PALETTE_CULTIVATEDPASTURES_AREA_END_COLOR <- darken(PALETTE_CULTIVATEDPASTURES_AREA_COLOR, 0.4)
PALETTE_CULTIVATEDPASTURES_AREA <- colorBin(
  bins = QUANTILES_CULTIVATEDPASTURES_AREA_VALUES,
  palette = c(PALETTE_CULTIVATEDPASTURES_AREA_START_COLOR, PALETTE_CULTIVATEDPASTURES_AREA_END_COLOR),
  na.color = NA
)
PALETTE_SHADECOFFEE_AREA_COLOR <- rgb(115, 38, 0, maxColorValue = 255) # CORINE CR - Café
PALETTE_SHADECOFFEE_AREA_START_COLOR <- lighten(PALETTE_SHADECOFFEE_AREA_COLOR, 0.4)
PALETTE_SHADECOFFEE_AREA_END_COLOR <- darken(PALETTE_SHADECOFFEE_AREA_COLOR, 0.4)
PALETTE_SHADECOFFEE_AREA <- colorBin(
  bins = QUANTILES_SHADECOFFEE_AREA_VALUES,
  palette = c(PALETTE_SHADECOFFEE_AREA_START_COLOR, PALETTE_SHADECOFFEE_AREA_END_COLOR),
  na.color = NA
)
PALETTE_PERMANENTCROPS_AREA_COLOR <- rgb(255, 210, 125, maxColorValue = 255) # CORINE CR - Otros cultivos permanentes
PALETTE_PERMANENTCROPS_AREA_START_COLOR <- lighten(PALETTE_PERMANENTCROPS_AREA_COLOR, 0.4)
PALETTE_PERMANENTCROPS_AREA_END_COLOR <- darken(PALETTE_PERMANENTCROPS_AREA_COLOR, 0.4)
PALETTE_PERMANENTCROPS_AREA <- colorBin(
  bins = QUANTILES_PERMANENTCROPS_AREA_VALUES,
  palette = c(PALETTE_PERMANENTCROPS_AREA_START_COLOR, PALETTE_PERMANENTCROPS_AREA_END_COLOR),
  na.color = NA
)
PALETTE_ANNUALCROPS_AREA_COLOR <- rgb(168, 112, 0, maxColorValue = 255) # CORINE CR - Otros cultivos anuales
PALETTE_ANNUALCROPS_AREA_START_COLOR <- lighten(PALETTE_ANNUALCROPS_AREA_COLOR, 0.4)
PALETTE_ANNUALCROPS_AREA_END_COLOR <- darken(PALETTE_ANNUALCROPS_AREA_COLOR, 0.4)
PALETTE_ANNUALCROPS_AREA <- colorBin(
  bins = QUANTILES_ANNUALCROPS_AREA_VALUES,
  palette = c(
    PALETTE_ANNUALCROPS_AREA_START_COLOR,
    PALETTE_ANNUALCROPS_AREA_END_COLOR
  ),
  na.color = NA
)


# USER INTERFACE
ui <- fluidPage(
  
  theme = "bootstrap",
  tags$head(
    tags$style(
      HTML(
        '/* Radio buttons size */
        #radiobuttons_indicators_biodiversityhabitat label {
          font-size: 18px;
        }
        #radiobuttons_indicators_regulating label {
          font-size: 18px;
        }
        #radiobuttons_indicators_foodprovisioning label {
          font-size: 18px;
        }        
        .texto_agradecimiento_logos_1 {
          text-align: center;
        }        
        .texto_agradecimiento_logos_2 {
          text-align: center;
        }'
      )
    )
  ),
  
  navbarPage(
    title = tags$span(
      tags$a(href = "https://atlasverde.net/", target = "_blank", "Atlas de servicios ecosistémicos de la GAM"),
      " - ",
      "Servicios ecosistémicos - Corredores biológicos"
    ),
    theme = shinytheme("lumen"),
    
    # Categoría: Soporte
    navbarMenu("Soporte", 
      icon = icon("globe-americas"),
               
      # Servicio ecosistémico: Hábitat para la biodiversidad
      tabPanel("Hábitat para la biodiversidad", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Categoría de servicio ecosistémico"), br(), "Soporte")),
            fluidRow(h2(strong("Servicio ecosistémico"), br(), "Hábitat para la biodiversidad")),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_biodiversityhabitat",
                label = "",
                choices = c(
                  INDICATOR_TREECOVER_DENSITY,
                  INDICATOR_WETLANDAREA_AREA
                ),
                selected = INDICATOR_TREECOVER_DENSITY
              )
            )
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_biodiversityhabitat")))),
            fluidRow(withSpinner(leafletOutput("map_biodiversityhabitat"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_biodiversityhabitat")))
          )              
        )
      )
    ),
    
    # Categoría: Regulación
    navbarMenu("Regulación", 
      icon = icon("globe-americas"),    
      
      # Servicio ecosistémico: Regulación
      tabPanel("Regulación", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Categoría de servicio ecosistémico"), br(), "Regulación")),
            fluidRow(h2(strong("Servicio ecosistémico"), br(), "Regulación")),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_regulating",
                label = "",
                choices = c(
                  INDICATOR_VEGETATION_PERCENTAGE,
                  INDICATOR_NATURALAREA_PERCENTAGE,
                  INDICATOR_SEMINATURALAREA_PERCENTAGE
                ),
                selected = INDICATOR_VEGETATION_PERCENTAGE
              )
            )            
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_regulating")))),
            fluidRow(withSpinner(leafletOutput("map_regulating"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_regulating")))
          )          
        )
      )
    ),
    
    # Categoría: Provisión
    navbarMenu("Provisión", 
      icon = icon("globe-americas"),    
      
      # Servicio ecosistémico: Suministro de alimentos
      tabPanel("Suministro de alimentos", fluid = TRUE, 
        icon = icon("globe-americas"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(h1(strong("Categoría de servicio ecosistémico"), br(), "Provisión")),
            fluidRow(h2(strong("Servicio ecosistémico"), br(), "Suministro de alimentos")),
            fluidRow(h3(strong("Indicadores"))),
            fluidRow(
              radioButtons("radiobuttons_indicators_foodprovisioning",
                label = "",
                choices = c(
                  INDICATOR_CULTIVATEDLAND_AREA,
                  INDICATOR_CULTIVATEDPASTURES_AREA,
                  INDICATOR_SHADECOFFEE_AREA,
                  INDICATOR_PERMANENTCROPS_AREA,
                  INDICATOR_ANNUALCROPS_AREA
                ),
                selected = INDICATOR_CULTIVATEDLAND_AREA
              )
            )            
          ),
          mainPanel(
            fluidRow(h3(strong(textOutput("header_foodprovisioning")))),
            fluidRow(withSpinner(leafletOutput("map_foodprovisioning"))),
            fluidRow(h1(column(width = 12))),
            fluidRow(withSpinner(plotlyOutput("barplot_foodprovisioning")))
          )          
        )
      )
    )
  ),
  
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12))),    
  h3(class = "texto_agradecimiento_logos_1", strong("Acerca del Atlas de Servicios Ecosistémicos de la GAM")),
  h3(class = "texto_agradecimiento_logos-2", "El Atlas de Servicios Ecosistémicos de la GAM es producto de la cooperación entre los Gobiernos de Alemania y Costa Rica en el marco del proyecto Biodiver_City – Establecimiento de Corredores Biológicos Interurbanos con el fin de promover el desarrollo urbano centrado en los beneficios de la naturaleza. El instrumento fue desarrollado por el CATIE, por encargo de la Cooperación alemana para el desarrollo GIZ, bajo una estrecha articulación con el MINAE, CENIGA, SINAC y con el apoyo técnico del Instituto de Estudios Ambientales Helmholtz, UFZ."),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-gcr20222026.png", height = 90)),
    column(width = 4, img(src = "logo-minae.png", height = 90)),
    column(width = 4, img(src = "logo-sinac.jpg", height = 90)),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-catie.jpeg", height = 90)),
    column(width = 4, img(src = "logo-giz.png", height = 90)),
    column(
      width = 4,
      img(src = "logo-minambientealemania-iki.png", height = 90)
    ),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12)))  
)


# SERVER LOGIC
server <- function(input, output) {
  # Biodiversity habitat indicators header
  output$header_biodiversityhabitat <- renderText(if (input$radiobuttons_indicators_biodiversityhabitat == INDICATOR_TREECOVER_DENSITY) {
    INDICATOR_TREECOVER_DENSITY
  } else if (input$radiobuttons_indicators_biodiversityhabitat == INDICATOR_WETLANDAREA_AREA) {
    INDICATOR_WETLANDAREA_AREA
  })
  
  # Regulating indicators header
  output$header_regulating <- renderText(if (input$radiobuttons_indicators_regulating == INDICATOR_VEGETATION_PERCENTAGE) {
    INDICATOR_VEGETATION_PERCENTAGE
  } else if (input$radiobuttons_indicators_regulating == INDICATOR_NATURALAREA_PERCENTAGE) {
    INDICATOR_NATURALAREA_PERCENTAGE
  } else if (input$radiobuttons_indicators_regulating == INDICATOR_SEMINATURALAREA_PERCENTAGE) {
    INDICATOR_SEMINATURALAREA_PERCENTAGE
  })   
  
  # Food provisioning indicators header
  output$header_foodprovisioning <- renderText(if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_CULTIVATEDLAND_AREA) {
    INDICATOR_CULTIVATEDLAND_AREA
  } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_CULTIVATEDPASTURES_AREA) {
    INDICATOR_CULTIVATEDPASTURES_AREA
  } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_SHADECOFFEE_AREA) {
    INDICATOR_SHADECOFFEE_AREA
  } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_PERMANENTCROPS_AREA) {
    INDICATOR_PERMANENTCROPS_AREA
  } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_ANNUALCROPS_AREA) {
    INDICATOR_ANNUALCROPS_AREA
  })
  

  # Biodiversity habitat indicators map
  output$map_biodiversityhabitat <- renderLeaflet({
    if (input$radiobuttons_indicators_biodiversityhabitat == INDICATOR_TREECOVER_DENSITY) {
      create_map(
        COLUMN_TREECOVER_DENSITY,
        INDICATOR_TREECOVER_DENSITY,
        INDICATOR_TREECOVER_DENSITY,
        QUANTILES_TREECOVER_DENSITY_LABELS,
        ~ PALETTE_TREECOVER_DENSITY(COLUMN_TREECOVER_DENSITY),
        PALETTE_TREECOVER_DENSITY,
        UNIT_TREECOVER_DENSITY
      )
    } else if (input$radiobuttons_indicators_biodiversityhabitat == INDICATOR_WETLANDAREA_AREA) {
      create_map(
        COLUMN_WETLANDAREA_AREA,
        INDICATOR_WETLANDAREA_AREA,
        INDICATOR_WETLANDAREA_AREA,
        QUANTILES_WETLANDAREA_AREA_LABELS,
        ~ PALETTE_WETLANDAREA_AREA(COLUMN_WETLANDAREA_AREA),
        PALETTE_WETLANDAREA_AREA,
        UNIT_WETLANDAREA_AREA
      )      
    }
  })
  
# Regulating indicators map
  output$map_regulating <- renderLeaflet({
    if (input$radiobuttons_indicators_regulating == INDICATOR_VEGETATION_PERCENTAGE) {
      create_map(
        COLUMN_VEGETATION_PERCENTAGE,
        INDICATOR_VEGETATION_PERCENTAGE,
        INDICATOR_VEGETATION_PERCENTAGE,
        QUANTILES_VEGETATION_PERCENTAGE_LABELS,
        ~ PALETTE_VEGETATION_PERCENTAGE(COLUMN_VEGETATION_PERCENTAGE),
        PALETTE_VEGETATION_PERCENTAGE,
        UNIT_VEGETATION_PERCENTAGE
      )            
    } else if (input$radiobuttons_indicators_regulating == INDICATOR_NATURALAREA_PERCENTAGE) {
      create_map(
        COLUMN_NATURALAREA_PERCENTAGE,
        INDICATOR_NATURALAREA_PERCENTAGE,
        INDICATOR_NATURALAREA_PERCENTAGE,
        QUANTILES_NATURALAREA_PERCENTAGE_LABELS,
        ~ PALETTE_NATURALAREA_PERCENTAGE(COLUMN_NATURALAREA_PERCENTAGE),
        PALETTE_NATURALAREA_PERCENTAGE,
        UNIT_NATURALAREA_PERCENTAGE
      )
    } else if (input$radiobuttons_indicators_regulating == INDICATOR_SEMINATURALAREA_PERCENTAGE) {
      create_map(
        COLUMN_SEMINATURALAREA_PERCENTAGE,
        INDICATOR_SEMINATURALAREA_PERCENTAGE,
        INDICATOR_SEMINATURALAREA_PERCENTAGE,
        QUANTILES_SEMINATURALAREA_PERCENTAGE_LABELS,
        ~ PALETTE_SEMINATURALAREA_PERCENTAGE(COLUMN_SEMINATURALAREA_PERCENTAGE),
        PALETTE_SEMINATURALAREA_PERCENTAGE,
        UNIT_SEMINATURALAREA_PERCENTAGE
      )      
    }
  })
  
# Food provisioning indicators map
  output$map_foodprovisioning <- renderLeaflet({
    if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_CULTIVATEDLAND_AREA) {
      create_map(
        COLUMN_CULTIVATEDLAND_AREA,
        INDICATOR_CULTIVATEDLAND_AREA,
        INDICATOR_CULTIVATEDLAND_AREA,
        QUANTILES_CULTIVATEDLAND_AREA_LABELS,
        ~ PALETTE_CULTIVATEDLAND_AREA(COLUMN_CULTIVATEDLAND_AREA),
        PALETTE_CULTIVATEDLAND_AREA,
        UNIT_CULTIVATEDLAND_AREA
      )            
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_CULTIVATEDPASTURES_AREA) {
      create_map(
        COLUMN_CULTIVATEDPASTURES_AREA,
        INDICATOR_CULTIVATEDPASTURES_AREA,
        INDICATOR_CULTIVATEDPASTURES_AREA,
        QUANTILES_CULTIVATEDPASTURES_AREA_LABELS,
        ~ PALETTE_CULTIVATEDPASTURES_AREA(COLUMN_CULTIVATEDPASTURES_AREA),
        PALETTE_CULTIVATEDPASTURES_AREA,
        UNIT_CULTIVATEDPASTURES_AREA
      )            
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_SHADECOFFEE_AREA) {
      create_map(
        COLUMN_SHADECOFFEE_AREA,
        INDICATOR_SHADECOFFEE_AREA,
        INDICATOR_SHADECOFFEE_AREA,
        QUANTILES_SHADECOFFEE_AREA_LABELS,
        ~ PALETTE_SHADECOFFEE_AREA(COLUMN_SHADECOFFEE_AREA),
        PALETTE_SHADECOFFEE_AREA,
        UNIT_SHADECOFFEE_AREA
      )
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_PERMANENTCROPS_AREA) {
      create_map(
        COLUMN_PERMANENTCROPS_AREA,
        INDICATOR_PERMANENTCROPS_AREA,
        INDICATOR_PERMANENTCROPS_AREA,
        QUANTILES_PERMANENTCROPS_AREA_LABELS,
        ~ PALETTE_PERMANENTCROPS_AREA(COLUMN_PERMANENTCROPS_AREA),
        PALETTE_PERMANENTCROPS_AREA,
        UNIT_PERMANENTCROPS_AREA
      )
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_ANNUALCROPS_AREA) {
      create_map(
        COLUMN_ANNUALCROPS_AREA,
        INDICATOR_ANNUALCROPS_AREA,
        INDICATOR_ANNUALCROPS_AREA,
        QUANTILES_ANNUALCROPS_AREA_LABELS,
        ~ PALETTE_ANNUALCROPS_AREA(COLUMN_ANNUALCROPS_AREA),
        PALETTE_ANNUALCROPS_AREA,
        UNIT_ANNUALCROPS_AREA
      )      
    }
  })    

    
  # Biodiversity habitat indicators bar plot
  output$barplot_biodiversityhabitat <- renderPlotly({
    if (input$radiobuttons_indicators_biodiversityhabitat == INDICATOR_TREECOVER_DENSITY) {
      create_barplot(
        COLUMN_TREECOVER_DENSITY,
        INDICATOR_TREECOVER_DENSITY,
        INDICATOR_TREECOVER_DENSITY,
        PALETTE_TREECOVER_DENSITY_END_COLOR,
        UNIT_TREECOVER_DENSITY
      )      
    } else if (input$radiobuttons_indicators_biodiversityhabitat == INDICATOR_WETLANDAREA_AREA) {
      create_barplot(
        COLUMN_WETLANDAREA_AREA,
        INDICATOR_WETLANDAREA_AREA,
        INDICATOR_WETLANDAREA_AREA,
        PALETTE_WETLANDAREA_AREA_END_COLOR,
        UNIT_WETLANDAREA_AREA
      )
    }
  })
  
  # Regulating indicators bar plot
  output$barplot_regulating <- renderPlotly({
    if (input$radiobuttons_indicators_regulating == INDICATOR_VEGETATION_PERCENTAGE) {
      create_barplot(
        COLUMN_VEGETATION_PERCENTAGE,
        INDICATOR_VEGETATION_PERCENTAGE,
        INDICATOR_VEGETATION_PERCENTAGE,
        PALETTE_VEGETATION_PERCENTAGE_END_COLOR,
        UNIT_VEGETATION_PERCENTAGE
      )      
    } else if (input$radiobuttons_indicators_regulating == INDICATOR_NATURALAREA_PERCENTAGE) {
      create_barplot(
        COLUMN_NATURALAREA_PERCENTAGE,
        INDICATOR_NATURALAREA_PERCENTAGE,
        INDICATOR_NATURALAREA_PERCENTAGE,
        PALETTE_NATURALAREA_PERCENTAGE_END_COLOR,
        UNIT_NATURALAREA_PERCENTAGE
      )      
    } else if (input$radiobuttons_indicators_regulating == INDICATOR_SEMINATURALAREA_PERCENTAGE) {
      create_barplot(
        COLUMN_SEMINATURALAREA_PERCENTAGE,
        INDICATOR_SEMINATURALAREA_PERCENTAGE,
        INDICATOR_SEMINATURALAREA_PERCENTAGE,
        PALETTE_SEMINATURALAREA_PERCENTAGE_END_COLOR,
        UNIT_SEMINATURALAREA_PERCENTAGE
      )
    }
  })  
  
  # Food provisioning indicators bar plot
  output$barplot_foodprovisioning <- renderPlotly({
    if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_CULTIVATEDLAND_AREA) {
      create_barplot(
        COLUMN_CULTIVATEDLAND_AREA,
        INDICATOR_CULTIVATEDLAND_AREA,
        INDICATOR_CULTIVATEDLAND_AREA,
        PALETTE_CULTIVATEDLAND_AREA_END_COLOR,
        UNIT_CULTIVATEDLAND_AREA
        )
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_CULTIVATEDPASTURES_AREA) {
      create_barplot(
        COLUMN_CULTIVATEDPASTURES_AREA,
        INDICATOR_CULTIVATEDPASTURES_AREA,
        INDICATOR_CULTIVATEDPASTURES_AREA,
        PALETTE_CULTIVATEDPASTURES_AREA_END_COLOR,
        UNIT_CULTIVATEDPASTURES_AREA
      )
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_SHADECOFFEE_AREA) {
      create_barplot(
        COLUMN_SHADECOFFEE_AREA,
        INDICATOR_SHADECOFFEE_AREA,
        INDICATOR_SHADECOFFEE_AREA,
        PALETTE_SHADECOFFEE_AREA_END_COLOR,
        UNIT_SHADECOFFEE_AREA
      )      
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_PERMANENTCROPS_AREA) {
      create_barplot(
        COLUMN_PERMANENTCROPS_AREA,
        INDICATOR_PERMANENTCROPS_AREA,
        INDICATOR_PERMANENTCROPS_AREA,
        PALETTE_PERMANENTCROPS_AREA_END_COLOR,
        UNIT_PERMANENTCROPS_AREA
      )
    } else if (input$radiobuttons_indicators_foodprovisioning == INDICATOR_ANNUALCROPS_AREA) {
      create_barplot(
        COLUMN_ANNUALCROPS_AREA,
        INDICATOR_ANNUALCROPS_AREA,
        INDICATOR_ANNUALCROPS_AREA,
        PALETTE_ANNUALCROPS_AREA_END_COLOR,
        UNIT_ANNUALCROPS_AREA
      )      
    } 
  })  

}


# RUN APPLICATION
shinyApp(ui = ui, server = server)