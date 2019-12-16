    library(tigris)
    library(mapview)
    library(leaflet)
    library(sf)
    library(dplyr)
    library(shiny)
    library(shinyWidgets)
    
    nv <- st_read('./testdata.shp') %>% mutate(super_region = case_when(region %in% c('1','2') ~ 'A', !region %in% c('1','2') ~ 'B')) %>% mutate(turf = as.character(turf)) %>% select(super_region,region,turf,areas,sales) %>% st_sf()
    
    ui <- fluidPage(
      fluidRow(
        column(8,
               width = 10, offset = 1,
               tags$h3("Select Area"),
               panel(
                 selectizeGroupUI(
                   id = "filters",
                   params = list(
                     SR = list(inputId = "super_region", title = "Super Region:"),
                     Reg = list(inputId = "region", title = "Region:"),
                     Turf = list(inputId = "turf", title = "Turf"),
                     Areas = list(inputId = "areas", title = "Areas:")
                   ))
               ),
               leafletOutput("test")
        )
      )
    )
    
    server <- function(input, output, session) {
      
      res_mod <- callModule(
        module = selectizeGroupServer,
        id = "filters",
        data = nv,
        vars = c('super_region', 'region', 'turf', 'areas')
      )
      
      
    map_data <- reactive({
    
    if(is.null(input$super_region) & is.null(input$region) & is.null(input$turf) & is.null(input$areas)) {
    
      md <- res_mod() %>% st_sf() %>% st_buffer(0) %>% group_by(super_region) %>% summarize(geometry = st_union(geometry))
    
    }
      
      
    else if(!is.null(input$super_region)) {
        
        md <- res_mod() %>% filter(super_region %in% input$super_region) %>% st_sf() %>% st_buffer(0) %>% group_by(region) %>% summarize(geometry = st_union(geometry))
        
    }
      
    else if(!is.null(input$region)) {
        
        md <- res_mod() %>% filter(region %in% input$region) %>% st_sf() %>% st_buffer(0) %>% group_by(turf) %>% summarize(geometry = st_union(geometry))
        
    }
      
      else if(!is.null(input$turf)) {
        
        md <- res_mod() %>% filter(turf %in% input$turf) %>% st_sf() %>% st_buffer(0) %>% group_by(areas) %>% summarize(geometry = st_union(geometry))
        
      }  
    
    else {
    
      md <- res_mod() %>% filter(areas %in% input$areas) %>% st_sf() %>% st_buffer(0) %>% group_by(areas) %>% summarize(geometry = st_union(geometry))
    
    }
    
    })
    
    output$test <- renderLeaflet({
      
      res <- map_data()
      
      mapview(res)@map
      
    })
      
    }  
    
    shinyApp(ui,server)
