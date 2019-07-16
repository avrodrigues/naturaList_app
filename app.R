#### GBIF shiny app
source("functions.R")

header <- dashboardHeader(title = "naturaList", titleWidth = 230)

sidebar <- dashboardSidebar(
  sidebarMenu(
    br(),
    fluidRow(
      column(12,offset = 3,
             img(src='naturaList_logo_size2.png',height= 97.5, width=  114.075, align = "center"))
    ),
    br(),
    menuItem("Introduction", tabName ="Tutorial", icon = icon("home")),
    menuItem("Upload and Classification", tabName ="Upload",icon = icon("upload")),
    menuItem("Grid Filter",tabName = "Criteria", icon = icon("filter")),
    menuItem("Check and Download", tabName ="Validation",icon = icon("download"))
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Tutorial", 
            box(width = NULL,
                includeMarkdown("naturaList_intro.Rmd"),
                fluidRow(
                  column(width = 7, offset=2,
                         imageOutput('tbl')
                         )
                ),
                includeMarkdown("naturaList_intro_part2.Rmd"),
                br(),
                fluidRow(
                  column(width=7, offset=2,
                         imageOutput('fig_grid')
                         )
                ),
                includeMarkdown('naturaList_intro_part3.Rmd'),
                fluidRow(
                  column(width = 7, offset = 2,
                         imageOutput('map_module')
                         )
                ),
                
                includeMarkdown('naturaList_intro_part4.Rmd'),
                
              
                
                fluidRow(
                  column(width = 6, offset = 3,
                         iframe(width = "560", height = "315",
                                url_link = "https://www.youtube.com/embed/QeVGtklLMRE"))
                ))
            
    ),
    tabItem(tabName = "Upload", 
            fluidRow(
              box(width = 4, height = NULL, 
                  title = "Species Ocurrences",
                  status = "success", solidHeader = T, 
                  fileInput("file.occ", "Species Ocurrences"),
                  actionButton("ex_spp", "Use occurence table example"),
                  radioButtons("file.type", "File type:", 
                               choices = c("csv","txt (not implemented)")),
                  radioButtons("file.sep", "Separator:", 
                               choices = c("tab",
                                           "comma (not implemented)",
                                           "semi-comma (not implemented)")),
                  #actionButton("clear.file.occ", "Clear file"),br(),br(),
                  column(width = 12,
                         valueBoxOutput("nrow", width = NULL),
                         valueBoxOutput("nrow.coords", width = NULL)
                  )
                  
                  
              ),
              box(width = 8, height = NULL,
                  title = "Classification of occurences",
                  status = "success", solidHeader = T, 
                  box(width = NULL,
                      fluidRow(
                        column(8,
                               fileInput("file.specialist", "Provide specialist names")),
                        column(4,
                               br(),
                               actionButton("ex_spec", "Use specialist table example"))
                      ),
                      fluidRow(
                        column(6,
                               radioButtons("file.spec.type", "File type:", 
                                            choices = c("csv","txt (not implemented)"))
                        ),
                        column(6,
                               radioButtons("file.spec.sep", "Separator:", 
                                            choices = c("comma",
                                                        "tab (not implemented)",
                                                        "semi-comma (not implemented)"))
                        )
                      )
                  ),
                  
                  
                  fluidRow(
                    column(6,
                           box(width = NULL,
                               h4("Classify occurrence points"),
                               "Occurrence points are classified in six levels,
                               in decresing order.",
                               br(), br(),
                               strong("Level 1"), "- species name was determined by a specialist taxonomist;", br(),
                               strong("Level 2"), "- name of the species determiner is provided;", br(),
                               strong("Level 3"), "- occurrence have an image", br(),
                               strong("Level 4"), "- specimen is preserved;", br(),
                               strong("Level 5"), "- occurence was regitered by human observation;", br(),
                               strong("Level 6"), "- no criteria was met.", br(), br(),
                               actionButton("run.criteria", "Go!")),
                           box(width = NULL,
                               uiOutput("spec.doubts"),
                               h4(textOutput("is.spec")),
                               strong(textOutput("check.specialist")),
                               uiOutput("spec.check"),
                               uiOutput("spec.yes"),
                               uiOutput("spec.no")
                               
                           )
                           
                    ),
                    
                    box(width = 6,
                        valueBoxOutput("Crit1", width = 6),
                        valueBoxOutput("Crit2", width = 6),
                        valueBoxOutput("Crit3", width = 6),
                        valueBoxOutput("Crit4", width = 6),
                        valueBoxOutput("Crit5", width = 6),
                        valueBoxOutput("Crit6", width = 6)
                    )
                  ) 
                  
              )
              
            )
    ),
    tabItem(tabName = "Criteria", 
            
            fluidRow(
              box(width = 8, height = NULL,
                  title = "Grid Filter", 
                  status = "success", solidHeader = T,
                  fluidRow(
                    column(6,
                           textInput("raster.resolution", "Grid cells size in decimal degrees:", 
                                     value = "1"),
                           checkboxGroupButtons("shape", "Select the spatial extent:",
                                                choices = c("Americas",
                                                            "Africa",
                                                            "Asia",
                                                            "Europe",
                                                            "Oceania",
                                                            "Antarctic"), status = "primary",
                                                direction = "vertical", size = "xs",
                                                justified = T),
                           plotOutput("cont.map", height = 170)),
                    column(6,
                           h4(strong("Select one occurrence per grid cell")),
                           "Before making the selection, make sure that occurrence classification has been done and that a grid cell size has been provided.",
                           br(),
                           "You also have the option of providing a spatial extent in which occurrences will be selected.",
                           br(),
                           br(),
                           actionButton("run.selection", "Go!"), 
                           br(),
                           br(),
                           br(),
                           valueBoxOutput("occ.selected", width = NULL))
                  )
              )
            )
    ),
    
    tabItem(tabName = "Validation", 
            fluidRow(
              column(3,
                     box(width = NULL,
                         checkboxGroupButtons(
                           inputId = "grbox", label = "What levels should be downloaded
                           from grid filter data?", 
                           choices = c("Level 1" = "Level_1",
                                       "Level 2" = "Level_2",
                                       "Level 3" = "Level_3",
                                       "Level 4" = "Level_4",
                                       "Level 5" = "Level_5",
                                       "Level 6" = "Level_6"),
                           justified = T, status = 'info', size = "xs", direction = "vertical",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                           selected = c("Level_1",
                                        "Level_2", 
                                        "Level_3", 
                                        "Level_4", 
                                        "Level_5",
                                        "Level_6"),
                           width = "100%"
                         ),
                         materialSwitch("del_mkr_button", 
                                        label = "Delete points with click", 
                                        status = "danger")
                         
                         
              ),
              box(width = NULL, title = "Download",
                  solidHeader = T, status = "success",
                  
                  
                  textOutput("sel_display"),
                  downloadButton("download_grid_filter.csv","Download from grid filter"),br(),br(),
                  textOutput("down.class.text"),
                  downloadButton("download_classified.csv","Download from classifier")
              )
              ),
              column(9,
                     leafletOutput("map", height = 500)
              )
            )
            
    )
  )
  )

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session){
  
  output$tbl <- renderImage({
    img <- normalizePath("www/table_fig.png", winslash = '/')
    list(src = img,
         contentType = 'image/png',
         width = 670,
         height = 244,
         alt = "Specialist table")
  }, deleteFile = F)
  
  output$fig_grid<- renderImage({
    img <- normalizePath("www/grid_filter_img.png", winslash = '/')
    list(src = img,
         contentType = 'image/png',
         width = 792,
         height = 300,
         alt = "Filtering proccess")
  }, deleteFile = F)
  
  output$map_module<- renderImage({
    map_mod <- normalizePath("www/map_module.png", winslash = '/')
    list(src = map_mod,
         contentType = 'image/png',
         width = 805,
         height = 350,
         alt = "map module")
  }, deleteFile = F)
  
  
  val <- reactiveValues()
  values <- reactiveValues()
  val$df <- data.frame()
  val$df.red <- data.frame()
  
  # Upload species file
  observeEvent(!is.null(input$file.occ),{
    req(input$file.occ)
    
    val$df <- read.csv(input$file.occ$datapath, 
                       sep = "\t", encoding = "UTF-8", stringsAsFactors = F)
    df <- val$df
    
    reduce.df(df)
    val$df.red <- reduce.df(df)
    
  })
  
  observeEvent(input$ex_spp,{
    
    
    val$df <- read.csv("www/Alsophila_setosa.csv", 
                       sep = "\t", encoding = "UTF-8", stringsAsFactors = F)
    df <- val$df
    
    reduce.df(df)
    val$df.red <- reduce.df(df)
    
  })
  
  # Num occurences
  output$nrow <- renderValueBox({
    row <- ifelse(is.null(val$df), 0, nrow(val$df))
    valueBox(row, "Total ocurrences", icon = icon("list"),
             color = "purple")
  })
  
  # Num occurrences with coordinates
  output$nrow.coords <- renderValueBox({
    row.red <- ifelse(is.null(val$df.red), 0, nrow(val$df.red))
    valueBox(row.red, "With coordinates", icon = icon("list"),
             color = "green")
  })
  
  # upload specialist data frame
  observeEvent(input$file.specialist, {
    val$specialist <- read.csv(input$file.specialist$datapath, 
                               sep = ",", stringsAsFactors = F)
    
  })
  
  # upload specialist example
  observeEvent(input$ex_spec, {
    val$specialist <- read.csv("www/fern_specialists.csv", 
                               sep = ",", stringsAsFactors = F)
    
   
  })
  
  # Clear specialist file 
  observeEvent(input$clear.file.spec, {
    if(input$clear.file.spec){
      
      val$specialist <- NULL
      reset('file.specialist')
    }
    
  })
  
  ## Clasification of occurences
  observeEvent(input$run.criteria,{
    
    df.r <- val$df.red  
    if(is.null(input$file.specialist)){
      val$df.crit <- include.criteria(df.r)
    }
    
    if(!is.null(input$file.specialist)|
       input$ex_spec > 0){
      specialist <- val$specialist
      val$df.crit <- include.criteria(df.r, specialist)
    }
    val$table.crt <- table(val$df.crit$vetor.criterio)
  })
  
  # Doubt about specialist name
  observeEvent({
    input$run.criteria|
      input$specialist.yes|
      input$specialist.no
  },{
    
    if(!is.null(input$file.specialist)|
       input$ex_spec > 0){
      output$check.specialist <- renderText({
        
        val$verify <- which(val$df.crit$vetor.criterio %in% "Level_1_verify")
        if(length(val$verify) > 0){
          txt <- as.character(val$df.crit$determined.by[val$verify[1]]) 
        }else{txt <- ""}
        
        txt
      })
    }
  })
  
  
  
  ### Check specialist name
  observeEvent({input$run.criteria|val$verify},{
    if(!is.null(input$file.specialist)|
       input$ex_spec > 0){
      
      output$is.spec <- renderText({
        if(input$run.criteria > 0){
          if(length(val$verify) > 0){
            "Is this a specialist?"
          }else{""}
        }
      })
    }
    
    
  })
  
  output$issue <- renderPrint(table(val$df.crit$vetor.criterio))
  
  # Confirm specialist
  observeEvent({input$specialist.yes},{
    val$df.crit$vetor.criterio[val$verify[1]] <- "Level_1"
    val$table.crt <- table(val$df.crit$vetor.criterio)
  })
  # Confirm specialist
  observeEvent({input$specialist.no},{
    
    val$df.crit$vetor.criterio[val$verify[1]] <- "Level_2" 
    
    val$table.crt <- table(val$df.crit$vetor.criterio)
  })
  
  # How many doubts about specialist names
  observeEvent({
    input$run.criteria |
      input$specialist.yes|
      input$specialist.no},
    {
      if(input$run.criteria > 0){
        if(!is.null(input$file.specialist)|
           input$ex_spec > 0){
          output$spec.doubts <- renderUI({
            
            if(length(val$verify) > 0){
              h4(paste("There are", length(val$verify), 
                       "doubts about specialists' names. Check the names below."))
            }else{
              h4("There is no doubt about specialists' names")
            }
            
          })
        }
        
      }
    })
  
  observeEvent({input$run.criteria|val$verify},{
    if(!is.null(input$file.specialist)|
       input$ex_spec > 0){
      if(length(val$verify) > 0){
        output$spec.yes <- renderUI({
          actionButton("specialist.yes", "Yes")
        })
      }else{output$spec.yes <- renderUI({})}
      
    }
  })
  
  observeEvent({input$run.criteria|val$verify},{
    if(!is.null(input$file.specialist)|
       input$ex_spec > 0){
      if(length(val$verify) > 0){
        output$spec.no <- renderUI({
          actionButton("specialist.no", "No")
        })
      }else{output$spec.no <- renderUI({})}
    }
    
    
  })
  
  # Value box with num of occurrence in each classification level
  observeEvent({
    input$run.criteria |
      input$specialist.yes|
      input$specialist.no},
    {
      output$Crit1 <- renderValueBox({
        
        v.Crit1 <- ifelse("Level_1" %in% names(val$table.crt), val$table.crt["Level_1"], 0 )
        
        valueBox(v.Crit1, "Level 1", color = "olive", icon = NULL)
      })
      
      output$Crit2 <- renderValueBox({
        v.Crit2 <- ifelse("Level_2" %in% names(val$table.crt), val$table.crt["Level_2"], 0 )
        valueBox(v.Crit2, "Level 2" , color = "olive", icon = NULL)
      })
      
      output$Crit3 <- renderValueBox({
        v.Crit3 <- ifelse("Level_3" %in% names(val$table.crt), val$table.crt["Level_3"], 0 )
        valueBox(v.Crit3, "Level 3" , color = "olive", icon = NULL)
      })
      
      output$Crit4 <- renderValueBox({
        v.none <- ifelse("Level_4" %in% names(val$table.crt), val$table.crt["Level_4"], 0 )
        valueBox(v.none, "Level 4" , color = "olive", icon = NULL)
      })
      
      output$Crit5 <- renderValueBox({
        v.none <- ifelse("Level_5" %in% names(val$table.crt), val$table.crt["Level_5"], 0 )
        valueBox(v.none, "Level 5" , color = "olive", icon = NULL)
      })
      
      output$Crit6 <- renderValueBox({
        v.none <- ifelse("Level_6" %in% names(val$table.crt), val$table.crt["Level_6"], 0 )
        valueBox(v.none, "Level 6" , color = "olive", icon = NULL)
      })
    })
  
  
  
  observeEvent(is.null(input$shape), {
    output$cont.map <- renderPlot({
      par(mar = c(0,0,0,0))
      plot(cont)
      
    })
    
  })
  
  # choose a continent to cut the points
  observeEvent(input$shape, {
    
    output$cont.map <- renderPlot({
      cont.selected <- continent.selection(input$shape)
      
      val$continents <- cont[cont.selected[[1]],]
      
      par(mar = c(0,0,0,0))
      
      plot(cont)
      plot(val$continents, col = cont.selected[[2]], add = T)
    })
    
    
    
  })
  
  observeEvent(input$run.selection,{
    resol <- rep(as.numeric(input$raster.resolution), 2)
    df.class <- val$df.crit 
    
    if(is.null(input$shape)){
      val$df.grid.sel <- grid.selector(df.class, resol)
    }
    
    if(!is.null(input$shape)){
      cont.selected <- continent.selection(input$shape)
      df.shp <- shape.remove(df.class, cont.selected[[1]])
      val$df.grid.sel <- grid.selector(df.shp, resol, session.app = session)
    }
    
    ## Dados para a espécies visualizada
    df.grid.sel <- val$df.grid.sel
    values$mt.occur <- rm.coord.dup(df.grid.sel)
    
    values$pt.ctr1 <- values$mt.occur[values$mt.occur$vetor.criterio == "Level_1",]
    values$pt.ctr2 <- values$mt.occur[values$mt.occur$vetor.criterio == "Level_2",]
    values$pt.ctr3 <- values$mt.occur[values$mt.occur$vetor.criterio == "Level_3",]
    values$pt.ctr4 <- values$mt.occur[values$mt.occur$vetor.criterio == "Level_4",]
    values$pt.ctr5 <- values$mt.occur[values$mt.occur$vetor.criterio == "Level_5",]
    values$pt.ctr6 <- values$mt.occur[values$mt.occur$vetor.criterio == "Level_6",]
  })
  
  output$occ.selected <- renderValueBox({
    row.sel <- ifelse(is.null(values$mt.occur), 0, nrow(values$mt.occur))
    valueBox(row.sel, "Occurence points selected", icon = icon("list"),
             color = "maroon")
  })
  
  
  values = reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric())
  
  ## Pontos a serem deletados
  values$MK <- data.frame(id = character(),
                          x = numeric(),
                          y = numeric())
  
  ## Linha a ser inserida em values$MK
  values$add_row <- data.frame(id = character(),
                               x = numeric(),
                               y = numeric())
  
  values$pol <- list()
  
  ## linhas das ocorrencias (grep) selecionadas de acordo com os critérios
  values$g.cri <- numeric()
  
  
  #observeEvent(input$file.occ, {
  #  proxy <- leafletProxy("map")
  #  proxy %>% removeShape("pol")
  #  
  #  values$DT = data.frame(x = numeric(),
  #                         y = numeric())
  #  values$pol <- list()
  #  
  #})
  
  observeEvent(input$run.selection,{
    # output$map.valid <- renderLeaflet({
    #   to.map <- val$df.grid.sel
    #   
    #   leaflet("map") %>%
    #     addTiles() %>%
    #     addCircleMarkers(lng = to.map$longitude, 
    #                      lat = to.map$latitude, 
    #                      color = "red",
    #                      weight = 7)
    # })
    
    ## Pontos a serem deletados
    values$MK <- data.frame(id = character(),
                            x = numeric(),
                            y = numeric())
    
    ## Linha a ser inserida em values$MK
    values$add_row <- data.frame(id = character(),
                                 x = numeric(),
                                 y = numeric())
    
    values$pol <- list()
    
    values$list.pol.df <- list()
    
    ## linhas das ocorrencias (grep) selecionadas de acordo com os critérios
    values$g.cri <- numeric()
    
    
    output$map <- renderLeaflet({
      leaflet("map") %>% 
        # Add two tiles
        addTiles(options = providerTileOptions(noWrap = TRUE),group="StreetMap")%>%
        addProviderTiles("Esri.WorldImagery", group="Satelite")  %>%
        
        
        # Add marker groups
        addCircleMarkers(data= values$pt.ctr1, 
                         lng= ~longitude , lat= ~latitude, radius=8 , layerId = row.names(values$pt.ctr1),
                         popup = paste(strong("ID:"), values$pt.ctr1$occ.id,"<br>",
                                       strong("Institution Source:"), values$pt.ctr1$institutionsource, "<br>",
                                       strong("Determined by:"), values$pt.ctr1$determined.by, "<br>",
                                       strong("Year of data colection:"), values$pt.ctr1$year.event, "<br>",
                                       strong("Date Identified:"), values$pt.ctr1$dateIdentified, "<br>"),
                         fillColor="red", stroke = F, fillOpacity = 0.8, group="Level 1") %>%
        
        addCircleMarkers(data=values$pt.ctr2, 
                         lng=~longitude , lat=~latitude, radius=8 , layerId = row.names(values$pt.ctr2),
                         popup = paste(strong("ID:"), values$pt.ctr2$occ.id,"<br>",
                                       strong("Institution:"), values$pt.ctr2$institutionsource, "<br>",
                                       strong("Determined by:"), values$pt.ctr2$determined.by, "<br>",
                                       strong("Year of data colection:"), values$pt.ctr2$year.event, "<br>",
                                       strong("Date of determination:"), values$pt.ctr2$dateIdentified, "<br>"),
                         fillColor="orange", stroke = F, fillOpacity = 0.8, group="Level 2") %>%
        
        addCircleMarkers(data=values$pt.ctr3, 
                         lng=~longitude , lat=~latitude, radius=8 , layerId = row.names(values$pt.ctr3),
                         popup = paste(strong("ID:"), values$pt.ctr3$occ.id,"<br>",
                                       strong("Institution:"), values$pt.ctr3$institutionsource, "<br>",
                                       strong("Determined by:"), values$pt.ctr3$determined.by, "<br>",
                                       strong("Year of data colection:"), values$pt.ctr3$year.event, "<br>",
                                       strong("Date of determination:"), values$pt.ctr3$dateIdentified, "<br>"),
                         fillColor="yellow", stroke = F, fillOpacity = 0.8, group="Level 3") %>%
        
        addCircleMarkers(data=values$pt.ctr4, 
                         lng=~longitude , lat=~latitude, radius=8 , layerId = row.names(values$pt.ctr4),
                         popup = paste(strong("ID:"), values$pt.ctr4$occ.id,"<br>",
                                       strong("Institution:"), values$pt.ctr4$institutionsource, "<br>",
                                       strong("Determined by:"), values$pt.ctr4$determined.by, "<br>",
                                       strong("Year of data colection:"), values$pt.ctr4$year.event, "<br>",
                                       strong("Date of determination:"), values$pt.ctr4$dateIdentified, "<br>"),
                         fillColor="darkgreen", stroke = F, fillOpacity = 0.8, group="Level 4") %>%
        
        addCircleMarkers(data=values$pt.ctr5, 
                         lng=~longitude , lat=~latitude, radius=8 , layerId = row.names(values$pt.ctr5),
                         popup = paste(strong("ID:"), values$pt.ctr5$occ.id,"<br>",
                                       strong("Institution:"), values$pt.ctr5$institutionsource, "<br>",
                                       strong("Determined by:"), values$pt.ctr5$determined.by, "<br>",
                                       strong("Year of data colection:"), values$pt.ctr5$year.event, "<br>",
                                       strong("Date of determination:"), values$pt.ctr5$dateIdentified, "<br>"),
                         fillColor="blue", stroke = F, fillOpacity = 0.8, group="Level 5") %>%
        
        addCircleMarkers(data=values$pt.ctr6, 
                         lng=~longitude , lat=~latitude, radius=8 , layerId = row.names(values$pt.ctr5),
                         popup = paste(strong("ID:"), values$pt.ctr6$occ.id,"<br>",
                                       strong("Institution:"), values$pt.ctr6$institutionsource, "<br>",
                                       strong("Determined by:"), values$pt.ctr6$determined.by, "<br>",
                                       strong("Year of data colection:"), values$pt.ctr6$year.event, "<br>",
                                       strong("Date of determination:"), values$pt.ctr6$dateIdentified, "<br>"),
                         fillColor="purple", stroke = F, fillOpacity = 0.8, group="Level 6") %>%
        
        # Add the control widget
        addLayersControl(overlayGroups = c("Level 1",
                                           "Level 2",
                                           "Level 3", 
                                           "Level 4",
                                           "Level 5",
                                           "Level 6") , 
                         baseGroups = c("StreetMap","Satelite"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        
        ## Add tool to design poligons shapes to selection
        addDrawToolbar(
          targetGroup='Markers',
          polylineOptions = F,
          polygonOptions = T, 
          circleOptions = F,
          rectangleOptions = F,
          markerOptions = F,
          circleMarkerOptions = F,
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
        addLegend("bottomright", colors = c("red", "orange", "yellow", "darkgreen", "blue", "purple"),
                  labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6"),
                  opacity = 0.8)
    })
  })
  
  ## Limpa lista de pontos selecionados 
  observeEvent(input$del_mkr_button,{
    if(input$del_mkr_button == FALSE){
      values$add_row <- data.frame(id = character(),
                                   x = numeric(),
                                   y = numeric())
    }
  })
  
  
  ## Deleta pontos de ocorrencia que forem clicados
  observeEvent(input$map_marker_click, {
    proxy <- leafletProxy("map")
    
    
    
    if(input$del_mkr_button == TRUE){
      values$add_row <- data.frame(id = input$map_marker_click$id,
                                   x = input$map_marker_click$lng,
                                   y = input$map_marker_click$lat)
      values$MK <-  rbind(values$MK, values$add_row)
      
      
      
      dp <- duplicated(values$MK$id)
      
      if(sum(dp) >= 1){
        
        del <- values$MK[,1] %in% values$MK[dp,1] 
        
      }
      
      id <- values$add_row$id
      proxy %>% 
        removeMarker(id)
    }
    
  })
  
  ## Cria uma data.frame com os as coordenadas geográficas para criar um poligo
  observeEvent(input$map_draw_all_features, {
    
    values$list.pol.df <- list()
    if(length(input$map_draw_all_features$features) == 0){return()}
    if(length(input$map_draw_all_features$features) >0){
      for(i in 1:length(input$map_draw_all_features$features)){
        values$list.pol.df[[i]] <- pol.coords(input$map_draw_all_features$features[[i]])
      }
    } 
    
  })
  
  observeEvent(is.null(input$grbox), {
    if(is.null(input$grbox)){
      g.cri <- NULL
      values$g.cri <- g.cri
      
    }
  })
  
  ## Pontos de ocorrencia selecionados de acordo com a classificação (criterios) 
  observeEvent({input$grbox
    input$run.selection}, {
      # dados de ocorrencia da espécie
      occ.df <- values$mt.occur
      
      pttn <- paste(input$grbox, collapse="|")
      values$g.cri <- grep(pttn, occ.df$vetor.criterio)
      
    })
  
  
  ## Cria o data.frame a ser salvo 
  # seleciona pelos critérios e pelos poligonos criados
  output$sel_display <-  renderText({
    
    if (is.null(values$g.cri)|length(values$g.cri) == 0){
      values$sel.points <- data.frame()
    }else{
      
      occ.df <- values$mt.occur
      n.id <- as.character(values$MK$id)
      del <- row.names(occ.df)%in% n.id
      c.d <- values$g.cri %in% which(!del)
      values$pol <- list()
      
      if(length(values$list.pol.df) == 1){
        values$pol <- make.polygon(values$list.pol.df[[1]])
      }
      
      
      if(length(values$list.pol.df) > 1){
        
        values$pol <- bind(lapply(values$list.pol.df, make.polygon))
      }
      
      df.crit <- values$mt.occur[values$g.cri[c.d],]
      
      spt.df <- SpatialPointsDataFrame(df.crit[,c("longitude", "latitude")], df.crit)
      
      if(length(values$pol) == 0){
        n.col <- ncol(spt.df)
        values$sel.points <- as.data.frame(spt.df)[,1:n.col]
      }
      
      if(length(values$pol) >= 1){
        n.col <- ncol(spt.df)
        values$sel.points <- as.data.frame(spt.df[values$pol,])[,1:n.col]
      }
    }
    
    paste("Selected", nrow(values$sel.points), "of", nrow(values$mt.occur), "occurrence points." )
    
  })
  
  output$down.class.text <- renderText({
    paste0("All ", nrow(val$df.crit), " occurrence points with coordinates will be downloaded.")
  })
  
  
  observeEvent({input$run.criteria
    values$sel.points},{
      
      uploaded.df <- val$df
      ID <- values$sel.points$rowID
      uploaded.df.sel <- uploaded.df[ID,]
      uploaded.df.sel$naturaList_levels <- values$sel.points$vetor.criterio
      
      output$download_grid_filter.csv <- downloadHandler(
        filename = function(file) {
          paste("naturaList_grid_selection", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.table(uploaded.df.sel, file, sep = "\t", row.names = F)
        })
    })
  
  
  observeEvent(input$run.criteria,{
    
    id <- val$df.crit$rowID
    df <- val$df
    
    data <- df[id,]
    data$naturaList_levels <- val$df.crit$vetor.criterio[id]
    
    output$download_classified.csv <- downloadHandler(
      filename = function() {
        paste('naturaList-classified', 'csv', sep='.')
      },
      content = function(con) {
        write.table(data, con, sep = "\t", row.names = F)
      })
    
  })
  
  
}


shinyApp(ui, server)
