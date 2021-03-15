library(sf)
library(tidyverse)
library(tm)
library(wordcloud)
library(memoise)
library(wordcloud2)
library(shinyWidgets)
library(RColorBrewer)
library(gsheet)
library(lubridate)
library(RSocrata)
library(ggplot2)
library(leaflet)
library(ggthemes)
library(leafgl)
library(shinydashboard)
library(shiny)
library(leaflet.extras)
library(gsheet)
library(mapdata)
############### Bases
years_ago <- today() - years(1)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
    arrange(desc(crash_date)) %>%
    transmute(
        injuries = if_else(injuries_total > 0, "injuries", "none"),
        crash_date,
        crash_hour,
        report_type = if_else(report_type == "", "UNKNOWN", report_type),
        num_units,
        posted_speed_limit,
        weather_condition,
        lighting_condition,
        roadway_surface_cond,
        first_crash_type,
        trafficway_type,
        prim_contributory_cause,
        latitude, longitude
    ) %>%
    na.omit()
crash<-crash %>% mutate(date_label = format(crash_date, "%B-%Y"))
choices_month <- format(seq.Date(from = as.Date(min(crash$crash_date)),
                                 to = as.Date(max(crash$crash_date)),
                                 by = "month"), "%B-%Y")


# Interface do Usuário
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Transito de Chicago"),
                    dashboardSidebar(# Barra de menu lateral    ,
                        sliderTextInput(inputId = "sliderDate",
                                        label = "Data do evento:",
                                        width = "100%",
                                        from_min = choices_month[1],
                                        to_max = choices_month[length(choices_month)],
                                        choices = choices_month,
                                        grid = T,
                                        animate = animationOptions(interval = 1500)),
                        
                        radioButtons("dist","Opcao de visualizacao:",c("Sem feridos" = "none","Com feridos" = "injuries")),
                        menuItem("Condicoes de luminosidade da estrada",
                                 tabName = "filterLightning",
                                 icon = icon("lightbulb"),
                                 radioButtons("lightningSelection",
                                              "Luminosidade:",
                                              c("Escuro com via iluminada" = "DARKNESS, LIGHTED ROAD",
                                                "Sombrio" = "DUSK",
                                                "Escuro" = "DARKNESS",
                                                "Amanhecendo" = "DAWN",
                                                "Luz do dia" = "DAYLIGHT",
                                                "Desconhecida" = "UNKNOWN",
                                                "Todas" = "all"))),
                        radioButtons("mapa","Tipo de mapa:",c("Pontos"="ponto","Marcacao" = "normal","Heatmap" = "calor","Circulos"="circulo","Cluster"="cluster")),
                        menuItem("Marcacao",tabName = "marcacao",icon = icon("map-marker")),
                        menuItem("Heatmap",tabName = "mapadecalor",icon = icon("temperature-high"),
                            sliderInput("raiocalor","Valor do raio",1,20,value = 10),
                            sliderInput("int","Intensidade",0.1,1,value = 1),
                            sliderInput("blur","Desfoque",1,30,value = 10),
                            selectInput("Grad","Cor",choices  = c("plasma","viridis","RdYlBu", "Accent","Greens"))),
                        menuItem("Circulos",tabName = "mapacirculo",icon = icon("circle")),
                        menuItem("Pontos",tabName = "mapapontos",icon = icon("dot-circle"),
                                 sliderInput("raio","Valor do raio",1,20,value = 10),
                                 sliderInput("fillOpacityP","Preenchimento",0.1,1,value = 1),
                                 selectInput("CorP","Cor",choices  = c("blue","red","green","black","orange"))),
                        menuItem("Cluster",tabName = "mapacluster",icon = icon("users"),
                                 sliderInput("raiocluster","Raio do ponto",1,20,value = 10),
                                 selectInput("Corcluster","Cor do ponto",choices  = c("blue","red","green","black","orange")),
                                 sliderInput("fillOpacitycluster","Preenchimento do ponto",0.1,1,value = 1))
                        )
                    ,
                    dashboardBody(
                        fluidPage(
                          leafletOutput("transito",width="100%",height="500px"),
                          wordcloud2Output("plot",width="100%") ,
                          plotOutput("plot2")
                         )
                    )
)
# Server
server <- function(input, output, session) {
    
    output$transito <- renderLeaflet({
        
        tipo_mapa <- switch(input$mapa,
                            calor="calor",
                            normal="normal",
                            circulo="circulo",
                            ponto="ponto",
                            cluster="cluster")
        
        if(tipo_mapa=="normal"){
            leaflet() %>%
            addTiles() %>%
            addMarkers(lng = data()$longitude, lat = data()$latitude)
            
        } else if(tipo_mapa == "calor") {
            leaflet(data()) %>%
                addTiles() %>%
                addHeatmap(lng = ~longitude, lat = ~latitude, radius = input$raiocalor, intensity = input$int, blur = input$blur, gradient = input$Grad)
            
        } else if(tipo_mapa == "circulo"){
            leaflet(data()) %>%
                addTiles() %>% 
                addCircleMarkers(lng= ~longitude, lat = ~latitude, radius = ~(posted_speed_limit/5))
            
        }else if(tipo_mapa == "ponto"){
            leaflet(data()) %>%
                addTiles() %>% 
                addCircleMarkers(lng= ~longitude, lat = ~latitude,stroke = FALSE, color = input$CorP, fillOpacity = input$fillOpacityP, radius = input$raio)
            
        }else if( tipo_mapa == "cluster"){
            leaflet(data()) %>%
                addTiles() %>%
                addCircleMarkers(lng = ~longitude, lat = ~latitude,stroke = FALSE, radius = input$raiocluster, color = input$Corcluster, fillOpacity = input$fillOpacitycluster, clusterOptions = markerClusterOptions())
            }
        })
    
    data <- reactive({
        years_ago <- today() - years(1)
        crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
        crash_raw <- as_tibble(read.socrata(crash_url))
        
        crash <- crash_raw %>%
            arrange(desc(crash_date)) %>%
            transmute(
                injuries = if_else(injuries_total > 0, "injuries", "none"),
                crash_date,
                crash_hour,
                report_type = if_else(report_type == "", "UNKNOWN", report_type),
                num_units,
                posted_speed_limit,
                weather_condition,
                lighting_condition,
                roadway_surface_cond,
                first_crash_type,
                trafficway_type,
                prim_contributory_cause,
                latitude, longitude
            ) %>%
            na.omit()
        crash<-crash %>% mutate(date_label = format(crash_date, "%B-%Y"))
        choices_month <- format(seq.Date(from = as.Date(min(crash$crash_date)),
                                         to = as.Date(max(crash$crash_date)),
                                         by = "month"), "%B-%Y")
        filtro <- switch(input$dist,
                         none="none",
                         injuries="injuries")
        
        filtered_data <- crash %>% 
                    mutate(date_label = format(crash_date, "%B-%Y"))%>%
                    filter(date_label == input$sliderDate) %>% 
                    filter(injuries==filtro, longitude!=0.00)
    
        if(input$lightningSelection != "all"){
            filtered_data <- filtered_data %>% filter(lighting_condition == input$lightningSelection)
        }
        
        filtered_data
    })
    
    
    # Make the wordcloud drawing predictable during a session
    
    output$plot <- renderWordcloud2({
        text <- data()$prim_contributory_cause  
        docs <- Corpus(VectorSource(text))
        
        dtm <- TermDocumentMatrix(docs) 
        matrix <- as.matrix(dtm) 
        words <- sort(rowSums(matrix),decreasing=TRUE) 
        df <- data.frame(word = names(words),freq=words)
        
        wordcloud2(data=df, size=2, color='random-dark',shape="car")
        
    })
    output$plot2<-renderPlot({
ggplot(data(),aes(x=report_type,fill=report_type))+geom_bar()+coord_flip()+labs(y="",x="Tipo de ocorrencia")+theme_pander()+ theme(legend.position = "none",text = element_text(size=2),axis.text.y.left = element_text(size=10))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
