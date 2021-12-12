############
## Página ##
############

CIOESTE1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosCIOESTE1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Indicadores das Cidades do Oeste da Região Metropolitana de São Paulo',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloCIOESTE1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaCIOESTE1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoCIOESTE1', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    dataTableOutput(outputId = 'tabelaCIOESTE1', width = '100%'),
    tags$div(id = 'separator3', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

CIOESTE1_Admin <- function(session) {
  ## variáveis e vetores ##

  indicador <- 'populacao_2017'
  indicadores <- c('População estimada (pessoas)' = 'populacao_2017',
                   'Densidade demográfica (hab/km²)' = 'densidade_demografica_2010',
                   'PIB per capita (reais)' = 'pib_per_capita_2015',
                   'IDHM' = 'idhm_2010',
                   'Matrículas no ensino fundamental' = 'matriculas_no_ensino_fundamental_2015',
                   'Matrículas no ensino médio' = 'matriculas_no_ensino_medio_2015',
                   'Área da unidade territorial (km²)' = 'area_da_unidade_territorial_2016'
                   )
  
  registrosPorPagina <- 10
  
  fontes <- 'IBGE, INEP, SUFRAMA e PNUD'
    
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Tabela' = 'tabela')
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf('<b>Indicador:</b> %s; <b>Fontes:</b> %s.', names(indicadores[indicadores == indicador]), fontes)
    )
  }

  camadaCidades <- function() {
    temp <- subset(dadosCidades, select = c('codigo', indicador))
    names(temp)[names(temp) == indicador] <- 'valor'
    sp::merge(cidades, temp, by.x = 'codigo', by.y = 'codigo')
  }

  dadosGrafico <- function() {
    temp <- subset(dadosCidades, select = c('nome', indicador))
    names(temp)[names(temp) == indicador] <- 'valor'
    temp$nome <- factor(temp$nome, levels = temp$nome[order(temp$valor)])
    return(temp)
  }
  
  colunasTabela <- function() {
    temp <- c('codigo', 'nome')
    for(item in indicadores) {
      temp <- c(temp, item) 
    }
    return(temp)
  }  

  titulosTabela <- function() {
    temp <- c('Código', 'Nome')
    temp <- c(temp, names(indicadores))
  }  

  dadosTabela <- function() {
    dadosCidades
  }

  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s', names(indicadores[indicadores == indicador]))
          camada <- camadaCidades()
          qpal <- colorBin('YlOrRd', domain = camada$valor, bins = unique(quantile(camada$valor, names = FALSE, prob = seq(0, 1, length = 7), type = 1)))
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Cidade: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$local,
                             format(x = camada$valor, big.mark = '.')
          ) %>%
            lapply(htmltools::HTML)          
          if (inicial) {
            session$output$cartogramaCIOESTE1 <- renderLeaflet({
              isolate({
                m <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 12))
                #m <- addTiles(map = m)
                m <- addProviderTiles(map = m, provider = providers$OpenStreetMap.BlackAndWhite)
                #m <- addProviderTiles(map = m, provider = providers$Stamen.Terrain)
                #m <- addProviderTiles(map = m, provider = providers$Stamen.TerrainBackground)
                #m <- addProviderTiles(map = m, provider = providers$Esri.WorldImagery)
                #m <- addProviderTiles(map = m, provider = providers$Esri.WorldGrayCanvas)
                #m <- addProviderTiles(map = m, provider = providers$CartoDB.Positron)
                m <- addEasyButton(map = m,
                                   button = easyButton
                                   (
                                     icon = 'fa-undo',
                                     title = 'Voltar às coordenadas iniciais',
                                     onClick = JS('function(btn, map){map.setView([-23.5600, -46.8500], 10);}')
                                   )
                )
                m <- setView(map = m, lng = -46.8500, lat = -23.5600, zoom = 10)
                m <- addPolygons(map = m,
                                 data = camada,
                                 fillColor = ~qpal(valor),
                                 weight = 2,
                                 opacity = 1,
                                 color = '#FFFFFF',
                                 #dashArray = '3',
                                 fillOpacity = 0.7,
                                 highlight = highlightOptions(
                                   weight = 5,
                                   color = '#666',
                                   dashArray = '',
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                                 ),
                                 label = rotulos,
                                 labelOptions = labelOptions(
                                   style = list('font-weight' = 'normal', padding = '3px 8px'),
                                   textsize = '15px',
                                   direction = 'auto'
                                 )
                )
                m <- addLegend(
                  map = m,
                  pal = qpal,
                  values = camada$valor,
                  opacity = 0.7,
                  title = titulo,
                  position = 'bottomright',
                  labFormat = labelFormat(big.mark = '.', between = ' a ')
                )  
                m
              })
            })
          } else {
            isolate({ 
              m <- leafletProxy(mapId = 'cartogramaCIOESTE1', session = session)
              m %>% clearShapes() 
              m %>% clearMarkers()
              m %>% clearControls()
              m <- addPolygons(map = m,
                               data = camada,
                               fillColor = ~qpal(valor),
                               weight = 2,
                               opacity = 1,
                               color = '#FFFFFF',
                               #dashArray = '3',
                               fillOpacity = 0.7,
                               highlight = highlightOptions(
                                 weight = 5,
                                 color = '#666',
                                 dashArray = '',
                                 fillOpacity = 0.7,
                                 bringToFront = TRUE
                               ),
                               label = rotulos,
                               labelOptions = labelOptions(
                                 style = list('font-weight' = 'normal', padding = '3px 8px'),
                                 textsize = '15px',
                                 direction = 'auto'
                               )
              )
              m <- addLegend(
                map = m,
                pal = qpal,
                values = camada$valor,
                opacity = 0.7,
                title = titulo,
                position = 'bottomright',
                labFormat = labelFormat(big.mark = '.', between = ' a ')
              )                      
            })
          }
          show(id = 'cartogramaCIOESTE1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaCIOESTE1')
          hide(id = 'separator1')
        }
        incProgress(1 / 3)
        ## Gráfico ##
        if ('grafico' %in% visoes) { 
          session$output$graficoCIOESTE1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$nome, y = dg$valor)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#ffaa00') +
                labs(title = sprintf('%s', names(indicadores[indicadores == indicador])), x = '', y = '') +
                geom_text(aes(label = format(x = dg$valor, big.mark = '.')), vjust = 1.6, color = '#000000', size = 4) +
                #geom_text(aes(label = dg$valor), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'graficoCIOESTE1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoCIOESTE1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 3)
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaCIOESTE1 <- DT::renderDataTable({
            isolate({
              DT::datatable(
                data = dadosTabela(),
                colnames = titulosTabela(),
                options = list(searching = TRUE,
                               pageLength = registrosPorPagina,
                               stateSave = TRUE,
                               language = list(url = 'Portuguese-Brasil.json')
                )        
              ) %>% formatStyle(columns = c(indicador), backgroundColor = '#e8e8e8')        
            })
          })  
          show(id = 'tabelaCIOESTE1')
          show(id = 'separator3')      
        } else {
          hide(id = 'tabelaCIOESTE1')
          hide(id = 'separator3')      
        }
        incProgress(1 / 3)
      }
    )
  }
  
  ## Eventos ##
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  observeEvent(input$buttonFiltrosCIOESTE1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectIndicadorCIOESTE1',
                             label = 'Indicador:',
                             choices = indicadores,
                             selected = ifelse(is.null(session$input$selectIndicadorCIOESTE1), indicador, session$input$selectIndicadorCIOESTE1)
                 )
          ),
          column(width = 6,
                 checkboxGroupInput(inputId = 'checkboxVisoesCIOESTE1',
                                    label = 'Visões:',
                                    choices = c('Cartograma' = 'cartograma',
                                                'Gráfico' = 'grafico',
                                                'Tabela' = 'tabela'
                                    ),
                                    selected = visoes
                 )
          )
        ),
        footer = tagList(
          actionButton(inputId = 'buttonOkCIOESTE1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelCIOESTE1', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkCIOESTE1, {
    ## Variáveis
    indicador <<- session$input$selectIndicadorCIOESTE1
    visoes <<- session$input$checkboxVisoesCIOESTE1
    ## Subtitulo ##
    output$textSubtituloCIOESTE1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Visões ## 
    atualizarVisoes(inicial = FALSE)
    ##
    removeModal()
  })
  
  observeEvent(input$buttonCancelCIOESTE1, {
    updateSelectInput(inputId = 'selectIndicadorCIOESTE1', session = session, selected = indicador)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesCIOESTE1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaCIOESTE1_state, {
    tryCatch({
      info <- input$tabelaCIOESTE1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloCIOESTE1 <- renderText({
    isolate({
      subTitulo()
    })
  })

  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'cidades')) {
        cidades <<- geojsonio::geojson_read(x = 'dados/geojsons/cidades.geojson', method = 'local', what = 'sp')        
      }            
      incProgress(1 / 3)      
      if (!exists(x = 'cidadesOk')) {
        for (coluna in colnames(cidades@data)) {
          if (is.factor(cidades@data[ , coluna])) {
            caractere <- as.character(cidades@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            cidades@data[ , coluna] <<- as.factor(caractere)
          }
        }
        cidadesOk <<- TRUE
      }      
      incProgress(1 / 3)            
      load(file = 'dados/CIOESTE1/dados-cidades.Rda')
      incProgress(1 / 3)
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
