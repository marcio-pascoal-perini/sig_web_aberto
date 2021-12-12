############
## Página ##
############

PNAD1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosPNAD1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Pesquisa Nacional por Amostra de Domicílios Contínua',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloPNAD1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaPNAD1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoPNAD1', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaPNAD1', width = '100%', height = '500px'),
    tags$div(id = 'separator3', style = 'padding: 5px;'),
    dataTableOutput(outputId = 'tabelaPNAD1', width = '100%'),
    tags$div(id = 'separator4', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

PNAD1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))  
  
  ## variáveis e vetores ##

  abrangencia <- 'estados'
  abrangencias <- c('Estados' = 'estados', 'Capitais' = 'capitais')
  
  trimestre <- 202001
  trimestres <- c('1º trimestre 2020' = 202001,
                  '4º trimestre 2019' = 201904,
                  '3º trimestre 2019' = 201903,
                  '2º trimestre 2019' = 201902,
                  '1º trimestre 2019' = 201901,
                  '4º trimestre 2018' = 201804,
                  '3º trimestre 2018' = 201803,
                  '2º trimestre 2018' = 201802,
                  '1º trimestre 2018' = 201801,                  
                  '4º trimestre 2017' = 201704,
                  '3º trimestre 2017' = 201703,
                  '2º trimestre 2017' = 201702,
                  '1º trimestre 2017' = 201701,
                  '4º trimestre 2016' = 201604,
                  '3º trimestre 2016' = 201603,
                  '2º trimestre 2016' = 201602,
                  '1º trimestre 2016' = 201601,
                  '4º trimestre 2015' = 201504,
                  '3º trimestre 2015' = 201503,
                  '2º trimestre 2015' = 201502,
                  '1º trimestre 2015' = 201501,                  
                  '4º trimestre 2014' = 201404,
                  '3º trimestre 2014' = 201403,
                  '2º trimestre 2014' = 201402,
                  '1º trimestre 2014' = 201401
  )
  
  indicador <- 4099
  indicadores <-   c('Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4096,
                     'Nível de ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4097,
                     'Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4099,
                     'Nível de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4098
                     
  )
  
  sexo <- 6794
  sexos <- c('Homens' = 4,
             'Mulheres' = 5,
             'Todos' = 6794
  )

  registrosPorPagina <- 10

  fonte <- 'IBGE - Tabela 4093'
  
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Histograma' = 'histograma', 'Tabela' = 'tabela')
  
  ## funções ##

  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Abrangência:</b> %s; <b>Trimestre:</b> %s; <b>Indicador:</b> %s; <b>Sexo:</b> %s; <b>Fonte:</b> %s.',
        names(abrangencias[abrangencias == abrangencia]),
        names(trimestres[trimestres == trimestre]),
        names(indicadores[indicadores == indicador]),
        names(sexos[sexos == sexo]),
        fonte
      )
    )
  }

  camadaEstados <- function() {
    temp <- subset(dadosEstados, D2C == indicador & D3C == trimestre & D4C == sexo, select = c('D1C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(estados, temp, by.x = 'codigo', by.y = 'D1C')
  }
  
  camadaCapitais <- function() {
    temp <- subset(dadosCapitais, D2C == indicador & D3C == trimestre & D4C == sexo, select = c('D1C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(capitais, temp, by.x = 'codigo', by.y = 'D1C')
  }

  dadosGrafico <- function() {
    temp <- switch(abrangencia,
                   'estados' = subset(dadosEstados, D2C == indicador & D3C == trimestre & D4C == sexo, select = c('D1N', 'V')),
                   'capitais' = subset(dadosCapitais, D2C == indicador & D3C == trimestre & D4C == sexo, select = c('D1N', 'V'))
    )
    maiores <- temp[order(temp$V, decreasing = TRUE),]
    maiores <- head(x = maiores, n = 4)
    menores <- temp[order(temp$V, decreasing = FALSE),]
    menores <- head(x = menores, n = 4)
    temp <- rbind(maiores, menores)
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$V)])
    return(temp)
  }

  dadosHistograma <- function() {
    temp <- subset(dadosBrasil, D2C == indicador & D4C == sexo, select = c('D3C', 'D3N', 'V'))
    temp$D3N <- factor(temp$D3N, levels = temp$D3N[order(temp$D3C)])
    return(temp)
  }
  
  colunasTabela <- function() {
    c('D1C', 'D1N', 'D2N', 'D3N', 'D4N', 'V')
  }  
  
  titulosTabela <- function() {
    c('Código', 'Nome', 'Indicador', 'Trimestre', 'Sexo', '%')
  }  

  dadosTabela <- function() {
    switch(abrangencia,
           'estados' = subset(dadosEstados, D2C == indicador & D3C == trimestre & D4C == sexo, select = colunasTabela()),
           'capitais' = subset(dadosCapitais, D2C == indicador & D3C == trimestre & D4C == sexo, select = colunasTabela())
    )
  }
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('Sexo: %s', names(sexos[sexos == sexo]))
          if (inicial) {
            session$output$cartogramaPNAD1 <- renderLeaflet({
              isolate({
                m <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 10))
                m <- addTiles(map = m)
                m <- setView(map = m, lng = -49.3810, lat = -15.3251, zoom = 3)
                m <- addEasyButton(map = m,
                                   button = easyButton
                                   (
                                     icon = 'fa-undo',
                                     title = 'Voltar às coordenadas iniciais',
                                     onClick = JS('function(btn, map){map.setView([-15.3251, -49.3810], 3);}')
                                   )
                )
                if (abrangencia == 'estados') {
                  camada <- camadaEstados()
                  caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
                  qpal <- colorBin('Greens', domain = camada$valor, bins = caixas)
                  rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s&#37;</strong>',
                                     camada$codigo,
                                     camada$local,
                                     format(x = camada$valor, big.mark = '.')
                  ) %>%
                    lapply(htmltools::HTML)
                  m <- addPolygons(map = m,
                                   data = camada,
                                   fillColor = ~qpal(valor),
                                   weight = 2,
                                   opacity = 1,
                                   color = '#FFFFFF',
                                   fillOpacity = 0.7,
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
                    labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%')
                  )        
                } else {
                  camada <- camadaCapitais()
                  caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
                  qpal <- colorBin('Greens', domain = camada$valor, bins = caixas)
                  rotulos <- sprintf('<strong>Codigo: %s</strong><br/><strong>Capital: %s</strong><br/><strong>%s&#37;</strong>',
                                     camada$codigo,
                                     camada$local,
                                     format(x = camada$valor, big.mark = '.')
                  ) %>%
                    lapply(htmltools::HTML)
                  m <- addCircleMarkers(map = m,
                                        data = camada,
                                        fillColor = ~qpal(valor),
                                        weight = 2,
                                        opacity = 1,
                                        color = '#FFFFFF',
                                        fillOpacity = 0.7,
                                        radius = 10,
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
                    labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%')
                  )        
                } 
                m
              })
            })
          } else {
            isolate({ 
              m <- leafletProxy(mapId = 'cartogramaPNAD1', session = session)
              m %>% clearShapes() 
              m %>% clearMarkers()
              m %>% clearControls()
              if (abrangencia == 'estados') {
                camada <- camadaEstados()
                caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
                qpal <- colorBin('Greens', domain = camada$valor, bins = caixas)
                rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s&#37;</strong>',
                                   camada$codigo,
                                   camada$local,
                                   format(x = camada$valor, big.mark = '.')
                ) %>%
                  lapply(htmltools::HTML)
                m <- addPolygons(map = m,
                                 data = camada,
                                 fillColor = ~qpal(valor),
                                 weight = 2,
                                 opacity = 1,
                                 color = '#FFFFFF',
                                 fillOpacity = 0.7,
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
                  labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%')
                )        
              } else {
                camada <- camadaCapitais()
                caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
                qpal <- colorBin('Greens', domain = camada$valor, bins = caixas)
                rotulos <- sprintf('<strong>Codigo: %s</strong><br/><strong>Capital: %s</strong><br/><strong>%s&#37;</strong>',
                                   camada$codigo,
                                   camada$local,
                                   format(x = camada$valor, big.mark = '.')
                ) %>%
                  lapply(htmltools::HTML)
                m %>% addCircleMarkers(data = camada,
                                       fillColor = ~qpal(valor),
                                       weight = 2,
                                       opacity = 1,
                                       color = '#FFFFFF',
                                       fillOpacity = 0.7,
                                       radius = 10,
                                       label = rotulos,
                                       labelOptions = labelOptions(
                                         style = list('font-weight' = 'normal', padding = '3px 8px'),
                                         textsize = '15px',
                                         direction = 'auto'
                                       )
                )
                m %>% addLegend(
                  pal = qpal,
                  values = camada$valor,
                  opacity = 0.7,
                  title = titulo,
                  position = 'bottomright',
                  labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%')
                )        
              } 
            })
          }
          show(id = 'cartogramaPNAD1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaPNAD1')
          hide(id = 'separator1')
        }
        incProgress(1 / 4)
        ## Gráfico ##
        if ('grafico' %in% visoes) { 
          session$output$graficoPNAD1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$D1N, y = dg$V)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#62ba81') +
                labs(title = sprintf('Sexo: %s', names(sexos[sexos == sexo])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'graficoPNAD1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoPNAD1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 4)
        ## Histograma ##
        if ('histograma' %in% visoes & stri_cmp_equiv(abrangencia, 'estados', strength = 2)) {
          session$output$histogramaPNAD1 <- renderPlot({
            isolate({
              dh <- dadosHistograma() 
              ggplot(data = dh, aes(x = dh$D3N, y = dh$V)) +
                geom_line(colour = '#62ba81', size = 2, linetype = 1, group = 1) +
                geom_point(colour = ifelse(dh$D3C == trimestre, '#ff0000', '#808080'), size = 4) +
                labs(title = sprintf('Sexo: %s', names(sexos[sexos == sexo])), x = '', y = '') +
                #labs(title = sprintf('Abrangência: Brasil - Sexo: %s', names(sexos[sexos == sexo])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dh$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )                
            })              
          })          
          show(id = 'histogramaPNAD1')
          show(id = 'separator3')      
        } else {
          hide(id = 'histogramaPNAD1')
          hide(id = 'separator3')      
        }
        incProgress(1 / 4)        
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaPNAD1 <- DT::renderDataTable({
            isolate({
              DT::datatable(
                data = dadosTabela(),
                colnames = titulosTabela(),
                options = list(searching = TRUE,
                               pageLength = registrosPorPagina,
                               stateSave = TRUE,
                               language = list(url = 'Portuguese-Brasil.json')
                )        
              )        
            })
          })  
          show(id = 'tabelaPNAD1')
          show(id = 'separator4')      
        } else {
          hide(id = 'tabelaPNAD1')
          hide(id = 'separator4')      
        }
        incProgress(1 / 4)
      }
    )
  }
  
  ## Eventos ##
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  observeEvent(input$buttonFiltrosPNAD1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectAbrangenciaPNAD1',
                             label = 'Abrangência:',
                             choices = abrangencias,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectAbrangenciaPNAD1), abrangencia, session$input$selectAbrangenciaPNAD1)
                 )
          ),        
          column(width = 6,
                 selectInput(inputId = 'selectTrimestrePNAD1',
                             label = 'Trimestre:',
                             choices = trimestres,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectTrimestrePNAD1), trimestre, session$input$selectTrimestrePNAD1)
                 )
          )),
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectIndicadorPNAD1',
                             label = 'Indicador:',
                             choices = indicadores,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectIndicadorPNAD1), indicador, session$input$selectIndicadorPNAD1)
                 )
          )),
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectSexoPNAD1',
                             label = 'Sexo:',
                             choices = sexos,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectSexoPNAD1), sexo, session$input$selectSexoPNAD1)
                 )
          ),
          column(width = 6,
                 checkboxGroupInput(inputId = 'checkboxVisoesPNAD1',
                                    label = 'Visões:',
                                    choices = c('Cartograma' = 'cartograma',
                                                'Gráfico' = 'grafico',
                                                'Histograma' = 'histograma',
                                                'Tabela' = 'tabela'
                                    ),
                                    selected = visoes
                 )
          )
        ),
        footer = tagList(
          actionButton(inputId = 'buttonOkPNAD1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelPNAD1', label = 'Cancelar')
        )      
      )
    )    
  })

  observeEvent(input$buttonOkPNAD1, {
    ## Fechar modal
    removeModal()
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    abrangencia <<- session$input$selectAbrangenciaPNAD1
    trimestre <<- session$input$selectTrimestrePNAD1
    indicador <<- session$input$selectIndicadorPNAD1
    sexo <<- session$input$selectSexoPNAD1
    visoes <<- session$input$checkboxVisoesPNAD1
    ## Subtitulo ##
    output$textSubtituloPNAD1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelPNAD1, {
    updateSelectInput(inputId = 'selectAbrangenciaPNAD1', session = session, selected = abrangencia)
    updateSelectInput(inputId = 'selectTrimestrePNAD1', session = session, selected = trimestre)
    updateSelectInput(inputId = 'selectIndicadorPNAD1', session = session, selected = indicador)
    updateSelectInput(inputId = 'selectSexoPNAD1', session = session, selected = sexo)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesPNAD1', session = session, selected = visoes)
    removeModal()
  })

  observeEvent(input$tabelaPNAD1_state, {
    tryCatch({
      info <- input$tabelaPNAD1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##

  ## Subtitulo ##
  session$output$textSubtituloPNAD1 <- renderText({
    isolate({
      subTitulo()
    })
  })

  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'capitais')) {
        capitais <<- geojsonio::geojson_read(x = 'dados/geojsons/capitais.geojson', what = 'sp')        
      }      
      incProgress(1 / 7) 
      if (!exists(x = 'capitaisOk')) {
        for (coluna in colnames(capitais@data)) {
          if (is.factor(capitais@data[ , coluna])) {
            caractere <- as.character(capitais@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            capitais@data[ , coluna] <<- as.factor(caractere)
          }
        }
        capitaisOk <<- TRUE
      }  
      incProgress(1 / 7) 
      if (!exists(x = 'estados')) {
        estados <<- geojsonio::geojson_read(x = 'dados/geojsons/estados.geojson', what = 'sp')        
      }      
      incProgress(1 / 7)      
      if (!exists(x = 'estadosOk')) {
        for (coluna in colnames(estados@data)) {
          if (is.factor(estados@data[ , coluna])) {
            caractere <- as.character(estados@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            estados@data[ , coluna] <<- as.factor(caractere)
          }
        }
        estadosOk <<- TRUE
      }
      incProgress(1 / 7)
      load(file = 'dados/PNAD1/dados-capitais.Rda')
      incProgress(1 / 7)
      load(file = 'dados/PNAD1/dados-estados.Rda')
      incProgress(1 / 7)
      load(file = 'dados/PNAD1/dados-brasil.Rda')
      incProgress(1 / 7)  
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
