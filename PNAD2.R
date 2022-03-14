############
## Página ##
############

PNAD2_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosPNAD2',
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
                   uiOutput(outputId = 'textSubtituloPNAD2'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaPNAD2', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoPNAD2', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaPNAD2', width = '100%', height = '500px'),
    tags$div(id = 'separator3', style = 'padding: 5px;'),
    dataTableOutput(outputId = 'tabelaPNAD2', width = '100%'),
    tags$div(id = 'separator4', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

PNAD2_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))  
  
  ## variáveis e vetores ##
  
  abrangencia <- 'estados'
  abrangencias <- c('Estados' = 'estados', 'Regiões' = 'regioes')
  
  trimestre <- 202104
  trimestres <- c('4º trimestre 2021' = 202104,
                  '3º trimestre 2021' = 202103,
                  '2º trimestre 2021' = 202102,
                  '1º trimestre 2021' = 202101,                  
                  '4º trimestre 2020' = 202004,
                  '3º trimestre 2020' = 202003,
                  '2º trimestre 2020' = 202002,
                  '1º trimestre 2020' = 202001,
                  '4º trimestre 2019' = 201904,
                  '3º trimestre 2019' = 201903,
                  '2º trimestre 2019' = 201902,
                  '1º trimestre 2019' = 201901,
                  '4º trimestre 2018' = 201804,
                  '3º trimestre 2018' = 201803,
                  '2º trimestre 2018' = 201802,
                  '1º trimestre 2018' = 201801                  
  )
  
  indicador <- 4099
  indicadores <-   c('Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4096,
                     'Nível de ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4097,
                     'Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4099,
                     'Nível de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade' = 4098
                     
  )

  registrosPorPagina <- 10

  fonte <- 'IBGE - Tabelas: 6461, 6466, 6467 e 6468'

  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Histograma' = 'histograma', 'Tabela' = 'tabela')
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Abrangência:</b> %s; <b>Trimestre:</b> %s; <b>Indicador:</b> %s; <b>Fonte:</b> %s.',
        names(abrangencias[abrangencias == abrangencia]),
        names(trimestres[trimestres == trimestre]),
        names(indicadores[indicadores == indicador]),
        fonte
      )
    )
  }
  
  camadaEstados <- function() {
    temp <- subset(dadosEstados, D2C == indicador & D3C == trimestre, select = c('D1C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(estados, temp, by.x = 'codigo', by.y = 'D1C')
  }
  
  camadaRegioes <- function() {
    temp <- subset(dadosRegioes, D2C == indicador & D3C == trimestre, select = c('D1C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(regioes, temp, by.x = 'codigo', by.y = 'D1C')
  }

  dadosGrafico1 <- function() {
    temp <- switch(abrangencia,
                   'estados' = subset(dadosEstados, D2C == indicador & D3C == trimestre, select = c('D1N', 'V')),
                   'regioes' = subset(dadosRegioes, D2C == indicador & D3C == trimestre, select = c('D1N', 'V'))
    )
    maiores <- temp[order(temp$V, decreasing = TRUE),]
    maiores <- head(x = maiores, n = 4)
    menores <- temp[order(temp$V, decreasing = FALSE),]
    menores <- head(x = menores, n = 4)
    temp <- rbind(maiores, menores)
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$V)])
    return(temp)
  }
  
  dadosGrafico2 <- function() {
    temp <- switch(abrangencia,
                   'estados' = subset(dadosEstados, D2C == indicador & D3C == trimestre, select = c('D1N', 'V')),
                   'regioes' = subset(dadosRegioes, D2C == indicador & D3C == trimestre, select = c('D1N', 'V'))
    )
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$V)])
    return(temp)
  }
  
  dadosHistograma <- function() {
    temp <- subset(dadosBrasil, D2C == indicador, select = c('D3C', 'D3N', 'V'))
    temp$D3N <- factor(temp$D3N, levels = temp$D3N[order(temp$D3C)])
    return(temp)
  }
  
  colunasTabela <- function() {
    c('D1C', 'D1N', 'D2N', 'D3N', 'V')
  }  
  
  titulosTabela <- function() {
    c('Código', 'Nome', 'Indicador', 'Trimestre', '%')
  }  
  
  dadosTabela <- function() {
    switch(abrangencia,
           'estados' = subset(dadosEstados, D2C == indicador & D3C == trimestre, select = colunasTabela()),
           'regioes' = subset(dadosRegioes, D2C == indicador & D3C == trimestre, select = colunasTabela())
    )
  }
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s', names(trimestres[trimestres == trimestre]))
          if (inicial) {
            session$output$cartogramaPNAD2 <- renderLeaflet({
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
                  camada <- camadaRegioes()
                  caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
                  qpal <- colorBin('Greens', domain = camada$valor, bins = caixas)
                  rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Região: %s</strong><br/><strong>%s&#37;</strong>',
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
                } 
                m
              })
            })
          } else {
            isolate({ 
              m <- leafletProxy(mapId = 'cartogramaPNAD2', session = session)
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
                camada <- camadaRegioes()
                caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
                qpal <- colorBin('Greens', domain = camada$valor, bins = caixas)
                rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Região: %s</strong><br/><strong>%s&#37;</strong>',
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
              } 
            })
          }
          show(id = 'cartogramaPNAD2')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaPNAD2')
          hide(id = 'separator1')
        }
        incProgress(1 / 4)
        ## Gráfico ##
        if ('grafico' %in% visoes) { 
          session$output$graficoPNAD2 <- renderPlot({
            isolate({
              if (abrangencia == 'estados') {
                dg <- dadosGrafico1()
              } else {
                dg <- dadosGrafico2()
              }
              ggplot(data = dg, aes(x = dg$D1N, y = dg$V)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#62ba81') +
                labs(title = sprintf('%s', names(trimestres[trimestres == trimestre])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'graficoPNAD2')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoPNAD2')
          hide(id = 'separator2')      
        }
        incProgress(1 / 4)
        ## Histograma ##
        if ('histograma' %in% visoes) {
          session$output$histogramaPNAD2 <- renderPlot({
            isolate({
              dh <- dadosHistograma() 
              ggplot(data = dh, aes(x = dh$D3N, y = dh$V)) +
                geom_line(colour = '#62ba81', size = 2, linetype = 1, group = 1) +
                geom_point(colour = ifelse(dh$D3C == trimestre, '#ff0000', '#808080'), size = 4) +
                labs(title = sprintf('%s', names(trimestres[trimestres == trimestre])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dh$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )                
            })              
          })          
          show(id = 'histogramaPNAD2')
          show(id = 'separator3')    
        } else {
          hide(id = 'histogramaPNAD2')
          hide(id = 'separator3')    
        }
        incProgress(1 / 4)        
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaPNAD2 <- DT::renderDataTable({
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
          show(id = 'tabelaPNAD2')
          show(id = 'separator4')      
        } else {
          hide(id = 'tabelaPNAD2')
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
  
  observeEvent(input$buttonFiltrosPNAD2, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectAbrangenciaPNAD2',
                             label = 'Abrangência:',
                             choices = abrangencias,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectAbrangenciaPNAD2), abrangencia, session$input$selectAbrangenciaPNAD2)
                 )
          ),        
          column(width = 6,
                 selectInput(inputId = 'selectTrimestrePNAD2',
                             label = 'Trimestre:',
                             choices = trimestres,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectTrimestrePNAD2), trimestre, session$input$selectTrimestrePNAD2)
                 )
          )),
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectIndicadorPNAD2',
                             label = 'Indicador:',
                             choices = indicadores,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectIndicadorPNAD2), indicador, session$input$selectIndicadorPNAD2)
                 )
          )),
        fluidRow(
          column(width = 6,
                 checkboxGroupInput(inputId = 'checkboxVisoesPNAD2',
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
          actionButton(inputId = 'buttonOkPNAD2', label = 'Ok'),
          actionButton(inputId = 'buttonCancelPNAD2', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkPNAD2, {
    ## Fechar modal
    removeModal()
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    abrangencia <<- session$input$selectAbrangenciaPNAD2
    trimestre <<- session$input$selectTrimestrePNAD2
    indicador <<- session$input$selectIndicadorPNAD2
    visoes <<- session$input$checkboxVisoesPNAD2
    ## Subtitulo ##
    output$textSubtituloPNAD2 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelPNAD2, {
    updateSelectInput(inputId = 'selectAbrangenciaPNAD2', session = session, selected = abrangencia)
    updateSelectInput(inputId = 'selectTrimestrePNAD2', session = session, selected = trimestre)
    updateSelectInput(inputId = 'selectIndicadorPNAD2', session = session, selected = indicador)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesPNAD2', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaPNAD2_state, {
    tryCatch({
      info <- input$tabelaPNAD2_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloPNAD2 <- renderText({
    isolate({
      subTitulo()
    })
  })
  
  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'regioes')) {
        regioes <<- geojsonio::geojson_read(x = 'dados/geojsons/regioes.geojson', what = 'sp')        
      }      
      incProgress(1 / 7) 
      if (!exists(x = 'regioesOk')) {
        for (coluna in colnames(regioes@data)) {
          if (is.factor(regioes@data[ , coluna])) {
            caractere <- as.character(regioes@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            regioes@data[ , coluna] <<- as.factor(caractere)
          }
        }
        regioesOk <<- TRUE
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
      load(file = 'dados/PNAD2/dados-regioes.Rda')
      incProgress(1 / 7)
      load(file = 'dados/PNAD2/dados-estados.Rda')
      incProgress(1 / 7)
      load(file = 'dados/PNAD2/dados-brasil.Rda')
      incProgress(1 / 7)  
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
