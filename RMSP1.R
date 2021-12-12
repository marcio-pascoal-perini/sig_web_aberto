############
## Página ##
############

RMSP1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosRMSP1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Indicadores da Região Metropolitana de São Paulo',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloRMSP1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaRMSP1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoRMSP1', width = '100%', height = '500px'),
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    dataTableOutput(outputId = 'tabelaRMSP1', width = '100%'),
    tags$div(id = 'separator3', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

RMSP1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))

  ## variáveis e vetores ##

  topico <- 'populacao'
  topicos <- c('População' = 'populacao',
               'Trabalho e Rendimento' = 'trabalho_e_rendimento',
               'Educação' = 'educacao',
               'Econômia' = 'economia',
               'Saúde' = 'saude',
               'Território e Ambiente' = 'territorio_e_ambiente'
  )
  
  topicoPopulacao <- 'populacao_estimada'
  listaPopulacao <- c('População Estimada (pessoas)' = 'populacao_estimada',
                      'População no Último Censo (pessoas)' = 'populacao_no_ultimo_censo',
                      'Densidade Demográfica (hab/km²)' = 'densidade_demografica'
  )
  
  topicoTrabalhoERendimento <- 'salario_medio_mensal_dos_trabalhadores_formais'
  listaTrabalhoERendimento <- c('Salário Médio Mensal dos Trabalhadores Formais (salários mínimos)' = 'salario_medio_mensal_dos_trabalhadores_formais',
                      'Pessoal Ocupado (pessoas)' = 'pessoal_ocupado',
                      'População Ocupada (%)' = 'populacao_ocupada'
  )
  
  topicoEducacao <- 'matriculas_no_ensino_fundamental'
  listaEducacao <- c('Matrículas no Ensino Fundamental' = 'matriculas_no_ensino_fundamental',
                     'Matrículas no Ensino Médio' = 'matriculas_no_ensino_medio',
                     'Docentes no Ensino Fundamental' = 'docentes_no_ensino_fundamental',
                     'Docentes no Ensino Médio' = 'docentes_no_ensino_medio',
                     'Escolas de Ensino Fundamental' = 'escolas_de_ensino_fundamental',
                     'Escolas de Ensino Médio' = 'escolas_de_ensino_medio'
  )

  topicoEconomia <- 'PIB_per_capita'
  listaEconomia <- c('PIB per capita (reais)' = 'PIB_per_capita',
                     'IDHM - Índice de Desenvolvimento Humano Municipal' = 'IDHM'
  )
  
  topicoSaude <- 'mortalidade_infantil'
  listaSaude <- c('Mortalidade Infantil (óbitos por mil nascidos vivos)' = 'mortalidade_infantil',
                  'Estabelecimentos de Saúde SUS' = 'estabelecimentos_de_saude_SUS'
  )

  topicoTerritorioEAmbiente <- 'area_da_unidade_territorial'
  listaTerritorioEAmbiente <- c('Área da Unidade Territorial (km²)' = 'area_da_unidade_territorial',
                                'Esgotamento Sanitário Adequado (%)' = 'esgotamento_sanitario_adequado',
                                'Arborização de Vias Públicas (%)' = 'arborizacao_de_vias_publicas',
                                'Urbanização de Vias Públicas (%)' = 'urbanizacao_de_vias_publicas'
  )

  indicador <- topicoPopulacao
  indicadores <- listaPopulacao

  registrosPorPagina <- 10
  
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Tabela' = 'tabela')

  fontes <- 'IBGE, INEP, SUFRAMA e PNUD'

  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Tópico:</b> %s; <b>Indicador:</b> %s; <b>Fontes:</b> %s.',
        names(topicos[topicos == topico]),
        names(indicadores[indicadores == indicador]),
        fontes
      )
    )
  }
  
  camadaRMSP <- function() {
    temp <- subset(dadosRMSP, select = c('codigo', indicador))
    names(temp)[names(temp) == indicador] <- 'valor'
    sp::merge(rmsp, temp, by.x = 'codigo', by.y = 'codigo')    
  }

  dadosGrafico <- function() {
    temp <- subset(dadosRMSP, select = c('local', indicador))
    names(temp)[names(temp) == indicador] <- 'valor'
    maiores <- temp[order(temp$valor, decreasing = TRUE),]
    maiores <- head(x = maiores, n = 4)
    menores <- temp[order(temp$valor, decreasing = FALSE),]
    menores <- head(x = menores, n = 4)
    temp <- rbind(maiores, menores)
    temp$local <- factor(temp$local, levels = temp$local[order(temp$valor)])
    return(temp)
  }

  colunasTabela <- function() {
    temp <- c('codigo', 'local')
    for(item in indicadores) {
      temp <- c(temp, item) 
    }
    return(temp)
  }  
  
  titulosTabela <- function() {
    temp <- c('Código', 'Cidade')
    temp <- c(temp, names(indicadores))
  }  
  
  dadosTabela <- function() {
    subset(dadosRMSP, select = colunasTabela())
  }
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s', names(indicadores[indicadores == indicador]))
          camada <- camadaRMSP()
          caixas <- unique(quantile(camada$valor, names = FALSE, probs = seq(0, 1, length = 7), type = 1))
          qpal <- colorBin('YlOrRd', domain = camada$valor, bins = caixas)
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Cidade: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$local,
                             format(x = camada$valor, big.mark = '.')
          ) %>%
            lapply(htmltools::HTML)          
          if (inicial) {
            session$output$cartogramaRMSP1 <- renderLeaflet({
              isolate({
                m <- leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12))
                # m <- addTiles(map = m)
                m <- addProviderTiles(map = m, provider = 'CartoDB.Positron')
                m <- addEasyButton(map = m,
                                   button = easyButton
                                   (
                                     icon = 'fa-undo',
                                     title = 'Voltar às coordenadas iniciais',
                                     onClick = JS('function(btn, map){map.setView([-23.65493774414063, -46.39663696289063], 9);}')
                                   )
                )
                m <- setView(map = m, lng = -46.39663696289063, lat = -23.65493774414063, zoom = 9)
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
                  labFormat = labelFormat(big.mark = '.', between = ' a ')
                )  
                m
              })
            })            
          } else {
            isolate({ 
              m <- leafletProxy(mapId = 'cartogramaRMSP1', session = session)
              m %>% clearShapes() 
              m %>% clearMarkers()
              m %>% clearControls()
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
                labFormat = labelFormat(big.mark = '.', between = ' a ')
              )                      
            })            
          }
          show(id = 'cartogramaRMSP1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaRMSP1')
          hide(id = 'separator1')
        }
        incProgress(1 / 3)
        ## Gráfico ##
        if ('grafico' %in% visoes) {
          session$output$graficoRMSP1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$local, y = dg$valor)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#ffaa00') +
                labs(title = sprintf('%s', names(indicadores[indicadores == indicador])), x = '', y = '') +
                geom_text(aes(label = format(x = dg$valor, big.mark = '.')), vjust = 1.6, color = '#000000', size = 4) +
                #geom_text(aes(label = dg$valor), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })
          show(id = 'graficoRMSP1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoRMSP1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 3)
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaRMSP1 <- DT::renderDataTable({
            isolate({
              DT::datatable(
                data = dadosTabela(),
                colnames = titulosTabela(),
                options = list(searching = TRUE,
                               pageLength = registrosPorPagina,
                               stateSave = TRUE,
                               language = list(url = 'Portuguese-Brasil.json')
                )        
              ) %>% formatStyle(columns = indicador, backgroundColor = '#e8e8e8') 
            })            
          })
          show(id = 'tabelaRMSP1')
          show(id = 'separator3')      
        } else {
          hide(id = 'tabelaRMSP1')
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
  
  observeEvent(input$buttonFiltrosRMSP1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectTopicoRMSP1',
                             label = 'Tópico:',
                             choices = topicos,
                             selectize = FALSE,
                             selected = topico
                 )
          ),
          column(width = 6,
                 checkboxGroupInput(inputId = 'checkboxVisoesRMSP1',
                                    label = 'Visões:',
                                    choices = c('Cartograma' = 'cartograma',
                                                'Gráfico' = 'grafico',
                                                'Tabela' = 'tabela'
                                    ),
                                    selected = visoes
                 )
          )
        ),
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectIndicadorRMSP1',
                             label = 'Indicador:',
                             choices = indicadores,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectIndicadorRMSP1), indicador, session$input$selectIndicadorRMSP1)
                 )
          )        
        ),
        footer = tagList(
          actionButton(inputId = 'buttonOkRMSP1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelRMSP1', label = 'Cancelar')
        )      
      )
    )    
  })

  observeEvent(input$selectTopicoRMSP1, {
    indicadores <<- switch(input$selectTopicoRMSP1,
                           'populacao' = {listaPopulacao},
                           'trabalho_e_rendimento' = {listaTrabalhoERendimento},
                           'educacao' =  {listaEducacao},
                           'economia' = {listaEconomia},
                           'saude' = {listaSaude},
                           'territorio_e_ambiente' = {listaTerritorioEAmbiente}
    ) 
    updateSelectInput(inputId = 'selectIndicadorRMSP1',
                      session = session,
                      choices = indicadores
    )
  })
  
  observeEvent(input$buttonOkRMSP1, {
    ## Fechar modal
    removeModal()  
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis ##
    topico <<- session$input$selectTopicoRMSP1
    indicador <<- session$input$selectIndicadorRMSP1
    visoes <<- session$input$checkboxVisoesRMSP1
    ## Subtitulo ##
    output$textSubtituloRMSP1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelRMSP1, {
    updateSelectInput(inputId = 'selectTopicoRMSP1', session = session, selected = topico)
    updateSelectInput(inputId = 'selectIndicadorRMSP1', session = session, selected = indicador)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesRMSP1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaRMSP1_state, {
    tryCatch({
      info <- input$tabelaRMSP1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloRMSP1 <- renderText({
    isolate({
      subTitulo()
    })
  })

  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'rmsp')) {
        rmsp <<- geojsonio::geojson_read(x = 'dados/geojsons/rmsp.geojson', what = 'sp')        
      }            
      incProgress(1 / 3)      
      if (!exists(x = 'rmspOk')) {
        for (coluna in colnames(rmsp@data)) {
          if (is.factor(rmsp@data[ , coluna])) {
            caractere <- as.character(rmsp@data[ , coluna])
            Encoding(caractere) <- 'UTF-8'
            rmsp@data[ , coluna] <<- as.factor(caractere)
          }
        }
        rmspOk <<- TRUE
      }      
      incProgress(1 / 3)            
      load(file = 'dados/RMSP1/dados-rmsp.Rda')
      incProgress(1 / 3)
    }
  )  

  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
