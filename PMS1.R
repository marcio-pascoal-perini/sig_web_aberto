############
## Página ##
############

PMS1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosPMS1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Pesquisa Mensal de Serviços',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloPMS1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaPMS1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoPMS1', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaPMS1', width = '100%', height = '500px'),
    tags$div(id = 'separator3', style = 'padding: 5px;'),    
    dataTableOutput(outputId = 'tabelaPMS1', width = '100%'),
    tags$div(id = 'separator4', style = 'padding: 5px;')
  )
  
  return(tabPage)
}

###################
## Administração ##
###################

PMS1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))
  
  ## variáveis e vetores ##
  
  mes <- 202211
  meses <- c('Novembro 2022' = 202211,
             'Outubro 2022' = 202210,
             'Setembro 2022' = 202209,
             'Agosto 2022' = 202208,
             'Julho 2022' = 202207,
             'Junho 2022' = 202206,
             'Maio 2022' = 202205,
             'Abril 2022' = 202204,
             'Março 2022' = 202203,
             'Fevereiro 2022' = 202202,
             'Janeiro 2022' = 202201,
             'Dezembro 2021' = 202112,
             'Novembro 2021' = 202111,
             'Outubro 2021' = 202110,
             'Setembro 2021' = 202109,
             'Agosto 2021' = 202108,
             'Julho 2021' = 202107,
             'Junho 2021' = 202106,
             'Maio 2021' = 202105,
             'Abril 2021' = 202104,
             'Março 2021' = 202103,
             'Fevereiro 2021' = 202102,
             'Janeiro 2021' = 202101,
             'Dezembro 2020' = 202012,
             'Novembro 2020' = 202011,
             'Outubro 2020' = 202010,
             'Setembro 2020' = 202009,
             'Agosto 2020' = 202008,
             'Julho 2020' = 202007,
             'Junho 2020' = 202006,
             'Maio 2020' = 202005,
             'Abril 2020' = 202004,
             'Março 2020' = 202003,
             'Fevereiro 2020' = 202002,
             'Janeiro 2020' = 202001
  )

  variavel <- 56725
  variaveis <- c('Índice de receita nominal de serviços' = 56725,
                 'Índice de volume de serviços' = 56726
  )
  
  tipo = 11624
  tipos = c('Variação mensal' = 11624,  
            'Variação acumulada no ano' = 11625,  
            'Variação acumulada em 12 meses' = 11626
  )

  registrosPorPagina <- 10
  
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Histograma' = 'histograma', 'Tabela' = 'tabela')

  fonte <- 'IBGE - Tabela 8161'
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Variável:</b> %s; <b>Tipo de índice:</b> %s; <b>Mês:</b> %s; <b>Fonte:</b> %s.',
        names(variaveis[variaveis == variavel]),
        names(tipos[tipos == tipo]),
        names(meses[meses == mes]),
        fonte
      )
    )
  }
  
  camadaEstados <- function() {
    temp <- subset(dadosEstados, D1C == mes & D4C == variavel & D2C == tipo, select = c('D3C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(estados, temp, by.x = 'codigo', by.y = 'D3C')
  }
  
  dadosGrafico <- function() {
    temp <- subset(dadosEstados, D1C == mes & D4C == variavel & D2C == tipo & !is.na(V), select = c('D3N', 'V'))
    maiores <- temp[order(temp$V, decreasing = TRUE),]
    maiores <- head(x = maiores, n = 5)
    menores <- temp[order(temp$V, decreasing = FALSE),]
    menores <- head(x = menores, n = 5)
    temp <- rbind(maiores, menores)    
    temp$D3N <- factor(temp$D3N, levels = temp$D3N[order(temp$V)])
    return(temp)
  }

  dadosHistograma <- function() {
    temp <- subset(dadosBrasil, D4C == variavel & D2C == tipo, select = c('D1C', 'D1N', 'V'))
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$D1C)])
    return(temp)
  }
  
  colunasTabela <- function() {
    c('D3C', 'D3N', 'D1N', 'D4N', 'D2N', 'V')
  }  
  
  titulosTabela <- function() {
    c('Código', 'Estado', 'Mês', 'Variável', 'Tipo de índice', '%')
  }  
  
  dadosTabela <- function() {
    subset(dadosEstados, D1C == mes & D4C == variavel & D2C == tipo, select = colunasTabela())
  }
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s<br>%s', names(variaveis[variaveis == variavel]), names(meses[meses == mes]))
          camada <- camadaEstados()
          caixas = unique(quantile(camada$valor, names = FALSE, na.rm = TRUE, probs = seq(0, 1, length = 7), type = 1))
          qpal <- colorBin('Reds', domain = camada$valor, na.color = '#a9a9a9', bins = caixas)
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$local,
                             ifelse(is.na(camada$valor), no = paste(format(x = camada$valor, big.mark = '.'),'%', sep = ''), yes = '')
          ) %>%
            lapply(htmltools::HTML)
          if (inicial) {          
            session$output$cartogramaPMS1 <- renderLeaflet({
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
                  labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%'),
                  na.label = 'Indisponível'
                )  
                m
              })
            })
          } else {
            isolate({
              m <- leafletProxy(mapId = 'cartogramaPMS1', session = session)
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
                labFormat = labelFormat(big.mark = '.', between = ' a ', suffix = '%'),
                na.label = 'Indisponível'
              )                
            })
          } 
          show(id = 'cartogramaPMS1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaPMS1')
          hide(id = 'separator1')
        }
        incProgress(1 / 4)
        ## Gráfico ##
        if ('grafico' %in% visoes) {
          session$output$graficoPMS1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$D3N, y = dg$V)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#ffad99') +
                labs(title = sprintf('%s - %s', names(variaveis[variaveis == variavel]), names(meses[meses == mes])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'graficoPMS1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoPMS1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 4)
        ## Histograma ##
        if ('histograma' %in% visoes) {
          session$output$histogramaPMS1 <- renderPlot({
            isolate({
              dh <- dadosHistograma() 
              ggplot(data = dh, aes(x = dh$D1N, y = dh$V)) +
                geom_line(colour = '#ffad99', size = 2, linetype = 1, group = 1) +
                geom_point(colour = ifelse(dh$D1C == mes, '#ff0000', '#808080'), size = 4) +
                labs(title = sprintf('%s\n%s', names(variaveis[variaveis == variavel]), names(tipos[tipos == tipo])), x = '', y = '') +
                geom_text(aes(label = sprintf('%s%s', format(x = dh$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )                
            })              
          })          
          show(id = 'histogramaPMS1')
          show(id = 'separator3')      
        } else {
          hide(id = 'histogramaPMS1')
          hide(id = 'separator3')      
        }
        incProgress(1 / 4)                        
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaPMS1 <- DT::renderDataTable({
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
          show(id = 'tabelaPMS1')
          show(id = 'separator4')      
        } else {
          hide(id = 'tabelaPMS1')
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
  
  observeEvent(input$buttonFiltrosPMS1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 6,
                 selectInput(inputId = 'selectMesPMS1',
                             label = 'Mês:',
                             choices = meses,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectMesPMS1), mes, session$input$selectMesPMS1)
                 )
          ),
          column(width = 6,
                 selectInput(inputId = 'selectVariavelPMS1',
                             label = 'Variável:',
                             choices = variaveis,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectVariavelPMS1), variavel, session$input$selectVariavelPMS1)
                 )
          )
        ),  
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectTipoPMS1',
                             label = 'Tipo de Índice:',
                             choices = tipos,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectTipoPMS1), tipo, session$input$selectTipoPMS1)
                 )
          )          
        ),
        fluidRow(
          column(width = 12,
                 checkboxGroupInput(inputId = 'checkboxVisoesPMS1',
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
          actionButton(inputId = 'buttonOkPMS1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelPMS1', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkPMS1, {
    ## Fechar modal
    removeModal()
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    mes <<- session$input$selectMesPMS1
    variavel <<- session$input$selectVariavelPMS1
    tipo <<- session$input$selectTipoPMS1
    visoes <<- session$input$checkboxVisoesPMS1
    ## Subtitulo ##
    output$textSubtituloPMS1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })
  
  observeEvent(input$buttonCancelPMS1, {
    updateSelectInput(inputId = 'selectMesPMS1', session = session, selected = mes)
    updateSelectInput(inputId = 'selectVariavelPMS1', session = session, selected = variavel)
    updateSelectInput(inputId = 'selectTipoPMS1', session = session, selected = tipo)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesPMS1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaPMS1_state, {
    tryCatch({
      info <- input$tabelaPMS1_state
      registrosPorPagina <<- info$length
    })    
  })

  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloPMS1 <- renderText({
    isolate({
      subTitulo()
    })
  })
  
  ## Dados ##  
  withProgress(
    message = 'Carregando dados...',
    value = 0, {
      if (!exists(x = 'estados')) {
        estados <<- geojsonio::geojson_read(x = 'dados/geojsons/estados.geojson', what = 'sp')        
      }      
      incProgress(1 / 4)      
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
      incProgress(1 / 4)
      load(file = 'dados/PMS1/dados-estados.Rda')
      incProgress(1 / 4) 
      load(file = 'dados/PMS1/dados-brasil.Rda')
      incProgress(1 / 4)        
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)
}
