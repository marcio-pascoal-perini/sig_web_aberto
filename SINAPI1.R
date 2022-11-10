############
## Página ##
############

SINAPI1_Pagina <- function(session) {
  on.exit({
    input <- session$input
    output <- session$output
  })  
  
  tabPage <- fluidPage(
    tags$table(style = 'border: 0 none; border-collapse: collapse; width: 100%;',
               tags$tr(
                 tags$td(actionButton(inputId = 'buttonFiltrosSINAPI1',
                                      label = 'Filtros',
                                      title = 'Filtros e Opções',
                                      class = 'btn-danger',
                                      style = 'font-weight: 600; color: #FFFFFF;'
                 ),
                 style = 'text-align: left; vertical-align: top;',
                 rowspan = '2'),
                 tags$td('Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil',
                         style = 'font-weight: normal; font-size: 22px; text-align: center; vertical-align: middle;')
               ),
               tags$tr(
                 tags$td(
                   uiOutput(outputId = 'textSubtituloSINAPI1'),
                   style = 'font-weight: normal; font-size: small; text-align: center; vertical-align: middle;'
                 )
               )
    ),
    tags$div(style = 'padding: 5px;'),
    leafletOutput(outputId = 'cartogramaSINAPI1', width = '100%', height = '500px'),
    tags$div(id = 'separator1', style = 'padding: 5px;'),
    plotOutput(outputId = 'graficoSINAPI1', width = '100%', height = '500px'),    
    tags$div(id = 'separator2', style = 'padding: 5px;'),
    plotOutput(outputId = 'histogramaSINAPI1', width = '100%', height = '500px'),
    tags$div(id = 'separator3', style = 'padding: 5px;'),    
    dataTableOutput(outputId = 'tabelaSINAPI1', width = '100%'),
    tags$div(id = 'separator4', style = 'padding: 5px;')
  )
  
  return(tabPage)  
}

###################
## Administração ##
###################

SINAPI1_Admin <- function(session) {
  ## Cortina ##
  
  show(id = 'cortina')
  delay(ms = 5000, hide(id = 'cortina'))
  
  ## variáveis e vetores ##
  
  mes <- 202210
  meses <- c('Outubro 2022' = 202210,
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
  
  variavel = 1196
  variaveis = c('Moeda corrente' = 48,  
                'Variação percentual no mês' = 1196,  
                'Variação percentual no ano' = 1197,
                'Variação percentual em doze meses' = 1198
  )
  
  registrosPorPagina <- 10
  
  visoes <- c('Cartograma' = 'cartograma', 'Gráfico' = 'grafico', 'Histograma' = 'histograma', 'Tabela' = 'tabela')
  
  fonte <- 'IBGE - Tabela 2296'
  
  ## funções ##
  
  subTitulo <- function() {
    HTML(
      sprintf(
        '<b>Variável:</b> %s; <b>Mês:</b> %s; <b>Fonte:</b> %s.',
        names(variaveis[variaveis == variavel]),
        names(meses[meses == mes]),
        fonte
      )
    )
  }  
  
  camadaEstados <- function() {
    temp <- subset(dadosEstados, D2C == mes & D3C == variavel, select = c('D1C', 'V'))
    names(temp)[names(temp) == 'V'] <- 'valor'
    sp::merge(estados, temp, by.x = 'codigo', by.y = 'D1C')
  }
  
  dadosGrafico <- function() {
    temp <- subset(dadosEstados, D2C == mes & D3C == variavel & !is.na(V), select = c('D1N', 'V'))
    maiores <- temp[order(temp$V, decreasing = TRUE),]
    maiores <- head(x = maiores, n = 5)
    menores <- temp[order(temp$V, decreasing = FALSE),]
    menores <- head(x = menores, n = 5)
    temp <- rbind(maiores, menores)    
    temp$D1N <- factor(temp$D1N, levels = temp$D1N[order(temp$V)])
    return(temp)
  }  
  
  dadosHistograma <- function() {
    temp <- subset(dadosBrasil, D3C == variavel, select = c('D2C', 'D2N', 'V'))
    temp$D2N <- factor(temp$D2N, levels = temp$D2N[order(temp$D2C)])
    return(temp)
  }
  
  colunasTabela <- function() {
    c('D1C', 'D1N', 'D2N', 'D3N', 'V')
  }  
  
  titulosTabela <- function() {
    c('Código', 'Estado', 'Mês', 'Variável (Custo médio m²)', 'Valor')
  }  
  
  dadosTabela <- function() {
    subset(dadosEstados, D2C == mes & D3C == variavel, select = colunasTabela())
  }  
  
  atualizarVisoes <- function(inicial) {
    withProgress(
      message = 'Atualizando...',
      value = 0, {   
        ## Cartograma ##
        if ('cartograma' %in% visoes) {
          titulo <- sprintf('%s<br>%s', names(variaveis[variaveis == variavel]), names(meses[meses == mes]))
          camada <- camadaEstados()
          caixas <- unique(quantile(camada$valor, names = FALSE, na.rm = TRUE, probs = seq(0, 1, length = 7), type = 1))
          qpal <- colorBin('Reds', domain = camada$valor, na.color = '#a9a9a9', bins = caixas)
          rotulos <- sprintf('<strong>Código: %s</strong><br/><strong>Estado: %s</strong><br/><strong>%s</strong>',
                             camada$codigo,
                             camada$local,
                             ifelse(is.na(camada$valor), no = format(x = camada$valor, big.mark = '.'), yes = '')
          ) %>%
            lapply(htmltools::HTML)
          if (inicial) {          
            session$output$cartogramaSINAPI1 <- renderLeaflet({
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
                  labFormat = labelFormat(big.mark = '.', between = ' a '),
                  na.label = 'Indisponível'
                )  
                m
              })
            })
          } else {
            isolate({
              m <- leafletProxy(mapId = 'cartogramaSINAPI1', session = session)
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
                labFormat = labelFormat(big.mark = '.', between = ' a '),
                na.label = 'Indisponível'
              )                
            })
          } 
          show(id = 'cartogramaSINAPI1')
          show(id = 'separator1')
        } else {
          hide(id = 'cartogramaSINAPI1')
          hide(id = 'separator1')
        }
        incProgress(1 / 4)
        ## Gráfico ##
        if ('grafico' %in% visoes) {
          session$output$graficoSINAPI1 <- renderPlot({
            isolate({
              dg <- dadosGrafico() 
              ggplot(data = dg, aes(x = dg$D1N, y = dg$V)) +
                geom_bar(stat = 'identity', colour = '#ffffff', fill = '#ffad99') +
                labs(title = sprintf('%s - %s', names(variaveis[variaveis == variavel]), names(meses[meses == mes])), x = '', y = '') +
               #geom_text(aes(label = sprintf('%s%s', format(x = dg$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                geom_text(aes(label = format(x = dg$V, big.mark = '.')), vjust = 1.6, color = '#000000', size = 4) +
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )
            })
          })          
          show(id = 'graficoSINAPI1')
          show(id = 'separator2')      
        } else {
          hide(id = 'graficoSINAPI1')
          hide(id = 'separator2')      
        }
        incProgress(1 / 4)
        ## Histograma ##
        if ('histograma' %in% visoes) {
          session$output$histogramaSINAPI1 <- renderPlot({
            isolate({
              dh <- dadosHistograma() 
              ggplot(data = dh, aes(x = dh$D2N, y = dh$V)) +
                geom_line(colour = '#ffad99', size = 2, linetype = 1, group = 1) +
                geom_point(colour = ifelse(dh$D2C == mes, '#ff0000', '#808080'), size = 4) +
                labs(title = names(variaveis[variaveis == variavel]), x = '', y = '') +
               #geom_text(aes(label = sprintf('%s%s', format(x = dh$V, big.mark = '.'), '%')), vjust = 1.6, color = '#000000', size = 4) +
                geom_text(aes(label = format(x = dh$V, big.mark = '.')), vjust = 1.6, color = '#000000', size = 4) +                
                theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                      axis.text.x = element_text(angle = 60, hjust = 1, face = 'bold', size = 10),
                      axis.text.y = element_text(face = 'bold', size = 10)
                )                
            })              
          })          
          show(id = 'histogramaSINAPI1')
          show(id = 'separator3')      
        } else {
          hide(id = 'histogramaSINAPI1')
          hide(id = 'separator3')      
        }
        incProgress(1 / 4)                        
        ## Tabela ##
        if ('tabela' %in% visoes) {
          session$output$tabelaSINAPI1 <- DT::renderDataTable({
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
          show(id = 'tabelaSINAPI1')
          show(id = 'separator4')      
        } else {
          hide(id = 'tabelaSINAPI1')
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
  
  observeEvent(input$buttonFiltrosSINAPI1, {
    showModal(
      session = session,
      modalDialog(
        easyClose = FALSE,
        fade = TRUE,
        size = 'l',
        title = 'Filtros e Opções',
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectMesSINAPI1',
                             label = 'Mês:',
                             choices = meses,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectMesSINAPI1), mes, session$input$selectMesSINAPI1)
                 )
          )
        ),  
        fluidRow(
          column(width = 12,
                 selectInput(inputId = 'selectVariavelSINAPI1',
                             label = 'Variável (Custo médio m²):',
                             choices = variaveis,
                             selectize = FALSE,
                             selected = ifelse(is.null(session$input$selectVariavelSINAPI1), variavel, session$input$selectVariavelSINAPI1)
                 )
          )          
        ),
        fluidRow(
          column(width = 12,
                 checkboxGroupInput(inputId = 'checkboxVisoesSINAPI1',
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
          actionButton(inputId = 'buttonOkSINAPI1', label = 'Ok'),
          actionButton(inputId = 'buttonCancelSINAPI1', label = 'Cancelar')
        )      
      )
    )    
  })
  
  observeEvent(input$buttonOkSINAPI1, {
    ## Fechar modal
    removeModal()
    ## Cortina ##
    show(id = 'cortina')
    delay(ms = 2500, hide(id = 'cortina'))    
    ## Variáveis
    mes <<- session$input$selectMesSINAPI1
    variavel <<- session$input$selectVariavelSINAPI1
    visoes <<- session$input$checkboxVisoesSINAPI1
    ## Subtitulo ##
    output$textSubtituloSINAPI1 <- renderText({
      isolate({
        subTitulo()
      })
    })
    ## Indicadores ## 
    atualizarVisoes(inicial = FALSE)
  })  
  
  observeEvent(input$buttonCancelSINAPI1, {
    updateSelectInput(inputId = 'selectMesSINAPI1', session = session, selected = mes)
    updateSelectInput(inputId = 'selectVariavelSINAPI1', session = session, selected = variavel)
    updateCheckboxGroupInput(inputId = 'checkboxVisoesSINAPI1', session = session, selected = visoes)
    removeModal()
  })
  
  observeEvent(input$tabelaSINAPI1_state, {
    tryCatch({
      info <- input$tabelaSINAPI1_state
      registrosPorPagina <<- info$length
    })    
  })
  
  ## Atribuições iniciais ##
  
  ## Subtitulo ##
  session$output$textSubtituloSINAPI1 <- renderText({
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
      load(file = 'dados/SINAPI1/dados-estados.Rda')
      incProgress(1 / 4) 
      load(file = 'dados/SINAPI1/dados-brasil.Rda')
      incProgress(1 / 4)        
    }
  )  
  
  ## Atualização inicial das visões ##  
  atualizarVisoes(inicial = TRUE)  
}
