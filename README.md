## SIG Web Aberto

##### **S**istema de **I**nformação **G**eográfica para **Web** de dados **Aberto**s. O proposito desse projeto é o de disponibilizar informações públicas de institutos, fundações, associações, organizações, etc. em forma de mapas, gráficos e de tabelas. Esse projeto está sendo desenvolvido basicamente com a linguagem de programação R. Sugestões e comentários são bem-vindos, é só entrar em contato.

### Observações:

##### Para instalar os pacotes necessários no Windows no console do R:
```
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinydashboard")
install.packages("stringi")
install.packages("DT")
install.packages("geojsonio")
install.packages("leaflet")
install.packages("ggplot2")
```
##### No Windows, para lançar a aplicação do console do R:
```
library(shiny)

setwd("C:\\sig_web_aberto")

shiny::runApp()
```
##### No arquivo INICIO.R
```
endereco <- 'YOUR ADDRESS ON SENDGRID'
chave <- 'YOUR KEY ON SENDGRID'
emailSuporte <- 'YOUR SUPORT EMAIL'
nomeSuporte <- 'YOUR SUPORT NAME'
```
