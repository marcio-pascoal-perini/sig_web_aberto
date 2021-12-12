RAIS <- function(session) {
  tabPage <- fluidPage(
    tags$h1('RAIS')    
  )
  
  on.exit({
    input <- session$input
    output <- session$output
  })
  
  return(tabPage)
}
