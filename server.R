# server.R
library(quantmod)
library(dplyr)
library(magrittr)
library(ggplot2)
source("helpers.R")
library(ggvis)

oil<-getSymbols('DCOILWTICO',src='FRED',
                from = start,
                to = end,
                auto.assign = FALSE) 

px_b <- oil[!is.na(oil[,1])]
names(px_b)<-c("oil.Adjusted")

shinyServer(function(input, output) {

  #dataInput <- reactive({
  #  getSymbols(input$symb, src = "yahoo", 
  #             from = input$dates[1],
  #             to = input$dates[2],
  #             auto.assign = FALSE)
  #})
  
  px_a <-reactive({ 
    #runs when the button is pushed
    input$getstock
    
    getSymbols(isolate(input$symb), src = "yahoo", 
               from = isolate(input$dates[1]),
               to = isolate(input$dates[2]),
               auto.assign = FALSE)
  })
    
  
    

  px_2<-reactive({
    merge(to.period(px_a(),
                        period = input$per),
              to.period(px_b,
                        period = input$per,
                        name=c("open","high","low","oil.Adjusted")),  
              join='inner')
  })

  
  clean_px<-  reactive({
    data.frame(date=index(px_2()),px_2(), row.names = NULL) %>%
    select(date,contains(".Adjusted")) %>%
    mutate(a_delt = (.[,2]-lag(.[,2]))/lag(.[,2])) %>%
    mutate(b_delt = (.[,3]-lag(.[,3]))/lag(.[,3])) %>%
    filter(!is.na(a_delt) & !is.na(b_delt)) %>%
    arrange(desc(date))
  })

  clean_zoo<-reactive({
  as.zoo(as.matrix(clean_px()[,-1]), as.Date(clean_px()[,1]))
  })

  
  coeffdata <-  reactive({
    rollapply(clean_zoo(), width=input$window,
                          FUN = function(z) {
                            as.numeric(coef(lm(a_delt ~ b_delt, data = as.data.frame(z))))},
                          by.column = FALSE,
                          fill = NA,
                          align="right",
                          partial = FALSE) %>%
    data.frame(date = index(.),coredata(.))
    
  })
  
  output$line <- renderPlot({
    LD<-coeffdata()
    l <- ggplot(LD, aes(x=as.Date(date), y=X2)) +
      geom_line() + 
      scale_x_date()
    l
  })
  
  output$scatter <- renderPlot({
    dd<-clean_px() %>%
      head(input$window)
    s <- ggplot(dd, aes(x = b_delt, y = a_delt)) + 
      geom_point() +
      geom_smooth(method="lm")
    print(s)
  })
  
  clean_px2 <- reactive({
    clean_px() %>%
    mutate(date_t = as.character(format(date,'%m-%d-%Y')))
  })
  
  
  scatterData <- reactive({
    clean_px2() %>%
      head(input$window)
  })
    
  scatterData %>%
    ggvis(x = ~b_delt, y = ~a_delt) %>%
    layer_points(size = 2, 
                 key:= ~date_t,
                 size := 50, 
                 size.hover := 100, 
                 fillOpacity := 0.5, fillOpacity.hover := 0.9) %>%
    layer_model_predictions(model="lm", formula = a_delt ~ b_delt, se = TRUE) %>%
    add_tooltip(function(data){
      paste0(names(data),":",format(data),collapse = "<br />")}
      ,"hover") %>%
    add_axis("x", title = "% Change in Oil Price", title_offset = 50) %>%
    add_axis("y", title = "% Change in Stock Price", title_offset = 50) %>%
    bind_shiny("ggscatter")

  output$scatterTable <- renderTable({
    scatterData()
  })
  
})