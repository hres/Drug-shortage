
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  tab_list <- NULL
  observeEvent(input$select_ing,{
    
    companies<-create_query(input$select_ing)%>%result()%>%distinct(company_name)%>%pull()
    
    updateSelectInput(session,"select_company",
                      choices=c(c("Start typing to search..." = "", companies)))
    
    
  })
   
  df<-reactive({
    
     df<-create_query(input$select_ing)%>%result()
      
      if(input$select_company!="")
        df<-create_query(input$select_ing,input$select_company)%>%result()
      
    return(df)
    
  })
  
  
  output$drugplot_title<-renderUI({
    
    title<-paste("Drug Shortage reports for:",input$select_ing)
    h2(strong(title))
  })
  
  output$drugplot<-renderPlotly({
    
    df<-df()
    
    n_brand<-length(unique(df$en_drug_brand_name))
    
    qual_col_pals = RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unique(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))
    
    if(n_brand==1){
    cols<-c('#8DD3C7')
    }else if(n_brand==2){
    cols<-c('#FFFFB3','#8DD3C7')
    }else{
    cols <- sample(col_vector,length(unique(df$en_drug_brand_name)),F)
    }
    
    df$color <- factor(df$en_drug_brand_name, labels = cols)
    df<-df%>%arrange(en_drug_brand_name)
   
    width<-20/(nrow(df)/12)     #automatically adjust line width according to report number
    
    # Initialize empty plot
    p <- plot_ly(height=450,width=1120)
    
    # Each task is a separate trace
    # Each trace is essentially a thick line plot
    # x-axis ticks are dates and handled automatically
    
    #control legend, show legend only for brandname groups
    df$legend<-ifelse(df$en_drug_brand_name==lag(df$en_drug_brand_name,default=T),F,T)
    
    for(i in 1:(nrow(df))){
      
      p <- add_trace(p,
                     x = c(df$actual_start_date[i], df$actual_end_date[i]),  # x0, x1
                     y = c(i, i),  # y0, y1
                     mode = "lines",
                     line = list(color = df$color[i], width = width),
                     showlegend = df$legend[i],
                     hoverinfo = "text",
                     name=df$en_drug_brand_name[i],
                     
                     # Create custom hover text
                     
                     text = paste("Product: ", df$en_drug_brand_name[i], "<br>",
                                  "Company: ", df$drug.company.name[i], "<br>",
                                  "Start: ", df$actual_start_date[i],"<br>",
                                  "End: ", df$actual_end_date[i]),
                     
                     evaluate = T  # needed to avoid lazy loading
      )
    }
    
    p%>%layout(legend = list(orientation = 'h'))
  
})
  
  
timeslot<-reactive({
  df<-df()
  
  shortageInterval<-interval(df$actual_start_date,df$actual_end_date)
  timeslot<-data.frame(day=seq(min(df$actual_start_date),max(df$actual_end_date),by='day'))
  timeslot$count<-sapply(timeslot$day, function(x) sum(x %within% shortageInterval))
  
  return(timeslot)
})
  
 
  output$freqplot<-renderPlotly({

    plot_ly(timeslot(),x=~day, y=~count,type='bar',height=300,width=1120,source='subset')%>%
      layout(xaxis = list(title = 'time'),
             yaxis = list(title = ''),
             dragmode =  "click")

  })
  
  
  
  observeEvent(event_data("plotly_click", source = "subset"),
               {
                 df<-df()
                   
                 timeslot<-timeslot()
                 event.data<-plotly::event_data("plotly_click", source = "subset")
                 
                 if(is.null(event.data) == T) return(NULL)
                 tab_title <- paste0('selected table from ',as.character(event.data$x[1]))
                 
                 if(tab_title %in% tab_list == FALSE){
                 
                 timeslot_subset<-timeslot%>%filter(day %in% ymd(event.data$x))
                 
                 min_date<-min(timeslot_subset$day)
                 max_date<-max(timeslot_subset$day)
                 
                 df<-df%>%filter(!(actual_end_date<min_date | actual_start_date >=max_date))
                 
                
                 
                 appendTab(inputId = "tabs",
                           tabPanel(
                             tab_title,
                             DT::renderDataTable(df,
                                                 options=list(scrollX=T))
                           ))
                 
                 tab_list <<- c(tab_list, tab_title)
                 
               }
               
               updateTabsetPanel(session, "tabs", selected = tab_title)
                 
                 
                 
               })
  

  output$freqplot_title<-renderUI({
    
    title<-"Shortage Frequency plot"
    h4(strong(title))
  })
  
  observeEvent(input$remove,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })
  
  
})
