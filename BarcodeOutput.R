################## Code for barcode output ###################

# Create dataset for barcode plot
data_barcode <- reactive({
  # selected genes
  selected_genes <- dataComp()
  
  full_data <- data.frame(Gene_name = character(),
                          percentiles = numeric(),
                          dataset = character())
  for (f in 1:length(files)){
    df <- get(files[f])
    df <- df %>% 
      select(Gene_name, percentiles, dataset) %>%  
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(percentiles)) %>% 
      distinct(Gene_name, .keep_all = T)
    full_data <- rbind(full_data, df)
  }
  # transform the dataset variable as factor and order the levels to group them by method
  full_data$dataset <- as.factor(full_data$dataset)
  full_data$dataset<- ordered(full_data$dataset, levels = files)
  
  full_data
})

# Zoom button
observeEvent((input$ab), {
  if(length(dataComp())!=0){
    if(nrow(data_barcode())!=0){
      showModal(modalDialog(
        renderPlot({
          plot_barcode() + theme(
            axis.title = element_text(size = 20),
            text = element_text(size = 20),
            plot.title = element_text(size = 26)
          )
        }, height = 600),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    }
  }
})

# Make the plot
plot_barcode <- reactive({
  full_data <- data_barcode()
  full_data$y=1 # because we need a y variable for geom_tile
  
  ggplot(full_data, aes(x=percentiles,y=y))+
    geom_tile(aes(fill=dataset), width = 0.4)+
    facet_wrap(~Gene_name, ncol = 2)+
    ylab("")+
    xlim(c(0,100))+
    theme_bw()+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"))+
    scale_fill_manual(values = c("Chung_scell"="#027B4B", 
                                 "Karaiskos_scell" = "#2DB84F", 
                                 "Park_scell" = "#57CB1D",
                                 "Boerries_transcriptome" = "#5739FC", 
                                 "Kann_transcriptome" = "#398CFC", 
                                 "Rinschen_Goedel_transcriptome" = "#39B5FC",
                                 "Boerries_proteome" = "#FC3939", 
                                 "Rinschen_proteome" = "#FC3992", 
                                 "CMS_proteome" = "#F339FC"))+
    NULL 
})

# Display barcode plot
output$barcode <- renderPlotly({
  if (length(dataComp())!=0){
    if(nrow(data_barcode())!=0){
      p <- plot_barcode()
      ggplotly(p, height = 200+(length(dataComp())*20))%>% 
        config(
          modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
          modeBarButtonsToAdd = list(dl_button)
        )
    }
  }
})