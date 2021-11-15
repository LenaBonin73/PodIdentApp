################## Code for barcode output ###################

# Create dataset for barcode plot
data_barcode <- reactive({
  # selected genes
  selected_genes <- dataComp()
  # color palettes
  blues <- c("#0033CC", "#00CCFF", "#0066CC", "#3366FF", "#0099FF",
             "#6699CC", "#003399", "#66CCFF", "#6699FF", "#0033FF", "#0099CC")
  reds <- c("#FF6600", "#CC0000", "#FF3366", "#CC3300", "#CC3366",
            "#FF6666", "FF0000",  "#CC0033", "#FF3300", "#990000", "#CC3333")
  greens <- c("#666600", "#669900", "#999900", "#00CC00", "#009966",
              "#336600", "#66CC00", "#66CC66", "#009900", "#006600", "#99CC33")
  # counter
  nb_scell <- 0
  nb_proteome <- 0
  nb_transcriptome <- 0
  filling_col_levels <- c()
  
  full_data <- data.frame(Gene_name = character(),
                          percentiles = numeric(),
                          dataset = character(),
                          filling_col = character())
  
  for (f in 1:length(files)){
    df <- get(files[f])
    df <- df %>% 
      select(Gene_name, percentiles, dataset) %>%  
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(percentiles)) %>% 
      distinct(Gene_name, .keep_all = T)
    
    # Define the method (scell or proteome or transcriptome)
    if(substr(files[f], nchar(files[f])-4, nchar(files[f]))=="scell"){
        nb_scell <- nb_scell+1
        df <- mutate(df, filling_col = greens[nb_scell])
        filling_col_levels <- c(filling_col_levels, greens[nb_scell])
    }
    
    else if(substr(files[f], nchar(files[f])-7, nchar(files[f]))=="proteome"){
      nb_proteome <- nb_proteome+1
      df <- mutate(df, filling_col = reds[nb_proteome])
      filling_col_levels <- c(filling_col_levels, reds[nb_proteome])
    }
    
    else {
      nb_transcriptome <- nb_transcriptome+1
      df <- mutate(df, filling_col = blues[nb_transcriptome])
      filling_col_levels <- c(filling_col_levels, blues[nb_transcriptome])
      }
    full_data <- rbind(full_data, df)
  }
  # transform the dataset variable as factor and order the levels to group them by method
  full_data$dataset <- as.factor(full_data$dataset)
  full_data$dataset<- ordered(full_data$dataset, levels = files)
  full_data$filling_col <- as.factor(full_data$filling_col)
  full_data$filling_col <- ordered(full_data$filling_col, levels = filling_col_levels)
  
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
    scale_fill_manual(breaks = levels(full_data$dataset),
                      values =  levels(full_data$filling_col))+#to associate color and dataset
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