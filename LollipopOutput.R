###################### Code for lollipop output ##########################

# Select data
data_lollipop <- reactive({
  # selected genes
  selected_genes <- dataComp()
  
  Boerries_proteome_filtered <- Boerries_proteome %>% 
    select(Gene_name, normalized_fold) %>% 
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset = "Boerries_p", method = "Proteome")
  
  Boerries_transcriptome_filtered <- Boerries_transcriptome %>% 
    select(Gene_name, normalized_fold) %>% 
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset="Boerries_t", method = "Transcriptome")
  
  Chung_filtered <- Chung_scell %>% 
    select(Gene_name, normalized_fold) %>%
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset = "Chung", method = "Single cell")
  
  CMS_filtered <- CMS_proteome %>% 
    select(Gene_name, normalized_fold) %>%
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset="CMS", method = "Proteome")
  
  # No relative quantification for Kann transcriptome
  
  Karaiskos_filtered <- Karaiskos_scell %>% 
    select(Gene_name, normalized_fold) %>% 
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset="Karaiskos", method = "Single cell")
  
  Park_filtered <- Park_scell %>% 
    select(Gene_name, normalized_fold) %>% 
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset="Park", method = "Single cell")
  
  Rinschen_filtered <- Rinschen_proteome%>% 
    select(Gene_name, normalized_fold) %>% 
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(abs(normalized_fold))) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset="Rinschen", method = "Proteome")
  
  RG_filtered <- Rinschen_Goedel_transcriptome %>% 
    select(Gene_name, normalized_fold) %>% 
    filter(Gene_name %in% selected_genes)%>% 
    arrange(desc(normalized_fold)) %>% 
    distinct(Gene_name, .keep_all = T) %>% 
    mutate(dataset="Rinschen Goedel", method = "Transcriptome")
  
  full_data <- rbind(Boerries_proteome_filtered, Rinschen_filtered)
  full_data <- rbind(full_data, CMS_filtered)
  full_data <- rbind(full_data, Boerries_transcriptome_filtered)
  full_data <- rbind(full_data, RG_filtered)
  full_data <- rbind(full_data, Chung_filtered)
  full_data <- rbind(full_data, Karaiskos_filtered)
  full_data <- rbind(full_data, Park_filtered)
  
  
  # Transform the dataset variable as factor and order the levels  
  full_data$dataset<- as.factor(full_data$dataset)
  full_data$dataset<- ordered(full_data$dataset, levels = c("CMS",
                                                            "Rinschen",
                                                            "Boerries_p",
                                                            "Rinschen Goedel",
                                                            "Boerries_t",
                                                            "Park",
                                                            "Karaiskos",
                                                            "Chung"
  ) )
  # Transform the method variable as factor
  full_data$method<- as.factor(full_data$method)
  
  full_data
})

# Zoom button
observeEvent((input$pl), {
  if(length(dataComp())!=0){
    if(nrow(data_lollipop())!=0){
      showModal(modalDialog(
        renderPlot({
          plot_lollipop() + theme(
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
plot_lollipop <- reactive({
  full_data <- data_lollipop()
  ggplot(full_data, aes(x = dataset, y = normalized_fold, color = method)) +
    geom_segment(aes(x = dataset, xend = dataset, y = 0, yend = normalized_fold),
                 color = "gray", lwd = 1) +
    geom_hline(yintercept = 0)+
    facet_wrap(~Gene_name, ncol = 3)+
    geom_point(size = 3, pch = 19, bg = 2) + #18
    coord_flip() +
    theme_bw()+
    ylim(c(-1,1))+
    ylab("Normalized fold change")+
    xlab("")+
    labs(color = "")+
    NULL
})

# Display the plot
output$lollipop <- renderPlotly({
  if(length(dataComp())!=0){
    if(nrow(data_lollipop())!=0){
      p <- plot_lollipop()
      ggplotly(p,
               height = 200+(length(dataComp())*40)) %>% 
        config(
          modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
          modeBarButtonsToAdd = list(dl_button)
        )
    }
  }
})