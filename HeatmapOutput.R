############# Code for heatmap output ##################

# Select data
data_radar <- reactive({
  # selected genes
  selected_genes <- dataComp()
  # empty file
  full_data <- data.frame(Gene_name = character())
  
  for(i in 1:length(files_fold_change)){
    data_i <- get(files_fold_change[i])
    data_i_filtered <- data_i %>% 
      select(Gene_name, normalized_fold) %>% 
      #rename(Rinschen_proteome = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    #names(data_i_filtered)[names(data_i_filtered) == 'normalized_fold'] <- files_fold_change[i]
    setnames(data_i_filtered, "normalized_fold", files_fold_change[i])
    print(colnames(data_i_filtered))
    full_data <- merge(full_data, data_i_filtered, "Gene_name", all = T)
      
  }
  full_data <- arrange(full_data, Gene_name)
  full_data
})

# Zoom button
observeEvent((input$ph), {
  if(length(dataComp())!=0){
    if(nrow(data_radar())>=2){
      showModal(modalDialog(
        renderPlotly({
          plot_heatmap()
        }),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    }
  }
})

# Make the heatmap
plot_heatmap <- reactive({
  if(length(dataComp())>=2){
    full_data <- data_radar()
    # Function to deal with missing values
    dist_no_na <- function(mat1) {
      mat <- mat1
      for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat)){
          if (is.na(mat[i,j])){
            mat[i,j]<- 100 # we arbitraly decide to assign 100 because it is much higher than every existed number
          }
        }
      }
      edist <- dist(mat)
      return(edist)
    }
    
    heatmaply(full_data[,-1],
              distfun = dist_no_na,
              dendrogram = "row",
              ylab = "",
              xlab = "",
              main = "",
              #scale = "column",
              margins = c(30,100,0,0),
              #grid_color = "white",
              grid_width = 0.00001,
              #limits = c(-2,2),
              scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                low = "blue", 
                high = "red", 
                mid = 'grey92',
                midpoint = 0,
                limits = c(min(full_data[,-1], na.rm = TRUE) , max(full_data[,-1], na.rm = TRUE) )
              ),
              titleX = FALSE,
              hide_colorbar = FALSE,
              branches_lwd = 0.1,
              labRow = full_data[,1],
              labCol = colnames(full_data[,-1]),
              heatmap_layers = theme(axis.line=element_blank()))
  }
})

#Display heatmap
output$heatmap <- renderPlotly({
  if(length(dataComp())>=2){ # At least 2 genes are needed to display the heatmap
    if(nrow(data_radar())>=2){ # At least 2 genes are needed to display the heatmap
      p <- plot_heatmap()
      ggplotly(p) %>% 
        layout(height = 400+(length(dataComp())*5)) %>% 
        config(
          modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
          modeBarButtonsToAdd = list(dl_button)
        )
    }
  }
})