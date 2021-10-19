################### Code for text outputs ####################

# Common text for the three plots of Compare Datasets
texta <- reactive({
  missing_genes <- setdiff(dataComp1(), dataComp2()$Gene_name)
  if(length(dataComp1())==0){
    texta <- "You must select at least one gene"
  }
  else if(length(missing_genes)>0){
    missing <- paste(missing_genes, collapse = ", ")
    texta <- paste("The following genes are not written correctly : ", missing,
                   "Please make sure you wrote them using the human gene symbol. 
      If you do not know it, try finding it in the gene list.")
  }
  else {texta <- ""}
  texta
})

### Text for the heatmap ###
output$heatmapText <- renderText({
  # Selected genes that exist but are not found in our datasets 
  not_found <- setdiff(dataComp(), unique(data_radar()$Gene_name) )
  # If less than 2 selected genes, heatmap not displayed
  if(length(dataComp1())<=1){
    texta <- "There are less than 2 selected genes. Select at least 2 genes to display the heatmap."
  }
  # We can display a heatmap and need texta()
  else {
    texta <- texta()
  }
  
  # text for the not_found genes : send a warning 
  if (length(not_found)>0){
    missing <- paste(not_found, collapse = ", ")
    textb <- paste("No data has been found in the datasets for the following genes even though they are
                     written correctly : ", missing)
  }
  else{ textb <- ""}
  
  #If after filtering less than 2 genes are found, heatmap cannot be displayed
  if(length(unique(data_radar()$Gene_name))<2 & length(dataComp1())>=2){
    textc <- "Less than 2 genes have been found so the heatmap cannot be displayed."
  }
  else{textc <- ""}
  
  #Text to be returned
  HTML(paste(texta, textb, textc, sep = '<br/><br/>'))  
})

### Text barcode ###
output$barcodeText <- renderText({
  texta <- texta()
  # Selected genes that exist but are not found in our datasets => send a warning
  not_found <- setdiff(dataComp(), unique(data_barcode()$Gene_name) )
  if (length(not_found)>0){
    missing <- paste(not_found, collapse = ", ")
    textb <- paste("No data has been found in the datasets for the following genes even though they are
                     written correctly : ", missing)
  }
  else{ textb <- ""}
  # Text to be returned
  HTML(paste(texta, textb, sep = '<br/><br/>'))
})

### Text lollipop ###
output$lollipopText <- renderUI({
  texta <- texta()
  # Selected genes that exist but are not found in our datasets => send a warning
  not_found <- setdiff(dataComp(), unique(data_lollipop()$Gene_name) )
  if (length(not_found)>0){
    missing <- paste(not_found, collapse = ", ")
    textb <- paste("No data has been found in the datasets for the following genes even though they are
                     written correctly : ", missing)
  }
  else{ textb <- ""}
  #Text to be returned
  HTML(paste(texta, textb, sep = '<br/><br/>'))
})