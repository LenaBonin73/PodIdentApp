########### Code for downloading original datasets

# Display first ten row of the selected dataset
output$firstrows <- renderDataTable({
  data <- get(input$datasetD)
  datatable(
    data,
    caption = "First ten rows of the dataset",
    options = list(
      dom = '<"top">rt',
      scrollX=TRUE, 
      scrollCollapse=TRUE))
})

# Download one file
output$goDownload <- downloadHandler(
  filename = function() {
    paste(input$datasetD, ".csv", sep = "")
  },
  content = function(file) {
    data <- get(input$datasetD)
    write.csv(data, file, row.names = FALSE)
  }
)

# Download all files in a zip
output$downloadALL <- downloadHandler(
  filename = function(){
    "origninal_datasets.zip"
  },
  content = function(file){
    #go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    filesIn <- c()
    
    AllData <- files
    #loop through the sheets
    for (i in 1:length(AllData)){
      #write each sheet to a csv file, save the name
      fileName <- paste(AllData[i],".csv",sep = "")
      data<- get(AllData[i])
      write.csv(data, fileName, row.names = FALSE)
      filesIn <- c(filesIn, fileName)
    }
    #create the zip file
    zip(file,filesIn)
  },
  contentType = "application/zip"
)
