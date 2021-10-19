source("global.R")

fluidRow(
  column(4,
         style = "background-color: #E8E8E8",
         
         ##put input boxes here
         div(style="display: inline-block;vertical-align:top; width: 200px;",
             strong("Select keyword"), 
             selectizeInput(
               inputId = "keyword",
               label = "",
               choices = list_keywords,
               selected = list_keywords[1],
               multiple = TRUE
             )),
         div(style="display: inline-block;vertical-align:top; width: 200px;",
             strong("Select File"), 
             selectizeInput(
               inputId = "selectedFile",
               label = "",
               choices = files,
               selected = files[1],
               multiple = TRUE
             )),
         br(),
         actionBttn('goEnrich', 'GO', color = "primary", style = "fill", size = "sm")
  ),
  
  # output
  
  column(8,
         tabPanel("", uiOutput("gseaMultiple") ,
            div(
           style = "position: absolute; right: 0.5em; bottom: 0.5em;",
           actionBttn(
             inputId = "gsea",
             icon = icon("search-plus", class = "opt"),
             style = "fill",
             color = "danger",
             size = "xs"
           )
         ))
)

)
