library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(rmarkdown)
library(tidyr)

app_dir <- getwd()

source("plot.R")

# Charger les données
test_new <- read_excel(paste(app_dir,"/Suivi Hbmass_FFTri_V2.xlsx", sep=""), 
                                    col_types = c("text", "numeric", "text", 
                                                  "text", "text", "text", "text", "date", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric"))

# Liste des athlètes uniques
athletes <- unique(test_new$id_athlete)

# Liste des 11 variables pour les graphiques (à adapter selon tes colonnes)
graph_vars <- colnames(test_new)
graph_vars <- graph_vars[-c(1:8)]



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
      }
      .sidebar {
        position: fixed;
        top: 0;
        left: 0;
        height: 100vh;
        width: 400px;
        overflow-y: auto;
        background-color: white;
        z-index: 100;
        padding: 20px;
        box-shadow: 2px 0 5px rgba(0,0,0,0.1);
      }
      .main-content {
        margin-left: 450px;
        padding: 0px;
      }
    "))
  ),
  div(class = "sidebar",
      selectInput("athlete", "Sélectionner un athlète :", choices = athletes, selected = athletes[1]),
      lapply(graph_vars, function(var) {
        checkboxInput(paste0("include_", var), paste("Inclure", var, "dans le rapport"), TRUE)
      }),
      lapply(graph_vars, function(var) {
        textAreaInput(paste0("comment_", var), paste("Commentaire pour", var), "", height = 80)
      }),
      textAreaInput("conclusion", "Conclusion générale :", "", height = 150),
      downloadButton("export", "Exporter en PDF")
  ),
  div(class = "main-content",
      lapply(graph_vars, function(var) {
        tagList(
          plotOutput(paste0(var, "_plot")),
          verbatimTextOutput(paste0(var, "_comment"))
        )
      }),
      h3("Conclusion générale"),
      verbatimTextOutput("conclusion_display")
  )
)


server <- function(input, output, session) {
  # Filtrer les données selon l'athlète sélectionné
  filtered_data <- reactive({
    test_new %>%
      filter(id_athlete == input$athlete) %>%
      mutate(date = as.Date(date)) %>% 
      mutate(iter = row_number()) %>% 
      group_by(stage_ref) 
  })
  
  # Générer dynamiquement les graphiques et les sorties de commentaires
  for (var in graph_vars) {
    local({
      current_var <- var
      # Graphique
      output[[paste0(current_var, "_plot")]] <- renderPlot({
        generate_plot(filtered_data(), current_var)
      })
      # Commentaire
      output[[paste0(current_var, "_comment")]] <- renderPrint({
        cat("Commentaire :\n", input[[paste0("comment_", current_var)]])
      })
    })
  }
  
  output$conclusion_display <- renderPrint({
    cat("Conclusion :\n", input$conclusion)
  })
  
  # Générer le rapport PDF
  output$export <- downloadHandler(
    filename = function() {
      paste("Rapport_", input$athlete,"_",Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Sauvegarder les données filtrées
      data_to_save <- filtered_data()
      tempData <- tempfile(fileext = ".rda")
      save(data_to_save, file = tempData)
      
      image_path <- paste(app_dir,"/photo/logo.png", sep="")  
      tempImage <- file.path(tempdir(), "header_image.png")
      file.copy(image_path, tempImage, overwrite = TRUE)
      
      # Créer le fichier Rmd
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Contenu de base du rapport avec en-tête personnalisé
      rmd_content <- c(
        "---",
        "title: \"Rapport des données\"",
        "date: \"`r Sys.Date()`\"",
        "output:",
        "  pdf_document:",
        "    includes:",
        "      in_header: header.tex",  # Fichier LaTeX pour l'en-tête
        "---",
        "",
        "```{r setup, include=FALSE}",
        "library(knitr)",
        "opts_chunk$set(echo = FALSE)",
        "library(ggplot2)",
        "library(dplyr)",
        sprintf("load('%s')", tempData),
        "data <- data_to_save",
        "```",
        ""
      )
      
      # Créer un fichier LaTeX pour l'en-tête
      header_tex <- file.path(tempdir(), "header.tex")
      writeLines(
        c(
          "\\usepackage{fancyhdr}",
          "\\usepackage{graphicx}",
          "\\fancyhf{}",
          "\\rhead{\\includegraphics[width=4cm]{header_image.png}}",
          "\\renewcommand{\\headrulewidth}{0.4pt}", 
          "\\fancypagestyle{plain}{",
          "\\fancyhf{}",
          "\\rhead{\\includegraphics[width=4cm]{header_image.png}}",
          "\\renewcommand{\\headrulewidth}{0.4pt}",
          "}",
          "\\pagestyle{fancy}",
          "\\setlength{\\headheight}{40pt}",
          "\\setlength{\\headsep}{30pt}"
          
        ),
        header_tex
      )
      
      rmd_content <- c(
        rmd_content,
        "```{r}",
        "```",
        paste0("### Athlète : ", input$athlete),
        ""
      )
      
      # Ajouter chaque graphique et commentaire si sélectionné
      for (var in graph_vars) {
        if (input[[paste0("include_", var)]]) {
          rmd_content <- c(
            rmd_content,
            "```{r}",
            sprintf("print(generate_plot(data, '%s'))", var),
            "```",
            "",
            "### Commentaire : ",
            input[[paste0("comment_", var)]],
            ""
          )
        }
      }
      
      rmd_content <- c(
        rmd_content,
        "## Conclusion générale",
        input$conclusion,
        ""
      )
      
      writeLines(rmd_content, tempReport)
      
      # Générer le PDF
      tryCatch({
        rmarkdown::render(tempReport, output_file = file)
      }, error = function(e) {
        message("Erreur lors de la génération du PDF : ", e$message)
      })
      
      # Nettoyer
      unlink(tempData)
      unlink(tempReport)
      unlink(header_tex)
      unlink(tempImage)
    }
  )
  
}

shinyApp(ui, server)



