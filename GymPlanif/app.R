
###########################
####### GYM PLANIF' #######
###########################

library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(plotly)
library(gridExtra)
library(kableExtra)

# Define predefined lists
muscles <- c("Pectoraux", "Abdominaux", "Trapèzes", "Dorsaux", "Epaules", "Quadriceps", "Ischio-jambiers", "Fessiers", "Adducteurs", "Mollets", "Biceps", "Triceps")

############################
############ UI ############
############################


ui <- fluidPage(theme = shinytheme("cyborg"),
                shinythemes::themeSelector(),
  navbarPage("Gym Planif'",
             tabPanel("\ud83c\udfcb\ufe0f\u200d\u2640 Organisation",
                      sidebarPanel(
                        width = 4,
                        h4("Sélectionnez vos activités"),
                        uiOutput("exercise_selection")
                      ),
                      mainPanel(
                        width = 8,
                        h4("Semaine type d'entraînement"),
                        plotlyOutput("plan_summary")
                      )
             ),
             tabPanel("\ud83c\udfc6 Entraînement",
                      sidebarPanel(
                        sliderInput("seances_par_semaine", "Nombre de séances par semaine : ", min = 1, max = 7, value = 4),
                        uiOutput("seance_select"),
                        actionButton("ajouter_ligne", "Ajouter un exercice", icon = icon("plus")),
                        actionButton("supprimer_ligne", "Supprimer un exercice", icon = icon("minus")),
                        h4("Volume par groupe musculaire"),
                        plotlyOutput("set_pie_chart"),
                        
                      ),
                      mainPanel(
                        width = 8,
                        uiOutput("sous_onglets"),
                      )
             ),
             tabPanel("Récapitulatif",
                      mainPanel(
                        downloadButton("downloadPdf", "Sauvegardez votre semaine type en PDF"),
                        tableOutput("recap_table")
                      )
             )
  ),
  tags$footer(
    style = "text-align: right; font-size: 12px;",
    "Développé par ", tags$em("Lucie HUBERT"), " - ", format(Sys.Date(), "%Y")
  )
)


###########################
######### SERVER ##########
###########################

server <- function(input, output, session) {
  
  ##########################
  ######## ADHESION ########
  ##########################
  
  # Reactive value to store exercise selections
  exercise_choices <- reactiveValues()
  
  # Initialize default values for the 7 days of the week
  observe({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    for (day in jours) {
      if (is.null(exercise_choices[[day]])) {
        exercise_choices[[day]] <- "Repos"
      }
    }
  })
  
  # Observer to update exercise selections
  observe({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    lapply(jours, function(day) {
      observeEvent(input[[paste0("exercise_", day)]], {
        exercise_choices[[day]] <- input[[paste0("exercise_", day)]]
      })
    })
  })
  
  # Dynamically render exercise selectors in the UI
  output$exercise_selection <- renderUI({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    lapply(jours, function(day) {
      selectInput(paste0("exercise_", day), paste("Séance du", day, ":"),
                  choices = c("Musculation", "Course", "Natation", "Cyclisme", "Mobilité", "Yoga", "Repos"),
                  selected = exercise_choices[[day]])
    })
  })
  
  output$plan_summary <- renderPlotly({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    exercises_per_day <- sapply(jours, function(day) exercise_choices[[day]])
    data <- data.frame(Jour = factor(jours, levels = jours),
                       Exercices_day = exercises_per_day)
    
    p <- ggplot(data, aes(x = Jour, fill = Exercices_day)) +
      geom_bar() +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_x_discrete(labels = jours)
    
    ggplotly(p)
  })
  
  ##############################
  ######## ENTRAINEMENT ########
  ##############################
  
  # Initialize a reactive list to store exercise data
  exercices <- reactiveValues()
  
  # Function to retrieve values of existing exercises
  get_exercise_values <- function(seance) {
    lapply(seq_along(exercices[[seance]]), function(j) {
      list(
        mouvement = input[[paste0("mouvement_", seance, "_", j)]],
        muscle = input[[paste0("muscle_", seance, "_", j)]],
        series = input[[paste0("series_", seance, "_", j)]],
        repetitions_min = input[[paste0("repetitions_", seance, "_", j)]][1],
        repetitions_max = input[[paste0("repetitions_", seance, "_", j)]][2]
      )
    })
  }
  
  # Observer to add sessions
  observeEvent(input$seances_par_semaine, {
    nouvelles_seances <- input$seances_par_semaine
    
    # Initialize each session with an empty list
    for (i in seq_len(nouvelles_seances)) {
      if (is.null(exercices[[as.character(i)]])) {
        exercices[[as.character(i)]] <- list()
      }
    }
    
    # Remove excess sessions if the number of sessions is reduced
    if (nouvelles_seances < length(exercices)) {
      for (i in (nouvelles_seances + 1):length(exercices)) {
        exercices[[as.character(i)]] <- NULL
      }
    }
  })
  
  # Observer to add exercises
  observeEvent(input$ajouter_ligne, {
    seance <- as.character(input$seance_select)
    
    if (is.null(exercices[[seance]])) {
      exercices[[seance]] <- list()
    }
    
    # Capture existing values
    valeurs_exercices_existants <- get_exercise_values(seance)
    
    # Check if less than 7 exercises already exist
    if (length(valeurs_exercices_existants) < 7) {
      exercices[[seance]] <- c(valeurs_exercices_existants, list(
        list(
          mouvement = "",
          muscle = "",
          series = 3,
          repetitions_min = 6,
          repetitions_max = 8
        )
      ))
    } else {
      # Show notification if trying to add more than 7 exercises
      showNotification("Vous avez atteint le nombre maximal d'exercices !", type = "warning")
    }
  })
  
  # Observer to delete exercises
  observeEvent(input$supprimer_ligne, {
    seance <- as.character(input$seance_select)
    
    if (!is.null(exercices[[seance]])) {
      # Capture existing values
      valeurs_exercices_existants <- get_exercise_values(seance)
      
      # Remove the last exercise
      if (length(valeurs_exercices_existants) > 1) {
        exercices[[seance]] <- valeurs_exercices_existants[-length(valeurs_exercices_existants)]
      }
    }
  })
  
  # Generate the UI for the exercises of each session
  output$sous_onglets <- renderUI({
    if (!is.null(input$seances_par_semaine) && input$seances_par_semaine > 0) {
      tab_list <- lapply(seq_len(input$seances_par_semaine), function(i) {
        seance_number <- i  # Utiliser le numéro de la séance pour le nom de l'onglet
        tabPanel(paste("Séance", seance_number), value = paste0("sous_onglet_", seance_number),
                 uiOutput(paste0("exercices_ui_", seance_number))
        )
      })
      do.call(tabsetPanel, c(type = "tabs", tab_list))
    }
  })
  
  # Generate a list of tables, one for each session
  output$recap_table <- renderUI({
    seances <- get_seances()
    n_seances <- length(seances)
    n_exercices <- sapply(seances, length)
    
    # Initialize an empty list to store the tables for each session
    session_tables <- vector("list", n_seances)
    
    # Loop through each session and create a table for the exercises
    for (i in seq_len(n_seances)) {
      if (n_exercices[i] > 0) {
        session_table <- data.frame(
          Exercice = sapply(seances[[i]], function(exercice) exercice$mouvement),
          Muscle = sapply(seances[[i]], function(exercice) exercice$muscle),
          Séries = sapply(seances[[i]], function(exercice) exercice$series),
          Répétitions = sapply(seances[[i]], function(exercice) paste(exercice$repetitions[1], "-", exercice$repetitions[2]))
        )
        
        # Add a title for the table
        session_title <- h4(paste0("Séance ", i))
        
        # Store the table and title in the list
        session_tables[[i]] <- list(session_title, session_table)
      } else {
        # Create an empty table with the correct column names and 0 rows
        session_tables[[i]] <- list(
          h4(paste0("Séance ", i)),
          data.frame(
            Exercice = character(0),
            Muscle = character(0),
            Séries = numeric(0),
            Répétitions = character(0)
          )
        )
      }
    }
    # Generate the UI for the tables
    table_uis <- lapply(seq_len(n_seances), function(i) {
      table_ui <- tagList(
        session_tables[[i]][[1]],  # Add the title to the UI
        tableOutput(paste0("recap_table_", i))
      )
      if (n_exercices[i] > 0) {
        output[[paste0("recap_table_", i)]] <- renderTable({
          session_tables[[i]][[2]]  # Render the table without the session column
        })
      }
      table_ui
    })
    
    # Split the tables into pairs
    table_pairs <- split(table_uis, ceiling(seq_along(table_uis) / 2))
    
    # Generate the UI for the pairs of tables
    table_pair_uis <- lapply(table_pairs, function(table_pair) {
      fluidRow(
        column(6, table_pair[[1]]),
        column(6, table_pair[[2]])
      )
    })
    
    # Return the UI for the pairs of tables
    return(tagList(table_pair_uis))
  })

  # Selection of the session
  output$seance_select <- renderUI({
    if (!is.null(input$seances_par_semaine) && is.numeric(input$seances_par_semaine) && input$seances_par_semaine >= 1) {
      choices <- as.character(seq_len(input$seances_par_semaine))
      selectInput("seance_select", "Choisir une séance à éditer :", choices = choices)
    }
  })
  
  # Generate the UI for the exercises of each session
  observe({
    req(input$seances_par_semaine)
    seances <- input$seances_par_semaine
    
    lapply(seq_len(seances), function(i) {
      output[[paste0("exercices_ui_", i)]] <- renderUI({
        seance <- as.character(i)
        
        if (!is.null(exercices[[seance]])) {
          lapply(seq_along(exercices[[seance]]), function(j) {
            fluidRow(
              column(3,
                     textInput(paste0("mouvement_", seance, "_", j), "Mouvement", value = exercices[[seance]][[j]]$mouvement)
              ),
              column(3,
                     selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé", choices = muscles, selected = exercices[[seance]][[j]]$muscle)
              ),
              column(1,
                     numericInput(paste0("series_", seance, "_", j), "Séries", value = exercices[[seance]][[j]]$series, min = 1, max = 10)
              ),
              column(5,
                     sliderInput(paste0("repetitions_", seance, "_", j), "Répetitions", min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max))
              )
            )
          })
        }
      })
    })
  })
  
  # Observer to aggregate data
  observe({
    if (!is.null(input$seances_par_semaine) && input$seances_par_semaine > 0) {
      series_data <- list()
      for (seance in seq_len(input$seances_par_semaine)) {
        seance_key <- as.character(seance)
        if (!is.null(exercices[[seance_key]])) {
          for (j in seq_along(exercices[[seance_key]])) {
            muscle_input <- paste0("muscle_", seance, "_", j)
            series_input <- paste0("series_", seance, "_", j)
            
            muscle <- input[[muscle_input]]
            series <- input[[series_input]]
            
            if (!is.null(muscle) && !is.na(muscle) && !is.null(series) && !is.na(series)) {
              if (is.null(series_data[[muscle]])) {
                series_data[[muscle]] <- data.frame(muscle = muscle, series = series)
              } else {
                series_data[[muscle]] <- rbind(series_data[[muscle]], data.frame(muscle = muscle, series = series))
              }
            }
          }
        }
      }
      
      if (length(series_data) > 0) {
        series_df <- do.call(rbind, series_data)
        series_summary <- aggregate(series_df$series, by = list(Muscle = series_df$muscle), FUN = sum)
        names(series_summary) <- c("Muscle", "Series")
        
        # Store the aggregated data in exercices$series_summary_par_semaine
        exercices$series_summary_par_semaine <- series_summary
      } else {
        # Reset the aggregated data if there are no valid data
        exercices$series_summary_par_semaine <- NULL
      }
    } else {
      # Reset the aggregated data if input$seances_par_semaine is NULL or <= 0
      exercices$series_summary_par_semaine <- NULL
    }
  })
  
  # Generate the pie chart of sets per muscle
  output$set_pie_chart <- renderPlotly({
    if (!is.null(exercices$series_summary_par_semaine) && nrow(exercices$series_summary_par_semaine) > 0) {
      plot_ly(exercices$series_summary_par_semaine, labels = ~Muscle, values = ~Series, type = 'pie')
    } else {
      plot_ly() %>%
        layout(title = "Aucune donnée disponible")
    }
  })
  
  # Récupérer le contenu des séances
  get_seances <- reactive({
    seances <- list()
    for (i in seq_len(input$seances_par_semaine)) {
      seance <- list()
      j <- 1
      repeat {
        mouvement_input <- paste0("mouvement_", i, "_", j)
        muscle_input <- paste0("muscle_", i, "_", j)
        series_input <- paste0("series_", i, "_", j)
        repetitions_input <- paste0("repetitions_", i, "_", j)
        if (!is.null(input[[mouvement_input]])) {
          seance[[j]] <- list(
            mouvement = input[[mouvement_input]],
            muscle = input[[muscle_input]],
            series = input[[series_input]],
            repetitions = c(input[[repetitions_input]][1], input[[repetitions_input]][2])
          )
          j <- j + 1
        } else {
          break
        }
      }
      seances[[i]] <- seance
    }
    return(seances)
  })
  
  # Download PDF
  output$downloadPdf <- downloadHandler(
    filename = function() {
      paste("Programme_", input$client_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf", sep = "")
    },
    content = function(file) {
      
      # Create a temporary R Markdown file
      tempReport <- file.path(tempdir(), "rapport_seances.Rmd")
      file.copy("rapport_seances.Rmd", tempReport, overwrite = TRUE)
      
      # Convert reactiveValues to lists
      exercices_list <- reactiveValuesToList(exercices)
      exercise_choices_list <- reactiveValuesToList(exercise_choices)
      exercises_data <- get_seances()
      
      # Prepare the parameters for rendering the report
      params_render <- list(
        exercices = exercices_list,
        exercise_choices = exercise_choices_list,
        series_summary_par_semaine = exercices$series_summary_par_semaine,
        seances = exercises_data
      )
      
      # Render the report using the specified parameters
      tryCatch({
        rmarkdown::render(input = tempReport, output_file = file, params = params_render)
      }, error = function(e) {
        # Show an error message and return
        showNotification(paste("Erreur lors de la génération du PDF :", e), type = "error")
        return()
      })
      
      # Show a notification after successful PDF generation
      showNotification("Le téléchargement a bien été effectué !", type = "message")
      
      # Show a warning if a session does not contain exercises
      for (seance_number in seq_len(input$seances_par_semaine)) {
        seance_key <- as.character(seance_number)
        if (length(exercices_list[[seance_key]]) == 0 || is.null(exercices_list[[seance_key]])) {
          showNotification(paste("La séance", seance_number, "ne contient pas d'exercices !"), type = "warning")
        }
      }
    }
  )
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
