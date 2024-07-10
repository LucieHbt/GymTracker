
###########################
####### GYM PLANIF' #######
###########################

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(bslib)
library(bsicons)
library(plotly)
library(gridExtra)
library(kableExtra)
library(slickR)
library(bs4Dash)
library(waiter)
library(fresh)

# Define predefined lists
muscles <- c("Pectoraux", "Abdominaux", "Trapèzes", "Dorsaux", "Epaules", "Quadriceps", "Ischio-jambiers", "Fessiers", "Adducteurs", "Mollets", "Biceps", "Triceps")

my_contrast <- create_theme(bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30"))

############################
############ UI ############
############################

ui <- dashboardPage(footer = NULL, help = NULL, dark = TRUE,
  preloader = list(html = tagList(spin_3(), "Chargement ..."), color = "black"),
  skin = my_contrast,
  header = dashboardHeader(
    rightUi = tagList(
      userOutput("user")
    )
  ),
  
  sidebar = dashboardSidebar(
    collapsed = TRUE,
    sidebarUserPanel(
      image = "brand - Copie.png",
      name = "Gym Planif' !"
    ),
    
    sidebarMenu(
      menuItem(
        text = HTML("\U0001F3CB\U0000FE0F\u200D\u2640 Organisation"),
        tabName = "org",
        icon = icon("circle")
      ),
      menuItem(
        text = HTML("\U0001F3C6 Entraînement"),
        tabName = "train",
        icon = icon("circle")
      ),
      menuItem(
        text = HTML("\U0001F4DD Récapitulatif"),
        tabName = "recap",
        icon = icon("circle")
      )
    )
  ),
  
  body = dashboardBody(
    chooseSliderSkin("Round"),
    includeCSS("style.css"),
    tabItems(
      # Tab 1: Organisation
      tabItem(tabName = "org",
              box(
                width = 12,
                status = "primary",
                title = "Semaine type d'entraînement",
                p("Cliquez sur l'engrenage pour selectionner vos activités"),
                sidebar = boxSidebar(width = 25,
                                     id = "mySidebar",
                                     uiOutput("exercise_selection")
                ),
                plotlyOutput("plan_summary")
              )
      ),
      
      # Tab 2: Entraînement
      tabItem(tabName = "train",
              fluidRow(
                column(
                  width = 8,
                  uiOutput("sous_onglets")
                ),
                column(
                  width = 4,
                  box(
                    width = 12,
                    title = "Organisation des séances",
                    status = "primary",
                    sliderInput("seances_par_semaine", "Nombre de séances par semaine : ", min = 1, max = 7, value = 4),
                    uiOutput("seance_select"),
                    actionButton("ajouter_ligne", "Ajouter un exercice", icon = icon("plus")),
                    actionButton("supprimer_ligne", "Supprimer un exercice", icon = icon("minus")),
                    plotlyOutput("set_pie_chart")
                  )
                )
              )
      ),
      
      # Tab 3: Récapitulatif
      tabItem(tabName = "recap",
              fluidRow(
                column(
                  width = 12,
                  downloadButton("downloadPdf", label = span("Sauvegarder !")),
                  box(
                    width = 12,
                    title = "Vue globale de vos séances",
                    status = "primary",
                    uiOutput("recap_table")
                  )
                )
              )
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    collapsed = FALSE,
    div(class = "p-3", skinSelector()), 
    pinned = FALSE
  )
)

###########################
######### SERVER ##########
###########################

server <- function(input, output, session) {
  
  output$user <- renderUser({
    dashboardUser(
      name = "Lucie HUBERT",
      image = "https://media.licdn.com/dms/image/D4E03AQErycNPVoJRhw/profile-displayphoto-shrink_800_800/0/1666273099421?e=1726099200&v=beta&t=EierabsopiNJWADMmh9YLrOzBQyShQ_6GGZucBfJ4RU",
      title = "Etudiante",
      fluidRow(
        column(
          width = 12,
          align = "center",
          tags$a(href = "www.linkedin.com/in/lucie-hubert-74490b233", 
                 HTML('<i class="fab fa-linkedin fa-2x"></i>'), 
                 style = "color: #0077B5; text-decoration: none; margin-left: 10px;")
          ),
          )
      )
    })
  
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
                  choices = c("PUSH", "PULL", "LEGS", "UPPER", "LOWER", "RUN", "REST"),
                  selected = exercise_choices[[day]])
    })
  })
  
  output$plan_summary <- renderPlotly({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    exercises_per_day <- sapply(jours, function(day) exercise_choices[[day]])
    data <- data.frame(Jour = factor(jours, levels = jours),
                       Exercices_day = exercises_per_day)
    
    p <- ggplot(data, aes(x = Jour, fill = Exercices_day)) +
      geom_bar(stat = "count") +  # Utilisation de stat = "count" pour compter les occurrences
      theme_minimal() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(color = "#007bff")) +  # Couleur du texte sur l'axe des abscisses
      scale_x_discrete(labels = jours)
    
    # Convertir le ggplot en plotly
    ggplotly(p) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",  # Couleur de l'arrière-plan transparente
        plot_bgcolor = "rgba(0,0,0,0)"    # Couleur de la zone de traçage transparente
      )
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
  
  # Selection of the session
  output$seance_select <- renderUI({
    if (!is.null(input$seances_par_semaine) && is.numeric(input$seances_par_semaine) && input$seances_par_semaine >= 1) {
      choices <- as.character(seq_len(input$seances_par_semaine))
      selectInput("seance_select", "Choisir une séance à éditer :", choices = choices)
    }
  })
  
  # Observer to show notification when a session is selected
  observeEvent(input$seance_select, {
    showNotification(paste("Vous avez sélectionné la séance", input$seance_select), type = "warning")
  })
  
  # Liste de couleurs pour les accordions
  couleurs_accordions <- c("primary", "success", "info", "warning", "danger", "purple", "orange")
  
  # Generate UI for each session's exercises
  output$sous_onglets <- renderUI({
    if (!is.null(input$seances_par_semaine) && input$seances_par_semaine > 0) {
      lapply(seq_len(input$seances_par_semaine), function(i) {
        seance_number <- i
        
        accordion(
          id = paste0("accordion_seance_", seance_number),
          accordionItem(
            title = paste("Séance", seance_number),
            status = sample(couleurs_accordions, 1),  # Couleur aléatoire
            collapsed = TRUE,
            uiOutput(paste0("exercices_ui_", seance_number))
          )
        )
      })
    }
  })
  
  # Observer to update exercise UI for each session
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
              column(2,
                     selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé", choices = muscles, selected = exercices[[seance]][[j]]$muscle)
              ),
              column(2,
                     numericInput(paste0("series_", seance, "_", j), "Séries", value = exercices[[seance]][[j]]$series, min = 1, max = 10)
              ),
              column(5,
                     {
                       repetitions_min <- exercices[[seance]][[j]]$repetitions_min
                       repetitions_max <- exercices[[seance]][[j]]$repetitions_max
                       # Utiliser des valeurs par défaut si les valeurs sont NULL
                       if (is.null(repetitions_min)) repetitions_min <- 6
                       if (is.null(repetitions_max)) repetitions_max <- 8
                       sliderInput(paste0("repetitions_", seance, "_", j), "Répetitions", min = 1, max = 20, value = c(repetitions_min, repetitions_max))
                     }
              )
            )
          })
        }
      })
    })
  })
  
  # Dummy data for exercices reactiveValues
  observe({
    seances <- input$seances_par_semaine
    
    lapply(seq_len(seances), function(i) {
      seance <- as.character(i)
      exercices[[seance]] <- replicate(1, list(
        mouvement = paste("Mouvement", i),
        muscle = sample(muscles, 1),
        series = sample(1:10, 1),
        repetitions_min = sample(1:10, 1),
        repetitions_max = sample(11:20, 1)
      ), simplify = FALSE)
    })
  })
  
  # Generate the pie chart of sets per muscle
  output$set_pie_chart <- renderPlotly({
    if (!is.null(exercices$series_summary_par_semaine) && nrow(exercices$series_summary_par_semaine) > 0) {
      plot_ly(exercices$series_summary_par_semaine, labels = ~Muscle, values = ~Series, type = 'pie') %>%
        layout(
          showlegend = FALSE,  # Remove legend
          paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
          plot_bgcolor = "rgba(0,0,0,0)"    # Transparent plot area background
        )
    } else {
      plot_ly() %>%
        layout(
          title = "Aucune donnée disponible",
          paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
          plot_bgcolor = "rgba(0,0,0,0)"    # Transparent plot area background
        )
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
      if (length(table_pair) == 2) {
        fluidRow(
          column(6, table_pair[[1]]),
          column(6, table_pair[[2]])
        )
      } else {
        fluidRow(
          column(6, table_pair[[1]])
        )
      }
    })
    
    # Return the UI for the pairs of tables
    return(tagList(table_pair_uis))
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
  
  # Download PDF
  output$downloadPdf <- downloadHandler(
    filename = function() {
      paste("Programme_", input$client_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf", sep = "")
    },
    content = function(file) {
      # Création d'un fichier temporaire R Markdown
      tempReport <- file.path(tempdir(), "rapport_seances.Rmd")
      file.copy("rapport_seances.Rmd", tempReport, overwrite = TRUE)
      
      # Conversion des reactiveValues en listes
      exercices_list <- reactiveValuesToList(exercices)
      exercise_choices_list <- reactiveValuesToList(exercise_choices)
      exercises_data <- get_seances()
      
      # Préparation des paramètres pour la génération du rapport
      params_render <- list(
        exercices = exercices_list,
        exercise_choices = exercise_choices_list,
        series_summary_par_semaine = exercices$series_summary_par_semaine,
        seances = exercises_data
      )
      
      # Génération du rapport
      tryCatch({
        rmarkdown::render(input = tempReport, output_file = file, params = params_render)
      }, error = function(e) {
        showNotification(paste("Erreur lors de la génération du PDF :", e), type = "error")
        return()
      })
      
      showNotification("Le téléchargement a bien été effectué !", type = "message")
      
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
