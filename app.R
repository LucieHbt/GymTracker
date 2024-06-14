
library(shiny)
library(shinythemes)
library(shinyBS)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(colourpicker)
library(corrplot)
library(reshape2)
library(tinytex)
library(kableExtra)

data <- read.csv(
  file = "Table_Ciqual_2020_FR_2020_07_07.csv",
  header = TRUE,
  sep = ";",
  fileEncoding = "latin1"
)

# Définir les listes prédéfinies
mouvements <- c("Muscle up", "Squat", "Pull Up", "Dips", "Bench", "Deadlift", "Renfo")
muscles <- c("Pectoraux","Abdominaux", "Trapèzes", "Dorsaux", "Epaules", "Quadriceps", "Ischio-jambiers", "Fessiers", "Mollets", "Bras")
activity_levels <- c("Sédentaire", "Activité légère", "Activité modérée", "Activité intense", "Activité très intense")

ui <- fluidPage(theme = shinytheme("cyborg"),
                shinythemes::themeSelector(),
                navbarPage("Gym Tracker",
                           
                           tabPanel("	\ud83c\udfaf Présentation",
                                    mainPanel(width = 4,
                                      h4("Bienvenue dans Gym Tracker"),
                                      p("Cette application vous aide à planifier vos séances d'entraînement et à suivre un plan nutritionnel."),
                                      h4("Fonctionnalités"),
                                      tags$ul(
                                        tags$li("Planifiez vos séances d'entraînement hebdomadaires."),
                                        tags$li("Ajoutez et personnalisez vos exercices."),
                                        tags$li("Calculez vos performances théoriques."),
                                        tags$li("Informez-vous sur la répartition du volume d'entraînement."),
                                        tags$li("Obtenez des conseils sur la selection des exercices."),
                                        tags$li("Prenez connaissance des bases de la nutrition sportive.")
                                      ),
                                      h4("Instructions"),
                                      p("Utilisez les onglets en haut pour naviguer à travers les différentes fonctionnalités de l'application."),
                                      tags$ul(
                                        tags$li("Entraînement : planifiez et éditez vos séances."),
                                        tags$li("Pyramide de l'entraînement : comprenez comment structurer vos séances selon vos objectifs."),
                                        tags$li("Pyramide de la nutrition : obtenez les bases de la nutrition sportive."),
                                        tags$li("Sources : consultez les références utilisées.")
                                      )
                                    ),
                                    sidebarPanel(width = 8,
                                      tags$figure(
                                        img(src = "PyramidesHelms.jpg", height = "auto", width = "100%"),
                                        tags$figcaption("Crédit image : The Muscle and Strength Pyramids: nutrition and training. (2023). https://muscleandstrengthpyramids.com/"))
                                    )
                           ),
    
    tabPanel("	\ud83c\udfc6 Entraînement",
             sidebarPanel(
               h4("Modulez votre semaine type d'entraînement"),
               sliderInput("seances_par_semaine", "Nombre de séances par semaine:", min = 1, max = 7, value = 3),
               uiOutput("seance_select"),
               actionButton("ajouter_ligne", "Ajouter un exercice", icon = icon("plus")),
               actionButton("supprimer_ligne", "Supprimer un exercice", icon = icon("minus")),
               plotlyOutput("set_pie_chart"),
               textInput("client_name", label = "Nom du pratiquant", value = "PrénomNom"),
               downloadButton("downloadPdf", "Télécharger le PDF")
             ),
             mainPanel(width = 8,
               titlePanel("Editez vos séances"),
               uiOutput("sous_onglets")
               )
             ),
    
    tabPanel("	\ud83c\udfcb\ufe0f\u200d\u2640 Pyramide de l'entraînement",
             mainPanel(
               titlePanel("	\ud83c\udfcb\ufe0f\u200d\u2640 Pyramide de l'entraînement"),
               tabsetPanel(
                 tabPanel("Volume/Intensité/Fréquence",
                          fluidRow(
                            sidebarPanel(width = 6,
                                         tags$figure(
                                           img(src = "rep.jpeg", height = "auto", width = "100%"),
                                           tags$figcaption("Crédit image : Ghaïs \"Geek'n'Fit\" Guelaïa, Combien de reps pour quels objectifs ?, panodyssey.com."))
                            ),
                            sidebarPanel(
                                         h4("Combien de séries par groupe musculaire ?"),
                                         actionButton(inputId = "dev", label = "Pour developper"),
                                         actionButton(inputId = "maintien", label = "Pour maintenir"),
                                         bsTooltip(id = "dev",
                                                   title = "Pour developper la qualité voulue, il est conseillé de réaliser entre 10 et 20 séries par groupe musculaire par semaine",
                                                   trigger = "hover"),
                                         bsTooltip(id = "maintien",
                                                   title = "Pour maintenir la qualité voulue, il est conseillé de réaliser entre 3 et 5 séries par groupe musculaire par semaine",
                                                   trigger = "hover")
                                         ),
                            sidebarPanel(
                                         h4("Combien de temps de repos entre les séries ?"),
                                         actionButton(inputId = "poly", label = "Polyarticulaires"),
                                         actionButton(inputId = "mono", label = "Monoarticulaires"),
                                         bsTooltip(id = "poly",
                                                   title = "Il est conseillé de prendre entre 3 et 5 minutes de repos entre chaque série de travail.",
                                                   trigger = "hover"),
                                         bsTooltip(id = "mono",
                                                   title = "Il est conseillé de prendre entre 2 et 3 minutes de repos entre chaque série de travail",
                                                   trigger = "hover")
                                         )
                            )
                          ),
                 tabPanel("Progression",
                          fluidRow(
                            sidebarPanel(width = 6,
                                         h4("Calculateur théorique du tonnage"),
                                         numericInput("set", "Nombre de séries :", min = 0, value = 0),
                                         numericInput("rep", "Nombre de répétitions :", min = 0, max = 20, value = c(6,8)),
                                         numericInput("kg", "Charge utilisée (en kg) :", min = 0, value = 0),
                                         verbatimTextOutput("result"),
                                         tags$p("Note : Le tonnage total est le produit du nombre de séries, de répétitions et de la charge utilisée.", style = "font-size: 90%;")
                            ),
                            sidebarPanel(width = 6,
                              h4("Calculateur théorique de 1RM"),
                              numericInput("charge", "Charge soulevée (kg)", value = 100, label = "Entrez la charge :"),
                              numericInput("repetitions", "Nombre de répétitions", value = 3, step = 1, label = "Entrez le nombre de répétitions :"),
                              numericInput("poids_corps", "Poids de corps (kg) (pour exercices lestés)", value = 0, label = "Entrez votre poids de corps :"),
                              verbatimTextOutput("resultat_1rm"),
                              tags$p("Note : La formule est précise pour 2 à 5 répétitions.", style = "font-size: 90%;"),
                              tags$p("Formule de Brzycki : 1RM = Charge soulevée / (1.0278 – (0.0278 x nRépétitions))", style = "font-size: 90%;")
                            )
                          )
                 ),
                 tabPanel("Selection d'exercices",
                          mainPanel(
                              tabPanel("Force",
                                       sidebarPanel(width = 6,
                                                    h4("Force"),
                                                    selectInput("mouvement", "Choisir un mouvement :",
                                                                choices = c("Muscle up", "Squat", "Pull Up", "Dips", "Bench", "Deadlift")),
                                                    tableOutput("exercice"),
                                                    tableOutput("muscles_cibles")
                                                    ),
                                       sidebarPanel(width = 6,
                                                    h4("Muscle"),
                                                    selectInput("muscle", "Choisir un muscle :",
                                                                choices = muscles),
                                                    tableOutput("exercices_muscle")
                                                    )
                                       
                            )
                          )
                 )
                 )
               )
             ),
    
    tabPanel("	\ud83c\udf4e Pyramide de la nutrition",
             mainPanel(
               titlePanel("	\ud83c\udf4e Pyramide de la nutrition"),
               tabsetPanel(
                 tabPanel("Balance énergétique",
                          fluidRow(
                            sidebarPanel(
                                         selectInput("sex", "Sexe", choices = c("Homme", "Femme")),
                                         numericInput("age", "Âge (années)", value = 22, min = 0),
                                         numericInput("weight", "Poids (kg)", value = 60, min = 0),
                                         numericInput("height", "Taille (cm)", value = 163, min = 0),
                                         selectInput("activity", "Niveau d'activité physique",
                                                     choices = list("Sédentaire" = 1.2, 
                                                                    "Activité légère" = 1.375, 
                                                                    "Activité modérée" = 1.55, 
                                                                    "Activité intense" = 1.725, 
                                                                    "Activité très intense" = 1.9),
                                                     selected = "Sédentaire"),
                                         textOutput("mb_result"),
                                         textOutput("bcj_result")
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,
                                       h4("Dépense Énergétique Quotidienne"),
                                       plotlyOutput("energy_plot")
                                ),
                                column(6,
                                       h4(textOutput("macro_title")),
                                       plotlyOutput("macro_plot")
                            )
                          )
                          )
                          )
                 ),
               tabPanel("Macronutriments",
                 sidebarPanel(
                   selectInput(inputId = "nutrient", label = "Sélectionner le nutriment :", 
                               choices = c("Protéines" = "proteines", "Glucides" = "glucides", "Lipides" = "lipides")),
                   radioButtons(inputId = "select_level", label = "Sélectionner le niveau : ",
                                choices = c("Groupes" = "alim_ssgrp_nom_fr",
                                            "Sous-groupes" = "alim_ssssgrp_nom_fr")),
                   colourInput(inputId = "hist_color", label = "Choisir une couleur", value = "#C43413")
                 ),
                 mainPanel(
                   plotlyOutput("histogram", height = 600)
                 )
             ),
             tabPanel("Micronutriments",
                      fluidRow(
                        mainPanel(width = 6,
                          h4("Matrice de corrélation"),
                          plotlyOutput("corr_Heatmap"),
                                     
                        ),
                        sidebarPanel(width = 6,
                                     h4("Que dit ce graphique ?"),
                                     actionButton(inputId = "correlation_interpretation_p", label = "Exemple de corrélation positive "),
                                     actionButton(inputId = "correlation_interpretation_n", label = "Exemple de corrélation négative"),
                                     bsTooltip(id = "correlation_interpretation_p",
                                               title = "Les Lipides (n°18) sont corrélés positivement avec les AG saturés (n°32), AG monoinsaturés (n°32), et AG polyinsaturés (n°34), mais aussi avec les acides oléiques (n°43) et palmitiques (n°41).",
                                               trigger = "hover"),
                                     bsTooltip(id = "correlation_interpretation_n",
                                               title = "L'Eau (n°14) est corrélée négativement avec notamment l'énergie (n°10:13), les Glucides (n°17) et les Lipides (n°18).",
                                               trigger = "hover"),
                                     p(""),
                                     selectInput(inputId = "number", label = "Sélectionner un numéro :", choices = as.character(seq(10, 76, 1))),
                                     textOutput("corresponding_name")
                        )
                        )
                        ),
             ))
             ),

    tabPanel("\ud83d\udcd8 Sources",
             h2("\ud83d\udcd8 Sources"),
             tags$div(
               class = "well",
               p("M. Brzycki (1993) Strength Testing-Predicting a One-Rep Max from Reps-to-Fatigue Journal of Physical Education, Recreation & Dance, 64:1, 88-90, DOI: 10.1080/07303084.1993.10606684"),
               p("Roza, A. M., & Shizgal, H. M. (1984). The Harris Benedict equation reevaluated: resting energy requirements and the body cell mass. the American Journal of Clinical Nutrition, 40(1), 168–182. https://doi.org/10.1093/ajcn/40.1.168"),
               p("Lucas Gouiffes. (2019, August 30). 10 Minutes pour être calé en Muscu. [Video]. YouTube. https://www.youtube.com/watch?v=gmV6jYhdRng"),
               p("The Muscle and Strength Pyramids: nutrition and training. (2023, November 28). The Muscle & Strength Pyramids. https://muscleandstrengthpyramids.com/"),
               p("Guelaïa, G. (2020). Combien de reps pour quels objectifs ? Panodyssey. https://panodyssey.com/fr/article/sport/combien-de-reps-pour-quels-objectifs-q95m2wmukppb")
               
               )
             )
    )
    )

# Server
server <- function(input, output, session) {
  
  ##########################
  ######## ONGLET 2 ########
  ##########################

  # Créer une liste réactive pour stocker les données des exercices
  exercices <- reactiveValues()
  
  # Fonction pour récupérer les valeurs des exercices existants
  get_exercise_values <- function(seance) {
    lapply(seq_len(length(exercices[[seance]])), function(j) {
      list(
        mouvement = input[[paste0("mouvement_", seance, "_", j)]],
        muscle = input[[paste0("muscle_", seance, "_", j)]],
        series = input[[paste0("series_", seance, "_", j)]],
        repetitions_min = input[[paste0("repetitions_", seance, "_", j)]][1],
        repetitions_max = input[[paste0("repetitions_", seance, "_", j)]][2]
      )
    })
  }
  
  observe({
    req(input$seances_par_semaine)
    seances <- input$seances_par_semaine
    
    lapply(seq_len(seances), function(i) {
      output[[paste0("exercices_ui_", i)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[as.character(i)]])), function(j) {
            fluidRow(
              column(3, textInput(paste0("mouvement_", i, "_", j), "Nom de l'exercice:", 
                                  value = ifelse(is.null(exercices[[as.character(i)]][[j]]$mouvement), "", exercices[[as.character(i)]][[j]]$mouvement))),
              column(3, selectInput(paste0("muscle_", i, "_", j), "Muscle ciblé:", 
                                    choices = muscles, 
                                    selected = ifelse(is.null(exercices[[as.character(i)]][[j]]$muscle), "", exercices[[as.character(i)]][[j]]$muscle))),
              column(3, sliderInput(paste0("series_", i, "_", j), "Nombre de séries:", 
                                    min = 1, max = 10, 
                                    value = ifelse(is.null(exercices[[as.character(i)]][[j]]$series), 1, exercices[[as.character(i)]][[j]]$series))),
              column(3, sliderInput(paste0("repetitions_", i, "_", j), "Nombre de répétitions:", 
                                    min = 1, max = 20, 
                                    value = ifelse(is.null(exercices[[as.character(i)]][[j]]$repetitions_min) || is.null(exercices[[as.character(i)]][[j]]$repetitions_max), c(1, 1), c(exercices[[as.character(i)]][[j]]$repetitions_min, exercices[[as.character(i)]][[j]]$repetitions_max))))
            )
          })
        )
      })
    })
  })
  
  # Observer pour ajouter des séances
  observeEvent(input$seances_par_semaine, {
    nouvelles_seances <- input$seances_par_semaine
    
    # Initialiser chaque séance avec une liste vide
    for (i in seq_len(nouvelles_seances)) {
      if (is.null(exercices[[as.character(i)]])) {
        exercices[[as.character(i)]] <- list()
      }
    }
    
    # Supprimer les séances en excès si le nombre de séances est réduit
    if (nouvelles_seances < length(exercices)) {
      for (i in (nouvelles_seances + 1):length(exercices)) {
        exercices[[as.character(i)]] <- NULL
      }
    }
  })
  
  # Observer pour ajouter des exercices
  observeEvent(input$ajouter_ligne, {
    seance <- as.character(input$seance_select)
    
    if (is.null(exercices[[seance]])) {
      exercices[[seance]] <- list()
    }
    
    # Capture existing values
    valeurs_exercices_existants <- get_exercise_values(seance)
    
    # Add a new exercise with default values
    if (length(valeurs_exercices_existants) < 7) {
      valeurs_exercices_existants <- c(valeurs_exercices_existants, list(
        list(
          mouvement = "",
          muscle = "",
          series = 3,
          repetitions_min = 6,
          repetitions_max = 8
        )
      ))
    }
    
    # Update reactive list
    exercices[[seance]] <- valeurs_exercices_existants
    
    # Render updated UI
    output[[paste0("exercices_ui_", seance)]] <- renderUI({
      fluidRow(
        lapply(seq_len(length(exercices[[seance]])), function(j) {
          fluidRow(
            column(3, textInput(paste0("mouvement_", seance, "_", j), "Nom de l'exercice:", value = exercices[[seance]][[j]]$mouvement)),
            column(3, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", choices = muscles, selected = exercices[[seance]][[j]]$muscle)),
            column(3, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[seance]][[j]]$series)),
            column(3, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max)))
          )
        })
      )
    })
  })
  
  # Observer pour supprimer des exercices
  observeEvent(input$supprimer_ligne, {
    seance <- as.character(input$seance_select)
    
    if (!is.null(exercices[[seance]])) {
      # Capture existing values
      valeurs_exercices_existants <- get_exercise_values(seance)
      
      # Remove the last exercise
      if (length(valeurs_exercices_existants) > 0) {
        valeurs_exercices_existants <- valeurs_exercices_existants[-length(valeurs_exercices_existants)]
      }
      
      # Update reactive list
      exercices[[seance]] <- valeurs_exercices_existants
      
      # Render updated UI
      output[[paste0("exercices_ui_", seance)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[seance]])), function(j) {
            fluidRow(
              column(3, textInput(paste0("mouvement_", seance, "_", j), "Nom de l'exercice:", value = exercices[[seance]][[j]]$mouvement)),
              column(3, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", choices = muscles, selected = exercices[[seance]][[j]]$muscle)),
              column(3, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[seance]][[j]]$series)),
              column(3, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max)))
            )
          })
        )
      })
    }
  })
  
  # Observer pour mettre à jour le nombre de séances
  observeEvent(input$seances_par_semaine, {
    nouvelles_seances <- input$seances_par_semaine
    
    # Ajouter des séances vides pour les nouvelles séances
    for (i in nouvelles_seances) {
      if (is.null(exercices[[as.character(i)]])) {
        exercices[[as.character(i)]] <- list()
      }
    }
  })
  
  # Générer l'interface utilisateur pour les sous-onglets
  output$sous_onglets <- renderUI({
    if (!is.null(input$seances_par_semaine) && input$seances_par_semaine > 0) {
      tab_list <- lapply(seq_len(input$seances_par_semaine), function(i) {
        tabPanel(paste("Séance", i), value = paste0("sous_onglet_", i),
                 uiOutput(paste0("exercices_ui_", i))
        )
      })
      do.call(tabsetPanel, c(type = "tabs", tab_list))
    }
  })
  
  # Sélection de la séance
  output$seance_select <- renderUI({
    if (!is.null(input$seances_par_semaine) && is.numeric(input$seances_par_semaine) && input$seances_par_semaine >= 1) {
      choices <- as.character(seq_len(input$seances_par_semaine))
      selectInput("seance_select", "Choisir une séance :", choices = choices)
    }
  })
  
  # Générer l'interface utilisateur pour les exercices de chaque séance
  observe({
    req(input$seances_par_semaine)
    seances <- input$seances_par_semaine
    
    lapply(seq_len(seances), function(i) {
      output[[paste0("exercices_ui_", i)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[as.character(i)]])), function(j) {
            # Obtenir les valeurs actuelles ou utiliser des valeurs par défaut si elles sont NULL ou NA
            mouvement_val <- ifelse(is.null(exercices[[as.character(i)]][[j]]$mouvement), "", exercices[[as.character(i)]][[j]]$mouvement)
            muscle_val <- ifelse(is.null(exercices[[as.character(i)]][[j]]$muscle), "", exercices[[as.character(i)]][[j]]$muscle)
            series_val <- ifelse(is.null(exercices[[as.character(i)]][[j]]$series), 3, exercices[[as.character(i)]][[j]]$series)
            repetitions_min_val <- ifelse(is.null(exercices[[as.character(i)]][[j]]$repetitions_min), 6, exercices[[as.character(i)]][[j]]$repetitions_min)
            repetitions_max_val <- ifelse(is.null(exercices[[as.character(i)]][[j]]$repetitions_max), 8, exercices[[as.character(i)]][[j]]$repetitions_max)
            
            fluidRow(
              column(3, textInput(paste0("mouvement_", i, "_", j), "Nom de l'exercice:", value = mouvement_val)),
              column(3, selectInput(paste0("muscle_", i, "_", j), "Muscle ciblé:", choices = muscles, selected = muscle_val)),
              column(3, sliderInput(paste0("series_", i, "_", j), "Nombre de séries:", min = 1, max = 10, value = series_val)),
              column(3, sliderInput(paste0("repetitions_", i, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(repetitions_min_val, repetitions_max_val)))
            )
          })
        )
      })
    })
  })
  
  
  # Créer une variable réactive pour stocker les séries par muscle
  series_par_muscle <- reactiveValues(data = NULL)
  
  # Observer pour mettre à jour les séries par muscle
  observe({
    if (!is.null(input$seances_par_semaine) && length(input$seances_par_semaine) > 0) {
      series_data <- list()
      for (seance in seq_len(input$seances_par_semaine)) {
        if (!is.null(exercices[[as.character(seance)]])) {
          for (j in seq_len(length(exercices[[as.character(seance)]]))) {
            muscle <- input[[paste0("muscle_", seance, "_", j)]]
            series <- input[[paste0("series_", seance, "_", j)]]
            # Vérifier si les valeurs ne sont pas nulles ou manquantes
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
        
        # Stocker les données agrégées dans exercices$series_summary_par_semaine
        exercices$series_summary_par_semaine <- series_summary
      } else {
        # Si aucune donnée valide n'a été trouvée, réinitialiser les données agrégées
        exercices$series_summary_par_semaine <- NULL
      }
    } else {
      # Si input$seances_par_semaine est nul ou non défini, réinitialiser les données agrégées
      exercices$series_summary_par_semaine <- NULL
    }
  })
  
  output$set_pie_chart <- renderPlotly({
    if (!is.null(exercices$series_summary_par_semaine)) {
      plot_ly(exercices$series_summary_par_semaine, labels = ~Muscle, values = ~Series, type = 'pie') %>%
        layout(title = "Volume par groupe musculaire")
    } else {
      plot_ly() %>%
        layout(title = "Aucune donnée disponible")
    }
  })
  
  observe({
    req(input$seances_par_semaine)
    seances <- input$seances_par_semaine
    
    lapply(seq_len(seances), function(i) {
      output[[paste0("exercices_ui_", i)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[as.character(i)]])), function(j) {
            fluidRow(
              column(3, textInput(paste0("mouvement_", i, "_", j), "Nom de l'exercice:", value = exercices[[as.character(i)]][[j]]$mouvement)),
              column(3, selectInput(paste0("muscle_", i, "_", j), "Muscle ciblé:", choices = muscles, selected = exercices[[as.character(i)]][[j]]$muscle)),
              column(3, sliderInput(paste0("series_", i, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[as.character(i)]][[j]]$series)),
              column(3, sliderInput(paste0("repetitions_", i, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[as.character(i)]][[j]]$repetitions_min, exercices[[as.character(i)]][[j]]$repetitions_max)))
            )
          })
        )
      })
    })
  })
  
  observe({
    req(input$seances_par_semaine)
    lapply(seq_len(input$seances_par_semaine), function(i) {
      lapply(seq_len(length(exercices[[as.character(i)]])), function(j) {
        observeEvent(input[[paste0("mouvement_", i, "_", j)]], {
          exercices[[as.character(i)]][[j]]$mouvement <- input[[paste0("mouvement_", i, "_", j)]]
        })
        observeEvent(input[[paste0("muscle_", i, "_", j)]], {
          exercices[[as.character(i)]][[j]]$muscle <- input[[paste0("muscle_", i, "_", j)]]
        })
        observeEvent(input[[paste0("series_", i, "_", j)]], {
          exercices[[as.character(i)]][[j]]$series <- input[[paste0("series_", i, "_", j)]]
        })
        observeEvent(input[[paste0("repetitions_", i, "_", j)]], {
          exercices[[as.character(i)]][[j]]$repetitions_min <- input[[paste0("repetitions_", i, "_", j)]][1]
          exercices[[as.character(i)]][[j]]$repetitions_max <- input[[paste0("repetitions_", i, "_", j)]][2]
        })
      })
    })
  })

  output$downloadPdf <- downloadHandler(
    filename = function() {
      paste("Programme_", input$client_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "rapport_seances.Rmd")
      file.copy("rapport_seances.Rmd", tempReport, overwrite = TRUE)
      
      # Vérifier et préparer les données des exercices
      if (is.null(exercices)) {
        stop("Les données des exercices sont absentes.")
      }
      exercices_list <- reactiveValuesToList(exercices)
      
      # Préparer les paramètres pour le rendu du rapport
      params_render <- list(
        exercices = exercices_list,
        client_name = input$client_name
      )
      
      # Nettoyer l'environnement de tricotage si nécessaire
      rm(list = ls(pattern = "^params"), envir = knitr::knit_global())
      
      # Rendre le rapport en utilisant les paramètres définis
      rmarkdown::render(input = tempReport, output_file = file, params = params_render)
    }
  )
  
  
  ##########################  
  ######## ONGLET 3 ########
  ##########################
  
  
  # Calcul tonnage théorique
  output$result <- renderText({
    set <- input$set
    rep <- input$rep
    kg <- input$kg
    
    tonnage <- set * rep * kg
    paste("Tonnage total :", tonnage, "kg")
  })

  # Calculer la 1RM théorique
  output$resultat_1rm <- renderPrint({
    req(input$charge, input$repetitions)
    
    pdc <- ifelse(is.na(input$poids_corps), 0, input$poids_corps)
    
    if (input$repetitions >= 1 & input$repetitions <= 5) {
      rm_theorique <- round((input$charge + pdc) / (1.0278 - (0.0278 * input$repetitions)) - pdc,2)
      cat("Charge maximale théorique :", rm_theorique, "kg\n")
    } else {
      cat("La formule n'est pas précise pour ce nombre de répétitions.")
    }
  })
  
  observeEvent(input$calculer_1rm, {
    output$resultat_1rm
  })
  
  ##########################  
  ######## ONGLET 4 ########
  ########################## 
  
  # Définir la sortie output$exercice
  output$exercice <- renderUI({
    exercice <- switch(input$mouvement,
                       "Muscle up" = "TIRAGE VERTICAL (tout types)\nCURL (toute variantes)\nPENDLAY ROW\nTIRAGE HORIZONTAL (tout types)\nOISEAU (poulie ou halteres)\nFACEPULL",
                       "Squat" = "FENTES (tout types)\nPRESSE INCLINE / CIRCULAIRE\nLEG EXTENSION\nLEG CURL\nHACK SQUAT\nHIP THRUST\nSOULEVE DE TERRE (ROUMAIN OU TRADI)",
                       "Pull Up" = "DEVELOPPE COUCHE (barre ou halteres)\nDEVELOPPE COUCHE PRISE SERREE (barre ou halteres)\nDEVELOPPE MILITAIRE (barre, halteres, debout ou assis)\nEXTENSIONS TRICEPS (toute prises)\nDEVELOPPE INCLINE (barre, halteres ou machine)\nELEVATION LATERALES (halteres, poulie ou machine)\nBARRE AU FRONT (barre, halteres ou poulie)",
                       "Dips" = "DEVELOPPE COUCHE (barre ou halteres)\nDEVELOPPE COUCHE PRISE SERREE (barre ou halteres)\nDEVELOPPE MILITAIRE (barre, halteres, debout ou assis)\nEXTENSIONS TRICEPS (toute prises)\nDEVELOPPE INCLINE (barre, halteres ou machine)\nELEVATION LATERALES (halteres, poulie ou machine)\nBARRE AU FRONT (barre, halteres ou poulie)",
                       "Bench" = "DEVELOPPE COUCHE (barre ou halteres)\nDEVELOPPE COUCHE PRISE SERREE (barre ou halteres)\nDEVELOPPE MILITAIRE (barre, halteres, debout ou assis)\nEXTENSIONS TRICEPS (toute prises)\nDEVELOPPE INCLINE (barre, halteres ou machine)\nELEVATION LATERALES (halteres, poulie ou machine)\nBARRE AU FRONT (barre, halteres ou poulie)",
                       "Deadlift" = "TIRAGE VERTICAL (tout types)\nCURL (toute variantes)\nPENDLAY ROW\nTIRAGE HORIZONTAL (tout types)\nOISEAU (poulie ou halteres)\nFACEPULL",
                       "Renfo" = "Aucun exercice spécifié"
    )
    exercice_list <- strsplit(exercice, "\n")[[1]]
    exercice_html <- tagList(lapply(exercice_list, function(x) {
      tags$li(x)
    }))
    tags$ul(exercice_html)
  })
  
  # Définir la sortie output$muscles_cibles
  output$muscles_cibles <- renderText({
    muscles_cibles <- switch(input$mouvement,
                             "Muscle up" = "TRAPEZES / RHOMBOIDES / DELTOIDES POSTERIEUR / BICEPS",
                             "Squat" = "ISCHIOS / FESSIERS / QUADRICEPS",
                             "Pull Up" = "TRICEPS / DELTOIDES ANTERIEUR / PECS",
                             "Dips" = "TRICEPS / DELTOIDES ANTERIEUR / PECS",
                             "Bench" = "TRICEPS / DELTOIDES ANTERIEUR / PECS",
                             "Deadlift" = "TRAPEZES / RHOMBOIDES / DELTOIDES POSTERIEUR / BICEPS",
                             "Renfo" = "Aucun muscles spécifiés"
    )
    muscles_cibles
  })
  
  # Définir la sortie output$exercices_muscle
  output$exercices_muscle <- renderUI({
    exercices_muscle <- switch(input$muscle,
                               "Pectoraux" = "DEVELOPPE COUCHE (barre ou halteres)\nDEVELOPPE COUCHE PRISE SERREE (barre ou halteres)\nDEVELOPPE MILITAIRE (barre, halteres, debout ou assis)\nEXTENSIONS TRICEPS (toute prises)\nDEVELOPPE INCLINE (barre, halteres ou machine)\nELEVATION LATERALES (halteres, poulie ou machine)\nBARRE AU FRONT (barre, halteres ou poulie)",
                               "Abdominaux" = "CRUNCH (toute variantes)\nPLANCHE (toute variantes)\nLEVE DE JAMBES (toute variantes)\nROTATION RUSSE (toute variantes)\nVELO BICYCLETTE",
                               "Trapèzes" = "SHRUG (barre, haltères ou machine)\nTIRAGE VERTICAL (tout types)\nROWING (tout types)\nDEVELOPPE NUQUE (haltères ou machine)",
                               "Dorsaux" = "TIRAGE VERTICAL (tout types)\nROWING (tout types)\nPENDLAY ROW\nTIRAGE HORIZONTAL (tout types)\nOISEAU (poulie ou halteres)\nFACEPULL",
                               "Epaules" = "DEVELOPPE MILITAIRE (barre, halteres, debout ou assis)\nELEVATION LATERALES (halteres, poulie ou machine)\nOISEAU (poulie ou halteres)\nFACEPULL\nDEVELOPPE ARNU (haltères ou machine)",
                               "Quadriceps" = "SQUAT (tout types)\nFENTES (tout types)\nPRESSE INCLINE / CIRCULAIRE\nLEG EXTENSION\nHACK SQUAT\nHIP THRUST",
                               "Ischio-jambiers" = "LEG CURL (tout types)\nSOULEVE DE TERRE (ROUMAIN OU TRADI)\nGLUTE HAM RAISE",
                               "Fessiers" = "HIP THRUST\nDONKEY KICK\nSQUAT (tout types)\nFENTES (tout types)\nGLUTE BRIDGE",
                               "Mollets" = "EXTENSIONS MOLLETS (un pied ou deux)\nSOULEVE DE TERRE JAMBES TENDUES",
                               "Bras" = "CURL (toute variantes)\nEXTENSIONS TRICEPS (toute prises)\nDIPS (toute variantes)"
    )
    exercices_muscle_list <- strsplit(exercices_muscle, "\n")[[1]]
    exercices_muscle_html <- tagList(lapply(exercices_muscle_list, function(x) {
      tags$li(x)
    }))
    tags$ul(exercices_muscle_html)
  })
  
  # Fonction pour générer un histogramme
  generate_histogram <- function(data, level, color, title) {
    histogram_data <- table(data[[level]])
    sorted_names <- sort(names(histogram_data))
    
    plot_ly(
      x = sorted_names,
      y = histogram_data[sorted_names],
      type = "bar",
      marker = list(color = color)
    ) %>%
      layout(
        yaxis = list(title = "Nombre d'aliments"),
        title = title
      )
  }
  
  # Histogramme pour le nombre d'alments
  output$histogram_data <- renderPlotly({
    color_data <- input$hist_color_data
    level <- switch(input$group_level,
                    "Groupes" = "alim_grp_nom_fr",
                    "Sous-groupes" = "alim_ssgrp_nom_fr",
                    "Sous-sous-groupes" = "alim_ssssgrp_nom_fr")
    
    generate_histogram(data, level, color_data, input$hist_title_data)
  })
  
  # Histogrammes pour les taux de protéines, glucides et lipides
  generate_nutrient_histogram <- function(nutrient, level, color) {
    nutrient_column <- switch(nutrient,
                              "proteines" = "Protéines..N.x.6.25..g.100.g.",
                              "glucides" = "Glucides..g.100.g.",
                              "lipides" = "Lipides..g.100.g.")
    
    histogram_data <- data %>%
      group_by(.data[[level]]) %>%
      summarize(mean = mean(.data[[nutrient_column]], na.rm = TRUE))
    
    plot_ly(
      data = histogram_data,
      x = ~.data[[level]],
      y = ~mean,
      type = "bar",
      marker = list(color = color)
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = paste("Taux moyen de", nutrient, "pour 100g")),
        title = paste("Distribution des", nutrient)
      )
  }
  
  output$histogram <- renderPlotly({
    generate_nutrient_histogram(input$nutrient, input$select_level, input$hist_color)
  })
  
  output$corr_Heatmap <- renderPlotly({
    # Conversion des données en une matrice
    M <- as.matrix(data[, 10:76])
    
    # Calcul de la matrice de corrélation
    R <- cor(M)
    
    # Attribution de noms de lignes et de colonnes à la matrice
    rownames(R) <- as.character(seq(10, 76, 1))
    colnames(R) <- as.character(seq(10, 76, 1))
    
    # Filtrage de la matrice pour exclure certaines colonnes et lignes
    indices_to_exclude <- c(10, 11, 12, 14, 15, 24, 29, 30, 33, 35, 37:67)
    R_filtered <- R[-indices_to_exclude, -indices_to_exclude]
    
    # Définition de la palette de couleurs pour la heatmap
    col <- colorRampPalette(c("blue", "white", "red"))(20)
    
    # Création de la heatmap avec plotly
    plot_ly(
      x = rownames(R_filtered),
      y = colnames(R_filtered),
      z = R_filtered,
      type = "heatmap",
      colors = col,
      hoverinfo = "x+y+z"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "")
      )
  })
  
  output$corresponding_name <- renderText({
    # Récupération des noms des colonnes
    col_names <- colnames(data)[10:76]
    # Trouver le nom correspondant au numéro sélectionné
    selected_number <- as.numeric(input$number)
    corresponding_name <- col_names[selected_number - 9]
    paste("Le numéro", input$number, "correspond à :", corresponding_name)
  })
  
  # Ajout des explications de corrélation (positif et négatif)
  observe({
    addTooltip(session, id = "correlation_interpretation_p",
            title = "Les Lipides (n°18) sont corrélés positivement avec les AG saturés (n°32),
            AG monoinsaturés (n°32), et AG polyinsaturés (n°34),
            mais aussi avec les acides oléiques (n°43) et palmitiques (n°41).",
            trigger = "hover")
    addTooltip(session, id = "correlation_interpretation_n",
            title = "L'Eau (n°14) est corrélée négativement avec notamment l'énergie (n°10:13), 
            les Glucides (n°17) et les Lipides (n°18).",
            trigger = "hover")
  })
  
  # Formule améliorée de Harris et Benedict par Roza et Shizgal (1984)
  calculate_mb <- reactive({
    if (input$sex == "Homme") {
      88.362 + (13.397 * input$weight) + (4.799 * input$height) - (5.677 * input$age)
    } else {
      447.593 + (9.247 * input$weight) + (3.098 * input$height) - (4.330 * input$age)
    }
  })
  
  calculate_bcj <- reactive({
    calculate_mb() * as.numeric(input$activity)
  })
  
  output$mb_result <- renderText({
    paste("Métabolisme de base : ", round(calculate_mb(), 2), " kcal/jour")
  })
  
  output$bcj_result <- renderText({
    paste("Besoins caloriques journaliers : ", round(calculate_bcj(), 2), " kcal/jour")
  })
  
  output$energy_plot <- renderPlotly({
    activity_factors <- c(1.2, 1.375, 1.55, 1.725, 1.9)
    energy_expenditure <- calculate_mb() * activity_factors
    
    data <- data.frame(
      Activity = factor(activity_levels, levels = activity_levels),
      Energy = energy_expenditure
    )
    
    plot_ly(data, x = ~Activity, y = ~Energy, type = 'bar', text = ~round(Energy, 2), textposition = 'auto') %>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(title = 'Calories (kcal)'))
  })
  
  output$macro_plot <- renderPlotly({
    bcj <- calculate_bcj()
    macros <- c("Glucides", "Protéines", "Lipides")
    percentages <- c(0.50, 0.20, 0.30)
    calories <- bcj * percentages
    grams <- c(calories[1] / 4, calories[2] / 4, calories[3] / 9)
    
    data <- data.frame(
      Macronutrient = macros,
      Calories = calories,
      Grams = grams
    )
    
    plot_ly(data, labels = ~Macronutrient, values = ~Calories, type = 'pie', 
            text = ~paste(round(Grams, 2), 'g'),
            textinfo = 'label+text+percent') %>%
      layout(showlegend = FALSE)
  })
  
  output$macro_title <- renderText({
    activity_labels <- c("Sédentaire", "Activité légère", "Activité modérée", "Activité intense", "Activité très intense")
    selected_activity <- activity_labels[which(c(1.2, 1.375, 1.55, 1.725, 1.9) == as.numeric(input$activity))]
    paste("Répartition des Macronutriments :", selected_activity)
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)