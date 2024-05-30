
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

data <- read.csv(
  file = "Table_Ciqual_2020_FR_2020_07_07.csv",
  header = TRUE,
  sep = ";",
  fileEncoding = "latin1"
)

# Définir les listes prédéfinies
mouvements <- c("Muscle up", "Squat", "Pull Up", "Dips", "Bench", "Deadlift", "Renfo")
muscles <- c("Pectoraux","Abdominaux", "Trapèzes", "Dorsaux", "Epaules", "Quadriceps", "Ischio-jambiers", "Fessiers", "Mollets", "Bras")

ui <- fluidPage(theme = shinytheme("cyborg"),
                shinythemes::themeSelector(),
                navbarPage("Gym Tracker",
                           
                           tabPanel("Présentation",
                                    sidebarPanel(
                                      h4("Bienvenue dans Gym Tracker"),
                                      p("Cette application vous aide à suivre et à planifier vos séances d'entraînement."),
                                      h4("Fonctionnalités"),
                                      tags$ul(
                                        tags$li("Planifiez vos séances d'entraînement hebdomadaires."),
                                        tags$li("Ajoutez et personnalisez vos exercices."),
                                        tags$li("Visualisez vos statistiques d'entraînement."),
                                        tags$li("Calculez vos performances théoriques."),
                                        tags$li("Obtenez des conseils sur les exercices."),
                                        tags$li("Prenez connaissance des bases de la nutrition sportive.")
                                      ),
                                      h4("Instructions"),
                                      p("Utilisez les onglets en haut pour naviguer à travers les différentes fonctionnalités de l'application."),
                                      tags$ul(
                                        tags$li("Entraînement : Planifiez et éditez vos séances."),
                                        tags$li("Conseils : Obtenez des recommandations d'exercices."),
                                        tags$li("Nutrition : Obtenez les bases de la nutrition sportive."),
                                        tags$li("Sources : Consultez les références utilisées.")
                                      )
                                    )
                           ),
    
    tabPanel("Entraînement",
             sidebarPanel(
               h4("Modulez votre semaine type d'entraînement"),
               sliderInput("seances_par_semaine", "Nombre de séances par semaine:", min = 1, max = 7, value = 3),
               uiOutput("seance_select"),
               actionButton("ajouter_ligne", "Ajouter un exercice", icon = icon("plus")),
               actionButton("supprimer_ligne", "Supprimer un exercice", icon = icon("minus")),
               plotlyOutput("set_pie_chart"),
               actionButton("export_pdf", "Exporter en PDF")
             ),
             mainPanel(width = 8,
               titlePanel("Editez vos séances"),
               uiOutput("sous_onglets")
               )
             ),
    
    tabPanel("Conseils",
             mainPanel(
               titlePanel("Quelques conseils"),
               tabsetPanel(
                 tabPanel("Volume",
                          fluidRow(
                            sidebarPanel(width = 6, 
                                         plotlyOutput("general_muscles_plot")),
                            sidebarPanel(width = 6,
                                         h4("Combien de séries par groupes musculaires ?"),
                                         actionButton(inputId = "moins_de_5_ans", label = "Moins de 5 ans d'expérience"),
                                         actionButton(inputId = "plus_de_5_ans", label = "Plus de 5 ans d'expérience"),
                                         bsTooltip(id = "moins_de_5_ans",
                                                   title = "Pour developper la qualité voulue, il est conseillé de réaliser entre 10 et 20 séries par groupe musculaire par semaine",
                                                   trigger = "hover"),
                                         bsTooltip(id = "plus_de_5_ans",
                                                   title = "Pour developper la qualité voulue, il est conseillé de réaliser entre 5 et 15 séries par groupe musculaire par semaine",
                                                   trigger = "hover")
                                         )
                            )
                          ),
                 tabPanel("Calculs théoriques",
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
                 tabPanel("Renforcement",
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
                                                    ),
                                       sidebarPanel(width = 4,
                                                    h4("Course à pied"),
                                                    tableOutput("exercices_course")
                                       
                              )
                            )
                          )
                 )
                 )
               )
             ),
    
    tabPanel("Nutrition",
             mainPanel(
               titlePanel("Quelques bases"),
               tabsetPanel(
               tabPanel("Macronutriments",
                 sidebarPanel(
                   selectInput(inputId = "nutrient", label = "Sélectionner le nutriment :", 
                               choices = c("Protéines" = "proteines", "Glucides" = "glucides", "Lipides" = "lipides")),
                   radioButtons(inputId = "select_level", label = "Sélectionner le niveau : ",
                                choices = c("Groupes" = "alim_grp_nom_fr",
                                            "Sous-groupes" = "alim_ssgrp_nom_fr",
                                            "Sous-sous-groupes" = "alim_ssssgrp_nom_fr")),
                   colourInput(inputId = "hist_color", label = "Choisir une couleur", value = "#C43413")
                 ),
                 mainPanel(
                   plotlyOutput("histogram", height = 600)
                 )
             ),
             tabPanel("Corrélation",
                      fluidRow(
                        sidebarPanel(width = 7,
                                     plotlyOutput("corr_Heatmap"),
                                     selectInput(inputId = "number", label = "Sélectionner un numéro :", choices = as.character(seq(10, 76, 1))),
                                     textOutput("corresponding_name")
                                     ),
                        sidebarPanel(width = 5,
                                     h4("Que dit ce graphique ?"),
                                     actionButton(inputId = "correlation_interpretation_p", label = "Exemple de corrélation positive "),
                                     actionButton(inputId = "correlation_interpretation_n", label = "Exemple de corrélation négative"),
                                     bsTooltip(id = "correlation_interpretation_p",
                                               title = "Les Lipides (n°18) sont corrélés positivement avec les AG saturés (n°32), AG monoinsaturés (n°32), et AG polyinsaturés (n°34), mais aussi avec les acides oléiques (n°43) et palmitiques (n°41).",
                                               trigger = "hover"),
                                     bsTooltip(id = "correlation_interpretation_n",
                                               title = "L'Eau (n°14) est corrélée négativement avec notamment l'énergie (n°10:13), les Glucides (n°17) et les Lipides (n°18).",
                                               trigger = "hover")
                        )
                        )
                        ),
             tabPanel("Et moi ?",
                      fluidRow(
                        sidebarPanel(
                          numericInput("age", "Âge", value = 25, min = 0),
                          selectInput("sex", "Sexe", choices = c("Homme", "Femme")),
                          numericInput("weight", "Poids (kg)", value = 70, min = 0),
                          numericInput("height", "Taille (cm)", value = 175, min = 0),
                          selectInput("activity", "Niveau d'activité", choices = c(
                            "Sédentaire (Peu ou pas d'exercice)" = 1.2,
                            "Légèrement actif (exercices/sports légers 3 à 5 jours par semaine)" = 1.375,
                            "Modérément actif (exercice/sports modérés 3 à 5 jours par semaine)" = 1.55,
                            "Très actif (exercice intense/sports 6 à 7 jours par semaine)" = 1.725,
                            "Extra actif (exercices intenses/sports 6 à 7 jours par semaine, plus travail physique)" = 1.9
                          )),
                          selectInput("goal", "Objectif", choices = c(
                            "Perte de poids" = "weight_loss",
                            "Maintien du poids" = "maintenance",
                            "Prise de muscle" = "muscle_gain"
                          )),
                          actionButton("calculate", "Calculer")
                        ),
                        mainPanel(
                          h4("Résultats"),
                          textOutput("calories"),
                          textOutput("protein"),
                          textOutput("fat"),
                          textOutput("carbs")
                          ),
                        sidebarPanel(width = 8,
                                     h4("Comment fonctionne le calculateur ?"),
                                     p("Il fonctionne sur la base de l'équation de Mifflin St. Jeor, considérée comme la « référence » en matière de calculateurs de calories. ",
                                       "On commence par calculer le taux métabolique de base (BMR) ou les calories que votre corps brûle simplement en étant en vie. "),
                                     tags$li("Pour les hommes : 10 x poids (kg) + 6,25 x taille (cm) – 5 x âge (y) + 5 (kcal/jour) "),
                                     tags$li("Pour les femmes : 10 x poids (kg) + 6,25 x taille (cm) – 5 x âge (y) -161 (kcal/jour) "),
                                     p("Ensuite, ce nombre BMR est multiplié, en fonction de votre niveau d'activité : "),
                                             tags$li("Sédentaire = 1,2 "),
                                             tags$li("Peu actif = 1,375 "),
                                             tags$li("Modérément actif = 1,550 "),
                                             tags$li("Très actif = 1,725 "),
                                             tags$li("Très actif = 1,9 "),
                                     p("Le nombre de calories est ensuite ajusté en fonction de votre objectif : "),
                                             tags$li("Perte de poids : Réduire de 10 à 20 % "),
                                             tags$li("Gain de poids : Augmenter de 10 à 20 % "),
                                             tags$li("Maintien du poids : Inchangé "),
                                     p("Ce nombre de calories est divisé en pourcentages de macronutriments basés sur les répartitions communément recommandées pour le gain musculaire, la perte de poids et le maintien du poids. ",
                                       "Ces grammes quotidiens de chaque « macro » proviennent de l’application de ces pourcentages à votre nombre quotidien de calories. ",
                                       "Pour information, chaque gramme d'un macronutriment « vaut » un certain nombre de calories : "),
                                     tags$ul(tags$li("Protéines : 4 calories "),
                                             tags$li("Glucides : 4 calories "),
                                             tags$li("Lipides : 9 calories "))
                                     )
                        )
                      )
             ))
             ),

    tabPanel("Sources",
             h2("\ud83d\udcd8 Sources"),
             tags$div(
               class = "well",
               p("M. Brzycki (1993) Strength Testing-Predicting a One-Rep Max from Reps-to-Fatigue Journal of Physical Education, Recreation & Dance, 64:1, 88-90, DOI: 10.1080/07303084.1993.10606684")
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
  
  # Par défaut, ajouter un exercice à la première séance
  exercices[[as.character(1)]] <- list(list(
    mouvement = "Squat",
    muscle = "Jambes",
    series = 3,
    repetitions_min = 6,
    repetitions_max = 8
  ))
  
  # Observer pour ajouter des exercices
  observeEvent(input$ajouter_ligne, {
    seance <- as.character(input$seance_select)
    if (is.null(exercices[[seance]])) {
      exercices[[seance]] <- list()
    }
    nouveau_exercice_id <- length(exercices[[seance]]) + 1
    
    # Stocker les valeurs des exercices existants
    valeurs_exercices_existants <- lapply(seq_len(length(exercices[[seance]])), function(j) {
      list(
        mouvement = input[[paste0("mouvement_", seance, "_", j)]],
        muscle = input[[paste0("muscle_", seance, "_", j)]],
        series = input[[paste0("series_", seance, "_", j)]],
        repetitions_min = input[[paste0("repetitions_", seance, "_", j)]][1],
        repetitions_max = input[[paste0("repetitions_", seance, "_", j)]][2]
      )
    })
    
    
    # Ajouter le nouvel exercice à la liste
    exercices[[seance]] <- c(valeurs_exercices_existants, list(list(
      mouvement = "",
      muscle = "",
      series = 3,
      repetitions_min = 6,
      repetitions_max = 8
      )))
    
    # Vérifier si input$seances_par_semaine est NULL avant d'utiliser seq_len()
    if (!is.null(input$seances_par_semaine)) {
      # Mettre à jour l'UI avec le nouvel exercice et les exercices existants
      output[[paste0("exercices_ui_", seance)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[seance]])), function(j) {
            fluidRow(
              column(2, selectInput(paste0("mouvement_", seance, "_", j), "Mouvement:", mouvements, selected = exercices[[seance]][[j]]$mouvement)),
              column(2, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", muscles, selected = exercices[[seance]][[j]]$muscle)),
              column(2, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[seance]][[j]]$series)),
              column(2, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max)))
              )
          })
        )
      })
    }
  })
  
  # Observer pour supprimer des exercices
  observeEvent(input$supprimer_ligne, {
    seance <- as.character(input$seance_select)
    if (!is.null(exercices[[seance]])) {
      exercices[[seance]] <- exercices[[seance]][-length(exercices[[seance]])]
      
      # Stocker les valeurs des exercices existants
      valeurs_exercices_existants <- lapply(seq_len(length(exercices[[seance]])), function(j) {
        list(
          mouvement = input[[paste0("mouvement_", seance, "_", j)]],
          muscle = input[[paste0("muscle_", seance, "_", j)]],
          series = input[[paste0("series_", seance, "_", j)]],
          repetitions_min = input[[paste0("repetitions_", seance, "_", j)]][1],
          repetitions_max = input[[paste0("repetitions_", seance, "_", j)]][2]
        )
      })
      
      # Mettre à jour l'UI avec les exercices restants
      output[[paste0("exercices_ui_", seance)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[seance]])), function(j) {
            fluidRow(
              column(2, selectInput(paste0("mouvement_", seance, "_", j), "Mouvement:", mouvements, selected = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$mouvement)))),
              column(2, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", muscles, selected = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$muscle)))),
              column(2, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", min = 1, max = 10, value = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$series)))),
              column(2, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max)))
            )
          })
        )
        
        # Mettre à jour les valeurs des entrées utilisateur après avoir supprimé un exercice
        for (j in seq_along(valeurs_exercices_existants)) {
          updateSelectInput(session, paste0("mouvement_", seance, "_", j), selected = valeurs_exercices_existants[[j]]$mouvement)
          updateSelectInput(session, paste0("muscle_", seance, "_", j), selected = valeurs_exercices_existants[[j]]$muscle)
          updateSliderInput(session, paste0("series_", seance, "_", j), value = valeurs_exercices_existants[[j]]$series)
          updateSliderInput(session, paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max))
        }
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
  
  output$seance_select <- renderUI({
    !is.null(input$seances_par_semaine) && is.numeric(input$seances_par_semaine) && input$seances_par_semaine >= 1
      choices <- as.character(seq_len(input$seances_par_semaine))
      selectInput("seance_select", "Choisir une séance :", choices = choices)
  })

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
  
  # Générer l'interface utilisateur pour les exercices de chaque séance
  observe({
    req(input$seances_par_semaine)
    
    seances <- as.integer(input$seances_par_semaine)
    
    if (is.na(seances) || seances <= 0) {
      return()
    }
    
    lapply(seq_len(seances), function(i) {
      # If the list for the current session is empty, add a default exercise
      if (is.null(exercices[[as.character(i)]]) || length(exercices[[as.character(i)]]) == 0) {
        exercices[[as.character(i)]] <- list(list(
          mouvement = "Squat",
          muscle = "Jambes",
          series = 3,
          repetitions_min = 6,
          repetitions_max = 8
        ))
      }
      
      output[[paste0("exercices_ui_", i)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[as.character(i)]])), function(j) {
            fluidRow(
              column(2, selectInput(paste0("mouvement_", i, "_", j), "Mouvement:", mouvements, selected = exercices[[as.character(i)]][[j]]$mouvement)),
              column(2, selectInput(paste0("muscle_", i, "_", j), "Muscle ciblé:", muscles, selected = exercices[[as.character(i)]][[j]]$muscle)),
              column(2, sliderInput(paste0("series_", i, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[as.character(i)]][[j]]$series)),
              column(2, sliderInput(paste0("repetitions_", i, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = c(exercices[[as.character(i)]][[j]]$repetitions_min, exercices[[as.character(i)]][[j]]$repetitions_max))))
          })
        )
      })
    })
  })
  

  ##########################  
  ######## ONGLET 3 ########
  ##########################
  
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
            } else {
              cat("Valeurs manquantes ou nulles pour l'exercice", j, "de la séance", seance, "\n")
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
  
  output$general_muscles_plot <- renderPlotly({
    # Créez un data frame avec les données
    data <- data.frame(
      Type = c("Force", "Hypertrophie", "Endurance"),
      Min = c(1, 6, 13),
      Max = c(5, 12, 20),
      Max = c(5, 12, 20),
      Color = c("lightgreen", "lightblue", "lightcoral"), 
      DarkColor = c("darkgreen", "darkblue", "darkred") 
    )
    # Créez le graphique avec Plotly
    p <- plot_ly(data, x = ~Type, y = ~Min, type = 'bar', name = 'Min', marker = list(color = ~Color))
    p <- add_trace(p, y = ~Max, name = 'Max', marker = list(color = ~DarkColor))
    
    # Ajoutez des titres et des étiquettes
    p <- layout(p,
                title = "Gammes de Répétitions recommandées",
                xaxis = list(title = ""),
                yaxis = list(title = "Nombre de répétitions", range = c(0, 20)),
                barmode = 'stack',
                showlegend = FALSE)
  })
  
  # Calcul tonnage théorique
  output$result <- renderText({
    set <- input$set
    rep <- input$rep
    kg <- input$kg
    
    tonnage <- set * rep * kg
    paste("Tonnage total :", tonnage, "kg")
  })

  observe({
    addTooltip(session, id = "moins_de_5_ans",
               title = "pour l'hypertrophie, il est conseillé de réaliser entre 10 et 20 séries par groupe musculaire par semaine",
               trigger = "hover")
    addTooltip(session, id = "plus_de_5_ans",
               title = "pour l'hypertrophie, il est conseillé de réaliser entre 5 et 15 séries par groupe musculaire par semaine",
               trigger = "hover"
    )
  })
  
  # Calculer la 1RM théorique
  output$resultat_1rm <- renderPrint({
    req(input$charge, input$repetitions)
    
    pdc <- ifelse(is.na(input$poids_corps), 0, input$poids_corps)
    
    if (input$repetitions >= 2 & input$repetitions <= 6) {
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
  
  
  # Créer le data frame contenant les données du tableau
  exercices_course <- data.frame(
    stringsAsFactors = FALSE,
    EXERCICE = c(
      "SQUAT ( sauté, pistol, barre, haltère ou élastique )",
      "FENTES ( tout types )",
      "PLANCHE ( genoux, pieds )",
      "HIP THRUST",
      "EXTENSIONS MOLLETS ( un pied ou deux )",
      "LEG EXTENSION",
      "LEG CURL",
      "SOULEVE DE TERRE ( ROUMAIN OU TRADI )"
    ),
    MUSCLES_CIBLES = c(
      "QUADRICEPS / FESSIERS",
      "ISCHIOS / FESSIERS / QUADRICEPS",
      "ABDOS",
      "ISCHIOS / FESSIERS",
      "MOLLETS",
      "QUADRICEPS",
      "ISCHIOS",
      "ISCHIOS / FESSIERS"
    )
  )
  
  # Définir la sortie output$exercices_course
  output$exercices_course <- renderTable({
    exercices_course
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
        title = "Matrice de corrélation",
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
  
  observeEvent(input$calculate, {
    # Calcul du métabolisme de base (BMR) en utilisant l'équation de Mifflin-St Jeor
    bmr <- if (input$sex == "Homme") {
      10 * input$weight + 6.25 * input$height - 5 * input$age + 5
    } else {
      10 * input$weight + 6.25 * input$height - 5 * input$age - 161
    }
    
    # Calcul du besoin calorique total (TDEE)
    tdee <- bmr * as.numeric(input$activity)
    
    # Ajustement des calories en fonction de l'objectif
    calories <- switch(input$goal,
                       "weight_loss" = tdee * 0.90, # -10%
                       "maintenance" = tdee,
                       "muscle_gain" = tdee + tdee * 0.10) # +10%
    
    # Distribution des macronutriments
    protein <- input$weight * 2.2  # en grammes, 1g par livre de poids corporel
    fat <- calories * 0.25 / 9  # 25% des calories proviennent des graisses
    carbs <- (calories - (protein * 4 + fat * 9)) / 4  # le reste des calories provient des glucides
    
    # Affichage des résultats
    output$calories <- renderText(paste("Calories journalières : ", round(calories), " kcal"))
    output$protein <- renderText(paste("Protéines : ", round(protein), " g"))
    output$fat <- renderText(paste("Lipides : ", round(fat), " g"))
    output$carbs <- renderText(paste("Glucides : ", round(carbs), " g"))
  })
  
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)