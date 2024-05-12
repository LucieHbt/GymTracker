
library(shiny)
library(shinythemes)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Définir les listes prédéfinies
mouvements <- c("Muscle up", "Squat", "Pull Up", "Dips", "Bench", "Deadlift", "Renfo")
muscles <- c("Pectoraux","Abdominaux", "Trapèzes", "Dorsaux", "Epaules", "Quadriceps", "Ischio-jambiers", "Fessiers", "Mollets", "Bras")

ui <- fluidPage(theme = shinytheme("cyborg"),
                shinythemes::themeSelector(),
  navbarPage(
    "Gym Tracker",
    tabPanel("1RM",
             sidebarPanel(
               h4("Calculateur théorique de 1RM"),
               numericInput("charge", "Charge soulevée (kg)", value = 100, label = "Entrez la charge :"),
               numericInput("repetitions", "Nombre de répétitions", value = 3, step = 1, label = "Entrez le nombre de répétitions :"),
               numericInput("poids_corps", "Poids de corps (kg) (pour exercices lestés)", value = 0, label = "Entrez votre poids de corps :"),
               verbatimTextOutput("resultat_1rm"),
               tags$p("Note : La formule est précise pour 2 à 5 répétitions.", style = "font-size: 90%;"),
               tags$p("Formule de Brzycki : 1RM = Charge soulevée / (1.0278 – (0.0278 x nRépétitions))", style = "font-size: 90%;")
             ),
             mainPanel(
               titlePanel("Entrez vos répétitions maximales"),
               uiOutput("exercices"),
               dataTableOutput("table_1rm")
             )
    ),
    tabPanel("Entraînement",
             sidebarPanel(
               h4("Modulez votre semaine type d'entraînement"),
               sliderInput("seances_par_semaine", "Nombre de séances par semaine:", min = 1, max = 7, value = 3),
               uiOutput("seance_select"),
               actionButton("ajouter_ligne", "Ajouter un exercice", icon = icon("plus"), value = 0),
               actionButton("supprimer_ligne", "Supprimer un exercice", icon = icon("minus")),
               actionButton("export_pdf", "Exporter en PDF"),
             ),
             mainPanel(
               titlePanel("Editez vos séances"),
               uiOutput("sous_onglets")
               )
             ),
    tabPanel("Statistiques",
             mainPanel(
               titlePanel("Quelques statistiques"),
               tabsetPanel(
                 tabPanel("Volume",
                          h4("De façon générale, pour l'hypertrophie, il est conseillé de réaliser entre 10 et 20 séries par groupe musculaire par semaine."),
                          fluidRow(
                            sidebarPanel(width = 6, plotlyOutput("set_pie_chart")),
                            sidebarPanel(width = 6, plotlyOutput("general_muscles_plot"))
                          )
                 ),
                 tabPanel("Tonnage",
                          h4("Le tonnage total est le produit du nombre de séries, de répétitions et de la charge utilisée."),
                          fluidRow(
                            sidebarPanel(width = 6,
                                         h4("Calculateur théorique du tonnage"),
                                         numericInput("set", "Nombre de séries :", min = 0, value = 0),
                                         numericInput("rep", "Nombre de répétitions :", min = 0, value = 0),
                                         numericInput("kg", "Charge utilisée (en kg) :", min = 0, value = 0),
                                         verbatimTextOutput("result")
                            )
                          )
                 ),
                 tabPanel("Progression",
                          h4("Observez votre progression potentielle"),
                          sidebarPanel(width = 4,
                                       h4("Extrapolez selon la durée de votre bloc"),
                                       sliderInput("extrapolation", "Durée de votre bloc:", min = 1, max = 10, value = 4),
                                       sliderInput("force_extrapolation", "Potentielle progression par semaine (en %):", min = 1, max = 5, value = 3)
                          ),
                          sidebarPanel(width = 8,
                                       plotlyOutput("strong_extrapolation_plot")
                                       )
                          )
                 )
               )
             ),
    tabPanel("Conseils",
             mainPanel(
               titlePanel("Exercices de renforcement"),
               tabsetPanel(
                 tabPanel("Force et muscles",
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
                 ),
                 tabPanel("Course à pied",
                          h4("Exercices de renforcement pour la course à pied"),
                          tableOutput("exercices_course")
                 )
               )
             )
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
  ######## ONGLET 1 ########
  ##########################  
  
  # Créer un tableau réactif pour stocker les données saisies
  table_reactif1RM <- reactiveVal(data.frame(Mouvement = mouvements, RM = rep(0, length(mouvements))))
  
  # Afficher le tableau éditable
  output$table_1rm <- renderDT({
    datatable(
      table_reactif1RM(),
      editable = TRUE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(targets = 1,
               type = 'num',
               validator = JS("function(value, settings) {
                              if (value <= 0 || value === 'null') {
                                return 'Veuillez entrer une valeur numérique positive.';
                              } else {
                                return '';
                              }
                            }"))
        )
      )
    )
  }, server = FALSE)
  
  # Mettre à jour le tableau réactif lorsque l'utilisateur modifie le tableau éditable
  observeEvent(input$table_1rm_cell_edit, {
    info <- input$table_1rm_cell_edit
    table_modifiee <- isolate(table_reactif1RM())
    if (info$col == 2 && info$value > 0) {
      table_modifiee[info$row, info$col] <- info$value
      table_reactif1RM(table_modifiee)
    } else if (info$col == 2) {
      showNotification("Veuillez entrer une valeur numérique positive.", type = "error")
    }
  }) 
  
  # Calculer la 1RM théorique
  output$resultat_1rm <- renderPrint({
    req(input$charge, input$repetitions)
    
    pdc <- ifelse(is.na(input$poids_corps), 0, input$poids_corps)
    
    if (input$repetitions >= 2 & input$repetitions <= 6) {
      rm_theorique <- round((input$charge + pdc) / (1.0278 - (0.0278 * input$repetitions)) - pdc,2)
      cat("MAX THÉORIQUE :", rm_theorique, "kg\n")
    } else {
      cat("La formule n'est pas précise pour ce nombre de répétitions.")
    }
  })
  
  observeEvent(input$calculer_1rm, {
    output$resultat_1rm
  })
  
  rm_values <- reactive({
    table_reactif1RM()
  })
  
  ##########################
  ######## ONGLET 2 ########
  ##########################

  # Epley : intensite / (1 + 0.033 * nReps) * 1RM
  calculate_load <- function(mouvement, repetitions, intensite) {
    rm_values <- table_reactif1RM()
    load <- rm_values$RM[rm_values$Mouvement == mouvement]
    intensite <- as.numeric(intensite)
    repetitions <- as.numeric(repetitions)
    charge <- (intensite / (1 + 0.033 * repetitions)) * load
    print(paste0("Calcul de la charge pour ", mouvement, " : ", charge))
    return(round(charge, 2))
  }
  
  # Créer une liste réactive pour stocker les données des exercices
  exercices <- reactiveValues()
  
  # Par défaut, ajouter un exercice à la première séance
  exercices[[as.character(1)]] <- list(list(
    mouvement = "Squat",
    muscle = "Jambes",
    intensite = 0.75,
    series = 3,
    repetitions = 8,
    RPE = 7
  ))
  
  # Fonction réactive pour calculer la charge
  charge_react <- reactive({
    seance <- input$seance_select
    if (!is.null(exercices[[as.character(seance)]])) {
      lapply(seq_len(length(exercices[[as.character(seance)]])), function(j) {
        mouvement <- input[[paste0("mouvement_", seance, "_", j)]]
        intensite <- input[[paste0("intensite_", seance, "_", j)]]
        repetitions <- input[[paste0("repetitions_", seance, "_", j)]]
        charge <- calculate_load(mouvement, repetitions, intensite)
        return(charge)
      })
    }
  })
  
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
        intensite = input[[paste0("intensite_", seance, "_", j)]],
        series = input[[paste0("series_", seance, "_", j)]],
        repetitions = input[[paste0("repetitions_", seance, "_", j)]],
        RPE = input[[paste0("RPE_", seance, "_", j)]]
      )
    })
    
    # Ajouter le nouvel exercice à la liste
    exercices[[seance]] <- c(valeurs_exercices_existants, list(list(
      mouvement = "",
      muscle = "",
      intensite = 0.75,
      series = 3,
      repetitions = 8,
      RPE = 7
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
              column(2, sliderInput(paste0("intensite_", seance, "_", j), "Intensité:", min = 0.5, max = 1.0, value = exercices[[seance]][[j]]$intensite, step = 0.05)),
              column(2, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[seance]][[j]]$series)),
              column(2, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = exercices[[seance]][[j]]$repetitions)),
              column(2, sliderInput(paste0("RPE_", seance, "_", j), "RPE:", min = 5, max = 10, value = exercices[[seance]][[j]]$RPE)),
              column(2, div(textOutput(paste0("charge_", seance, "_", j))))
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
          intensite = input[[paste0("intensite_", seance, "_", j)]],
          series = input[[paste0("series_", seance, "_", j)]],
          repetitions = input[[paste0("repetitions_", seance, "_", j)]],
          RPE = input[[paste0("RPE_", seance, "_", j)]]
        )
      })
      
      # Mettre à jour l'UI avec les exercices restants
      output[[paste0("exercices_ui_", seance)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[seance]])), function(j) {
            fluidRow(
              column(2, selectInput(paste0("mouvement_", seance, "_", j), "Mouvement:", mouvements, selected = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$mouvement)))),
              column(2, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", muscles, selected = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$muscle)))),
              column(2, sliderInput(paste0("intensite_", seance, "_", j), "Intensité:", min = 0.5, max = 1.0, value = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$intensite)), step = 0.05)),
              column(2, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", min = 1, max = 10, value = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$series)))),
              column(2, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$repetitions)))),
              column(2, sliderInput(paste0("RPE_", seance, "_", j), "RPE:", min = 5, max = 10, value = ifelse(is.null(input$seance_select), NULL, req(valeurs_exercices_existants[[j]]$RPE)))),
              column(2, div(textOutput(paste0("charge_", seance, "_", j))))
            )
          })
        )
        
        # Mettre à jour les valeurs des entrées utilisateur après avoir supprimé un exercice
        for (j in seq_along(valeurs_exercices_existants)) {
          updateSelectInput(session, paste0("mouvement_", seance, "_", j), selected = valeurs_exercices_existants[[j]]$mouvement)
          updateSelectInput(session, paste0("muscle_", seance, "_", j), selected = valeurs_exercices_existants[[j]]$muscle)
          updateSliderInput(session, paste0("intensite_", seance, "_", j), value = valeurs_exercices_existants[[j]]$intensite)
          updateSliderInput(session, paste0("series_", seance, "_", j), value = valeurs_exercices_existants[[j]]$series)
          updateSliderInput(session, paste0("repetitions_", seance, "_", j), value = valeurs_exercices_existants[[j]]$repetitions)
          updateSliderInput(session, paste0("RPE_", seance, "_", j), value = valeurs_exercices_existants[[j]]$RPE)
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
        tabPanel(paste("Séance", i), value = paste0("sous_onglet_", i), width = "100%",
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
          intensite = 0.75,
          series = 3,
          repetitions = 8,
          RPE = 7
        ))
      }
      
      output[[paste0("exercices_ui_", i)]] <- renderUI({
        fluidRow(
          lapply(seq_len(length(exercices[[as.character(i)]])), function(j) {
            fluidRow(
              column(2, selectInput(paste0("mouvement_", i, "_", j), "Mouvement:", mouvements, selected = exercices[[as.character(i)]][[j]]$mouvement)),
              column(2, selectInput(paste0("muscle_", i, "_", j), "Muscle ciblé:", muscles, selected = exercices[[as.character(i)]][[j]]$muscle)),
              column(2, sliderInput(paste0("intensite_", i, "_", j), "Intensité:", min = 0.5, max = 1.0, value = exercices[[as.character(i)]][[j]]$intensite, step = 0.05)),
              column(2, sliderInput(paste0("series_", i, "_", j), "Nombre de séries:", min = 1, max = 10, value = exercices[[as.character(i)]][[j]]$series)),
              column(2, sliderInput(paste0("repetitions_", i, "_", j), "Nombre de répétitions:", min = 1, max = 20, value = exercices[[as.character(i)]][[j]]$repetitions)),
              column(2, sliderInput(paste0("RPE_", i, "_", j), "RPE:", min = 5, max = 10, value = exercices[[as.character(i)]][[j]]$RPE)),
              column(2, div(textOutput(paste0("charge_", i, "_", j))))
            )
          })
        )
      })
    })
  })
  
  # Observer pour calculer la charge en fonction des données de 1RM
  observe({
    req(input$seances_par_semaine)
    
    seances <- as.integer(input$seances_par_semaine)
    
    if (is.na(seances) || seances <= 0) {
      return()
    }
    
    lapply(seq_len(input$seances_par_semaine), function(i) {
      lapply(seq_len(ifelse(is.null(exercices[[as.character(i)]]), 0, length(exercices[[as.character(i)]]))), function(j) {
        observeEvent(c(paste0("intensite_", i, "_", j), paste0("repetitions_", i, "_", j), paste0("mouvement_", i, "_", j)), {
          intensite <- input[[paste0("intensite_", i, "_", j)]]
          repetitions <- input[[paste0("repetitions_", i, "_", j)]]
          mouvement <- input[[paste0("mouvement_", i, "_", j)]]
          
          # Calculer la charge en fonction des données de 1RM
          charge <- calculate_load(mouvement, repetitions, intensite)
          
          # Mettre à jour la sortie textOutput avec la nouvelle charge
          output[[paste0("charge_", i, "_", j)]] <- renderText({
            validate(
              need(charge > 0, "La charge n'a pas pu être calculée.")
            )
            paste("Charge :", charge, "kg")
          })
        })
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
  
  # Create a reactive expression for the extrapolation
  extrapolated_rm_values <- reactive({
    req(input$extrapolation)
    
    # Calculate the extrapolation
    extrapolation_factor <- 1 + (0.02 * input$extrapolation)
    extrapolated_values <- data.frame(
      Mouvement = rm_values()$Mouvement,
      RM_Initial = rm_values()$RM,
      RM_Extrapolated = rm_values()$RM * extrapolation_factor
    )
    
    return(extrapolated_values)
  })
  
  # Create a reactive expression for the extrapolation
  extrapolated_rm_values <- reactive({
    req(input$extrapolation, input$force_extrapolation)
    
    # Calculate the extrapolation
    extrapolation_factor <- 1 + ((0.02 * input$extrapolation) * input$force_extrapolation)
    extrapolated_values <- data.frame(
      Mouvement = rm_values()$Mouvement,
      RM_Initial = rm_values()$RM,
      RM_Extrapolated = rm_values()$RM * extrapolation_factor
    )
    
    return(extrapolated_values)
  })
  
  # Update the plot when the input duration or force_extrapolation changes
  observeEvent({
    input$extrapolation
    input$force_extrapolation
  }, {
    # Create the Plotly plot
    plot_data <- extrapolated_rm_values()
    plot_data <- plot_data[plot_data$Mouvement != "Renfo", ]
    
    p <- plot_ly(
      data = plot_data,
      x = ~Mouvement,
      y = ~RM_Initial,
      type = 'bar',
      name = 'RM Initial',
      marker = list(color = 'rgba(0, 127, 127, 0.6)')
    )
    
    p <- add_trace(
      p,
      data = plot_data,
      x = ~Mouvement,
      y = ~RM_Extrapolated,
      type = 'bar',
      name = 'RM Extrapolated',
      marker = list(color = 'rgba(153, 50, 204, 0.6)')
    )
    
    # Ajoutez des titres et des étiquettes
    p <- layout(
      p,
      title = "Extrapolation de la 1RM selon la durée du bloc",
      xaxis = list(title = "Mouvement"),
      yaxis = list(title = "1RM (kg)"),
      barmode = 'group',
      legend = list(title = list(text = "Type de RM")),
      showlegend = TRUE
    )
    
    output$strong_extrapolation_plot <- renderPlotly(p)
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
  
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)