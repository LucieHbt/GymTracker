
##########################
######## GLOBAL ##########
##########################

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
library(knitr)
library(plotly)
library(gridExtra)
library(grid)
library(tinytex)

data <- read.csv(
  file = "Table_Ciqual_2020_FR_2020_07_07.csv",
  header = TRUE,
  sep = ";",
  fileEncoding = "latin1"
)

# Définir les listes prédéfinies
mouvements <- c("Muscle up", "Squat", "Pull Up", "Dips", "Bench", "Deadlift", "Renfo")
muscles <- c("Pectoraux", "Abdominaux", "Trapèzes", "Dorsaux", "Epaules", "Quadriceps", "Ischio-jambiers", "Fessiers", "Adducteurs", "Mollets", "Biceps", "Triceps")
activity_levels <- c("Sédentaire", "Activité légère", "Activité modérée", "Activité intense", "Activité très intense")

##########################
########### UI ###########
##########################

ui <- fluidPage(theme = shinytheme("cyborg"),
                shinythemes::themeSelector(),
                navbarPage("Gym Tracker",
                           
                           tabPanel("	\ud83c\udfaf Présentation",
                                    mainPanel(width = 4,
                                      h3("Bienvenue dans Gym Tracker !"),
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
                                        tags$li("Pyramide de l'entraînement : comprenez comment structurer vos séances selon vos objectifs."),
                                        tags$li("Entraînement : planifiez et éditez vos séances."),
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
    
    tabPanel("	\ud83c\udfcb\ufe0f\u200d\u2640 Pyramide de l'entraînement",
             mainPanel(
               titlePanel("	\ud83c\udfcb\ufe0f\u200d\u2640 Pyramide de l'entraînement"),
               tabsetPanel(
                 tabPanel("Adhésion",
                            mainPanel(width = 12,
                                      h4("Le meilleur programme c'est celui que vous gardez !"),
                                      p("Le meilleur programme, c'est celui qui est faisable, qui vous stimule et qui vous rend fière !"),
                                      p("Se soummettre régulière à des charges de travail, permet une amélioration continue de vos capacités de performance."),
                                      p("Interrompre cette continuité induit une baisse de la capacité de performance."),
                                      h4("Tous les autres aspects sont bonus !"),
                                      p("Suivre le programme de quelqu'un d'autre car il est « optimal », ne sert à rien s'il ne vous motive pas, qu'il vous demande trop de temps et que le matériel est trop coûteux."),
                                      p("Comprendre les principes de l'entraînement permet de maintenir un programme sur le long terme en prenant en compte vos contraintes personnelles, professionnelles et vos objectifs évolutifs.")
                            ),
                            sidebarPanel(width = 4,
                              fluidRow(
                                h4("Sélectionnez vos activités"),
                                uiOutput("exercise_selection"))
                              ),
                            mainPanel(width = 8,
                              h4("Semaine type d'entraînement"),
                              plotlyOutput("plan_summary")
                 )
               ),
                 tabPanel("Volume/Intensité/Fréquence",
                          fluidRow(
                            mainPanel(width = 4,
                                      h4("Les principes fondamentaux"),
                                      p("Le nombre d’exercices et votre temps n’étant pas illimités, voici les principales variables d'entraînement sur lesquelles vous pouvez jouer :"),
                                      actionButton(inputId = "vol", label = "Volume"),
                                      actionButton(inputId = "int", label = "Intensité"),
                                      actionButton(inputId = "freq", label = "Fréquence"),
                                      actionButton(inputId = "exe_btn", label = "Exécution"),
                                      bsTooltip(id = "vol",
                                                title = "Réfléchissez au nombre de répétitions/séries que vous souhaitez sur les groupes musculaires voulus.",
                                                trigger = "hover"),
                                      bsTooltip(id = "int",
                                                title = "Mettez de la charge (tension mécanique) de manière progressive sur vos exercices.",
                                                trigger = "hover"),
                                      bsTooltip(id = "freq",
                                                title = "Choisisez les exercices par séances que vous aimez et que vous êtes capable de réaliser sans douleurs.",
                                                trigger = "hover"),
                                      bsTooltip(id = "exe_btn",
                                                title = "Soignez la technique d`exécution de vos exercices (phase concentrique/excentrique, vitesse, amplitude...).",
                                                trigger = "hover"),
                                      
                                      h4("Combien de séries par groupe musculaire ?"),
                                      actionButton(inputId = "dev", label = "Pour developper"),
                                      actionButton(inputId = "maintien", label = "Pour maintenir"),
                                      bsTooltip(id = "dev",
                                                title = "Pour developper la qualité voulue, il est conseillé de réaliser entre 10 et 20 séries par groupe musculaire par semaine.",
                                                trigger = "hover"),
                                      bsTooltip(id = "maintien",
                                                title = "Pour maintenir la qualité voulue, il est conseillé de réaliser entre 3 et 5 séries par groupe musculaire par semaine.",
                                                trigger = "hover"),
                                      
                                      h4("Combien de temps de repos entre les séries ?"),
                                      actionButton(inputId = "poly", label = "Polyarticulaires"),
                                      actionButton(inputId = "mono", label = "Monoarticulaires"),
                                      bsTooltip(id = "poly",
                                                title = "Il est conseillé de prendre entre 3 et 5 minutes de repos entre chaque série de travail.",
                                                trigger = "hover"),
                                      bsTooltip(id = "mono",
                                                title = "Il est conseillé de prendre entre 2 et 3 minutes de repos entre chaque série de travail.",
                                                trigger = "hover")
                            ),
                            
                            sidebarPanel(width = 7,
                                         tags$figure(
                                           img(src = "rep.jpeg", height = "auto", width = "100%"),
                                           tags$figcaption("Crédit image : Ghaïs \"Geek'n'Fit\" Guelaïa, Combien de reps pour quels objectifs ?, panodyssey.com."))
                            )
                            )
                          ),
                 tabPanel("Progression",
                            mainPanel(width =12,
                                      h4("Comment progresser en musculation ?"),
                                      p("Chaque série de travail est une opportunité de progresser. Toutefois, il est fondamental de faire preuve de progressivité."),
                                      p("Uiliser des cycles de progression peut permettre de programmer votre progression de séance en séance. Des cycles de 8 à 12 semaines sont intéressants pour varier votre pratique et mettre l'accent sur un élément de votre physique : force, hypertrophie de certains groupes musculaires, endurance..."),
                                      p("Tenir un cahier d'entraînement peur également faciliter le suivi de vos performances si vous notez vos charges et le RPE (effort perçu) pour chaque série de travail de chaque exercice à chaque séance."),
                                      h4("Les principales variables"),
                                      p("Vous ne devez moduler q'une seule variable à la fois, afin de progresser tout en respectant vos capacités de récupération."),
                                         actionButton(inputId = "volume", label = "Volume"),
                                         actionButton(inputId = "intensite", label = "Intensité"),
                                         actionButton(inputId = "fréquence", label = "Fréquence"),
                                         actionButton(inputId = "duree", label = "Tempo"),
                                         bsTooltip(id = "volume",
                                                   title = "Visez plus de répétitions/séries sur vos groupes musculaires cibles à chaque entraînement.",
                                                   trigger = "hover"),
                                         bsTooltip(id = "intensite",
                                                   title = "Augmentez les charges (tension mécanique) que vous soulevez à chaque entraînement.",
                                                   trigger = "hover"),
                                         bsTooltip(id = "fréquence",
                                                   title = "Ajoutez des exercices/séances suplémentaires par semaine.",
                                                   trigger = "hover"),
                                         bsTooltip(id = "duree",
                                                   title = "Prolongez votre temps sous tension lors de vos exercices (tempo, ralentir la phase excentrique...).",
                                                   trigger = "hover"),
                                      h4("Suivi des performances"),
                            ),
                            sidebarPanel(width = 6,
                                         h5("Calculateur théorique du tonnage"),
                                         numericInput("set", "Nombre de séries :", min = 0, value = 0),
                                         numericInput("rep", "Nombre de répétitions :", min = 0, max = 20, value = c(6,8)),
                                         numericInput("kg", "Charge utilisée (en kg) :", min = 0, value = 0),
                                         verbatimTextOutput("result"),
                                         tags$p("Note : Le tonnage total est le produit du nombre de séries, de répétitions et de la charge utilisée.", style = "font-size: 90%;")
                            ),
                            sidebarPanel(width = 6,
                              h5("Calculateur théorique de 1RM"),
                              numericInput("charge", "Charge soulevée (kg)", value = 100, label = "Entrez la charge :"),
                              numericInput("repetitions", "Nombre de répétitions", value = 3, step = 1, label = "Entrez le nombre de répétitions :"),
                              numericInput("poids_corps", "Poids de corps (kg) (pour exercices lestés)", value = 0, label = "Entrez votre poids de corps :"),
                              verbatimTextOutput("resultat_1rm"),
                              tags$p("Note : La formule est précise pour 2 à 5 répétitions.", style = "font-size: 90%;"),
                              tags$p("Formule de Brzycki : 1RM = Charge soulevée / (1.0278 – (0.0278 x nRépétitions))", style = "font-size: 90%;")
                            )
                 ),
                 tabPanel("Selection d'exercices",
                          mainPanel(width = 12,
                                    h4("Comment choisir les bons exercices ?"),
                                    p("Chaque morphologie est unique. C'est pourquoi, le choix des exercices doit être individuel. Il ne s'agit pas de reprendre le programme tout fait d'un ami ou celui de votre influenceur préféré. Car, certains de ces exercices vous iront, mais d'autres beaucoup moins."),
                                    p("Il existe de grandes inégalités entre chaque personne pour les mêmes exercices. Si un exercice ne vous convient pas, parce que son exécution vous fait mal, il est inutile d'insister : mettez cet exercice de côté."),
                                    p("Choisissez vos exercices selon votre morphologie et selon vos objectifs, dirigez vous vers des exercices agréables à exécuter et utiles."),
                                    h4("Deux grands groupes d'exerices"),
                                    tags$ul(
                                      tags$li("Les exercices polyarticulaires : permettent de soulever lourd avec une forte sollicitation des muscles (forte tension mécanique) en peu de temps (faible stress métabolique), mais ils sont fatiguants et ne permettent pas forcément de mettre un muscle au centre des priorités."),
                                      tags$li("Les exercices monoarticulaires ou d'isolation : sollicitent moins de muscles (faible tension mécanique), mais demandent moins d'énergie, il donc possible de cibler des muscles en particulier plus longtemps (fort stress métabolique).")),
                                    p("Les exercices polyarticulaires doivent composer l'essentiel d'un programme (1 à 2 exercices par séance), puis les exercices d'isolation peuvent se greffer à votre programme afin d'aider au développement de muscles en retard ou auxquels vous voulez donner la priorité.")
                                    ),
                          sidebarPanel(width = 12,
                                       h4("Exercices de renforcement pour les exercices polyarticulaires courants"),
                                       selectInput("mouvement", "Choisir un mouvement :",
                                                   choices = c("Muscle up", "Squat", "Pull Up", "Dips", "Bench", "Deadlift")),
                                       tableOutput("exercice"),
                                       tableOutput("muscles_cibles")
                                       ),
                          sidebarPanel(width = 12,
                                       h4("Exercices ciblés pour votre objectif"),
                                       selectInput("muscle", "Choisir un muscle :",
                                                   choices = muscles),
                                       tableOutput("exercices_muscle")
                                       )
                          )
               )
               )
             ),
    
    tabPanel("	\ud83c\udfc6 Entraînement",
             sidebarPanel(
               h4("Modulez votre semaine type d'entraînement"),
               sliderInput("seances_par_semaine", "Nombre de séances par semaine : ", min = 1, max = 7, value = 3),
               uiOutput("seance_select"),
               actionButton("ajouter_ligne", "Ajouter un exercice", icon = icon("plus")),
               actionButton("supprimer_ligne", "Supprimer un exercice", icon = icon("minus")),
               h4("Volume par groupe musculaire"),
               plotlyOutput("set_pie_chart"),
               textInput("client_name", label = "Nom du pratiquant : ", value = ""),
               downloadButton("downloadPdf", "Sauvegardez votre semaine type en PDF")
             ),
             mainPanel(width = 8,
                       titlePanel("Editez vos séances"),
                       uiOutput("sous_onglets")
             )
    ),
    
    tabPanel("	\ud83c\udf4e Pyramide de la nutrition",
             mainPanel(
               titlePanel("	\ud83c\udf4e Pyramide de la nutrition"),
               tabsetPanel(
                 tabPanel("Balance énergétique",
                            mainPanel(width = 12,
                              h4("De quoi parle t-on ?"),
                              p("La balance énergétique fait référence à l’équilibre entre les calories que vous consommez (par l’alimentation) et les calories que vous brûlez (par l’exercice physique et le métabolisme de base) au cours d’une journée."),
                              tags$ul(
                                tags$li("Si vous consommez plus de calories que vous n’en brûlez, vous prendrez du poids."),
                                tags$li("Si vous brûlez plus de calories que vous n’en consommez, vous perdrez du poids.")),
                              p("Il existe deux leviers pour agir sur la balance énergétique :"),
                              tags$ul(
                                tags$li("Agir sur votre activité physique : marche, sport..."),
                                tags$li("Agir sur votre apport calorique : surplus/maintien/déficit calorique.")),
                              h4("Mon niveau d'activité ?"),
                              actionButton(inputId = "sed", label = "Sédentaire"),
                              actionButton(inputId = "l_actif", label = "Légèrement actif"),
                              actionButton(inputId = "actif", label = "Actif"),
                              actionButton(inputId = "t_actif", label = "Très actif"),
                              actionButton(inputId = "ext_actif", label = "Extrênement actif"),
                              bsTooltip(id = "sed",
                                        title = "Aucun exercice quotidien ou presque.",
                                        trigger = "hover"),
                              bsTooltip(id = "l_actif",
                                        title = "Vous faites parfois des exercices physiques (1 à 3 fois par semaine).",
                                        trigger = "hover"),
                              bsTooltip(id = "actif",
                                        title = "Vous faites régulièrement des exercices physiques (3 à 5 fois par semaine).",
                                        trigger = "hover"),
                              bsTooltip(id = "t_actif",
                                        title = "Vous faites quotidiennement du sport ou des exercices physiques soutenus.",
                                        trigger = "hover"),
                              bsTooltip(id = "ext_actif",
                                        title = "Votre travail est extrêmement physique ou bien vous vous considérez comme un athlète.",
                                        trigger = "hover"),
                              p("Par exemple, si vous êtes étudiant et que vous faites du sport 4 fois par semaine, alors vous êtes considéré comme étant « actif »."),
                              h4("Et pour atteindre mon objectif ?"),
                              actionButton(inputId = "main", label = "Maintenir mon poids"),
                              actionButton(inputId = "prendre", label = "Prendre du poids"),
                              actionButton(inputId = "perdre", label = "Perdre du poids"),
                              bsTooltip(id = "main",
                                        title = "Consommez ce que vous dépensez.",
                                        trigger = "hover"),
                              bsTooltip(id = "prendre",
                                        title = "\u2197 de 15% votre apport calorique pendant 2 semaines, puis 	\u2197 progressivement de 10% toutes les 2 semaines.",
                                        trigger = "hover"),
                              bsTooltip(id = "perdre",
                                        title = "\u2198 de 15% votre apport calorique (glucides) pendant 2 semaines, puis \u2198 progressivement de 10% toutes les 2 semaines.",
                                        trigger = "hover"),
                              h4("Comment calculer mes besoins énergétiques ?"),
                              p("	\ud83d\udce2 ATTENTION ce ne sont que des estimations théoriques, vous devez ajuster le chiffre obtenu selon vos résultats."),
                              
                              
                          ),
                            sidebarPanel(width = 3,
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
                              textOutput("bcj_result"),
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,
                                       h4("Dépense Énergétique Quotidienne"),
                                       plotlyOutput("energy_plot"),
                                ),
                                column(6,
                                       h4(textOutput("macro_title")),
                                       plotlyOutput("macro_plot"))
                          )
                          )
                 ),
               tabPanel("Macronutriments",
                 mainPanel(width = 12,
                   h4("Protéines, Glucides, Lipides ?"),
                   p("Nos apports énergétiques nous proviennent uniquement de ce que nous mangeons et buvons."),
                   p("Les calories issues de l’alimentation existent principalement sous 3 formes, appelés macronutriments : les protéines, les glucides et les lipides."),
                   tags$ul(
                     tags$li("	\ud83c\udf73 1 g de protéine = 4 Cal."),
                     tags$li("	\ud83c\udf5a 1 g de glucides = 4 Cal."),
                     tags$li("	\ud83e\udd51 1 g de lipides = 9 Cal.")),
                   p("Pensez au fait qu'aucun aliment ne fait grossir ou mincir en soi. L’effet d’une calorie sur notre poids est le même : 1.000 kcal de pâte à tartiner « pèsera » autant dans votre bilan énergétique que 1.000 kcal de haricots rouges."),
                   p("Toutefois, votre pourcentage de graisse, d'os et de masse musculaire est en perpétuelle évolution, c'est pourquoi, l'origine des calories ingérées impacte directement votre composition corporelle."),
                   h4("Comment ajuster mes macronutriments en perte de poids ?"),
                   p("En perte de poids, les protéines jouent un rôle crucial en préservant votre masse musculaire, tandis que les lipides sont essentiels pour maintenir le bon fonctionnement de vos fonctions hormonales et cognitives. C'est pourquoi il est préférable de réduire votre apport calorique en diminuant vos apports glucidiques."),
                   p("Vous pourriez ressentir une baisse d'énergie, mais vos fonctions vitales et reproductives resteront opérationnelles."),
                   h4("Où trouver mes macronutriments ?"),
                   p("\ud83d\udd0e Pour faciliter un bon tracking de vos calories et de vos nutriments, utilisez des applications comme ",
                     a("MyFitnessPal", href = "https://www.myfitnesspal.com/fr"),
                     " ou encore ",
                     a("Yazio", href = "https://www.yazio.com/fr"),
                     ".")
                 ),
                   sidebarPanel(
                   selectInput(inputId = "nutrient", label = "Sélectionner le nutriment :", 
                               choices = c("Protéines" = "proteines", "Glucides" = "glucides", "Lipides" = "lipides")),
                   radioButtons(inputId = "select_level", label = "Sélectionner le niveau : ",
                                choices = c("Groupes" = "alim_ssgrp_nom_fr",
                                            "Sous-groupes" = "alim_ssssgrp_nom_fr")),
                   colourInput(inputId = "hist_color", label = "Choisir une couleur", value = "#C43413"),
                   p("Source : Ciqual 2020"),
                 ),
                 mainPanel(
                   uiOutput("histo_title"),
                   plotlyOutput("histogram", height = 600)
                 )
             ),
             
             tabPanel("Micronutriments",
                      mainPanel(width = 12,
                                h4("Minéraux et vitamines ?"),
                                p("L’alimentation apporte des macronutriments et des micronutriments."),
                                p("La micronutrition demande de la vigilance et de la disponibilité pour la préparation d'une alimentation à base de produits frais adaptée à l'intensité de vos exercices."),
                                tags$ul(
                                  tags$li("Les macronutriments : protéines, lipides et glucides fournissent l’énergie nécessaire au fonctionnement du métabolisme. Ils sont issus des aliments."),
                                  tags$li("Les micronutriments : vitamines, minéraux et oligoéléments, acides gras contenus dans les aliments ou résultant de la transformation des macronutriments. Ils sont nécessaires pour que nos cellules transforment correctement les macronutriments en énergie.")),
                                h4("Quelques conseils"),
                                p("Pour identifier et suivre d'éventuelles carences, vous pouvez réaliser une prise de sang par an."),
                                tags$ul(
                                  tags$li("De bonnes graisses (omega 3) pour les articulations (poisson, oléagineux, huile de colza, lin...)."),
                                  tags$li("De la viande rouge (fer) 2 fois par semaine et pas 2 jours consécutifs."),
                                  tags$li("Des légumineuses pour faire le stock de glycogène et apporter des glucides de qualité."),
                                  tags$li("Des légumes en quantité illimitée pour les fibres et les vitamines."),
                                  tags$li("Des fruits riches en antioxydants : fruits rouges et oranges en particulier."),
                                  tags$li("Des aliments probiotiques pour l'équilibre intestinal.")),
                                h4("Où trouver mes micronutriments ?"),
                                p("Ici, vous pouvez observer certains micronutriments qui se trouvent généralement dans les protéines, glucides et lipides.")
                                
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
                                     selectInput(inputId = "number", label = "Sélectionner un numéro :", choices = as.character(seq(10, 35, 1))),
                                     textOutput("corresponding_name"),
                                     p("Source : Ciqual 2020"),
                                     
                        ),
                        mainPanel(width = 6,
                          h4("Matrice de corrélation"),
                          plotlyOutput("corr_Heatmap"),
                        )
                        ),
             
             tabPanel("Supplémentation",
                      mainPanel(width = 12,
                                h4("Gardez à l'esprit qu'il n'existe pas de pilule magique !"),
                                p("Certains suppléments peuvent vous aider à combler des lacunes alimentaires et à soutenir vos entraînements, mais avant tout, il est fondamental d'affiner votre programme d'entraînement et de mettre en place un plan nutritionnel en phase avec vos objectifs."),
                                p("Ne vous laissez pas piéger par le marketing et les publicités aguicheuses, tous les compléments et toutes les marques ne se valent pas !"),
                                p("Pour choisir les bons compléments qui prennent soin tant de votre santé, que de celle de la planète, utilisez le ScanNuts de ",
                                  a("Innutswetrust.", href = "https://innutswetrust.fr/"),
                                ),
                        h4("Quels compléments alimentaires ?"),
                        actionButton(inputId = "pré", label = "Pré-entraînement"),
                        actionButton(inputId = "intra", label = "Intra-entraînement"),
                        actionButton(inputId = "post", label = "Post-entraînement"),
                        actionButton(inputId = "santé", label = "Santé"),
                        bsTooltip(id = "pré",
                                  title = "Caféine, Bêta-Alanine, L-Arginine, Glutamine, Malate De Citrulline"),
                                  trigger = "hover"),
                        bsTooltip(id = "intra",
                                  title = "Maltodextrine, Electrolytes",
                                  trigger = "hover"),
                        bsTooltip(id = "post",
                                  title = "Créatine, Whey",
                                  trigger = "hover"),
                      bsTooltip(id = "santé",
                                title = "Multivitamines, Multiminéraux, Collagène type I, Omega 3, Vitamine D, Zinc, Magnésium...",
                                trigger = "hover")
                        )
             )
             )
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
    ),
    # Pied de page avec la signature
    tags$footer(
      style = "text-align: right; font-size: 12px;",
      "Développé par ", tags$em("Lucie HUBERT"), " - ", format(Sys.Date(), "%Y")
    )
    )

  ##########################
  ######## SERVER ##########
  ##########################

server <- function(input, output, session) {
  
  ##########################
  ######## ADHESION ########
  ##########################
  
  # Réactif pour stocker les sélections d'exercice
  exercise_choices <- reactiveValues()
  
  # Initialiser les valeurs par défaut pour les 7 jours de la semaine
  observe({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    for (day in jours) {
      if (is.null(exercise_choices[[day]])) {
        exercise_choices[[day]] <- "Repos"
      }
    }
  })
  
  # Observer pour mettre à jour les sélections d'exercice
  observe({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    lapply(jours, function(day) {
      observeEvent(input[[paste0("exercise_", day)]], {
        exercise_choices[[day]] <- input[[paste0("exercise_", day)]]
      })
    })
  })
  
  # Rendu dynamique des sélecteurs d'exercice dans l'UI
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
            column(3, textInput(paste0("mouvement_", seance, "_", j), "Nom de l'exercice:", 
                                value = exercices[[seance]][[j]]$mouvement)),
            column(3, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", 
                                  choices = muscles, selected = exercices[[seance]][[j]]$muscle)),
            column(3, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", 
                                  min = 1, max = 10, value = exercices[[seance]][[j]]$series)),
            column(3, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", 
                                  min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max)))
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
              column(3, textInput(paste0("mouvement_", seance, "_", j), "Nom de l'exercice:", 
                                  value = exercices[[seance]][[j]]$mouvement)),
              column(3, selectInput(paste0("muscle_", seance, "_", j), "Muscle ciblé:", 
                                    choices = muscles, selected = exercices[[seance]][[j]]$muscle)),
              column(3, sliderInput(paste0("series_", seance, "_", j), "Nombre de séries:", 
                                    min = 1, max = 10, value = exercices[[seance]][[j]]$series)),
              column(3, sliderInput(paste0("repetitions_", seance, "_", j), "Nombre de répétitions:", 
                                    min = 1, max = 20, value = c(exercices[[seance]][[j]]$repetitions_min, exercices[[seance]][[j]]$repetitions_max)))
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
      plot_ly(exercices$series_summary_par_semaine, labels = ~Muscle, values = ~Series, type = 'pie')
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
      
      exercices_list <- reactiveValuesToList(exercices)
      exercices_per_day <- reactiveValuesToList(exercise_choices)
      
      # Préparer les paramètres pour le rendu du rapport
      params_render <- list(
        exercices = exercices_list,
        exercise_choices = exercices_per_day,
        client_name = input$client_name,
        series_summary_par_semaine = input$series_summary_par_semaine
      )

      # Rendre le rapport en utilisant les paramètres définis
      rmarkdown::render(input = tempReport, output_file = file, params = params_render)
    }
  )
  
  ##########################  
  ######## PROGRESSION #####
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
      cat("1RM théorique :", rm_theorique, "kg\n")
    } else {
      cat("La formule n'est pas précise pour ce nombre de répétitions.")
    }
  })
  
  observeEvent(input$calculer_1rm, {
    output$resultat_1rm
  })
  
  ##################################### 
  ######## SELECTION EXERCICES ########
  #####################################
  
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
                               "Pectoraux" = "DEVELOPPE COUCHE (barre ou halteres)\nDEVELOPPE COUCHE PRISE SERREE (barre ou halteres)\nDEVELOPPE INCLINE (barre, halteres ou machine)\nECARTES POULIE (toutes variantes)",
                               "Abdominaux" = "CRUNCH (toutes variantes)\nPLANCHE (toutes variantes)\nLEVE DE JAMBES (toutes variantes)\nRUSSIAN TWISTS",
                               "Trapèzes" = "SHRUG (barre, haltères ou machine)\nROWING (coudes ouverts)\nTIRAGE MENTON (barre, haltères ou machine)",
                               "Dorsaux" = "TRACTIONS (toutes variantes)\nTIRAGE VERTICAL (toutes variantes)\nROWING (toutes variantes)\nPENDLAY ROW\nTIRAGE HORIZONTAL (toutes variantes)\nOISEAU (poulie ou halteres)\nFACEPULL",
                               "Epaules" = "DEVELOPPE MILITAIRE (barre, halteres, debout ou assis)\nELEVATION LATERALES (halteres, poulie ou machine)\nOISEAU (poulie ou halteres)\nFACEPULL",
                               "Quadriceps" = "SQUAT (toutes variantes)\nFENTES (toutes variantes)\nPRESSE INCLINE / CIRCULAIRE\nLEG EXTENSION",
                               "Ischio-jambiers" = "LEG CURL (toutes variantes)\nSOULEVE DE TERRE (ROUMAIN OU TRADI)\nSTEP UP (toutes variantes)",
                               "Adducteurs" = "MACHINE A ADDUCTIONS\nSQUAT SUMO",
                               "Fessiers" = "HIP THRUST (toutes variantes)\nSQUAT (toutes variantes)\nFENTES (toutes variantes)",
                               "Mollets" = "EXTENSIONS MOLLETS (un pied ou deux/assis ou debout)",
                               "Biceps" = "CURL (toutes variantes)\nTRACTIONS SUPINATION",
                               "Triceps" = "EXTENSIONS TRICEPS (toutes prises)\nDIPS (toutes variantes)\nSKULL CRUSHERS (toutes variantes)",
    )
    exercices_muscle_list <- strsplit(exercices_muscle, "\n")[[1]]
    exercices_muscle_html <- tagList(lapply(exercices_muscle_list, function(x) {
      tags$li(x)
    }))
    tags$ul(exercices_muscle_html)
  })
  
  #################################
  ######## MACRONUTRIMENTS ########
  #################################
  
  # Function to generate nutrient histograms
  generate_nutrient_histogram <- function(data, nutrient, level, color) {
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
        yaxis = list(title = paste("Taux moyen de", nutrient, "pour 100g"))
      )
  }
  
    output$histogram <- renderPlotly({
      generate_nutrient_histogram(data, input$nutrient, input$select_level, input$hist_color)
      })
    

  # Reactive expression to create the dynamic title
  output$histo_title <- renderUI({
    nutrient_name <- switch(input$nutrient,
                            "proteines" = "Protéines",
                            "glucides" = "Glucides",
                            "lipides" = "Lipides")
    h4(paste("Distribution des", nutrient_name))
  })
  
  #################################
  ######## MICRONUTRIMENTS ########
  #################################
  
  output$corr_Heatmap <- renderPlotly({
    # Conversion des données en une matrice
    M <- as.matrix(data[, 10:35])
    
    # Calcul de la matrice de corrélation
    R <- cor(M)
    
    # Attribution de noms de lignes et de colonnes à la matrice
    rownames(R) <- as.character(seq(10, 35, 1))
    colnames(R) <- as.character(seq(10, 35, 1))
    
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
        xaxis = list(title = "", tickangle = 35),
        yaxis = list(title = "")
      )
  })
  
  output$corresponding_name <- renderText({
    # Récupération des noms des colonnes
    col_names <- colnames(data)[10:45]
    # Trouver le nom correspondant au numéro sélectionné
    selected_number <- as.numeric(input$number)
    corresponding_name <- col_names[selected_number - 9]
    paste("Le numéro", input$number, "correspond à :", corresponding_name)
  })
  
  # Ajout des explications de corrélation (positif et négatif)
  observe({
    addTooltip(session, id = "correlation_interpretation_p",
            title = "Les Lipides (n°18) sont corrélés positivement avec les AG saturés (n°32),
            AG monoinsaturés (n°32), et AG polyinsaturés (n°34).",
            trigger = "hover")
    addTooltip(session, id = "correlation_interpretation_n",
            title = "L'Eau (n°14) est corrélée négativement avec notamment l'énergie (n°10:13), 
            les Glucides (n°17) et les Lipides (n°18).",
            trigger = "hover")
  })
  
  #################################
  ######## BALANCE ENERGETIQUE ####
  #################################
  
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
    paste("MB: ", round(calculate_mb(), 2), " kcal/jour")
  })
  
  output$bcj_result <- renderText({
    paste("BCJ: ", round(calculate_bcj(), 2), " kcal/jour")
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