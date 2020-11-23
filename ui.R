library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)

customLogo <- shinyDashboardLogoDIY(
   boldText = "Población"
   ,mainText = "& Economía"
   ,textSize = 16
   ,badgeText = icon("chart-line")
   ,badgeTextColor = "white"
   ,badgeTextSize = 2
   ,badgeBackColor = "rgb(23,103,124)"
   ,badgeBorderRadius = 3
)

ui <- dashboardPage(title="Equipo 1",
                    #skin = "black",
   dashboardHeader(title = customLogo),
   dashboardSidebar(
      sidebarMenu(h5("Econometría Aplicada 2020", align = "center"),
                  #img(height=30,width=30,src="colmex.jpg"),
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Glosarios", tabName = "glosarios", icon = icon("book")),
      menuItem("México y el mundo", tabName = "internacional", icon = icon("globe-americas")),
      menuItem("Natalidad y fecundidad", icon = icon("baby"), startExpanded = F,
               menuSubItem("Visualización de datos", tabName = "nat_visual"),
               menuSubItem("Mapas", tabName = "nat_mapas")
      ),
      menuItem("Mortalidad", icon = icon("skull"), startExpanded = F,
               menuSubItem("Visualización de datos", tabName = "mor_visual"),
               menuSubItem("Mapas", tabName = "mor_mapas")
      ),
      menuItem("Nupcialidad", icon = icon("user-friends"), startExpanded = F,
               menuSubItem("Visualización de datos", tabName = "nup_visual"),
               menuSubItem("Mapas", tabName = "nup_mapas")
      ),
      menuItem("Análisis económico",icon = icon("chart-area"), startExpanded = F,
               menuSubItem("Clustering y correlación", tabName = "relacion"),
               menuSubItem("Análisis econométrico entidad", tabName = "econometria1"),
               menuSubItem("Análisis econométrico municipio", tabName = "econometria2"),
               menuSubItem("Análisis espacial", tabName = "espacial")
               ),
      menuItem("Sobre el Equipo 1", tabName = "team", icon = icon("users"))
   )
   ),
   dashboardBody(
      #shinyDashboardThemes(
      #   theme = customTheme
      #),
      customTheme,
      tabItems(
      tabItem(tabName = "inicio",
              h4("Bienvenido", align = "center"),
              h3("¿Conoces la relación entre población y economía?", align = "center", style = "color:gray"),
              br(),
              h4(align="center","Los agentes económicos forman parte de una población con características particulares, ubicados en un contexto histórico, con interéses y acceso a recuros específicos.
                Es por esto que desde la Economía es muy importante considerar a la población como un elemento dinámico en el proceso económico.",br(),
                 br(),"Este proyecto de visualización de datos tiene como objetivo presentar de manera interactiva información relevante sobre las principales
                 variables de población para México y su relación con el ciclo económico"
               ),
              br(),
              fluidRow( 
                 column(9,h5(style="text-align: justify;","La estructura es la siguiente:",br(),
                             br(),"Para comenzar, presentamos una sección de ", strong("glosarios interactivos"), " con los principales conceptos y definiciones para el estudio de la población tomando 
                 cuatro variables principales para nuestro análisis: ", strong("natalidad, mortalidad, matrimonios y divorcios.") ,br(),
                             br(),"En segundo lugar, presentamos una ", strong("comparación internacional")," de las principales variables poblacionales para México con otros países, 
                             para conocer nuestra posición en el contexto internacional. Encontrará en esta sección una herramienta que compara la tendencia histórica en los últimos 20 años de variables de población y económicas.",br(),
                             br(),"Posteriormente, para cada variable tenemos una sección de ", strong("visualización y mapas")," donde encontrará gráficos y mapas interactivos de los fenómenos poblacionales 
                             desagregados de acuerdo a sus características, además encontrará la opción de descargar los mapas en dos formatos.",br(),
                             br(),"Enseguida encontrará nuestra sección de ", strong("análisis económico.")," Aquí estudiamos la relación de las variables de población con variables económicas como el 
                             empleo, el ingreso, la pobreza y la desigualdad. Presentamos primero un análisis de clasificación no supervisada a través del agorítmo de ", strong("K-means clustering y correlaciones"), "entre variables.
                             Posteriormente presentamos un análisis econométrico de" , strong("regresiones cuantiles e las variables económicas sobre las poblacionaes a nivel entidad y municipio. "), 
                             "Finalmente una sección de análisis espacial con una herramienta de comparación de variables y un" , strong("análisis espacial de la relación de esperanza de vida e ingreso medio"), "a nivel entidad.",br(),
                             br(),"Al final de este sitio encontrará información sobre este proyecto, el repositorio con las bases de datos y scripts utilizados y los autores.",br(),
                             br(),"Te invitamos a interactuar con todas las gráficas. Elegir distintas variables, años y utilizar tu cursor para colocarte
                             sobre las gráficas y obtener más información."
                           )
                        ),
                 column(3,box(
                    title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                    p(align="center","\"En 2019, la esperanza de vida de las personas en México es de 75.1 años en promedio y para 2030 se estima que sea de 76.7 años. 
                 La de las mujeres es superior a la de los hombres con una brecha de casi seis años.\" Fuente: Conapo")
                              ),
                              helpText(align="center","A lo largo de este sitio encontrarás cajas como esta con datos y estadísticas sobre la población 
                                         y economía mexicana para complementar tu experiencia en este sitio.")
                        )
                     ),
              br(),
              HTML('<center><img src="imagen_pobla.png" width="200"></center>'),
              ),
      tabItem(tabName = "glosarios", # Inicia tab Inicio
              h2("¿Conoces los principales conceptos y variables en el estudio de la población?", align = "center"),
              p(style="text-align: justify;","Por población se hace referencia habitualmente al conjunto de personas que habitan un determinado espacio geográfico o territorio.
                Para poder estudiar y cuantificar a la población, se generan categorías para poder separar los distintos aspectos de la naturaleza humana.",
                "A continuación presentamos un conjunto de glosarios interactivos que permiten conocer los principales conceptos que se utilizan en el estudio de nuestras 4 variables principales."),
              br(),
              fluidRow(
                 tabBox(width = 12,
                 title = "Selecciona una categoría",
                 id = "tabset1", height = "300px",
                 tabPanel("Población", box(
                    title = "Conceptos principales", solidHeader = TRUE,
                    helpText("Selecciona un concepto para conocer su definición. Fuente: Glosario de CONAPO"),
                    selectInput("select_conceptos_pob", label = h4(""), 
                                choices = glosario_pob$concepto, 
                                selected = 1)
                 ),
                 box(title = "Definición", solidHeader = TRUE, textOutput("glosario_pob_var")
                 )),
                 tabPanel("Natalidad", box(
                    title = "Conceptos principales", solidHeader = TRUE,
                    helpText("Selecciona un concepto para conocer su definición. Fuente: Glosario de INEGI"),
                    selectInput("select_conceptos_nat", label = h4(""), 
                                choices = glosario_nat$concepto, 
                                selected = 1)
                 ),
                 box(title = "Definición", solidHeader = TRUE, textOutput("glosario_nat_var")
                 )),
                 tabPanel("Mortalidad", box(
                    title = "Conceptos principales", solidHeader = TRUE,
                    helpText("Selecciona un concepto para conocer su definición. Fuente: Glosario de INEGI"),
                    selectInput("select_conceptos_mor", label = h4(""), 
                                choices = glosario_mor$concepto, 
                                selected = 1)
                 ),
                 box(title = "Definición", solidHeader = TRUE, textOutput("glosario_mor_var")
                 )),
                 tabPanel("Matrimonios", box(
                    title = "Conceptos principales", solidHeader = TRUE,
                    helpText("Selecciona un concepto para conocer su definición. Fuente: Glosario de INEGI"),
                    selectInput("select_conceptos_mat", label = h4(""), 
                                choices = glosario_mat$concepto, 
                                selected = 1)
                 ),
                 box(title = "Definición", solidHeader = TRUE, textOutput("glosario_mat_var")
                 )),
                 tabPanel("Divorcios", box(
                    title = "Conceptos principales", solidHeader = TRUE,
                    helpText("Selecciona un concepto para conocer su definición. Fuente: Glosario de INEGI"),
                    selectInput("select_conceptos_div", label = h4(""), 
                                choices = glosario_div$concepto, 
                                selected = 1)
                 ),
                 box(title = "Definición", solidHeader = TRUE, textOutput("glosario_div_var")
                 ))
                  )
              ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"El Consejo Nacional de Población (Conapo, por su acrónimo) es una instancia gubernamental mexicana que tiene por objeto
                      el diseño, operación y evaluación de las iniciativas públicas destinadas a regular el crecimiento de la población, 
                      los movimientos demográficos, así como la distribución de los habitantes de México en el territorio.\"")
              )
      ), # Termina tab Inicio
      tabItem(tabName = "visual",
              h2("Visualización de datos")
      ), # Inicia Tab Internacional
      tabItem(tabName = "internacional",
              h2("Comparación internacional de indicadores 2000-2020", align = "center"),
              h4("¿Sabes cómo se compara la población de México con la otros países?", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar un panorama general de la población y economía Mexicana respecto a la de otros países como lo es Estados Unidos,
                Brasil, Chile, la región de Latinoamérica y los países de la OCDE. Te invitamos a seleccionar distintas variables e interactuar con las gráficas para conocer mejor a México."),
              br(),
              fluidRow(box(title = "Variable de población",collapsible = TRUE,
                           helpText("Seleccione la variable para mostrar. Fuente: Banco Mundial"),
                           selectInput("var1_inter", 
                                   label = "",
                                   choices = internacionales$serie,
                                   selected = 1), h6(textOutput("glosario_intern_var"))
                           ),
                       box(title = "Variable económica",collapsible = TRUE,
                           helpText("Seleccione la variable para mostrar. Fuente: Banco Mundial"),
                           selectInput("var2_inter", 
                                       label = "",
                                       choices = internacionales_eco$serie,
                                       selected = 1), h6(textOutput("glosario_intern_var_eco"))
                       )
                       ),
              br(),
              fluidRow(column(6,plotlyOutput('plot1_internacional')),
                       column(6,plotlyOutput('plot2_internacional'))
                       ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"Para 2015 (el Censo más reciente) se estima que México tiene una población de 119,938,473 personas y ocupa el lugar 10 a escala mundial por número de habitantes. 
                 Sin embargo, el ritmo de crecimiento –y la tasa de fecundidad (para 2019 de 2.1 hijos por mujer)– será cada vez menor hasta llegar al punto de disminución de la población por crecimiento natural, 
                 fenómeno que ocurrirá por primera vez desde la época revolucionaria.\" Fuente: Proyecciones de la población de México y de las entidades federativas 2016-2050, 
                   elaborado por la Secretaría de Gobernación, El Colegio de México, y el Fondo de Población de las Naciones Unidas (Unfpa).")
              ),
              sidebarLayout(
                 sidebarPanel(
                    h3("Esperanza de vida"),
                    helpText("Seleccione el año y variable"),
                    sliderInput(inputId = "year_esperanza", 
                                label = "Año", 
                                value = 1, min = 2010, max = 2018, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                    selectInput("var_esperanza", 
                                label = "Variable",
                                choices = variables_esperanza,
                                selected = 1
                    ),
                    h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Esperanza_vida.gif", "Descargar GIF"), align = "center"),
                    h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Esperanza_vida.png", "Descargar PNG"), align = "center"),
                    br(),
                    helpText("Elaboración propia con datos de INEGI y CONAPO.")
                 ),
                 mainPanel(plotOutput('mapa_esperanza'))
              ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"En 2019, la esperanza de vida de las personas en México es de 75.1 años en promedio y para 2030 se estima que sea de 76.7 años. 
                 La de las mujeres es superior a la de los hombres con una brecha de casi seis años.\" Fuente: Conapo")
              )
      ), # Termina Tab Internacional
      # Inicia Natalidad Visual
      tabItem(tabName = "nat_visual",
              h2("Estudio de la fecundidad y natalidad en México 2000-2020", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar estadísticas descriptivas de la natalidad en México. En particular
                buscamos mostrar las características de las mujeres que se embarazan, además de presentar datos sobre embarazos adolecentes y su evolución en el tiempo."),
              br(),
              p(style="text-align: justify;","Posteriormente presentamos con más detalle la evolución de los embarazos adolecentes de acuerdo a la región donde se producen, con distitnos niveles
                de agregación, desde municipios hasta nacional."),
              br(),
              fluidPage(
                 fluidRow(box(width = 8,title = "Embarazos por características de la madre",
                              helpText("Seleccione la variable para mostrar"),
                              selectInput("tip_emb", 
                                          label = "Tipos de embarazo",
                                          choices = c("Totales","Adolescentes"),
                                          selected = "Opción_1bis"),
                              helpText("Nota: Embarazos adolecetes sólo disponibles con escolaridad y condición de empleo"),
                              selectInput("var_emb", 
                                          label = "Característica",
                                          choices = nombres_variables1$X1,
                                          selected = "Opción_1"),
                 ), box(width = 4,title = "",p("La gráfica nos muestra la evolución de los embarazos por características de la madre. Podemos seleccionar Totales para conocer
                                     la evolución de todo tipo de embarazo en México y también la opción de observar la evolución de embarazos adolecentes (menores de 19 años). 
                                    La elección de caratcterística nos permite conocer más a detalle las características sociodemográficas de las mujeres embarazadas."))
                 ),
                 plotlyOutput('emb_car'),
                 box(
                    title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                    p(align="center","\"México ocupa el primer lugar en embarazo en adolescentes, 
                    entre los países de la Organización para la Cooperación y el Desarrollo Económico (OCDE) con una tasa de fecundidad de 77 nacimientos por cada mil adolescentes de 15 a 19 
                    años de edad. Asimismo, en México, 23% de las y los adolescentes inician su vida sexual entre los 12 y los 19 años. De estos, 15% de los hombres y 33% de las mujeres no utilizaron ningún 
                    método anticonceptivo en su primera relación sexual.\" Fuente: Estrategia Nacional para la Prevención del Embarazo en Adolescentes")
                 ),
                 fluidRow(box(width = 12,title = "Embarazos adolecentes por área geográfica",
                              helpText("Seleccione el nivel geográfico"),
                              selectInput("var_area", 
                                          label = "Región geográfica",
                                          choices = embarazos_area$area,
                                          selected = "Opción_1")
                              )
                        ),
                 h5("La gráfica nos muestra la evolución temporal, de los embarazos adolescentes (menores de 19 años) en distintos niveles de 
                    agregación: municipios, estados, regiones y nacional. Para municipios y estados, se presentan los 5 casos con un mejor 
                    desempeño promedio de 2000-2019, y los 5 casos con peor desempeño."), 
                 h5("La división de las entidades por nivel regional es el siguiente: el Norte incluye B.C, Chih., Coah., N.L., Son. y Tam.; 
                    el Centro Norte considera a B.C.S, Ags., Col., Dgo., Jal., Mich., Nay., S.L.P., Sin. y Zac.; el Centro lo integran D.F., 
                    Edo. de Méx., Gto., Hgo., Mor., Pue., Qro. y Tlx.; y el Sur Camp., Chis., Gro, Oax., Q.Roo, Tab., Ver. y Yuc. Esta división 
                    se realizó con base en la división habitual de regiones hecha por Banco de México."),
                 plotlyOutput('emb_area')
              ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"El embarazo adolescente no es solo un problema de salud global, sino que expresa desigualdad al presentarse en los estratos 
                   sociales bajos en comparación con los altos. Las variaciones en México van desde 97 adolescentes por cada 1.000 mujeres embarazadas entre la 
                   clase más desfavorecida, a 15 adolescentes por cada 1.000 mujeres embarazadas en la más alta.\" Fuente: Estrategia Nacional para la Prevención del Embarazo en Adolescentes")
              )
              
      ),# Termina Natalidad Visual
      # Inicia Natalidad Mapas
      tabItem(tabName = "nat_mapas",
              h2("Mapas interactivos de natalidad en México 2000-2020", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar mapas coropléticos que puedan ilustrar el fenómeno del embarazo adolescente en México. 
                Además, podemos ver de manera interactiva la evolución de este fenómeno en los últimos 20 años en tres niveles de agregación:
                municipios entidades y regiones. La división de las entidades por nivel regional es el siguiente: el Norte incluye B.C, Chih., 
                Coah., N.L., Son. y Tam.; el Centro Norte considera a B.C.S, Ags., Col., Dgo., Jal., Mich., Nay., S.L.P., Sin. y Zac.; el Centro 
                lo integran D.F., Edo. de Méx., Gto., Hgo., Mor., Pue., Qro. y Tlx.; y el Sur Camp., Chis., Gro, Oax., Q.Roo, Tab., Ver. y Yuc. 
                Esta división se realizó con base en la división habitual de regiones hecha por Banco de México."),
              fluidPage(
                 h3("Embarazos adolescentes por cada 100 mil habitantes"),
                 tabsetPanel(
                    tabPanel("Nivel Entidad", 
                             sidebarLayout(
                                sidebarPanel(
                                   helpText("Seleccione el año"),
                                   sliderInput(inputId = "year_map_entidad_emba", 
                                               label = "Año", 
                                               value = 1, min = 2000, max = 2019,
                                               animate = animationOptions(interval = 2000, loop = TRUE)),
                                   h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Embarazos_adolecentes.gif", "Descargar GIF"), align = "center"),
                                   h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Embarazos_adolecentes.png", "Descargar PNG"), align = "center"),
                                   br(),
                                   helpText("Elaboración propia con datos de INEGI y CONAPO.")
                                          ),
                                mainPanel(plotOutput('map_entidad_emba'))
                                       )
                             ), 
                    tabPanel("Nivel Municipio", 
                             sidebarLayout(
                                sidebarPanel(
                                   helpText("Seleccione el año"),
                                   sliderInput(inputId = "year_map_muni_emba", 
                                               label = "Año", 
                                               value = 1, min = 2000, max = 2019, animate=animationOptions(interval = 2000, loop = TRUE)),
                                   helpText("Elaboración propia con datos de INEGI y CONAPO.")
                                ),mainPanel(plotOutput('map_muni_emba'))
                                       )
                             ),
                    tabPanel("Nivel Región", 
                             sidebarLayout(
                                sidebarPanel(
                                   helpText("Seleccione el año"),
                                   sliderInput(inputId = "year_map_region_emba", 
                                               label = "Año", 
                                               value = 1, min = 2000, max = 2019, animate=animationOptions(interval = 2000, loop = TRUE)),
                                   helpText("Elaboración propia con datos de INEGI y CONAPO.")
                                ),mainPanel(plotOutput('map_region_emba'))
                             )
                    )
                        ),
                 box(
                    title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                    p(align="center","\"Para 2019, las entidades con la mayor tasa de nacimientos son: Chiapas (94.5), Guerrero (81.3),  
                      Zacatecas (73), Michoacán (72) y Puebla (70.9). El caso contrario los representan: Ciudad de México (41.9), Veracruz (52) y 
                      Estado de México (52.2). En el caso de Chiapas, la mayor tasa de nacimientos se reporta entre mujeres de entre 10 y 17 años, 
                      con 25.5 por cada 1,000 de ese mismo grupo de edad. La tasa a nivel nacional es de 16.2.\" Fuente: INEGI Boletín de Prensa sobre Natalidad 2019")
                 ),
                 box(
                    title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "info",
                    p(align="center","\"Para reducir los embarazos adolescentes en México, se han implementado estrategias como el Programa de becas de apoyo a la educación 
                      básica de madres jóvenes y jóvenes embarazadas (Promajoven) de la Secretaria de Educación Pública. Sin embargo, los indicadores muestran que ha tenido
                      poco éxito, ya que solo cuenta con un 22% de efectividad. De esta forma, la problemática social de los padres adolescentes continúa con sus hijos, pues ellos 
                      tienen mayores tasas de abuso y negligencia, mayores posibilidades de ser criminales y mayor probabilidad de convertirse ellas mismas en madres adolescentes, 
                      lo que perpetúa esta situación.\" Fuente: INEGI Boletín de Prensa sobre Natalidad 2019")
                 )
              )
      ),# Temrmina Natalidad Mapas
      # Inicia Mortalidad Visual
      tabItem(tabName = "mor_visual",
              h2("Estudio de la mortalidad en México 2000-2020", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar estadísticas descriptivas de la mortalidad en México, conocer cómo se comporta de acuerdo al género,
                las principales causas de muerte en nuestro país y su evolución en el tiempo."),
              p(style="text-align: justify;","Comenzamos presentando la evolución en el tiempo de acuerdo al género, posteriormente presentamos la distribución de muertes de acuerdo 
                a las causas principales y finalmente la tendencia de mortalidad de acuerdo a las causas principales."),
              br(),
              fluidPage(
                 fluidRow(box(title = "Muertes de acuerdo al sexo",
                              helpText("Seleccione el Rango de interés"),
                              sliderInput("mort_sexo_rango", 
                                          label = "Rango",
                                          min = 2000, max = 2018, value = c(2005, 2015)),
                              ), box(title = "",p("La gráfica nos muestra la mortalidad por género en el 
                                                  rango seleccionado por el usuario. Podemos observar que la tasa de mortadlidad 
                                                  por cada 100 mil personas a nivel nacional ha incrementado alrededor del 70% de 2000 a 2010.
                                                  Además que las muertes de hombres son mayores a las de mujeres en la misma proporción para todo el periodo."))
                           ),
                 fluidRow(
                    column(6,plotlyOutput('mort_sex')),
                    column(6,plotlyOutput('tasa_sex'))
                 ),
                 fluidRow(box(title = "Muertes de acuerdo a la causa",
                              helpText("Seleccione el año"),
                              sliderInput("mort_anio", label = "Año",
                                          min = 2000, max = 2018, value =2000, step=1, animate=animationOptions(interval = 2500, loop = TRUE))
                                          ),
                 box(title = " ",p("Las siguientes gráficas nos muestra las principales causas de muerte en México por género para el
                                     año seleccionado. Se agrupa en la categoría \"demas\" todas las causas con proporción menor al tres por ciento.\". "))
                 ),
                 fluidRow(
                    column(6,plotlyOutput('mort_causa')),
                    column(6,plotlyOutput('mort_causa1'))
                    ),
                 box(
                    title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                    p(align="center","\"Para 2017, se registraron 703 mil 047 defunciones en México, de las cuales alrededor del 56% correspondía a hombres. 
                    La mayoría de las causas de muerte en nuestro país son las enfermedades prevenibles. En la población general, la principal causa de muerte fueron las 
                    enfermedades del corazón (20.1%), seguida de diabetes (15.2%), tumores malignos (12%), enfermedades del hígado (5.5%) y accidentes (5.2%)
                      .\" Fuente: Instituto Nacional de Salud Pública (INSP)")
                 ),
                 fluidRow(box(title = "Tasa de mortalidad por causa",
                              helpText("Seleccione la causa de interés"),
                              selectInput("causa_mort", 
                                          label = "Causa de muerte",
                                          choices = mort_causa_anio$causa,
                                          selected = "Opción"),
                              ), box(title = "",p("En la gráfica podemos observar la tendencia histórica de la mortalidad por género de acuerdo a las principales
                                                  causas de muerte en México. Notemos que en general se mantiene la tendencia de mayor mortalidad para hombres, excepto
                                                  para la Diabetes donde se ha cerrado la brecha en los últimos 3 años."))
                     ),
                 plotlyOutput("mort_causa_plot")
                 )
      ), #Termina Mortalidad Visual
      # Inica Mortalidad Mapas
      tabItem(tabName = "mor_mapas",
              h2("Mapas interactivos de mortalidad en México 2000-2020", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar mapas coropléticos que puedan ilustrar la mortalidad México y sus principales causas.
                Además podemos ver de manera interactiva la evolución de este fenómeno en los últimos 20 años en dos agregaciones: Nivel entidad y a nivel municipio"),
              br(),
              tabsetPanel(
                 tabPanel("Tasa de mortalidad general y por sexo", 
                          sidebarLayout(
                             sidebarPanel(
                                helpText("Seleccione el tipo de tasa"),
                                selectInput("sexo_est_mort", 
                                            label = "Tipo de tasa",
                                            choices = mort_estado_sexo$tipo_tasa,
                                            selected = "Opción"),
                                helpText("Seleccione el año"),
                                sliderInput("sexo_est_anio", label = "Año",
                                            min = 2000, max = 2018, value =2000, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Tasa_Mortalidad.gif", "Descargar GIF"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Tasa_Mortalidad.png", "Descargar PNG"), align = "center"),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput("map1_morta_pl"))
                          )
                        ),
                 tabPanel("Tasa de mortalidad por causa", 
                          sidebarLayout(
                             sidebarPanel(
                                helpText("Seleccione el tipo de tasa"),
                                selectInput("causa_est_mort", 
                                            label = "Tipo de tasa",
                                            choices = mort_estado_causa$causa,
                                            selected = "Opción"),
                                sliderInput("causa_est_anio", label = "Año",
                                            min = 2000, max = 2018, value =2000, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                h4(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mortalidad_Diabetes.gif", "Descargar GIF Diabetes"), align = "center"),
                                h4(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mortalidad_Diabetes.png", "Descargar PNG Diabetes"), align = "center"),
                                h4(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mortalidad_Accidente.gif", "Descargar GIF Accidente"), align = "center"),
                                h4(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mortalidad_Accidente.png", "Descargar PNG Accidente"), align = "center"),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput("map2_morta_pl"))
                          )
                 ),
                 tabPanel("Tasa de mortalidad: Municipios (Año)", 
                          sidebarLayout(
                             sidebarPanel(
                                helpText("Seleccione el año"),
                                sliderInput("mort_gen_anio", label = "Año",
                                            min = 2000, max = 2018, value =2000, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput("map3_morta"))
                          )
                 ),
                 tabPanel("Tasa de mortalidad por causa: Municipios (Tipo de tasa)", 
                          sidebarLayout(
                             sidebarPanel(
                                selectInput("mort_mun", 
                                            label = "Tipo de tasa",
                                            choices = opc_mun,
                                            selected = "Opción"), 
                                   helpText("Seleccione el año"),
                                   sliderInput("mort_mun_anio", label = "Año",
                                               min = 2000, max = 2018, value =2000, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput("map4_morta"))
                          )
                 )
              ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"Para 2017, las cinco primeras causas de muerte en hombres fueron las enfermedades del corazón (20.1%), diabetes (14.1%), 
                   tumores malignos (10.8%), enfermedades del hígado (7.6%) y homicidios (7.3%). Por otro lado, la muerte en mujeres se debió principalmente 
                   a enfermedades del corazón (22.7%), diabetes (18.6%), tumores malignos (14.5%), enfermedades cerebrovasculares (6.1%) y enfermedades pulmonares 
                   obstructivas crónicas (3.8%).\" Fuente: Instituto Nacional de Salud Pública (INSP)")
              )
              
      ), #Termina Mortalidad Mapas
      # Inicia Nupcualidad Visual
      tabItem(tabName = "nup_visual",
              h2("Estudio de la nupcialidad en México 2000-2020", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar estadísticas descriptivas de los matrimonios y divorcios en México. En particular
                buscamos mostrar las características sociodemográfico de los individuos que contraen matrimonio y conocer las principales causas de divorcio."),
              br(),
              p(style="text-align: justify;","En las siguientes gráficas interactivas revisamos las características de los matrimonio en México. 
              Encontrarás respuestas a preguntas como: ¿Qué edad tienen los contrayentes? ¿Qué nivel de educación?
                ¿Bajo qué régimen se casan? ¿Son parejas del mismo sexo?"),
              br(),
              fluidPage( 
                 fluidRow(box(title = "Características principales de matrimonios",
                              helpText("Seleccione la variable de interés"),
                              selectInput("selec_divorcio", 
                                          label = "Variable",
                                          choices = names_divorcio,
                                          selected = 1),
                 ), box(title = "Características principales de divorcios",
                        helpText("Seleccione la variable de interés"),
                        selectInput("selec_matrimonio", 
                                    label = "Variable",
                                    choices = names_matrimonio,
                                    selected = 1))
                 ),
                 fluidRow(
                    column(6,plotlyOutput('tendencia_divorcio')),
                    column(6,plotlyOutput('tendencia_matrimonio'))
                 ),
                 box(
                    title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "primary",
                    h5(align="center","\"Por cada 100 matrimonios en 2019 ocurrieron 31.7 divorcios. La tasa de divorcios se incrementó 
                    57.26% mientras que la de matrimonios disminuyó 24.68% en la última década en México. Para 2019, tenemos índice de 18.4 
                    separaciones por cada 10,000 habitantes superior al 11.7 que registraba en 2010.
                      .\" Fuente: Datos de INEGI")
                 ),
                 fluidRow(box(title = "Principales tipos de matrimonio",
                              sliderInput("matrimonios_anio", label = "Año",
                                          min = 2000, max = 2019, value =2000, step=1, animate=TRUE),
                              selectInput("matrimonios_clase", 
                                          label = "Característica",
                                          choices = matrimonios_tipo_anio$clase,
                                          selected = 1)
                 ),
                 box(title = " ",p("Esta gráfica nos muestra la composición de los matrimonios de acuerdo a distintas categorías para el
                                     año seleccionado como, notar que el porcentaje no representa el total de matrimonios para el año seleccionado.
                                       El objetivo es presentar las proporciones para poder dimensionar distitnos aspectos del fenómeno de matrimonio."))
                 ),
                 fluidRow(
                    column(6,plotlyOutput('matrimonios1')),
                    column(6,box(
                       title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                       p(align="center","\"A pesar de que en 2019 se registraron 504,923 matrimonios en el Registro Civil, la tasa de matrimonios por 1,000 
                       habitantes siguió su caída secular. Esta relación se duplicó en la última década, al pasar de 15.1 en 2010 a 31.7 en 2019.",br(),br()
                         ,"Casi la mitad de los matrimonios, el 47%, duraron entre seis y 20 años, con base en datos de 2019. (Ver variable Matrimonios Cortos en gráfica de Divorcios).
                         Casi un tercio, el 30%, se disolvieron legalmente después de 20 años de matrimonio, mientras que uno de cada cinco, el 21%, tuvo una duración legal de entre 
                         uno y cinco años, con solo 1.5% con duración menor a un año. \" ",br(),"Fuente: INEGI")
                              )
                           )
                        ),
                 fluidRow(box(title = "Principales causas de divorcio",
                              helpText("Seleccione el año"),
                              sliderInput("divorcios_anio", label = "Año",
                                          min = 2000, max = 2019, value =2000, step=1, animate=TRUE)
                 ),
                 box(title = " ",p("Las siguientes gráficas de pastel nos muestra los principales motivos de divorcio en México para el
                                     año seleccionado. Debido al peso de Concentimiento Mutuo se añade una segunda gráfica que lo excluye
                                   para poder observar los motivos secundarios."))
                 ),
                 fluidRow(
                    column(6,plotlyOutput('divorcio_causa')),
                    column(6,plotlyOutput('divorcio_causa_sinmutuo'))
                        )
              )
      ),# Termina Nupcualidad Visual
      # Inicia Nupcualidad Mapas
      tabItem(tabName = "nup_mapas",
              h2("Mapas interactivos de nupcialidad en México 2000-2020", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar mapas coropléticos que puedan ilustrar el fenómeno del matrimonio y divorcio México de
              acuerdo a sus principales características. Además podemos ver de manera interactiva la evolución de este fenómeno en los últimos 20 años en dos agregaciones: 
                Nivel entidad y a nivel municipio"),
              br(),
              tabsetPanel(
                 tabPanel("Matrimonios nivel entidad", 
                          sidebarLayout(
                             sidebarPanel(
                                helpText("Seleccione la variable y año"),
                                # Primer Mapa
                                sliderInput(inputId = "year_mat_entidad", 
                                            label = "Año", 
                                            value = 1, min = 2000, max = 2020, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                selectInput("var_mat_entidad", 
                                            label = "Variable",
                                            choices = matrimonios_ent_names
                                ),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Matrimonios_Totales.gif", "Descargar GIF Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Matrimonios_Totales.png", "Descargar PNG Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mismo_sexo.gif", "Descargar GIF Mismo sexo"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mismo_sexo.png", "Descargar PNG Mismo sexo"), align = "center"),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput('mat_entidad'))
                          )
                 ),
                 tabPanel("Divorcios nivel entidad", 
                          sidebarLayout(
                             sidebarPanel(
                                # Segundo Mapa
                                helpText("Seleccione la variable y año"),
                                sliderInput(inputId = "year_div_entidad", 
                                            label = "Año", 
                                            value = 1, min = 2000, max = 2020, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                selectInput("var_div_entidad", 
                                            label = "Variable",
                                            choices = divorcios_ent_names
                                ),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Divorcios_Totales.gif", "Descargar GIF Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Divorcios_Totales.png", "Descargar PNG Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Incitación_a_violencia.gif", "Descargar GIF Violencia doméstica"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Incitación_a_violencia.png", "Descargar PNG Violencia doméstica"), align = "center"),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput('div_entidad'))
                          )
                 ),
                 tabPanel("Matrimonios nivel municipio", 
                          sidebarLayout(
                             sidebarPanel(
                                # Tercer Mapa
                                helpText("Seleccione la variable y año"),
                                sliderInput(inputId = "year_mat_muni", 
                                            label = "Año", 
                                            value = 1, min = 2000, max = 2020, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                selectInput("var_mat_muni", 
                                            label = "Variable",
                                            choices = matrimonios_mun_names
                                ),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Matrimonios_Totales_muni.gif", "Descargar GIF Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Matrimonios_Totales_muni.png", "Descargar PNG Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mismo_sexo_muni_muni.gif", "Descargar GIF Mismo sexo"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Mismo_sexo_muni.png", "Descargar PNG Mismo sexo"), align = "center"),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput('mat_munic'))
                          )
                 ),
                 tabPanel("Divorcios nivel municipio", 
                          sidebarLayout(
                             sidebarPanel(
                                # Cuarto Mapa
                                helpText("Seleccione la variable y año"),
                                sliderInput(inputId = "year_div_muni", 
                                            label = "Año", 
                                            value = 1, min = 2000, max = 2020, step=1, animate=animationOptions(interval = 2000, loop = TRUE)),
                                selectInput("var_div_muni", 
                                            label = "Variable",
                                            choices = divorcios_mun_names
                                ),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Divorcios_Totale_muni.gif", "Descargar GIF Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Divorcios_Totales_muni.png", "Descargar PNG Totales"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Incitación_a_violencia_muni.gif", "Descargar GIF Violencia doméstica"), align = "center"),
                                h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Incitación_a_violencia_muni.png", "Descargar PNG Violencia doméstica"), align = "center"),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotOutput('div_munic'))
                          )
                 )
              ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"Para 2019 las entidades con mayores tasas de divorcios por cada 100 mil habitantes en 2019 fueron Campeche, con 3,800; Sinaloa, con 3,770, y Nuevo León, con 3,700.
                 Por el contrario, las menores tasas correspondieron a Veracruz, con 5.7; Tlaxcala, con 8.3, y Chiapas, con 8.9 separaciones legales por cada 10,000 personas.
                   Las entidades con mayores tasas de matrimonios por cada 100 mil habitantes mayores de 17 años fueron Quintana Roo, con 930; Sinaloa y Guanajuato, con 750, y Sonora, con 740. 
                   Las que presentaron las tasas más bajas fueron Ciudad de México y Puebla, con 3.9; Tlaxcala, con 4.2, e Hidalgo y Baja California Sur, con 4.4."),br(),
                 p(align="center","\"De las 32 entidades del país, en 23 se registraron un total de 3,596 matrimonios entre personas del mismo sexo en 2019. 
                   De ellos, la mayoría fueron en Ciudad de México (1,539), seguida de Jalisco (593), Chihuahua (242) y Nuevo León (185).\" Fuente: INEGI")
                 )
      ), # Termina Nupcualidad Mapas 
      tabItem(tabName = "relacion",
              h2("Clustering y análisis de correlación.", align = "center"),
              h4("¿Cómo podemos aplicar métodos de machine learning al estudio de la población?", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar un análisis de clustering basado en las principales
                         variables económicas y de población a nivel entidad. Posteriormente presentamos una visualización de la correlación que tienen nuestras
                variables de estudio para todos los años."),br(),
              p(style="text-align: justify;","Utilizamos el algorítmo de clasificación no supervisada K-Means Clustering 
                         que agrupa objetos en k grupos basándose en sus características. El agrupamiento se realiza minimizando la 
                         suma de distancias entre cada objeto y el centroide de su grupo o cluster. Este tipo de algotimo de aprendizaje no supervisado es útil para explorar, 
                         describir y resumir datos de una forma distinta. También nos puede ayudar a descubrir patrones y relaciones que desconocíamos."),
              br(),
              tabsetPanel(
                 tabPanel("Clustering en 3 dimensiones", 
                          sidebarLayout(
                             sidebarPanel(
                                selectInput('xcol1', 'Variable X ', vars),
                                selectInput('ycol1', 'Variable Y ', vars, selected = vars[[37]]),
                                selectInput('zcol1', 'Variable Z ', vars, selected = vars[[17]]),
                                numericInput('clusters1', 'Cluster count', 3, min = 1, max = 9),
                                sliderInput(inputId = "year_cluster1", 
                                            label = "Año", 
                                            value = 1, min = 2010, max = 2018, step=1),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                             ),mainPanel(plotlyOutput('clustering3D'),
                                         helpText("Utiliza tu mouse para mover, acercarte y alejarte de la gráfica. 
                                                  Coloca tu cursor sobre cada punto para saber a qué entidad corresponde 
                                                  y conocer los valores de cada variable seleccionada."))
                          )
                 ),
                 tabPanel("Clustering en 2 dimensiones", 
                          sidebarLayout(
                             sidebarPanel(
                                helpText("Seleccione las variables de interés y el año"),
                                selectInput('xcol', 'Variable X Población', variables_poblacion),
                                selectInput('ycol', 'Variable Y Económicas', variables_economicas, selected = variables_economicas[[2]]),
                                numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                                sliderInput(inputId = "year_cluster", 
                                            label = "Año", 
                                            value = 1, min = 2010, max = 2018, step=1, animate=T),
                                helpText("Elaboración propia con datos de INEGI y CONAPO.")
                                ),mainPanel(plotOutput('clustering2D'),
                                            helpText("La cruz representa el centroide de cada cluster. Este punto es determinado por el agoritmo como el equidistante
                                                     de todas las observaciones pertenecientes al cluster."))
                             )
                          )
                 ),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 helpText(align="center","Existen diversas clasificaciones de las regiones en México. El Indicador Trimestral de la Actividad Económica Regional reportado por Banxico 
                                                     divide el país en 4 regiones: Norte, Centro Norte, Centro y Sur. De la siguiente forma: el norte incluye B.C, Chih., Coah., N.L., Son. y Tam.; 
                                                 el centro norte considera a B.C.S, Ags., Col., Dgo., Jal., Mich., Nay., S.L.P., Sin. y Zac.; 
                                                 el centro lo integran D.F., Edo. de Méx., Gto., Hgo., Mor., Pue., Qro. y Tlx.; 
                                                 y el sur Camp., Chis., Gro, Oax., Q.Roo, Tab., Ver. y Yuc.")
              ),
              br(),
              fluidRow(column(8,p(style="text-align: justify;","A continuación presentamos una visualización de las correlaciones entre las principales variables
                                  utilizadas en este estudio para el periodo de 2010 a 2018. Tener la posibilidad de observar la correlación en el tiempo nos permite
                                  identificar si la correlación entre las variables se mantiene constante o varía en el tiempo. Podemos observar que predominan las correlaciones 
                                  positivas y se mantienen costantes en el tiempo.")),
                       column(4,sliderInput(inputId = "year_corr", 
                                            label = "Año", 
                                            value = 1, min = 2010, max = 2018, step=1)
                              )
              ),
              plotOutput('correlation1'),
              plotOutput('correlation2'),
              plotOutput('correlation3')
              
      ),
      tabItem(tabName = "econometria1",
              h2("Análisis econométrico por entidad", align = "center"),
              h4("Regresiones cuantiles para el estudio de la población.", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar un análisis de regresión cuantil y 
                de promedios de las variables poblaciones sobre las variables económicas, de acuerdo al cuantil de la distribución de estas últimas."),br(),
              p(style="text-align: justify;","También se presentan los resultados de regresiones OLS simples de las variables poblacionales sobre las económicas. 
                Con ello se busca identificar los pares de variables que guardan una mayor relación entre sí, para, a partir de esto, formular hipótesis sobre los 
                mecanismos detrás de los efectos que se dan entre población y economía."),
              p(style="text-align: justify;","Las observaciones consideradas se toman para todos los estados y tienen en su mayoría periodicidad trimestral (Gini y Pobreza son anuales). 
                El rango de tiempo considerado es, para la mayoría de las variables, de 2005-2019, pero también algunas se toman de 2000-2019, y algunas más sólo unos años (Gini y Pobreza)."),
              br(),
              fluidRow(box(title = "Tasas poblacionales por veintil de la variable económica",
                           helpText("Seleccione el tipo de tasa"),
                           selectInput("var_e", 
                                       label = "Variable Económica",
                                       choices = prom_cuantiles_estados$vari_econ,
                                       selected = "Opción1_bis"),
                           selectInput("var_p", 
                                       label = "Variable Poblacional",
                                       choices = prom_cuantiles_estados$vari_pob,
                                       selected = "Opción1")
                           ),
                       box(title = " ", "Esta primera gráfica nos permite observar los promedios de los valores de la variable poblacional escogida, agrupando por los veintiles (veinte partes iguales)
                            de la variable económica escogida. De esta forma, se pueden identificar las variables con una mayor relación como aquellas en las que la curva muestra una tendencia clara. 
                           Destacan los casos de esperanza de vida vs mediana del ingreso por hora, y tasa de informalidad laboral vs tasa de divorcios."
                           ),
              ),
              fluidRow(plotlyOutput('prom_cuantiles')),
              br(),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","El impacto económico total del embarazo en adolescentes para la sociedad (por pérdida de ingresos y empleos) es de casi 63 mil millones de pesos, y más de 11 mil millones 
                   de pesos de pérdida de ingresos fiscales para el Estado – un costo que representa el 0.27 por ciento del PIB.")
              ),
              fluidRow(box(title = "Regresión cuantil de la variable poblacional sobre la económica",
                           helpText("Seleccione el tipo de tasa"),
                           selectInput("vari_e", 
                                       label = "Variable Económica",
                                       choices = coef_cuantiles_estados$var_econ,
                                       selected = "Opción_1bis"),
                           selectInput("vari_p", 
                                       label = "Variable Poblacional",
                                       choices = coef_cuantiles_estados$var_pob,
                                       selected = "Opción_1")
                        ),
              box(title = " ", "Esta segunda gráfica nos permite observar la regresión cuantil (por veintiles) de la variable poblacional 
                  sobre la variable económica, de modo que se pueda apreciar los cambios en correlación para los distintos niveles de la distribución 
                  de la variable económica. Destacan los casos de tasa de divorcios vs mediana del ingreso por hora, y de tasa de informalidad laboral 
                  vs tasa de nacimientos."),
              ),
              br(),
              fluidRow(plotlyOutput('coef_cuantiles')),
              fluidRow(box(title = "Coeficientes de la regresión cuantil",
                           helpText("Seleccione el tipo de tasa"),
                           selectInput("varia_e", 
                                       label = "Variable Económica",
                                       choices =nom_var_ols$econ_bueno,
                                       selected = "Opción_1bis"),
                           selectInput("varia_p", 
                                       label = "Variable Poblacional",
                                       choices = na.omit(nom_var_ols)$pob_bueno,
                                       selected = "Opción_1")
                        ),
              box(title = " ", "En esta sección se muestran los resultados de regresiones OLS de la variable poblacional sobre la variable económica. 
                  Se estiman 5 modelos, uno para cada periodo de rezago de la variable económica. Con esto se pretende identificar ciertos patrones de 
                  rezago del ciclo económico, pues es natural pensar que los efectos no suelen ser inmediatos. Los rezagos se presentan en trimestres, 
                  excepto para el coeficiente de Gini y la variable de pobreza, que son anuales. Destacan los casos de empleos en el IMSS vs esperanza de vida, 
                  y tasa de informalidad laboral vs tasa de natalidad."),
              ),
              fluidRow(imageOutput('image_ols'))
              
      ),
      tabItem(tabName = "econometria2",
              h2("Análisis econométrico por municipio", align = "center"),
              h4("Regresiones cuantiles para el estudio de la población.", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar un análisis de regresión cuantil para observar la relación
                de las variables económicas como el ingreso medio y el nivel de empleo sobre las variables poblaciones de acuerdo al cuantil de la 
                distribución."),br(),
              p(style="text-align: justify;","El método de regresión cuantil nos permite realizar una regresión sobre cualquier parte de la distribución, 
                permite conocer la influencia de los predictores desde el mínimo al máximo rango de la variable respuesta. Esto es especialmente útil en 
                modelos de regresión en los que no se cumple la condición de varianza constante, ya que esto significa que no hay un único ratio de cambio
                (pendiente) que represente bien a toda la variable respuesta a la vez."),
              br(),
              fluidRow(box(title = "Tasas medias por percentil poblacional",
                           helpText("Seleccione el tipo de tasa"),
                           selectInput("tasa_sel_tasa", 
                                       label = "Tipo de tasa",
                                       choices = tasas_mun,
                                       selected = "Opción"),
                           sliderInput("tasa_sel_anio", label = "Año",
                                       min = 2000, max = 2018, value =2000, step=1, animate=TRUE)),
                       box(title = " ", "Esta primer gráfica nos permite observar las tasas medias por percentil poblacional (determinado con base al ingreso) para los años 2000-2018. 
                           Se encuentra una relación negativa entre la tasa media de embarazos adolescentes y el ingreso, de tal forma que los percentiles con menor ingreso tienen mayores tasas 
                           de embarazo adolescente, para 2018 esta tasa fue de 26 y 18 (embarazos por cada 100,000 personas) para los percentiles 5 y 95 respectivamente para todos los años."
                           )
              ),
              fluidRow(
                       column(12,plotlyOutput('percent_tasa_b'))
              ),
              br(),
              fluidRow(box(title = "Evolución de las tasas medias por percentil poblacional",
                           selectInput("perct_sel_tasa", 
                                       label = "Tipo de tasa",
                                       choices = tasas_mun,
                                       selected = "Opción"),
                           selectInput("perc_sel_perct", label = "Percentil de comparación 1",
                                       choices = percentiles_municipios$perct, selected = "Opción"),
                           selectInput("perc_sel_perct_1", label = "Percentil de comparación  2",
                                       choices = percentiles_municipios$perct, selected = "Opción")
                           ),
                       box(title = " ",
                           selectInput("perc_sel_perct_2", label = "Percentil de comparación 3",
                                       choices = percentiles_municipios$perct, selected = "Opción"),
                           "Es importante señalar que estas tasas han bajado considerablemente para todos los percentiles con el paso de los años, 
                           los mismos percentiles (5 y 95) redujeron en casi 50% sus tasas de embarazos adolescentes entre 2000 y 2020 (de 50 a 16 y de 34 a 18, 
                           respectivamente). Este resultado es observado en todos los percentiles."
                       )
              ),
              fluidRow(
                       column(12,plotlyOutput('evol_tasa_b'))
              ),
              br(),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","Las tasas medias de nacimiento guardan una relación negativa con el percentil del ingreso (aunque menos pronunciada que el caso de los 
                   embarazos adolescentes). Lo que resulta más obvio para esta variable es la reducción de las tasas a lo largo del tiempo, pasando de 234 a 141 nacimientos 
                   por cada 100 mil personas (para el cuantil 50 de ingreso). Esta tendencia es observada para todos los percentiles.")
              ),
              fluidRow(box(title = "Regresión cuantil del ingreso sobre las tasas",
                           helpText("Seleccione el tipo de tasa y el periodo de rezago"),
                           selectInput("tasa_sel_qreg", 
                                       label = "Tipo de tasa",
                                       choices = tasas_mun,
                                       selected = "Opción"),
                           selectInput("periodo_ingreso", 
                                       label = "Periodo del ingreso",
                                       choices = per_ing,
                                       selected = "Opción")),
                       box(title = " ",
                           sliderInput("periodo_year1", label = "Año",
                                       min = 2000, max = 2018, value =2000, step=1, animate=TRUE),
                           "A continuación presentamos regresiones cuantiles con ingreso medio como variable explicativa y la tasa
                           seleccionada como variable de respuesta."
                       )
              ),
              fluidRow(column(6,plotlyOutput('tasas_perct_qreg')),
                       column(6,plotlyOutput('ratio_perct_qreg'))
              ),
              helpText("Las regresiones cuantiles de las tasas de embarazo adolescente, en general muestran coeficientes negativos mayores 
                       a 3 veces su error estándar, ya sea que se tome como variable explicativa el log del ingreso o alguno de sus primeros 
                       rezagos, aunque en algunos casos se pierde significancia principalmente en los primeros y últimos percentiles poblacionales. 
                       Los coeficientes obtenidos para las tasas de nacimiento presentan un comportamiento similar al de embarazos adolescentes. 
                       Remarcando la evidente relación que de ante mano sabíamos existía."),
              br(),
              fluidRow(box(title = "Regresión cuantil del empleo sobre las tasas",
                           helpText("Seleccione el tipo de tasa y el periodo de rezago"),
                           selectInput("tasa_sel_qreg_emp", 
                                       label = "Tipo de tasa",
                                       choices = tasas_mun,
                                       selected = "Opción"),
                           selectInput("periodo_empleo", 
                                       label = "Periodo del empleo",
                                       choices = per_ing,
                                       selected = "Opción")),
                       box(title = " ",
                           sliderInput("periodo_year2", label = "Año",
                                       min = 2000, max = 2018, value =2000, step=1, animate=TRUE),
                           "A continuación presentamos regresiones cuantiles el nivel de empleo como variable explicativa y la tasa
                           seleccionada como variable de respuesta."
                       )
              ),
              fluidRow(column(6,plotlyOutput('tasas_perct_qreg_emp')),
                       column(6,plotlyOutput('ratio_perct_qreg_emp'))
              ),
              helpText("Los coeficientes de las tasas de mortalidad presentan una relación negativa para los percentiles de la población 
                       más bajos, mientras que esta tasa se vuelve positiva para los percentiles más altos, en ambos casos los coeficientes 
                       son de más de 3 veces su error estándar. Es decir, el aumento en el ingreso aumenta las tasas de mortalidad para los 
                       percentiles más altos mientras que para los más bajos las tasas se ven disminuidas. Esta significancia se pierde para 
                       percentiles no extremos.")
      ),
      tabItem(tabName = "espacial",
              h2("Análisis económico-espacial de la población", align = "center"),
              br(),
              fluidRow(column(8,p(style="text-align: justify;","El objetivo de esta sección es presentar mapas coropléticos para ilustrar la relación de la población con las principales variables económicas para
                tener una imagen general de la diferencia geográfica de los fenómenos estudiados en este proyecto.")),
                       column(4,sliderInput(inputId = "year_comparacion_mapa", 
                                             label = "Año", 
                                             value = 1, min = 2010, max = 2018, step=1, animate=animationOptions(interval = 2000, loop = TRUE))),
                       column(6,selectInput("var_poblacion_mapa", 
                                            label = "Variable de población",
                                            choices = variables_poblacion,
                                            selected = 1
                                          )
                              ),
                       column(6,selectInput("var_economica_mapa", 
                                            label = "Variable económica",
                                            choices = variables_economicas,
                                            selected = 1
                                          )
                              )
              ),
              fluidRow(column(6,plotOutput('mapa_poblacion')),
                       column(6,plotOutput('mapa_economico'))
              ),
              fluidRow(column(12,plotOutput('mapa_ratio'))),
              box(
                 title = "¿Sabías qué?", width = NULL, solidHeader = TRUE, status = "warning",
                 p(align="center","\"De acuerdo con datos de las Proyecciones de la Población de México y de las Entidades Federativas 2016-2050 del Conapo, 
                 se estima que la población nacida en 2019 alcance en promedio los 75.1 años, una esperanza de vida mayor a la que se tenía hace 40 años 
                 (66.2 años).Las mujeres tienen una esperanza de vida superior a la de los hombres con una brecha de casi seis años. Para el año 2030, 
                 la esperanza de vida al nacimiento alcanzará los 76.7 años en promedio, para las mujeres será de 79.6 años y para los hombres de 73.8 años.
                 \" Fuente: Conapo")
              ),
              h2("¿Los ricos viven más tiempo?", align = "center"),
              p(style="text-align: justify;","El objetivo de esta sección es presentar un análisis espacial de la distribución de la esperanza
                de vida en las 32 entidades del país y entender la relación que tiene esta con el ingreso medio estatal. Este análisis está inspirado en
                un artículo reciente de Raj Chetty, Michael Stepner y Sarah Abraham :",tags$a(target="_blank",href="https://jamanetwork.com/journals/jama/article-abstract/2513561",
                                                                                              "The Association Between Income and Life Expectancy in the United States, 2001-2014"),
                ". Resumido de excelente forma en el artículo del New York Times ",tags$a(target="_blank",href="https://www.nytimes.com/interactive/2016/04/11/upshot/for-the-poor-geography-is-life-and-death.html",
                                                                                          "The Rich Live Longer Everywhere. For the Poor, Geography Matters.")),
              br(),
              fluidRow(box(title = "Comparación esperanza de vida e ingreso medio",
                           sliderInput("year_diff", label = "Año",
                                       min = 2010, max = 2018, value =2000, step=1, animate=animationOptions(interval = 3500, loop = TRUE)),
                           h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Diff_esperanza_vida.gif", "Descargar GIF Esp Vida. "),
                              a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Diff_esperanza_vida.png", " Descargar PNG Esp Vida"),align = "center"),
                           h5(a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Diff_ingreso.gif", "Descargar GIF Ingreso medio. "),
                              a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion/blob/main/gifs/Diff_ingreso.png", " Descargar PNG Ingreso medio"),align = "center"),
                           helpText("Elaboración propia con datos de INEGI y CONAPO.")
              ),
              box(title = " ",p("Las siguientes gráficas nos muestran las diferencias respecto a las media nacional de la esperanza de vida y el ingreso medio
                                por entidad federativa. Los estados púrpura se encuentran \"mejor\" en el sentido que tienen una mayor esperanza de vida y mayor ingreso
                                medio respecto a la media. Los estados amarillos se encuentran \"peor\" con una menor esperanza de vida e ingreso medio. La relación entre estas variables es
                                evidente a pesar de no contar con los mismos microdatos del estudio de Chetty et al. 2016")
              )),
              fluidRow(
                 column(6,plotOutput('mapa_dif_esperanza')),
                 column(6,plotOutput('mapa_dif_ingreso'))
              ),
              helpText(style="text-align: justify;","Es evidente que la región norte cumple con la relación positiva entre esperanza de vida e ingreso medio. La relación en el sur del país es más compleja,
                                estados como Guerrero y Oaxaca presentan una esperanza de vida casi un año por debajo de la media nacional pero sus salarios medios no se encuentran tan alejados
                                de la media. Esto puede ser parcialmente explicado porque estamos tomando salarios formales (Imss) en promedio mayores. Se requiere mayor investigación.")
              
      ),
      tabItem(tabName = "team",
              h1("El Colegio de México", align = "center"),
              h3("Econometría Aplicada 2020",br(),"Dr. Raymundo Campos Vázquez", align = "center"),
              br(),
              fluidRow(
                 box(h4(strong("Joel Castillo Espinosa"),align="center"),width = 4,
                     br(),
                     HTML('<center><img src="joel.png" width="100"></center>'),
                     br(),
                     a("jocastillo@colmex.mx", href = "mailto:jocastillo@colmex.mx"),align="center"),
                 box(h4(strong("Williams Gómez Cerino"),align="center"),width = 4,
                     br(),
                     HTML('<center><img src="will.png" width="100"></center>'),
                     br(),
                     a("wgomez@colmex.mx", href = "mailto:wgomez@colmex.mx"),align="center"),
                 box(h4(strong("Diego López Tamayo "),align="center"),width = 4,
                     br(),
                     HTML('<center><img src="diego.png" width="100"></center>'),
                     br(),
                     a("diego.lopez@colmex.mx", href = "mailto:diego.lopez@colmex.mx"),br(),
                     a("@diego_lopezt", href = "https://twitter.com/diego_lopezt"), align="center"),
                 
                 ),
              br(),
              HTML('<center><img src="colmex.jpg" width="100"></center>'),
              br(),
              h5("Este proyecto de visualización de datos se realizó con la paquetería de Shiny en lenguaje R con datos abiertos de Inegi, Conapo, Imss y el Banco Mundial.
              Está abierto a todo público, encontrará la documentación, bases de datos y scripts en el siguiente",
                 tags$a(target="_blank",href="https://github.com/diego-eco/proyecto_visualizacion","repositorio de GitHub")," con licencia de código abierto. Agradecemos sus comentarios.",br(), 
                 strong("Gracias por su visita."),align = "center"),
            
      )
   )
   )
)
