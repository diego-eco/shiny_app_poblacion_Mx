server <- function(input, output) { 
  # Inicia glosario_pob interactivo
  output$glosario_pob_var <- renderText({ 
    selec_conc_pob <- input$select_conceptos_pob
    glosario_pob_display <- glosario_pob%>%filter(concepto==selec_conc_pob)
    paste(glosario_pob_display$def[1])
  })
  # Termina glosario_pob interactivo
  # Inicia glosario_nat interactivo
  output$glosario_nat_var <- renderText({ 
    selec_conc_nat <- input$select_conceptos_nat
    glosario_nat_display <- glosario_nat%>%filter(concepto==selec_conc_nat)
    paste(glosario_nat_display$def[1])
  })
  # Termina glosario_nat interactivo
  # Inicia glosario_mor interactivo
  output$glosario_mor_var <- renderText({ 
    selec_conc_mor <- input$select_conceptos_mor
    glosario_mor_display <- glosario_mor%>%filter(concepto==selec_conc_mor)
    paste(glosario_mor_display$def[1])
  })
  # Termina glosario_mor interactivo
  # Inicia glosario_mat interactivo
  output$glosario_mat_var <- renderText({ 
    selec_conc_mat <- input$select_conceptos_mat
    glosario_mat_display <- glosario_mat%>%filter(concepto==selec_conc_mat)
    paste(glosario_mat_display$def[1])
  })
  # Termina glosario_mat interactivo
  # Inicia glosario_div interactivo
  output$glosario_div_var <- renderText({ 
    selec_conc_div <- input$select_conceptos_div
    glosario_div_display <- glosario_div%>%filter(concepto==selec_conc_div)
    paste(glosario_div_display$def[1])
  })
  # Termina glosario_div interactivo
  # Inicia glosario_int interactivo
  output$glosario_intern_var <- renderText({ 
    selec_conc_int <- input$var1_inter
    glosario_int_display <- glosario_intern%>%filter(concepto==selec_conc_int)
    paste(glosario_int_display$def[1])
  })
  # Termina glosario_int interactivo
  
  # Inicia glosario_int_eco interactivo
  output$glosario_intern_var_eco <- renderText({ 
    selec_conc_int_eco <- input$var2_inter
    glosario_int_eco_display <- glosario_intern_eco%>%filter(concepto==selec_conc_int_eco)
    paste(glosario_int_eco_display$def[1])
  })
  # Termina glosario_int_eco interactivo
  
  ###################################
  
  # Internacionales Pobla
  selectedData_inter <- reactive({
    internacional_sel<-internacionales%>%dplyr::filter(serie==input$var1_inter)
  })
  # Inicia gráficas internacionales
  output$plot1_internacional <- renderPlotly({
    plot_ly(selectedData_inter()) %>%
      add_trace(selectedData_inter(), x = ~year, y = ~México, type = 'scatter', mode = 'lines',name = "México")%>%
      add_trace(selectedData_inter(), x = ~year, y = ~USA, type = 'scatter', mode = 'lines',name = "USA")%>%
      add_trace(selectedData_inter(), x = ~year, y = ~Brasil, type = 'scatter', mode = 'lines',name = "Brasil")%>%
      add_trace(selectedData_inter(), x = ~year, y = ~Chile, type = 'scatter', mode = 'lines',name = "Chile") %>%
      add_trace(selectedData_inter(), x = ~year, y = ~Latinoamérica, type = 'scatter', mode = 'lines',name = "Latinoamérica") %>%
      add_trace(selectedData_inter(), x = ~year, y = ~OCDE, type = 'scatter', mode = 'lines',name = "OCDE") %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list (title = input$var1_inter))%>%
      layout(yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)) ) %>% layout(legend = list(orientation = 'h'))
  })
  
  # Internacionales Eco
  selectedData_inter_eco <- reactive({
    internacional_sel_eco<-internacionales_eco%>%dplyr::filter(serie==input$var2_inter)
  })
  # Inicia gráficas internacionales
  output$plot2_internacional <- renderPlotly({
    plot_ly(selectedData_inter_eco()) %>%
      add_trace(selectedData_inter_eco(), x = ~year, y = ~México, type = 'scatter', mode = 'lines',name = "México")%>%
      add_trace(selectedData_inter_eco(), x = ~year, y = ~USA, type = 'scatter', mode = 'lines',name = "USA")%>%
      add_trace(selectedData_inter_eco(), x = ~year, y = ~Brasil, type = 'scatter', mode = 'lines',name = "Brasil")%>%
      add_trace(selectedData_inter_eco(), x = ~year, y = ~Chile, type = 'scatter', mode = 'lines',name = "Chile") %>%
      add_trace(selectedData_inter_eco(), x = ~year, y = ~Latinoamérica, type = 'scatter', mode = 'lines',name = "Latinoamérica") %>%
      add_trace(selectedData_inter_eco(), x = ~year, y = ~OCDE, type = 'scatter', mode = 'lines',name = "OCDE") %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list (title = input$var2_inter))%>%
      layout(yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)) ) %>% layout(legend = list(orientation = 'h'))
  })
  # Termina gráficas internacionales
  
  # Inicia gráficas Joel Mortalidad
  
  ## selección de las bases 
  mort_sel <- reactive({
    mort_sel <-mort_causa_anio%>%dplyr::filter(year==input$mort_anio)
  })
  
  mort_causa_sel <- reactive({
    mort_causa_sel <-mort_causa_anio%>%dplyr::filter(causa==input$causa_mort)
  })
  
  # Gráfico 1 (barras por sexo)
  output$mort_sex <- renderPlotly({
    plot_ly(mort_sexo_anio) %>%
      add_bars(mort_sexo_anio,y = ~mort_hom, x=~year, name = 'Muertes hombres', mode='bar') %>% 
      add_bars(mort_sexo_anio,y = ~mort_muj, x=~year, name = 'Muertes mujeres', mode='bar') %>%
      layout(yaxis = list(title = 'Personas'),  
             xaxis = list(title = " ",range = input$mort_sexo_rango), barmode = 'stack')  %>% layout(legend = list(orientation = 'h'))
  })
  
  # Gráfico 2 (tendencia por sexo)
  output$tasa_sex <- renderPlotly({
    plot_ly(mort_sexo_anio) %>%
      add_trace(mort_sexo_anio, x = ~year, y = ~tasa_mor_m, mode = 'lines',name = "Mujeres")%>%
      add_trace(mort_sexo_anio, x = ~year, y = ~tasa_mor_h, mode = 'lines',name = "Hombres")%>%
      add_trace(mort_sexo_anio, x = ~year, y = ~tasa_mor_nac, mode = 'lines',name = "Nacional")%>%
      layout(title = "",
             xaxis = list(title = " ",range = input$mort_sexo_rango),
             yaxis = list (title = "Muertes por cada 100,000 personas"))  %>% layout(legend = list(orientation = 'h'))
    
  })
  
  # Gráfico 3 (tasas por causa)
  output$mort_causa_plot <- renderPlotly({
    plot_ly(mort_causa_sel() ) %>%
      add_trace(mort_causa_sel() , x = ~year, y = ~tasa_nac, mode = 'lines',name = "Nacional")%>%
      add_trace(mort_causa_sel() , x = ~year, y = ~tasa_m, mode = 'lines',name = "Mujeres")%>%
      add_trace(mort_causa_sel() , x = ~year, y = ~tasa_h, mode = 'lines',name = "Hombres")%>%
      layout(title = "Tasa de mortalidad en México",
             xaxis = list(title = "Año",range = c(2000,2015)),
             yaxis = list (title = "Muertes por cada 100,000 personas"))
    
  })
  
  # Gráfico 4 ( pastel hombres)
  output$mort_causa <- renderPlotly({
    plot_ly(mort_sel(), labels = ~causa, values =~muertes_hom, type = 'pie') %>%
      layout(title = 'Muertes por causa (hombres)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) #%>% layout(legend = list(orientation = 'h'))
  })
  
  # Gráfico 5 ( pastel mujeres)
  output$mort_causa1 <- renderPlotly({
    plot_ly(mort_sel(), labels = ~causa, values =~muertes_muj, type = 'pie') %>%
      layout(title = 'Muertes por causa (mujeres)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) #%>% layout(legend = list(orientation = 'h'))
  })
  
  
  # Termina gráficas Joel Mortalidad
  
  # Inicia Mapas Mortalidad Joel
  
  ################ Mapas con plotly ###############
  
  ## Primer mapa
  output$map1_morta_pl <- renderPlot({
    
    mort_sel_est_sexo <- mort_estado_sexo%>%dplyr::filter(tipo_tasa==input$sexo_est_mort)
    mort_sel_est_sexo <- mort_sel_est_sexo%>%dplyr::filter(year==input$sexo_est_anio)
    mort_sel_est_sexo$geometry <- mapa_ent$geometry 
    
    #mapa1_morta <- 
      ggplot(data = mort_sel_est_sexo) +
      geom_sf(aes(fill = tasa_mort, geometry= geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(limits=c(100,1000), direction = -1)+
      #scale_fill_gradient2(low="red4", mid="yellow", high = "darkgreen", na.value="gray60")+
      theme(panel.background= element_rect(fill = "white"),
            panel.grid=element_blank(),
            legend.background = element_rect(fill="white"),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=10, face="bold"), axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(caption = "Fuente: Elaboración propia con datos de INEGI y Conapo."
           , fill = "Tasa de mortalidad")
    
    #ggplotly(mapa1_morta)
  })
  
  ## Segundo mapa
  output$map2_morta_pl <- renderPlot({
    
    mort_sel_est_causa <- mort_estado_causa%>%dplyr::filter(causa==input$causa_est_mort)
    mort_sel_est_causa <- mort_sel_est_causa%>%dplyr::filter(year==input$causa_est_anio)
    mort_sel_est_causa$geometry <- mapa_ent$geometry 
    
    #mapa2_morta <- 
      ggplot(data = mort_sel_est_causa) +
      geom_sf(aes(fill = tasa_mort, geometry= geometry),color="white",size = 0.10)+
      scale_fill_viridis_b(limits=c(0,250), direction = -1)+
      #scale_fill_gradient2(low="red4", mid="yellow", high = "darkgreen", na.value="gray60")+
      theme(panel.background= element_rect(fill = "white"),
            panel.grid=element_blank(),
            legend.background = element_rect(fill="white"),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=10, face="bold"), axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(caption = "Fuente: Elaboración propia con datos de INEGI y Conapo."
           , fill = "Tasa de mortalidad")
    
    #ggplotly(mapa2_morta)
  })
  
  # Tercer mapa 
  
  output$map3_morta <- renderPlot({
    
    mort_gen_sel <- cbind(mort_muni$year,mort_muni$CVEGEO,mort_muni$mort_muerte)
    colnames(mort_gen_sel) <- c("year", "CVEGEO", "tasa_mort")
    mort_gen_sel <- as.data.frame(mort_gen_sel)
    
    mort_gen_sel <- mort_gen_sel%>%dplyr::filter(year==input$mort_gen_anio)
    mort_gen_sel1 <- merge(mapa_mun, mort_gen_sel,by = "CVEGEO", all.x=TRUE)
    mort_gen_sel1$tasa_mort <- as.numeric(mort_gen_sel1$tasa_mort)
    
    ggplot(data = mort_gen_sel1) +
      geom_sf(aes(fill = tasa_mort, geometry=geometry),color= NA)+
      scale_fill_viridis_b(limits=c(300,800), direction = -1)+
      #scale_fill_gradient(low = "#80FF00", high ="#CC0066" , na.value="lightgray")+
      theme(panel.background= element_rect(fill = "white"),
            panel.grid=element_blank(),
            legend.background = element_rect(fill="white"),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=10, face="bold"), axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(fill = "Tasa de mortalidad", 
           caption ="Fuente: Elaboración propia con datos de INEGI y Conapo.")
    
  })
  
  
  # Cuarto mapa 
  
  output$map4_morta <- renderPlot({
    if (input$mort_mun == "Enfermedades del corazón"){
      mort_mun_sel <- cbind(mort_muni$year,mort_muni$CVEGEO, mort_muni$mort_corazon)
    } else if (input$mort_mun == "Enfermedades del higado"){
      mort_mun_sel <- cbind(mort_muni$year,mort_muni$CVEGEO,mort_muni$mort_higado)
    } else if (input$mort_mun == "Tumor maligno"){
      mort_mun_sel <- cbind(mort_muni$year,mort_muni$CVEGEO,mort_muni$mort_tumor)
    } else if (input$mort_mun == "Diabetes"){
      mort_mun_sel <- cbind(mort_muni$year,mort_muni$CVEGEO,mort_muni$mort_diabetes)
    } else if (input$mort_mun == "Accidente"){
      mort_mun_sel <- cbind(mort_muni$year,mort_muni$CVEGEO,mort_muni$mort_accidente)
    } else if (input$mort_mun == "Homicidio"){
      mort_mun_sel <- cbind(mort_muni$year,mort_muni$CVEGEO,mort_muni$mort_homicidio)
    }
    
    colnames(mort_mun_sel) <- c("year", "CVEGEO", "tasa_mort")
    mort_mun_sel <- as.data.frame(mort_mun_sel)
    
    mort_mun_sel <- mort_mun_sel%>%dplyr::filter(year==input$mort_mun_anio)
    mort_mun_sel1 <- merge(mapa_mun, mort_mun_sel,by = "CVEGEO", all.x=TRUE)
    mort_mun_sel1$tasa_mort <- as.numeric(mort_mun_sel1$tasa_mort)
    
    ggplot(data = mort_mun_sel1) +
      geom_sf(aes(fill = tasa_mort, geometry=geometry),color= NA)+
      scale_fill_viridis_b(limits=c(0,250), direction = -1)+
      #scale_fill_gradient(low = "#80FF00", high ="#CC0066" , na.value="lightgray")+
      theme(panel.background= element_rect(fill = "white"),
            panel.grid=element_blank(),
            legend.background = element_rect(fill="white"),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=10, face="bold"), axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(fill = "Tasa de mortalidad", 
           caption ="Fuente: Elaboración propia con datos de INEGI y Conapo.")
    
  })
  
  # Termina Mapas Mortalidad Joel
  
  # Inicia Gráficas Will Embarazos por características de madre
  embarazos <- reactive({
    if (input$tip_emb=="Totales"){
      embarazos <- embarazos1
    }else{
      embarazos <- embarazos2
    }
  })
  
  base_emb_sel <- reactive({
    #internacional_sel<-internacionales%>%dplyr::filter(serie==input$var_inter)
    dplyr::filter(nombres_variables1, X1 %in% c(input$var_emb))[-1]%>%select_if(~ !any(is.na(.)))
  })
  
  nombres_catego <- reactive({
    #internacional_sel<-internacionales%>%dplyr::filter(serie==input$var_inter)
    dplyr::filter(nombres_buenos1, V1 %in% c(input$var_emb))[-1]%>%select_if(~ !any(is.na(.)))
  })
  
  
  output$emb_car <- renderPlotly({
    a <- plot_ly()
    for (i in 1:length(base_emb_sel())){
      prov <- cbind(embarazos()$anio,embarazos()[,as.character(base_emb_sel()[i])])
      names(prov) <- c("anio","y")
      a <- add_trace(a, data=prov, x = ~anio, y = ~y, mode = 'lines',name = nombres_catego()[i])
    }
    a%>%layout(xaxis = list(title = ""),
               yaxis = list (title = "Embarazos por cada 100 mil habs."))
  })

  # Termina Gráficas Will Embarazos por características de madre
  
  # Inica gráficas Will Embarazasos por región
  
  emba_ado_area <- reactive({
    embarazos_area%>%dplyr::filter(area==input$var_area)
  })
  
  output$emb_area <- renderPlotly({
    a <- plot_ly()
    for (i in unique((emba_ado_area())$nombre)){
      prov <- emba_ado_area()%>%dplyr::filter(nombre==as.character(i))
      names(prov) <- c("nombre","anio","area","y")
      a <- add_trace(a, data=prov, x = ~anio, y = ~y, mode = 'lines',
                     name = as.character(i), annotations = as.character(i))
    }
    a%>%layout(xaxis = list(title = ""),
               yaxis = list (title = "Embarazos adolescentes por cada 100 mil habs."))
  })
  
  # Termina gráficas Will Embarazos por región
  
  # Inicia Mapas Will Embarazos adolecentes por entidad
  
  entidad_anio_sel <- reactive({
    map_sel_entidad <- emb_mapa_estados[,c("geometry",paste0("emb",input$year_map_entidad_emba))]
    names(map_sel_entidad) <- c("geometry","y")
    map_sel_entidad
  })
  
  mun_anio_sel <- reactive({
    map_sel_mun <- emb_mapa_muni[,c("geometry",paste0("emb",input$year_map_muni_emba))]
    names(map_sel_mun) <- c("geometry","y")
    map_sel_mun
  })
  
  region_anio_sel <- reactive({
    map_sel_region <- emb_mapa_reg[,c("geometry",paste0("emb",input$year_map_region_emba))]
    names(map_sel_region) <- c("geometry","y")
    map_sel_region
  })
  
  output$map_entidad_emba <- renderPlot({
    #mapita <- 
      ggplot(data = entidad_anio_sel()) +
      geom_sf(aes(fill = y, geometry=geometry), color="white",size = 0.10)+
      #scale_fill_gradient2(low="red4", mid="yellow", high = "darkgreen", na.value="gray60")+
      scale_fill_viridis_b(limits=c(90,600), direction = -1)+
      theme(panel.background= element_blank(),
            panel.grid=element_blank(),
            legend.background = element_blank(),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=11, face="bold", hjust=0.5),
            legend.text = element_text(size=10, face="bold", hjust = 0.5),
            axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(fill = "Embarazos")
    #ggplotly(mapita)
  })
  
  output$map_region_emba <- renderPlot({
    #mapita <- 
      ggplot(data = region_anio_sel()) +
      geom_sf(aes(fill = y, geometry=geometry), color="white",size = 0.10)+
      #scale_fill_gradient2(low="red4", mid="yellow", high = "darkgreen", na.value="gray60")+
      scale_fill_viridis_b(limits=c(90,600), direction = -1)+
      theme(panel.background= element_blank(),
            panel.grid=element_blank(),
            legend.background = element_blank(),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=11, face="bold", hjust=0.5),
            legend.text = element_text(size=10, face="bold", hjust = 0.5),
            axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(fill = "Embarazos")
    #ggplotly(mapita)
  })
  
  output$map_muni_emba <- renderPlot({
    ggplot(data = mun_anio_sel()) +
      geom_sf(aes(fill = y, geometry=geometry), color = NA)+
      #scale_fill_gradient2(low="red4", mid="yellow", high = "darkgreen", na.value="gray60")+
      scale_fill_viridis_b(limits=c(90,600), direction = -1)+
      theme(panel.background= element_blank(),
            panel.grid=element_blank(),
            legend.background = element_blank(),
            legend.position = c(0.9, 0.7),
            legend.title =element_text(size=11, face="bold", hjust=0.5),
            legend.text = element_text(size=10, face="bold", hjust = 0.5),
            axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(fill = "Embarazos")
  })
  

  # Termina Mapas Will Embarazos adolecentes por entidad
  
  # Inicia Mapa Will Embarazos adolecentes por municipio
  mapa_anio_emb_ent <- reactive({
    map_sel <- embarazos_ent_map[,c("geometry",paste0("emb",input$year_ent_ado))]
    names(map_sel) <- c("geometry","y")
    map_sel
  })
  
  output$map2_emb <- renderPlot({
    ggplot(data = mapa_anio_emb_ent()) +
      geom_sf(aes(fill = y, geometry=geometry), color = NA)+
      scale_fill_gradient2(low="red4", mid="yellow", high = "darkgreen", na.value="gray60")+
      theme(panel.background=element_blank(), legend.position = c(0.9, 0.7),
            legend.title =element_blank(),
            legend.text = element_text(size=15), 
            axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(caption = "Fuente: Elaboración propia con datos de INEGI y Conapo.
           Los cálculos se realizaron sobre la población de los municipios con más de 100 mil habs.",
           title= "", fill = "Embarazos")
  })

  # Termina Mapa Will Embarazos adolecentes por municipio
  
  # Inicia Gráficas Visual Diego Nupcialidad
  
  tendencia_divorc <- reactive({
    tendencia_div<-divorcios_anio%>%dplyr::select(year,input$selec_divorcio)
    names(tendencia_div)<-c("year","selected_var")
    tendencia_div
  })
  
  output$tendencia_divorcio <- renderPlotly({
    plot_ly(tendencia_divorc()) %>%
      add_trace(tendencia_divorc(), x = ~year, y = ~selected_var, type = "scatter", mode = 'lines')%>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list (title = input$selec_divorcio))
  })
  
  tendencia_matrim <- reactive({
    tendencia_mat<-matrimonios_anio%>%dplyr::select(year,input$selec_matrimonio)
    names(tendencia_mat)<-c("year","selected_var")
    tendencia_mat
  })
  
  output$tendencia_matrimonio <- renderPlotly({
    plot_ly(tendencia_matrim()) %>%
      add_trace(tendencia_matrim(), x = ~year, y = ~selected_var, type = "scatter", mode = 'lines')%>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list (title = input$selec_matrimonio))
  })
  
  divor_sel <- reactive({
    divor_sel <-divorcios_causa_anio%>%dplyr::filter(year==input$divorcios_anio)
  })
  
  divor_sel_sinmutuo <- reactive({
    divor_sel <-divorcios_causa_anio_sinmutuo%>%dplyr::filter(year==input$divorcios_anio)
  })
  
  # Gráfico Pastel divorcios1
  output$divorcio_causa <- renderPlotly({
    plot_ly(divor_sel(), labels = ~causa, values =~divorcios, type = 'pie') %>%
      layout(title = ' ',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% layout(legend = list(orientation = 'h'))
  })
  
  # Gráfico Pastel divorcios2
  output$divorcio_causa_sinmutuo <- renderPlotly({
    plot_ly(divor_sel_sinmutuo(), labels = ~causa, values =~divorcios, type = 'pie') %>%
      layout(title = ' ',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% layout(legend = list(orientation = 'h'))
  })
  
  matrim_selec <- reactive({
    matrim_sel <-matrimonios_tipo_anio%>%dplyr::filter(year==input$matrimonios_anio,clase==input$matrimonios_clase)
  })
  
  # Gráfico Pastel matrimonios1
  output$matrimonios1 <- renderPlotly({
    plot_ly(matrim_selec(), labels = ~tipo, values =~matrimonios, type = 'pie') %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% layout(legend = list(orientation = 'h'))
  })
  
  # Terminan Gráficas Visual Diego Nupcialidad
  
  # Inician Mapas Diego Nupcialidad
  
  ## Primer Mapa
  output$mat_entidad <- renderPlot({
    variable <- input$var_mat_entidad
    year <- input$year_mat_entidad
    datos_mat_ent<- matrimonios_ent[matrimonios_ent$año==year,]
    datos_mat_ent <- datos_mat_ent%>%select(CVE_EDO,variable)
    datos_mat_ent<- dplyr::left_join(mapa_ent, datos_mat_ent,by = "CVE_EDO")
    var_ent <- datos_mat_ent %>%pull(variable) 
    
    #mapa_mat_entidad <- 
      ggplot(data = datos_mat_ent) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(#limits=c(1000,10000), 
        direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = " ",
           subtitle = "",
           caption = "Fuente: Elaboración propia con datos de INEGI",
           fill = "Matrimonios") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    #ggplotly(mapa_mat_entidad)
  })
  
  ## Segundo Mapa
  output$div_entidad <- renderPlot({
    variable <- input$var_div_entidad
    year <- input$year_div_entidad
    datos_div_ent<- divorcios_ent[divorcios_ent$año==year,]
    datos_div_ent <- datos_div_ent%>%select(CVE_EDO,variable)
    datos_div_ent<- dplyr::left_join(mapa_ent, datos_div_ent,by = "CVE_EDO")
    var_ent <- datos_div_ent %>%pull(variable) 
    
    #mapa_div_entidad <- 
      ggplot(data = datos_div_ent) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(#limits=c(1000,10000), 
        direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = " ",
           subtitle = "",
           caption = "Fuente: Elaboración propia con datos de INEGI",
           fill = "Divorcios") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    #ggplotly(mapa_div_entidad)
  })
  
  ## Tercer Mapa
  output$mat_munic <- renderPlot({
    variable <- input$var_mat_muni
    year <- input$year_mat_muni
    datos_mat_mun<- matrimonios_mun[matrimonios_mun$año==year,]
    datos_mat_mun <- datos_mat_mun%>%select(CVEGEO,variable)
    datos_mat_mun<- dplyr::left_join(mapa_mun, datos_mat_mun,by = "CVEGEO")
    var_mun <- datos_mat_mun %>%pull(variable) 
    
    ggplot(data = datos_mat_mun) +
      geom_sf(aes(fill = var_mun, geometry=geometry), color = NA)+
      scale_fill_viridis_b(#limits=c(1000,10000), 
        direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = " ",
           subtitle = "",
           caption = "Fuente: Elaboración propia con datos de INEGI",
           fill = "Matrimonios") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  ## Cuarto Mapa
  output$div_munic <- renderPlot({
    variable <- input$var_div_muni
    year <- input$year_div_muni
    datos_div_mun<- divorcios_mun[divorcios_mun$año==year,]
    datos_div_mun <- datos_div_mun%>%select(CVEGEO,variable)
    datos_div_mun<- dplyr::left_join(mapa_mun, datos_div_mun,by = "CVEGEO")
    var_mun <- datos_div_mun %>%pull(variable) 
    
    ggplot(data = datos_div_mun) +
      geom_sf(aes(fill = var_mun, geometry=geometry), color = NA)+
      scale_fill_viridis_b(#limits=c(1000,10000), 
        direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = " ",
           subtitle = "",
           caption = "Fuente: Elaboración propia con datos de INEGI",
           fill = "Divorcios") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  # Terminan Mapas Diego Nupcialidad
  
  # Inicia Diego Análisis Mapas Chetty  
  
  ## Primer Mapa
  output$mapa_esperanza <- renderPlot({
    year_sel <- input$year_esperanza
    variable <- input$var_esperanza
    datos_esperanza<- chetty[chetty$year==year_sel,]
    datos_esperanza <- datos_esperanza%>%select(CVE_EDO,variable)
    datos_esperanza<- dplyr::left_join(mapa_ent, datos_esperanza,by = "CVE_EDO")
    var_ent <- datos_esperanza %>%pull(variable) 
    
    #mapa_esperanzaa <- 
      ggplot(data = datos_esperanza) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(#limits=c(69,80), 
        direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = " ",
           subtitle = "",
           caption = "Fuente: Elaboración propia con datos de INEGI",
           fill = input$var_esperanza) +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    #ggplotly(mapa_esperanzaa)
  })
  
  ## Segundo Mapa
  output$mapa_dif_esperanza <- renderPlot({
    year_sel <- input$year_diff
    datos_dif_esp<- chetty[chetty$year==year_sel,]
    datos_dif_esp <- datos_dif_esp%>%select(CVE_EDO,diferencia_esp_vida_nac)
    datos_dif_esp<- dplyr::left_join(mapa_ent, datos_dif_esp,by = "CVE_EDO")
    var_ent <- datos_dif_esp %>%pull(diferencia_esp_vida_nac) 
    ggplot(data = datos_dif_esp) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      ggplot2::scale_fill_gradient2(low = "#FDE725FF", mid="white",high = "#440154FF")+
      labs(x = NULL,
           y = NULL,
           title = "Esperanza de vida promedio por entidad",
           subtitle = "Diferencias respecto a la media nacional para el año seleccionado",
           caption = "",
           fill = "Diff Esp Vida") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  ## Tercer Mapa
  output$mapa_dif_ingreso <- renderPlot({
    year_sel <- input$year_diff
    datos_dif_esp<- chetty[chetty$year==year_sel,]
    datos_dif_esp <- datos_dif_esp%>%select(CVE_EDO,diferencia_log_salario_nac)
    datos_dif_esp<- dplyr::left_join(mapa_ent, datos_dif_esp,by = "CVE_EDO")
    var_ent <- datos_dif_esp %>%pull(diferencia_log_salario_nac) 
    ggplot(data = datos_dif_esp) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      ggplot2::scale_fill_gradient2(low = "#FDE725FF", mid="white",high = "#440154FF")+
      labs(x = NULL,
           y = NULL,
           title = "Ingreso promedio por entidad",
           subtitle = "Diferencias respecto a la media nacional para el año seleccionado",
           caption = "",
           fill = "Diff Salarios") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  ## Mapa población
  output$mapa_poblacion <- renderPlot({
    year_sel <- input$year_comparacion_mapa
    variable <- input$var_poblacion_mapa
    datos_poblacion<- comparacion[comparacion$year==year_sel,]
    datos_poblacion <- datos_poblacion%>%select(CVE_EDO,variable)
    datos_poblacion<- dplyr::left_join(mapa_ent, datos_poblacion,by = "CVE_EDO")
    var_ent <- datos_poblacion %>%pull(variable) 
    ggplot(data = datos_poblacion) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = "",
           subtitle = "",
           caption = "",
           fill = input$var_poblacion_mapa) +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  ## Mapa económico
  output$mapa_economico <- renderPlot({
    year_sel <- input$year_comparacion_mapa
    variable <- input$var_economica_mapa
    datos_poblacion<- comparacion[comparacion$year==year_sel,]
    datos_poblacion <- datos_poblacion%>%select(CVE_EDO,variable)
    datos_poblacion<- dplyr::left_join(mapa_ent, datos_poblacion,by = "CVE_EDO")
    var_ent <- datos_poblacion %>%pull(variable) 
    ggplot(data = datos_poblacion) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = "",
           subtitle = "",
           caption = "",
           fill = input$var_economica_mapa) +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  ## Mapa ratios
  output$mapa_ratio <- renderPlot({
    year_sel <- input$year_comparacion_mapa
    variable1 <- input$var_poblacion_mapa
    variable2 <- input$var_economica_mapa
    datos_poblacion<- comparacion[comparacion$year==year_sel,]
    datos_poblacion <- datos_poblacion%>%select(CVE_EDO,variable1,variable2)
    names(datos_poblacion)<-c("CVE_EDO","variable1","variable2")
    datos_poblacion<- dplyr::left_join(mapa_ent, datos_poblacion,by = "CVE_EDO")
    datos_poblacion <- datos_poblacion%>%mutate(ratio=variable1/variable2)
    var_ent <- datos_poblacion %>%pull(ratio) 
    ggplot(data = datos_poblacion) +
      geom_sf(aes(fill = var_ent, geometry=geometry), color="white",size = 0.10)+
      scale_fill_viridis_b(direction = -1)+
      labs(x = NULL,
           y = NULL,
           title = paste("Ratio ",variable1,"/",variable2),
           subtitle = "",
           caption = "",
           fill = "Ratio") +
      theme_minimal()+
      theme(legend.position = "right",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  
  # Termina Diego Análisis Mapas Chetty  
  
  # Inicia clustering corr Diego
  
  # Cluster 2D
  comparacion_sel <- reactive({
    comparacion_nuevo%>%dplyr::filter(year==input$year_cluster) %>%dplyr::select(input$xcol, input$ycol)
  })
  
  clusters <- reactive({
    kmeans(comparacion_sel(), input$clusters)
  })
  
  output$clustering2D <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar = c(5.1, 4.1, 0, 1))
    plot(comparacion_sel(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 2, lwd = 2)
    text(comparacion_sel(), nomb, cex=0.6, pos=1, col="black")
  })
  
  # Plotly 3D
  comparacion_sel1 <- reactive({
    comparacion_nuevo%>%dplyr::filter(year==input$year_cluster1) %>%dplyr::select(input$xcol1, input$ycol1, input$zcol1)
  })
  
  clusters1 <- reactive({
    kmeans<-kmeans(comparacion_sel1(), input$clusters1)
  })
  
  output$clustering3D <- renderPlotly({
    varx<- comparacion_sel1()%>%pull(input$xcol1) 
    vary<- comparacion_sel1()%>%pull(input$ycol1) 
    varz<- comparacion_sel1()%>%pull(input$zcol1) 
    kmeans1<-clusters1()$cluster
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    plot_ly(x = ~varx, y = ~vary, z = ~varz,mode = 'text', text = ~nombres, marker = list(color = ~kmeans1,
                                                                                          colorscale='Viridis') ) %>% 
      add_markers() %>% layout(scene = list(xaxis = list(title = input$xcol1),
                                            yaxis = list(title = input$ycol1),
                                            zaxis = list(title = input$zcol1))
      )
    
  })
  
  corr1 <- reactive({
    correlations%>%dplyr::filter(year==input$year_corr)
    correlations[2:10]
  })
  
  corr2 <- reactive({
    correlations%>%dplyr::filter(year==input$year_corr)
    correlations[11:20]
  })
  
  corr3 <- reactive({
    correlations%>%dplyr::filter(year==input$year_corr)
    correlations[23:31]
  })
  
  output$correlation1 <- renderPlot({
    corr <- round(cor(corr1()), 2)
    ggcorrplot(corr, method = "circle",colors = c("red", "white", "blue"))
  })
  
  output$correlation2 <- renderPlot({
    corr <- round(cor(corr2()), 2)
    ggcorrplot(corr, method = "circle",colors = c("red", "white", "blue"))
  })
  
  output$correlation3 <- renderPlot({
    corr <- round(cor(corr3()), 2)
    ggcorrplot(corr, method = "circle",colors = c("red", "white", "blue"))
  })
  
  # Termina clustering corr Diego
  
  # Inicia gráficos análisis Joel

  # Gráfico 2 (Tendencia por percentil en el año seleccionado) Burbujas 
  output$percent_tasa_b <- renderPlotly({
    perc_mun_year <- percentiles_municipios%>% dplyr::filter(year==input$tasa_sel_anio)
    if(input$tasa_sel_tasa== "Tasa de mortalidad"){ 
      perc_mun_year$Tasa <- perc_mun_year$tasa_mort
      ytit <- "Tasa media de mortalidad"
      yrange <- c(20,80)
      scalar <- 20}
    else if (input$tasa_sel_tasa == "Tasa de nacimientos"){ 
      perc_mun_year$Tasa <- perc_mun_year$tasa_nac 
      ytit <- "Tasa media de nacimientos"
      yrange <- c(100,300)
      scalar <- 100}
    else if (input$tasa_sel_tasa == "Tasa de embarazos adolescentes"){ 
      perc_mun_year$Tasa <- perc_mun_year$tasa_emb_ado
      ytit <- "Tasa media de embrazos adolecentes"
      yrange <- c(15,70)
      scalar <- 15}
    
    plot_ly(perc_mun_year, text=~paste("Percentil", perct, sep=" "),
            x= ~perct, y= ~Tasa, type = "scatter", #colors = "Blues",
            mode="markers", size = ~Tasa - scalar,color= ~Tasa, name = input$tasa_sel_tasa,
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')))%>% 
      layout(xaxis = list(title = "Percentil",range = c(-5,110), zerolinewidth = 0),
             yaxis = list (title = ytit, range=yrange))
    
  })
  
  
  # Gráfico 4 (Evolución de tasa media por percentil) burbujas 
  output$evol_tasa_b <- renderPlotly({
    perc_mun_year_1 <- percentiles_municipios%>% dplyr::filter(perct==input$perc_sel_perct)
    perc_mun_year_2 <- percentiles_municipios%>% dplyr::filter(perct==input$perc_sel_perct_1)
    perc_mun_year_3 <- percentiles_municipios%>% dplyr::filter(perct==input$perc_sel_perct_2)
    
    if(input$perct_sel_tasa == "Tasa de mortalidad"){ 
      perc_mun_year_1$tasa <- perc_mun_year_1$tasa_mort
      perc_mun_year_1$tasa_1 <- perc_mun_year_2$tasa_mort
      perc_mun_year_1$tasa_2 <- perc_mun_year_3$tasa_mort
      ytit <- "Tasa media de mortalidad"
      yrange <- c(20,65)
      scalar1 <- 20
      scalar2 <- 10}
    else if (input$perct_sel_tasa == "Tasa de nacimientos"){ 
      perc_mun_year_1$tasa <- perc_mun_year_1$tasa_nac 
      perc_mun_year_1$tasa_1 <- perc_mun_year_2$tasa_nac
      perc_mun_year_1$tasa_2 <- perc_mun_year_3$tasa_nac
      ytit <- "Tasa media de nacimientos"
      yrange <- c(100,280) 
      scalar1 <- 100
      scalar2 <- 50}
    else if (input$perct_sel_tasa == "Tasa de embarazos adolescentes"){ 
      perc_mun_year_1$tasa <- perc_mun_year_1$tasa_emb_ado
      perc_mun_year_1$tasa_1 <- perc_mun_year_2$tasa_emb_ado
      perc_mun_year_1$tasa_2 <- perc_mun_year_3$tasa_emb_ado
      ytit <- "Tasa media de embrazos adolecentes"
      yrange <- c(15,50)
      scalar1 <- 15
      scalar2 <- 10}
    
    
    plot_ly(perc_mun_year_1) %>% 
      add_trace(perc_mun_year_1, text=~paste("Año", year, sep=" "),
                x= ~year, y= ~tasa, type = "scatter",
                mode="markers", color=paste("Percentil", input$perc_sel_perct, sep=" "),
                marker = list(symbol = 'circle', size = ~(tasa*10 - scalar1)/scalar2
                              , opacity =0.8,sizemode = 'diameter'
                              ,line = list(width = 2, color = '#FFFFFF')))%>% 
      add_trace(perc_mun_year_1, text=~paste("Año", year, sep=" "),
                x= ~year, y= ~tasa_1, type = "scatter", 
                mode="markers", color=paste("Percentil", input$perc_sel_perct_1, sep=" "),
                marker = list(symbol = 'circle', size = ~(tasa_1*10 - scalar1)/scalar2 , 
                              opacity =0.8,sizemode = 'diameter'
                              ,line = list(width = 2, color = '#FFFFFF'))) %>%
      add_trace(perc_mun_year_1, text=~paste("Año", year, sep=" "),
                x= ~year, y= ~tasa_2, type = "scatter",
                mode="markers", color= paste("Percentil", input$perc_sel_perct_2, sep=" "), 
                marker = list(symbol = 'circle', size = ~(tasa_2*10 - scalar1)/scalar2 
                              , opacity =0.8,sizemode = 'diameter'
                              ,line = list(width = 2, color = '#FFFFFF'))) %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list (title = ytit, range=yrange))
    
  })
  
  # Gráfico 5 ( ols vs qreg por año) Efecto del ingreso en las tasas 
  output$tasas_perct_qreg <- renderPlotly({ 
    if(input$tasa_sel_qreg == "Tasa de mortalidad" & input$periodo_ingreso =="Sin rezago"){ 
      coef1 <- coef_mort_1 
      coef1$tasa <- coef1$ingreso/10
      coef1$tasa_ols <- coef1$ingreso_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de mortalidad" & input$periodo_ingreso =="1 rezago (1 mes)"){ 
      coef1 <- coef_mort_2 
      coef1$tasa <- coef1$ingreso_1/10
      coef1$tasa_ols <- coef1$ingreso_1_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de mortalidad" & input$periodo_ingreso =="2 rezagos (2 meses)"){ 
      coef1 <- coef_mort_3 
      coef1$tasa <- coef1$ingreso_2/10
      coef1$tasa_ols <- coef1$ingreso_2_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de embarazos adolescentes" & input$periodo_ingreso =="Sin rezago"){ 
      coef1 <- coef_emb_1 
      coef1$tasa <- coef1$ingreso/10
      coef1$tasa_ols <- coef1$ingreso_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de embarazos adolescentes" & input$periodo_ingreso =="1 rezago (1 mes)"){ 
      coef1 <- coef_emb_2 
      coef1$tasa <- coef1$ingreso_1/10
      coef1$tasa_ols <- coef1$ingreso_1_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de embarazos adolescentes" & input$periodo_ingreso =="2 rezagos (2 meses)"){ 
      coef1 <- coef_emb_3 
      coef1$tasa <- coef1$ingreso_2/10
      coef1$tasa_ols <- coef1$ingreso_2_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de nacimientos" & input$periodo_ingreso =="Sin rezago"){ 
      coef1 <- coef_nac_1 
      coef1$tasa <- coef1$ingreso/10
      coef1$tasa_ols <- coef1$ingreso_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de nacimientos" & input$periodo_ingreso =="1 rezago (1 mes)"){ 
      coef1 <- coef_nac_2
      coef1$tasa <- coef1$ingreso_1/10
      coef1$tasa_ols <- coef1$ingreso_1_ols/10}
    else if(input$tasa_sel_qreg == "Tasa de nacimientos" & input$periodo_ingreso =="2 rezagos (2 meses)"){ 
      coef1 <- coef_nac_3 
      coef1$tasa <- coef1$ingreso_2/10
      coef1$tasa_ols <- coef1$ingreso_2_ols/10}
    
    coef_sel <-coef1%>%dplyr::filter(year==input$periodo_year1)
    
    plot_ly(coef_sel) %>% add_trace(coef_sel, x=~n, y=~tasa, 
                                    mode="lines", name="Coef: Reg. Cuantil") %>%
      add_trace(coef_sel, x=~n, y=~tasa_ols, mode="lines", name="Coeficiente: OLS")%>%
      layout(xaxis = list(title = "Percentil"),
             yaxis = list (title = "Coeficiente (por cada 10% del ingreso)"))%>% layout(legend = list(orientation = 'h',x = 0.50, y = -0.2))
    
  })
  
  # Gráfico 6 ( ratio coefeiciente/ee) Efecto del ingreso en las tasas 
  output$ratio_perct_qreg <- renderPlotly({ 
    if(input$tasa_sel_qreg == "Tasa de mortalidad" & input$periodo_ingreso =="Sin rezago"){ 
      coef1 <- coef_mort_1 
      coef1$ratio <- coef1$ingreso/coef1$ingreso_se
      coef1$ratio_ols <- coef1$ingreso_ols/coef1$ingreso_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de mortalidad" & input$periodo_ingreso =="1 rezago (1 mes)"){ 
      coef1 <- coef_mort_2 
      coef1$ratio <- coef1$ingreso_1/coef1$ingreso_1_se
      coef1$ratio_ols <- coef1$ingreso_1_ols/coef1$ingreso_1_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de mortalidad" & input$periodo_ingreso =="2 rezagos (2 meses)"){ 
      coef1 <- coef_mort_3 
      coef1$ratio <- coef1$ingreso_2/coef1$ingreso_2_se
      coef1$ratio_ols <- coef1$ingreso_2_ols/coef1$ingreso_2_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de embarazos adolescentes" & input$periodo_ingreso =="Sin rezago"){ 
      coef1 <- coef_emb_1 
      coef1$ratio <- coef1$ingreso/coef1$ingreso_se
      coef1$ratio_ols <- coef1$ingreso_ols/coef1$ingreso_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de embarazos adolescentes" & input$periodo_ingreso =="1 rezago (1 mes)"){ 
      coef1 <- coef_emb_2 
      coef1$ratio <- coef1$ingreso_1/coef1$ingreso_1_se
      coef1$ratio_ols <- coef1$ingreso_1_ols/coef1$ingreso_1_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de embarazos adolescentes" & input$periodo_ingreso =="2 rezagos (2 meses)"){ 
      coef1 <- coef_emb_3 
      coef1$ratio <- coef1$ingreso_2/coef1$ingreso_2_se
      coef1$ratio_ols <- coef1$ingreso_2_ols/coef1$ingreso_2_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de nacimientos" & input$periodo_ingreso =="Sin rezago"){ 
      coef1 <- coef_nac_1 
      coef1$ratio <- coef1$ingreso/coef1$ingreso_se
      coef1$ratio_ols <- coef1$ingreso_ols/coef1$ingreso_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de nacimientos" & input$periodo_ingreso =="1 rezago (1 mes)"){ 
      coef1 <- coef_nac_2
      coef1$ratio <- coef1$ingreso_1/coef1$ingreso_1_se
      coef1$ratio_ols <- coef1$ingreso_1_ols/coef1$ingreso_1_se_ols}
    else if(input$tasa_sel_qreg == "Tasa de nacimientos" & input$periodo_ingreso =="2 rezagos (2 meses)"){ 
      coef1 <- coef_nac_3 
      coef1$ratio <- coef1$ingreso_2/coef1$ingreso_2_se
      coef1$ratio_ols <- coef1$ingreso_2_ols/coef1$ingreso_2_se_ols}
    
    coef_sel <-coef1%>%dplyr::filter(year==input$periodo_year1)
    
    plot_ly(coef_sel) %>% 
      add_trace(coef_sel, x=~n, y=~ratio, type="scatter",  mode="markers", 
                name="Ratio: Reg. Cuantil", color="Red", size=1) %>%
      add_trace(coef_sel, x=~n, y=~ratio_ols, mode="lines", 
                name="Ratio: OLS", color="Blue") %>%
      layout(xaxis = list(title = "Percentil"),
             yaxis = list (title = "Ratio coeficiente del ingreso/error estándar"))%>% layout(legend = list(orientation = 'h',x = 0.50, y = -0.2))
  })
  
  
  # Gráfico 7 ( ols vs qreg por año) Efecto del empleo en las tasas 
  output$tasas_perct_qreg_emp <- renderPlotly({ 
    if(input$tasa_sel_qreg_emp == "Tasa de mortalidad" & input$periodo_empleo =="Sin rezago"){ 
      coef1 <- coef_mort_1 
      coef1$tasa <- coef1$empleo*10000
      coef1$tasa_ols <- coef1$empleo_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de mortalidad" & input$periodo_empleo =="1 rezago (1 mes)"){ 
      coef1 <- coef_mort_2 
      coef1$tasa <- coef1$empleo_1*10000
      coef1$tasa_ols <- coef1$empleo_1_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de mortalidad" & input$periodo_empleo =="2 rezagos (2 meses)"){ 
      coef1 <- coef_mort_3 
      coef1$tasa <- coef1$empleo_2*10000
      coef1$tasa_ols <- coef1$empleo_2_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de embarazos adolescentes" & input$periodo_empleo =="Sin rezago"){ 
      coef1 <- coef_emb_1 
      coef1$tasa <- coef1$empleo*10000
      coef1$tasa_ols <- coef1$empleo_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de embarazos adolescentes" & input$periodo_empleo =="1 rezago (1 mes)"){ 
      coef1 <- coef_emb_2 
      coef1$tasa <- coef1$empleo_1*10000
      coef1$tasa_ols <- coef1$empleo_1_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de embarazos adolescentes" & input$periodo_empleo =="2 rezagos (2 meses)"){ 
      coef1 <- coef_emb_3 
      coef1$tasa <- coef1$empleo_2*10000
      coef1$tasa_ols <- coef1$empleo_2_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de nacimientos" & input$periodo_empleo =="Sin rezago"){ 
      coef1 <- coef_nac_1 
      coef1$tasa <- coef1$empleo*10000
      coef1$tasa_ols <- coef1$empleo_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de nacimientos" & input$periodo_empleo =="1 rezago (1 mes)"){ 
      coef1 <- coef_nac_2
      coef1$tasa <- coef1$empleo_1*10000
      coef1$tasa_ols <- coef1$empleo_1_ols*10000}
    else if(input$tasa_sel_qreg_emp == "Tasa de nacimientos" & input$periodo_empleo =="2 rezagos (2 meses)"){ 
      coef1 <- coef_nac_3 
      coef1$tasa <- coef1$empleo_2*10000
      coef1$tasa_ols <- coef1$empleo_2_ols*10000}
    
    coef_sel <-coef1%>%dplyr::filter(year==input$periodo_year2)
    
    plot_ly(coef_sel) %>% add_trace(coef_sel, x=~n, y=~tasa, 
                                    mode="lines", name="Coef: Reg. Cuantil") %>%
      add_trace(coef_sel, x=~n, y=~tasa_ols, mode="lines", name="Coeficiente: OLS")%>%
      layout(xaxis = list(title = "Percentil"),
             yaxis = list (title = "Coeficiente (por cada 10,000 empleos)"))%>% layout(legend = list(orientation = 'h',x = 0.50, y = -0.2))
    
  })
  
  # Gráfico 8 (ratio coefeiciente/ee) Efecto del empleo en las tasas 
  output$ratio_perct_qreg_emp <- renderPlotly({ 
    if(input$tasa_sel_qreg_emp == "Tasa de mortalidad" & input$periodo_empleo =="Sin rezago"){ 
      coef1 <- coef_mort_1 
      coef1$ratio <- coef1$empleo/coef1$empleo_se
      coef1$ratio_ols <- coef1$empleo_ols/coef1$empleo_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de mortalidad" & input$periodo_empleo =="1 rezago (1 mes)"){ 
      coef1 <- coef_mort_2 
      coef1$ratio <- coef1$empleo_1/coef1$empleo_1_se
      coef1$ratio_ols <- coef1$empleo_1_ols/coef1$empleo_1_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de mortalidad" & input$periodo_empleo =="2 rezagos (2 meses)"){ 
      coef1 <- coef_mort_3 
      coef1$ratio <- coef1$empleo_2/coef1$empleo_2_se
      coef1$ratio_ols <- coef1$empleo_2_ols/coef1$empleo_2_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de embarazos adolescentes" & input$periodo_empleo =="Sin rezago"){ 
      coef1 <- coef_emb_1 
      coef1$ratio <- coef1$empleo/coef1$empleo_se
      coef1$ratio_ols <- coef1$empleo_ols/coef1$empleo_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de embarazos adolescentes" & input$periodo_empleo =="1 rezago (1 mes)"){ 
      coef1 <- coef_emb_2 
      coef1$ratio <- coef1$empleo_1/coef1$empleo_1_se
      coef1$ratio_ols <- coef1$empleo_1_ols/coef1$empleo_1_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de embarazos adolescentes" & input$periodo_empleo =="2 rezagos (2 meses)"){ 
      coef1 <- coef_emb_3 
      coef1$ratio <- coef1$empleo_2/coef1$empleo_2_se
      coef1$ratio_ols <- coef1$empleo_2_ols/coef1$empleo_2_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de nacimientos" & input$periodo_empleo =="Sin rezago"){ 
      coef1 <- coef_nac_1 
      coef1$ratio <- coef1$empleo/coef1$empleo_se
      coef1$ratio_ols <- coef1$empleo_ols/coef1$empleo_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de nacimientos" & input$periodo_empleo =="1 rezago (1 mes)"){ 
      coef1 <- coef_nac_2
      coef1$ratio <- coef1$empleo_1/coef1$empleo_1_se
      coef1$ratio_ols <- coef1$empleo_1_ols/coef1$empleo_1_se_ols}
    else if(input$tasa_sel_qreg_emp == "Tasa de nacimientos" & input$periodo_empleo =="2 rezagos (2 meses)"){ 
      coef1 <- coef_nac_3 
      coef1$ratio <- coef1$empleo_2/coef1$empleo_2_se
      coef1$ratio_ols <- coef1$empleo_2_ols/coef1$empleo_2_se_ols}
    
    coef_sel <-coef1%>%dplyr::filter(year==input$periodo_year2)
    
    plot_ly(coef_sel) %>% 
      add_trace(coef_sel, x=~n, y=~ratio, type="scatter",  mode="markers", 
                name="Ratio: Reg. Cuantil", color="Red", size=1) %>%
      add_trace(coef_sel, x=~n, y=~ratio_ols, mode="lines", 
                name="Ratio: OLS", color="Blue") %>%
      layout(xaxis = list(title = "Percentil"),
             yaxis = list (title = "Ratio coeficiente del empleo/error estándar"))%>% layout(legend = list(orientation = 'h',x = 0.50, y = -0.2))
  })
  
  # Termina gráficos análisis Joel
  
  # Inicia gráficos análisis Will
  
  prom_cuant_sel <- reactive({
    #internacional_sel<-internacionales%>%dplyr::filter(serie==input$var_inter)
    var_econ_sel <- prom_cuantiles_estados%>%dplyr::filter(vari_econ==input$var_e)
    var_econ_sel%>%dplyr::filter(vari_pob==input$var_p)
    #dplyr::filter(nombres_variables1, X1 %in% c(input$var_emb))[-1]%>%select_if(~ !any(is.na(.)))
  })
  
  
  output$prom_cuantiles <- renderPlotly({
    a <- plot_ly()
    a <- add_trace(a, data=prom_cuant_sel(), x=~perct, y=~promedio_var_pob, mode='line')
    a%>%layout(xaxis = list(title = "Cuantil de la variable económica"),
               yaxis = list (title = "Tasa de la variable poblacional"))
    
  })
  
  coef_cuant_sel <- reactive({
    #internacional_sel<-internacionales%>%dplyr::filter(serie==input$var_inter)
    coef_econ <- coef_cuantiles_estados%>%dplyr::filter(var_econ==input$vari_e)
    coef_econ%>%dplyr::filter(var_pob==input$vari_p)
    #dplyr::filter(nombres_variables1, X1 %in% c(input$var_emb))[-1]%>%select_if(~ !any(is.na(.)))
  })
  
  
  output$coef_cuantiles <- renderPlotly({
    a <- plot_ly()
    a <- add_trace(a, data=coef_cuant_sel(), x=~q, y=~coef, mode='line', name="Regresión cuantil")
    a <- add_trace(a, data=coef_cuant_sel(), x=~q, y=~coef_ols, mode='line', name="Regresión OLS")
    a%>%layout(xaxis = list(title = "Veintil"),
               yaxis = list (title = "Coeficiente de la regresión"))
  })
  
  variab_econ <- reactive({
    nom_var_econ <- nom_var_ols%>%dplyr::filter(econ_bueno==input$varia_e)
    malo_econ <- nom_var_econ$econ[1]
    malo_econ
    #internacional_sel<-internacionales%>%dplyr::filter(serie==input$var_inter)
    #coef_econ <- coef_cuantiles_estados%>%dplyr::filter(var_econ==input$varia_e)
    #coef_econ%>%dplyr::filter(var_pob==input$vari_p)
    #dplyr::filter(nombres_variables1, X1 %in% c(input$var_emb))[-1]%>%select_if(~ !any(is.na(.)))
  })
  
  variab_pob <- reactive({
    nom_var_p <- nom_var_ols%>%dplyr::filter(pob_bueno==input$varia_p)
    malo_pob <- nom_var_p$pob[1]
    malo_pob
    #internacional_sel<-internacionales%>%dplyr::filter(serie==input$var_inter)
    #coef_econ <- coef_cuantiles_estados%>%dplyr::filter(var_econ==input$vari_e)
    #coef_econ%>%dplyr::filter(var_pob==input$vari_p)
    #dplyr::filter(nombres_variables1, X1 %in% c(input$var_emb))[-1]%>%select_if(~ !any(is.na(.)))
  })
  
  
  output$image_ols <- renderImage({
    return(list(
      src = paste0("www/reg_",variab_pob(),"_",variab_econ(),".png"),
      contentType = "image/png",
      alt = "coeficientes",
      style="display: block; margin-left: auto; margin-right: auto", width = "70%"
    ))
  }, deleteFile = FALSE)
  
  # Termina gráficos análisis Will
  
  rm(mapa_mun)
  } # Termina Server