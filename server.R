shinyServer(function(input, output, session) {
  
  # ------------------------------------------------------------------------
  # General settings
  # ------------------------------------------------------------------------

  # global switch to hide unselected institutions 
  observe({
    hide <- input$all_hideunselected
    
    switches=c("level1_spermplot_hideunselected","level1_scissorsplot_hideunselected",
               "level1_balance_years_hideunselected","level1_balance_students_years_hideunselected",
               "level1_prestigeplot_hideunselected",
               "level2_balance_years_hideunselected","level2_balance_students_years_hideunselected",
               "level2_prestigeplot_hideunselected",
               "level3_balance_years_hideunselected","level3_balance_students_years_hideunselected",
               "level3_prestigeplot_hideunselected"
               )
      for(switch in switches){
        updateMaterialSwitch(session,switch,value=hide)
      }
  })
    
  # ------------------------------------------------------------------------
  # Level 1
  # ------------------------------------------------------------------------

if(enable.level1) {## DEBUG

  rv.level1.selected_uni <- reactiveVal(NULL)  

    
  output$level1map <- renderLeaflet({
    leaflet(data = level1) |>
      addProviderTiles(providers$Stamen.Watercolor) |>
      addMarkers(~long,~lat, label=~Institusjonsnavn, popup = ~popup,
                 clusterOptions = markerClusterOptions())  
  })
  
  output$level1table <- renderDataTable({
    level1 |> select(Kortnavn, Institusjonsnavn) -> d
    d
  })
  
  # update the currently selected list of level1-institutions (Institusjonskode)
  observe({
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    rv.level1.selected_uni(sel.uni)             # rv$value <- newValue
    #cat("UPDATE: ",sel.uni)
  })  
  
  level1table.proxy = dataTableProxy('level1table')
  
  ## when map marker is clicked, highlight row in Table
  observeEvent(input$level1map_marker_click, { 
    p <- input$level1map_marker_click

    # which uni does the marker belong to?
    pt=c(p$lng, p$lat)
    # euclidean distance to each of the unis
    ix <- which.min(sqrt(rowSums((level1[c("long","lat")] - matrix(pt, nrow=dim(level1)[1], ncol=2, byrow=T))^2)))
    d <- level1[ix,]
    
    ## select row corresponding to selected marker
    level1table.proxy |> selectRows(ix)
    
    rv.level1.selected_uni(d$Institusjonskode)             # rv$value <- newValue
  })

  
  #'
  #' Total num students/employees
  #'------------
  output$level1_total_num <- renderPlot({
    sel.uni=rv.level1.selected_uni() 

    left_join(level1, level1.employees, by="Institusjonskode") |>
      filter(Årstall==max(Årstall)) |> # current year only
      rename(male_employees=`Antall menn`, female_employees=`Antall kvinner`) |>
      left_join(level1.students, by=c("Institusjonskode", "Årstall")) |>
      rename(male_students=`Antall menn`, female_students=`Antall kvinner`) |>
      gather(var, number, male_students, female_students, male_employees, female_employees) |>
      separate(var, c("gender","type")) |>
      mutate(highlight=(Institusjonskode %in% sel.uni)) -> d.tmp
    
    d.tmp |> 
      ggplot(aes(x=reorder(Kortnavn, `Antall totalt.x`), y=number, fill=gender,alpha=highlight))+
      geom_bar(stat="identity")+coord_flip()+
      labs(y="Number of employees", x="")+
      scale_fill_manual(values=colors.female.male)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      facet_wrap(~type,ncol = 2, scales="free_x")+
      theme(legend.position="top")
    
  })

  #'
  #' Proportion per year
  #'------------
  output$level1_balance_years <- renderPlot({
    sel.uni=rv.level1.selected_uni() 

    left_join(level1, level1.employees, by="Institusjonskode") |>
      mutate(highlight=(Institusjonskode %in% sel.uni)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) -> d.tmp

    if(input$level1_balance_years_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |> 
      ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
      geom_point(aes(shape=Kortnavn))+geom_line(aes(group=Kortnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> filter(Årstall==max(Årstall)), aes(label=Kortnavn), force=50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:dim(level1)[1])+
      guides(color = guide_legend(override.aes = list(label=""))) +
      annotate("text_repel", x=min(level1.employees$Årstall), y=50+0.02*diff(range(d.tmp$`Percent Male`)), label="Perfect balance", color="grey") 
  })
  
  #'
  #' Proportion (students) per year
  #'------------
  output$level1_balance_students_years <- renderPlot({
    sel.uni=rv.level1.selected_uni() 
    
    left_join(level1, level1.students, by="Institusjonskode") |>
      mutate(highlight=(Institusjonskode %in% sel.uni)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |> 
      filter(Årstall>1995) -> d.tmp

    if(input$level1_balance_students_years_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |>
      ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
      geom_point(aes(shape=Kortnavn))+geom_line(aes(group=Kortnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> filter(Årstall==max(Årstall)), aes(label=Kortnavn), force = 50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:dim(level1)[1])+
      guides(color = guide_legend(override.aes = list(label=""))) +
      annotate("text_repel", x=min(d.tmp$Årstall), y=50+0.02*diff(range(d.tmp$`Percent Male`)), label="Perfect balance", color="grey") 
  })
  
  #'
  #' "Prestige plot"
  #'------------
  output$level1_prestigeplot <- renderPlot({
    sel.uni=rv.level1.selected_uni() 
    
    cur.year=max(level1.employees$Årstall)
    ref.year=input$level1_prestigeplot_refyear
    
    left_join(level1, level1.employees, by="Institusjonskode") |>
      mutate(highlight=(Institusjonskode %in% sel.uni)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |>
      filter(Årstall %in% c(ref.year, cur.year)) |>
      select(-`Antall menn`, -`Antall kvinner`) |> group_by(Institusjonskode) |>
      mutate(`Antall totalt`=max(`Antall totalt`)) |>
      ungroup() |>
      spread(Årstall, `Percent Male`) -> d.tmp
    bgdat=data.frame(v=-10:110) %>%
      mutate(c=case_when(#v>40 & v<60 ~ 0,
        T ~ abs(v-50)))
    grid.maj.x=c(0,25,50,75,100)
    grid.maj.y=c(0,25,50,75,100)
    grid.min.x=c(12.5, 37.5, 62.5, 87.5)
    grid.min.y=c(12.5, 37.5, 62.5, 87.5)

    if(input$level1_prestigeplot_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }

    d.tmp |> 
      ggplot(aes(alpha=highlight))+
      geom_rect(data=bgdat, aes(xmin=-Inf, xmax=Inf, ymin=v, ymax=v+1, fill=c), alpha=1, show.legend=F)+
      scale_fill_gradientn(colours=c("#6b9733","#fec200","#b64a1a"))+
      geom_abline(slope=1, intercept=0, color="white", size=1)+
      geom_vline(xintercept = grid.maj.x, color="white", size=0.5)+
      geom_vline(xintercept = grid.min.x, color="white", size=0.1)+
      geom_hline(yintercept = grid.maj.y, color="white", size=0.5)+
      geom_hline(yintercept = grid.min.y, color="white", size=0.1)+
      geom_point(aes_(x=as.name(ref.year), y=as.name(cur.year), 
                      size=as.name("Antall totalt"), color=as.name("Kortnavn")))+
      geom_text_repel(aes_(as.name(ref.year), y=as.name(cur.year), 
                           #color=as.name("Kortnavn"), 
                           label=as.name("Kortnavn")),
                      #arrow = arrow(),#length = unit(0.03, "npc"), type = "closed", ends = "first"),
                      force=60)+
      scale_x_continuous(expand = c(.1, .1)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      labs(x=glue("Gender balance (% male) in {ref.year}"),
           y=glue("Gender balance (% male) in {cur.year}"))+
      coord_fixed()+
      theme(panel.background = element_rect(fill = NA))
  })
  
  #'
  #' "Scissors plot"
  #'------------
  output$level1_scissorsplot <- renderPlot({
    sel.uni=rv.level1.selected_uni() 
    disp.positions <- input$level1_scissorsplot_positions
    
    level1.students |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`,
             Benevnelse="Student") |>
      select(Institusjonskode, Årstall, Benevnelse, `Percent Male`) -> d.stud
    
    cur.year=max(level1.employees.positions$Årstall)
    level1.employees.positions |>
      filter(Benevnelse %in% disp.positions) |>
      group_by(Institusjonskode, Årstall,Benevnelse) |> # get rid of different Stillingskode for same Benevnelse
      summarise(`Antall menn`=sum(`Antall menn`), `Antall årsverk`=sum(`Antall årsverk`), .groups="drop") |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall årsverk`) |>
      select(-`Antall menn`,-`Antall årsverk`) |> 
      full_join(d.stud) |>
      filter(Benevnelse %in% disp.positions) |>
      mutate(Benevnelse=factor(Benevnelse, levels=disp.positions)) |> arrange(Institusjonskode, Årstall, Benevnelse) |>
      left_join(level1) |> 
      mutate(Benevnelse=factor(Benevnelse, levels=disp.positions)) |> 
      mutate(highlight=(Institusjonskode %in% sel.uni)) -> d.tmp
    
    if(input$level1_scissorsplot_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |>
      ggplot(aes(x=Benevnelse, y=`Percent Male`, alpha=highlight))+
        geom_line(aes(group=Årstall,color=Årstall))+
        geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
        scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
        facet_wrap(~Kortnavn)+
        theme(axis.text.x = element_text(angle=70, hjust=1))
  })

  #'
  #' "Sperm plot"
  #'------------
  output$level1_spermplot <- renderPlot({
    sel.uni=rv.level1.selected_uni() 
    
    level1.students |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`,
             Benevnelse="Student") |>
      select(Institusjonskode, Årstall, Benevnelse, `Percent Male`) -> d.stud
    
    ref.cat=input$level1_spermplot_refpos
    target.cat=input$level1_spermplot_maxpos
    cur.year=max(level1.employees.positions$Årstall)
    level1.employees.positions |>
      filter(Benevnelse %in% default.positions) |>
      group_by(Institusjonskode, Årstall,Benevnelse) |> # get rid of different Stillingskode for same Benevnelse
      summarise(`Antall menn`=sum(`Antall menn`), `Antall årsverk`=sum(`Antall årsverk`), .groups="drop") |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall årsverk`) |>
      select(-`Antall menn`,-`Antall årsverk`) |> 
      full_join(d.stud) |>
      mutate(Benevnelse=factor(Benevnelse, levels=default.positions)) |> arrange(Institusjonskode, Årstall, Benevnelse) |>
      left_join(level1) |> 
      mutate(Benevnelse=factor(Benevnelse, levels=default.positions)) |> 
      mutate(highlight=(Institusjonskode %in% sel.uni)) |>
      mutate(current.year=Årstall==cur.year) |>
      filter(Benevnelse %in% c(ref.cat, target.cat)) |>
      spread(Benevnelse, `Percent Male`) |>
      mutate(Kortnavn=factor(Kortnavn)) -> d.tmp
    
    if(input$level1_spermplot_hideunselected){
      d.tmp |> filter(highlight==T) -> d.tmp  
    }
    
    d.tmp |>
      ggplot(aes_(x=as.name(ref.cat), y=as.name(target.cat), alpha=as.name("highlight")))+
        geom_point(aes(color=Kortnavn, size=current.year))+
        geom_line(aes(group=Kortnavn,color=Kortnavn))+
        geom_abline(slope=1,intercept=0,color="grey",linetype="dashed")+
        geom_text_repel(data=d.tmp |> filter(Årstall==cur.year), aes(label=Kortnavn), force=60)+
        scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
        scale_size_manual(values=c(0, 5.0), breaks=c(F,T), guide="none")+
        coord_fixed(xlim=c(0,100),ylim=c(0,100))
  })
    
}## DEBUG: enable.level1
  
  # ------------------------------------------------------------------------
  # Level 2
  # ------------------------------------------------------------------------

if( enable.level2 ){ ## DEBUG  
  rv.level2.selected_fac <- reactiveVal(NULL)
  rv.level2.num_fac <- reactiveVal(NULL)
  
  observe({
    uni.sel=first(rv.level1.selected_uni())  
    lev2 <- level2 |> filter(Institusjonskode==uni.sel)
    
    sel.rows=input$level2table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(lev2)[1]
    }
    sel.fac=lev2$Fakultetskode[sel.rows]
    rv.level2.selected_fac(sel.fac)
    rv.level2.num_fac(dim(lev2)[1])
  })  
  
  output$level2_title <- renderUI({
    uni.sel <- first(rv.level1.selected_uni())
    level1 |>
      filter(Institusjonskode==uni.sel) |>
      mutate(uniname=glue("{Institusjonsnavn} ({Kortnavn})")) -> d.tmp
    uniname <- d.tmp |> pull(uniname)

    p(img(src=get_logo(uni.sel, version="wide", link_type="www"), width="300px"),
      h1(uniname))
  })
  
  output$level2table <- renderDataTable({
    uni.sel=first(rv.level1.selected_uni())  
    
    level2 |> filter(Institusjonskode==uni.sel) |>
      select(Fakultetsnavn,Kortnavn)
  }, options=list(paging=T))

  #'
  #' Proportion per year
  #'------------
  output$level2_balance_years <- renderPlot({
    uni.sel=first(rv.level1.selected_uni())  
    sel.fac= rv.level2.selected_fac()
    
    level2 |> filter(Institusjonskode==uni.sel) |>
      left_join(level2.employees, by=c("Institusjonskode", "Avdelingskode")) |>
      mutate(highlight=(Fakultetskode %in% sel.fac)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) -> d.tmp
    
    if(input$level2_balance_years_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |> 
      ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
      geom_point(aes(shape=Kortnavn))+geom_line(aes(group=Kortnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> group_by(Kortnavn) |> 
                        filter(Årstall==max(Årstall)) |> ungroup(), aes(label=Kortnavn), force=50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:rv.level2.num_fac())+
      guides(color = guide_legend(override.aes = list(label=""))) +
      annotate("text_repel", x=min(level2.employees$Årstall), 
               y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
               label="Perfect balance", color="grey") +
      theme(legend.position="bottom")+
      coord_cartesian(xlim=range(d.tmp$Årstall))
  })  
  
  #'
  #' Proportion (students) per year
  #'------------
  output$level2_balance_students_years <- renderPlot({
    uni.sel=first(rv.level1.selected_uni())  
    sel.fac= rv.level2.selected_fac()
    
    level2 |> filter(Institusjonskode==uni.sel) |>
      left_join(level2.students, by=c("Institusjonskode", "Avdelingskode")) |>
      mutate(highlight=(Fakultetskode %in% sel.fac)) |> 
      na.omit() |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) -> d.tmp

    if(input$level2_balance_students_years_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |>
      ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
      geom_point(aes(shape=Kortnavn))+geom_line(aes(group=Kortnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> group_by(Kortnavn) |> 
                        filter(Årstall==max(Årstall)) |> ungroup(), 
                      aes(label=Kortnavn), force = 50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:rv.level2.num_fac())+
      guides(color = guide_legend(override.aes = list(label=""))) +
      annotate("text_repel", x=min(d.tmp$Årstall), y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
               label="Perfect balance", color="grey")+
      theme(legend.position="bottom")
  })
  
  #'
  #' "Prestige plot"
  #'------------
  output$level2_prestigeplot <- renderPlot({
    sel.uni=rv.level1.selected_uni() 
    sel.fac= rv.level2.selected_fac()
    
    cur.year=max(level2.employees$Årstall)
    ref.year=input$level2_prestigeplot_refyear
    
    level2 |> filter(Institusjonskode==sel.uni) |>
      left_join(level2.employees, by=c("Institusjonskode", "Avdelingskode")) |>
      mutate(highlight=(Fakultetskode %in% sel.fac)) |> 
      na.omit() |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |>
      filter(Årstall %in% c(ref.year, cur.year)) |>
      select(-`Antall menn`, -`Antall kvinner`) |> group_by(Avdelingskode) |>
      mutate(`Antall totalt`=max(`Antall totalt`)) |>
      ungroup() |>
      spread(Årstall, `Percent Male`) -> d.tmp
    bgdat=data.frame(v=-10:110) %>%
      mutate(c=case_when(#v>40 & v<60 ~ 0,
        T ~ abs(v-50)))
    grid.maj.x=c(0,25,50,75,100)
    grid.maj.y=c(0,25,50,75,100)
    grid.min.x=c(12.5, 37.5, 62.5, 87.5)
    grid.min.y=c(12.5, 37.5, 62.5, 87.5)
    
    if(input$level2_prestigeplot_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |> 
      ggplot(aes(alpha=highlight))+
      geom_rect(data=bgdat, aes(xmin=-Inf, xmax=Inf, ymin=v, ymax=v+1, fill=c), alpha=1, show.legend=F)+
      scale_fill_gradientn(colours=c("#6b9733","#fec200","#b64a1a"))+
      geom_abline(slope=1, intercept=0, color="white", size=1)+
      geom_vline(xintercept = grid.maj.x, color="white", size=0.5)+
      geom_vline(xintercept = grid.min.x, color="white", size=0.1)+
      geom_hline(yintercept = grid.maj.y, color="white", size=0.5)+
      geom_hline(yintercept = grid.min.y, color="white", size=0.1)+
      geom_point(aes_(x=as.name(ref.year), y=as.name(cur.year), 
                      size=as.name("Antall totalt"), color=as.name("Kortnavn")))+
      geom_text_repel(aes_(as.name(ref.year), y=as.name(cur.year), 
                           #color=as.name("Kortnavn"), 
                           label=as.name("Kortnavn")),
                      #arrow = arrow(),#length = unit(0.03, "npc"), type = "closed", ends = "first"),
                      force=60)+
      scale_x_continuous(expand = c(.1, .1)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      labs(x=glue("Gender balance (% male) in {ref.year}"),
           y=glue("Gender balance (% male) in {cur.year}"))+
      coord_fixed()+
      theme(panel.background = element_rect(fill = NA))
  })  
  
} ## DEBUG: enable.level2
  
  # ------------------------------------------------------------------------
  # Level 3
  # ------------------------------------------------------------------------
  
if(enable.level3) { ##DEBUG
  rv.level3.selected_inst <- reactiveVal(NULL)

  observe({
    sel.uni <- first(rv.level1.selected_uni())
    sel.fac <- first(rv.level2.selected_fac())
    
    lev3 <- level3 |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac)
    
    sel.rows=sort(input$level3table_rows_selected)
    if(is.null(sel.rows)){ # show all unis initially
      sel.inst=NULL
    } else {
      sel.inst=lev3$Avdelingskode[sel.rows]
    }
    rv.level3.selected_inst(sel.inst)
    cat("UPDATE LEV3: ", sel.inst)
  })
  
  
  output$level3_title <- renderUI({
    sel.uni <- first(rv.level1.selected_uni())
    sel.fac <- first(rv.level2.selected_fac())
    
    level1 |> filter(Institusjonskode==sel.uni) |>
      mutate(uniname=glue("{Institusjonsnavn} ({Kortnavn})")) -> d.tmp
    uniname <- d.tmp |> pull(uniname)
    uniid <- d.tmp |> pull(Institusjonskode)
    level2 |> filter(Institusjonskode==uniid, Fakultetskode==sel.fac) |>
      pull(Avdelingsnavn) -> facname
    
    p(img(src=get_logo(sel.uni, version="wide", link_type="www"), width="300px"),
      h1(uniname), h2(facname))
  })
  
  output$level3table <- renderDataTable({
    sel.uni <- first(rv.level1.selected_uni())
    sel.fac <- first(rv.level2.selected_fac())
    level3 |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac) |>
      select(Avdelingsnavn, Kortnavn)
  }, options=list(paging=T))

  
  #'
  #' Separate boxes with diverging pips over years for selected institutes
  #'------------
  output$level3_divpips <- renderUI({
    sel.uni <- first(rv.level1.selected_uni())
    sel.fac <- first(rv.level2.selected_fac())
    sel.inst <- rv.level3.selected_inst()
    
    lev3 <- level3 |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac)
    
    plot.ids <- sprintf("level3_divpips_%s",sel.inst)
    names(plot.ids) <- sel.inst
    inst.names <- lev3 |> filter(Avdelingskode %in% sel.inst) |> pull(Avdelingsnavn)
    names(inst.names) <- sel.inst
    map(sel.inst, \(ikode){
      output[[plot.ids[ikode]]] <- renderPlot({
        level3.employees.positions |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac, Avdelingskode==ikode) -> dd
        sel.pos=input[[sprintf("%s_sel",plot.ids[ikode])]] 

        if(sel.pos=="All"){ ## sum across all positions
          dd |> group_by(Institusjonskode, Fakultetskode, Avdelingskode, Årstall) |>
            summarize(`Antall kvinner`=sum(`Antall kvinner`),
                      `Antall menn`=sum(`Antall menn`)) -> dd2
          year=sort(unique(dd2$Årstall))
          
          dd2 |> 
            gather(var, val, all_of(c("Antall menn", "Antall kvinner"))) |> arrange(Årstall,var) |> pull(val) |>
            as.array() -> freq
          dim(freq) <- c(2,length(year))
          dimnames(freq) <- list(c("Kvinner","Menn"), year)
          diverging_pip_plot(freq, bar.width = .3, bar.width.n = 4, bar.col = colors.female.male, add.pct=T,
                             sym = F, cluster.width = 0.33, panel.lty = 1, top.cex = 0.3)
        } else {
          dd |> 
            filter(Benevnelse==sel.pos) |> 
            group_by(Institusjonskode, Fakultetskode, Avdelingskode, Årstall) |>
            summarize(`Antall kvinner`=sum(`Antall kvinner`),
                      `Antall menn`=sum(`Antall menn`)) -> dd2
          year=sort(unique(dd2$Årstall))
          print(dd2)
          dd2 |> 
            gather(var, val, all_of(c("Antall menn", "Antall kvinner"))) |>  arrange(Årstall,var) |> pull(val) |>
            as.array() -> freq
          dim(freq) <- c(2,length(year))
          dimnames(freq) <- list(c("Kvinner","Menn"), year)
          print(freq)
          
          diverging_pip_plot(freq, bar.width = .3, bar.width.n = 4, bar.col = colors.female.male, add.pct=T,
                             sym = F, cluster.width = 0.33, panel.lty = 1, top.cex = 0.3)
          title(main=sel.pos)
        }

      }, height=400)
    })   
    map(sel.inst, \(ikode){
      avail.pos <- level3.employees.positions |> 
        filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac, 
               Avdelingskode==ikode) |> pull(Benevnelse) |> unique()
      box(plotOutput(plot.ids[ikode], height=400), 
          selectInput(sprintf("%s_sel",plot.ids[ikode]), label="Show position", 
                      choices = c("All",avail.pos), selected="All"), 
          title=inst.names[ikode], 
          width = 6, height="550")
    })
  })
    
  
  
  #'
  #' Proportion per year
  #'------------
  output$level3_balance_years <- renderPlot({
    sel.uni <- first(rv.level1.selected_uni())
    sel.fac <- first(rv.level2.selected_fac())
    sel.inst <- rv.level3.selected_inst()

    
    lev3 <- level3 |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac)
    
    lev3 |> select(Institusjonskode, Fakultetskode, Avdelingskode) |> 
      left_join(level3.employees, by=c("Institusjonskode", "Fakultetskode", "Avdelingskode")) |>
      mutate(highlight=ifelse(is.null(sel.inst), rep(T,n()), (Avdelingskode %in% sel.inst))) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |> na.omit() -> d.tmp
    
    if(input$level3_balance_years_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    if(dim(d.tmp)[1]>0){
      d.tmp |> 
        ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
        geom_point(aes(shape=Kortnavn))+geom_line(aes(group=Kortnavn),size=1)+
        geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
        geom_text_repel(data=d.tmp |> group_by(Kortnavn) |> 
                          filter(Årstall==max(Årstall)) |> ungroup(), aes(label=Kortnavn), force=50)+
        scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
        scale_shape_manual(values=1:dim(lev3)[1])+
        guides(color = guide_legend(override.aes = list(label=""))) +
        annotate("text_repel", x=min(d.tmp$Årstall), 
                 y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
                 label="Perfect balance", color="grey") +
        theme(legend.position="bottom")+
        coord_cartesian(xlim=range(d.tmp$Årstall))
    } else {
      d.tmp |> 
        ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
        geom_blank()
    }
  })  
  
  
  #'
  #' Proportion (students) per year
  #'------------
  output$level3_balance_students_years <- renderPlot({
    sel.uni <- first(rv.level1.selected_uni())
    sel.fac <- first(rv.level2.selected_fac())
    sel.inst <- rv.level3.selected_inst()
    
    
    lev3 <- level3 |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac)

    lev3 |> select(Institusjonskode, Fakultetskode, Avdelingskode) |> 
      left_join(level3.students, by=c("Institusjonskode", "Fakultetskode", "Avdelingskode")) |>
      mutate(highlight=ifelse(is.null(sel.inst), rep(T,n()), (Avdelingskode %in% sel.inst))) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |> na.omit() -> d.tmp
    
    if(input$level3_balance_students_years_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    if(dim(d.tmp)[1]>0){
      d.tmp |> 
        ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
        geom_point(aes(shape=Kortnavn))+geom_line(aes(group=Kortnavn),size=1)+
        geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
        geom_text_repel(data=d.tmp |> group_by(Kortnavn) |> 
                          filter(Årstall==max(Årstall)) |> ungroup(), aes(label=Kortnavn), force=50)+
        scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
        scale_shape_manual(values=1:dim(lev3)[1])+
        guides(color = guide_legend(override.aes = list(label=""))) +
        annotate("text_repel", x=min(d.tmp$Årstall), 
                 y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
                 label="Perfect balance", color="grey") +
        theme(legend.position="bottom")+
        coord_cartesian(xlim=range(d.tmp$Årstall))
    } else {
      d.tmp |> 
        ggplot(aes(Årstall, `Percent Male`, color=Kortnavn,alpha=highlight))+
        geom_blank()
    }
  })
  
  #'
  #' "Prestige plot"
  #'------------
  output$level3_prestigeplot <- renderPlot({
    sel.uni=rv.level1.selected_uni() 
    sel.fac= rv.level2.selected_fac()
    sel.inst <- rv.level3.selected_inst()
    
    cur.year=max(level3.employees$Årstall)
    ref.year=input$level3_prestigeplot_refyear
    
    level3 |> filter(Institusjonskode==sel.uni, Fakultetskode==sel.fac) |>
      left_join(level3.employees, by=c("Institusjonskode", "Fakultetskode", "Avdelingskode",
                                       "Avdelingsnavn","Kortnavn","Fakultetsnavn")) |>
      mutate(highlight=ifelse(is.null(sel.inst), rep(T,n()), (Avdelingskode %in% sel.inst))) |> 
      na.omit() |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |>
      filter(Årstall %in% c(ref.year, cur.year)) |>
      select(-`Antall menn`, -`Antall kvinner`) |> group_by(Avdelingskode) |>
      mutate(`Antall totalt`=max(`Antall totalt`)) |>
      ungroup() |>
      spread(Årstall, `Percent Male`) -> d.tmp
    bgdat=data.frame(v=-10:110) %>%
      mutate(c=case_when(#v>40 & v<60 ~ 0,
        T ~ abs(v-50)))
    grid.maj.x=c(0,25,50,75,100)
    grid.maj.y=c(0,25,50,75,100)
    grid.min.x=c(12.5, 37.5, 62.5, 87.5)
    grid.min.y=c(12.5, 37.5, 62.5, 87.5)
    
    if(input$level3_prestigeplot_hideunselected){
      d.tmp |> filter(highlight) -> d.tmp
    }
    
    d.tmp |> 
      ggplot(aes(alpha=highlight))+
      geom_rect(data=bgdat, aes(xmin=-Inf, xmax=Inf, ymin=v, ymax=v+1, fill=c), alpha=1, show.legend=F)+
      scale_fill_gradientn(colours=c("#6b9733","#fec200","#b64a1a"))+
      geom_abline(slope=1, intercept=0, color="white", size=1)+
      geom_vline(xintercept = grid.maj.x, color="white", size=0.5)+
      geom_vline(xintercept = grid.min.x, color="white", size=0.1)+
      geom_hline(yintercept = grid.maj.y, color="white", size=0.5)+
      geom_hline(yintercept = grid.min.y, color="white", size=0.1)+
      geom_point(aes_(x=as.name(ref.year), y=as.name(cur.year), 
                      size=as.name("Antall totalt"), color=as.name("Kortnavn")))+
      geom_text_repel(aes_(as.name(ref.year), y=as.name(cur.year), 
                           #color=as.name("Kortnavn"), 
                           label=as.name("Kortnavn")),
                      #arrow = arrow(),#length = unit(0.03, "npc"), type = "closed", ends = "first"),
                      force=60)+
      scale_x_continuous(expand = c(.1, .1)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      labs(x=glue("Gender balance (% male) in {ref.year}"),
           y=glue("Gender balance (% male) in {cur.year}"))+
      coord_fixed()+
      theme(panel.background = element_rect(fill = NA))
  })    
  
} ## DEBUG: enable.level3  
  
})
