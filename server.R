shinyServer(function(input, output, session) {
  rv.level1.selected <- reactiveVal(NULL)
  # ------------------------------------------------------------------------
  # Level 1
  # ------------------------------------------------------------------------

if(enable.level1) {## DEBUG
  
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
  ## DOES NOT QUITE WORK for some reason; check back later?
  observeEvent(input$level1table_rows_selected, {
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    rv.level1.selected(sel.uni)             # rv$value <- newValue
    
    print(rv.level1.selected)
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
    
  })

  #'
  #' Proportion per year
  #'------------
  output$level1_balance_years <- renderPlot({
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    
    left_join(level1, level1.employees, by="Institusjonskode") |>
      mutate(highlight=(Institusjonskode %in% sel.uni)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) -> d.tmp
    
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
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    left_join(level1, level1.students, by="Institusjonskode") |>
      mutate(highlight=(Institusjonskode %in% sel.uni)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |> 
      filter(Årstall>1995) -> d.tmp
    
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
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    
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
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    
    level1.students |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`,
             Benevnelse="Student") |>
      select(Institusjonskode, Årstall, Benevnelse, `Percent Male`) -> d.stud
    
    cur.year=max(level1.employees.positions$Årstall)
    level1.employees.positions |>
      filter(Benevnelse %in% positions) |>
      group_by(Institusjonskode, Årstall,Benevnelse) |> # get rid of different Stillingskode for same Benevnelse
      summarise(`Antall menn`=sum(`Antall menn`), `Antall årsverk`=sum(`Antall årsverk`), .groups="drop") |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall årsverk`) |>
      select(-`Antall menn`,-`Antall årsverk`) |> 
      full_join(d.stud) |>
      mutate(Benevnelse=factor(Benevnelse, levels=positions)) |> arrange(Institusjonskode, Årstall, Benevnelse) |>
      left_join(level1) |> 
      mutate(Benevnelse=factor(Benevnelse, levels=positions)) |> 
      mutate(highlight=(Institusjonskode %in% sel.uni)) |> 
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
    sel.rows=input$level1table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(level1)[1]
    }
    sel.uni=level1$Institusjonskode[sel.rows]
    
    level1.students |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`,
             Benevnelse="Student") |>
      select(Institusjonskode, Årstall, Benevnelse, `Percent Male`) -> d.stud
    
    ref.cat=input$level1_spermplot_refpos
    target.cat=input$level1_spermplot_maxpos
    cur.year=max(level1.employees.positions$Årstall)
    
    level1.employees.positions |>
      filter(Benevnelse %in% positions) |>
      group_by(Institusjonskode, Årstall,Benevnelse) |> # get rid of different Stillingskode for same Benevnelse
      summarise(`Antall menn`=sum(`Antall menn`), `Antall årsverk`=sum(`Antall årsverk`), .groups="drop") |>
      mutate(`Percent Male`=100*`Antall menn`/`Antall årsverk`) |>
      select(-`Antall menn`,-`Antall årsverk`) |> 
      full_join(d.stud) |>
      mutate(Benevnelse=factor(Benevnelse, levels=positions)) |> arrange(Institusjonskode, Årstall, Benevnelse) |>
      left_join(level1) |> 
      mutate(Benevnelse=factor(Benevnelse, levels=positions)) |> 
      mutate(highlight=(Institusjonskode %in% sel.uni)) |>
      mutate(current.year=Årstall==cur.year) |>
      filter(Benevnelse %in% c(ref.cat, target.cat)) |>
      spread(Benevnelse, `Percent Male`) -> d.tmp
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
  
  output$level2_title <- renderUI({
    uni.kort=input$level2_selectuni
    
    #sel.rows=input$level1table_rows_selected
    #uni <- level1$Institusjonskode[sel.rows][1]
    level1 |> filter(Kortnavn==uni.kort) |>
      mutate(uniname=glue("{Institusjonsnavn} ({Kortnavn})")) -> d.tmp
    uniname <- d.tmp |> pull(uniname)
    uniid <- d.tmp |> pull(Institusjonskode)
    

    p(img(src=get_logo(uniid, version="wide", link_type="www"), width="300px"),
      h1(uniname))
  })
  
  output$level2table <- renderDataTable({
    uni=level1$Institusjonskode[level1$Kortnavn==input$level2_selectuni]
    
    level2 |> filter(Institusjonskode==uni) |>
      select(Fakultetsnavn)
  }, options=list(paging=T))

  #'
  #' Proportion per year
  #'------------
  output$level2_balance_years <- renderPlot({
    uni=level1$Institusjonskode[level1$Kortnavn==input$level2_selectuni]
    lev2 <- level2 |> filter(Institusjonskode==uni)
    
    sel.rows=input$level2table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(lev2)[1]
    }
    sel.fac=lev2$Avdelingskode[sel.rows]
    
    left_join(lev2, level2.employees, by=c("Institusjonskode", "Avdelingskode")) |>
      mutate(highlight=(Avdelingskode %in% sel.fac)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) -> d.tmp
    
    d.tmp |> 
      ggplot(aes(Årstall, `Percent Male`, color=Fakultetsnavn,alpha=highlight))+
      geom_point(aes(shape=Fakultetsnavn))+geom_line(aes(group=Fakultetsnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> group_by(Fakultetsnavn) |> 
                        filter(Årstall==max(Årstall)) |> ungroup(), aes(label=Fakultetsnavn), force=50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:dim(lev2)[1])+
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
    uni=level1$Institusjonskode[level1$Kortnavn==input$level2_selectuni]
    lev2 <- level2 |> filter(Institusjonskode==uni)
    
    sel.rows=input$level2table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(lev2)[1]
    }
    sel.fac=lev2$Avdelingskode[sel.rows]
    
    left_join(lev2, level2.students, by=c("Institusjonskode", "Avdelingskode")) |>
      mutate(highlight=(Avdelingskode %in% sel.fac)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) -> d.tmp
      #filter(Årstall>1995) -> d.tmp
    
    d.tmp |>
      ggplot(aes(Årstall, `Percent Male`, color=Fakultetsnavn,alpha=highlight))+
      geom_point(aes(shape=Fakultetsnavn))+geom_line(aes(group=Fakultetsnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> group_by(Fakultetsnavn) |> 
                        filter(Årstall==max(Årstall)) |> ungroup(), 
                      aes(label=Fakultetsnavn), force = 50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:dim(lev2)[1])+
      guides(color = guide_legend(override.aes = list(label=""))) +
      annotate("text_repel", x=min(d.tmp$Årstall), y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
               label="Perfect balance", color="grey")+
      theme(legend.position="bottom")
  })
  
} ## DEBUG: enable.level2
  
  # ------------------------------------------------------------------------
  # Level 3
  # ------------------------------------------------------------------------
  
if(enable.level3) { ##DEBUG
  
  rv.level3.selected_uni <- reactiveVal(NULL)
  rv.level3.selected_fac <- reactiveVal(NULL)
  
  observe({
    uni <- input$level3_selectuni
    unikode=first(level1$Institusjonskode[level1$Kortnavn==uni])
    rv.level3.selected_uni(unikode)             # rv$value <- newValue
    facs=unique(level3$Fakultetsnavn[level3$Institusjonskode==unikode]);
    updateSelectInput(session, "level3_selectfac",
                      choices=facs,
                      selected=facs[1])
  })
  observe({
    fac <- input$level3_selectfac
    
    fackode=(level2 |> filter(Institusjonskode==rv.level3.selected_uni(), Avdelingsnavn==fac) |> pull(Fakultetskode))
    rv.level3.selected_fac(fackode)
  })
  
  
  output$level3_title <- renderUI({
    level1 |> filter(Institusjonskode==rv.level3.selected_uni()) |>
      mutate(uniname=glue("{Institusjonsnavn} ({Kortnavn})")) -> d.tmp
    uniname <- d.tmp |> pull(uniname)
    uniid <- d.tmp |> pull(Institusjonskode)
    level2 |> filter(Institusjonskode==uniid, Fakultetskode==rv.level3.selected_fac()) |>
      pull(Avdelingsnavn) -> facname
    
    p(img(src=get_logo(uniid, version="wide", link_type="www"), width="300px"),
      h1(uniname), h2(facname))
  })
  
  output$level3table <- renderDataTable({
    level3 |> filter(Institusjonskode==rv.level3.selected_uni(), Fakultetskode==rv.level3.selected_fac()) |>
      select(Avdelingsnavn, Kortnavn)
  }, options=list(paging=T))

  
  #'
  #' Separate boxes with diverging pips over years for selected institutes
  #'------------
  output$level3_divpips <- renderUI({
    bar.col=c("red", "blue") # female/male colors
    
    lev3 <- level3 |> filter(Institusjonskode==rv.level3.selected_uni(), Fakultetskode==rv.level3.selected_fac())
    #lev3 <- level3 |> filter(Institusjonskode==id.uni, Fakultetskode==id.fac)
    
    sel.rows=sort(input$level3table_rows_selected)
    if(is.null(sel.rows)){ # show all unis initially
      sel.inst=NULL
    } else {
      sel.inst=lev3$Avdelingskode[sel.rows]
    }

    plot.ids <- sprintf("level3_divpips_%s",sel.inst)
    names(plot.ids) <- sel.inst
    inst.names <- lev3 |> filter(Avdelingskode %in% sel.inst) |> pull(Avdelingsnavn)
    names(inst.names) <- sel.inst
    map(sel.inst, \(ikode){
      output[[plot.ids[ikode]]] <- renderPlot({
        level3.employees.positions |> filter(Institusjonskode==rv.level3.selected_uni(), Fakultetskode==rv.level3.selected_fac(), Avdelingskode==ikode) -> dd
        #level3.employees.positions |> filter(Institusjonskode==id.uni, Fakultetskode==id.fac, Avdelingskode==id.inst) -> dd
        sel.pos=input[[sprintf("%s_sel",plot.ids[ikode])]] 
        #sel.pos=c("Professor","Stipendiat") 

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
          diverging_pip_plot(freq, bar.width = .3, bar.width.n = 4, bar.col = bar.col, add.pct=T,
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
          
          diverging_pip_plot(freq, bar.width = .3, bar.width.n = 4, bar.col = bar.col, add.pct=T,
                             sym = F, cluster.width = 0.33, panel.lty = 1, top.cex = 0.3)
          title(main=sel.pos)
        }

      }, height=400)
    })   
    map(sel.inst, \(ikode){
      avail.pos <- level3.employees.positions |> 
        filter(Institusjonskode==rv.level3.selected_uni(), Fakultetskode==rv.level3.selected_fac(), 
               Avdelingskode==ikode) |> pull(Benevnelse) |> unique()
      box(plotOutput(plot.ids[ikode], height=400), 
          selectInput(sprintf("%s_sel",plot.ids[ikode]), label="Split by position", 
                      choices = c("All",avail.pos), selected="All"), 
          title=inst.names[ikode], 
          width = 6, height="550")
    })
  })
    
  
  
  #'
  #' Proportion per year
  #'------------
  output$level3_balance_years <- renderPlot({
    lev3 <- level3 |> filter(Institusjonskode==rv.level3.selected_uni(), Fakultetskode==rv.level3.selected_fac())
    
    sel.rows=input$level3table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(lev3)[1]
    }
    sel.inst=lev3$Avdelingskode[sel.rows]
    
    lev3 |> select(Institusjonskode, Fakultetskode, Avdelingskode) |> 
      left_join(level3.employees, by=c("Institusjonskode", "Fakultetskode", "Avdelingskode")) |>
      mutate(highlight=(Avdelingskode %in% sel.inst)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |> na.omit() -> d.tmp
    
    d.tmp |> 
      ggplot(aes(Årstall, `Percent Male`, color=Avdelingsnavn,alpha=highlight))+
      geom_point(aes(shape=Avdelingsnavn))+geom_line(aes(group=Avdelingsnavn),size=1)+
      geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
      geom_text_repel(data=d.tmp |> group_by(Avdelingsnavn) |> 
                        filter(Årstall==max(Årstall)) |> ungroup(), aes(label=Avdelingsnavn), force=50)+
      scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
      scale_shape_manual(values=1:dim(lev3)[1])+
      guides(color = guide_legend(override.aes = list(label=""))) +
      annotate("text_repel", x=min(d.tmp$Årstall), 
               y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
               label="Perfect balance", color="grey") +
      theme(legend.position="bottom")+
      coord_cartesian(xlim=range(d.tmp$Årstall))
  })  
  
  
  #'
  #' Proportion (students) per year
  #'------------
  output$level3_balance_students_years <- renderPlot({
    lev3 <- level3 |> filter(Institusjonskode==rv.level3.selected_uni(), Fakultetskode==rv.level3.selected_fac())
    
    sel.rows=input$level3table_rows_selected
    if(is.null(sel.rows)){ # show all unis initially
      sel.rows=1:dim(lev3)[1]
    }
    sel.inst=lev3$Avdelingskode[sel.rows]

    lev3 |> select(Institusjonskode, Fakultetskode, Avdelingskode) |> 
      left_join(level3.students, by=c("Institusjonskode", "Fakultetskode", "Avdelingskode")) |>
      mutate(highlight=(Avdelingskode %in% sel.inst)) |> 
      mutate(`Percent Male`=100*`Antall menn`/`Antall totalt`) |> na.omit() -> d.tmp
    if(dim(d.tmp)[1]!=0){
      d.tmp |> 
        ggplot(aes(Årstall, `Percent Male`, color=Avdelingsnavn,alpha=highlight))+
        geom_point(aes(shape=Avdelingsnavn))+geom_line(aes(group=Avdelingsnavn),size=1)+
        geom_hline(yintercept = 50, linetype="dashed", color="grey", size=1)+
        geom_text_repel(data=d.tmp |> group_by(Avdelingsnavn) |> 
                          filter(Årstall==max(Årstall)) |> ungroup(), aes(label=Avdelingsnavn), force=50)+
        scale_alpha_manual(values=c(0.3, 1.0), breaks=c(F,T), guide="none") +
        scale_shape_manual(values=1:dim(lev3)[1])+
        guides(color = guide_legend(override.aes = list(label=""))) +
        annotate("text_repel", x=min(d.tmp$Årstall), 
                 y=50+0.02*diff(range(d.tmp$`Percent Male`)), 
                 label="Perfect balance", color="grey") +
        theme(legend.position="bottom")+
        coord_cartesian(xlim=range(d.tmp$Årstall))
    }
  })
  
} ## DEBUG: enable.level3  
  
})
