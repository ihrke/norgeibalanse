
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Level 1: Institutions", tabName = "level1", icon = icon("university")),
        menuItem("Level 2: Faculties", tabName = "level2", icon = icon("school")),
        menuItem("Level 3: Institutes", tabName = "level3", icon = icon("building") ),
        menuItem("Help", icon = icon("question-circle"), tabName = "help")
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # ------------------------------------------------------------------------
      # Level 1
      # ------------------------------------------------------------------------
        tabItem(tabName = "level1",
                box(leafletOutput("level1map"),
                    p("This map shows all universities in Norway. Click on any marker
                      to get additional information and to highlight data in the plots
                      below."),
                    title="Institutions in Norway", 
                    width=5),
                box(DT::dataTableOutput("level1table"), 
                    title="Institusjoner", 
                    width=7),
                box(
                    dropdownButton(
                    p("This figures shows the development of the overall gender balance
                    (Percentage of employees that were men) at the institution over time.
                      Institutions close to 50% (grey line) have an almost optimal
                      gender balance."),
                    icon = icon("info-circle"), width = "100%", size='sm',
                    tooltip = tooltipOptions(title = "Click for help")
                    ),
                    plotOutput("level1_balance_years"), 
                    title="Gender Balance (all employees)", 
                    width=6),
                box(
                    dropdownButton(
                        p("This figures shows the development of the overall gender balance
                    (percentage of students that were men) at the institution over time.
                      Institutions close to 50% (grey line) have an almost optimal
                      gender balance."),
                        icon = icon("info-circle"), width = "100%", size='sm',
                        tooltip = tooltipOptions(title = "Click for help")
                    ),
                    plotOutput("level1_balance_students_years"), 
                    title="Gender Balance (all students)", 
                    width=6),                
                box(
                    dropdownButton(
                    p("The figure displays the current proportion of male employees
                (y-axis) against the proportion of male employees in a previous year.
                Institutions located in the green area of the figure have currently a good
                      gender balance."),
                    icon = icon("info-circle"), width = "100%", size='sm',
                    tooltip = tooltipOptions(title = "Click for help")
                    ),
                    plotOutput("level1_prestigeplot"), 
                    selectInput("level1_prestigeplot_refyear", "Reference year: ", 
                                sort(unique(level1.employees$Årstall),decreasing=T)[-1],
                                width="33%"),
                    title="Gender Balance (all employees)", 
                    width=6),
                box(                    
                    dropdownButton(
                        p("This figure shows the percentage of males in a recruitment-
                          position (default is 'Student') against the percentage of 
                          males in top-positions. Institutions located in the upper
                          left part of the figure have more males in top-positions
                          than would be expected given the distribution in the recruitment
                          positions. The little 'tail' at the end of the point is the
                          development over time from earlier years to the most 
                          current year (large point)."),
                        icon = icon("info-circle"), width = "100%", size='sm',
                        tooltip = tooltipOptions(title = "Click for help")
                    ),
                    plotOutput("level1_spermplot"), 
                    fillRow(
                        selectInput("level1_spermplot_refpos", "Reference position: ", 
                                    positions, selected = "Student",
                                    width="80%"),
                        selectInput("level1_spermplot_maxpos", "High-prestige position: ", 
                                    positions, selected = "Professor",
                                    width="80%"),
                        height="60px",
                        ),
                    title="Gender Balance by position (all employees)", 
                    width=6),                
                box(                    
                    dropdownButton(
                    p("The figure displays the proportion of male employees
                (y-axis) as a function of academic position (roughly increasing
                in academic degree). It is often found that gender balance is 
                worse for positions with highest standing. The development over time
                is displayed with different shades of blue. Darker colors are earlier
                in time while the most current data is diplayed in light blue."),
                    icon = icon("info-circle"), width = "100%", size='sm',
                    tooltip = tooltipOptions(title = "Click for help")
                ),
                    plotOutput("level1_scissorsplot"), 
                    title="Gender Balance by position (all employees)", 
                    width=6),
                
        ),
      # ------------------------------------------------------------------------
      # Level 2
      # ------------------------------------------------------------------------
      tabItem(tabName = "level2",
              selectInput("level2_selectuni", "Select Institusjon: ", 
                          unique(level1$Kortnavn), selected = "UiT",
                          width="200px"),
              htmlOutput('level2_title'),
              box(DT::dataTableOutput("level2table"), 
                  title="Fakulteter", 
                  width=6),
              box(
                  dropdownButton(
                      p("This figures shows the development of the overall gender balance
                    (Percentage of employees that were men) at the institution over time.
                      Institutions close to 50% (grey line) have an almost optimal
                      gender balance."),
                      icon = icon("info-circle"), width = "100%", size='sm',
                      tooltip = tooltipOptions(title = "Click for help")
                  ),
                  plotOutput("level2_balance_years",height="500px"), 
                  title="Gender Balance (all employees)", 
                  width=6, height="600px"),     
              box(
                  dropdownButton(
                      p("This figures shows the development of the overall gender balance
                    (percentage of students that were men) at the institution over time.
                      Institutions close to 50% (grey line) have an almost optimal
                      gender balance."),
                      icon = icon("info-circle"), width = "100%", size='sm',
                      tooltip = tooltipOptions(title = "Click for help")
                  ),
                  plotOutput("level2_balance_students_years", height="500px"), 
                  title="Gender Balance (all students)", 
                  width=6, height="600px"),                
      ),
      
      
      # ------------------------------------------------------------------------
      # Level 3
      # ------------------------------------------------------------------------
      tabItem(tabName = "level3",
              fillRow(
                selectInput("level3_selectuni", "Select Institusjon: ", 
                            unique(level1$Kortnavn), selected = "UiT",
                            width="45%"),
                selectInput("level3_selectfac", "Select Faculty: ", 
                            unique(level2$Avdelingsnavn), selected = "Det helsevitenskapelige fakultet",
                            width="45%"),
                height="80px", width="100%"
              ),
              htmlOutput('level3_title'),
              box(DT::dataTableOutput("level3table"), 
                  title="Institutes", 
                  width=6),
              uiOutput("level3_divpips"), ## individual diverging pips plots for selected institutes (over years)
              box(
                dropdownButton(
                  p("This figures shows the development of the overall gender balance
                    (Percentage of employees that were men) at the institution over time.
                      Institutions close to 50% (grey line) have an almost optimal
                      gender balance."),
                  icon = icon("info-circle"), width = "100%", size='sm',
                  tooltip = tooltipOptions(title = "Click for help")
                ),
                plotOutput("level3_balance_years",height="500px"), 
                title="Gender Balance (all employees)", 
                width=6, height="600px"),     
              box(
                dropdownButton(
                  p("This figures shows the development of the overall gender balance
                    (percentage of students that were men) at the institution over time.
                      Institutions close to 50% (grey line) have an almost optimal
                      gender balance."),
                  icon = icon("info-circle"), width = "100%", size='sm',
                  tooltip = tooltipOptions(title = "Click for help")
                ),
                plotOutput("level3_balance_students_years", height="500px"), 
                title="Gender Balance (all students)", 
                width=6, height="600px")             
      ),
      # ------------------------------------------------------------------------
      # Help
      # ------------------------------------------------------------------------
        tabItem(tabName = "help",
                includeMarkdown("help.md")
        )
    )
)

dashboardPage(
    dashboardHeader(title = "Norge i Balanse"),
    sidebar,
    body
)