ui <- dashboardPage(skin = 'red',
  dashboardHeader(
    title = 'Simulation Dashboard'
  ),
  
  
  dashboardSidebar(sidebarMenu(
    menuItem('Overview', tabName = 'overview', icon = icon('home')),
    menuItem('Correlation Analysis', tabName = 'cor_analysis', icon = icon('link')),
    menuItem('Municipality Info', tabName = 'munc_info', icon = icon('city'))
  )),
  
  dashboardBody(
    tabItems(
      #tabItem for overview page
      tabItem(tabName='overview', 
              fluidRow(
                column(6,
                       box(width = '100%', title = 'Simulated Municipalities',
                           plotlyOutput('Places_Overview'))),
                column(6,
                       box(width = '100%', title = 'Infections Overview (Median)',
                           plotlyOutput('Median_Infections_Overview')))
              ),
              fluidRow(
                column(6,
                       box(width = '100%', title = 'Infections Overview (Mean)',
                           plotlyOutput('Mean_Infections_Overview'))),
                column(6,
                       box(width = '100%', title = 'Variance Overview',
                           plotlyOutput('Variance_Overview')))
              ),
              fluidRow(
                column(12,
                       box(width = '100%', title = 'Density Overview',
                           plotlyOutput('Density_Overview'))
              )
              ), 
              fluidRow(
                column(12,
                       box(width = '100%', title = 'Infections in Municipalities',
                           plotlyOutput('InfectionsMunBox'))
                )
              ),
              fluidRow(
                column(12,
                       box(width = '100%', title = 'ANOVA Infections ~ Municipality',
                           verbatimTextOutput('InfectionsMunANOVA'))
                )
              ), 
              fluidRow(
                column(12,
                       box(width = '100%', title = 'Infections in RegBez',
                           plotlyOutput('InfectionsRegBezBox'))
                )
              ),
              fluidRow(
                column(12,
                       box(width = '100%', title = 'ANOVA Infections ~ RegBez',
                           verbatimTextOutput('InfectionsRegBezANOVA'))
                )
              )
      ),
      
      #tabItem for correlations page
      tabItem(tabName = 'cor_analysis',
              fluidRow(
                box(width = '100%', title = "Complete Correlations Matrix",
                    plotOutput('CorFullMatrix'))
              ),
              
              fluidRow(
                column(4,
                       selectInput('valueA', 'Select 1st Correlation Value', colnames(reduceOverview()))
                ),
                column(8,
                       selectInput('valueB', 'Select 2nd Correlation Value', colnames(reduceOverview()))
                )
              ),
              fluidRow(
                column(4,
                       box(title = "Correlation by Value", width = 12,
                           verbatimTextOutput("FullCorData"))),
                column(8,
                       box(title = "Scatterplot and Linear Regression", width = 12,
                           plotlyOutput('PlotLinReg')))
                
              )
      ),
      
      #tabItem for single munc page
      tabItem(tabName = 'munc_info', tags$head(tags$style( type = 'text/css',  '.rpivotTable{ overflow-x: scroll; }')),
              #fluidPage(
                
                fluidRow(
                  column(8,
                         selectInput('selected_munc', 'Select Municipality', cnames$Name)
                  )
                ),
                fluidRow(
                  column(6,
                         box(title = 'Infections by Age', width = '100%',
                             imageOutput('InfAge'))),
                  column(6,
                         box(title = 'Percentage of Infections per Age', width = '100%',
                             imageOutput('InfPercPerAge')))
                ),
                fluidRow(
                  column(12,
                         box(height = "1100px", width = '100%',
                             plotlyOutput('muncMap'))
                  )),
                fluidRow(
                  column(12,
                         box(width = '100%', height = "1100px", plotlyOutput('timeCases')))
                ),
                
                fluidRow(
                  box(title = 'General Information',
                      dataTableOutput('GeneralMunc'))
                )
              #)

      )
      
      
    )
    
  )
  
)

