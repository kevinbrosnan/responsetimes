

library(shiny)
library(shinydashboard)

dashboardPage(title = "Response-Times",
  skin = ("red"),
  dashboardHeader(title = "Response-Times",
                  
                  # Email Sharing link
                  tags$li(class = "dropdown",
                          tags$a(href = "mailto:?Subject=Response Time of Elite Sprinters&Body=Effects of false-start disqualification rules on response-times of elite-standard sprinters. Brosnan, K.C., Hayes, K, Harrison, A.J. (2016). http://significantstats.org/shiny/responsetimes/ ",
                                 tags$img(height = "18px", 
                                          src = "images/email.png")
                          )
                  ),
                  
                  # Twitter Sharing Link
                  tags$li(class = "dropdown",
                          tags$a(href = "http://twitter.com/share?url=http://significantstats.org/shiny/responsetimes/&text=Response-Times of Elite-Standard Sprinters from @kevbros93 @KevHayes235 and @drewhar06660675 in @JSportsSci ", 
                                 target = "_blank", 
                                 tags$img(height = "18px", 
                                          src = "images/twitter.png")
                          )
                  ),
                  
                  # Facebook Sharing link
                  tags$li(class = "dropdown",
                          tags$a(href = "http://www.facebook.com/sharer.php?u=http://significantstats.org/shiny/responsetimes/", 
                                 target = "_blank", 
                                 tags$img(height = "18px", 
                                          src = "images/facebook.png")
                          )
                  ),
                  
                  # LinkedIn Sharing link
                  tags$li(class = "dropdown",
                          tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=http://significantstats.org/shiny/responsetimes/", 
                                 target = "_blank", 
                                 tags$img(height = "18px", 
                                          src = "images/linkedin.png")
                          )
                  )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("home")),
      menuItem("Data", tabName = "datafile", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("binoculars")),
      menuItem("Paper", tabName = "paper", icon = icon("file-pdf-o")),
      menuItem("Presentation", tabName = "present", icon = icon("microphone")),
      menuItem("About", tabName = "about", icon = icon("info")),
      hr(),
      sidebarUserPanel(name = a("Kevin Brosnan", target = "_blank_",
                                href = "http://significantstats.org"), 
                       subtitle = "Applied Statistician",
                       image = "images/kevinbrosnan.png"),
      sidebarUserPanel(name = a("Dr. Kevin Hayes", target = "_blank_",
                                href = "http://www.ulsites.ul.ie/macsi/kevin-hayes-profile"), 
                       subtitle = "Applied Statistician",
                       image = "images/kevinhayes.png"),
      sidebarUserPanel(name = a("Prof. Andrew Harrison", target = "_blank_",
                                href = "http://www.ul.ie/pess/iframe-staff/dr-drew-harrison-profile"), 
                       subtitle = "Sports Scientist",
                       image = "images/drewharrison.png"),
      hr(),
      menuItem("Source code", icon = icon("file-code-o"), 
               href = "https://github.com/significantstats/responsetimes"),
      menuItem("Bug Reports", icon = icon("bug"),
               href = "https://github.com/significantstats/responsetimes/issues")
    )
  ),
  
  dashboardBody(
    tags$head(includeScript("www/js/google-analytics.js"),
              HTML('<link rel="apple-touch-icon" sizes="57x57" href="icons/apple-icon-57x57.png">
                    <link rel="apple-touch-icon" sizes="60x60" href="icons/apple-icon-60x60.png">
                    <link rel="apple-touch-icon" sizes="72x72" href="icons/apple-icon-72x72.png">
                    <link rel="apple-touch-icon" sizes="76x76" href="icons/apple-icon-76x76.png">
                    <link rel="apple-touch-icon" sizes="114x114" href="icons/apple-icon-114x114.png">
                    <link rel="apple-touch-icon" sizes="120x120" href="icons/apple-icon-120x120.png">
                    <link rel="apple-touch-icon" sizes="144x144" href="icons/apple-icon-144x144.png">
                    <link rel="apple-touch-icon" sizes="152x152" href="icons/apple-icon-152x152.png">
                    <link rel="apple-touch-icon" sizes="180x180" href="icons/apple-icon-180x180.png">
                    <link rel="icon" type="image/png" sizes="192x192"  href="icons/android-icon-192x192.png">
                    <link rel="icon" type="image/png" sizes="32x32" href="icons/favicon-32x32.png">
                    <link rel="icon" type="image/png" sizes="96x96" href="icons/favicon-96x96.png">
                    <link rel="icon" type="image/png" sizes="16x16" href="icons/favicon-16x16.png">
                    <link rel="manifest" href="icons/manifest.json">
                    <meta name="msapplication-TileColor" content="#ffffff">
                    <meta name="msapplication-TileImage" content="icons/ms-icon-144x144.png">
                    <meta name="theme-color" content="#ffffff">')),
    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro", includeHTML("intro.html")),
      
      # Data Tab
      tabItem(tabName = "datafile",
              style = "overflow-y:scroll;",
              box(width = 12, 
                  title = "Response Times 1999-2014 - Select a column for Validation", 
                  DT::dataTableOutput("dtrt")
              ),
              box(width = 4, DT::dataTableOutput("dtrtval")),
              box(width = 8, plotOutput("plotrtval", height = "300px"))
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              style = "overflow-y:scroll;",
              box(width = 6, height = "280px", title = "User Inputs",
                  HTML("If you wish to focus on a particular Sex of athlete 
                       please select the appropriate radiobutton below."),
                  radioButtons("subsex", label = "Subset by Sex:", 
                               choices = c(unique(response.times$Sex), "All"),
                               selected = "All", inline = TRUE),
                  HTML("Select the grouping variable which you want to compare  
                       across using the appropriate radiobutton below."),
                  radioButtons("comparison", label = "Compare across:",
                               choices = c("Sex", "Rounds", "Rule Period"),
                               selected = "Rule Period", inline = TRUE)
              ),
              box(width = 6, height = "280px", 
                  title = "Model Fit - Exponentially Modified Gaussian Distribution",
                  DT::dataTableOutput("modeltab")),
              box(width = 6, title = "Descriptive Statistics",
                  plotOutput("analdesc", height = "350px")
              ),
              box(width = 6, title = "Results",
                  plotOutput("modelplot", height = "350px")
              )
      ),
      
      # Journal Paper Tab
      tabItem(tabName = "paper",
              tags$iframe(style = "height:calc(100vh - 80px); width:100%", 
                          src = "docs/JSSBrosnanHayesHarrison2016.pdf")  
      ),
      
      # Presentation Tab
      tabItem(tabName = "present", includeHTML("presentation.html")
      ),
      
      # About Tab
      tabItem("about", includeMarkdown("about.md"))
    )
  )
)

