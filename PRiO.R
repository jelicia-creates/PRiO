# PRiO App


# Setup -------------------------------------------------------------------
req_pkgs <- c('shiny', 'tidyverse', 'bslib', 'magrittr', 'gridExtra', 'thematic', 'ragg', 'shinyscreenshot')
#install.packages(req_pkgs)
lapply(req_pkgs, require, character.only = TRUE)

options(shiny.useragg = TRUE)
thematic_shiny(font = 'auto')
thematic::thematic_on()


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = 'lux',
    base_font = bslib::font_google('Nunito Sans')
  ),
  #Application Title 
  tags$br(),
  titlePanel(tagList(h1('PRiO | Powering Productivity'), tags$br(), 
  HTML("The PRiO web application was created to help YOU unlock your productivity potential by reimagining your approach to
                                                      work. By adopting insights from the Eisenhower and Impact vs. Effort matrices, you can
                                                      better prioritize your efforts to maximize impact and focus your time + energy on what's most important.", style =
         'background-color: white;'),
  span(actionButton('refresh', 'Clear Inputs', icon = shiny::icon('refresh'),
               style = 'position: absolute; right: 40px; top: 180px; background-color: #F7F7F7; padding: 7px; border-color: #CECECE;'))
  )),
  tags$hr(),
  tags$br(),
  
  tags$head(
    tags$style(
      HTML('.eisenhower-green {
           background-color: #558464; 
           color: white;
           padding: 5px;
           display: inline-block;
           font-weight:bold;}'), 
      HTML('.ie-blue {
           background-color: #49637D;
           color: white; 
           padding: 5px;
           display: inline-block; 
           font-weight: bold;}')
    )
  ),
  
  #Designing Interactive Side Panel
  sidebarLayout(
    sidebarPanel(
      tags$div(
        tags$b("Input Your Project/Task Details"),
        tags$br(),
        tags$br(),
      ),
      textInput('proj_title', 'Title of Project/Item:', value = '', width = '400px'),
      sliderInput('urgency', 'Rank the Urgency Level (1 = Low/No Deadline...5 = High/Turnaround Time in < 1-2 Weeks)', min = 1, max = 5, value = 1), 
      sliderInput('importance', 'Rank the Importance Level (1 = Low/Nice-to-Have...5 = High/Mission Critical)', min = 1, max = 5, value = 1), 
      sliderInput('effort', 'Rank the Effort Level (1 = Low/Quick Win...5 = High/Longer Term Major Project)', min = 1, max = 5, value = 1), 
      sliderInput('impact', 'Rank the Impact Level (1 = Low/Minimal Reach...5 = High/Drives Critical Decisions for Key Stakeholders)', min = 1, max = 5, value = 1), 
      actionButton('save', 'Plot & Add New Item', style = 'margin-bottom: 5px;', icon = shiny::icon('chart-simple')),
      actionButton('screenshot', 'Take Screenshot', icon = shiny::icon('camera'), style = 'margin-bottom: 5px; float:right;')
    ),
    
    #Designing Main Panel for Plotting Projects on Matrices
    mainPanel(
      tabsetPanel(id = 'tabs', 
      tabPanel('Plot Your Ongoing Projects/Tasks',
               tags$br(),
      plotOutput('eisenhowerplot'),
      tags$br(),
      tags$br(),
      plotOutput('ieplot'),
      tags$br()), 
      tabPanel('Interpret Your PRiO Insights',
               tags$br(), 
               h5('Overview'),
               'With greater clarity on the level of importance, urgency, effort, and impact of your projects/tasks, you can begin implementing the recommendations below
               to get the most impact from your work and more plainly stated..."Work Smarter, Not Harder"',
               tags$br(),
               tags$br(),
               tags$h5(class = 'eisenhower-green', 'The Eisenhower Matrix - Importance vs. Urgency Explained'),
               tags$br(),
               'The Eisenhower Matrix offers a straightforward approach to categorizing projects by level of importance and urgency. With this approach, you can 
               easily identify your most important work and prioritize focusing more of your energy on the important things, while creating processes around or delegating 
               less important tasks.', 
               tags$br(),
               tags$br(),
               'For items identified as...',
               tags$br(),
               tags$br(),
               tags$div(
                 tags$ul(
                   tags$li(
                     HTML("<span style='color: #558464; font-weight: bold;'> IMPORTANT BUT NOT URGENT - Try SCHEDULING IT:</span>"), "Tasks in this quadrant contribute to long term success, but have flexible or unclear timelines. 
                     Unfortunately, this lack of urgency sometimes results in projects in this quadrant going untouched untouched until they become urgent. 
                     For these items, it's recommended that you schedule time to make incremental progress and leverage the flexibility as time to do additional 
                     exploration + add creative touches."
                   ), 
                   tags$br(),
                   tags$li(
                     HTML("<span style='color: #558464; font-weight: bold;'> URGENT AND IMPORTANT - Try DOING IT NOW:</span>"), "Tasks in this quadrant require are generally mission
                     critical and involve issues that demand an immediate resolution. As a result, this is also known as the 'High Stress' quadrant. Try to prioritize getting tasks in
                     this quadrant done as quickly as possible. Additionally, make it a practice to monitor the volume of projects that fall into this quadrant so you can identify 
                     when additional assistance or further delegation is necessary to maintain your overall wellnesss."
                     
                   ),
                   tags$br(),
                   tags$li(
                     HTML("<span style='color: #558464; font-weight: bold;'> NOT URGENT OR IMPORTANT - Try ELIMINATING IT:</span>."), "Tasks in this quadrant are 'affectionately
                     known' as TIME WASTERS. These are activities that provide little or no value to your goals or responsibilities. Try to eliminate any items that fall into this
                     quadrant as they're likely to lead to a lack of personal or professional growth, minimal productivity, and procrastination from more important work."
                           
                   ),
                   tags$br(),
                   tags$li(
                     HTML("<span style='color: #558464; font-weight: bold;'> URGENT BUT NOT IMPORTANT - Try DELEGATING OR AUTOMATING IT:</span>"), "Tasks in this quadrant give
                     the illusion of importance due to their urgency, but do not significantly contribute to long term objectives or goals. Consider tasks that fall into this quadrant
                     as distractions or interruptions that are actively pulling away from more important work. Try automating or delegating away tasks in this quadrant to maintain focus 
                     on the most meaningful aspects of your work and to avoid getting overwhelmed."
                   ),
                 )
               ), 
               tags$br(), 
               tags$br(), 
               tags$h5(class = 'ie-blue', 'The Action Priority Matrix - Impact vs. Effort Explained'),
               tags$br(),
               'The Action Priority Matrix also offers a straightforward approach to categorizing projects but instead by offering comparisons between the anticipated
               amount of effort and the expected "return" (impact). With this approach, you can easily make the most of your time by focusing your time and energy
               on more impactful work.', 
               tags$br(),
               tags$br(),
               'For items identified as...',
               tags$br(),
               tags$br(),
               tags$div(
                 tags$ul(
                   tags$li(
                     HTML("<span style='color: #49637D; font-weight: bold;'> HIGH IMPACT, LOW EFFORT - Try DOING IT FIRST:</span>"), "Tasks in this quadrant are 'quick wins' that 
                     have a require low effort and deliver quick and significant results/impact. Working on these tasks first will build momentum and a sense of accomplishment
                     which can prepare you to take on higher effort items with enthusiasm. Prioritize these tasks for the quick and impactful rewards."
                   ), 
                   tags$br(),
                   tags$li(
                     HTML("<span style='color: #49637D; font-weight: bold;'> HIGH IMPACT, HIGH EFFORT - Try PLANNING FOR IT:</span>"), "Tasks in this quadrant are major/larger projects
                     that also have a high impact, but require a substantial amount of focus, time, and effort. These projects are important and should be planned out and prioritized 
                     carefully. Try breaking down items in this quadrant into smaller, manageable tasks and creating timelines for completion. Also, be sure to schedule dedicated time
                     to work on tasks in this quadrant to maintain consistent progress and minimize stress."
                     
                   ),
                   tags$br(),
                   tags$li(
                     HTML("<span style='color: #49637D; font-weight: bold;'> LOW IMPACT, LOW EFFORT - Try DELEGATING OR AUTOMATING IT:</span>."), "Tasks in this quadrant are often referred to as
                     'thankless tasks' because they are generally unnecessary activities that provide little value. However, sometimes these tasks are just outright unavoidable. If that's the
                     case for you, try automating or delegating away tasks in this quadrant to free up time for more impactful endeavors like major projects or...self care!"
                     
                   ),
                   tags$br(),
                   tags$li(
                     HTML("<span style='color: #49637D; font-weight: bold;'> URGENT BUT NOT IMPORTANT - Try ELIMINATING IT:</span>"), "Tasks in this quadrant are often distractions or
                     busywork that are time consuming without delivering meaningful outcomes. It's advisable to eliminate on minimize time spent on these tasks as much as possible to
                     prioritize more impactful activities.")
          )
        )
      )
      )
    )
  )
)

  
# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
 # theme = bs_theme_update(theme, bootswatch = "lux")

  proj_df <- reactiveVal(data.frame(
    active_project = character(),
    urgency_rating = numeric(), 
    importance_rating = numeric(), 
    impact_rating = numeric(), 
    effort_rating = numeric(),
    stringsAsFactors = FALSE))
  
  observeEvent(input$save, {
    to_add <- data.frame(
      active_project = input$proj_title,
      urgency_rating = input$urgency, 
      importance_rating = input$importance, 
      impact_rating = input$impact, 
      effort_rating = input$effort
    )
    
    new_proj_df <- rbind(proj_df(), to_add) #adding new data to proj_df based on inputs
    proj_df(new_proj_df) #updating data
    
    #refreshing inputs so users can add a new project
    updateTextInput(session, 'proj_title', 'Title of Project/Item:', '')
    updateNumericInput(session, 'urgency', 'Rank the Urgency Level (1 = Low/No Deadline...5 = High/Turnaround Time in < 1-2 Weeks)', min = 1, max = 5, value = 1)
    updateNumericInput(session, 'importance', 'Rank the Importance Level (1 = Low/Nice-to-Have...5 = High/Mission Critical)', min = 1, max = 5, value = 1)
    updateNumericInput(session, 'effort', 'Rank the Effort Level (1 = Low/Quick Win...5 = High/Longer Term Major Project)', min = 1, max = 5, value = 1)
    updateNumericInput(session, 'impact', 'Rank the Impact Level (1 = Low/Minimal Reach...5 = High/Drives Critical Decisions for Key Stakeholders)', min = 1, max = 5, value = 1) 
    updateActionButton(session, 'save', 'Plot & Add New Item')
    updateActionButton(session, 'screenshot', 'Take Screenshot')
  })
  
  output$eisenhowerplot <- renderPlot({
    
  ggplot(proj_df(), aes(urgency_rating, importance_rating)) + 
      geom_hline(yintercept = 2.75) + 
      geom_vline(xintercept = 2.75) +
      scale_x_continuous(expand = c(0,0), limits = c(0, 5.5), breaks = seq(1, 5, 1)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, 5.5), breaks = seq(1, 5, 1)) + 
      theme_minimal() +
      theme(text = element_text(size = 14)) + 
      labs(color = 'Key:') + 
      annotate('rect', xmin = 2.75, xmax = 5.5, ymin = 2.75, ymax = 5.5, fill = '#E8F4EA') +
      annotate('rect', xmin = 0, xmax = 2.75, ymin = 0, ymax = 2.75, fill = '#E8F4EA') +
      annotate('rect', xmin = 2.75, xmax = 5.5, ymin = 0, ymax = 2.75, fill = '#FFFFFF') +
      annotate('rect', xmin = 0, xmax = 2.75, ymin = 2.75, ymax = 5.5, fill = '#FFFFFF') +
      geom_label(aes(x = 1.35, y = 5.35, label = 'IMPORTANT BUT NOT URGENT'), label.padding = unit(2, 'mm'), fill = '#558464', color = '#FFFFFF') + 
      geom_label(aes(x = 1.35, y = 0.18, label = 'NOT URGENT OR IMPORTANT'), label.padding = unit(2, 'mm'), fill = '#558464', color = '#FFFFFF') + 
      geom_label(aes(x = 4.2, y = 5.35, label = 'URGENT AND IMPORTANT'), label.padding = unit(2, 'mm'), fill = '#558464', color = '#FFFFFF') +
      geom_label(aes(x = 4.2, y = 0.18, label = 'URGENT BUT NOT IMPORTANT'), label.padding = unit(2, 'mm'), fill = '#558464', color = '#FFFFFF') +
      geom_point(aes(color = active_project), position = position_dodge(width = 0.05), size = 6) +
      geom_text(aes(label = active_project), nudge_x = 0, nudge_y = 0.4, size = 5, check_overlap = TRUE, fontface = 'bold') + 
      scale_color_manual(values = c('#204035FF', '#BEB59CFF', '#735231FF', '#49271BFF', '#273649FF','#226060FF', '#4F3855FF', '#803342FF', '#2D1E78FF', '#1BB6AFFF')) +
      ggtitle('EISENHOWER MATRIX - IMPORTANCE vs. URGENCY') +
      theme(plot.title = element_text(size = 18, margin = margin(0, 0, 30, 0), color = '#2E4542', family = bslib::font_google('Nunito Sans'))) + 
      labs(x = 'URGENCY', y = 'IMPORTANCE') + 
      theme(axis.title.x = element_text(size = 12, face = 'bold', color = '#2E4542')) +
      theme(axis.title.y = element_text(size = 12, face = 'bold', color = '#2E4542')) +
      theme(
        axis.text = element_text(size = 10),
        legend.position = 'bottom',
        axis.line.y = element_line(arrow = grid::arrow(length = unit(4, 'mm'), ends = 'last'), color = '#2E4542'),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(4, 'mm'), ends = 'last'), color = '#2E4542')
      )
  })
    
    output$ieplot <- renderPlot({
      
      ggplot(proj_df(), aes(effort_rating, impact_rating)) + 
        geom_hline(yintercept = 2.75) + 
        geom_vline(xintercept = 2.75) +
        scale_x_continuous(expand = c(0,0), limits = c(0, 5.5), breaks = seq(1, 5, 1)) +
        scale_y_continuous(expand = c(0,0), limits = c(0, 5.5), breaks = seq(1, 5, 1)) + 
        theme_minimal() +
        theme(text = element_text(size = 14)) + 
        annotate('rect', xmin = 2.75, xmax = 5.5, ymin = 2.75, ymax = 5.5, fill = '#CEE5ED') +
        annotate('rect', xmin = 0, xmax = 2.75, ymin = 0, ymax = 2.75, fill = '#CEE5ED') +
        annotate('rect', xmin = 2.75, xmax = 5.5, ymin = 0, ymax = 2.75, fill = '#FFFFFF') +
        annotate('rect', xmin = 0, xmax = 2.75, ymin = 2.75, ymax = 5.5, fill = '#FFFFFF') +
        geom_label(aes(x = 1.35, y = 5.35, label = 'LOW EFFORT, HIGH IMPACT'), label.padding = unit(2, 'mm'), fill = '#49637d', color = '#FFFFFF') + 
        geom_label(aes(x = 1.35, y = 0.18, label = 'LOW EFFORT, LOW IMPACT'), label.padding = unit(2, 'mm'), fill = '#49637d', color = '#FFFFFF') + 
        geom_label(aes(x = 4.2, y = 5.35, label = 'HIGH EFFORT, HIGH IMPACT'), label.padding = unit(2, 'mm'), fill = '#49637d', color = '#FFFFFF') +
        geom_label(aes(x = 4.2, y = 0.18, label = 'HIGH EFFORT, LOW IMPACT'), label.padding = unit(2, 'mm'), fill = '#49637d', color = '#FFFFFF') +
        geom_point(aes(color = active_project), size = 6) +
        geom_text(aes(label = active_project), nudge_x = 0, nudge_y = 0.4, size = 5, check_overlap = TRUE, fontface = 'bold') +
        scale_color_manual(values = c('#204035FF', '#BEB59CFF', '#735231FF', '#49271BFF', '#273649FF','#226060FF', '#4F3855FF', '#803342FF', '#2D1E78FF', '#1BB6AFFF')) +
        ggtitle('ACTION PRIORITY MATRIX - IMPACT vs. EFFORT') +
        theme(plot.title = element_text(size = 18, margin = margin(0, 0, 30, 0), color = '#3A5874', family = bslib::font_google('Nunito Sans'))) + 
        labs(x = 'EFFORT', y = 'IMPACT') + 
        theme(axis.title.x = element_text(size = 12, face = 'bold', color = '#3A5874')) +
        theme(axis.title.y = element_text(size = 12, face = 'bold', color = '#3A5874')) +
        theme(
          axis.text = element_text(size = 10),
          legend.position = 'none',
          axis.line.y = element_line(arrow = grid::arrow(length = unit(4, 'mm'), ends = 'last'), color = '#3A5874'),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(4, 'mm'), ends = 'last'), color = '#3A5874')
        ) 
    })
    
    #screenshot
    observeEvent(input$screenshot, {
      screenshot(filename = paste0('My PRiO Matrix_', Sys.Date()))
    })
    

    #refresh plots
    observeEvent(input$refresh, {
      session$reload()
      })
    
    
}

#launch the application
runApp(shinyApp(ui = ui, server = server))