library(tidyverse)
library(shiny)

library(reactable)

data <- read_csv("satgpa.csv")

ui <- fluidPage(
  h2("Statistical Measures"),
  tabsetPanel(
    tabPanel("Description", fluid = TRUE,
             h1('Introduction'),
             h4('Created By: Helen Lin'),
             p('I chose to create an interactive display based on a dataset
               of SAT scores and GPA data for 1000 students at an
               unnamed college. The dataset contains the students\' high
               school GPA, SAT score, and first year GPA.'),
             p('To look at the measures of central tendencies on this data, go to
               the \'SAT\' tab, and click through the tabs below the main plot.
               The \'SAT and GPA\' tab displays a scatterplot
               that plots points for (high school GPA, SAT score) and 
               (first year GPA, SAT score), and describes the correlations
               in the data if there are any.'),
             helpText(a("Click here to access the dataset.",
                        href="https://www.openintro.org/data/index.php?data=satgpa",
                        target = '_blank')),
             p('This program was created using RStudio and Shiny.'),
             ),
    tabPanel("SAT", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("min_sat", "Min SAT:",
                             min = 400, max = 1600, value = 400),
                 sliderInput("max_sat", "Max SAT:",
                             min = 400, max = 1600, value = 1600),
                 sliderInput("sat_bins","Bins",min=5,max=60,value=20)
               ),
               mainPanel(
                 tabPanel("SAT", plotOutput("sat")),
                 )),
             reactableOutput("sat_measures"),
             tabsetPanel(
               tabPanel("Mean", fluid = TRUE,
                        h4('Mean'),
                        uiOutput("mean_inst")
                        ),
               tabPanel("Median", fluid = TRUE,
                        h4('Median'),
                        uiOutput("median_inst")
               ),
               tabPanel("Mode", fluid = TRUE,
                        h4('Mode'),
                        uiOutput("mode_inst")
               ),
               tabPanel("Range", fluid = TRUE,
                        h4('Range'),
                        uiOutput("range_inst")
               ),
               tabPanel("Standard Deviation", fluid = TRUE,
                        h4('Standard Deviation'),
                        uiOutput("sd_inst")
               ),
               tabPanel("Distributions", fluid = TRUE,
                        h4('Distributions'),
                        uiOutput("dist_inst")
               ),
               ),
             ),
    tabPanel("SAT and GPA", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("sample_size", "Display __ points:",
                             min = 1, max = 1000, value = 50),
                 p("Click to show or hide:"),
                 actionButton("hsgpa", "High School GPA, SAT"),
                 actionButton("fygpa", "First Year GPA, SAT")
               ),
               mainPanel(
                 tabPanel("all", plotOutput("all_measures")),
               )),
             h4('Correlations'),
             uiOutput("corr_inst")
    )
  )
)

server <- function(input, output, session) {
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  getMeasures <- function(x) {
    df <- data.frame(
      Mean = format(round(mean(x),2)),
      Median = format(round(median(x),2)),
      Mode = format(round(getmode(x),2)),
      Range = format(round(max(x) - min(x),2)),
      SD = format(round(sd(x),2))
    )
  }
  
  observeEvent(input$tabs, {
    updateSliderInput(session, "min_sat", value = 400)
    updateSliderInput(session, "max_sat", value = 1600)
  })
  
  filtered_sat <- reactive({
    data[data$sat_sum >= input$min_sat & data$sat_sum <= input$max_sat,]
  })
  
  output$sat <- renderPlot({
    x <- filtered_sat()[['sat_sum']]
    bins <- seq(input$min_sat, input$max_sat, length.out = input$sat_bins)
    
    hist(main = "SAT Score Distribution", xlab = "Score", ylab = "Frequency",
         x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$sat_measures <- renderReactable({
    df <- getMeasures(filtered_sat()[['sat_sum']])
    reactable(df)
  })
  
  rv <- reactiveValues(
    showHS = TRUE,
    showFY = TRUE)
  
  output$all_measures <- renderPlot({
    all_sat <- filtered_sat()[['sat_sum']][1:input$sample_size]
    all_hsgpa <- unlist(data$hs_gpa[1:input$sample_size])
    all_fygpa <- unlist(data$fy_gpa[1:input$sample_size])
    
    plot(1, main = 'Correlation Bewteen GPA and SAT Scores',
         xlab = 'GPA', ylab = 'SAT Score', col = 'blue',
         xlim = c(1,4.5), ylim = c(400,1600))
    
    if(rv$showHS)
      points(all_hsgpa, all_sat, pch = 20, col = 'blue')
    
    if(rv$showFY)
      points(all_fygpa, all_sat, pch = 20, col = 'pink')
    
    legend("topleft", legend = c('High School GPA', 'First Year GPA'),
           col = c('blue','pink'), pch = 20, bg = 'lightblue')
  })
  
  observeEvent(input$hsgpa, {
    rv$showHS <- !rv$showHS
  })
  
  observeEvent(input$fygpa, {
    rv$showFY <- !rv$showFY
  })
  
  output$mean_inst <- renderUI(
    HTML("<ol>
            <li>The mean is the average of all values. We can calculate
              the mean by adding up all the scores and dividing by the
              number total number of scores. 50% of the scores are greater
              than this mean and 50% of the scores are less than this
              mean. The mean of the histogram above is 1033.29. </li>
            <li>Drag the \'Min SAT\' slider to \'1000\'. The mean should
              increase since we are excluding any scores below 1000.</li>
            <li>Now reset the \'Min SAT\' slider and drag the \'Max SAT\'
              slider to \'1000\'. The mean should decrease since we are
              excluding any scores above 1000.</li>
         </ol>")
  )
  
  output$median_inst <- renderUI(
    HTML("<ol>
            <li>The median is the \'middle\' score; the point that divides
              the distribution into the lower half of values and the upper
              half of values. The median is calculated by ordering the
              scores from least to greatest and locating the middle (odd
              number of scores) or the middle two numbers (even number of
              scores). The median of the histogram above is 1030. </li>
            <li>Drag the \'Min SAT\' slider to \'1000\'. The median
              should change to reflect the new \'middle.\'</li>
            <li>Now reset the \'Min SAT\' slider and drag the \'Max SAT\'
              slider to \'1000\'. The median
              should change to reflect the new \'middle.\'</li>
         </ol>")
  )
  
  output$mode_inst <- renderUI(
    HTML("<ol>
            <li>The mode is the score that occurs the most often. The mode
              is calculated by counting which score occurs the most often.
              The mode of the histogram above is 1030.</li>
            <li>Drag the \'Min SAT\' slider to any value above \'1030\'.
              The mode should change to reflect the new most common value
              since 1030 is no longer included in the data.</li>
            <li>Now reset the \'Min SAT\' slider and drag the \'Max SAT\'
              slider to any value below \'1030\'. The mode should change to
              reflect the new most common value since 1030 is no longer
              included in the data.</li>
         </ol>")
  )
  
  output$range_inst <- renderUI(
    HTML("<ol>
            <li>The range is the higest score minus the lowest score. The
              range of the histogram above is 910 since the highest score is
              1440 and the lowest score is 530: 1440 - 530 = 910.</li>
            <li>Drag the \'Min SAT\' slider to \'1000\'. The range should
              decrease since we're considering a smaller range of values.</li>
            <li>Now reset the \'Min SAT\' slider and drag the \'Min SAT\'
              slider to \'1000\'. The range should decrease since we're 
              considering a smaller range of scores.</li>
         </ol>")
  )
  
  output$sd_inst <- renderUI(
    HTML("<ol>
            <li>The standard deviation is the average distance (or deviation)
              from the mean. It considers the distance between each score
              and the mean. A higher standard deviation means the scores
              are more scattered and a lower standard deviation means the
              scores are clustered together. The standard deviation of the
              above histogram is 142.87. This means that on average, scores
              are 142.87 points from the mean.</li>
            <li>Drag the \'Min SAT\' slider to \'800\' and the \'Max SAT\' 
              slider to \'1200\'. The standard deviation should decrease
              since we're considering a smaller range of scores, which will
              be more tightly clustered.</li>
         </ol>")
  )
  
  output$dist_inst <- renderUI(
    HTML("<ol>
            <li>The data is unimodal: there is only one \'hump\' since there
            is a singular highest value.</li>
            <li>The data is symmetrical: the scores appear at regular intervals
            around the mean.</li>
            <li>Drag the \'Min SAT\' slider to \'1000.\' The updated histogram
            is now \'positively skewed\' since the tail extends to the left.
            </li>
            <li>Reset the \'Min SAT\' slider and drag the \'Max SAT\' slider
            to \'1000.\' The updated histogram is now \'negatively skewed\' 
            since the tail extends to the left.
            </li>
         </ol>")
  )
  
  output$corr_inst <- renderUI(
    HTML("<ol>
            <li>Correlation is a statistical measure that indicates the
            entent to which two or more variables fluctuate together. A
            positive relationship means that as X increases, Y increases,
            and a negative relationship measn that as X increases, Y
            decreases.</li>
            <li>This set of data has no correlation. Therefore, for this set
            of students, the SAT score has no affect on the GPA.</li>
         </ol>")
  )
  
}

shinyApp(ui = ui, server = server)

