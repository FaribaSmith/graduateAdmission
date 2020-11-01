library(shiny)
library(tidyverse)
library(caret)
library(ggplot2)

# loading the data 
data <- read.csv("schooladmission.csv")
data1 <- data

# change numeric labels into characters that are categorized for data
data$admit[data$admit == 0] <- 'No'
data$admit[data$admit == 1] <- 'Yes'
data$admit = factor(data$admit)

# change numeric labels into characters that are categorized for data1
data1$admit[data1$admit == 0] <- "Not admitted"
data1$admit[data1$admit == 1] <- "Admitted"

# Building the different models
# data partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind == 1,]
test <- data[ind == 2,]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="repeatedcv", number=10, repeats = 3)
metric <- "Accuracy"

# kNN
set.seed(7)
fit.knn <- train(admit~., data=data, method="knn",
                 metric=metric, trControl=control, preProcess = c("center", "scale"))

# SVM
set.seed(7)
fit.svm <- train(admit~., data=data, method="svmRadial",
                 metric=metric, trControl=control, preProcess = c("center", "scale"))

# CART
set.seed(7)
fit.cart <- train(admit~., data=data, method="rpart",
                  metric=metric, trControl=control, preProcess = c("center", "scale"))

# Random Forest
set.seed(7)
fit.rf <- train(admit~., data=training, method="rf",
                metric=metric, trControl=control, preProcess = c("center", "scale"))


algos <- list("knn (K-Nearest Neighbors)" = "knn",
              "svm (Support Vector Machine)" = "svm",
              "cart (Classification And Regression Trees)" = "cart",
              "rf (Random Forest)" = "rf")

# Make scatter plots of GRE versus GPA for each school rank

rank1 <- data %>% 
    filter(rank =="1")

prank1 <- ggplot(rank1, aes(x=gpa, y=gre, color=admit)) +
    geom_point() + 
    ggtitle(" school rank = 1") +
    xlab("GPA") + ylab("GRE") +
    scale_color_manual(labels = c("Not admitted", "Admitted"), values = c("blue", "red")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(" "))

rank2 <- data %>% 
    filter(rank =="2")

prank2 <- ggplot(rank2, aes(x=gpa, y=gre, color=admit)) +
    geom_point() + 
    ggtitle(" school rank = 2") +
    xlab("GPA") + ylab("GRE") +
    scale_color_manual(labels = c("Not admitted", "Admitted"), values = c("blue", "red")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(" "))

rank3 <- data %>% 
    filter(rank =="3")

prank3 <- ggplot(rank3, aes(x=gpa, y=gre, color=admit)) +
    geom_point() + 
    ggtitle(" school rank = 3") +
    xlab("GPA") + ylab("GRE") +
    scale_color_manual(labels = c("Not admitted", "Admitted"), values = c("blue", "red")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(" "))

rank4 <- data %>% 
    filter(rank =="4")

prank4 <- ggplot(rank4, aes(x=gpa, y=gre, color=admit)) +
    geom_point() + 
    ggtitle(" school rank = 4") +
    xlab("GPA") + ylab("GRE") +
    scale_color_manual(labels = c("Not admitted", "Admitted"), values = c("blue", "red")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(" "))



ui <- fluidPage(
    navbarPage("Welcom to Graduate College Admission App",
               
               tabPanel("About", # -------------------------------------------
                        uiOutput("intro")
                        
               ), # About -------------------------------------------
               
               navbarMenu("Data", # ------------------------------
                        tabPanel("Data table",
                                 titlePanel("School Admission DataTable"),
                                 
                                 # Create a new Row in the UI for selectInputs
                                 fluidRow(
                                     column(2,
                                            selectInput("admitT",
                                                        "Admission:",
                                                        c("All", "Admitted", "Not admitted"))
                                     ),
                                     column(2,
                                            selectInput("greT",
                                                        "GRE:",
                                                        c("All", "GRE > 600", "GRE < 600"))
                                     ),
                                     column(2,
                                            selectInput("gpaT",
                                                        "GPA:",
                                                        c("All", "4 > GPA > 3", "3 > GPA > 1"))
                                     ),
                                     column(2,
                                            selectInput("rankT",
                                                        "School's rank:",
                                                        c("All", "rank = 1", "rank = 2", "rank = 3", "rank = 4" ))
                                     )
                                 ),
                                 # Create a new row for the table.
                                 DT::dataTableOutput("table"), 
                                 #   datatable(head("table"), options = list(dom = 'ltipr'))
                        
                        ),# Data table
                        tabPanel("Data statistics",
                                 sidebarLayout(
                                     
                                     # Define the sidebar with one input
                                     sidebarPanel(
                                         selectInput("parameter", "Parameter:",
                                                     choices=colnames(data)),
                                         hr(),
                                         helpText("Data from graduate school admission")
                                     ),
                                     
                                     # Create a spot for the barplot
                                     mainPanel(
                                         plotOutput("paramPlot")
                                     )
                                     
                                 )
                                 
                                 
                        ),# Data statistics
                        tabPanel("Data exploration",
                                 selectInput("selectExplor", label = h3("Let's explore the data:"), 
                                             choices = list("correlation between features" = 1, 
                                                            "scatter plot of features" = 2,
                                                            "percentage of admitted students" = 3,
                                                            "GPA statistics for admitted students" = 4, 
                                                            "GRE statistics for admitted students" = 5),
                                             selected = 1),
                                 
                                 mainPanel(
                                     # textOutput(outputId = "textR"),
                                     uiOutput("dataExplore")
                                 )
                                 
                                 
                        )# Data exploration
                        
               ),# Data --------------------------------------------
               navbarMenu("Machine learning models", # ------------------------------
                          tabPanel("KNN",
                                   mainPanel(uiOutput("KNNExplore"))),
                          tabPanel("SVM",
                                   mainPanel(uiOutput("SVMExplore"))),
                          tabPanel("CART",
                                   mainPanel(uiOutput("CARTExplore"))),
                          tabPanel("RF",
                                   mainPanel(uiOutput("RFExplore")))
                        
                        ),# ML models --------------------------------------
               
               tabPanel("Predicting admission", # --------------------------
                        titlePanel("Predicting the admission:"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("selection", "Choose a model:",
                                        choices = algos),
                                sliderInput("gre", "GRE score:",
                                        min = 1,  max = 800, value = 650, step = 1),
                                sliderInput("gpa", "GPA score:",
                                        min = 1,  max = 4,  value = 3, step = 0.01),
                                numericInput("rank", "school's rank:", 
                                        min = 1,  max = 4, value = 2, step = 1),
                                actionButton("Run_model", "Run model"),
                                actionButton("plot_point", "plot")
                            ),# Predicting admission
                            
                            mainPanel(
                                textOutput("select_algo"),
                                textOutput("result"),
                                plotOutput("youAreHere")
                            )
                        )
                        
                        ) # Predicting admission ---------------------------
               

)#navbarpage
)#fluidepage



server <- function(input, output, session) {
    
    # about
    output$intro <- renderUI({img(src='intro.png', height = '900px')})
    

    # Data ---------------------------------------------------------- 
    
    output$table <- DT::renderDataTable(DT::datatable({
        
        if (input$admitT != "All") {
            data1 <- data1[data1$admit == input$admitT,]
        }
        if (input$greT != "All") {
            if (input$greT == "GRE > 600") {
                data1 <- data1[data1$gre > 600,]
            }else {
                data1 <- data1[data1$gre <= 600,]
            }
        }
        if (input$gpaT != "All") {
            if (input$gpaT == "4 > GPA > 3") {
                data1 <- data1[data1$gpa > 3,]
            }else {
                data1 <- data1[data1$gpa <= 3,]
            }
        }
        if (input$rankT != "All") {
            switch(input$rankT,
                   "rank = 1" = {data1 <- data1[data1$rank == 1,]},
                   "rank = 2" = {data1 <- data1[data1$rank == 2,]},
                   "rank = 3" = {data1 <- data1[data1$rank == 3,]},
                   "rank = 4" = {data1 <- data1[data1$rank == 4,]})
            
        }
        data1
        
    }))
    
    output$paramPlot <- renderPlot({
        
        if (input$parameter == "admit") {
            slices <- c(sum(data$admit == "No"), sum(data$admit == "Yes")) 
            lbls <- c("not admitted", "admitted")
            pct <- round(slices/sum(slices)*100)
            lbls <- paste(lbls, pct) # add percents to labels 
            lbls <- paste(lbls,"%",sep="") # ad % to labels 
            pie(slices,labels = lbls, col=rainbow(length(lbls)),
                main="Pie Chart of admissions")
        }
        
        if (input$parameter == "gre") {
            hist(data[,input$parameter],
                 freq = TRUE,
                 col = 'darkmagenta',
                 main=input$parameter,
                 ylab="frequency",
                 xlab="range of values")
        }
        
        if (input$parameter == "gpa") {
            hist(data[,input$parameter],
                 freq = TRUE,
                 col = 'blue',
                 main=input$parameter,
                 ylab="frequency",
                 xlab="range of values")
        }
        
        if (input$parameter == "rank") {
            slices <- c(sum(data$rank == 1), sum(data$rank == 2),
                        sum(data$rank == 3), sum(data$rank == 4)) 
            lbls <- c("rank 1   ", "rank 2   ", "rank 3   ", "rank 4   ")
            pct <- round(slices/sum(slices)*100)
            lbls <- paste(lbls, pct) # add percents to labels 
            lbls <- paste(lbls,"%",sep="") # ad % to labels 
            pie(slices,labels = lbls, col=rainbow(length(lbls)),
                main="Pie Chart of school ranks")
            
        }
        
        choix <- reactive ({switch(input$selectExplor,
                                   "1" = 1,
                                   "2" = 2,
                                   "3" = 3,
                                   "4" = 4,
                                   "5" = 5
        ) # switch
            
        }) 
        # output$textR<-renderText({
        #     print(choix())
        # })

        
        output$dataExplore <- renderUI({
            switch(choix(),
                   "1" = img(src='corr.png', height = '500px'),
                   "2" = img(src='scatterplots.png', height = '1200px'),
                   "3" = img(src='percentAdmitted.png', height = '500px'),
                   "4" = img(src='GPA.png', height = '500px'),
                   "5" = img(src='GRE.png', height = '500px')
            ) # switch
            
            
        }) # choix
        
    }) # Data -----------------------------------------------------
    
    # ML models -----------------------------------------------------------
    
    output$KNNExplore <- renderUI({img(src='KNN.png', height = '800px')})
    
    output$SVMExplore <- renderUI({img(src='SVM.png', height = '800px')})
    
    output$CARTExplore <- renderUI({img(src='CART.png', height = '800px')})
    
    output$RFExplore <- renderUI({img(src='RF.png', height = '800px')})
    
    # ML models -----------------------------------------------------------
    
    # Predicting admission ------------------------------------------------

    test <- reactive({
        # this is how you fetch the input variables from ui component
        admit <- as.factor("")
        gre <- as.integer(input$gre)
        gpa <- as.numeric(input$gpa)
        rank <- as.integer(input$rank)

        test <- cbind(admit,gre,gpa,rank)
        test <- as.data.frame(test)
    })
    
    pred <- eventReactive(input$Run_model, {
        switch(input$selection,
               "knn" = predict(fit.knn, newdata = test()),
               "svm" = predict(fit.svm, newdata = test()),
               "cart" = predict(fit.cart, newdata = test()),
               "rf" = predict(fit.rf, newdata = test())
        )
    })
    
    pp <- eventReactive(input$plot_point, { 
        gre <- as.integer(input$gre)
        gpa <- as.numeric(input$gpa)
        rank <- as.integer(input$rank)
        
        switch(rank,
               "1" = (prank1 + ggplot2::annotate("point",x=gpa ,y=gre, color = "yellow", size = 4) +
                          ggplot2::annotate("text",x=gpa ,y=gre+30, label = "your scores", color = "black", size = 4)),
               "2" = (prank2 + ggplot2::annotate("point",x=gpa ,y=gre, color = "yellow", size = 4) +
                          ggplot2::annotate("text",x=gpa ,y=gre+30, label = "your scores", color = "black", size = 4)),
               "3" = (prank3 + ggplot2::annotate("point",x=gpa ,y=gre, color = "yellow", size = 4) +
                          ggplot2::annotate("text",x=gpa ,y=gre+30, label = "your scores", color = "black", size = 4)),
               "4" = (prank4 + ggplot2::annotate("point",x=gpa ,y=gre, color = "yellow", size = 4) +
                          ggplot2::annotate("text",x=gpa ,y=gre+30, label = "your scores", color = "black", size = 4))
        )
        
    })
    
    answer <- reactive( ifelse( pred()=="No", "You are not admitted.", ifelse(pred()=="Yes", "You are admitted!")) )
    
    output$select_algo <- renderText({paste("You have selected: ", input$selection)})
    output$result <- renderText(answer())
    output$youAreHere <- renderPlot({ pp() }) 
    
    # Predicting admission ------------------------------------------------

    
}

# Run the application 
shinyApp(ui = ui, server = server)
