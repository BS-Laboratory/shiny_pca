library(shiny)
library(plotly)
library(tidyr)

ui<-navbarPage(span( "Online PCA by BSL", style = "color: #0176a5 "),
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel(
                            fileInput("file1", "Upload Your CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            tags$hr(),
                            h5(strong("Select the Upload Parameters Below")),
                            checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                            checkboxInput(inputId = "stringAsFactors", "stringAsFactors", TRUE),
                            radioButtons("disp", "Display Option",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head"),
                            helpText("Please upload file before navigate to the other tabs")
                        ),
                        mainPanel(tableOutput("contents"))
                        ) ),
               tabPanel("Visualize Eigenvalues ",
                        sidebarLayout(sidebarPanel(
                            uiOutput("vars_select"),
                            helpText("Results will not display if none of the variables are selected")),
                            mainPanel(h5(strong("Eigenvalue Table")),
                                      tableOutput("eigtab"),
                                      helpText("Rule of thumb: choose principle with eigenvalue > 1"),
                                      h5(strong("Scree Plot")),
                                      plotOutput("screeplot"),
                                      h5(strong("Item Loading Table")),
                                      tableOutput("loading"),
                                      helpText("Common factor loading cut-off = |0.7|")))),
               tabPanel("PCA Plots",
                        fluidRow(
                            column(6,
                                   h5(strong("Individual PCA Plot")),
                                   helpText("Color by the quality of representation"),
                                   plotOutput("ind_plot")),
                            column(6,
                                   h5(strong("Variable PCA Plot")),
                                   helpText("Color by contributions to the components"),
                                   plotOutput("var_plot"))
                        )),
               tabPanel("Autoplotly",
                        sidebarLayout(sidebarPanel(
                            uiOutput("type_select"),
                            helpText("Please use factor or character variable"),
                            checkboxInput(inputId = 'frame', label = 'Draws Frame for Each Group',
                                          value = TRUE)),
                            mainPanel(h5(strong("PCA Autoplotly")),
                                      plotlyOutput("auto_plot"))))
)
server<-function(input,output) { 
    
    dataInput <- reactive({
        read.csv(input$file1$datapath,
                 header = input$header)
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header)
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    output$type_select<-renderUI({
        selectInput("typeselect","Stratify PCA by",choices = as.list(names(dataInput())),multiple = FALSE)
    })
    output$vars_select<-renderUI({
        checkboxGroupInput("varsselect","Select the Variables for Principal Component Analysis", choices =as.list(names(dataInput())))
    })
    output$screeplot<-renderPlot({
        library(factoextra)
        library(psych)
        pc <- dataInput()[input$varsselect]
        data.pca <- prcomp(pc, scale = TRUE)
        p <- fviz_eig(data.pca, addlabels = TRUE)
        print(p)
        
    })
    
    output$eigtab <- renderTable({
        pc <- dataInput()[input$varsselect]
        data.pca <- prcomp(pc, scale = TRUE)
        t1 <- get_eigenvalue(data.pca)
        print(t1)
    }, rownames = TRUE)
    
    output$loading <- renderTable({
        pc <- dataInput()[input$varsselect]
        data.pca <- prcomp(pc, scale = TRUE)
        res.var <- get_pca_var(data.pca)
        t2 <- (res.var$coord)
        print(t2)
    }, rownames = TRUE)
    
    output$ind_plot <- renderPlot({
        pc <- dataInput()[input$varsselect]
        data.pca <- prcomp(pc, scale = TRUE)
        p2 <- fviz_pca_ind(data.pca,
                           col.ind = "cos2", # Color by the quality of representation
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE     # Avoid text overlapping
        )
        print(p2)
    })
    
    output$var_plot <- renderPlot({
        pc <- dataInput()[input$varsselect]
        data.pca <- prcomp(pc, scale = TRUE)
        p2 <- fviz_pca_var(data.pca,
                           col.var = "contrib", # Color by contributions to the components
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE     # Avoid text overlapping
        )
        print(p2)
    })
    
    output$auto_plot <- renderPlotly({
        library(autoplotly)
        
        
        
        pc <- dataInput()[input$varsselect]
        data.pca <- prcomp(pc, scale = TRUE)
        p3 <- autoplotly(data.pca, data = dataInput(), colour = input$typeselect,
                         shape=input$typeselect, label = TRUE, frame = input$frame, height = 600) 
        print(p3)
    })
    
}
shinyApp(ui=ui,server=server)