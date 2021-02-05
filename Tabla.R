library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)
library(mice)
library(missForest)
library(VIM)
library(scatterplot3d)
library(rgl)
library(car)
library(olsrr)
library(latticeExtra)
library(lmtest)
library(MASS)
#library(naniar)
# library(ggplot2)
library(plotly)
#install.packages("car")
missing_percent<-function(x) {
  sum(is.na(x))/length(x)*100
}
cols<-function(n) {
  colorRampPalette(c("#FFC0CB", "#CC0000"))(20)                                 # 20 distinct colors
}
tags$head(tags$script(src = "message-handler.js"))
ui <- dashboardPage(
  dashboardHeader(title = "PREDICA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload File", tabName = "Up_fl", icon = icon("arrow-circle-up")),
      menuItem("Missing Values",tabName = "M_val", icon = icon("times-circle")),
      menuItem("Plots", tabName = "plots", icon = icon("times-circle")),
      menuItem("Multiple Linear Regression", tabName = "M_reg", icon = icon("line-chart"),
               menuItem("Fitting of model", tabName = "model_fit",icon = icon("angle-double-right")),
               menuItem("Assumptions", tabName = "assumpt",icon = icon("angle-double-right"),
                        menuSubItem('Linearity',tabName = 'linear',icon = icon('angle-double-right')),
                        menuSubItem('Multivariate Normality',tabName = 'multivar',icon = icon('angle-double-right')),
                        menuSubItem('Multicollinearity',tabName = 'multicoll',icon = icon('angle-double-right')),
                        menuSubItem('Homoscedasticity',tabName = 'homosced',icon = icon('angle-double-right')),
                        menuSubItem('Autocorrelation',tabName = 'autocorr',icon = icon('angle-double-right'))),
               menuItem("ANOVA", tabName = "anova",icon = icon("angle-double-right"))),
      menuItem("Prediction", tabName = "predict",icon = icon("angle-double-right"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Up_fl",
              h2("Upload File"),
               tabsetPanel(id="tb1",
                 tabPanel("Upload File",
                          tags$hr(),
                          radioButtons("type","Choose the type of file:",choices=c("CSV","EXCEL")),
                          checkboxInput("header", "Header", TRUE),
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
                          radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
                          tags$hr(), 
                          radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),selected = "head"),
                          tags$hr(),
                      fileInput("file1", "Choose CSV File",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))
                  ),
                tabPanel("Data",
                      tags$hr(),
                      # fluidPage(
                      # fluidRow(
                      #   box(title = "Data",
                      #       tableOutput("contents"),status = "primary",solidHeader = TRUE,collapsible = TRUE,width = unlist(textOutput("len_data"))
                      #      )),
                      # fluidRow(
                      #   box(title="Summary",status = "warning",verbatimTextOutput("sum"),solidHeader = TRUE,collapsible = TRUE,width = 7)
                      # 
                      # ))
                      tabBox(
                        id="data",width=14,
                          tabPanel("Data",
                                   fluidRow(
                                     box(title = "Data",
                                     tableOutput("contents"),status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 12)
                                    )),
                          tabPanel("Summary",
                                   fluidRow(
                                     box(title="Summary",status = "warning",verbatimTextOutput("sum"),solidHeader = TRUE,collapsible = TRUE,width = 7)
                                   )
                          )
                )
                )
              )
      ),
      
      tabItem(tabName = "M_val",
              h2("Missing Values"),
              tags$hr(),
              fluidRow(
                box(actionButton("disp_plot","Plot Missing values")),
                box(actionButton("miss_val","Perform Imputation"))
                ),
              tabBox(id="hideplot",
                tabPanel("Plot",
                  fluidRow(
                    box(plotOutput("miss_plot"),width = 12,status = "primary",collapsed = TRUE,collapsible = TRUE)
                  )
                  ),
                tabPanel("Missing values",
                  fluidRow(
                    box(verbatimTextOutput("imput"),width = 12,status = "warning",collapsed = TRUE,collapsible = TRUE)
                  )
                )
              )
              ),
      tabItem(tabName = "plots",
              tabBox(width = 10,
                tabPanel("Select variables",
                         tags$hr(),
                         selectInput("ind_var1_lin","select the response variable",""),
                         tags$hr(),
                         selectInput("d_var1_lin","select the predictor variable",""),
                         tags$hr(),
                         selectInput("d_var2_lin","select the predictor variable",""),
                         tags$hr()),
                tabPanel("scatter_plot",
                         # selectInput("ind_var1_lin","select the response variable",""),
                         # tags$hr(),
                         # selectInput("d_var1_lin","select the predictor variable",""),
                         # tags$hr(),
                         # selectInput("d_var2_lin","select the predictor variable",""),
                         # tags$hr(),
                         actionButton("sc","View Scatter plot"),
                         plotOutput("scat")
                         ),
                tabPanel("box_plot",
                         # selectInput("ind_var1_box","select the response variable",""),
                         # tags$hr(),
                         # selectInput("d_var1_box","select the response variable",""),
                         # tags$hr(),
                         actionButton("bo","View Boxplot"),
                         plotOutput("box")
                         )
                # tabPanel("heat_map",
                #          actionButton("heat","View Heatmap"),
                #          plotOutput("heatmap")
                #          )
              )
              ),
      tabItem(tabName = "model_fit",
              tabBox(width = 10,height = 12,
                tabPanel("Select variables",
                         selectInput("ind_var1","select the response variable",""),
                         tags$hr()
                ),
                tabPanel("Fiiting First model",
                         
                       verbatimTextOutput("model"),
                       fluidRow(
                         infoBoxOutput("model_rsq"),
                         infoBoxOutput("model_adj_rsq")
                       )),
                tabPanel("Significant variables",
                       tableOutput("signif")),
                tabPanel("Fitting Latest model",
                        
                       verbatimTextOutput("model_sign2"),
                       fluidRow(
                         infoBoxOutput("model2_rsq"),
                         infoBoxOutput("model2_adj_rsq")
                       ))
              )
      ),
      tabItem(tabName = "assumpt",
              h2("Assumptions")
      ),
      tabItem(tabName = 'linear',
              h2("Linearity"),
              tags$hr(),
              actionButton("linear_action","View Scatter plot"),
              tags$hr(),
              fluidRow(
              box(plotOutput("linear_scatter"),status = "primary",solidHeader = TRUE,collapsible = TRUE,title = "Scatter plot")
              )
      ),
      tabItem(tabName = "multivar",
              h2("Multivariate Normality"),
              tags$hr(),
              box(plotOutput("normal_dist"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "QQplot (First Model)"),
              box(plotOutput("normal_dist1"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "QQplot (Latest Model)")
              ),
      tabItem(tabName = "multicoll",
              h2("Multicollinearity"),
              tags$hr(),
              box(verbatimTextOutput("multicollinear"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "Multicollinearity for First model"),
              box(verbatimTextOutput("multicollinear2"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "Multicollinearity for Latest model")
              ),
      tabItem(tabName = "homosced",
              h2("Homoscedasticity"),
              tags$hr(),
              box(plotOutput("homosced_plot"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "First model"),
              box(plotOutput("homosced_plot1"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "Latest model")),
      tabItem(tabName = "autocorr",
              h2("Autocorrelation"),
              tags$hr(),
              fluidRow(
              box(verbatimTextOutput("autocorrel"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "First model"),
              box(verbatimTextOutput("autocorrel1"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "Latest model")),
              fluidRow(
                tags$hr(),
                infoBoxOutput("dw_pval1",width=6),
                infoBoxOutput("dw_pval2",width = 6)
              )),
      tabItem(tabName = "anova",
              h2("Anova"),
              tags$hr(),
              selectInput("cat_var","select the response variable",""),
              tags$hr(),
              selectInput("dep_var","select the predictor (categorical) variable",""),
              tags$hr(),
              tags$hr(),
              box(verbatimTextOutput("anova_sum"),status = "primary",collapsible = TRUE,solidHeader = TRUE,title = "One way anova"),
              fluidRow(
                tags$hr(),
                infoBoxOutput("anov_fval",width=6),
                infoBoxOutput("anov_pval",width = 6)
              )
      ),
      tabItem(tabName = "predict",
              h2("Upload File to predict"),
              tabsetPanel(id="tb_predict",
                          tabPanel("Upload File",
                                   tags$hr(),
                                   radioButtons("type1","Choose the type of file:",choices=c("CSV","EXCEL")),
                                   checkboxInput("header1", "Header", TRUE),
                                   radioButtons("sep1", "Separator",
                                                choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
                                   radioButtons("quote1", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
                                   tags$hr(), 
                                   radioButtons("disp1", "Display",choices = c(Head = "head",All = "all"),selected = "head"),
                                   tags$hr(),
                                   fileInput("file2", "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"))
                          ),
                          tabPanel("Prediction",
                                   tags$hr(),
                                   tabBox(
                                     id="data",width=14,
                                     tabPanel("Data",
                                              fluidRow(
                                                box(title = "Data",
                                                    verbatimTextOutput("predict_val"),status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 12)
                                              ))
                                   )
                          )
              )
    )
  )
)
)
server <- function(input, output, session) {
  # 
  # observe({
  #   if(!isTruthy(input$file1)){
  #     shinyjs::hide("contents")
  #     shinyjs::hide("sum")
  #   }
  #   else
  #   {
  #     shinyjs::show("contents")
  #     shinyjs::show("sum")
  #   }
  # })
  # 
  df<-reactive(
    {
      if(is.null(input$file1)){
        return(NULL)
      }
      else{
        if(input$type=="CSV"){
          csv_data<-read.csv(input$file1$datapath,sep=input$sep,quote=input$quote)
        }
        else if(input$type=="EXCEL"){
          read_excel(input$file1$datapath)
        }
        else{
          return(NULL)
        }
        
      }
    }
  )  
  
  #=======Display the TableOutput of the file============
  
  output$contents<-renderTable({
    req(input$file1)
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  },striped=TRUE,hover=TRUE,bordered=TRUE,align='c')
  
  df3<-reactive(
    {
      if(is.null(input$file2)){
        return(NULL)
      }
      else{
        if(input$type1=="CSV"){
          csv_data<-read.csv(input$file2$datapath,sep=input$sep1,quote=input$quote1)
        }
        else if(input$type1=="EXCEL"){
          read_excel(input$file2$datapath)
        }
        else{
          return(NULL)
        }
        
      }
    }
  ) 
  
  pr<-reactive({
    dataf3<-df3()[,-1]
    sign2<-rownames(summary(model())$coef)[summary(model())$coef[,"Pr(>|t|)"]<0.05]
    if("(Intercept)" %in% sign2)
    {
      sign2<-sign2[-match("(Intercept)",sign2)]
    }
    sign_var2=data.frame(dataf3[sign2[1]])
    if(length(sign2)>1)
    {
      for(i in 2:length(sign2))
      {
        sign_var2<-data.frame(sign_var2,dataf3[sign2[i]])
      }
    }
    pred<-predict(model2(),newdata=sign_var2)
  })



  output$predict_val<-renderPrint({
  pr()

  })
  
  output$len_data<-renderText({
    length(df())
  })
  
  observe({
    hideTab(inputId = "hideplot",target = "plot")
  })
  
  observe({
    updateSelectInput(session,"ind_var",choices=names(df()))
  })
  
  observe({
    updateSelectInput(session,"ind_var1",choices = names(df()))
  })
  observe({
    updateSelectInput(session,"ind_var1_lin",choices = names(df()))
  })
  observe({
    updateSelectInput(session,"d_var1_lin",choices = names(df()))
  })
  observe({
    updateSelectInput(session,"d_var2_lin",choices = names(df()))
  })
  # observe({
  #   updateSelectInput(session,"ind_var1_box",choices = names(df()))
  # })
  # observe({
  #   updateSelectInput(session,"d_var1_box",choices = names(df()))
  # })
  observe({
    updateSelectInput(session,"ind_var1_linear",choices = names(df()))
  })
  observe({
    updateSelectInput(session,"d_var1_linear",choices = names(df()))
  })
  observe({
    updateSelectInput(session,"cat_var",choices = names(df()))
  })
  observe({
    updateSelectInput(session,"dep_var",choices = names(df()))
  })
  
    output$sum <- renderPrint({
      if(isTruthy(input$file1)){
        ry(input$file1)
      }
    })

    observe({
      if(is.factor(input$ind_var1))
      {
        
      }
    })
    
    aggr_plot <- reactive({
      aggr(df(),col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,labels=names(df()),cex.axis=.7,gap=3,ylab=c("Missing data","Pattern"))
    })
    
    
    impu<-reactive({
      req(input$file1)
      return(apply(df(),2,missing_percent))
    })
    
    observeEvent(input$disp_plot,{
      output$miss_plot <- renderPlot({
        req(input$file1)
        aggr_plot()
        # gg_miss_upset(df())
        # ggplot(data=as.data.frame(df()),aes(x=(factor(variable,levels=variable,ordered=FALSE)),y=missing)) +
        #   geom_bar(stat = "identity",position="dodge") + labs(x="Variables",y="Number of Missing Values") + 
        #   theme(axis.text.x=element_text(angle=45, hjust=1))
      }) 
      output$imput<-renderPrint({
         impu()[impu()>0.000000 & impu()<=40.000000]
      })
    })
      
      nam<-reactive({
        names(impu())[(impu()>0.000000) & (impu()<=40.000000)]
      })
      
      observeEvent(input$miss_val,{
        if(length(nam())>0){
            for(i in 1:ncol(df()))
            {
              if(!is.numeric(unlist(df()[ ,i]))){
                unlist(df()[is.na(df()[ ,i]),i]) <- round(mean(unlist(df()[ ,i]),na.rm=TRUE),2)
                showModal(modalDialog("hai",i))
              }
              else
              {
                
              }
            }
        }
      })
      
      model<- reactive({
      req(input$file1)
      # df1<-df()[complete.cases(df()),]
      dataf<-df()[,-1]
      lm(unlist(dataf[ ,input$ind_var1])~.,data=dataf[,1:length(dataf)-1],na.action = na.omit)
      #return(result)
    })
      
      output$model <- renderPrint({
        summary(model())
        #summary(model())$coef[ ,4] < 0.05 
        # summary(big.mod)$coefficients[ ,4] < 0.05 
         # r<-rownames(summary(model())$coef)[summary(model())$coef[,"Pr(>|t|)"]<0.05]
         # r
        # p[p>0 & p<0.05]
      })
      output$model_rsq<-renderInfoBox({
        infoBox("R Square value",
                value = summary(model())[8],
                fill=TRUE,
                color = "black")
      })
      output$model_adj_rsq<-renderInfoBox({
        infoBox("Adjusted R Square value",
                value = summary(model())[9],
                fill=TRUE,
                color = "blue")})
    
      pval<-reactive({
        sign<-rownames(summary(model())$coef)[summary(model())$coef[,"Pr(>|t|)"]<0.05]
        if("(Intercept)" %in% sign)
        {
          sign<-sign[-match("(Intercept)",sign)]
        }
        sign_var=data.frame(df()[sign[1]])
        if(length(sign)>1)
        {
          for(i in 2:length(sign))
          {
            sign_var<-data.frame(sign_var,df()[sign[i]])
          }
        }
        return(sign_var)
      })

      output$signif<-renderTable({
        req(input$file1)
        return(head(pval()))
      },striped=TRUE,hover=TRUE,bordered=TRUE,align='c')
      
      model2<- reactive({
        req(input$file1)
        # df1<-df()[complete.cases(df()),]
        
        lm(unlist(df()[ ,input$ind_var1])~.,data=pval(),na.action = na.omit)
        #return(result)
      })
      
      output$model_sign2<- renderPrint({
        summary(model2())
        #summary(model())$coef[ ,4] < 0.05 
        # summary(big.mod)$coefficients[ ,4] < 0.05 
        # r<-rownames(summary(model())$coef)[summary(model())$coef[,"Pr(>|t|)"]<0.05]
        # r
        # p[p>0 & p<0.05]
      })
      
      output$model2_rsq<-renderInfoBox({
        infoBox("R Square value",
                value = summary(model2())$r.squared,
                fill=TRUE,
                color = "black")
      })
      output$model2_adj_rsq<-renderInfoBox({
        infoBox("Adjusted R Square value",
                value = summary(model2())$adj.r.squared,
                fill=TRUE,
                color = "blue")
      })
      observeEvent(input$sc,{
          output$scat<-renderPlot({
          s3d<-scatterplot3d(unlist(df()[ ,input$ind_var1_lin]),unlist(df()[ ,input$d_var1_lin]),unlist(df()[ ,input$d_var2_lin]),pch = 16,highlight.3d = TRUE,type = "h",main="3D Scatterplot",xlab = input$ind_var1_lin,ylab = input$d_var1_lin)
          #s3d1<-s3d+labs(x=input$ind_var1_lin,y=input$d_var1_lin)
          return(s3d)
           #with(df(),plot3d(unlist(df()[ ,input$ind_var1_lin]),unlist(df()[ ,input$d_var1_lin]),unlist(df()[ ,input$d_var2_lin]),type = "s"))
          })
      })
      
      
      observeEvent(input$bo,{
        output$box<-renderPlot({
          #boxplot(unlist(df()[ ,input$ind_var1_box]))
          g<-ggplot(df(),aes(x=" ",df()[ ,input$ind_var1_lin]))+geom_boxplot(color="red",outlier.shape = 25)+theme_bw()
          # g<-ggplot(df(),aes(as.factor(unlist(df()[ ,input$ind_var1_box])),unlist(df()[ ,input$ind_var1_box])))+geom_boxplot(fill="navy",color="red",outlier.shape = 25)+theme_dark()
          g1<-g+labs(x=input$ind_var1_lin, y=input$d_var1_lin)
          return(g1)
        })
      })
      
      heat_df<-reactive({
        empty_df <- data.frame(df()[,1])
        for(i in 2:ncol(df()))
        {
          if(is.numeric(unlist(df()[,i])))
          {
            empty_df<-data.frame(empty_df,df()[,i])
          }
        }
        round(cor(empty_df),2)
        
      })
      
      observeEvent(input$heat,{
        output$heatmap<-renderPlot({
          mel_cor<-melt(heat_df())
          ggplot(data = mel_cor, aes(Var2, Var1, fill = value))+
            geom_tile(color = "white")+
            scale_fill_gradient2(
              midpoint = 0, limit = c(-1,1), space = "Lab",
              name="Pearson\nCorrelation") +
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                             size = 12, hjust = 1))+
            coord_fixed()+ geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
        })
      })
      
      observeEvent(input$linear_action,{
        output$linear_scatter<-renderPlot({
          plot(fitted(model2()),resid(model2()))
          #plot(unlist(df()[ ,input$ind_var1_linear]),unlist(df()[ ,input$d_var1_linear]),xlab=input$ind_var1_linear,ylab=input$d_var1_linear)
          #s3d<-scatterplot3d(unlist(df()[ ,input$ind_var1_lin]),unlist(df()[ ,input$d_var1_lin]),unlist(df()[ ,input$d_var2_lin]),pch = 16,highlight.3d = TRUE,type = "h",main="3D Scatterplot",xlab = input$ind_var1_lin,ylab = input$d_var1_lin)
          #s3d1<-s3d+labs(x=input$ind_var1_lin,y=input$d_var1_lin)
          
        })
      })
      
      output$normal_dist<-renderPlot({
        plot(model(),which=2,xlab="Theoretical Quantiles")
        #qqplot(Residuals<-resid(model()),main="QQ plot for Residuals", xlab="Residuals",col="navy blue")
      })
      
      output$normal_dist1<-renderPlot({
        plot(model2(),which=2,xlab="Theoretical Quantiles")
        #qqplot(Residuals<-resid(model()),main="QQ plot for Residuals", xlab="Residuals",col="navy blue")
      })
      
      vif1<-reactive({
        vif(model())
      })
      
      output$multicollinear<-renderPrint({
         vif1()
      })
      
      output$multicollinear2<-renderPrint({
        vif(model2())
      })
      
      output$homosced_plot<-renderPlot({
        plot(fitted(model()),sqrt(MASS::stdres(model())),main="Homogeneity of Variance",xlab="Residuals",ylab="Predicted")
      })
      
      output$homosced_plot1<-renderPlot({
        plot(fitted(model2()),sqrt(MASS::stdres(model())),main="Homogeneity of Variance",xlab="Residuals",ylab="Predicted")
      })
      
      output$autocorrel<-renderPrint({
        dwtest(model())
      })
      
      output$autocorrel1<-renderPrint({
        dwtest(model2())
      })
      
      output$anova_sum<-renderPrint({
        anova(lm(unlist(df()[ ,input$cat_var])~unlist(df()[ ,input$dep_var]),data=df(),na.action = na.omit))
      })
      
      output$dw_pval1<-renderInfoBox({
        infoBox(title="First model",
                value=dwtest(model())[4],
                subtitle = "P value",
                fill=TRUE,
                color="blue"
                )
      })
      output$dw_pval2<-renderInfoBox({
        infoBox(title="Latest model",
                value=dwtest(model2())[4],
                subtitle = "P value",
                fill=TRUE,color = "black")
      })
      output$anov_fval<-renderInfoBox({
        infoBox(title="F value",
                value=anova(lm(unlist(df()[ ,input$cat_var])~unlist(df()[ ,input$dep_var]),data=df(),na.action = na.omit))[1,4],
                fill=TRUE,
                color="black"
        )
      })
      output$anov_pval<-renderInfoBox({
        infoBox(title="P value",
                value=anova(lm(unlist(df()[ ,input$cat_var])~unlist(df()[ ,input$dep_var]),data=df(),na.action = na.omit))[1,5],
                fill=TRUE,color = "blue")
      })
}
shinyApp(ui, server)