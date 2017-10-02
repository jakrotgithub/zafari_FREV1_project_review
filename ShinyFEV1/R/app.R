library(shiny)
library(ggplot2)
library(plotly)
library(shinyBS)
#devtools::install_github("shiny", "rstudio")


fev1_projection <- function(fev1_0, int_effect, tio="No"){
  
  
  x<-c(0:11)
  
  beta_0<-2.7594
  beta_t<--0.04314
  beta_t2<--0.00093
  v_0<-0.3988
  cov1<--0.00048
  v_t<-0.000941
  v_e<-0.01724
  
  tioBefore <- 0.015
  tioAfter <- 0.022
  
  
  fev1_avg <- c()
  vari <- c()
  obs <- fev1_0
  
  for (i in 1:11){
    
    t1 <- i
    beta_t_x <- 0;
    beta_x_p <- 0;
    beta_x <- int_effect
    
    
    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )
    
    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
    
    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])
    
    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }
  
  
  fev1_avg<-c(fev1_0, fev1_avg)
  
  vari<-c(0,vari)
  
  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)
  
  df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
  
  cv1<-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa1<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv1)*100,0))
  
  n_mean1<-(fev1_avg[12]-fev1_0)/11*1000
  n_sd1<-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000
  bb1<-data.frame(round(pnorm(-40, n_mean1, n_sd1)*100,0))
  
  df_aa1 <- list("df"=df, "aa1"=aa1, "bb1"=bb1, "options"=1)
  print(df_aa1)
  return(df_aa1)
  
  
}

fev1_projection2 <- function(fev1_0, int_effect, sex, smoking, age, weight, height, oco, tio="No"){
  
  x<-c(0:11)
  
  if (sex=="male"){
    gender<-1
  } else if (sex=="female"){
    gender<-0
  }
  
  if (smoking=="Smoker"){
    smo<-1
    int<-0
  } else if (smoking=="Sustained quitter"){
    smo<-0
    int<-0
  }
  
  tioBefore <- 0.015
  tioAfter <- 0.022
  
  beta_0<-1.4212
  beta_t<--0.1779
  beta_t2<--0.00044
  v_0<-0.09728
  cov1<-0.000597
  v_t<-0.000749
  v_e<-0.01703
  
  fev1_avg <- c()
  vari <- c()
  
  obs<-fev1_0
  
  
  for (i in 1:11) {
    
    t1 <- i
    
    beta_x <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*smo + -0.04131*int + 0.002613*oco + -0.00820*age*height*height +
      0.02735*(1-smo) + int_effect;
    
    beta_t_x <- 0.002313*age*t1 + -0.00886*gender*t1 + 0.000149*weight*t1 +
      0.07413*height*t1 + 0.01139*height*height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*oco*t1 +
      -0.00092*age*height*height*t1;
    
    
    beta_x_p <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*(1) + -0.04131*(0) + 0.002613*oco + -0.00820*age*height*height;
    
    beta_t_x_p <- 0.002313*age*(-1) + -0.00886*gender*(-1) + 0.000149*weight*(-1) +
      0.07413*height*(-1) + 0.01139*height*height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*oco*(-1) +
      -0.00092*age*height*height*(-1);
    
    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )
    
    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
    
    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])
    
    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }
  
  
  fev1_avg<-c(fev1_0, fev1_avg)
  vari<-c(0,vari)
  
  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)
  
  df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
  
  cv2<-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa2<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv2)*100,0))
  
  n_mean2<-(fev1_avg[12]-fev1_0)/11*1000
  n_sd2<-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000
  
  bb2<-data.frame(round(pnorm(-40, n_mean2, n_sd2)*100,0))
  
  df_aa2 <- list("df"=df, "aa1"=aa2, "bb1"=bb2, "options"=2)
  print(df_aa2)
  return(df_aa2)
  
}

fev1_projection3 <- function(fev1_0, int_effect, sex, smoking, age, weight, height, tio="No"){
  
  print("kinda workings")
  x<-c(0:11)
  
  if (sex=="male"){
    gender<-1
  } else if (sex=="female"){
    gender<-0
  }
  
  if (smoking=="Smoker"){
    smo<-1
    int<-0
  } else if (smoking=="Sustained quitter"){
    smo<-0
    int<-0
  }
  
  tioBefore <- 0.015
  tioAfter <- 0.022
  
  beta_0<-1.4258
  beta_t<--0.1795
  beta_t2<--0.00044
  v_0<-0.1008
  cov1<-0.000873
  v_t<-0.000769
  v_e<-0.01703
  
  fev1_avg <- c()
  vari <- c()
  
  obs<-fev1_0
  
  for (i in 1:11) {
    
    t1 <- i
    
    beta_x <- -0.00482*age + 0.4828*gender + -0.00041*weight + -1.8759*height + 1.9527*height*height +
      -0.07634*smo + -0.04159*int + -0.00837*age*height*height +
      0.02830*(1-smo) + int_effect;
    
    beta_t_x <- 0.002358*age*t1 + -0.00739*gender*t1 + 0.000127*weight*t1 +
      0.06680*height*t1 + 0.01565*height*height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 +
      -0.00094*age*height*height*t1;
    
    
    beta_x_p <- -0.00482*age + 0.4828*gender + -0.00041*weight + -1.8759*height + 1.9527*height*height +
      -0.07634*(1) + -0.04159*(0) + -0.00837*age*height*height;
    
    beta_t_x_p <- 0.002358*age*(-1) + -0.00739*gender*(-1) + 0.000127*weight*(-1) +
      0.06680*height*(-1) + 0.01565*height*height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) +
      -0.00094*age*height*height*(-1);
    
    
    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )
    
    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
    
    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])
    
    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }
  
  
  fev1_avg<-c(fev1_0, fev1_avg)
  vari<-c(0,vari)
  
  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)
  
  df <-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
  
  cv3 <-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa3 <-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv3)*100,0))
  
  n_mean3 <-(fev1_avg[12]-fev1_0)/11*1000
  n_sd3 <-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000
  bb3 <-data.frame(round(pnorm(-40, n_mean3, n_sd3)*100,0))
  
  df_aa3 <- list("df"=df, "aa1"=aa3, "bb1"=bb3, "options"=3)
  print(df_aa3)
  return(df_aa3)
  
}

fev1_projection4 <- function(fev1_0, fev1_prev, int_effect, sex, smoking, age, weight, height, oco, tio="No"){
  
  x<-c(-1:11)
  print(x)
  print("Test")
  
  if (sex=="male"){
    gender<-1
  } else if (sex=="female"){
    gender<-0
  }
  
  if (smoking=="Smoker"){
    smo<-1
    int<-0
  } else if (smoking=="Sustained quitter"){
    smo<-0
    int<-0
  }
  
  tioBefore <- 0.015
  tioAfter <- 0.022
  
  beta_0<-1.4212
  beta_t<--0.1779
  beta_t2<--0.00044
  v_0<-0.09728
  cov1<-0.000597
  v_t<-0.000749
  v_e<-0.01703
  
  fev1_avg <- c()
  vari <- c()
  
  obs<-c(fev1_prev,fev1_0)
  
  for (i in 1:11)
  {
    t1 <- i
    
    beta_x <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*smo + -0.04131*int + 0.002613*oco + -0.00820*age*height*height +
      0.02735*(1-smo) + int_effect;
    
    beta_t_x <- 0.002313*age*t1 + -0.00886*gender*t1 + 0.000149*weight*t1 +
      0.07413*height*t1 + 0.01139*height*height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*oco*t1 +
      -0.00092*age*height*height*t1;
    
    
    beta_x_p <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*(1) + -0.04131*(0) + 0.002613*oco + -0.00820*age*height*height;
    
    beta_t_x_p <- 0.002313*age*(-1) + -0.00886*gender*(-1) + 0.000149*weight*(-1) +
      0.07413*height*(-1) + 0.01139*height*height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*oco*(-1) +
      -0.00092*age*height*height*(-1);
    
    
    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )
    
    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
    
    unconditional_mu<-c(unconditional_mu, mu_p=beta_0 + beta_x_p + beta_t_x_p + beta_t*(-1) + beta_t2*(-1)*(-1))
    v_t_p<-v_0 + v_t + 2*(-1)*cov1 + v_e
    cov_0_p<-v_0 + (-1)*cov1
    cov_f_p<-v_0 + t1*cov1 + (-1)*cov1 + (-1)*t1*v_t
    cov_mat<-rbind(cbind(cov_mat,c(cov_f_p,cov_0_p)),c(cov_f_p,cov_0_p,v_t_p))
    
    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])
    
    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }
  
  
  fev1_avg<-c(fev1_prev, fev1_0, fev1_avg)
  vari<-c(0,0,vari)
  
  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)
  
  df<-data.frame(x, y=fev1_avg, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "FEV1_lower", "FEV1_upper")
  
  cv4<-sqrt(vari[3:13])/(fev1_avg[3:13]-fev1_prev)
  aa4<-rbind(fev1_avg[3:13], fev1_up[3:13], fev1_low[3:13], round(abs(cv4)*100,0))
  
  n_mean4<-(fev1_avg[13]-fev1_prev)/12*1000
  n_sd4<-((fev1_avg[13]-fev1_prev)/12-(fev1_low[13]-fev1_prev)/12)/1.96*1000
  bb4<-data.frame(round(pnorm(-40, n_mean4, n_sd4)*100,0))
  
  df_aa4 <- list("df"=df, "aa1"=aa4, "bb1"=bb4, "options"=4)
  print(df_aa4)
  return(df_aa4)
}




options(shiny.error = function() {
  stop("")
}) # removes red error message from Shiny webpage

# Define UI for dataset viewer application
ui <- fluidPage(
  tags$head(tags$style("#prob_decliner{color: black;
                        font-size: 16px;
           }"
                        )),
  titlePanel("Individualized Prediction of FEV1"),

  sidebarLayout(



    sidebarPanel(

    # h4("Developed by Zafar Zafari"),
    # h5("This web application is based on the paper entitled 'Individualised prediction of lung function decline in COPD'.
    #    Authors: Zafar Zafari, Don D. Sin, Dirkje S. Postma, Claes-Goran Lofdahl, Judith Vonk, Stirling Bryan, Rahman Khakban,
    #    S.F. Paul Man, Donald Tashkin, Robert A. Wise, John E. Connett, Bruce McManus, MD, Raymond Ng, Zsuszanna Hollander,
    #    Mohsen Sadatsafavi. Please cite this paper when using the results of this web application."),
    #
    #
    # br(),
    # br(),
    #
    # h4("About this Web Application"),
    #
    # #br() and hr() produce extra horizontal space, and hr()>br()
    #
    # h5("This web application is built for the individualized prediction of lung function decline based on 3 models:
    #    (1) Prediction of future lung function decline based on baseline FEV1,
    #    (2) Prediction of future lung function decline based on baseline FEV1 and patients' clinical characteristics,
    #    and (3) Prediction of future lung function decline based on baseline FEV1, patients' clinical characteristics,
    #    and 1-year prior history of FEV1. Interested model as well as other patient's characteristics can be selected
    #    below for seeing the predicted rate of lung function delcine."),
    #
    #
    # br(),
    # br(),
    # br(),
    # br(),
    #

    selectInput('model', 'Please select your model', c("Basic model with only baseline FEV1", "Complete model with baseline FEV1 and patient's characteristics",
                                                       "Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)",
                                                       "Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")),

    uiOutput('inputParam'),

    br(),
    br()
    #submitButton("xx")
    ),


  # Show a summary of the dataset and an HTML table with the
  # requested number of observations. Note the use of the h4
  # function to provide an additional header above each output
  # section.
  mainPanel(

          tabsetPanel(
                  type = "tabs",
                  tabPanel("FEV1 Projection", plotlyOutput("figure"), br(), br(), textOutput("prob_decliner"),
                           br(), tableOutput("cv"), br(),
                          h4("This table quantifies heterogeneity. Please note that Coefficient of Variation (CV)
                              is a measure of heterogeneity calculated by the ratio of standard deviation to the mean
                              FEV1 decline (i.e., it represents noise to signal ratio). In this table CV is shown
                              at different years. For instance, CV for year 2 represents the amount of heterogeneity around
                              mean FEV1 decline over 2 years.")),
                  tabPanel("GOLD Grade", br(), br(), plotlyOutput("severity"), tableOutput("sevTab")),
                  tabPanel("About",
                           h4("About this Web Application"),
                            h5("This web application is built for the individualized prediction of lung function decline
                              based on 3 models:"),
                            h5("(1) Prediction of future lung function decline based on baseline FEV1,"),
                            h5("(2) Prediction of future lung function decline based on baseline FEV1 and patients'
                               clinical characteristics, &"),
                            h5("(3) Prediction of future lung function decline based on baseline FEV1, patients' clinical
                                characteristics, and 1-year prior history of FEV1. Interested model as well as other
                                patient's characteristics can be selected below for seeing the predicted rate of lung
                                function decline."),
                          h4("Developed by Zafar Zafari"),
                          h5("This web application is based on the paper entitled 'Individualised prediction of lung function
                              decline in COPD'.", br(), "Authors: Zafar Zafari, Don D. Sin, Dirkje S. Postma, Claes-Goran Lofdahl,
                              Judith Vonk, Stirling Bryan, Rahman Khakban, S.F. Paul Man, Donald Tashkin, Robert A. Wise,
                              John E. Connett, Bruce McManus, MD, Raymond Ng, Zsuszanna Hollander, Mohsen Sadatsafavi.
                              Please cite this paper when using the results of this web application."))
            )
  )

))



# Define server logic required to draw a histogram
server <- (function(input, output, session) {

  # Output Function Constants-------------------------------------------------------------------------------------------------

  buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
  interventionTitle <- paste0('If the patient is going to use a new intervention,\n',
                              'please indicate the effect of the new intervention\n',
                              'relative to his/her current therapy on initial\n',
                              'improvement in lung function (L).\n',
                              'If you only want to model the natural course of\n',
                              'disease progression irrespective of specific intervention,\n',
                              'please select 0 in here.')
  modelOptions <- c("Basic model with only baseline FEV1",
                    "Complete model with baseline FEV1 and patient's characteristics",
                    "Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)",
                    "Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
  years <- c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8',
            'Year 9', 'Year 10', 'Year 11')

  ######## Ouput Figure (FEV1 Projection Plot) -----------------------------------

  coverageInterval <- "95% coverage interval"
  xlab="Time (years)"
  ylab="FEV1 (L)"
  errorLineColor <- "darkcyan"

  # Output Functions-----------------------------------------------------------------------------------------------------------

	output$inputParam<-renderUI({

		if (input$model==modelOptions[1]) {

		  list(numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)),

		       selectInput('tio', 'Is the patient being treated with tiotropium?', c('No', 'Yes')))

		} else if (input$model==modelOptions[2]){

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),
		       selectInput('sex', 'Sex', c('male', 'female')),

		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

		       selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

		       numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       numericInput('oco', "O'Connor", -12.70, min=-300.00, max=2.00),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)),
		       selectInput('tio', 'Is the patient being treated with tiotropium?', c('No', 'Yes')))

		} else if (input$model==modelOptions[3]) {

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),

		       selectInput('sex', 'Sex', c('male', 'female')),

		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

		       selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

		       numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)),
		       selectInput('tio', 'Is the patient being treated with tiotropium?', c('No', 'Yes')))


		} else if (input$model==modelOptions[4]) {

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),

		       selectInput('sex', 'Sex', c('male', 'female')),

		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

		       selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

		       numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       numericInput('oco', "O'Connor", -12.70, min=-300.00, max=2.00),

		       numericInput('fev1_prev', 'FEV1 at previous year (L)', 2.8, min=1.25, max=3.55),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)),
		       selectInput('tio', 'Is the patient being treated with tiotropium?', c('No', 'Yes')))

		}

	  })

  data <- reactive({

    if(!is.null(input$fev1_0) & input$model==modelOptions[1]) {

        fev1_projection(input$fev1_0, input$int_effect, input$tio)


    } else if(!is.null(input$age) & input$model==modelOptions[2]){

        fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                             input$height, input$oco, input$tio)


    } else if(!is.null(input$age) & input$model==modelOptions[3]) {
        fev1_projection3(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                             input$height, input$tio)

    } else if(!is.null(input$fev1_prev) & input$model==modelOptions[4]){

        fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                               input$height, input$oco, input$tio)

    }

  })

	output$figure<-renderPlotly({

      df <- data()$df
      print(class(df))

			p <- ggplotly(ggplot(df, aes(Time, FEV1)) + geom_line(aes(y = FEV1), color="black", linetype=1) +
			                geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1) +
			                geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
			                geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
			                annotate("text", 1, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
			                annotate("text", 1.15, 3.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
			                labs(x=xlab, y=ylab) +
			                theme_bw()) %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)
			print(p)

			p$x$data[[1]]$text <- paste0("Time (years): ", df$Time, "<br />", "FEV1 (L): ", round(df$FEV1,3),
			                             "<br />FEv1 lower (L): ", round(df$FEV1_lower,3), "<br />FEV1 upper (L): ",
			                             round(df$FEV1_upper),3)

			p$x$data[[3]]$hoverinfo="none"
			p$x$data[[4]]$hoverinfo="none"
			p


	})

	output$cv<-renderTable({

      aa1 <- data()$aa1
			rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
			                 "Coefficient of Variation (CV) (%)")
			colnames(aa1)<- years

			return(aa1)
	},

	  include.rownames=T,
	  caption="FEV1 Heterogeneity",
	  caption.placement = getOption("xtable.caption.placement", "top"))

	output$prob_decliner<-renderText({

      bb1 <- data()$bb1
			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years
			  (declines more than 40 ml/year): '
      bb1 <- paste0(prob_text, as.numeric(bb1), "%")
			return(bb1)
	})

	output$severity<-renderPlotly({

		  if(data()$options==1){
		    gender<-1
		    age_x<-55
		    height_x<-1.7
		  } else {
		    if (input$sex=="male"){
		      gender<-1
		    } else if (input$sex=="female"){
		      gender<-0
		    }
		    age_x <- input$age
		    height_x <- input$height
		  }

      if(data()$options==4){
        x<-c(-1:11)
        rnames <-	c('Previous','Baseline', years)
      } else {
			  x<-c(0:11)
			  rnames <- c('Baseline', years)
      }

	  print("testing")

			df <- data()$df
			fev1_avg <- df$FEV1
			fev1_low <- df$FEV1_lower
			fev1_up <- df$FEV1_upper



			fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
						(0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)
			print("testing2")

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
      p_severe<-100-p_mild-p_moderate


      u1<-matrix(0,nrow=length(x),ncol=4)
      u1[,1]<-x
      u1[,2]<-p_mild
      u1[,3]<-p_moderate
      u1[,4]<-p_severe
      colnames(u1)<-c("year","mild", "moderate", "severe")
      rownames(u1)<- rnames
      print(u1)
      data <- as.data.frame(u1)

			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild', marker = list(color = toRGB("#009E73"))) %>%
			  add_trace(y = ~moderate, name='Moderate', marker = list(color = toRGB("#E69F00"))) %>%
			  add_trace(y = ~severe, name='Severe', marker = list(color = toRGB("#D55E00"))) %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack',
			         xaxis=list(title='Year', type='category', categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade',
			         hovermode='x') %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)

    print(p)

	})


	output$sevTab<-renderTable({


	  if(data()$options==1){
	    gender<-1
	    age_x<-55
	    height_x<-1.7
	  } else {
	    if (input$sex=="male"){
	      gender<-1
	    } else if (input$sex=="female"){
	      gender<-0
	    }
	    age_x <- input$age
	    height_x <- input$height
	  }

	  if(data()$options==4){
	    x<-c(-1:11)
	    cnames <-	c('Previous','Baseline', years)
	  } else {
	    x<-c(0:11)
	    cnames <- c('Baseline', years)
	  }

	  df <- data()$df
	  fev1_avg <- df$FEV1
	  fev1_low <- df$FEV1_lower
	  fev1_up <- df$FEV1_upper

	  fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
	    (0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)

	  p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
	  p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
	  p_severe<-100-p_mild-p_moderate

	  u1<-matrix(0,nrow=3, ncol=length(x))
	  u1[1,]<-p_mild
	  u1[2,]<-p_moderate
	  u1[3,]<-p_severe

	  colnames(u1)<- cnames
	  rownames(u1)<-c("Probability of being mild", "Probability of being moderate", "Probability of being severe")
	  return(u1)

	},
	include.rownames=T)
})

shinyApp(ui = ui, server = server)


