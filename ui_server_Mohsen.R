library(shiny)
library(ggplot2)
library(plotly)
#devtools::install_github("shiny", "rstudio")
#library(shiny)

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
                  tabPanel("FEV1 Projection", plotlyOutput("figure"), br(), br(), textOutput("prob_decliner")),
                  tabPanel("GOLD Grade", br(), br(), plotlyOutput("severity"), tableOutput("sevTab")),
                  tabPanel("Heterogeneity", br(), br(), tableOutput("cv"), br(),
                  h4("This table quantifies heterogeneity. Please note that Coefficient of Variation (CV) is a measure of heterogeneity calculated
			 by the ratio of standard deviation to the mean FEV1 decline (i.e., it represents noise to signal ratio). In this table CV is shown
			 at different years. For instance, CV for year 2 represents the amount of heterogeneity around mean FEV1 decline over 2 years.")),
                  tabPanel("About", 
                           h4("About this Web Application"),
                           h5("This web application is built for the individualized prediction of lung function decline based on 3 models:"), 
       h5("(1) Prediction of future lung function decline based on baseline FEV1,"), 
       h5("(2) Prediction of future lung function decline based on baseline FEV1 and patients' clinical characteristics, &"),
       h5("(3) Prediction of future lung function decline based on baseline FEV1, patients' clinical characteristics,
       and 1-year prior history of FEV1. Interested model as well as other patient's characteristics can be selected
       below for seeing the predicted rate of lung function decline."),
       h4("Developed by Zafar Zafari"),
       h5("This web application is based on the paper entitled 'Individualised prediction of lung function decline in COPD'. 
          Authors: Zafar Zafari, Don D. Sin, Dirkje S. Postma, Claes-Goran Lofdahl, Judith Vonk, Stirling Bryan, Rahman Khakban,
          S.F. Paul Man, Donald Tashkin, Robert A. Wise, John E. Connett, Bruce McManus, MD, Raymond Ng, Zsuszanna Hollander, 
          Mohsen Sadatsafavi. Please cite this paper when using the results of this web application."))
       )

          
#     #paste("Figure 1. Prediction based on baseline FEV1"),
#     plotOutput("figure"),
#     br(),
#     br(),
#     br(),
#     br(),
#     tableOutput("prob_decliner"),
#     br(),
#     br(),
#     br(),
#     br(),
#     h4("This table quantifies heterogeneity. Please note that Coefficient of Variation (CV) is a measure of heterogeneity calculated
# 			 by the ratio of standard deviation to the mean FEV1 decline (i.e., it represents noise to signal ratio). In this table CV is shown
# 			 at different years. For instance, CV for year 2 represents the amount of heterogeneity around mean FEV1 decline over 2 years."),
#     br(),
#     tableOutput("cv"),
#     br(),
#     br(),
#     br(),
#     br(),
#     plotOutput("severity"),
#     br(),
#     br(),
#     tableOutput("sevTab")
#  
  )
  
    ))





#devtools::install_github("shiny", "rstudio")
#library(shiny)

# Define server logic required to draw a histogram
server <- (function(input, output) {

  options(shiny.sanitize.errors=F)


	output$inputParam<-renderUI({

		if (input$model=='Basic model with only baseline FEV1')
		{
 			list(numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

 			numericInput('int_effect', 'If the patient is going to use a new intervention, please indicate the effect of the new intervention relative to his/her current therapy on initial improvement in lung function (L). If you only want to model the natural course of disease progression irrespective of specific intervention, please select 0 in here.', 0, min=0, max=0.1))

			

		} else if (input$model=="Complete model with baseline FEV1 and patient's characteristics")
		{

 			list(numericInput('age', 'Please select age', 55, min=40, max=90),

				selectInput('sex', 'Sex', c('male', 'female')),

 				numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

 				numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

				selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

 				numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

 				numericInput('oco', "O'Connor", -12.70, min=-300.00, max=2.00),
	
 				numericInput('int_effect', 'If the patient is going to use a new intervention, please indicate the effect of the new intervention relative to his/her current therapy on initial improvement in lung function (L). If you only want to model the natural course of disease progression irrespective of specific interevntion, please select 0 in here.', 0, min=0, max=0.1))




		} else if (input$model=="Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)")
		{

 			list(numericInput('age', 'Please select age', 55, min=40, max=90),

				selectInput('sex', 'Sex', c('male', 'female')),

 				numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

 				numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

				selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

 				numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

 				numericInput('int_effect', 'If the patient is going to use a new intervention, please indicate the effect of the new intervention relative to his/her current therapy on initial improvement in lung function (L). If you only want to model the natural course of disease progression irrespective of specific interevntion, please select 0 in here.', 0, min=0, max=0.1))




		} else if (input$model=="Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
		{
 			list(numericInput('age', 'Please select age', 55, min=40, max=90),

				selectInput('sex', 'Sex', c('male', 'female')),

 				numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

 				numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

				selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

 				numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

 				numericInput('oco', "O'Connor", -12.70, min=-300.00, max=2.00),
	
 				numericInput('fev1_prev', 'FEV1 at previous year (L)', 2.8, min=1.25, max=3.55),

 				numericInput('int_effect', 'If the patient is going to use a new intervention, please indicate the effect of the new intervention relative to his/her current therapy on initial improvement in lung function (L). If you only want to model the natural course of disease progression irrespective of specific interevntion, please select 0 in here.', 0, min=0, max=0.1))

		}

	})





























	output$figure<-renderPlotly({

		if (!is.null(input$fev1_0) & input$model=="Basic model with only baseline FEV1")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			beta_0<-2.7594
			beta_t<--0.04314
			beta_t2<--0.00093
			v_0<-0.3988
			cov1<--0.00048
			v_t<-0.000941
			v_e<-0.01724

			fev1_avg <- c()
			vari <- c()

			obs<-input$fev1_0

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- input$int_effect;

			  beta_t_x <- 0;

			  beta_x_p <- 0;


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
			}


			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)

			df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
			names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
      print(names(df))

			# p<-ggplot(df, aes(Time, FEV1))
			# p <- p + geom_line(aes(y = FEV1), color="black", linetype=1) +
			#   geom_line(aes(y = FEV1_lower), color="red", linetype=2) +
			#   geom_line(aes(y = FEV1_upper), color="red", linetype=2) +
			#   annotate("text", 1, 3.4, label="Mean FEV1 decline", colour="black", size=3, hjust=0) +
			#   annotate("text", 1, 3.3, label="99.5% coverage interval", colour="red", size=3, hjust=0) +
			#   labs(x="Time (years)", y="FEV1 (L)") +
			#   theme_bw()

			#p2 <- plotly::ggplotly(p)
			#print(p2)



		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics")

		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)


			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703

			fev1_avg <- c()
			vari <- c()

			obs<-input$fev1_0

			for (i in 1:11)
			{
			    t1 <- i

			    beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			      -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			      0.02735*(1-smo) + input$int_effect;

			    beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			      0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 +
			      -0.00092*input$age*input$height*input$height*t1;


			    beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			      -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;

			    beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			      0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) +
			      -0.00092*input$age*input$height*input$height*(-1);

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
			}


			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)

			df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
			names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
			
			p2 <- plotly_empty()
# 
# 			p<-ggplot(df, aes(Time, FEV1))
# 			p <- p + geom_line(aes(y = FEV1), color="black", linetype=1) +
#    			  geom_line(aes(y = FEV1_lower), color="red", linetype=2) +
#     			geom_line(aes(y = FEV1_upper), color="red", linetype=2) +
#    			  annotate("text", 1, 3.4, label="Mean FEV1 decline", colour="black", size=3, hjust=0) +
#     			annotate("text", 1, 3.3, label="99.5% coverage interval", colour="red", size=3, hjust=0) +
#     			labs(x="Time (years)", y="FEV1 (L)") +
#    			theme_bw()
# 
# 			p2 <- ggplotly(p)
# 			print(p2)
			



		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)")

		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)


			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4258
			beta_t<--0.1795
			beta_t2<--0.00044
			v_0<-0.1008
			cov1<-0.000873
			v_t<-0.000769
			v_e<-0.01703

			fev1_avg <- c()
			vari <- c()

			obs<-input$fev1_0

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height +
			    -0.07634*smo + -0.04159*int + -0.00837*input$age*input$height*input$height +
			    0.02830*(1-smo) + input$int_effect;

			  beta_t_x <- 0.002358*input$age*t1 + -0.00739*gender*t1 + 0.000127*input$weight*t1 +
			    0.06680*input$height*t1 + 0.01565*input$height*input$height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 +
			    -0.00094*input$age*input$height*input$height*t1;


			  beta_x_p <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height +
			    -0.07634*(1) + -0.04159*(0) + -0.00837*input$age*input$height*input$height;

			  beta_t_x_p <- 0.002358*input$age*(-1) + -0.00739*gender*(-1) + 0.000127*input$weight*(-1) +
			    0.06680*input$height*(-1) + 0.01565*input$height*input$height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) +
			    -0.00094*input$age*input$height*input$height*(-1);


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
			}


			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)

			df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
			names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
			

# 			p<-ggplot(df, aes(Time, FEV1))
# 			p <- p + geom_line(aes(y = FEV1), color="black", linetype=1) +
#    			geom_line(aes(y = FEV1_lower), color="red", linetype=2) +
#     			geom_line(aes(y = FEV1_upper), color="red", linetype=2) +
#    			annotate("text", 1, 3.4, label="Mean FEV1 decline", colour="black", size=3, hjust=0) +
#     			annotate("text", 1, 3.3, label="99.5% coverage interval", colour="red", size=3, hjust=0) +
#     			labs(x="Time (years)", y="FEV1 (L)") +
#    			theme_bw()
# 			
# 			p2 <- ggplotly(p)
# 			print(p2)




		} else if (!is.null(input$fev1_prev) & input$model=="Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
		{
			x<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}


			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703

			fev1_avg <- c()
			vari <- c()

			obs<-c(input$fev1_prev,input$fev1_0)

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;

			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 +
			    -0.00092*input$age*input$height*input$height*t1;


			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;

			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) +
			    -0.00092*input$age*input$height*input$height*(-1);


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
			}


			fev1_avg<-c(input$fev1_prev, input$fev1_0, fev1_avg)
			vari<-c(0,0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)

			df<-data.frame(x, y=fev1_avg, fev1_low, fev1_up)
			names(df) <- c("Time", "FEV1", "FEV1_lower", "FEV1_upper")
			
			
			# p<-ggplot(df, aes(Time, FEV1))
			# p <- p + geom_line(aes(y = FEV1), color="black", linetype=1) +
			#        geom_line(aes(y = FEV1_lower), color="red", linetype=2) +
			#        geom_line(aes(y = FEV1_upper), color="red", linetype=2) +
			#        annotate("text", 1, 3.4, label="Mean FEV1 decline", colour="black", size=3, hjust=0) +
			#        annotate("text", 1, 3.3, label="99.5% coverage interval", colour="red", size=3, hjust=0) +
			#        labs(x="Time (years)", y="FEV1 (L)") +
			#        theme_bw()
			# 
			# p2 <- ggplotly(p)
			# print(p2)
		}

	})

























	output$cv<-renderTable({

		if (!is.null(input$fev1_0) & input$model=="Basic model with only baseline FEV1")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			beta_0<-2.7594
			beta_t<--0.04314
			beta_t2<--0.00093
			v_0<-0.3988
			cov1<--0.00048
			v_t<-0.000941
			v_e<-0.01724

			fev1_avg <- c()
			vari <- c()

			obs<-input$fev1_0

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- input$int_effect;

			  beta_t_x <- 0;

			  beta_x_p <- 0;

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
			}


			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)


			cv1<-sqrt(vari[2:12])/(fev1_avg[2:12]-input$fev1_0)

			aa1<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv1)*100,0))
			rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound", "Coefficient of Variation (CV) (%)")
			colnames(aa1)<-c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')

			return(aa1)


		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics")
		{


			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703

			fev1_avg <- c()
			vari <- c()

			obs<-input$fev1_0

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;

			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 +
			    -0.00092*input$age*input$height*input$height*t1;


			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;

			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) +
			    -0.00092*input$age*input$height*input$height*(-1);

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
			}


			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)


			cv2<-sqrt(vari[2:12])/(fev1_avg[2:12]-input$fev1_0)

			aa2<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv2)*100,0))
			rownames(aa2)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound", "Coefficient of Variation (CV) (%)")
			colnames(aa2)<-c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')

			return(aa2)



		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)")
		{


			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4258
			beta_t<--0.1795
			beta_t2<--0.00044
			v_0<-0.1008
			cov1<-0.000873
			v_t<-0.000769
			v_e<-0.01703

			fev1_avg <- c()
			vari <- c()

			obs<-input$fev1_0

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height +
			    -0.07634*smo + -0.04159*int + -0.00837*input$age*input$height*input$height +
			    0.02830*(1-smo) + input$int_effect;

			  beta_t_x <- 0.002358*input$age*t1 + -0.00739*gender*t1 + 0.000127*input$weight*t1 +
			    0.06680*input$height*t1 + 0.01565*input$height*input$height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 +
			    -0.00094*input$age*input$height*input$height*t1;


			  beta_x_p <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height +
			    -0.07634*(1) + -0.04159*(0) + -0.00837*input$age*input$height*input$height;

			  beta_t_x_p <- 0.002358*input$age*(-1) + -0.00739*gender*(-1) + 0.000127*input$weight*(-1) +
			    0.06680*input$height*(-1) + 0.01565*input$height*input$height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) +
			    -0.00094*input$age*input$height*input$height*(-1);


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
			}


			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)


			cv3<-sqrt(vari[2:12])/(fev1_avg[2:12]-input$fev1_0)

			aa3<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv3)*100,0))
			rownames(aa3)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound", "Coefficient of Variation (CV) (%)")
			colnames(aa3)<-c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')

			return(aa3)



		} else if (!is.null(input$fev1_prev) & input$model=="Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
		{


			x<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703

			fev1_avg <- c()
			vari <- c()

			obs<-c(input$fev1_prev,input$fev1_0)

			for (i in 1:11)
			{
			  t1 <- i

			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;

			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 +
			    -0.00092*input$age*input$height*input$height*t1;


			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height +
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;

			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) +
			    -0.00092*input$age*input$height*input$height*(-1);


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
			}


			fev1_avg<-c(input$fev1_prev, input$fev1_0, fev1_avg)
			vari<-c(0,0,vari)

			fev1_up<-fev1_avg+1.96*sqrt(vari)

			fev1_low<-fev1_avg-1.96*sqrt(vari)

			cv4<-sqrt(vari[3:13])/(fev1_avg[3:13]-input$fev1_prev)

			aa4<-rbind(fev1_avg[3:13], fev1_up[3:13], fev1_low[3:13], round(abs(cv4)*100,0))
			rownames(aa4)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound", "Coefficient of Variation (CV) (%)")
			colnames(aa4)<-c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')

			return(aa4)
		}
	}, 
	include.rownames=T,
	caption="FEV1 Heterogeneity",
	caption.placement = getOption("xtable.caption.placement", "top"))



	output$prob_decliner<-renderText({

		if (!is.null(input$fev1_0) & input$model=="Basic model with only baseline FEV1")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			beta_0<-2.7594
			beta_t<--0.04314
			beta_t2<--0.00093
			v_0<-0.3988
			cov1<--0.00048
			v_t<-0.000941
			v_e<-0.01724
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- input$int_effect;
			  
			  beta_t_x <- 0;
			  
			  beta_x_p <- 0;
			  
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
			}  
			
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)						
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)	

			n_mean1<-(fev1_avg[12]-input$fev1_0)/11*1000
			n_sd1<-((fev1_avg[12]-input$fev1_0)/11-(fev1_low[12]-input$fev1_0)/11)/1.96*1000

			bb1<-data.frame(round(pnorm(-40, n_mean1, n_sd1)*100,0))
			print(bb1)
			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years 
			  (declines more than 40 ml/year): '
      bb1 <- paste0(prob_text, as.numeric(bb1), "%")
			return(bb1)


		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics")
		{

			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 + 
			    -0.00092*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) + 
			    -0.00092*input$age*input$height*input$height*(-1);
			  
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
			}  
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)

			n_mean2<-(fev1_avg[12]-input$fev1_0)/11*1000
			n_sd2<-((fev1_avg[12]-input$fev1_0)/11-(fev1_low[12]-input$fev1_0)/11)/1.96*1000

			bb2<-data.frame(round(pnorm(-40, n_mean2, n_sd2)*100,0))
			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years 
			(declines more than 40 ml/year): '
			bb2 <- paste0(prob_text, as.numeric(bb2), "%")
			
			return(bb2)



		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4258
			beta_t<--0.1795
			beta_t2<--0.00044
			v_0<-0.1008
			cov1<-0.000873 
			v_t<-0.000769
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height + 
			    -0.07634*smo + -0.04159*int + -0.00837*input$age*input$height*input$height +
			    0.02830*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002358*input$age*t1 + -0.00739*gender*t1 + 0.000127*input$weight*t1 +
			    0.06680*input$height*t1 + 0.01565*input$height*input$height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 + 
			    -0.00094*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height + 
			    -0.07634*(1) + -0.04159*(0) + -0.00837*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002358*input$age*(-1) + -0.00739*gender*(-1) + 0.000127*input$weight*(-1) +
			    0.06680*input$height*(-1) + 0.01565*input$height*input$height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) + 
			    -0.00094*input$age*input$height*input$height*(-1);			  
			  
			  
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
			}  
		
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)			
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)

			n_mean3<-(fev1_avg[12]-input$fev1_0)/11*1000
			n_sd3<-((fev1_avg[12]-input$fev1_0)/11-(fev1_low[12]-input$fev1_0)/11)/1.96*1000

			bb3<-data.frame(round(pnorm(-40, n_mean3, n_sd3)*100,0))

			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years 
			(declines more than 40 ml/year): '
			bb3 <- paste0(prob_text, as.numeric(bb3), "%")

			return(bb3)



		} else if (!is.null(input$fev1_prev) & input$model=="Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
		{
			x<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-c(input$fev1_prev,input$fev1_0)
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 + 
			    -0.00092*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) + 
			    -0.00092*input$age*input$height*input$height*(-1);
			  
			  
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
			}  
			
			
			fev1_avg<-c(input$fev1_prev, input$fev1_0, fev1_avg)
			vari<-c(0,0,vari)
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)
	
			n_mean4<-(fev1_avg[13]-input$fev1_prev)/12*1000
			n_sd4<-((fev1_avg[13]-input$fev1_prev)/12-(fev1_low[13]-input$fev1_prev)/12)/1.96*1000

			bb4<-data.frame(round(pnorm(-40, n_mean4, n_sd4)*100,0))

			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years 
			  (declines more than 40 ml/year): '
			bb4 <- paste0(prob_text, as.numeric(bb4), "%")
			return(bb4)
		}
	})






















	output$severity<-renderPlotly({

		if (!is.null(input$fev1_0) & input$model=="Basic model with only baseline FEV1")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			beta_0<-2.7594
			beta_t<--0.04314
			beta_t2<--0.00093
			v_0<-0.3988
			cov1<--0.00048
			v_t<-0.000941
			v_e<-0.01724
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- input$int_effect;
			  
			  beta_t_x <- 0;
			  
			  beta_x_p <- 0;
			  
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
			}  
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)						
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)	

			gender<-1
			age_x<-55
			height_x<-1.7

			fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
						(0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)
			
			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
      p_severe<-100-p_mild-p_moderate


			# stage<-rep(0, 1200)
			# stage<-c(rep("Mild",p_mild[1]), rep("Mod",p_moderate[1]), rep("Sev",p_severe[1]),
			# 		rep("Mild",p_mild[2]), rep("Mod",p_moderate[2]), rep("Sev",p_severe[2]),
			# 		rep("Mild",p_mild[3]), rep("Mod",p_moderate[3]), rep("Sev",p_severe[3]),
			# 		rep("Mild",p_mild[4]), rep("Mod",p_moderate[4]), rep("Sev",p_severe[4]),
			# 		rep("Mild",p_mild[5]), rep("Mod",p_moderate[5]), rep("Sev",p_severe[5]),
			# 		rep("Mild",p_mild[6]), rep("Mod",p_moderate[6]), rep("Sev",p_severe[6]),
			# 		rep("Mild",p_mild[7]), rep("Mod",p_moderate[7]), rep("Sev",p_severe[7]),
			# 		rep("Mild",p_mild[8]), rep("Mod",p_moderate[8]), rep("Sev",p_severe[8]),
			# 		rep("Mild",p_mild[9]), rep("Mod",p_moderate[9]), rep("Sev",p_severe[9]),
			# 		rep("Mild",p_mild[10]), rep("Mod",p_moderate[10]), rep("Sev",p_severe[10]),
			# 		rep("Mild",p_mild[11]), rep("Mod",p_moderate[11]), rep("Sev",p_severe[11]),
			# 		rep("Mild",p_mild[12]), rep("Mod",p_moderate[12]), rep("Sev",p_severe[12])
			# 		)
			# 
			# print(stage)
			# stage2<-rep(0, 1200)
			# stage2<-c(rep(0,p_mild[1]), rep(1,p_moderate[1]), rep(2,p_severe[1]),
			# 		rep(0,p_mild[2]), rep(1,p_moderate[2]), rep(2,p_severe[2]),
			# 		rep(0,p_mild[3]), rep(1,p_moderate[3]), rep(2,p_severe[3]),
			# 		rep(0,p_mild[4]), rep(1,p_moderate[4]), rep(2,p_severe[4]),
			# 		rep(0,p_mild[5]), rep(1,p_moderate[5]), rep(2,p_severe[5]),
			# 		rep(0,p_mild[6]), rep(1,p_moderate[6]), rep(2,p_severe[6]),
			# 		rep(0,p_mild[7]), rep(1,p_moderate[7]), rep(2,p_severe[7]),
			# 		rep(0,p_mild[8]), rep(1,p_moderate[8]), rep(2,p_severe[8]),
			# 		rep(0,p_mild[9]), rep(1,p_moderate[9]), rep(2,p_severe[9]),
			# 		rep(0,p_mild[10]), rep(1,p_moderate[10]), rep(2,p_severe[10]),
			# 		rep(0,p_mild[11]), rep(1,p_moderate[11]), rep(2,p_severe[11]),
			# 		rep(0,p_mild[12]), rep(1,p_moderate[12]), rep(2,p_severe[12])
			# 		)
			# 
			# 
			# year<-c(rep(0,100),
			# 		rep(1,100),
			# 		rep(2,100),
			# 		rep(3,100),
			# 		rep(4,100),
			# 		rep(5,100),
			# 		rep(6,100),
			# 		rep(7,100),
			# 		rep(8,100),
			# 		rep(9,100),
			# 		rep(10,100),
			# 		rep(11,100)
			# 		)
      
      
      u1<-matrix(0,nrow=12,ncol=4)
      u1[,1]<-c(0:11)
      u1[,2]<-p_mild
      u1[,3]<-p_moderate
      u1[,4]<-p_severe
      colnames(u1)<-c("year","mild", "moderate", "severe")
      rownames(u1)<-c('Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
      print(u1)
      data <- as.data.frame(u1)

			# dat_sev<-table(stage,year)
			# print(dat_sev)
			# data <- data.frame(year=as.numeric(colnames(dat_sev)), mild=as.numeric(dat_sev[1,]), moderate=as.numeric(dat_sev[2,]),
			#                    severe=as.numeric(dat_sev[3,]))
			
			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild') %>%
			  add_trace(y = ~moderate, name='Moderate') %>%
			  add_trace(y = ~severe, name='Severe') %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack', xaxis=list(title='Year', type='category',
			                                                                          categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade')


# 			barplot(dat_sev, main="Probability of the selected patient being at each GOLD grade",
#   					xlab="Year", ylab="Probability(%)", col=c("green", "yellow", "red")[sort(unique(stage2))+1],
#  					legend = c("Mild", "Moderate", "Severe")[sort(unique(stage2))+1],
# 					args.legend = list(x ="topleft")
# 					)
    print(p)



		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}
			
			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 + 
			    -0.00092*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) + 
			    -0.00092*input$age*input$height*input$height*(-1);
			  
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
			}  
			
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)			


			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)


			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate

			# stage<-rep(0, 1200)
			# stage<-c(rep("Mild",p_mild[1]), rep("Mod",p_moderate[1]), rep("Sev",p_severe[1]),
			# 		rep("Mild",p_mild[2]), rep("Mod",p_moderate[2]), rep("Sev",p_severe[2]),
			# 		rep("Mild",p_mild[3]), rep("Mod",p_moderate[3]), rep("Sev",p_severe[3]),
			# 		rep("Mild",p_mild[4]), rep("Mod",p_moderate[4]), rep("Sev",p_severe[4]),
			# 		rep("Mild",p_mild[5]), rep("Mod",p_moderate[5]), rep("Sev",p_severe[5]),
			# 		rep("Mild",p_mild[6]), rep("Mod",p_moderate[6]), rep("Sev",p_severe[6]),
			# 		rep("Mild",p_mild[7]), rep("Mod",p_moderate[7]), rep("Sev",p_severe[7]),
			# 		rep("Mild",p_mild[8]), rep("Mod",p_moderate[8]), rep("Sev",p_severe[8]),
			# 		rep("Mild",p_mild[9]), rep("Mod",p_moderate[9]), rep("Sev",p_severe[9]),
			# 		rep("Mild",p_mild[10]), rep("Mod",p_moderate[10]), rep("Sev",p_severe[10]),
			# 		rep("Mild",p_mild[11]), rep("Mod",p_moderate[11]), rep("Sev",p_severe[11]),
			# 		rep("Mild",p_mild[12]), rep("Mod",p_moderate[12]), rep("Sev",p_severe[12])
			# 		)
			# 
			# stage2<-rep(0, 1200)
			# stage2<-c(rep(0,p_mild[1]), rep(1,p_moderate[1]), rep(2,p_severe[1]),
			# 		rep(0,p_mild[2]), rep(1,p_moderate[2]), rep(2,p_severe[2]),
			# 		rep(0,p_mild[3]), rep(1,p_moderate[3]), rep(2,p_severe[3]),
			# 		rep(0,p_mild[4]), rep(1,p_moderate[4]), rep(2,p_severe[4]),
			# 		rep(0,p_mild[5]), rep(1,p_moderate[5]), rep(2,p_severe[5]),
			# 		rep(0,p_mild[6]), rep(1,p_moderate[6]), rep(2,p_severe[6]),
			# 		rep(0,p_mild[7]), rep(1,p_moderate[7]), rep(2,p_severe[7]),
			# 		rep(0,p_mild[8]), rep(1,p_moderate[8]), rep(2,p_severe[8]),
			# 		rep(0,p_mild[9]), rep(1,p_moderate[9]), rep(2,p_severe[9]),
			# 		rep(0,p_mild[10]), rep(1,p_moderate[10]), rep(2,p_severe[10]),
			# 		rep(0,p_mild[11]), rep(1,p_moderate[11]), rep(2,p_severe[11]),
			# 		rep(0,p_mild[12]), rep(1,p_moderate[12]), rep(2,p_severe[12])
			# 		)
			# 
			# 
			# 
			# year<-c(rep(0,100),
			# 		rep(1,100),
			# 		rep(2,100),
			# 		rep(3,100),
			# 		rep(4,100),
			# 		rep(5,100),
			# 		rep(6,100),
			# 		rep(7,100),
			# 		rep(8,100),
			# 		rep(9,100),
			# 		rep(10,100),
			# 		rep(11,100)
			# 		)
			
			u1<-matrix(0,nrow=12,ncol=4)
			u1[,1]<-c(0:11)
			u1[,2]<-p_mild
			u1[,3]<-p_moderate
			u1[,4]<-p_severe
			colnames(u1)<-c("year","mild", "moderate", "severe")
			rownames(u1)<-c('Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			print(u1)
			data <- as.data.frame(u1)


			# dat_sev<-table(stage,year)
			# data <- data.frame(year=colnames(dat_sev), mild=as.numeric(dat_sev[1,]), moderate=as.numeric(dat_sev[2,]),
			#                    severe=as.numeric(dat_sev[3,]))
			
			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild') %>%
			  add_trace(y = ~moderate, name='Moderate') %>%
			  add_trace(y = ~severe, name='Severe') %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack', xaxis=list(title='Year', type='category',
			                                                                          categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade')

			print(p)
			
			
		
# 			barplot(dat_sev, main="Probability of the selected patient being at each GOLD grade",
#   					xlab="Year", ylab="Probability(%)", col=c("green", "yellow", "red")[sort(unique(stage2))+1],
#  					legend = c("Mild", "Moderate", "Severe")[sort(unique(stage2))+1],
# 					args.legend = list(x ="topleft")
# 					)




		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4258
			beta_t<--0.1795
			beta_t2<--0.00044
			v_0<-0.1008
			cov1<-0.000873 
			v_t<-0.000769
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height + 
			    -0.07634*smo + -0.04159*int + -0.00837*input$age*input$height*input$height +
			    0.02830*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002358*input$age*t1 + -0.00739*gender*t1 + 0.000127*input$weight*t1 +
			    0.06680*input$height*t1 + 0.01565*input$height*input$height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 + 
			    -0.00094*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height + 
			    -0.07634*(1) + -0.04159*(0) + -0.00837*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002358*input$age*(-1) + -0.00739*gender*(-1) + 0.000127*input$weight*(-1) +
			    0.06680*input$height*(-1) + 0.01565*input$height*input$height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) + 
			    -0.00094*input$age*input$height*input$height*(-1);			  
			  
			  
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
			}  
			
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)			
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)	


			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate


			# stage<-rep(0, 1200)
			# stage<-c(rep("Mild",p_mild[1]), rep("Mod",p_moderate[1]), rep("Sev",p_severe[1]),
			# 		rep("Mild",p_mild[2]), rep("Mod",p_moderate[2]), rep("Sev",p_severe[2]),
			# 		rep("Mild",p_mild[3]), rep("Mod",p_moderate[3]), rep("Sev",p_severe[3]),
			# 		rep("Mild",p_mild[4]), rep("Mod",p_moderate[4]), rep("Sev",p_severe[4]),
			# 		rep("Mild",p_mild[5]), rep("Mod",p_moderate[5]), rep("Sev",p_severe[5]),
			# 		rep("Mild",p_mild[6]), rep("Mod",p_moderate[6]), rep("Sev",p_severe[6]),
			# 		rep("Mild",p_mild[7]), rep("Mod",p_moderate[7]), rep("Sev",p_severe[7]),
			# 		rep("Mild",p_mild[8]), rep("Mod",p_moderate[8]), rep("Sev",p_severe[8]),
			# 		rep("Mild",p_mild[9]), rep("Mod",p_moderate[9]), rep("Sev",p_severe[9]),
			# 		rep("Mild",p_mild[10]), rep("Mod",p_moderate[10]), rep("Sev",p_severe[10]),
			# 		rep("Mild",p_mild[11]), rep("Mod",p_moderate[11]), rep("Sev",p_severe[11]),
			# 		rep("Mild",p_mild[12]), rep("Mod",p_moderate[12]), rep("Sev",p_severe[12])
			# 		)
			# 
			# stage2<-rep(0, 1200)
			# stage2<-c(rep(0,p_mild[1]), rep(1,p_moderate[1]), rep(2,p_severe[1]),
			# 		rep(0,p_mild[2]), rep(1,p_moderate[2]), rep(2,p_severe[2]),
			# 		rep(0,p_mild[3]), rep(1,p_moderate[3]), rep(2,p_severe[3]),
			# 		rep(0,p_mild[4]), rep(1,p_moderate[4]), rep(2,p_severe[4]),
			# 		rep(0,p_mild[5]), rep(1,p_moderate[5]), rep(2,p_severe[5]),
			# 		rep(0,p_mild[6]), rep(1,p_moderate[6]), rep(2,p_severe[6]),
			# 		rep(0,p_mild[7]), rep(1,p_moderate[7]), rep(2,p_severe[7]),
			# 		rep(0,p_mild[8]), rep(1,p_moderate[8]), rep(2,p_severe[8]),
			# 		rep(0,p_mild[9]), rep(1,p_moderate[9]), rep(2,p_severe[9]),
			# 		rep(0,p_mild[10]), rep(1,p_moderate[10]), rep(2,p_severe[10]),
			# 		rep(0,p_mild[11]), rep(1,p_moderate[11]), rep(2,p_severe[11]),
			# 		rep(0,p_mild[12]), rep(1,p_moderate[12]), rep(2,p_severe[12])
			# 		)
			# 
			# 
			# year<-c(rep(0,100),
			# 		rep(1,100),
			# 		rep(2,100),
			# 		rep(3,100),
			# 		rep(4,100),
			# 		rep(5,100),
			# 		rep(6,100),
			# 		rep(7,100),
			# 		rep(8,100),
			# 		rep(9,100),
			# 		rep(10,100),
			# 		rep(11,100)
			# 		)
			# 
			# 
			# dat_sev<-table(stage,year)
			# data <- data.frame(year=colnames(dat_sev), mild=as.numeric(dat_sev[1,]), moderate=as.numeric(dat_sev[2,]),
			#                    severe=as.numeric(dat_sev[3,]))
			
			u1<-matrix(0,nrow=12,ncol=4)
			u1[,1]<-c(0:11)
			u1[,2]<-p_mild
			u1[,3]<-p_moderate
			u1[,4]<-p_severe
			colnames(u1)<-c("year","mild", "moderate", "severe")
			rownames(u1)<-c('Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			print(u1)
			data <- as.data.frame(u1)
			
			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild') %>%
			  add_trace(y = ~moderate, name='Moderate') %>%
			  add_trace(y = ~severe, name="Severe") %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack', xaxis=list(title='Year',type='category',
			                                                                             categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade')
			
			print(p)



		} else if (!is.null(input$fev1_prev) & input$model=="Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
		{
			x<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}
			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-c(input$fev1_prev,input$fev1_0)
			
			for (i in 1:11){
			  t1 <- i
			  
			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 + 
			    -0.00092*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) + 
			    -0.00092*input$age*input$height*input$height*(-1);
			  
			  
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
			}  
			
			
			fev1_avg<-c(input$fev1_prev, input$fev1_0, fev1_avg)
			vari<-c(0,0,vari)
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)

			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate

			# stage<-rep(0, 1300)
			# stage<-c(rep("Mild",p_mild[1]), rep("Mod",p_moderate[1]), rep("Sev",p_severe[1]),
			# 		rep("Mild",p_mild[2]), rep("Mod",p_moderate[2]), rep("Sev",p_severe[2]),
			# 		rep("Mild",p_mild[3]), rep("Mod",p_moderate[3]), rep("Sev",p_severe[3]),
			# 		rep("Mild",p_mild[4]), rep("Mod",p_moderate[4]), rep("Sev",p_severe[4]),
			# 		rep("Mild",p_mild[5]), rep("Mod",p_moderate[5]), rep("Sev",p_severe[5]),
			# 		rep("Mild",p_mild[6]), rep("Mod",p_moderate[6]), rep("Sev",p_severe[6]),
			# 		rep("Mild",p_mild[7]), rep("Mod",p_moderate[7]), rep("Sev",p_severe[7]),
			# 		rep("Mild",p_mild[8]), rep("Mod",p_moderate[8]), rep("Sev",p_severe[8]),
			# 		rep("Mild",p_mild[9]), rep("Mod",p_moderate[9]), rep("Sev",p_severe[9]),
			# 		rep("Mild",p_mild[10]), rep("Mod",p_moderate[10]), rep("Sev",p_severe[10]),
			# 		rep("Mild",p_mild[11]), rep("Mod",p_moderate[11]), rep("Sev",p_severe[11]),
			# 		rep("Mild",p_mild[12]), rep("Mod",p_moderate[12]), rep("Sev",p_severe[12]),
			# 		rep("Mild",p_mild[13]), rep("Mod",p_moderate[13]), rep("Sev",p_severe[13])
			# 		)
			# 
			# stage2<-rep(0, 1300)
			# stage2<-c(rep(0,p_mild[1]), rep(1,p_moderate[1]), rep(2,p_severe[1]),
			# 		rep(0,p_mild[2]), rep(1,p_moderate[2]), rep(2,p_severe[2]),
			# 		rep(0,p_mild[3]), rep(1,p_moderate[3]), rep(2,p_severe[3]),
			# 		rep(0,p_mild[4]), rep(1,p_moderate[4]), rep(2,p_severe[4]),
			# 		rep(0,p_mild[5]), rep(1,p_moderate[5]), rep(2,p_severe[5]),
			# 		rep(0,p_mild[6]), rep(1,p_moderate[6]), rep(2,p_severe[6]),
			# 		rep(0,p_mild[7]), rep(1,p_moderate[7]), rep(2,p_severe[7]),
			# 		rep(0,p_mild[8]), rep(1,p_moderate[8]), rep(2,p_severe[8]),
			# 		rep(0,p_mild[9]), rep(1,p_moderate[9]), rep(2,p_severe[9]),
			# 		rep(0,p_mild[10]), rep(1,p_moderate[10]), rep(2,p_severe[10]),
			# 		rep(0,p_mild[11]), rep(1,p_moderate[11]), rep(2,p_severe[11]),
			# 		rep(0,p_mild[12]), rep(1,p_moderate[12]), rep(2,p_severe[12]),
			# 		rep(0,p_mild[13]), rep(1,p_moderate[13]), rep(2,p_severe[13])
			# 		)
			# 
			# year<-c(rep(-1,100),
			# 		rep(0,100),
			# 		rep(1,100),
			# 		rep(2,100),
			# 		rep(3,100),
			# 		rep(4,100),
			# 		rep(5,100),
			# 		rep(6,100),
			# 		rep(7,100),
			# 		rep(8,100),
			# 		rep(9,100),
			# 		rep(10,100),
			# 		rep(11,100)
			# 		)

			# dat_sev<-table(stage,year)
			# data <- data.frame(year=colnames(dat_sev), mild=as.numeric(dat_sev[1,]), moderate=as.numeric(dat_sev[2,]),
			#                    severe=as.numeric(dat_sev[3,]))
			
			u1<-matrix(0,nrow=13,ncol=4)
			u1[,1]<-c(-1:11)
			u1[,2]<-p_mild
			u1[,3]<-p_moderate
			u1[,4]<-p_severe
			colnames(u1)<-c("year","mild", "moderate", "severe")
			rownames(u1)<-c('Previous','Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			print(u1)
			data <- as.data.frame(u1)
			print(data)
			
			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild') %>%
			  add_trace(y = ~moderate, name='Moderate') %>%
			  add_trace(y = ~severe, name='Severe') %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack', xaxis=list(title='Year', type='category',
			                                                                          categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade')
			
			
			
			# 			barplot(dat_sev, main="Probability of the selected patient being at each GOLD grade",
			#   					xlab="Year", ylab="Probability(%)", col=c("green", "yellow", "red")[sort(unique(stage2))+1],
			#  					legend = c("Mild", "Moderate", "Severe")[sort(unique(stage2))+1],
			# 					args.legend = list(x ="topleft")
			# 					)
			print(p)
		}
	})

































	output$sevTab<-renderTable({

		if (!is.null(input$fev1_0) & input$model=="Basic model with only baseline FEV1")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)
			
			beta_0<-2.7594
			beta_t<--0.04314
			beta_t2<--0.00093
			v_0<-0.3988
			cov1<--0.00048
			v_t<-0.000941
			v_e<-0.01724
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- input$int_effect;
			  
			  beta_t_x <- 0;
			  
			  beta_x_p <- 0;
			  
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
			}  
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)						
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)	


			gender<-1
			age_x<-55
			height_x<-1.7

			fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
						(0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate


			u1<-matrix(0,nrow=3,ncol=12)
			u1[1,]<-p_mild
			u1[2,]<-p_moderate
			u1[3,]<-p_severe
			rownames(u1)<-c("Probability of being mild", "Probability of being moderate", "Probability of being severe")
			colnames(u1)<-c('Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			print(u1)
			return(u1)
			




		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 + 
			    -0.00092*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) + 
			    -0.00092*input$age*input$height*input$height*(-1);
			  
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
			}  
			
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)
			

			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate


			u2<-matrix(0,nrow=3,ncol=12)
			u2[1,]<-p_mild
			u2[2,]<-p_moderate
			u2[3,]<-p_severe
			rownames(u2)<-c("Probability of being mild", "Probability of being moderate", "Probability of being severe")
			colnames(u2)<-c('Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			return(u2)






		} else if (!is.null(input$age) & input$model=="Complete model with baseline FEV1 and patient's characteristics (without hyperresponsiveness)")
		{
			x<-c(0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}

			beta_0<-1.4258
			beta_t<--0.1795
			beta_t2<--0.00044
			v_0<-0.1008
			cov1<-0.000873 
			v_t<-0.000769
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-input$fev1_0
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height + 
			    -0.07634*smo + -0.04159*int + -0.00837*input$age*input$height*input$height +
			    0.02830*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002358*input$age*t1 + -0.00739*gender*t1 + 0.000127*input$weight*t1 +
			    0.06680*input$height*t1 + 0.01565*input$height*input$height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 + 
			    -0.00094*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00482*input$age + 0.4828*gender + -0.00041*input$weight + -1.8759*input$height + 1.9527*input$height*input$height + 
			    -0.07634*(1) + -0.04159*(0) + -0.00837*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002358*input$age*(-1) + -0.00739*gender*(-1) + 0.000127*input$weight*(-1) +
			    0.06680*input$height*(-1) + 0.01565*input$height*input$height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) + 
			    -0.00094*input$age*input$height*input$height*(-1);			  
			  
			  
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
			}  
			
			fev1_avg<-c(input$fev1_0, fev1_avg)
			vari<-c(0,vari)			
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)	

			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate


			u3<-matrix(0,nrow=3,ncol=12)
			u3[1,]<-p_mild
			u3[2,]<-p_moderate
			u3[3,]<-p_severe
			rownames(u3)<-c("Probability of being mild", "Probability of being moderate", "Probability of being severe")
			colnames(u3)<-c('Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			return(u3)





		} else if (!is.null(input$fev1_prev) & input$model=="Model with baseline FEV1, 1-year history of FEV1, and patient's characteristics")
		{
			x<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11)

			if (input$sex=="male")
			{
				gender<-1
			} else if (input$sex=="female")
			{
				gender<-0
			}

			if (input$smoking=="Smoker")
			{
				smo<-1
				int<-0
			} else if (input$smoking=="Sustained quitter")
			{
				smo<-0
				int<-0
			}
			
			beta_0<-1.4212
			beta_t<--0.1779
			beta_t2<--0.00044
			v_0<-0.09728
			cov1<-0.000597
			v_t<-0.000749
			v_e<-0.01703
			
			fev1_avg <- c()
			vari <- c()
			
			obs<-c(input$fev1_prev,input$fev1_0)
			
			for (i in 1:11)
			{
			  t1 <- i
			  
			  beta_x <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*smo + -0.04131*int + 0.002613*input$oco + -0.00820*input$age*input$height*input$height +
			    0.02735*(1-smo) + input$int_effect;
			  
			  beta_t_x <- 0.002313*input$age*t1 + -0.00886*gender*t1 + 0.000149*input$weight*t1 +
			    0.07413*input$height*t1 + 0.01139*input$height*input$height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*input$oco*t1 + 
			    -0.00092*input$age*input$height*input$height*t1;
			  
			  
			  beta_x_p <- -0.00519*input$age + 0.4625*gender + -0.00011*input$weight + -1.7603*input$height + 1.8931*input$height*input$height + 
			    -0.07722*(1) + -0.04131*(0) + 0.002613*input$oco + -0.00820*input$age*input$height*input$height;
			  
			  beta_t_x_p <- 0.002313*input$age*(-1) + -0.00886*gender*(-1) + 0.000149*input$weight*(-1) +
			    0.07413*input$height*(-1) + 0.01139*input$height*input$height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*input$oco*(-1) + 
			    -0.00092*input$age*input$height*input$height*(-1);
			  
			  
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
			}  
			
			fev1_avg<-c(input$fev1_prev, input$fev1_0, fev1_avg)
			vari<-c(0,0,vari)
			
			fev1_up<-fev1_avg+1.96*sqrt(vari)
			
			fev1_low<-fev1_avg-1.96*sqrt(vari)

			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate

			u4<-matrix(0,nrow=3,ncol=13)
			u4[1,]<-p_mild
			u4[2,]<-p_moderate
			u4[3,]<-p_severe
			rownames(u4)<-c("Probability of being mild", "Probability of being moderate", "Probability of being severe")
			colnames(u4)<-c('Previous year', 'Baseline', 'Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9', 'Year 10', 'Year 11')
			return(u4)
		}
	},
	include.rownames=T)
})

shinyApp(ui = ui, server = server)















