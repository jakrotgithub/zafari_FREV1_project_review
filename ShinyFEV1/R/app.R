library(shiny)
library(ggplot2)
library(plotly)
library(shinyBS)
#devtools::install_github("shiny", "rstudio")

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
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)))

		} else if (input$model==modelOptions[2]){

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),
		       selectInput('sex', 'Sex', c('male', 'female')),

		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

		       selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

		       numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       numericInput('oco', "O'Connor", -12.70, min=-300.00, max=2.00),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)))

		} else if (input$model==modelOptions[3]) {

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),

		       selectInput('sex', 'Sex', c('male', 'female')),

		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

		       selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

		       numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)))


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
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)))

		}

	  })

	output$figure<-renderPlotly({

		if (!is.null(input$fev1_0) & input$model==modelOptions[1]) {

		  df <- fev1_projection(input$fev1_0, input$int_effect)$df

			p <- ggplotly(ggplot(df, aes(Time, FEV1)) + geom_line(aes(y = FEV1), color="black", linetype=1) +
			                geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1) +
			                geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
			                geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
			                annotate("text", 1, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
			                annotate("text", 1.15, 3.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
			                labs(x=xlab, y=ylab) +
			                theme_bw()) %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)

			p$x$data[[1]]$text <- paste0("Time (years): ", df$Time, "<br />", "FEV1 (L): ", round(df$FEV1,3),
			                             "<br />FEv1 lower (L): ", round(df$FEV1_lower,3), "<br />FEV1 upper (L): ",
			                             round(df$FEV1_upper),3)

			p$x$data[[3]]$hoverinfo="none"
			p$x$data[[4]]$hoverinfo="none"
			p


		} else if (!is.null(input$age) & input$model==modelOptions[2]) {

		  df <- fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                             input$height, input$oco)$df


			p <- ggplotly(ggplot(df, aes(Time, FEV1)) + geom_line(aes(y = FEV1), color="black", linetype=1) +
			                 geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1) +
			                 geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
			                 geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
			                 annotate("text", 1, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
			                 annotate("text", 1.15, 3.4, label=coverageInterval, colour=errorLineColor, size=4) +
			                 labs(x=xlab, y=ylab) +
			                 theme_bw()) %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)

			p$x$data[[1]]$text <- paste0("Time (years): ", df$Time, "<br />", "FEV1 (L): ", round(df$FEV1,3),
			                             "<br />FEv1 lower (L): ", round(df$FEV1_lower,3), "<br />FEV1 upper (L): ",
			                             round(df$FEV1_upper),3)

			p$x$data[[3]]$hoverinfo="none"
			p$x$data[[4]]$hoverinfo="none"
			p


		} else if (!is.null(input$age) & input$model==modelOptions[3]){

		  df <- fev1_projection3(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                         input$height)$df

			p <- ggplotly(ggplot(df, aes(Time, FEV1)) + geom_line(aes(y = FEV1), color="black", linetype=1) +
			                 geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1) +
			                 geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
			                 geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
			                 annotate("text", 1, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
			                 annotate("text", 1.15, 3.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
			                 labs(x=xlab, y=ylab) +
			                 theme_bw()) %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)

			p$x$data[[1]]$text <- paste0("Time (years): ", df$Time, "<br />", "FEV1 (L): ", round(df$FEV1,3),
			                             "<br />FEv1 lower (L): ", round(df$FEV1_lower,3), "<br />FEV1 upper (L): ",
			                             round(df$FEV1_upper),3)
			p$x$data[[3]]$hoverinfo="none"
			p$x$data[[4]]$hoverinfo="none"
			p


		} else if (!is.null(input$fev1_prev) & input$model==modelOptions[4]) {

		  df <- fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                         input$height, input$oco)$df

			p <- ggplotly(ggplot(df, aes(Time, FEV1)) + geom_line(aes(y = FEV1), color="black", linetype=1) +
			                 geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1) +
			                 geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
			                 geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
			                 annotate("text", 1, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
			                 annotate("text", 1.15, 3.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
			                 labs(x=xlab, y=ylab) +
			                 theme_bw(), tooltip=c('x','y')) %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)
			p$x$data[[1]]$text <- paste0("Time (years): ", df$Time, "<br />", "FEV1 (L): ", round(df$FEV1,3),
			                             "<br />FEv1 lower (L): ", round(df$FEV1_lower,3), "<br />FEV1 upper (L): ",
			                             round(df$FEV1_upper),3)
			p$x$data[[3]]$hoverinfo="none"
			p$x$data[[4]]$hoverinfo="none"
			p



		}

	})

	output$cv<-renderTable({

		if (!is.null(input$fev1_0) & input$model==modelOptions[1]) {

      aa1 <- fev1_projection(input$fev1_0, input$int_effect)$aa1
			rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
			                 "Coefficient of Variation (CV) (%)")
			colnames(aa1)<- years

			return(aa1)

		} else if (!is.null(input$age) & input$model==modelOptions[2]){

		  aa2 <- fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                          input$height, input$oco)$aa2
		  rownames(aa2)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
		                   "Coefficient of Variation (CV) (%)")
		  colnames(aa2)<- years

			return(aa2)


		} else if (!is.null(input$age) & input$model==modelOptions[3]){

      aa3 <- fev1_projection3(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                              input$height)$aa3

			rownames(aa3)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
			                 "Coefficient of Variation (CV) (%)")
			colnames(aa3)<- years

			return(aa3)


		} else if (!is.null(input$fev1_prev) & input$model==modelOptions[4]){

		  aa4 <- fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking,
		                          input$age, input$weight, input$height, input$oco)$aa4

		  rownames(aa4)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
		                   "Coefficient of Variation (CV) (%)")
		  colnames(aa4)<- years

		  return(aa4)
		}
	},

	include.rownames=T,
	caption="FEV1 Heterogeneity",
	caption.placement = getOption("xtable.caption.placement", "top"))

	output$prob_decliner<-renderText({

		if (!is.null(input$fev1_0) & input$model==modelOptions[1]) {

      bb1 <- fev1_projection(input$fev1_0, input$int_effect)$bb1
			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years
			  (declines more than 40 ml/year): '
      bb1 <- paste0(prob_text, as.numeric(bb1), "%")
			return(bb1)

		} else if (!is.null(input$age) & input$model==modelOptions[2]) {

		  bb2 <- fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                         input$height, input$oco)$bb2

		  prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years
			              (declines more than 40 ml/year): '
			bb2 <- paste0(prob_text, as.numeric(bb2), "%")

			return(bb2)


		} else if (!is.null(input$age) & input$model==modelOptions[3]){

		  bb3 <- fev1_projection3(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                          input$height)$bb3

			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years
			(declines more than 40 ml/year): '
			bb3 <- paste0(prob_text, as.numeric(bb3), "%")

			return(bb3)

		} else if (!is.null(input$fev1_prev) & input$model==modelOptions[4]){

		  bb4 <- fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking,
		                          input$age, input$weight, input$height, input$oco)$bb4

			prob_text <- 'Probability that this patient will be a rapid decliner over the next 11 years
			  (declines more than 40 ml/year): '
			bb4 <- paste0(prob_text, as.numeric(bb4), "%")
			return(bb4)
		}
	})

	output$severity<-renderPlotly({

		if (!is.null(input$fev1_0) & input$model==modelOptions[1]) {

			gender<-1
			age_x<-55
			height_x<-1.7
			x<-c(0:11)

			df <- fev1_projection(input$fev1_0, input$int_effect)$df
			fev1_avg <- df$FEV1
			fev1_low <- df$FEV1_lower
			fev1_up <- df$FEV1_upper

			fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
						(0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
      p_severe<-100-p_mild-p_moderate

      u1<-matrix(0,nrow=12,ncol=4)
      u1[,1]<-c(0:11)
      u1[,2]<-p_mild
      u1[,3]<-p_moderate
      u1[,4]<-p_severe
      colnames(u1)<-c("year","mild", "moderate", "severe")
      rownames(u1)<- c('Baseline', years)
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


		} else if (!is.null(input$age) & input$model==modelOptions[2]) {

		  df <- fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                          input$height, input$oco)$df

		  fev1_avg <- df$FEV1
		  fev1_low <- df$FEV1_lower
		  fev1_up <- df$FEV1_upper
		  x<-c(0:11)

		  if (input$sex=="male"){
		    gender<-1
		  } else if (input$sex=="female"){
		    gender<-0
		  }

			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate

			u1<-matrix(0,nrow=12,ncol=4)
			u1[,1]<-c(0:11)
			u1[,2]<-p_mild
			u1[,3]<-p_moderate
			u1[,4]<-p_severe
			colnames(u1)<-c("year","mild", "moderate", "severe")
			rownames(u1)<-c('Baseline', years)
			print(u1)
			data <- as.data.frame(u1)

			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild', marker = list(color = toRGB("#009E73"))) %>%
			  add_trace(y = ~moderate, name='Moderate', marker = list(color = toRGB("#E69F00"))) %>%
			  add_trace(y = ~severe, name='Severe', marker = list(color = toRGB("#D55E00"))) %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack',
			         xaxis=list(title='Year', type='category', categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade',
			         hovermode='x') %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)


		} else if (!is.null(input$age) & input$model==modelOptions[3]){

		  df <- fev1_projection3(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                          input$height)$df

		  fev1_avg <- df$FEV1
		  fev1_low <- df$FEV1_lower
		  fev1_up <- df$FEV1_upper
		  x<-c(0:11)

		  if (input$sex=="male"){
		    gender<-1
		  } else if (input$sex=="female"){
		    gender<-0
		  }


			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate

			u1<-matrix(0,nrow=12,ncol=4)
			u1[,1]<-c(0:11)
			u1[,2]<-p_mild
			u1[,3]<-p_moderate
			u1[,4]<-p_severe
			colnames(u1)<-c("year","mild", "moderate", "severe")
			rownames(u1)<-c('Baseline', years)
			print(u1)
			data <- as.data.frame(u1)

			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild', marker = list(color = toRGB("#009E73"))) %>%
			  add_trace(y = ~moderate, name='Moderate', marker = list(color = toRGB("#E69F00"))) %>%
			  add_trace(y = ~severe, name='Severe', marker = list(color = toRGB("#D55E00"))) %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack',
			         xaxis=list(title='Year',type='category', categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade',
			         hovermode='x') %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)

			print(p)


		} else if (!is.null(input$fev1_prev) & input$model==modelOptions[4]) {

		  df <- fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking,
		                          input$age, input$weight, input$height, input$oco)$df

		  fev1_avg <- df$FEV1
		  fev1_low <- df$FEV1_lower
		  fev1_up <- df$FEV1_upper
		  x<-c(-1:11)

			if (input$sex=="male"){
				gender<-1
			} else if (input$sex=="female"){
				gender<-0
			}

			fev_pred<-(0.5536 + -0.01303*(input$age+x) + -0.000172*(input$age+x)^2 + 0.00014098*(input$height*100)^2)*gender +
						(0.4333 + -0.00361*(input$age+x) + -0.000194*(input$age+x)^2 + 0.00011496*(input$height*100)^2)*(1-gender)

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_severe<-100-p_mild-p_moderate

			u1<-matrix(0,nrow=13,ncol=4)
			u1[,1]<-c(-1:11)
			u1[,2]<-p_mild
			u1[,3]<-p_moderate
			u1[,4]<-p_severe
			colnames(u1)<-c("year","mild", "moderate", "severe")
			rownames(u1)<-c('Previous','Baseline', years)
			print(u1)
			data <- as.data.frame(u1)
			print(data)

			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild', marker = list(color = toRGB("#009E73"))) %>%
			  add_trace(y = ~moderate, name='Moderate', marker = list(color = toRGB("#E69F00"))) %>%
			  add_trace(y = ~severe, name='Severe', marker = list(color = toRGB("#D55E00"))) %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack',
			         xaxis=list(title='Year', type='category', categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade',
			         hovermode='x') %>% config(displaylogo=F, modeBarButtonsToRemove=buttonremove)

			print(p)
		}
	})


	output$sevTab<-renderTable({

		if (!is.null(input$fev1_0) & input$model==modelOptions[1]) {

			gender<-1
			age_x<-55
			height_x<-1.7
			x<-c(0:11)

			df <- fev1_projection(input$fev1_0, input$int_effect)$df
			fev1_avg <- df$FEV1
			fev1_low <- df$FEV1_lower
			fev1_up <- df$FEV1_upper

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
			colnames(u1)<-c('Baseline', years)
			print(u1)
			return(u1)

		} else if (!is.null(input$age) & input$model==modelOptions[2]) {

		  df <- fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
		                         input$height, input$oco)$df

		  fev1_avg <- df$FEV1
		  fev1_low <- df$FEV1_lower
		  fev1_up <- df$FEV1_upper
		  x<-c(0:11)

		  if (input$sex=="male"){
		    gender<-1
		  } else if (input$sex=="female"){
		    gender<-0
		  }

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
			colnames(u2)<- c('Baseline', years)

		} else if (!is.null(input$age) & input$model==modelOptions[3]) {


		  df <- fev1_projection3(fev1_0=input$fev1_0, int_effect=input$int_effect, sex=input$sex, smoking=input$smoking,
		                         age=input$age, weight=input$weight, height=input$height)$df

		  fev1_avg <- df$FEV1
		  fev1_low <- df$FEV1_lower
		  fev1_up <- df$FEV1_upper
		  x<-c(0:11)

			if (input$sex=="male"){
				gender<-1
			} else if (input$sex=="female"){
				gender<-0
			}


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
			colnames(u3)<-c('Baseline', years)
			return(u3)

		} else if (!is.null(input$fev1_prev) & input$model==modelOptions[4]) {

			df <- fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking,
			                       input$age, input$weight, input$height, input$oco)$df

			fev1_avg <- df$FEV1
			fev1_low <- df$FEV1_lower
			fev1_up <- df$FEV1_upper
			x<-c(-1:11)

			if (input$sex=="male") {
				gender<-1
			} else if (input$sex=="female") {
				gender<-0
			}

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
			colnames(u4)<-c('Previous year', 'Baseline', years)
			return(u4)
		}
	},
	include.rownames=T)
})

shinyApp(ui = ui, server = server)


