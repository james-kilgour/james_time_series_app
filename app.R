


# 1. Housekeeping and set up ----------------------------------------------

library(tidyverse)
library(fpp3)
library(patchwork)
library(gt)
library(htmlwidgets)


## 1.2 Load data: ---------------------------------------------------------

map(list.files("../data", full.names = T), # Returns all files within the data folder 
		function(x){
	
	# Parse name from string
	name <- str_remove(x, ".csv")
	name <- str_remove(name, "../data/")
	name <- str_replace(name, " ", "_") # Add underscore so R doesn't freak out about the space
	
	data <- read_csv(x)  # Load data
		
	
	# Assign the data to the global environment using the name we parsed
	assign(name, data, envir = globalenv())
})



## 1.3 Tidying up and formatting -----------------------------------------

# We need to do some tidying of the data before we can visualise it. 
# We're interested in England-wide time series analysis and the data 
# are aggregated to regional level so we need go a level higher:

Population_data <- Population_data[c(8:nrow(Population_data)), ] |> 
	# formats variables (ahead of join)
	rename(year = 1,
				 mid_year_estimate = 2) |> 
	# Filters for data we have prescription counts for
	filter(year >= 2020) |> 
	# Formats observations as numbers
	mutate(across(everything(), as.numeric))

# We need to format all our prescibing data in the same way, so let's use a 
# function to save us some time.

map(setdiff(ls(), "Population_data"), # Returns our prescribing data from the global envir.
		function(x){

	data <- get(x) |> # Gets data from global envir
		group_by(date) |> 
		# Adds all NHS regions within the same month up 
		summarise(y_items = sum(y_items), 
							y_actual_cost = sum(y_actual_cost)) |> 
		# Prep for join
		mutate(year = year(date),
					 date = yearmonth(date)) |>
		# Joins to mid year population estimates
		left_join(Population_data, 
							by = "year") |> 
		# Padds any empty observations with historic data
		fill(mid_year_estimate, .direction = "downup") |> 
		as_tsibble(index = date) # Formats as time series object, with date as the main index variable
	
	assign(x, data, envir = globalenv()) # Takes object name and reassigns it to the same object, newly formatted
	
})



# 5 Forecasting -------------------------------------------------------------

# Let's now forecast the data. 

# Going to have to do lots of tinkering with the data to compare different 
# models (will probably just stick to SES models; data aren't stationary so 
# ARIMA are no use and we don't have wider variables so can't regress
# either), so let's make another app.



library(shiny)

# Made a quick and dirty shiny app to play around with exponential smoothing methods

ui <- fluidPage(
	sidebarPanel(
		
		# User selects data to model
		selectInput('data_frame', 'Dataset', c("Antibacterial_data", "Antifungal_data", "Antiprotozoal_data","Antiviral_data")),
		
		# First of three user inputs determining model type
		selectInput('error_term', 'error', c("N", "A", "Ad", "M")),
		
		# Second of three user inputs determining model type
		selectInput('trend_term', 'trend', c("N", "A", "Ad", "M")),
		
		# Dynamic UI element to be rendered in server
		uiOutput("phi_popup"),
		
		# User input to change length of forecast. 
		# Can go higher than max specified here but SES models aren't great at long
		# term forecasts so short(ish) 12m max should only really be used here.
		numericInput('forecast_length', 'forecast_length', 6, min = 1, max = 12),
		
		# Third of three user inputs determining model type
		selectInput('season_term', 'season', c("N", "A", "Ad", "M")),
		
		# Resets whole app - easier than clicking all inputs
		actionButton("reset_input", "Reset inputs")
		
	),
	mainPanel(
		plotOutput('plot1'), # Model based on user inputs
		plotOutput('plot2'), # Model based on ETS() algo
		tableOutput("table1") # Comparator table to benchmark against
	)
)



server <- function(input, output, session) {

	# Enders depending on whether dampened term is requested by user
	# Gives a wee slider for coefficient phi term to use in model.
	output$phi_popup <- renderUI({
		
		if(input$trend_term == "Ad"){
			sliderInput('phi_term', "phi term", min = 0, max = 1, value = 1)
		} else{
			NULL # If not addiditve dampened model, do/return nothing
		}
		
		
	})

	output$plot1 <- renderPlot({
		
		# First plot we'll show at the top. 
		data <- get(input$data_frame) # Pull the data the user wants

		# Then, model depending on wider inputs. 
		# IF/ELSE logic here only adjusts whether phi term is needed in dampened model
		# Otherwise, it's the same modelling process.
		if(input$trend_term == "Ad"){
			fit <- data |>
				model(additive = ETS(y_items ~ error(input$error_term) + trend(input$trend_term, phi = input$phi_term) + season(input$season_term)))
		} else{
			fit <- data |>
				model(additive = ETS(y_items ~ error(input$error_term) + trend(input$trend_term) + season(input$season_term)))
		}
		
		# Take model fit, forecast by how long user wants
		fit |> forecast(h = input$forecast_length) |>
			autoplot(data) + # Plot the main data
			geom_line(aes(y = .fitted), col="#D55E00", # And then overlay the model based on user inputs (incl forecast)
								data = augment(fit)) +
			scale_y_continuous(labels = scales::label_comma()) + # Make y axis nice and pretty
			# Tweak model title; telling users what they've chosen
			labs(title = str_glue("Your model: ETS({input$error_term},{input$trend_term},{input$season_term})"),
					 subtitle = "Blue data = forecast period",
					 x = "Date",
					 y = "Items presribed")
		
		# Implicit returned plot object
		
	})
	
	output$plot2 <- renderPlot({
		
		# Second plot we'll show
		data <- get(input$data_frame) # Pull the data the user wants
		
		# Model data, optimal model chosen by minimising AICc
		fit <- data |>
			model(ETS(y_items))

		type <- report(fit) # Pull model title (used in plot title below)

		# Take model fit, forecast by how long user wants
		fit |>
			forecast(h = input$forecast_length) |> 
			autoplot(data) + # Plot the main data
			geom_line(aes(y = .fitted), col="#D55E00", # Also overlay "optimal" model based on user inputs (incl forecast)
								data = augment(fit)) +
			scale_y_continuous(labels = scales::label_comma()) + # Make y axis nice
			# Tweak model title; telling users what the algo has chosen
			labs(title = str_glue("Algorithmically optimised model: {type$`ETS(y_items)`}"),
					 subtitle = "Blue data = forecast period",
					 x = "Date",
					 y = "Items presribed")
		
		# Implicit returned plot object
		
	})
	
	output$table1 <- renderTable({
		
		# Final object we'll return in main page
		data <- get(input$data_frame) # Get user data

		# Basically repeat both of the modelling process above.
		
		# Note to self:: 
		## As I'm documenting this, there's probably an easier/more efficient way to
		## do this modelling servert side but this works for now and the app is fast 
		## enough
				
		if(input$trend_term == "Ad"){
			
			my_fit <- data |>
				model(my_model = ETS(y_items ~ error(input$error_term) + trend(input$trend_term, phi = input$phi_term) + season(input$season_term)))
			
		} else{
			
			my_fit <- data |>
				model(my_model = ETS(y_items ~ error(input$error_term) + trend(input$trend_term) + season(input$season_term)))
			
		}
		
		algo_fit <- data |>
			model(ETS(y_items))
		
		# Make a wee table of the model AICc data
		
		tibble(`Your AIC (corrected) value` = broom::glance(my_fit)$AICc,
					 `Alogithm-derived AIC (corrected) value` = broom::glance(algo_fit)$AICc
					 ) |> 
			gt() |> # Make it nice and pretty
			fmt_number() # And format it corrently.
		
		# Implicit returned plot object
		
	})
	
	
	# Linked to reset button - reloads app in initial state
	observeEvent(input$reset_input, {
		session$reload()
	})
	
}

shinyApp(ui = ui, server = server)