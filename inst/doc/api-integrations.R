## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE,
  eval = FALSE # API examples require API keys
)

## ----eval=TRUE----------------------------------------------------------------
library(lares)

## -----------------------------------------------------------------------------
# # lares will prompt you to set the directory
# creds <- get_credentials("openai")
# 
# # Or specify manually
# creds <- get_credentials("openai", dir = "~/my_credentials")

## -----------------------------------------------------------------------------
# # Simple question
# response <- gpt_ask("Explain linear regression in 2 sentences")

## -----------------------------------------------------------------------------
# # Create a structured prompt
# prompt <- gpt_prompter(
#   instruction = "Classify these items",
#   input = c("Dog", "Rose", "Car", "Oak"),
#   context = c("Animal", "Plant", "Object"),
#   output = "table",
#   cols = c("Item", "Category")
# )
# 
# # Send to ChatGPT
# result <- gpt_ask(prompt$prompt)

## -----------------------------------------------------------------------------
# # Classify text into categories
# items <- c("Python tutorial", "R package", "Java course", "Statistics book")
# categories <- c("Programming", "Statistics", "Other")
# 
# result <- gpt_classify(items, categories)
# print(result)

## -----------------------------------------------------------------------------
# # Extract specific information
# texts <- c(
#   "My email is john@example.com",
#   "Call me at +1-555-0123",
#   "Visit us at 123 Main St, NYC"
# )
# 
# extractions <- c("email", "phone number", "city")
# 
# result <- gpt_extract(texts, extractions)
# print(result)

## -----------------------------------------------------------------------------
# # Translate text
# text <- rep("Hello, how are you?", 3)
# languages <- c("Spanish", "French", "German")
# 
# result <- gpt_translate(text, languages)
# print(result)

## -----------------------------------------------------------------------------
# # Standardize date formats
# dates <- c("March 15, 2024", "15/03/2024", "2024-03-15")
# format_spec <- "ISO 8601 (YYYY-MM-DD)"
# 
# result <- gpt_format(dates, format = format_spec)
# print(result)

## -----------------------------------------------------------------------------
# # Tag items with multiple labels
# items <- c("Machine learning tutorial", "Data visualization guide")
# tags <- c("AI", "Statistics", "Programming", "Visualization")
# 
# result <- gpt_tag(items, tags)
# print(result)

## -----------------------------------------------------------------------------
# # See all previous prompts and responses from this session
# history <- gpt_history()
# head(history)

## -----------------------------------------------------------------------------
# # Customize model and parameters
# response <- gpt_ask(
#   "Write a haiku about data science",
#   model = "gpt-4",
#   temperature = 0.9, # More creative (0-2)
#   num_retries = 3
# )

## -----------------------------------------------------------------------------
# # Text generation
# response <- gemini_ask(
#   "Explain the Central Limit Theorem",
#   api_key = get_credentials("gemini")$api_key
# )

## -----------------------------------------------------------------------------
# # Analyze an image
# response <- gemini_image(
#   prompt = "Describe this plot in detail",
#   image_path = "path/to/plot.png",
#   api_key = get_credentials("gemini")$api_key
# )

## ----eval=FALSE---------------------------------------------------------------
# # Fetch Apple stock data
# aapl <- stocks_hist(
#   symbols = "AAPL",
#   from = Sys.Date() - 90, # Last 90 days
#   to = Sys.Date()
# )
# 
# head(aapl, 3)

## ----eval=FALSE---------------------------------------------------------------
# # Compare multiple stocks
# tech_stocks <- stocks_hist(
#   symbols = c("AAPL", "GOOGL", "MSFT"),
#   from = "2024-01-01"
# )
# 
# head(tech_stocks, 3)

## ----eval=FALSE---------------------------------------------------------------
# # Real-time quotes
# quote <- stocks_quote("AAPL")
# print(quote)

## -----------------------------------------------------------------------------
# # Track a portfolio
# portfolio <- daily_portfolio(
#   symbols = c("AAPL", "GOOGL", "TSLA"),
#   shares = c(10, 5, 8),
#   from = "2024-01-01"
# )
# 
# # Visualize performance
# splot_summary(portfolio)

## -----------------------------------------------------------------------------
# # Growth over time
# splot_growth(tech_stocks, symbols = c("AAPL", "GOOGL"))
# 
# # Returns on investment
# splot_roi(tech_stocks, investment = 10000)
# 
# # Sector performance
# splot_types(tech_stocks)

## -----------------------------------------------------------------------------
# # Read a Google Sheet
# data <- readGS(
#   title = "My Spreadsheet",
#   sheet = "Sheet1",
#   creds = get_credentials("google")
# )

## -----------------------------------------------------------------------------
# # Write data to Google Sheets
# writeGS(
#   data = mtcars,
#   title = "My Data",
#   sheet = "Cars",
#   creds = get_credentials("google")
# )

## -----------------------------------------------------------------------------
# # Query a database
# query <- "SELECT * FROM users WHERE active = TRUE LIMIT 10"
# result <- queryDB(
#   query = query,
#   from = "my_database_creds"
# )

## -----------------------------------------------------------------------------
# # Get trend data
# trends <- gtrends_time(
#   keyword = "machine learning",
#   from = "2023-01-01",
#   to = "2024-01-01"
# )
# 
# head(trends)

## -----------------------------------------------------------------------------
# # Find related search terms
# related <- gtrends_related(
#   keyword = "data science",
#   categories = c("top", "rising")
# )

## ----eval=FALSE---------------------------------------------------------------
# # ❌ DON'T hardcode
# # api_key <- "sk-1234567890"
# 
# # ✅ DO use credential management
# api_key <- get_credentials("openai")$secret_key

## -----------------------------------------------------------------------------
# # Wrap in try-catch
# result <- tryCatch(
#   {
#     gpt_ask("Your question here")
#   },
#   error = function(e) {
#     message("API error: ", e$message)
#     NULL
#   }
# )

## -----------------------------------------------------------------------------
# # Add delays for bulk operations
# items <- c("item1", "item2", "item3")
# 
# results <- lapply(items, function(item) {
#   result <- gpt_ask(item)
#   Sys.sleep(1) # 1 second delay
#   result
# })

## ----eval=FALSE---------------------------------------------------------------
# # Cache expensive API calls
# result <- cache_pipe(
#   {
#     # This only runs once, then cached and loaded on subsequent calls
#     gpt_ask("Expensive query here")
#   },
#   base = "my_gpt_query"
# )

## -----------------------------------------------------------------------------
# # Use cheaper models for simple tasks
# simple_task <- gpt_ask(
#   "Summarize: The meeting is at 3pm",
#   model = "gpt-3.5-turbo" # Cheaper than gpt-4
# )
# 
# # Use expensive models only when needed
# complex_task <- gpt_ask(
#   "Analyze this complex dataset: ...",
#   model = "gpt-4"
# )

## ----eval=TRUE----------------------------------------------------------------
# In .Renviron file:
# LARES_GPT_MODEL=gpt-4
# LARES_GPT_URL=https://api.openai.com/v1/chat/completions
# LARES_GEMINI_API=https://generativelanguage.googleapis.com/v1beta/models/

# Check current settings
Sys.getenv(c("LARES_GPT_MODEL", "LARES_GEMINI_API"))

## -----------------------------------------------------------------------------
# # Verify credentials are loaded
# creds <- get_credentials("openai")
# if (is.null(creds$secret_key)) {
#   stop("OpenAI API key not found!")
# }

## ----eval=TRUE----------------------------------------------------------------
# Check internet connection
if (haveInternet()) {
  message("Connected to internet")
} else {
  stop("No internet connection")
}

## -----------------------------------------------------------------------------
# # 1. Load data
# data(dft)
# 
# # 2. Get AI summary of data structure
# prompt <- sprintf(
#   "Summarize this dataset structure: %d rows, columns: %s",
#   nrow(dft),
#   paste(colnames(dft), collapse = ", ")
# )
# summary <- gpt_ask(prompt)
# 
# # 3. Get stock data
# stocks <- stocks_hist("AAPL", from = Sys.Date() - 30)
# 
# # 4. Cache the analysis
# analysis <- cache_pipe(
#   {
#     gpt_ask(sprintf(
#       "Analyze this stock: Recent high: $%.2f, Low: $%.2f",
#       max(stocks$High, na.rm = TRUE),
#       min(stocks$Low, na.rm = TRUE)
#     ))
#   },
#   base = "aapl_analysis"
# )
# 
# print(analysis)

