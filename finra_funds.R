library(jsonlite)

finra.funds.get <- function(file) {
  # Initialize a dataframe to capture the output
  out_df <- data.frame(stringsAsFactors = FALSE)
  
  # Read the CSV file into a dataframe
  in_df <- read.csv(file)
  
  # Loop through the tickers fetching JSON data from finra api
  for (ticker in in_df$Ticker) {
    message("Fetching data for fund with ticker: ", ticker)
    
    # Initialize data elements to capture
    date <- ''
    fticker <- ''
    min_investment <- 0
    expense_ratio <- 0
    cdsc_fee1_months <- 0
    cdsc_fee1 <- 0
    cdsc_fee2_months <- 0
    cdsc_fee2 <- 0
    front_load <- 0
    redemption_fee1_days <- 0
    redemption_fee1 <- 0
    redemption_fee2_days <- 0
    redemption_fee2 <- 0
    
    # Search for the fund by ticker
    search_req <- httr::POST("https://tools.finra.org/fa_api/search/funds",
                             httr::add_headers("Content-Type" = "application/json"),
                             body = paste('{"keyword":"', ticker, '"}'));
    #print(search_req)
    if (search_req$status_code == 200) {
      # Parse the JSON data after a successful request
      search_resp <- httr::content(search_req, "parsed")
      
      if (length(search_resp)) {
        faID <- search_resp[[1]]$faID
        
        # Use the faID to make the funds request
        funds_req <- httr::POST("https://tools.finra.org/fa_api/funds",
                                httr::add_headers("Content-Type" = "application/json"),
                                body = paste('[{"fundId":0, "id":"', faID, '"}]'));
        #print(funds_req)
        if (funds_req$status_code == 200) {
          # Parse the JSON data after a successful request
          funds_resp <- httr::content(funds_req, "parsed")
          
          # Copy the JSON data elements
          date <- funds_resp[[1]]$fund$dataAsOfDate
          fticker <- funds_resp[[1]]$fund$fundDetails$ticker
          min_investment <- funds_resp[[1]]$fund$fundDetails$initialPurchaseMin
          expense_ratio <- funds_resp[[1]]$fund$fees$expenseRatio
          cdsc_fee1_months <- funds_resp[[1]]$backLoad[1][[1]]$cdscPeriod
          cdsc_fee1 <- funds_resp[[1]]$backLoad[1][[1]]$cdscPercentage
          cdsc_fee2_months <- funds_resp[[1]]$backLoad[2][[1]]$cdscPeriod
          cdsc_fee2 <- funds_resp[[1]]$backLoad[2][[1]]$cdscPercentage
          front_load <- funds_resp[[1]]$frontLoad[[1]]$flPercentage
        }
        
        # Make the product info request
        product_req <- httr::POST("https://tools.finra.org/fa_api/productInfo",
                                  httr::add_headers("Content-Type" = "application/json"),
                                  body = paste('[{"fundId":0, "id":"', faID, '"}]'));
        #print(product_req)
        if (product_req$status_code == 200) {
          # Parse the JSON data after a successful request
          product_resp <- httr::content(product_req, "parsed")
          
          # Copy the JSON data elements
          if (length(product_resp)) {
            redemption_fee1_days <- product_resp[[1]][1][[1]]$redemptionPeriod
            redemption_fee1 <- product_resp[[1]][1][[1]]$redemptionPercentage
            redemption_fee2_days <- product_resp[[1]][2][[1]]$redemptionPeriod
            redemption_fee2 <- product_resp[[1]][2][[1]]$redemptionPercentage
          }
        }
      }
      
      # Create a list of data elements. Take care of NULLs.
      row <- list(Ticker = ticker,
                  Date = date,
                  FTicker = fticker,
                  Min_Investment = ifelse(is.null(min_investment), 0, min_investment),
                  Expense_Ratio = ifelse(is.null(expense_ratio), 0, expense_ratio),
                  CDSC_Fee1_Months = ifelse(is.null(cdsc_fee1_months), 0, cdsc_fee1_months),
                  CDSC_Fee1 = ifelse(is.null(cdsc_fee1), 0, cdsc_fee1),
                  CDSC_Fee2_Months = ifelse(is.null(cdsc_fee2_months), 0, cdsc_fee2_months),
                  CDSC_Fee2 = ifelse(is.null(cdsc_fee2), 0, cdsc_fee2),
                  Front_Load = ifelse(is.null(front_load), 0, front_load),
                  Redemption_Fee1_Days = ifelse(is.null(redemption_fee1_days), 0, redemption_fee1_days),
                  Redemption_Fee1 = ifelse(is.null(redemption_fee1), 0, redemption_fee1),
                  Redemption_Fee2_Days = ifelse(is.null(redemption_fee2_days), 0, redemption_fee2_days), 
                  Redemption_Fee2 = ifelse(is.null(redemption_fee2), 0, redemption_fee2))
      
      # Append the list to the output dataframe
      out_df <- rbind(out_df, row, stringsAsFactors = FALSE)
    }
  }
  
  out_df
}

# Main Entry
df <- finra.funds.get(paste(getwd(), "/tickers.all.csv", sep = ''))
message("Done.")
write.csv(df, paste(getwd(), "/funds.all.csv", sep = ''), row.names = FALSE)

