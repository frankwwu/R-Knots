# Read a csv file of historical quotes with Yahoo! Finance APIs; convert the Date; plot a line chart.
# For Yahoo Fanance APIs, see https://code.google.com/p/yahoo-finance-managed/wiki/YahooFinanceAPIs

Yahoo_Quote<-function() {
  uri <- "http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=0&b=2&c=2010&d=0&e=2&f=2014&g=m"
  df <- read.csv(uri)
  
  #convert the 'Date' from a string to a Date type
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  plot(df$Date, df$Adj.Close, ylab="$", xlab="Year", ylim=c(0, 700))
  lines(df$Date, df$Adj.Close)
}