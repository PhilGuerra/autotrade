#### IF YOU USE ANY PART OF THIS CODE, YOU ASSUME ALl RISK ASSOCIATED WITH ITS USE - INCLUDING TOTAL LOSS OF PRINCIPAL
#### FOR EDUCATIONAL USE ONLY - NOT FOR PRODUCTION USE!

tradeOn <- 0 # THIS FLAG IS USED AS A SAFETY TO SEND THE ORDERS OR NOT. 1 = TRANSMIT ORDERS. 0 = DON'T TRANSMIT.

# 1. DETERMINE THE NEW ALLOCATION AS RECOMMENDED BY YOUR STRATEGY
      # CREATE A DATAFRAME WITH ONE SYMBOL PER ROW AND ONE COLUMN THAT HOLDS THE NUMBER OF SHARES YOU ARE SUPPOSED TO BUY FOR EACH SYMBOL
      # FOR EXAMPLE, BELOW ARE THE **WEIGHTS** OF THE NEW RECOMMENDED ALLOCATION. THE WEIGHTS NEED TO BE CHANGED TO **ACTUAL NUMBER OF SHARES** OR AUTOTRADE WON'T WORK.
      # IF YOUR STRATEGY GIVES YOU WEIGHTS, JUST MULTIPLY WEIGHT BY YOUR CAPITAL AVAILABLE (SEE #2 BELOW)
      # AAXJ 0.006202671
      # GLD  0.062441354  #<-- ALL THESE WEIGHTS NEED TO BE CONVERTED INTO ACTUAL SHARE NUMBERS!
      # BND  0.628828412
      # MTUM 0.302527563
      # EXCEPT FOR buy.df, sell.df and df1 dataframes below, YOU MUST REMEMBER TO USE THE SAME "UNIVERSE" OF STOCKS AT ALL TIMES 
      # SO THAT THE NEW ALLOCATION AND CURRENT PORTFOLIO DATAFRAMES HAVE EQUAL NUMBERS OF ROWS. 


# 2. USING IBROKERS AND TWSINSTRUMENT, DOWNLOAD YOUR CAPITAL BALANCE AND YOUR CURRENT PORTFOLIO AT IB
      library(IBrokers)
      library(twsInstrument)
      eventWrapper = eWrapper()
      tws  = twsConnect(clientId=110, port=4001) 
      res1 = reqAccountUpdates(tws, subscribe = TRUE, eventWrapper = eWrapper(),  CALLBACK=twsCALLBACK)
      res2 <- twsPortfolioValue(res1, zero.pos=FALSE)
      capital = as.numeric(res1[[1]]$NetLiquidationByCurrency[1]) * 0.99 # LEAVE A LITTLE ROOM FOR COMMISSIONS
      twsDisconnect(tws)
      curPort <- res2 # THIS IS YOUR CURRENT PORTFOLIO AT IB
      curPort <- data.frame(as.character(res2$local),as.numeric(as.character(res2$position)),stringsAsFactors = FALSE)
      colnames(curPort) <- c('id', 'SharesHeld')
      
      ### OR USING IB_INSYNC AND RETICULATE LIBRARIES, HERE'S ANOTHER WAY TO DOWNLOAD CAPITAL BALANCE AND CURRENT PORTFOLIO ###
      # getBalance <- function(accountNumber)  ib$accountSummary(account=accountNumber)[[21]]$value
      # 
      # getPositions <- function(accountNumber) {
      #   position.df <- data.frame()
      #   position.insync <- ib$positions(account=accountNumber)
      #   for (i in 1:length(position.insync)) {
      #     position.df[i,1] <- position.insync[[i]]$contract$localSymbol
      #     position.df[i,2] <- position.insync[[i]]$position[1]
      #     colnames(position.df) <- c('id','SharesHeld')
      #   }
      #   return(position.df)
      # }


# 3. CREATE A DATAFRAME THAT HOLDS BOTH THE OLF/PRIOR ALLOCATION (I.E. CURRENT PORTFOLIO) AND THE NEW ALLOCATION (AS RECOMMENDED BY YOUR STRATEGY) - 
      # INCLUDING THE NUMBER OF SHARES IN BOTH THE OLD/CURRENT PORTFOLIO AND THE NEW RECOMMENDED ALLOCATION

      ### CREATE AND MERGE WITH DUMMY DATAFRAME GET NEW RECOMMENDED ALLOCATION
            # EXAMPLE OF DATAFRAME WITH YOUR NEW, RECOMMENDED ALLOCATION AS DETERMINED BY YOUR SIGNAL/STRATEGY. THIS SHOULD
            # CONTAIN THE ENTIRE UNIVERSE OF SECURITIES THAT ARE AVAILABLE FOR YOUR STRATEGY TO TRADE.
            #
            #         2018-06-04 (I.E. THE NEW, RECOMMENDED ALLOCATION FROM YOUR STRATEGY)
            # 6AM8             4
            # 6BM8             6
            # 6CM8             4
            # 6EM8             2
            #
            df <- as.data.frame(t(tail(round(YOUR_STRATEGY,0),1)))  # AS IN EXAMPLE ABOVE, THIS IS THE MOST RECENT RECOMMENDED ALLOCATION OF YOUR STRATEGY I.E. THE NEW ALLOCATION THAT WILL REPLACE YOUR CURRENT PORTFOLIO IN #2 ABOVE
            df$id <- row.names(df)
            colnames(df) <- c('SharesHeld','id')
            df$SharesHeld <- 0 #DUMMY DATAFRAME
      
      ### MERGE IB PORTFOLIO AND NEW RECOMMENDED ALLOCATION
            df1 <- merge(curPort,df,by='id',all=TRUE)
            df1[is.na(df1)] <- 0
            colnames(df1) <- c('id','prior','new')
            
      # CREATE DATAFRAME WITH PRIOR ALLOCATION AND NEW ALLOCATION
            tmp <- as.data.frame(t(tail(round(YOUR_STRATEGY,0),1))) #THIS IS YOUR MOST RECENT ALLOCATION FROM YOUR STRATEGY - AGAIN.
            tmp$id <- row.names(tmp)
            colnames(tmp) <- c('SharesHeld','id')
            df1$new <- plyr::arrange(tmp, id) #?NOT NECESSARY IF SYMBOLNAMES IN ALPHABETICAL ORDER ALREADY?
            colnames(df1) <- c('id','prior','new')
            df1 <- data.frame(df1$id,df1$prior,df1$new$SharesHeld)
            colnames(df1) <- c('id','prior','new')

# 4. CREATE BUY AND SELL DATAFRAMES
            # EXAMPLE:
            # > df1
            #         id prior new  sell (NEGATIVE NUMBERS HERE = BUY THOSE NUMBER OF SHARES. SEE NOTE BELOW.)
            # 1     6AM8     3   4    -1
            # 2     6BM8     2   6    -4
            # 3     6CM8     3   4    -1
            # 4     6EM8     2   2     0
            # 
            # EXAMPLE: buy.df$sell will be what you BUY. sell.df$sell will be what you SELL.
            #         id prior new  sell* see note below!
            # 1     6AM8     3   4     1
            # 2     6BM8     2   6     4
            # 3     6CM8     3   4     1
            # 6     6EM8     2   2     0
            df1$sell <- df1$prior - df1$new  #THIS COLUMN "SELL" IS ACTUALLY BOTH A BUY AND SELL COLUMN. POSITIVE NUMBER = NUMBER OF SHARES TO SELL, NEGATIVE NUMBER = NUMBER TO BUY.
            sell.df <- data.frame(df1[df1$sell > 0, ],stringsAsFactors = FALSE)
            sell.df$id <- as.character(sell.df$id)
            buy.df  <- data.frame(df1[df1$sell < 0, ],stringsAsFactors = FALSE)
            buy.df$sell <- abs(buy.df$sell)
            buy.df$id <- as.character(buy.df$id)

######################################################################################################
# BEGIN RETICULATE PYTHON CODE #
######################################################################################################
Sys.setenv(RETICULATE_PYTHON = "/opt/Python-3.6.3") # IF YOU INSTALL PYTHON CORRECTLY IN LINUX, YOU SHOULDN'T NEED THIS
library(reticulate) 
insync <- import("ib_insync")
ib <- insync$IB()
ib$connect( port = "4001" )
###################
### SELL ORDERS ###
###################
if (nrow(sell.df) > 0 ) {
  
  for (i in 1:nrow(sell.df)) {
    conContract = insync$Contract()
    conContract$symbol = sell.df[i,1] # THE SYMBOL THAT YOU WILL BUY OR SELL
    conContract$secType = "STK"
    conContract$exchange = "SMART"
    conContract$currency = "USD"
    ib$qualifyContracts(conContract)
    print(conContract)
    
    order = insync$MarketOrder(action = 'SELL',
                               totalQuantity = sell.df[i,4],  # THE NUMBER OF SHARES TO BUY OR SELL
                               algoStrategy = 'Adaptive',
                               algoParams = list(insync$TagValue('adaptivePriority', 'Normal'))) #IBALGO ACTION!
    
    print(order)
    if (tradeOn == 1) trade = ib$placeOrder(conContract,order) #tradeOn is my safety. 1 = yes transmit the trade. 0 = don't transmit order.
    
  }
  
}

###################
### BUY  ORDERS ###
###################
if (nrow(buy.df) > 0 ) {
  
  for (i in 1:nrow(buy.df)) {
    conContract = insync$Contract()
    conContract$symbol = buy.df[i,1]
    conContract$secType = "STK"
    conContract$exchange = "SMART"
    conContract$currency = "USD"
    ib$qualifyContracts(conContract)
    print(conContract)
    
    order = insync$MarketOrder(action = 'BUY',
                               totalQuantity = buy.df[i,4],
                               algoStrategy = 'Adaptive',
                               algoParams = list(insync$TagValue('adaptivePriority', 'Patient')))
    
    print(order)
    if (tradeOn == 1) trade = ib$placeOrder(conContract,order)
    
  }
  
}

ib$disconnect()
