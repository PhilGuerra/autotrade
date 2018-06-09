library(PerformanceAnalytics)
# THIS SCRIPT IS USED TO DETERMINE THE DISTRIBUTION OF YOUR ALGO'S SHARPE RATIO WITHOUT HAVING TO BOOTSTRAP
# 1. LOAD DATA
# 2. TEST FOR SERIAL CORRELATION / AUTOREGRESSION IN YOUR RETURN TIMESERIES
# 3. IF RETURNS ARE NOT AUTOCORRELATED, ASSUME I.I.D. - BE CAREFUL HERE
# 4. CALCULATE EMPIRICAL SHARPE RATIO
# 5. APPLY A. LO FORMULA TO DETERMINE VARIANCE OF SHARPE RATIO +/- CONFIDENCE INTERVALS.
# NOTE: IT'S PROBABLY BETTER TO BOOTSTRAP WHICH I WOULD ALMOST ALWAYS RECOMMEND OVER THIS METHOD.


####################################
### Load Data and convert to XTS ###
####################################
AcctUpdate <-(read.csv("~/AcctUpdate.csv",header=T,stringsAsFactors = FALSE))  # PROVIDE A COLUMN OF YOUR ALGO'S DAILY RETURNS HERE.
AcctUpdate <- AcctUpdate[,-1]    
AcctUpdate$Return <- as.xts(as.numeric(AcctUpdate$Return), order.by=as.Date(AcctUpdate[,1]),"%Y-%M-%D")

########################################
### Determine if serial correlation. ###
########################################
Box.test(AcctUpdate$Return, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
# Box-Pierce test
# 
# data:  AcctUpdate$Return
# X-squared = 0.6028, df = 1, p-value = 0.4375

table.Autocorrelation(AcctUpdate$Return, digits=4)
# rho1          0.0788
# rho2         -0.1086
# rho3         -0.1361
# rho4          0.0919
# rho5          0.0978
# rho6          0.1387
# Q(6) p-value  0.2686

##############################################
### Not autocorrelateed, now assume(!) iid ###
##############################################
SharpeRatio.annualized(AcctUpdate$Return,scale=252)
# Annualized Sharpe Ratio (Rf=0%) 2.637257 <<-- SHARPE RATIO IS ALSO CORROBORATED BY INTERACTIVE BROKERS RISK ANALYSIS DOCUMENT

####################################### "The Statistics of Sharpe Ratios, Lo, A. 2002 AIMR"
### Calculate Sharpe Ratio variance ### w = (1+ 0.5SR^2)/N
#######################################
# st.error.var <- (1+0.5*2.637257^2/98)  # 0.04568941 from 98 daily observations
