
candy <- read.csv("/Users/emiliatrulsson/Desktop/IPG3113N (1).csv")

colnames(candy)<-c("date", "IPG3113N")
candy$date<-as.Date(candy$date, format="%Y-%m-%d")

candyts<- ts(candy$IPG3113N, start=c(1972,1),end=c(2022,4), frequency=12)

candy <- candy %>%
  mutate(Month = yearmonth(date))

candytsi <- as_tsibble(candy, index = Month)
#______________________________________EDA_________________________________________

autoplot(candyts) + labs(y = "Production",
                         x = "Year",
                         title = "Candy Production 1972-2022")
ggAcf(candyts)

ts_heatmap(candyts)
ts_seasonal(candyts, type="cycle")
ts_seasonal(candyts, type="box")

#____________________________________DECOMPOSE_________________________________________

candytsi %>% model(classical_decomposition(IPG3113N, type = "additive")) %>%
  components() %>%
  autoplot() +  labs(title = "Classical Decomposition of Orginial Data")

#________________________CHECK FOR HETRO__________________________________________________

library(lmtest)

model <- lm(IPG3113N~date, data = candy)
lmtest::bptest(model)

distBCMod <- caret::BoxCoxTrans(candy$IPG3113N)
print(distBCMod)

candy <- cbind(candy, IP_bc=predict(distBCMod, candy$IPG3113N)) # append the transformed variable to candy

lmMod_bc <- lm(IP_bc ~ date, data=candy)
lmtest::bptest(lmMod_bc)

candy$IPG3113N <-  NULL

candyts1 <- ts(candy$IP_bc, start=c(1972,1),end=c(2022,4), frequency=12 )
candytsi1 <- as_tsibble(candyts1, index = Month)

autoplot(candyts1)
ggAcf(candyts1)

ts_heatmap(candyts1)
ts_seasonal(candyts1, type="cycle")
ts_seasonal(candyts1, type="box")

#__________________________________SPLIT______________________________________________

train_ts1 <-window(candyts1, start=c(1972,12), end=c(2012,12))
test_ts1 <-window(candyts1, start=c(2013,1))


#____________________________________ETS_________________________________________________

m_ets<-forecast(ets(train_ts1), h=112)
summary(m_ets)
autoplot(m_ets) + autolayer(test_ts1) + labs(title = "ETS(A,N,A) Model of Candy Production")

checkresiduals(m_ets)
accuracy(m_ets, test_ts1)

#____________________________________HOLT WINTER_________________________________________________

m_holtw<-hw(train_ts1, seasonal="additive", h=112)
autoplot(m_holtw) + autolayer(test_ts1)+ ggtitle("Holt Winter Model of Candy Production")

summary(m_holtw)
checkresiduals(m_holtw)
accuracy(m_holtw, test_ts1)


#__________________________________DIFFERENCING______________________________________________

candyts1 %>% diff() -> candy_diff1 # 1 seasonal difference

candyts %>% diff(lag=12) -> candy_diff

#___________________________________KPSS AND ADF______________________________________________

candyts1 %>% ur.kpss(type=c("mu")) %>% summary

candy_diff1 %>% ur.kpss(type=c("mu")) %>% summary

candyts1 %>% ur.df(type=c("drift")) %>% summary

candy_diff1 %>% ur.df(type=c("drift")) %>% summary

#___________________________________SPLIT DATA_______________________________________

train_diff<-window(candy_diff1, start=c(1973,1), end=c(2012,12))
test_diff<-window(candy_diff1, start=c(2013,1))

#____________________________________CHOOSE ARIMA_________________________________________________

acf2(candy_diff, plot=TRUE, main="ACF and PACF of the Differenced Data")


#OR

ggtsdisplay(candy_diff1)

#______________________________________ARIMA______________________________________________


fit_arima <- auto.arima(train_diff1, d=1, D=1, stepwise = FALSE, approximation = FALSE, allowdrift=F, trace= TRUE)

arima_auto=Arima(train_diff, order=c(1,1,0),
            seasonal=list(order=c(1,1,0), period=12))
summary(arima_auto)


arima_guess=Arima(train_diff, order=c(2,1,1),
             seasonal=list(order=c(3,1,1), period=12))


arima_auto_f =forecast(arima_auto, h=112)
autoplot(arima_auto_f) + autolayer(test_diff)

arima_guess_f =forecast(arima_guess, h=112)
autoplot(arima_guess_f) + autolayer(test_diff)


checkresiduals(arima_auto)
checkresiduals(arima_guess)

accuracy(arima_auto_f, test_diff)
accuracy(arima_guess_f, test_diff)

#_______________________________FINAL FORECAST_________________________________________


future_candy=Arima(train_ts1, order=c(2,1,1),
                seasonal=list(order=c(3,1,1), period=12))

arima_auto %>%
  forecast(h=28) %>%
  autoplot(candyts1) 

arima_auto_f =forecast(arima_auto, h=28)
autoplot(arima_auto_f) 

arima_auto_f =forecast(arima_auto, h=28)
autoplot(arima_auto_f) + autolayer(test_diff)
