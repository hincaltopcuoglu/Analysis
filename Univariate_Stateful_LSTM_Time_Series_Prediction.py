# -*- coding: utf-8 -*-
"""
Created on Thu May 24 14:32:40 2018

@author: Administrator
"""

# -*- coding: utf-8 -*-
"""
Created on Wed Apr 11 13:36:46 2018

@author: Administrator
"""

# -*- coding: utf-8 -*-
"""
Created on Wed Apr  4 13:25:03 2018

@author: Administrator
"""

from pandas import DataFrame
from pandas import Series
from pandas import concat
from pandas import read_csv
from pandas import datetime
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from math import sqrt
from matplotlib import pyplot
from numpy import array
import pandas as pd
import numpy as np
from keras.layers import Dropout, Flatten, Dense, Activation, Reshape, LeakyReLU , CuDNNLSTM ,PReLU
from keras.layers import BatchNormalization


# load dataset
market_data=pd.read_csv('C:\\Users\\Administrator\\Desktop\\Time_Series_Forecasting\\FEASMIN_Share_D.txt',sep=";",dtype={"Date":object})
market_data = market_data.assign(Date=pd.to_datetime(market_data['Date']))  
market_data['Vol'] = (pd.to_numeric(market_data['Vol'], errors='coerce').fillna(0))



# sort data frame by Date column
market_data = market_data.sort_values(['Date'],ascending = True)

## take last 2  years data
max_date = max(market_data['Date'])
min_date = min(market_data['Date'])
max_date = max_date.strftime('%d/%m/%Y')
enddate = pd.to_datetime(max_date) - pd.DateOffset(days=450)
market_data_filtered = market_data[market_data['Date'] >= enddate]

market_data_droppped = market_data_filtered.drop(market_data_filtered.columns[[0,6]], axis=1)


# CREATING OWN INDEX FOR FLEXIBILITY
obs = np.arange(1, len(market_data_droppped) + 1, 1)

# TAKING DIFFERENT INDICATORS FOR PREDICTION
market_data_filtered['OHLC_avg'] = market_data_filtered[['Open','High','Low','Close']].mean(axis = 1)


market_data_filtered.index = pd.to_datetime(market_data_filtered.pop('Date'))

market_data_droppped = market_data_filtered.drop(market_data_filtered.columns[[0,1,2,4,5,6]], axis=1)



def estimated_autocorrelation(x):
    """
    http://stackoverflow.com/q/14297012/190597
    http://en.wikipedia.org/wiki/Autocorrelation#Estimation
    """
    n = len(x)
    variance = x.var()
    x = x-x.mean()
    r = np.correlate(x, x, mode = 'full')[-n:]
    assert np.allclose(r, np.array([(x[:n-k]*x[-(n-k):]).sum() for k in range(n)]))
    result = r/(variance*(np.arange(n, 0, -1)))
    return result


values = market_data_droppped['Close']

####################
# check auto correlation
from matplotlib import pyplot
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import plot_pacf
#series = Series.from_csv('daily-minimum-temperatures.csv', header=0)
plot_acf(values,lags=50)
pyplot.show()

####################
# check correlation
from matplotlib import pyplot
from pandas.tools.plotting import lag_plot

lag_plot(market_data_droppped['Close'])
pyplot.show()

####################

# determine best lag
from pandas import Series
from matplotlib import pyplot
from statsmodels.tsa.ar_model import AR
from sklearn.metrics import mean_squared_error

# split dataset
X = values.values
train, test = X[1:len(X)-5], X[len(X)-5:]
# train autoregression
model = AR(train)
model_fit = model.fit()
print('Lag: %s' % model_fit.k_ar)
print('Coefficients: %s' % model_fit.params)

# make predictions
predictions = model_fit.predict(start=len(train), end=len(train)+len(test)-1, dynamic=False)
for i in range(len(predictions)):
	print('predicted=%f, expected=%f' % (predictions[i], test[i]))
error = mean_squared_error(test, predictions)
print('Test MSE: %.3f' % error)

# plot results
pyplot.plot(test)
pyplot.plot(predictions, color='red')
pyplot.show()

####################


########################## Data Prep for LSTM Phase ##########################   




# date-time parsing function for loading the dataset
def parser(x):
	return datetime.strptime('190'+x, '%Y-%m')
 
# convert time series into supervised learning problem
def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
	n_vars = 1 if type(data) is list else data.shape[1]
	df = DataFrame(data)
	cols, names = list(), list()
	# input sequence (t-n, ... t-1)
	for i in range(n_in, 0, -1):
		cols.append(df.shift(i))
		names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
	# forecast sequence (t, t+1, ... t+n)
	for i in range(0, n_out):
		cols.append(df.shift(-i))
		if i == 0:
			names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
		else:
			names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
	# put it all together
	agg = concat(cols, axis=1)
	agg.columns = names
	# drop rows with NaN values
	if dropnan:
		agg.dropna(inplace=True)
	return agg
 
# create a differenced series
#def difference(dataset, interval=1):
#	diff = list()
#	for i in range(interval, len(dataset)):
#		value = dataset[i] - dataset[i - interval]
#		diff.append(value)
#	return Series(diff)

# confifure time to look back,time to predict,size of train and test set
# 15 come from above arima model
n_lag = 15 
# 5 days will be predicted ahead
n_seq = 5
# 1 feature (OHLC_Average) will be used
features = 1

# transform series into train and test sets for supervised learning
# extract raw values
raw_values = market_data_droppped.values
raw_values = raw_values.reshape(len(raw_values),1) 

# transform data to be stationary
#diff_series = raw_values(raw_values, 1)
#diff_values = diff_series.values
#diff_values = diff_values.reshape(len(diff_values), 1)
# rescale values to 0, 1
scaler = MinMaxScaler(feature_range=(-1, 1))
scaled_values = scaler.fit_transform(raw_values)
scaled_values = scaled_values.reshape(len(scaled_values), 1)
# transform into supervised learning problem X, y
supervised = series_to_supervised(scaled_values, n_lag, n_seq)
supervised_values = supervised.values

#calculate best train/test split
Dict = {}
for i in range(1,100):
    train_len = supervised_values.shape[0]*(i*0.01)
    test_len = supervised_values.shape[0] - train_len
    if train_len % test_len == 0:
       Dict[train_len] = test_len
    else:
        i = i+1

# 62 selected from Dict above        
n_test = 62

# split into train and test sets
train, test = supervised_values[0:-n_test], supervised_values[-n_test:]


# find different batch sizes from Dict 
for i in range(1,300):
    if 186 % i == 0:
        if 62 % i ==0:
            print(i)
        
batch_size = 62
####################

    
# reshape training into [samples, timesteps, features]
train_X, train_y = train[:, 0:n_lag], train[:, n_lag:]
train_X = train_X.reshape(train_X.shape[0], n_lag, features)

test_X , test_y = test[:, 0:n_lag], test[:, n_lag:]
test_X = test_X.reshape(test_X.shape[0], n_lag, features)
		

# design network
# in stateful model one should but batch_size into input shape and the same value also should be put on batch_size down
model = Sequential()
model.add(LSTM(512, batch_input_shape=(batch_size,train_X.shape[1], train_X.shape[2]),activation='tanh',stateful=True,return_sequences=False))
model.add(Dropout(0.2))
model.add(Dense(n_seq))
model.add(Activation('linear'))
#model.add(LeakyReLU())
model.compile(loss='mse', optimizer='adam')
# fit network
model.fit(train_X, train_y, epochs=1, batch_size=batch_size, validation_data=(test_X, test_y), verbose=2, shuffle=False)
model.reset_states()
# plot history
#pyplot.plot(history.history['loss'], label='train')
#pyplot.plot(history.history['val_loss'], label='test')
#pyplot.legend()
#pyplot.show()







# make a prediction
# test data should be predicted with batch size
yhat = model.predict(test_X,batch_size)

# invert scaling for forecast
# take every 7 variables on certain time ranges like 0 to 6 and 7 to 13 and so forth
# assign this variables to dictionary
# make inverse transform
t1=list()
t2=list()
dic = {}
for g in range(0,n_seq,features):
    for j in range(features-1,n_seq,features):
        t1.append(g)
        t1 = list(set(t1))
        t2.append(j)
        t2 = list(set(t2))
        t1 = sorted(t1)
        t2 = sorted(t2)
        dictionary = dict(zip(t1, t2))        
        for key,value in dictionary.items():
            inv_yhat = np.array(pd.DataFrame(yhat).loc[:,key:value])
            dic[key] = scaler.inverse_transform(inv_yhat)

# take each first column as targeted predicted variable (here is Son) in dict           
listed = list()                 
for k in range(0,n_seq,features):
                listed.append(dic[k][:,0])                    

# make this values data frame
# this DF indicates the target variable that wanted to predicted as times in t,t+1,t+2,t+3,t+4
predicted = pd.DataFrame.from_records(listed)               
predicted = predicted.transpose()
                

# invert scaling for actual 
# same processing as above  for real values which in test_y dataset.remember test_y dataset includes variables from t to t+5
# we trained model from t-35 to t-1,and predicted 5 times ahead ,and then checked predicted results with these real ones in train_y set            
m1=list()
m2=list()
dic = {}
for g in range(0,n_seq,features):
    for j in range(features-1,n_seq,features):
        m1.append(g)
        m1 = list(set(m1))
        m2.append(j)
        m2 = list(set(m2))
        m1 = sorted(m1)
        m2 = sorted(m2)
        dictionary = dict(zip(m1, m2))        
        for key,value in dictionary.items():
            inv_y = np.array(pd.DataFrame(test_y).loc[:,key:value])
            dic[key] = scaler.inverse_transform(inv_y)
           
listed = list()                 
for k in range(0,n_seq,features):
                listed.append(dic[k][:,0])                    
            
real = pd.DataFrame.from_records(listed)               
real = real.transpose()                 

# calculate RMSE
rmse = sqrt(mean_squared_error(real, predicted))
print('Test RMSE: %.3f' % rmse)




## For predicting unseen t+5 closing rates ##

# above section we trained a model with splitting it into train and test sets
# we trained model and predicted on test values
# what if we want to predict forward 5 days which not included in data set ?
# for it I developed a solution
# I took whole data set which ends in 16.03.2018 as new dataset
# then predicted it with previous trained model


# same data operations with different variable names
train_for_pred, test_for_pred = supervised_values[0:-n_test], supervised_values[-n_test:]


# split into input and outputs
# now train and test for pred has 20 columns like above trained model dataset
# Here is the magic,I shifted it with 5 times to make a predition
# the intitution behind time seris prediction is shifting data
# if you take whole data with 20 colums ,this data will not be predicted
# because in above you trained model with 15 time steps(n_lag) with 1 features
# to make data same , I shifted it or take it from colum 5 to 20 which reaches 15 columns data set and this number can be 
# computed by 35*7
# so we can predict 5 days forward this data set
# in this data our last value is 16.03.2018 so we want to predict further days
# in above trained dataset the last value is 11.03.2018 value,then we predicted 16.03.2018
# difference here I took  16.03.2018 as last value to predict unseen future
train_X_pred = train_for_pred[:, n_seq:train_for_pred.shape[1]]
test_X_pred = test_for_pred[:, n_seq:test_for_pred.shape[1]]


# reshape input to be 3D [samples, timesteps, features]
train_X_pred = train_X_pred.reshape((train_X_pred.shape[0], train_X_pred.shape[1] , features))
test_X_pred = test_X_pred.reshape((test_X_pred.shape[0], test_X_pred.shape[1], features))
print(train_X_pred.shape,test_X_pred.shape)

# predict now
yhat_pred = model.predict(test_X_pred,batch_size)


# invert scaling for forecast
t1=list()
t2=list()
dic_pred = {}
for g in range(0,n_seq,features):
    for j in range(features-1,n_seq,features):
        t1.append(g)
        t1 = list(set(t1))
        t2.append(j)
        t2 = list(set(t2))
        t1 = sorted(t1)
        t2 = sorted(t2)
        dictionary = dict(zip(t1, t2))        
        for key,value in dictionary.items():
            inv_yhat_pred = np.array(pd.DataFrame(yhat_pred).loc[:,key:value])
            dic_pred[key] = scaler.inverse_transform(inv_yhat_pred)

# select "Son" variable which wanted to be predicted           
listed_pred = list()                 
for k in range(0,n_seq,features):
                listed_pred.append(dic_pred[k][:,0])                    

predicted_t_5 = pd.DataFrame.from_records(listed_pred)               
predicted_t_5 = predicted_t_5.transpose()

p = list()
for i in range(0,5):
    p.append(predicted_t_5.iloc[-1,i].astype('float32'))
       
f1 = pd.DataFrame(p)
final_prediction_values = f1.transpose()
final_prediction_values.columns = ['t','t+1','t+2','t+3','t+4']










