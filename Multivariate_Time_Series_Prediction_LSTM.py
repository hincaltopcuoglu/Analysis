# -*- coding: utf-8 -*-
"""
Created on Tue May  8 14:18:47 2018

@author: Administrator
"""

# Importing the libraries
import pandas as pd
import time
import matplotlib.pyplot as plt
import datetime
import numpy as np
import gc
import keras
from keras.models import Sequential
from keras.layers import Activation, Dense
from keras.layers import LSTM
from keras.layers import Dropout
from matplotlib import pyplot
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
from math import sqrt
from numpy import concatenate
from keras.layers import Dropout, Flatten, Dense, Activation, Reshape, LeakyReLU

######################################### IMPORT DATA AND MAKE BASIC DATA OPERATIONS #########################################


DIRPATH = "C:\\Users\\Administrator\\Desktop\\All\\Daily\\"
ALLDATA = DIRPATH + "all.txt"


tempx = pd.read_csv(ALLDATA,delimiter=';')



gb = tempx.groupby('Firm')
#make all the groups data frame in a dict
d = {}
for k in gb.groups:
 d[k] = pd.DataFrame(gb.get_group(k))
 
 

# delete shares less then 100 days 
for key in list(d.keys()):  
    if len(d[key])<100:
        try:
            del d[key]
        except KeyError:
            pass


pd.set_option('display.float_format', lambda x: '%.3f' % x)

#sort it according to Date
for key in d:
    d[key]=d[key].groupby(['Firm']).apply(lambda x: x.sort_values(["Date"], ascending = True))

   

#make date column in date format
for key in d:
    d[key]['Date']=pd.to_datetime(d[key]['Date'], format='%Y%m%d', errors='ignore')
    

#################################################################
## investigate about autocorrelation
def acf_by_hand(x, lag):
    # Slice the relevant subseries based on the lag
    y1 = x[:(len(x)-lag)]
    y2 = x[lag:]
    # Subtract the mean of the whole series x to calculate Cov
    sum_product = np.sum((y1-np.mean(x))*(y2-np.mean(x)))
    # Normalize with var of whole series
    return sum_product / ((len(x) - lag) * np.var(x))

ff={}
for key in d:
    ff[key]=acf_by_hand(d[key]["Close"],10)
    

from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import plot_pacf
#series = Series.from_csv('daily-minimum-temperatures.csv', header=0)
plot_acf(d['XShare']['Close'])   

plot_pacf(g['XShare']['Close'],lags=50)  

#################################################################
    
    
data = d['AAP_Share']    
#BESHAHAB_Share

# adding new variables function
def add_volatility(data):
  """
  data: input data, pandas DataFrame
  coins: default is for 'btc and 'eth'. It could be changed as needed
  This function calculates the volatility and close_off_high of each given coin in 24 hours, 
  and adds the result as new columns to the DataFrame.
  Return: DataFrame with added columns
  """
  #for coin in coins:
    # calculate the daily change
  kwargs = {'Change': lambda x: (x['High'] - x['Low']) / x['Low'],
             'close_off_high': lambda x: 2*(x['High'] - x['Close']) / (x['High'] - x['Low']) - 1,
             'volatility': lambda x: (x['High'] - x['Low']) / (x['Low'])}
  data = data.assign(**kwargs)
  return data

# add new columns which calculated in upper function
market_data_added = add_volatility(data)

# select some columns
market_data_final = market_data_added[['Close','Vol','Change','volatility']]

#leave last 5 days apart from modelling phase because they will be predicted later
market_data_final_2 = market_data_final[0:-5]

# prepare validation set to be predicted later
validation_set = market_data_final[len(market_data_final)-5:]


# convert series to supervised learning
def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
	n_vars = 1 if type(data) is list else data.shape[1]
	df = pd.DataFrame(data)
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
	agg = pd.concat(cols, axis=1)
	agg.columns = names
	# drop rows with NaN values
	if dropnan:
		agg.dropna(inplace=True)
	return agg



values = market_data_final_2.values

#encode and transform high volumed (bigint) colums(volume here)  
encoder = LabelEncoder()
values[:,1] = encoder.fit_transform(values[:,1])


# ensure all data is float
values = values.astype('float32')

# normalize features in range 0 and 1
scaler = MinMaxScaler(feature_range=(-1, 1))
scaled = scaler.fit_transform(values)



# frame as supervised learning
# take t-20 times forward to t+5 times
to_forward = 5
features = values.shape[1]
output_size = to_forward * features

# apply function
reframed = series_to_supervised(scaled, output_size, to_forward)
print(reframed.head())

#split it into train and test set
values = reframed.values
train = values[:int(values.shape[0]*0.90), :]
test = values[int(values.shape[0]*0.90):, :]



# prepare data for neural network
train_X, train_y = train[:, :train.shape[1]-output_size], train[:, -output_size:]
test_X, test_y = test[:, :test.shape[1]-output_size], test[:, -output_size:]

# reshape input to be 3D [samples, timesteps, features]
# reshaping into sample,timesteps and features is essential for deep learning mode
# here if we make a calculation 4*5 indicates 20 that we splitted data above for taking t-20 to t-1
train_X = train_X.reshape((train_X.shape[0], output_size, features))
test_X = test_X.reshape((test_X.shape[0], output_size, features))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)




# design network
# batch_size = 1 means that it is "online learning"
model = Sequential()
model.add(LSTM(256, input_shape=(train_X.shape[1], train_X.shape[2]),activation='tanh',return_sequences=False))
#model.add(LSTM(64,activation='tanh'))
model.add(Dropout(0.2))
model.add(Dense(output_size))
#model.add(Activation('linear'))
model.add(LeakyReLU())
model.compile(loss='mse', optimizer='adam')
# fit network
history = model.fit(train_X, train_y, epochs=5, batch_size=1, validation_data=(test_X, test_y), verbose=2, shuffle=False)
# plot history
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()




yhat = model.predict(test_X)

# invert scaling for forecast
# take every 4 variables on certain time ranges like 0 to 3 and 4 to 7 and so forth
# assign this variables to dictionary
# make inverse transform
t1=list()
t2=list()
dic = {}
for g in range(0,output_size,features):
    for j in range(features-1,output_size,features):
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
for k in range(0,output_size,features):
                listed.append(dic[k][:,0])                    

# make this values data frame
# this DF indicates the target variable that wanted to predicted as times in t,t+1,t+2,t+3,t+4
predicted = pd.DataFrame.from_records(listed)               
predicted = predicted.transpose()
                

# invert scaling for actual 
# same processing as above  for real values which in test_y dataset.remember test_y dataset includes variables from t to t+5
# we trained model from t-20 to t-1,and predicted 5 times ahead ,and then checked predicted results with these real ones in train_y set            
m1=list()
m2=list()
dic = {}
for g in range(0,output_size,features):
    for j in range(features-1,output_size,features):
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
for k in range(0,output_size,features):
                listed.append(dic[k][:,0])                    
            
real = pd.DataFrame.from_records(listed)               
real = real.transpose()                 

# calculate RMSE
rmse = sqrt(mean_squared_error(real, predicted))
print('Test RMSE: %.3f' % rmse)

#############################################
### saving model ###
# serialize model to JSON
model_json = model.to_json()
with open("model.json", "w") as json_file:
    json_file.write(model_json)
# serialize weights to HDF5
model.save_weights("model.h5")
print("Saved model to disk")
###

#for loading saved model
from keras.models import model_from_json
 
# load json and create model
json_file = open('model.json', 'r')
loaded_model_json = json_file.read()
json_file.close()
loaded_model = model_from_json(loaded_model_json)
# load weights into new model
loaded_model.load_weights("model.h5")
print("Loaded model from disk")

#############################################

## Predicting next 5 unseen days part ##

# same data operations with different variable names
# prepare data for predicting next 5 days
values = reframed.values
train_for_pred = values[:int(values.shape[0]*0.90), :]
test_for_pred = values[int(values.shape[0]*0.90):, :]



# split into input and outputs
# now train and test for pred has 100 columns like above trained model dataset
# Here is the magic,I shifted it with 35 times to make a predition
# the intitution behind time seris prediction is shifting data
# if you take whole data with 100 colums ,this data will not be predicted
# because in above you trained model with 20 time steps with 4 features
# to make data same , I shifted it or take it from colum 20 to 100 which reaches 80 columns data set and this number can be 
# computed by 20*4
# so we can predict 5 days forward this data set
# in this data our last value is 16.03.2018 so we want to predict further days
# in above trained dataset the last value is 11.03.2018 value,then we predicted 16.03.2018
# difference here I took  16.03.2018 as last value to predict unseen future
train_X_pred = train_for_pred[:, output_size:train_for_pred.shape[1]]
test_X_pred = test_for_pred[:, output_size:test_for_pred.shape[1]]

# reshape input to be 3D [samples, timesteps, features]
train_X_pred = train_X_pred.reshape((train_X_pred.shape[0], output_size, features))
test_X_pred = test_X_pred.reshape((test_X_pred.shape[0], output_size, features))
print(train_X_pred.shape,test_X_pred.shape)



#predict unseen data
yhat_pred = model.predict(test_X_pred)


# invert scaling for forecast
t1=list()
t2=list()
dic_pred = {}
for g in range(0,output_size,features):
    for j in range(features-1,output_size,features):
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

# select "Close" variable which wanted to be predicted           
listed_pred = list()                 
for k in range(0,output_size,features):
                listed_pred.append(dic_pred[k][:,0])                    

predicted_t_5 = pd.DataFrame.from_records(listed_pred)               
predicted_t_5 = predicted_t_5.transpose()

p = list()
for i in range(0,5):
    p.append(predicted_t_5.iloc[-1,i].astype('float32'))
       
f1 = pd.DataFrame(p)
final_prediction_values = f1.transpose()
final_prediction_values.columns = ['t','t+1','t+2','t+3','t+4']


# calculate RMSE
rmse = sqrt(mean_squared_error(validation_set['Close'], final_prediction_values.transpose()))
print('Test RMSE: %.3f' % rmse)

# calculate average prediction rate
validation_set.index.levels[0].name = "predict"
final_predicts = final_prediction_values.transpose()
final_predicts.columns = ['predictions']
final_predicts.index.name = "predict"
validation_set.reset_index(drop = True, inplace = True)
final_predicts.reset_index(drop = True, inplace = True)
print(np.nanmean(validation_set.iloc[:,0]/final_predicts.iloc[:,0]))



















