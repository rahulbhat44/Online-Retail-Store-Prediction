#Load Libraries
import pandas as pd
import pandas
from csv import reader
import graphlab as gl
import datetime
from dateutil import parser as datetime_parser

#Load Data Set
retail = pandas.read_csv('/Users/Bhat/Downloads/UKretail.csv', delimiter=',')

#Convert the datetime strings to Python datetimes and create a GraphLab Create TimeSeries from the InvoiceDate column.
# Convert InvoiceDate strings (e.g. "12/1/10 8:26") to datetimes
reatil['InvoiceDate'] = retail['InvoiceDate'].apply(datetime_parser.parse)

# Create a TimeSeries
timeseries = gl.TimeSeries(retail, 'InvoiceDate')

#Train Churn Predictor Model
# Split the data into train and testing data set
train, valid = gl.churn_predictor.random_split(timeseries, user_id='CustomerID', fraction=0.8, seed = 1)

# Train the model using data before August
churn_boundary_oct = datetime.datetime(year = 2011, month = 8, day = 1)

# Define churn as "inactive for 30 days after August 1st 2011"
churn_period = datetime.timedelta(days = 30)

model = gl.churn_predictor.create(train, user_id='CustomerID',
                                  features = ['Quantity'],
                                  churn_period = churn_period,
                                  time_boundaries = [churn_boundary_oct])

#Explore and Evaluate the Model

# Interactively explore churn predictions
view = model.views.overview(exploration_set=timeseries,
                            validation_set=valid,
                            exploration_time=churn_boundary_oct,
                            validation_time=churn_boundary_oct)
view.show()
