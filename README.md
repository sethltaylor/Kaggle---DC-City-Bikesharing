# DC City Bikeshare Analysis and Prediction

## Overview
This project examines data from DC's Capital Bikeshare program to determine trends in the data and to attempt to predict bikeshare usage. 

## Data Used
The data used is a combination of 2011 and 2012 data for Capital Bikeshare along with collected weather and seasonal information. This data is taken from Kaggle's [Bike Sharing in Washington D.C. Dataset](https://www.kaggle.com/marklvl/bike-sharing-dataset). The data has been cleaned previously, so minimal processing is neccessary. 

### Dataset Characteristics 
I use the hour.csv file for this analysis which contains the following variables:
- instant: record index
- dteday : date
- season : season (1:spring, 2:summer, 3:fall, 4:winter)
- yr : year (0: 2011, 1:2012)
- mnth : month ( 1 to 12)
- hr : hour (0 to 23)
- holiday : weather day is holiday or not (extracted from http://dchr.dc.gov/page/holiday-schedule)
- weekday : day of the week
- workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
+ weathersit : 
   - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
	- 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
	- 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
	- 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
- temp : Normalized temperature in Celsius. The values are divided to 41 (max)
- atemp: Normalized feeling temperature in Celsius. The values are divided to 50 (max)
- hum: Normalized humidity. The values are divided to 100 (max)
- windspeed: Normalized wind speed. The values are divided to 67 (max)
- casual: count of casual users
- registered: count of registered users
- cnt: count of total rental bikes including both casual and registered
	
## Progress Log
- Initialize repo and upload data 09/15/18
- Commit .rmd and .md displaying early EDA and model 09/21/18
