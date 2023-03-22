#python


cd /Users/danjack/Documents/BirdSurveys_CAPLTER/

# first, i opened the csv in excel and split the "date" column into three columns: day, month, year.

# I also added on the observations from Gammage, and observations from August
python

import pandas as pd
import scipy
from scipy import stats

df = pd.read_csv('knb-lter-cap.642.3/642_pass_birds_5a0e0f47a075c0b002cca9b441ae991b.csv')

# Remove  sites p16 and U16 due to a lack of associated PASS data

df = df[df.site_code != 'p16']
df = df[df.site_code != 'U16']
df = df[df.site_code != 'U16']

# add label to each row that designates year and season [2007-2008, 2012-2013]
# include 2005 august samples from ESCA for everything except the RDA

filter_list_2008_dec = ['2007']
year_2008_dec = df[df['year'].isin(filter_list_2008_dec)]

filter_list_winter_dec = ['12']
winter_2008_dec = year_2008_dec[year_2008_dec['month'].isin(filter_list_winter_dec)]

filter_list_2008 = ['2008']
year_2008 = df[df['year'].isin(filter_list_2008)]

filter_list_winter = ['1']
winter_2008 = year_2008[year_2008['month'].isin(filter_list_winter)]

filter_list_spring = ['3', '4', '5']
spring_2008 = year_2008[year_2008['month'].isin(filter_list_spring)]




filter_list_2012_dec = ['2011']
year_2012_dec = df[df['year'].isin(filter_list_2012_dec)]

filter_list_winter_dec = ['12']
winter_2012_dec = year_2012_dec[year_2012_dec['month'].isin(filter_list_winter_dec)]

filter_list_2012 = ['2012']
year_2012 = df[df['year'].isin(filter_list_2012)]

filter_list_winter = ['1']
winter_2012 = year_2012[year_2012['month'].isin(filter_list_winter)]

filter_list_spring = ['3', '4', '5']
spring_2012 = year_2012[year_2012['month'].isin(filter_list_spring)]



filter_list_2019_dec = ['2018']
year_2019_dec = df[df['year'].isin(filter_list_2019_dec)]

filter_list_winter_dec = ['12']
winter_2019_dec = year_2019_dec[year_2019_dec['month'].isin(filter_list_winter_dec)]

filter_list_2019 = ['2019']
year_2019 = df[df['year'].isin(filter_list_2019)]

filter_list_winter = ['1']
winter_2019 = year_2019[year_2019['month'].isin(filter_list_winter)]

filter_list_spring = ['3', '4', '5']
spring_2019 = year_2019[year_2019['month'].isin(filter_list_spring)]






filter_list_Gammage = ['Gammage_A' , 'Gammage_B', 'Gammage_C']
gammage = df[df['site_code'].isin(filter_list_Gammage)]

filter_list_august = ['8']
august = df[df['month'].isin(filter_list_august)]

winter_2008_dec['site_code'] = 'wi_08_' + winter_2008_dec['site_code'].astype(str)
winter_2008['site_code'] = 'wi_08_' + winter_2008['site_code'].astype(str)
spring_2008['site_code'] = 'sp_08_' + spring_2008['site_code'].astype(str)
winter_2012['site_code'] = 'wi_12_' + winter_2012['site_code'].astype(str)
winter_2012_dec['site_code'] = 'wi_12_' + winter_2012_dec['site_code'].astype(str)
spring_2012['site_code'] = 'sp_12_' + spring_2012['site_code'].astype(str)
winter_2019['site_code'] = 'wi_19_' + winter_2019['site_code'].astype(str)
winter_2019_dec['site_code'] = 'wi_19_' + winter_2019_dec['site_code'].astype(str)
spring_2019['site_code'] = 'sp_19_' + spring_2019['site_code'].astype(str)




df = pd.concat([winter_2008_dec, winter_2008, spring_2008, winter_2012, winter_2012_dec, spring_2012, gammage, august, winter_2019, winter_2019_dec, spring_2019])

# remove all riparian site_type observations from birds dataframe
env = pd.read_csv('sitetype.csv')

filter_list_sitetype = ['riparian']
riparian = env[env['site_type'].isin(filter_list_sitetype)]

filter_list_riparian = riparian['site']
df = df[~df['site_code'].isin(filter_list_riparian)]

# I need to make a dataframe where each unique 'site_code' is a line, and each column is the unique values in 'code'

sites = df.site_code.unique()
birds = df.code.unique()

# first, create df with same site_code column but with also columns of each "code" value and associated "bird_count" total

df2 = df.groupby(by=['site_code', 'code'], as_index=False)['bird_count'].sum()

df3 = df.groupby(by=['site_code', 'code'], as_index=False)['bird_count'].mean()


df3 = pd.pivot(df2, index='site_code', columns='code', values='bird_count').fillna(0).astype(int)

df3.to_csv("BirdSurveys_pivoted.csv")
