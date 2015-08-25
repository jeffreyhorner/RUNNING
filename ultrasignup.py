import re
import requests
import pandas
from pandas.io.json import json_normalize
import json
from urllib import parse
import numpy as np
from bs4 import BeautifulSoup


_us_root_url = "http://ultrasignup.com"


def _collect_event_pages(event="GetFeaturedEventsSearch",q=""):
  us_url = _us_root_url + "/service/events.svc/%s/p=%d/q=%s"
  page = 0
  df = pandas.DataFrame()
  while True:
    js_data = json.loads(
      requests.get(us_url % (event,page,parse.quote_plus(q))).text)
    page += 1
    if len(js_data) != 0:
      df = pandas.concat([df,json_normalize(js_data)])
    else:
      break
  return df


def search_events(q="StumpJump"):
  return _collect_event_pages(event="GetFeaturedEventsSearch",q=q)


def search_runners(q="Horner, Jeffrey"):
  return _collect_event_pages(event="GetParticipantSearch",q=q)


def _collect_event_results(distance=None,year=None,result_id=None):
    res_url = _us_root_url + \
      "/service/events.svc/results/%s/json?_search=false"
    js_data = json.loads(
      requests.get(res_url % (result_id)).text)
    df = json_normalize(js_data)
    #
    # Data conversions
    # 
    df['time'] = df['time'].astype(np.float64) / 1000
    #
    # Data cleaning
    df['age'][df['age'] == 0] = None
    df['gender'][df['gender'] == 'f'] = 'F'
    #
    # New columns
    #
    df['time_hour'] = df['time'] / 60 / 60
    df['year'] = int(year)
    df['distance'] = distance
    return df


def event_results(eid=None):
  if eid is None:
    raise ValueError("Must provide an eid argument")
  response = requests.get(_us_root_url + "/register.aspx?eid=%d" % (eid))
  #
  # results_df: DataFrame of results
  #
  results_df = pandas.DataFrame()
  #
  # reg_bs: beautiful soup object of registration page. contains links on
  #         all years the event was contested
  #
  reg_bs = BeautifulSoup(response.text)
  if len(re.findall(r'Object moved',reg_bs.find('title').text)):
    response = requests.get(_us_root_url + reg_bs.find('a').attrs['href'])
    reg_bs = BeautifulSoup(response.text)
  #
  event_name = reg_bs.find('h1',attrs={'class':'event-title'}).string
  #
  #  years_rs: result set of all a tags that contain urls to results by year 
  #
  years_rs = reg_bs.findAll(
    'table',id='ContentPlaceHolder1_Results11_dlResultYears')[0].findAll('a')
  #
  # Gather all distances contested by scraping first element of years_rs
  #
  dist_bs = BeautifulSoup(
    requests.get(_us_root_url + years_rs[0].attrs['href']).text)
  #
  # distance_rs: result set of a tags containing urls to distance specific 
  #              results
  #
  distance_rs = dist_bs.findAll(
    'div',attrs={'class':'unit-1 text-right'})[0].findAll('a')
  #
  # Loop over each distance and gather results
  #
  for i in distance_rs:
    distance = i.text
    dist_bs = BeautifulSoup(
      requests.get(_us_root_url + i.attrs['href']).text)
    years_rs = dist_bs.findAll('table',id='ContentPlaceHolder1_dlYears')[0]
    for j in years_rs.findAll('a'):
      year = j.text
      result_id = re.sub(r'GetResults\((\d+),\d+\)','\\1',j.attrs['onclick'])
      results_df = pandas.concat(
        [results_df,_collect_event_results(distance,year,result_id)])
  #
  #
  #
  return results_df
