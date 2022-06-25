#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 26 00:38:00 2021

@author: arulahuja
"""

from flask import Flask, render_template, request, redirect, url_for
from shapely.geometry import shape, Point
import json
from namedict import final
from difflib import SequenceMatcher



with open('Delhi_Wards.geojson') as f:
    js = json.load(f)

def similar(a, b):
    return SequenceMatcher(None, a, b).ratio()

def WardFind(lt, ln):
    for feature in js['features']:
        polygon = shape(feature['geometry'])
        point = Point(ln, lt)
        if polygon.contains(point):
            for k, v in final.items():
                if similar(feature['properties']['Ward_Name'], k)>=0.9:
                    returned = True
                    return(feature['properties']['Ward_Name'] + " " + v)
                    
                else:
                    pass
            return(feature['properties']['Ward_Name'])


app = Flask(__name__)


@app.route('/')
def index1():
    return render_template('index.html')


@app.route('/', methods =["GET", "POST"])
def index():
    if request.method == "POST":
        lat = float(request.form.get("lat"))
        lon = float(request.form.get("lon"))
        ward = WardFind(lat, lon)
        # return ward

    return render_template("index.html", result = ward)

# @app.route('/result')
# def result():
#     return ward
    
    
if __name__ == "__main__":
    app.run(debug=True)
