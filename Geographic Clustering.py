# -*- coding: utf-8 -*-
"""
Created on Mon Feb 15 17:46:15 2016

@author: sackettj
"""

# Import necessary packages
from math import radians, cos, sin, asin, sqrt
from pandas import read_csv
import numpy as np
from itertools import combinations
import matplotlib.pyplot as plt

# Read in location csv
latlong = read_csv('Location_data.csv')

# Group csv by identifier
latlong_grouped = latlong.groupby('Id')

# Calculate centroid location of each object
centroid = latlong_grouped.aggregate(np.mean)

# Use haversine formula to calculate lateral distance between any two objects
def lateral_distance(a, b):
    lat1 = centroid.ix[a, 'Latitude']
    long1 = centroid.ix[a, 'Longitude']
    lat2 = centroid.ix[b, 'Latitude']
    long2 = centroid.ix[b, 'Longitude']
    lat1, long1, lat2, long2 = map(radians, [lat1, long1, lat2, long2])
    dlat = lat2 - lat1
    dlong = long2 - long1
    h = sin(dlat/2)**2 + cos(lat1)*cos(lat2)*sin(dlong/2)**2
    km = 12742 * asin(sqrt(h))
    return(km)

# Calculate absolute value of vertical distance between any two objects
def vertical_distance(a, b):
    z1 = centroid.ix[a, 'Elevation']
    z2 = centroid.ix[b, 'Elevation']
    vert = abs(z1 - z2)
    return(vert)  

# Creating distance matrix
## Create empty matrix
distance_matrix = np.zeros((len(centroid), len(centroid)))

## Create iteration counter -- all 2-value combinations in dimensional range without repetition
iterator = combinations(range(len(centroid)), 2)

## Fill in matrix with squared distances
for tup in iterator:
    distance_matrix[tup[0], tup[1]] = lateral_distance(tup[0], tup[1]) ** 2
    distance_matrix[tup[1], tup[0]] = lateral_distance(tup[0], tup[1]) ** 2
    
## Visualize unclustered data
plt.scatter(centroid.Latitude, centroid.Longitude)
plt.show()

## Initialize 'Cluster' variable
centroid['Cluster'] = 0
    
## K-medoids clustering algorithm   

def kMedoids(D, k, tmax):
    # Define shape of distance matrix    
    m, n = D.shape    
    # Randomly initialize array of k medoid indices
    global M    
    M = np.sort(np.random.choice(n, k))
    # Create a copy of the above array
    Mcopy = np.copy(M)
    # Initialize an empty dictionary for clusters
    global C    
    C = {}
    # K-medoid clustering algorithm
    for num in range(tmax):
        J = np.argmin(D[:,M], axis = 1)
        for kappa in range(k):
            C[kappa] = np.where(J==kappa)[0]
        for kappa in range(k):
            J = np.mean(D[np.ix_(C[kappa], C[kappa])], axis = 1)
            j = np.argmin(J)
            Mcopy[kappa] = C[kappa][j]
        np.sort(Mcopy)
        
        if np.array_equal(M, Mcopy):
            break
        M = np.copy(Mcopy)
    else:
        J = np.argmin(D[:,M], axis = 1)
        for kappa in range(k): 
            C[kappa] = np.where(J==kappa)[0]   
    return M, C

## Assign clusters
for key, value in C.items():
    centroid.ix[value, 'Cluster'] = key
        
# Revisualize plot
plt.scatter(centroid.Latitude, centroid.Longitude, c = centroid.Cluster)
plt.show()   