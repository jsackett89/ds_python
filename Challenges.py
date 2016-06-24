# -*- coding: utf-8 -*-
"""
Created on Thu Apr 28 18:54:11 2016

@author: sackettj
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Max profit in a day

## Inititalize stock price list
stock_price_yest = np.random.randint(40, 65, 10)

def maxProfit(stockPrices):
    # Create empty list to store "max of maxes"
    l_1 = []
    # Iterate over each time index in the stock price list
    for i in range(0, len(stockPrices)):
        # Create an empty sublist for each time index in the stock price list
        l_2 = []
        x=0
        # Append the profits for each time up to the current time
        while x < i+1:
            l_2.append(stockPrices[i] - stockPrices[x])
            x += 1
        # Append the max of the sublist to the "max of maxes" list
        l_1.append(max(l_2))
    # Return the max of maxes
    return max(l_1)

# Test         
maxProfit(stock_price_yest)   

# Product of all other elements of a list except the current
n = np.random.randint(1, 11, 5)

def prodOthers(nums):
    l = []
    for x in range(len(nums)):
        nums_reduced = list(nums[:x]) + list(nums[x+1:])
        l.append(np.product(nums_reduced))
    return l
    
# Test
prodOthers(n)

#  Highest product of any three integers from a list
import numpy as np
import itertools

n = np.random.randint(-10, 10, 5)

## First approach
def highProd1(nums, k):
    combos = [x for x in itertools.combinations(nums, k)]
    combos = map(lambda x: np.product(x), combos)
    return max(combos)

## Second approach
def highProd2(nums, k):
    return max([np.product(x) for x in list(itertools.combinations(nums, k))])
    
## Test
highProd1(n, 3)
highProd2(n, 4)

# Pairs of integers that sum to 10
combos = itertools.combinations(n, 2)

def sum_to_ten(nums):
    combos = itertools.combinations(nums, 2)
    l = []
    for i in combos:
        if sum(list(i)) == 10:
            l.append(list(i))
    return l
    
## Test            
sum_to_ten(n)   

# Print out binary form of number
from math import log

def binary(k):
    l = []
    if k == 0:
        l.append('0')
        print(l)
    else:
        x = k
        y = int(log(x, 2))
        while y >= 0:
            if 2 ** y <= x:
                l.append('1')
                x = x - 2**y
                y -= 1
            else:
                l.append('0')
                y -= 1
    return ''.join(l)

binary(100)

# Fibonacci numbers
def fibo(k):
    l = [1, 1]
    for i in range(2, k):
        l.append(l[i-1] + l[i-2])
    return l[k-1]

## Test     
fibo(6)

# Anagrams
from collections import Counter
import numpy as np
import string

s1 = "Lies--Let's Recount!"
s2 = "Election Results"

def anagram(s1, s2):
    s1 = s1.lower()
    s2 = s2.lower()
    s1 = [s for s in s1 if s not in string.punctuation and s not in string.whitespace]
    s2 = [s for s in s2 if s not in string.punctuation and s not in string.whitespace]
    freq1 = Counter(s1)
    freq2 = Counter(s2)
    if freq1 == freq2:
        print("It's an anagram!")
    else:
        print("It's not an anagram.")
 
## Test
anagram(s1, s2)

# Intersection of two rectangles
import numpy as np

my_rectangle1 = {
    'left_x': 1, 
    'bottom_y': 7, 
    'width': 4, 
    'height': 4}
    
my_rectangle2 = {
    'left_x': 3, 
    'bottom_y': 1, 
    'width': 4, 
    'height': 7}
    
def overlap(rect1, rect2):
    rect_intersect = {}
    a1 = rect1['left_x']
    b1 = rect1['left_x'] + rect1['width']
    a2 = rect2['left_x']
    b2 = rect2['left_x'] + rect2['width']
    c1 = rect1['bottom_y']
    d1 = rect1['bottom_y'] + rect1['height']
    c2 = rect2['bottom_y']
    d2 = rect2['bottom_y'] + rect2['height']
    if b1 > a2 and b2 > a1 and d1 > c2 and d2 > c1:
        rect_intersect['left_x'] = max(a1, a2)
        rect_intersect['width'] = min(b1 - max(a1, a2), b2 - max(a1, a2))
        rect_intersect['bottom_y'] = max(c1, c2)
        rect_intersect['height'] = min(d1 - max(c1, c2), d2 - max(c1, c2))
        return rect_intersect
    else:
        print('There is no intersection between these rectangles.')
        
## Test
overlap(my_rectangle1, my_rectangle2)
