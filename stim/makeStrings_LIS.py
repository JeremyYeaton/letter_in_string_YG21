# -*- coding: utf-8 -*-
"""
Stimulus generation for experiments on letter-in-string extraction
(C) Jeremy D Yeaton
February 2021
"""

import numpy as np
import matplotlib.pyplot as plt
#%% Create experimental stim strings
np.random.seed(3)

letters = 'RTPSDFGHKLCBNM'
# Exclude: J,Q,W,Y,Z,X,V; E,U,I,O,A

letters = [l for l in letters]
nLetters = len(letters)

strLen = 5
nConditions = 1
nTrials = 20*nConditions*strLen

nPermute = round((nTrials*strLen)/len(letters))
letters = letters * nPermute

allTrials = []
k = 0

def getIdxs(n):
    IdxCheck = False
    while IdxCheck == False:
        Idxs = np.random.randint(0,len(letters),n)
        Idxs = [j for j in Idxs]
        IdxSum = 0
        for Idx in Idxs:
            IdxSum += Idxs.count(Idx)
        if IdxSum == 5:
            IdxCheck = True
    return Idxs

def visLetFreq(allTrials):
    # Visualize co-occurrence of letters
    dLet = {'B':0,'C':1,'D':2,'F':3,'G':4,'H':5,'K':6,'L':7,'M':8,'N':9,'P':10,'R':11,'S':12,'T':13}
    counter = np.zeros((nLetters,nLetters))
    minC, maxC = 500,0
    for s in allTrials:
        for l in s:
            Idx1 = dLet[l]
            for l in s:
                Idx2 = dLet[l]
                counter[Idx1,Idx2] += 1
    diag = [counter[i,i] for i in range(nLetters)]
    # print(np.max(diag)-np.min(diag))
    print(sum(diag))
    # print(maxC,minC)
    for i in range(nLetters):
        for j in range(nLetters):
            if counter[i,j] > 0 and counter[i,j] < minC:
                minC = counter[i,j]
            if counter[i,j] > maxC and counter[i,j] not in diag:
                maxC = counter[i,j]
    for i in range(nLetters):
        counter[i,i+1:]=0
    plt.figure()
    plt.imshow(counter,vmin = minC, vmax = maxC)

while len(allTrials) < nTrials:
    bad = False
    Idxs = getIdxs(5)
    S = [l for j,l in enumerate(letters) if j in Idxs]
    for l in S:
        if S.count(l) > 1:
            bad = True
    if S not in allTrials and bad == False:
        allTrials.append(''.join(S))
        letters = [l for j,l in enumerate(letters) if j not in Idxs]
    else:
        k += 1
print(k)  
visLetFreq(allTrials)
#%% Save stim
f = open('LIS_strings.csv','w')
for string in allTrials:
    f.write(string)
    f.write(',')
f.close()

#%% Create practice strings
strLen = 5
nTrials = 10
letters = 'RTPSDFGHKLCBNM'
letters = [l for l in letters]

nPermute = round((nTrials*strLen)/len(letters))
letters = letters * nPermute

pracTrials = []
k = 0

while len(pracTrials) < nTrials:
    bad = False
    Idxs = getIdxs(5)
    S = [l for j,l in enumerate(letters) if j in Idxs]
    for l in S:
        if S.count(l) > 1:
            bad = True
    if S not in allTrials and S not in pracTrials and bad == False:
        pracTrials.append(''.join(S))
        letters = [l for j,l in enumerate(letters) if j not in Idxs]
    else:
        k += 1
print(k) 
#%% Save practice stim
f = open('LIS_strings_practice.csv','w')
for string in pracTrials:
    f.write(string)
    f.write(',')
f.close()

#%% Generate position assignments for variable position trials
stringPos = ['left','right','center'] * 34

np.random.shuffle(stringPos)

f = open('stringPos_fix.csv','w')
f.write(','.join(['center']*100))
f.close()

f = open('stringPos_var.csv','w')
f.write(','.join(stringPos[:100]))
f.close()