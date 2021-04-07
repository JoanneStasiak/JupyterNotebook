#!/usr/bin/env python
# coding: utf-8

# # Certainty Task
# 
# ### First clean the data
# 
# Use the following blocks to separate out data lists from certainty task. First, import the packages & modules we need and change directory

# In[186]:


import csv
import os
import pandas as pd
import re
import numpy as np
from matplotlib import pyplot
import matplotlib.pyplot as plt
import matplotlib.animation as ani
from matplotlib.animation import FuncAnimation
from pandas.plotting import autocorrelation_plot
import matplotlib.animation as animation
from matplotlib import style
get_ipython().run_line_magic('matplotlib', 'inline')

os.chdir("F://Certainty Study//data")


# Open the data file we need to clean, then find all of the lists 

# In[183]:


with open('data1.csv', newline='') as f:
    reader = csv.reader(f)
    data = list(reader)

    
flat=[]
dataLists = []

for i in data:
    for j in i:
        flat.append(j)

for i in flat:
    if re.search("\['*", i):
        dataLists.append(i)
        
dataLists


# Now we have a giant list of 'lists' (they're not technically lists yet), but we want to make them into their individual lists.
# 
# When we check the type of each individual object, we see that they are strings rather than lists. 

# In[184]:


joyPos = dataLists[0]
joyBeg = dataLists[1]
certRat= dataLists[2]
innRat=dataLists[3]
rt=dataLists[4]
guiltyRat=dataLists[5]
frames = dataLists[6]


type(joyPos)


# Because these elements are now strings, we have to strip certain special characters away so we're left with plain objects 

# In[185]:


bad_chars = ["[", "]", "'"]
lists = [joyPos, joyBeg, certRat, innRat, rt, guiltyRat, frames]

        
for i in bad_chars:
    joyPos = joyPos.replace(i,'')
    joyBeg = joyBeg.replace(i,'')
    certRat = certRat.replace(i,'')
    innRat = innRat.replace(i,'')
    rt = rt.replace(i,'')
    guiltyRat = guiltyRat.replace(i,'')
    frames = frames.replace(i,'')


# In[173]:


joyPos


# Turn them into lists, use ',' as delimiter.
# 
# Now when we check the type, we can see they are lists

# In[174]:



print(type(joyPos))

joyPos = joyPos.split(",")
joyBeg = joyBeg.split(",")
certRat = certRat.split(",")
innRat = innRat.split(",")
rt = rt.split(",")
guiltyRat = guiltyRat.split(",")
frames = frames.split(",")

print(type(joyPos))


# Finally, let's transform these lists back into a usable dataframe. First create a dictionary, then save to csv

# In[175]:


lists2 = [joyPos,joyBeg, certRat, innRat, rt, guiltyRat, frames]

# for i in lists2:
#     i2 = []
#     for x in i:
#         k = float(x)
#         i2.append(k)
        
        

joyPos1 = []
joyBeg1 = []
certRat1 = []
innRat1 = []
rt1 = []
guiltyRat1 = []
frames1 = []

for i in joyPos:
    k = float(i)
    joyPos1.append(k)
for i in joyBeg:
    k = float(i)
    joyBeg1.append(k)
for i in certRat:
    k = float(i)
    certRat1.append(k)
for i in innRat:
    k = float(i)
    innRat1.append(k)
for i in rt:
    k = float(i)
    rt1.append(k)
for i in guiltyRat:
    k = float(i)
    guiltyRat1.append(k)
for i in frames:
    k = float(i)
    frames1.append(k)
print("joyPos1 = ", joyPos1)
print("joyBeg1 = ", joyBeg1)


# In[176]:


dict = {'Joystick Position': joyPos1, 'Joystick Beginning': joyBeg1, 'Certainty Ratings': certRat1, 'InnocentRatings': innRat1, 'RT': rt1, 'GuiltyRatings': guiltyRat1, 'Frames': frames1}  
df = pd.DataFrame.from_dict(dict, orient='index')
df = df.transpose()
df.index.name = "Index"
df.to_csv("CleanData2.csv")


# In[187]:


get_ipython().run_line_magic('matplotlib', 'inline')
certDF = pd.read_csv("CleanData2.csv")
index = certDF["Index"]
certainty = certDF["Certainty Ratings"]
plt.plot(index, certainty)
# plt.xscale('log') 

plt.xlabel("Frames")
plt.ylabel("Certainty")
tick_val = [-60, -40, -20, 0, 20, 40]
# tick_lab = ['Guilty', '10k', '100k']
plt.axhline(y = 0, color = 'r', linestyle = '-')

# Adapt the ticks on the x-axis
plt.yticks(tick_val)
plt.show()


# In[188]:


get_ipython().run_line_magic('matplotlib', 'nbagg')

style.use("default")
x = certDF["Index"]
x = x.tolist()
y=certDF["Certainty Ratings"]
y = y.tolist()

fig, ax = plt.subplots()
plt.axhline(y = 0, color = 'r', linestyle = '-')
plt.xlabel("Video Progression")
plt.ylabel("Certainty")
line, = ax.plot(x, y, color='k')

def update(num, x, y, line):
    line.set_data(x[:num], y[:num])
    line.axes.axis([0, 250, -60, 40])
    return line,

ani = FuncAnimation(fig, update, len(x), fargs=[x, y, line],
                              interval=25, blit=True)
ani.save('certainty.gif')
plt.show()


# In[ ]:




