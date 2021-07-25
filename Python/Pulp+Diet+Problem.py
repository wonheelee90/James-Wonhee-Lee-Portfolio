
# coding: utf-8

# In[1]:

from pulp import *
import pandas as pd


# In[2]:

diet = pd.read_excel("/Users/Wonhee/Desktop/Georgia Tech/Fall 2017/ISYE 6501 Intro to Analytics Modeling/Homework/Homework 11/diet.xls")
diet = diet.drop(diet.index[[64,65,66]])


# In[3]:

foods = [ i for i in diet["Foods"]]
cost = dict(zip(foods,diet["Price/ Serving"]))
calories = dict(zip(foods,diet["Calories"]))
cholesterol = dict(zip(foods,diet["Cholesterol mg"]))
fat = dict(zip(foods,diet["Total_Fat g"]))
sodium = dict(zip(foods,diet["Sodium mg"]))
carbon = dict(zip(foods,diet["Carbohydrates g"]))
fiber = dict(zip(foods,diet["Dietary_Fiber g"]))
protein = dict(zip(foods,diet["Protein g"]))
vita = dict(zip(foods,diet["Vit_A IU"]))
vitc = dict(zip(foods,diet["Vit_C IU"]))
calcium = dict(zip(foods,diet["Calcium mg"]))
iron = dict(zip(foods,diet["Iron mg"]))


# In[4]:

prob = LpProblem("Diet Problem", LpMinimize)

#variables 
foods_vars = LpVariable.dicts("foods",foods,0)

#objective funcction
prob += lpSum([cost[i]*foods_vars[i] for i in foods])

#constraint for the min & max daily intake
prob += lpSum([calories[i] * foods_vars[i] for i in foods]) >= 1500
prob += lpSum([calories[i] * foods_vars[i] for i in foods]) <= 2500
prob += lpSum([cholesterol[i] * foods_vars[i] for i in foods]) >= 30
prob += lpSum([cholesterol[i] * foods_vars[i] for i in foods]) <= 240
prob += lpSum([fat[i] * foods_vars[i] for i in foods]) >= 20 
prob += lpSum([fat[i] * foods_vars[i] for i in foods]) <= 70 
prob += lpSum([sodium[i] * foods_vars[i] for i in foods]) >= 800
prob += lpSum([sodium[i] * foods_vars[i] for i in foods]) <= 2000
prob += lpSum([carbon[i] * foods_vars[i] for i in foods]) >= 130
prob += lpSum([carbon[i] * foods_vars[i] for i in foods]) <= 450
prob += lpSum([fiber[i] * foods_vars[i] for i in foods]) >= 125
prob += lpSum([fiber[i] * foods_vars[i] for i in foods]) <= 250
prob += lpSum([protein[i] * foods_vars[i] for i in foods]) >= 60
prob += lpSum([protein[i] * foods_vars[i] for i in foods]) <= 100
prob += lpSum([vita[i] * foods_vars[i] for i in foods]) >= 1000
prob += lpSum([vita[i] * foods_vars[i] for i in foods]) <= 10000
prob += lpSum([vitc[i] * foods_vars[i] for i in foods]) >= 400
prob += lpSum([vitc[i] * foods_vars[i] for i in foods]) <= 5000
prob += lpSum([calcium[i] * foods_vars[i] for i in foods]) >= 700
prob += lpSum([calcium[i] * foods_vars[i] for i in foods]) <= 1500
prob += lpSum([iron[i] * foods_vars[i] for i in foods]) >= 10
prob += lpSum([iron[i] * foods_vars[i] for i in foods]) <= 40

prob.solve()

#results
for v in prob.variables():
    if v.varValue > 0.0:
        print(v.name, "=", v.varValue)
print("Total Cost of food = ", value(prob.objective))


# In[5]:

prob = LpProblem("Diet Problem", LpMinimize)

#Variable for the amount of the food and Binary Vairable where it's chosen or not
foods_vars = LpVariable.dicts("foods",foods,0)
foods_bi = pulp.LpVariable.dicts('binary', foods, 
                            lowBound = 0,
                            upBound = 1,
                            cat = "Binary")

#objective funcction
prob += lpSum( [foods_vars[i]*cost[i] for i in foods ])

#constraint for the min & max daily intake
prob += lpSum([calories[i] * foods_vars[i] for i in foods]) >= 1500
prob += lpSum([calories[i] * foods_vars[i] for i in foods]) <= 2500
prob += lpSum([cholesterol[i] * foods_vars[i] for i in foods]) >= 30
prob += lpSum([cholesterol[i] * foods_vars[i] for i in foods]) <= 240
prob += lpSum([fat[i] * foods_vars[i] for i in foods]) >= 20 
prob += lpSum([fat[i] * foods_vars[i] for i in foods]) <= 70 
prob += lpSum([sodium[i] * foods_vars[i] for i in foods]) >= 800
prob += lpSum([sodium[i] * foods_vars[i] for i in foods]) <= 2000
prob += lpSum([carbon[i] * foods_vars[i] for i in foods]) >= 130
prob += lpSum([carbon[i] * foods_vars[i] for i in foods]) <= 450
prob += lpSum([fiber[i] * foods_vars[i] for i in foods]) >= 125
prob += lpSum([fiber[i] * foods_vars[i] for i in foods]) <= 250
prob += lpSum([protein[i] * foods_vars[i] for i in foods]) >= 60
prob += lpSum([protein[i] * foods_vars[i] for i in foods]) <= 100
prob += lpSum([vita[i] * foods_vars[i] for i in foods]) >= 1000
prob += lpSum([vita[i] * foods_vars[i] for i in foods]) <= 10000
prob += lpSum([vitc[i] * foods_vars[i] for i in foods]) >= 400
prob += lpSum([vitc[i] * foods_vars[i] for i in foods]) <= 5000
prob += lpSum([calcium[i] * foods_vars[i] for i in foods]) >= 700
prob += lpSum([calcium[i] * foods_vars[i] for i in foods]) <= 1500
prob += lpSum([iron[i] * foods_vars[i] for i in foods]) >= 10
prob += lpSum([iron[i] * foods_vars[i] for i in foods]) <= 40

#constrain for minimum serving
for i in foods:
    prob += foods_vars[i] >= (0.1 * foods_bi[i])

#to make sure that when foods_bi==0, foods_vars==0 also
for i in foods:
    prob += foods_vars[i] <= foods_bi[i]*2500

#contraint for one of brocccoli or celery
prob += (foods_bi['Frozen Broccoli'] + foods_bi['Celery, Raw']) <= 1

#constraint for the variety in protein
prob += (foods_bi['Bologna,Turkey'] + foods_bi['Roasted Chicken'] + foods_bi['Frankfurter, Beef'] + foods_bi['Poached Eggs'] + foods_bi['Pork'] + foods_bi['Scrambled Eggs'] + foods_bi['White Tuna in Water'] + foods_bi['Sardines in Oil'] ) >= 3


prob.solve()

#results
for v in prob.variables():
    if v.varValue > 0.0:
        print(v.name, "=", v.varValue)
print("Total Cost of food = ", value(prob.objective))


# In[ ]:



