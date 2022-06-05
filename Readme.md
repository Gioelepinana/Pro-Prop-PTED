# Proposal for Semester Project

**Patterns & Trends in Environmental Data / Computational Movement
Analysis Geo 880**

| Semester:      | FS22                                |
|----------------|-------------------------------------|
| **Data:**      | Wild Boar Movement Data             |
| **Title:**     | Movement analysis of wild boar data |
| **Student 1:** | Gioele Pinana                       |
| **Student 2:** | Mathujah Manikkan                   |

## Abstracst 
<!-- (50-60 words) --> 
Over the past few years, the population of wild boars in Switzerland has increased rapidly. As the number of wild boars increases, so does the damage to agriculture, as they seek for food in the fields. To counteract this damage, new preventive measures such scare-off measures have been developed with the aim of preventing the wild boars from entering the fields. Within this semester project...

A: ...the effectiveness of scare-off measures is analyzed. 
B: ...the impact of hiking trails is analyzed. 

## Research Questions
<!-- (50-60 words) -->
* How can the effectiveness of scare-off measures on wild boar be measured?  
* Which impact do hiking trails (disturbance element) have on the movement patterns of wild boars?  (Territory use / trail density) Influence with Parameters such as topology, land cover use, etc.) 

## Results / products
<!-- What do you expect, anticipate? -->
A: Maps
B: If we compare two wild boar movement with and without hiking trails, we expect that the distance with hiking trails is longer. Product: Map

## Data
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->
* Wild boar movement data
* Scare-Off coordinates
* Topographic landscape model (TLM) data
  + Land use
  + Roads and paths
  + Topology


## Analytical concepts
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? --> 
Since the two questions are not related together, we are going to approach the two questions separately. The idea behind the first question is to compare similar but different approach, in order to determine the better way to measure the effectiveness of scare-off measures, from a statistical but also ecological perspective. Wild boars move through space following a typical limited 2D Euclidean space. 
To answer the second question, the idea is to study the influence that roads and trails (as an index of disturbance) have on wild boars. To do this, wild boar movements will have to be compared with simulated wild boar movements. In this way, it will be possible to determine whether the wild boars have become accustomed to the presence of humans. 

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->
To import and to manipulate tabular data we use the libraries "readr" and "dplyr". ggplot2-package is used to plot data and sf-package to handle spatial vector data. Since we are working on time-data we also need the library "lubridate". Other packages will be loaded and used if needed. 

## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? --> 

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->
- The analytical and R concept for both questions still has to be defined exactly, can you give us any tips? 

