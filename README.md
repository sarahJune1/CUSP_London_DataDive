# CUSP London Data Dive

### Team D:

<b>Konstantin, Mateo, Sarah and Zahra<b>

<b>Question at hand:<b>
Each day the population of London swells with commuters and tourists. Can we use transport data
to enhance our understanding of the population of London at different times of the day, and
therefore better predict demand?


## Optimized Emergency Deployment for a Dynamic City

### Goal:

Understand the dynamic changes in demand and environment to address the needs of the London population at different times throughout the day. 

#### Method:

    - Use traffic data gleaned from response data file
    
    - Calculate shortest route between incident and response/dispatch
        - OSMX

#### Purpose: Optimize deployment time to reach incident
    - Grid of travel time in boroughs of Westminster and Bexley
        - Add incidents
        - Add travel speed 
        - Purpose: Identify hotspots of long travel times
    
    - Cross-reference with incident count
        - Analyze time series of response running time
        - Broken down by incident category, dispatch type, month, day of week, and hour
        - Find cyclic patterns
        - Predict future running time
        
    - Analyze time series of incident count
        - Geospatial understanding
        - Goal: Deploy response units effectively 

#### Consideration:

When counting incidents, we only look at cat1 & cat2, these are the most severely dependent on speedy travel times.

#### Analysis:

Points of high risk

Running time as it relates to time of day, week, month

How distance is measured

#### Future Analysis:
Incorporate network features

Factor risk predictor… predictive model for where to locate to increase speed

Scenario: 3 available ambulances, which do you dispatch

