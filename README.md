# Customer Churn - Top 34% (40 teams)
> Internal competition organized by Austral University


## What was about?
An internal university competition, where we had to predict which customers will leave the bank two month before it happens

## My experience through this
This was my first contact with real dataset and real machine learning case. Probably at the beginning of 2019. At that point on my learning path it was extremely frustrating to me trying to increase the preformance of my model, mainly for two reasons.
- Just a few months before the competition, and before it one I've never code anything before
- A lot of theoretical aspects in a short period of time

I remember the most shocking part was that I tried to predict N/A in the dataser to improve the score. But I didn't know
that xgoosbost handle it by itself, I noticed it two weeks after the competition ended. That event really encourage me to study the background of the code. It was a competition with a lot of support the focus were in educative purposes not just in the leaderboard. As a consequence, a lot of code was give by the organizer (Gustavo Denicolay) A teacher who deserves all my admiration

## Technical aspects
For those educative purpose, a lot of code was given by the organizers. It looked like as a stairs. In the first steps we just used liner regression (such as a baseline), and along time we ended using xgboost with hyperparameter optimization with feature engineering in the data.
Also we used google cloud to run all our code and nobody used python, only /R
The challenging thing about this competition was the "time window" to predict, because we had to predict 2 months before it happened, as a consequence it change the way to select the data to test it and train it

## About the dataset:
- It was real data taken from a Argentinian bank
- Features with dates were really important
- N/A had a meaning. In this case, the majority of N/A people used to be a bad customers in terms of the bank
- All numerical data


*Still A lot to improve, but im working on it.*
