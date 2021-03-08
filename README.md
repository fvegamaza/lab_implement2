# Customer Churn - Top 34% (40 teams)
> Internal competition organized by Austral University

## My experience through this
This was my first contact with real dataset and real coding structure. Probably at the beginning of 2019. At that point on my learning path was extremely frustanting
to me trying to increase the preformance of my model, mainly for two reasons.
- Just a few months before the competition, and before this one I've never code anything before
- A lot of teoric in a short period of time
We were two in the team. And I remember the most shocking part was that I tried to predict N/A in the dataser to improve the score. But I didn't know
that xgoosbost handle it by itself, I noticed it two weeks after the competition had finished. That event really encourage me to study the background of the code.
It was a competition with a lot of support the focus were in educative purpuses not just in the leaderboard. As a consquence, a lot of code was give by the organizer
(Gustavo Denicolay) A teacher who deserves all my admiration

## Technical aspects
For those educative purpose, a lot of code was given by the organizers. It looked like as a stairs. In the first steps we just used liner regression (such as a baseline), and along time we enden used xgboost with hyper-parameter optimization with feature enginierin in the data
Also we used google cloud to run all our code and nobody used python, just /R
The challenging thing about this competition was the "time window" to predict, because we had to predict 2 months before it happened, as a consequence it change the
way to select the data to test it and train it

## About the dataset:
- It was real data taken from a Argentinian bank
- Features with dates were really important
- N/A had a meaning. In this case, the majority of N/A people used to be a bad customers in terms of the bank
- All numerical data


*Still A lot to improve, but im working on it.*
