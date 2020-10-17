# Markov4Tennis
:information_source: An R script to modelize a tennis match with Markov chains. The only parameters are the point winning probabilities on serve for a point (2 probabilities : one for p1 and one for p2). Future work :
- Adapt the code to various sports
- Create evolving models of Markov Chains (i.e. if the player 1 won the first set then he might be less likely to win the second set, hence we could adapt the model depending on the result of the first games)

:information_source: Markov Chains explained visually [1] 

:information_source: Functionalities :
- Compute winning probabilities for a game, a set, a tie-break and a match
- Compute winning probabilities for already started match
- Compute final score probabilities in terms of set
- Display Markov Chain models

:information_source: Functions overview : <br />
Function | Purpose | Working ? 
------------ | ------------- | ------------- 
determiMM() | Compute match winning probabilities from any score| :heavy_check_mark: 
resGAME() | Compute game winning probabilities | :heavy_check_mark: 
resTIE() | Compute tie-break winning probabilities | :heavy_check_mark: 
resSET() | Compute set winning probabilities |:heavy_check_mark: 
resMATCH() | Compute exact set scores probabilities | :heavy_check_mark: 

:information_source: The main functions which you can use are the following one : <br />
```python
1.scrape_oddsportal_historical(sport = 'soccer', country = 'france', league = 'ligue-1', start_season = '2010-2011', nseasons = 5, current_season = 'yes', max_page = 25)
2.scrape_oddsportal_current_season(sport = 'soccer', country = 'finland', league = 'veikkausliiga', season = '2020', max_page = 25)
3.scrape_oddsportal_specific_season(sport = 'soccer', country = 'finland', league = 'veikkausliiga', season = '2019', max_page = 25)
4.scrape_oddsportal_next_games(sport = 'tennis', country = 'germany', league = 'exhibition-bett1-aces-berlin-women', season = '2020') 
```
:information_source: Outputs of the functions : </br> 
![Scraper_final code](Screenshots/ScreenshotCode.PNG)
..then console when running code : <br/>

:information_source: Please report any bug/issue in the *issues* section or directly at sebcararo@hotmail.fr (Any feedback is really appreciated :speech_balloon: :+1:)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
You can also have a look at the *functions.py* source code in order to understand the mechanics and eventually adapt the code to your own purpose. I suggest the reader to start by reading lines from n°722 to the end, because these functions are the most speaking ones and that I commented them to clarify the inputs/outputs and use. 

Markov chains are a great tool for modelizing all many kinds of stochastic process, including statistical physics, information theory, statistics, finance, but also sports modelizations. Several bookmakers use related techniques to predict odds for in the sports betting industry. 

[1] https://setosa.io/ev/markov-chains/ <br />
