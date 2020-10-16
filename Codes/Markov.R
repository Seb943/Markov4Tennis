## Markov4Tennis repository
## (c) Sébastien Cararo - 2020
## This package helps modelize a tennis match with the help of markov chains. 
## Because of the particular structure of tennis scores, i.e. imbricated sequential models which 
## are perfectly described with Hierarchical Hidden Markov Models (HHMM). 
## Given our only two inputs - the probabilities of each player winning a point on his serve -
## we can modelize a game and a tie-break first, then we can use these modelizations 
## to modelize a set, and finally we can use the set model to modelize a whole match. 

## The final-user might only use the functions that are described from line 728 in the functions script.
## These functions are described through use cases just there down (Markov.R), and you can also 
## find extensive explanations in the GitHub repository page https://github.com/Seb943/Markov4Tennis
## Contact : sebcararo@hotmail.fr

library(markovchain)
library(dplyr)
library(igraph)
# Need to set the working directory to file's location before... (Session>Set Working Directory > To Source File location)

source("functions.R") 


ppoint_srv1 <- 0.7
ppoint_srv2 <- 0.6



################################# USE CASES #########################################
# A - Compute game winning probabilities if score is s_game
resGAME(ppoint_server = 0.7, s_game = s0game, graph = TRUE)
# B - Compute tie-break winning probabilities if score is s_tb
resTIE(ppoint_srv1 = 0.7, ppoint_srv2= 0.6, s_tb = s0tb, graph = TRUE)
# C - Compute set winning probabilities if score is s_set
resSET(phold1 = 0.7, phold2 = 0.6, 
       ptie1 = resTIE(ppoint_srv1 = 0.7, ppoint_srv2= 0.6, s_tb = s0tb, graph = FALSE)[1, "SETv1"], 
       s_set = s0set, graph = TRUE)
# D - Compute match winning probabilities if score is s_match
resMATCH(pset_v1 = 0.7, s_match = s0match, graph = TRUE)
# E -  Finally, we can use the function to compute probabilities of victory from a given score
determiMM(ppoint_srv1 = 0.8, ppoint_srv2 = 0.7, setscore = '1-0', gamescore = '3-0', 
          ingamesc = '30-0', server = 1, s0match, s0set, s0game, s0tb)




