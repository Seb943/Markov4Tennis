## Markov4Tennis repository
## (c) Sébastien Cararo - 2020
## This package helps modelize a tennis match with the help of markov chains. 
## Because of the particular structure of tennis scores, i.e. imbricated sequential models which 
## are perfectly described with Hierarchical Hidden Markov Models (HHMM). 
## Given our only two inputs - the probabilities of each player winning a point on his serve -
## we can modelize a game and a tie-break first, then we can use these modelizations 
## to modelize a set, and finally we can use the set model to modelize a whole match. 
## NB : As 30-30 and 40-40 are strictly the same Markov states inside a game, I implemented a unique state called
## 30-30(DEUCE). Same for 6-6, 8-8, 10-10, etc... in tie-breaks.
## NB2 : Player 1 is serving first at the beginning of the match. 

## The final-user might only use the functions that are described from line 722 in the functions script.
## These functions are described through use cases just there down (Markov.R), and you can also 
## find extensive explanations in the GitHub repository page https://github.com/Seb943/Markov4Tennis
## Contact : sebcararo@hotmail.fr

library(markovchain)
library(dplyr)
library(igraph)
# Need to set the working directory to file's location before... (Session>Set Working Directory > To Source File location)
source("functions.R") 


################################# USE CASES #########################################
# I - Compute game winning probabilities if score is s_game
# (a) Starting from 0-0
resGAME(ppoint_server = 0.7, s_game = s0game, graph = TRUE) 

# (b) Another example : Starting from 0-15
s_game <- s0game;s_game[1,'0-0'] <- 0; s_game[1, '0-15'] <- 1
resGAME(ppoint_server = 0.7, 
        s_game = s_game, 
        graph = FALSE) 


# II - Compute tie-break winning probabilities if score is s_tb
# (a) Starting from 0-0
resTIE(ppoint_srv1 = 0.7, ppoint_srv2= 0.6, s_tb = s0tb, graph = TRUE)

# (b) Another example : Starting from 3-4
s_tb <- s0tb;s_tb[1,'0-0'] <- 0; s_tb[1, '3-4'] <- 1
resTIE(ppoint_srv1 = 0.7, ppoint_srv2= 0.6, 
       s_tb = s_tb, graph = FALSE)

# III - Compute set winning probabilities if score is s_set
# (a) Starting from 0-0
resSET(phold1 = resGAME(ppoint_server = 0.7, s_game = s0game, graph = TRUE)[1, "HOLD"] , 
       phold2 = resGAME(ppoint_server = 0.6, s_game = s0game, graph = TRUE)[1, "HOLD"] , 
       ptie1 = resTIE(ppoint_srv1 = 0.7, ppoint_srv2= 0.6, s_tb = s0tb, graph = FALSE)[1, "SETv1"], 
       s_set = s0set, graph = TRUE)

# (b) Another example : Starting from 4-5
s_set <- s0set;s_set[1,'0-0'] <- 0; s_set[1, '4-5'] <- 1
resSET(phold1 = resGAME(ppoint_server = 0.7, s_game = s0game, graph = TRUE)[1, "HOLD"], 
       phold2 = resGAME(ppoint_server = 0.6, s_game = s0game, graph = TRUE)[1, "HOLD"], 
       ptie1 = resTIE(ppoint_srv1 = 0.7, ppoint_srv2= 0.6, s_tb = s0tb, graph = FALSE)[1, "SETv1"], 
       s_set = s_set, graph = FALSE)

# IV - Compute match winning probabilities if score is s_match
# (a) Starting from 0-0
resMATCH(pset_v1 = 0.7, s_match = s0match, graph = TRUE)

# (b) Another example : Starting from 0-1
s_match <- s0match;s_match[1,'0-0'] <- 0; s_match[1, '0-1'] <- 1
resMATCH(pset_v1 = 0.7, s_match = s_match, graph = FALSE)

# V -  Finally, we can use the function to compute probabilities of victory from a given score
determiMM(ppoint_srv1 = 0.7, ppoint_srv2 = 0.6, setscore = '1-0', gamescore = '0-3', 
           s0match, s0set, s0game, s0tb)

