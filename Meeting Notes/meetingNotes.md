## 9/12/24

- lit review on tournament structures or competition frameworks
    - go through papers - Goossens paper
        - go through interesting citations
    - go through leagues that exist and catalog how winners are decided
        - Olympic sports
        - major sports leagues
- something statistical - what can we say about the properties of each structure?
    - theory and simulation
    - are there tournament structures that produce biased estimators
- distinction between paired comparisons vs XC or Nascar or golf
    - paired vs ranked based sports
        - things get weird with rank based sports (sports climbing medals changing)

binary outcome tournament types

- round robin (nRR)
    - 2RR - premier league
    - 1RR - group stage World Cup
    - partial (fixed) RR - major league baseball
    - Swiss tournament - dynamically choose who you play next - NFL playoffs (reseeding)
        - what if first p rounds were fixed and then you assign dynamically
    - Jacobian ladder - rugby (top 4 from the 8 play, bottom 4 play)
- knock out (L KO)
    - 1 KO - NCAA
        - true 3rd- second and third play each other for the win - USA Olympic Trials wrestling
        - 3rd place - losers of semifinal play each other for third
        - repechage - Judo
        - ladder tournaments - squash, badminton, basketball (A10 tournament)
            - complete ladders (a ton of byes) and partial ladders (a few byes)
    - 2 KO
        - true (keep going until only one team has not lost 2 games) - maybe league of legends, esports, old man softball
            - where the game is not super physically taxing
        - lose (highest you can get is third) - college wrestling
        - repechage - wrestling, double bronze
    - 3 KO - curling
- hybrid
    - RR → KO - every league in the US basically

dynamic - branches change

deterministic - branches do not change

other kinds of competitions (not two competitors and a winner)

- track heats

other

- need notation for NCAA (one game, one loss) vs NBA (seven games, one loss)
    - how many people you start with, number of losses, number of games to determine a loss
- seeding in a knockout tournament
    - if goal is to find best team, use KO with no seeds
        - want second, third, etc., use seeding
    - full, partial (seed top n, then random draw), tennis group seeding
    - seeds are like priors, and the tournament is the data
        - Bayesian Prior
        - look into Bayesian statistics
- Bradley Terry model
    - logistic regression with indictors for the competitors
- Glickman - researcher

## 9/26/24

- Bradley Terry model
    - logistic regression
    - estimate strength of a team
    - want tournament to give us information
- ratings - score for strength of team. rankings - function of ratings, best team is 1, etc.
- transitive property: a > b, b > c, so a < c
    - weak stochastic transitivity property
        - let A, B, C be teams
        - if P(A → B), P(B → C) > 0.5, then P(A → C) > 0.5
- graph theory
    - graph G = (V, E), or sets V of vertices and E of edges
    - Vertices A and B are adjacent if there is an edge (A, B)
    - in a directed graph, arrow A → B
    - simple graph contains no loops (A playing or beating itself)
    - a graph with n vertices with every pair of vertices is adjacent is a complete graph
        - tournament T is a simple and complete directed graph
    - a directed graph can be represented by an adjacency matrix
        - ith row and jth column represent ith and jth vertices; ijth entry is a directed edge (i,j)
- Kendall and Smith (1940)
    - thought judges should make consistent preferences
    - single judge comparing 4 objects, A should have 3 wins, B have 2, etc.
    - circular triad: A → B → C → A
        - if too many circular triads, judge is inconsistent
        - measure of inconsistency, d
            - d is the number of circular triads in a tournament
    - Elo (1978) or Glicko (1998) for comparisons before and after tournaments
- **PAPER IDEA**: if you have a bunch of team strengths, simulate tournament structure, true ranks vs estimated ranks, spearman correlations
    - vector of 4 teams and their rankings
    - P one team will beat another team
    - round robin vs single elimination
        - simulate a bunch of the tournaments, get different rankings, calculate spearman correlation coefficient distribution
            - then compare the tournaments
        - how close can we get the coefficient to one in the fewest number of games?
        - what kind of tournament works better for a transitive vs intransitive model?

- **TO DO**: functions to start simulating tournaments
    - start with Bradley Terry type model
    - take in a vector of 4 teams/strengths, and returns a ranking
        - need to specify the strengths and their distribution?
            - complete parity (all zeros) vs no parity
                - need to have realistic, medium between these
        - then run this function a lot
    - each function for different type of tournament
    - wins and loses only
    - ranking based only on number of wins; if ties, rank average
    - start with round robin, single elimination
        - teams that lose in semis in single elimination get 3.5 ranking
        - single elimination - allow byes, need seeding structure?
- paper name for repechage: Run It Back

## 10/10/24

- round robin function
    - $\theta$ = vector of strengths using `runif()`
    - $\theta_i / (\theta_i + \theta_j)$ = prob(i > j)
    - function inputs: team list and strength list
    - output: team, number of wins, and ranking
        - want to know correlation between original strength and output ranking
- single elimination
    - inputs: teams, true rank, seeds
        - types of seeding:
            - shuffle with replacement
            - $f(\theta)$ - some function of the rankings
            - true rank ($\theta$)
        - types of structure
            - bracketology (1v8, 2v7, etc.)
                - compare to how bad 1v2, 3v4, etc. is
                - how many different unique tournaments are there (regarding seeding structures)?
                    - for 4 teams, A can only play 3 different teams, so 3 possibilities
                    - what about reseeding after each round?
            - bye handling
                - step 2 is ways to handle byes, right now $2^n$ teams
- ladder tournament, caterpillar, waterfall?
    - ladder tournament: could go on forever
        - https://en.wikipedia.org/wiki/Ladder_tournament

- **TO DO:** focus for next week
    - round robin distribution of correlations (Kendall rank) between true strengths and function output ranks
        - team, score (different in each simulation), rank (diff, call rank hat), true strength
        - correlation between rank hat and rank
            - distribution of correlations between different simulations
    - mess around with seeding structures
        - how many ways to set up a tournament based on seeds?
    - bracketology
        - for every seeding structure, get correlation distribution for ranks and predicted ranks
        - 4 teams - 3 options: ABCD, ACBD, ADBC
            - which seeding structure gets closest to true strength ranks
    - generate probabilities using $\exp(i-j)/(1+\exp(i-j))$
        - vector of $\theta$s (strengths) - more than just between 0 and 1
- generalize these approaches so we do not have to rewrite for every type
    - pass in true strengths, seeds, and structure
- how many unique seeding structures in an 8 team tournament
    - if $n = 2^k$ teams, how many unique structures (number of rounds is k-1)
    - answer: $(2^n)!/2^{(2^n-1)}$
        - a(n) is also the number of knockout tournament seedings with 2^n teams
        - ex. 8 teams = $2^n$
            - $8!/2^7 = 315$

## 10/17/24 Progress

- number of distinct tournament setups for $2^n$ teams: $(2^n)!/2^{(2^n-1)}$
- added round robin distribution of correlations (Kendall rank) between true strengths and function output ranks
    - team, strength, rank, rank hat, wins
- generating winning probabilities using $\exp(i-j)/(1+\exp(i-j))$
    - why would we want to use this?
- histograms for correlations
    - Bradley Terry Model
        - mean = 0.6945, median = 0.6944
    - using exponential distribution thing
        - mean = 0.4321, median = 0.4536


## 10/18/24

- round robin with two or three rounds and compare kendall coefficient
- $e^x / (1+e^x) = e^{(i-j)}/(1+e^{(i-j)})$
    - start with all real numbers → (0, 1]
        - rather than decimals (0, 1] → all reals (Bradley Terry)
    - start with vector of strengths (1, 2, …, 8)
        - could do random distribution or normal distribution (possibly closest to reality)
    - simulate strengths one time and see what that looks like
        - then maybe simulation of simulations (random leagues with normal distribution)
            - one season versus concept of a league
- when we compare tournament structures, we have fixed theta and see which tournament structure gets closest to true ranks
- tournament structure function:
    - main input: team names, strengths, tournament structure
    - **BUILD THESE FIRST:** different functions for each type, main function to simulate tournament
        - round robin function: teams, strengths, schedule, (seed?)
        - single elimination function: teams, strengths, seeds, schedule, number games per round, home or away
            - byes: A plays NA, if a team plays NA then A wins automatically
- eventually, simulate like NBA regular season
    - schedule effects?
- look at kendall’s tau and spearman’s correlation

- **TO DO:**
    - Josie: vector 1-8, vector using normal distribution
        - use $e^x / (1+e^x)$ for winning probability
    - Zach: work on single elimination function

## 10/31/24

- don’t want to generate strengths randomly because we want to control them more
    - pick the values so they are the same every time (space out to match normal)
        - use inverse cdf - qnorm() and pass a set of evenly spaced percentages (values 0 to 1)
        - don’t want first cut to be at 0, want it to be half a unit
    - for uniform distribution, just 1-8
        - different by one, different by two, etc
- comparing correlations between different tournament structures

round robin

- what if they play twice or three times
- partial round robin
    - every time plays exactly 2 or whatever games
    - balanced design
    - Taguchi

## 11/8/24

- run round robin with 4 teams for 1-4 times
    - make scatterplot of correlations between different number of round robins run
        - where does kendalls tau level off?
    - try with different number of teams (4, 6, 8)
        - does the number of teams actually affect this? if you have more teams, does the correlation drop?
    - then maybe try with different thetas
- also try qunif
- what’s the worst kendalls tau we can get for a structure
    - averages could be good, but full distribution could be different
- run single elimination of best 5 structures a lot more times to get better idea of kendalls tau
    - take one tournament structure and write out every possible tournament outcome (128 outcomes) to get different kendall’s taus. can get exact distribution
        - do best and worst correlation

## 12/13/24

- how we are evaluating the tournaments
    - probability curve of the top team winning the whole thing
        - prob(best team won the tournament)
        - prob(second best in 2nd | best was first)
        - prob(third best in 3rd | top 1 and 2 teams in top 2)
            - order for 1 and 2 does not matter
        - prob(fourth in 4th | top 1, 2, 3 teams are in top 3)
            - basically throwing out top teams and seeing if we can identify the highest of the remaining teams
            - **probability of identifying next highest team out of n remaining teams**
            - if the teams are all strength: 1 / # of remaining teams
                - if all the orderings are equally likely
        - can plot this curve with this baseline of all the orderings being equal (1/n)
            - shows how tournament structure does and how it differs from the baseline
        - get a curve above this baseline curve
            - however far you pull off the baseline is how good the tournament structure is
            - x is the rank of the strengths
- kendalls tau of the ranking is easy for round robin because everyone plays everyone
    - need similar measure for other structures
        - two extremes: did it get the top one right and did it get everyone right
            - Olympics: want 1, 2, 3 to be correct then it does not matter
- how many switches does it take to get original order from the predicted tournament order

# 2025

## 1/24/25

- run 1000 simulations for different numbers of teams
    - uniform and normal distributions for strengths
    - 3-8 teams, 16 teams
    - save them as list objects to send to Zach to be run in probability curve code

## 2/27/25

- talking about entropy and mutual information
    - trying to convey information: message is the true ranks, tournament structure processes it, and the outcome is the message that is received
    - issue with mutual information: if 1234 is true and 4321 is outcome, gives the same mutual information as if the outcome was correct
        - so need weighted information - weight outcomes we want more
        - weight: squared difference between true and predicted and take reciprocal
            - force differences of 0 to be 1
            - large differences between true and predicted give big sum of squared but small reciprocal so small weight
    - but also times where we only care about top team or top three teams
        - additional layer of weighting that is either a number or 0 for whether we care about the placement of that team
        - called rank weights

## 10/21/25

- way to expand our research
    - if we care about the top 3 teams but not the order of the rankings
        - what if we only care about identifying the top n but not getting their order correct
        - modify weighting function so they get the same weights
        - for kendall’s tau, how many swaps to make sure the group is correct
        - just changing the weighting

## 11/7/25

- structure of the paper
- have been working on
    - solved if we want to get the top 4 teams but we do not care about the rankings
        - r hat to 1 1 1 1 for top four teams rather than 1 2 3 4
    - do something “real” so we have actual “data”
        - take real season of nfl, estimate thetas for a single season - 2010
        - try different playoff structures to see how they perform
        - bad seeding and reseeding after rounds
        - playoff structure is different than other major sports in america
- maybe need standard error if we want to use this measure in decision making process of designing tournaments
    - bootstrapping
- structures we want to do (16 teams?)
    - single elimination
        - normal
        - series
        - third place game
        - reseeding
        - bad seeding
    - double elimination
        - true
        - consolation bracket (?)
        - repechage (greg will code) (!!!)
    - ~~triple elimination~~
    - deterministic outcomes (better team always wins, round robin)
    - round robin
        - (1, 2, 4) rounds (?)
    - stepladder
    - waterfall / stepladder round robin (round robin → round robin) (!!!)
    - group stage (round robin → knockout)
    - high school tennis ladder (n-1 iterations) (!!!)
- lit review

## 11/7/25 - Ryan Notes

Waterfall with 16 teams:
- Seeds: 1 - 16
- Division 2: seeds 9 - 16 get placed into groups (e.g., 9/10 randomly assigned to D2A and D2B, 11/12 to D2A, D2B, etc.)
  - winners of D2A and D2B get floated to Division 1
- Division 1: seeds 3 - 8 get placed into groups randomly; D2A and D2B winners also
  - winners of D1A and D1B get floated to top 4
- Top 4: double elimination (where loser of final goes to consolation and winner of consolation to grand final against final winner)

Waterfall Examples (n large):
- https://www.start.gg/tournament/supernova-2024/event/64-1v1-singles
- https://www.start.gg/tournament/super-smash-con-2023/event/64-1v1-singles


See also idea for plot with deterministic bounds (for any tournament type).

## 11/14/25

- parameters for different loops
    - RR
        - distributions: normal, uniform, equal chance
            - add Exp(1)
        - ties: randomly assign or ties allowed
        - k = 1, 2, 4
        - n = 16
    - SE
        - distribution
        - ties
        - series: 1, 3, 7
        - third: T/F
        - resseded
        - worst seeding
        - stepladder is variation of SE
    - DE
        - distribution
        - ties
        - series
        - consolation vs true DE
        - worst seeding
    - group stage
        - distribution
        - ties
        - rounds within groups: 1, 2, 4
        - group size: 4 or 8 (where top 2 advance)
    - staged RR (working on it)
        - 4x4 → 4x4
        - 8x2 → 8x2
- paper sections
    - tournament structures (compare within tournament structures)
    - different distributions (compare across structures)
        - normal
        - uniform
        - equal
        - exponential
- other notes
    - just use random assignment for ties & always have a third place game
    - round robin - facet graphs
        - normal (k = 1, 2, 4)
        - uniform
        - equal
        - exp
    - SE
        - X axis: different distributions
        - Y: number of rounds (1, 3, 7)
