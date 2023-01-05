# SIM_2_VoteChoiceGermany
## 1. Data Description
The data has 1000 individual observations with personal information related to politics and personal information. These variables are the original ones in the dataset.


- vote: Voting decision for party into 6 levels (represented parties in the Bundestag) (target):
  - AfD: Alternative für Deutschland, right wing populist party (right)
  - CDU/CSU: Center-right Christian-democratic political alliance (center)
  - FDP: Free democratic party -- liberal party center or center-right of the political spectrum (center)
  - Gruene: Die Grünen -- "the Greens" (left)
  - LINKE: DIE LINKE the left party is a democratic socialist political party in Germany, it is the furthest left-wing party of the six represented in the Bundestag (left)
  - SPD: Social Democratic Party of Germany, center left (center).
- egoposition_immigration: Ego-position toward immigration (0 = very open to 10 = very restrictive )
- ostwest: Dummy for respondents from Eastern Germany (= 1)
- political_interest: Measurement for political interest (0 = low, 4 = high)
- income: Self-reported income satisfaction (0 = low, 4 = high)
- gender: Self-reported gender (binary coding with 1 = female)

Important things to remark:
- Is an unbalanced dataset.
- All variables are categorical.
- There are no missing values.



## 2. Problem description and approach
We will create three binomial and one polytomous models to create a hierarchical one in order to predict right, center and left wing voting in the political spectrum and with those results predict the party more likely to vote for each individual. With this approach we will drag the error to the next models, but probably we will obtain better results than predicting each feature separately.
