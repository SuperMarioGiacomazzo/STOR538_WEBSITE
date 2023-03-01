#Lineup Calcuations
Warrior2A_Min=225
Warrior2A_Game=Warrior2A_Min/48
Warrior2A_PM = 125
PM_Per_Game= Warrior2A_PM/Warrior2A_Game

#Number of Lineups
choose(12,5)

#Lineup Test
l1.rating=-2.4
l1.games=426/48
l2.rating=28
l2.games=126/48
Z=(-2.4-28)/sqrt(12^2/8.875+12^2/2.625)
pnorm(Z)

pnorm(-30.4,mean=0,sd=8.43)
