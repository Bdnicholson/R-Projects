# Reading into Data ----
pokemon <- read.csv("Pokemon.csv", header = TRUE)

#print first 10 rows ----
head(pokemon)

#print last 10 rows ----
tail(pokemon)

#print info ----
str(pokemon)

#print summary for each variable ----
summary(pokemon)


#plots ----
plot(pokemon) #plots entire data frame
plot(pokemon[,3:10]) #plots all rows and just columns between 3 & 10 (inclusive)
plot(pokemon[, "Type.I"], pokemon[, "Atk"]) #Type is on the x axis and Atk is on the y axis

# T test ----
psychic <- pokemon[pokemon$Type.I == "Psychic", "Atk"] #look at the pokemon data set, select the row where its equal to psychic and select the Attack column
rock <- pokemon[pokemon$Type.I == "Rock", "Atk"] #same as above, just rock instead

t.test(psychic, rock) #Two sample t-test (Weslh)
t.test(psychic, rock, var.equal = TRUE) #Standard t-test.
t.test(psychic, rock, alternative = "less")
t.test(psychic, rock, paired = TRUE)


#Linear regression ----
plot(pokemon$Atk, pokemon$Def)
regModel <- lm(Def ~ Atk, data = pokemon)
regModel
summary(regModel)

hist(regModel$residuals)
qqnorm(regModel$residuals)
qqline(regModel$residuals, col = "red", lwd = 3)

#Taking the log10 of Def inside of the regression call ----
RegModelLog <- lm(log10(Def) ~ Atk, data = pokemon)
RegModelLog
plot(pokemon$Atk, log10(pokemon$Def))
summary(RegModelLog)
hist(RegModelLog$residuals)
qqnorm(RegModelLog$residuals)
qqline(RegModelLog$residuals, col = "red", lwd = 3)


#ANOVA ----
pokeSubset <- pokemon[pokemon$Type.I == "Ghost" | 
                        pokemon$Type.I == "Grass" |
                        pokemon$Type.I == "Ground" |
                        pokemon$Type.I == "Ice",]
pokeSubset$Type.I

oneWay <- lm(Atk ~ Type.I, data = pokeSubset)
oneWay
summary(oneWay)

anova(oneWay)
hist(oneWay$residuals)
qqnorm(oneWay$residuals)
qqline(oneWay$residuals, col = "red")

bartlett.test(pokeSubset$Atk, pokeSubset$Type.I)
oneway.test(Atk ~ Type.I, data = pokeSubset)

kruskal.test(pokeSubset$Atk, pokeSubset$Type.I)

#post-hoc tests ----
library(mosaic)
TukeyHSD(oneWay)
pairwise.t.test(pokeSubset$Atk, pokeSubset$Type.I, p.adjust.method = "none")
p.adjust(c(0.316, 0.069), method = "holm")

#two-way anova ----
pokeSubset2 <- pokemon[pokemon$Type.I == "Bug" | 
                        pokemon$Type.I == "Electric" |
                        pokemon$Type.I == "Fire" |
                        pokemon$Type.I == "Poison",]

twoWay <- lm(Atk ~ Type.I * Captive, data = pokeSubset2)
twoWay

summary(twoWay)
anova(twoWay)
