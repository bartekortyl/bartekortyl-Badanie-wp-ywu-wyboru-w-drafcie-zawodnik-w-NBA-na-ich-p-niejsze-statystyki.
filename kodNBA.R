#1st draft vs 2nd draft vs 3rd draft

 players2<-subset(players, draft_pick=="1st overall"|draft_pick=="2nd overall"|draft_pick=="3rd overall") #TU SA TYLKO GRACZE Z DRAFT 1/2/3

#H1: Ró¿nice w punktach ze wzgledu na draft (1/2/3)
#H2: Róznice w asystach ze wzgledu na draft (1/2/3)
#H3: Róznice w rozegranych meczach ze wzgledu na draft (1/2/3)
#H4: Roznice w punktach a recznosc (lewa/prawa)
#H5: Roznice w asystach a recznosc (lewa/prawa)
#H6: Roznice w rozegranych meczach a recznosc (lewa/prawa)
  
#anova - TRZY GRUPY

 #H1
 library(car)
shapiro.test(players2$career_PTS[players2$draft_pick=="1st overall"]) #ten test trzeba zrobic dla zmiennej zaleznej w kazdej podgrupie
shapiro.test(players2$career_PTS[players2$draft_pick=="2nd overall"]) 
shapiro.test(players2$career_PTS[players2$draft_pick=="3rd overall"]) #zalozenie jest spelnione gdy p>0.05 = rozklad jest normalny
leveneTest(players2$career_PTS~players2$draft_pick) #zalozenie jest spelnione gdy p>0,05 = wariancje sa rowne
model1<- aov(players2$career_PTS~players2$draft_pick)
summary(model1)

#p>0.05 - nie ma istotnych roznic w punktach ze wzgledu na draft

#levene

#H2
library(car)
shapiro.test(players2$career_AST[players2$draft_pick=="1st overall"]) #ten test trzeba zrobic dla zmiennej zaleznej w kazdej podgrupie
shapiro.test(players2$career_AST[players2$draft_pick=="2nd overall"]) 
shapiro.test(players2$career_AST[players2$draft_pick=="3rd overall"])
leveneTest(players2$career_AST~players2$draft_pick) #zalozenie jest spelnione gdy p>0,05 = wariancje sa rowne
model2<- aov(players2$career_AST~players2$draft_pick)
summary(model2)

#H3
shapiro.test(players2$career_G[players2$draft_pick=="1st overall"]) #ten test trzeba zrobic dla zmiennej zaleznej w kazdej podgrupie
shapiro.test(players2$career_G[players2$draft_pick=="2nd overall"]) 
shapiro.test(players2$career_G[players2$draft_pick=="3rd overall"])
leveneTest(players2$career_G~players2$draft_pick) #zalozenie jest spelnione gdy p>0,05 = wariancje sa rowne
model3<- aov(players2$career_G~players2$draft_pick)
summary(model3)

#POROWNANIE DWOCH GRUP

players$shoots<-as.factor(players$shoots) #zmiana z character na factor
table(players$shoots) #trzy wartosci, a jedna ma tylko jedna obserwacje
which(players$shoots=="Left Right")
players<-players[-4132, ]

#porownujemy tylko dwie grupy = lewo/praworeczni gracze

#test t-Studenta
#H4
shapiro.test(players$career_PTS[players$shoots=="Left"]) #gdy p>0.05 - to zalozenie spelnione
shapiro.test(players$career_PTS[players$shoots=="Right"])
var.test(players$career_PTS~players$shoots) #gdy p>0.05 to zalozenie spelnione
t1<-t.test(players$career_PTS~players$shoots, alternative="greater") #porownanie punktow a lewo/praworecznosc - JEDNOSTRONNY TEST


left right = ujemna
leworeczni gracze zdobywaja wiecej punktow


Jesli p<0.05 to wystepuja istotne roznice


#H5 

shapiro.test(players$career_AST[players$shoots=="Left"]) #gdy p>0.05 - to zalozenie spelnione
shapiro.test(players$career_AST[players$shoots=="Right"])
var.test(players$career_AST~players$shoots)
t2<-t.test(players$career_AST~players$shoots, alternative="greater")
#gracze leworeczni zdobywaja wiecej asyst, tylko to p male


#H6
shapiro.test(players$career_G[players$shoots=="Left"]) #gdy p>0.05 - to zalozenie spelnione
shapiro.test(players$career_G[players$shoots=="Right"])
var.test(players$career_G~players$shoots)
t3<-t.test(players$career_G~players$shoots, alternative="greater")
#gracze lewo reczni graja wiecej gier


#chi-kwadrat
#Tworzymy tabelê pomocnicz¹ grupuj¹c¹ powtarzaj¹ce siê odpowiedzi i zliczaj¹c¹ liczbê ich wyst¹pieñ w danym zakresie
tabelka<-as.data.frame(table(players$draft_team))
tabelka$p<-rep(1/65, 65)
chisq.test(tabelka$Freq, p=tabelka$p)
jesli p<0.05 - kategorie wystepuja z roznym prawdopodobienstwem = do zespolow przypisywano z rozna czestoscia

#regresja
y=ax+b
points ~ gry
model4<-lm(career_PTS~career_G,data=players) #da sie przewidziec punkty przez gry
summary(model4)
punkty = 3.43 + 0.01*career_G
im wyzsze career_G, tym wiecej punkty