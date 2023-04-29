##############################################################################
# Titel:         R Tutorium Nr. 1                                            #
#                                                                            #
# Beschreibung:  Hier werden Basiskonzepte der Programmiersprache R erläutert#
#                                                                            #
# Autor:         Marco Kerkemeier                                            #
#                                                                            #
# Datum:         4. April 2023                                               #
##############################################################################



# Einfache Berechnungen
50+5
66-9
5*4
5:3
5/3
2^2
2**2
7%%4
10-3*2
10-(3*2)



# Variablen
x=2*10^2
x*10

y=10
z=x/y
z

(z=x/y)

print(z)



# Einfache Datentypen
mein_integer = 5
hans_peter = 5.467
mein_character = "Oliver Kahn"
mein_character_1 = 'Oliver Kahn'
mein_boolean = FALSE



# Logische Operatoren
X = 5
Y = 4
X==Y

X!=Y

X > Y
X>=Y
X<Y
X<=Y



# Vektoren
int_vector = c(1,2,3,4,5)
int_vector

str_vector = c("Hallo", "Tschüss", "Servus")
str_vector

bool_vector = c(TRUE, FALSE, FALSE, TRUE)
bool_vector

wrong_vector = c(1, 'Hallo', FALSE)
wrong_vector

test_vector = c(1,5,7,8,9,0,4,5,6.777)
length(test_vector)
class(test_vector)
test_vector[3]
test_vector[1:4]
test_vector[-2]

large_vector = c(test_vector, 1, 5, 6, 7, 888)
large_vector

large_vector[(length(large_vector)-1)]



# Matrizen
my_matrix = matrix(data=c(1,2,3,4), nrow=2, ncol=2, byrow=FALSE)
my_matrix

colnames(my_matrix) = c("Note Mathe", "Noten Statistik")
rownames(my_matrix) = c("Torsten", "Marianne")
my_matrix

dim(my_matrix)


Noten_statistik = c(1,1,3,4,5,2,3)
Noten_mathe = c(1,5,3,2,1,1,3)
Noten_1 = cbind(Noten_statistik, Noten_mathe)
Noten_1

Noten_2 = rbind(Noten_statistik, Noten_mathe)
Noten_2

element_1 = Noten_1[1,1]
element_1

element_2 = Noten_1[,1]
element_2

element_3 = Noten_1[1,]
element_3

test_matrix = matrix(data=1:36, nrow=6, ncol=6, byrow=TRUE)
test_matrix

test_matrix[,-5]


A = matrix(c(5,6,7,1,2,5,8,8,6), nrow=3, ncol=3, byrow=TRUE)
B = matrix(c(7,7,7,1,1,1,5,4,8), nrow=3, ncol=3, byrow=TRUE)

A+B
A-B
A%*%B
A*B
A/B


Ce=matrix(data=c(5,6,7,3,4,1,2,3,6), nrow=3, ncol=3, byrow=TRUE)
solve(Ce)

t(Ce)



# Data Frames
vec_1 = c(1,2,3,4,6,8)
vec_2 = c(5,5,5,5,5,7)
vec_3 = c(7,7,6,8,5,2)
df_1 = as.data.frame(cbind(vec_1, vec_2, vec_3))
df_1

df_2 = cbind.data.frame(vec_1, vec_2, vec_3)
df_2

colnames(df_2) = c('col1', 'col2', 'col3')
rownames(df_2) = c('row1', 'row2', 'row3', 'row4', 'row5', 'row6')
df_2


Noten_Statistik = c(1.0, 1.7, 4.0, 5.0, 2.0, 1.3, 3.7, 2.0, 2.0, 3.0)
Noten_VWL = c(1.3, 4.0, 5.0, 1.3, 1.7, 2.3, 4.0, 3.0, 3.0, 3.0)
Noten_BWL = c(1.7, 3.0, 4.0, 1.3, 1.0, 3.3, 5.0, 2.0, 3.0, 3.0)
Noten_gesamt = cbind.data.frame(Noten_Statistik, Noten_VWL, Noten_BWL)
colnames(Noten_gesamt) = c("Noten_Statistik", "Noten_VWL", "Noten_BWL")
rownames(Noten_gesamt) = c("Peter", "Thomas", "Sophie", "Sarah", "Walter",
                           "Michael", "Sonja", "Nena", "Susanne", "Markus")
Noten_gesamt

Einser_Statistik = subset(Noten_gesamt, Noten_gesamt$Noten_Statistik <= 1.3)
Einser_Statistik

Einser_BWL_VWL = subset(Noten_gesamt, (Noten_gesamt$Noten_BWL <= 1.3) &
                            (Noten_gesamt$Noten_VWL <=1.3))
Einser_BWL_VWL

Einser_BWL_o_VWL= subset(Noten_gesamt, (Noten_gesamt$Noten_BWL <=1.3) | 
                             (Noten_gesamt$Noten_VWL <=1.3))
Einser_BWL_o_VWL



# Listen
fussball_vereine = c("Bayern", "Paderborn", "Liverpool")
glueckszahl = 11
stadion_matrix = matrix(c("Anfield Road", "Allianz Arena", "Bernabeu", 
                          "Guiseppe Meazza", "Wembley", "Emirates"), nrow=3,
                        ncol=2, byrow=TRUE)
fussball_list = list(fussball_vereine, glueckszahl, stadion_matrix)
fussball_list

names(fussball_list) = c("Fussball_Vereine", "Glueckszahl", "Stadien")
fussball_list

fussball_list$Fussball_Vereine
fussball_list$Stadien

fussball_list$Stadien[[1]]
fussball_list$Stadien[[4]]
fussball_list$Stadien[1,2]



# Daten einlesen
setwd("C:\\Repositories\\lasso_oil_price_forcast\\R-Tutorium")
getwd()
Gold = read.table("Gold.txt", header = TRUE, dec=".")
head(Gold, n=10)
tail(Gold)
summary(Gold)
str(Gold)


Gold_1 = read.csv2("Gold.csv", header=TRUE, dec=".")
head(Gold_1)
tail(Gold_1)
summary(Gold_1)
str(Gold_1)



# if-Bedingungen
X=10
if(X>8){
    print("X is greater than 8!")
}



# if else Bedingungen
X = 1
if(X>8){
    print("X is greater than 8!")
}else{
    print("X is not greater than 8!")
}



# if else if else Bedingungen
X = 8
if(X>8){
    print("X is greater than 8!")
}else if(X<8){
    print("X is smaller than 8!")
}else{
    print("X is equal to 8!")
}



# while Schleifen
i = 21
while(i>10){
    print(i)
    i=i-1
}



# for Schleifen
for(i in 1:10){
    print(i)
}

number_vec = 10:20
for(i in number_vec){
    print(i)
}
