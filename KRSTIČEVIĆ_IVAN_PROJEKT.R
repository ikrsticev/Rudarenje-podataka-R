library(tensorflow)
library(keras)

#install.packages(c("keras","tensorflow"))
#install_tensorflow()

border_crossing <- read.csv(file = 'Border_Crossing_Entry_Data.csv')

border_crossing[,1] <- as.numeric(border_crossing[,1]) #port name
border_crossing[,2] <- as.numeric(border_crossing[,2]) #state
#3. stupac je vec numeric (port code)
border_crossing[,4] <- as.numeric(border_crossing[,4]) #border (kanada ili mexico)
border_crossing[,5] <- as.numeric(border_crossing[,5]) #datum
border_crossing[,6] <- as.numeric(border_crossing[,6]) #measure (sredstvo prijevoza)
#7. stupac je vec numeric (value) 

border_crossing <- as.matrix(border_crossing)
dimnames(border_crossing) <- NULL

#dataset ima 7 stupaca i 355511 redaka

set.seed(123)

#podatke treba podijeliti na podakte za treniranje i testiranje
#ali takoder i na ulazne podatke i "target" podatke

skup_no <- sample(2, nrow(border_crossing), replace=TRUE, prob=c(0.7, 0.3))
skup_za_treniranje <- border_crossing[skup_no==1,]
skup_za_testiranje <- border_crossing[skup_no==2,]

skup_za_treniranje_target <- to_categorical(skup_za_treniranje[,2])
skup_za_treniranje <- skup_za_treniranje[,-2]
skup_za_testiranje_target <- to_categorical(skup_za_testiranje[,2])
skup_za_testiranje <- skup_za_testiranje[,-2]

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 12, activation = 'relu', input_shape = c(6)) %>% 
  layer_dense(units = 14, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'softmax')

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)


# Fit the model 
model %>% fit(
  skup_za_treniranje,
  skup_za_treniranje_target,
  epochs = 100, 
  batch_size = 100, 
  validation_split = 0.2
)

#model je dosegao preciznost od 80% na testnim primjerima nakon zadanih postavki

score <- model %>% evaluate(skup_za_testiranje, 
                            skup_za_testiranje_target, batch_size = 6)
print(score)

#accuracy je 83.61%

#Vrlo zanimljivo, preciznost je veca ovdje nego nad podatcima za treniranje

#Iako ovo nije visoka preciznost opcenito gledano, ja sam zapravo dosta
#zadovoljan s obzirom na to koliko je dataset nezgodan. Dosta stupaca dataseta
#daju vrlo malo tragova o tome o kojoj je drzavi rijec. Port name i port code su
#ubiti ista stvar i mogli smo cak ukloniti jedan od tih stupaca radi brze 
#performanse. Border otprilike govori gdje bi se drzava mogla nalaziti. Ostali
#stupci su datum, prijevozno sredstvo i value je broj ljudi koji su tu prosli tim
#prijevoznim sredstvom na taj dan.
#Ta tri stupca ne govore nista o drzavi. Znaci da je mreza morala nauciti koji port
#je u kojoj drzavi, i eventualno koristiti stupac "border". Ali moguce je i da 
#neke drzave imaju veci promet na granici nego druge pa ipak treba uvaziti 
#sve mogucnosti.

#Pokusajmo sa stablom

border_crossing <- read.csv(file = 'Border_Crossing_Entry_Data.csv')

border_crossing[,1] <- as.numeric(border_crossing[,1]) #port name
#border_crossing[,2] <- as.numeric(border_crossing[,2]) #state
#3. stupac je vec numeric (port code)
border_crossing[,4] <- as.numeric(border_crossing[,4]) #border (kanada ili mexico)
border_crossing[,5] <- as.numeric(border_crossing[,5]) #datum
border_crossing[,6] <- as.numeric(border_crossing[,6]) #measure (sredstvo prijevoza)
#7. stupac je vec numeric (value) 

skup_no <- sample(2, nrow(border_crossing), replace=TRUE, prob=c(0.7, 0.3))
skup_za_treniranje <- border_crossing[skup_no==1,]
skup_za_testiranje <- border_crossing[skup_no==2,]

library(C50)
c50_stablo <- C5.0(State ~ .,
                            data = skup_za_treniranje)
summary(c50_stablo)
rez <- predict(c50_stablo, newdata = skup_za_testiranje, type = "class")
table(rez, skup_za_testiranje$State)

plot(c50_stablo)

#Rezultat je iznenadujuce dobar, stablo je uspjelo prepoznati korelaciju
#port numbera i statea, cini se da je to dovoljno da se rijesi ovaj problem
#No sigurno ce biti puno gore na iducem problemu
#Ali kao i sad zapoceti cemo neuronskom mrezom u kerasu

#Sada zelim isto napraviti za predvidanje prometnosti, tj. broja ljudi.

border_crossing <- read.csv(file = 'Border_Crossing_Entry_Data.csv')
summary(border_crossing$Value)
#Prema ovome sam odlucio da je "malo" < 100, 101 < "srednje" < 30000 i 
#"puno" > 30001 ljudi koji prolaze

border_crossing$malo <- 0
border_crossing$srednje <- 0
border_crossing$puno <- 0

border_crossing$malo[border_crossing$Value < 100] <- 1
border_crossing$srednje[border_crossing$Value > 101 &&  border_crossing$Value < 30000] <- 1
border_crossing$puno[border_crossing$Value > 30001] <- 1

sum(border_crossing$malo)    #177727
sum(border_crossing$srednje) #355511
sum(border_crossing$puno)    #35345

#iako se ne cini ravnomjerno, "puno" stupac zna predstavljati 
#stvarno masovne brojeve (preko 4 mil.) pa cu zadrzati ovako

border_crossing[,1] <- as.numeric(border_crossing[,1]) #port name
border_crossing[,2] <- as.numeric(border_crossing[,2]) #state
#3. stupac je vec numeric (port code)
border_crossing[,4] <- as.numeric(border_crossing[,4]) #border (kanada ili mexico)
border_crossing[,5] <- as.numeric(border_crossing[,5]) #datum
border_crossing[,6] <- as.numeric(border_crossing[,6]) #measure (sredstvo prijevoza)
#7. stupac je vec numeric (value) 

border_crossing <- as.matrix(border_crossing)
dimnames(border_crossing) <- NULL

#dataset ima 7 stupaca i 355511 redaka

set.seed(123)

#podatke treba podijeliti na podakte za treniranje i testiranje
#ali takoder i na ulazne podatke i "target" podatke

skup_no <- sample(2, nrow(border_crossing), replace=TRUE, prob=c(0.7, 0.3))
skup_za_treniranje <- border_crossing[skup_no==1,]
skup_za_testiranje <- border_crossing[skup_no==2,]

skup_za_treniranje_target <- skup_za_treniranje[,8:10]
skup_za_treniranje <- skup_za_treniranje[,1:6]
#stupac "value" ne koristimo za treniranje jer je to varanje
skup_za_testiranje_target <- skup_za_testiranje[,8:10]
skup_za_testiranje <- skup_za_testiranje[,1:6]

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 12, activation = 'relu', input_shape = c(6)) %>% 
  layer_dense(units = 7, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)


# Fit the model 
model %>% fit(
  skup_za_treniranje,
  skup_za_treniranje_target,
  epochs = 100, 
  batch_size = 100, 
  validation_split = 0.2
)

#model nikako nije uspio preci preko 50% preciznosti
#jako dugo je ostao na istoj preciznosti tijkom treniranja
#mislim da bez obzira na kolicinu treninga on nikad ne bi dospio dalje
#iako je nesto naucio cim je preko 33%, ovo je svejedno jako neprecizan model
#po svim standardima
#cini se da nije moguce precizno odrediti koliko je ljudi proslo granicom
#na osnovu ostalih podataka

score <- model %>% evaluate(skup_za_testiranje, 
                            skup_za_testiranje_target, batch_size = 6)
print(score) #0.4976% preciznost na testnim podatcima

#A sada stablo
#Za stablo ce biti samo 1 flag a ne 3, znaci imati ce vrijednosti 0, 1 ili 2
#Ovisno o kolicini ljudi (malo, srednje, puno)

border_crossing <- read.csv(file = 'Border_Crossing_Entry_Data.csv')

border_crossing[,1] <- as.numeric(border_crossing[,1]) #port name
border_crossing[,2] <- as.numeric(border_crossing[,2]) #state
#3. stupac je vec numeric (port code)
border_crossing[,4] <- as.numeric(border_crossing[,4]) #border (kanada ili mexico)
border_crossing[,5] <- as.numeric(border_crossing[,5]) #datum
border_crossing[,6] <- as.numeric(border_crossing[,6]) #measure (sredstvo prijevoza)
#7. stupac je vec numeric (value) 

border_crossing$promet[border_crossing$Value > 101 &&  border_crossing$Value < 30002] <- 1
border_crossing$promet[border_crossing$Value < 102] <- 0
border_crossing$promet[border_crossing$Value > 30001] <- 2
border_crossing$promet <- factor(border_crossing$promet)

skup_no <- sample(2, nrow(border_crossing), replace=TRUE, prob=c(0.7, 0.3))
skup_za_treniranje <- border_crossing[,-7][skup_no==1,]
skup_za_testiranje <- border_crossing[,-7][skup_no==2,]

library(C50)
c50_stablo <- C5.0(promet ~ .,
                   data = skup_za_treniranje)
summary(c50_stablo)
rez <- predict(c50_stablo, newdata = skup_za_testiranje, type = "class")
table(rez, skup_za_testiranje$promet)

#Cak i ovdje je stablo uspjesno. Stablo je pomocu port code/name-a i 
#prijevoznog sredstva uvelike tocno zakljucilo koliko ljudi prolazi granicom.
#Neuronska mreza nije bila ni blizu ovoliko uspjesna. To mora da je rezultat
#lose arhitekture mreze, lose aktivacijske funkcije i tako dalje. Neuronska
#mreza ovakva kakva je nije prigodna za ovaj zadatak. Podesavanjem raznih faktora
#sigurno se moze dobiti optimalna mreza, ali moje znanje o toj temi nije
#dovoljno da to postignem. Mogu prepoznati da je mreza neprikladna kad
#obicno stablo moze rijesiti problem a mreza ne, no ne znam u cemu tocno
#je problem i kako bih ga popravio osim nasumicnim promjenama. Mislim da 
#je ta tema dosta duboka i trenutno izvan mog dosega.