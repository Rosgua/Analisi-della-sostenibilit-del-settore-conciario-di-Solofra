##librerie
library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)
library(readxl)

#carico i dati
data <- read_xlsx("C:\\Users\\rosse\\OneDrive\\Desktop\\Tesi\\DataConcerie.xlsx")
str(data)
########Preparazione dei dati###############
data$n_dipendenti <- as.numeric(data$n_dipendenti)
data$fatturato_annuo <- as.numeric(data$fatturato_annuo)
data$volume_produzione <- as.numeric(data$volume_produzione)
#data$volume_produzione <- factor(data$volume_produzione,
 #                                levels = c(1,2,3),
  #                               labels = c("<5000 m2", "5000–10000 m2", ">10000 m2"))
data$consumo_acqua <- factor(data$consumo_acqua, 
                             levels= c(1,2,3),
                             labels = c("<5000 m³", "5000–15000 m³", ">15000 m³"))
data$consumo_energia <- factor(data$consumo_energia, 
                               levels= c(1,2,3),
                               labels = c("<50000 kWh", "50000–150000 kWh", ">150000 kWh"))
data$gestione_acque_reflue <- factor(data$gestione_acque_reflue,
                                     levels= c('Internamente','Esternamente'))
data$investimenti_ambientali <- factor(data$investimenti_ambientali,
                                       levels= c(1,2,3),
                                       labels = c("<5000 ", "5000–20000 ", ">20000 "))
data$formazione_ambientale <- factor(data$formazione_ambientale, levels= c('Si','No'))
data$welfare <- factor(data$welfare, levels= c('Si','No'))
str(data)

ca<- c("iso 14001", "lwg", "esg ecowadis", "sost", "trc_sc_410", "uni en 16484", " trc_pc_412" )

ca_vars <- ca %>%
  map(~ expr(ifelse(str_detect(tolower(certificazioni_ambientali), !!.x), 1, 0))) %>%
  set_names(paste0("has_", gsub("\\s|/", "_", ca)))

cs<- c("iso 450001", "unic codice condotta", "sa 8000")

cs_vars <- cs %>%
  map(~ expr(ifelse(str_detect(tolower(certificazioni_sociali), !!.x), 1, 0))) %>%
  set_names(paste0("has_", gsub("\\s|/", "_", cs)))

data <- data %>%
  mutate(!!!ca_vars)%>%
  mutate(!!!cs_vars)

data %>% select(starts_with("has_")) %>% glimpse()

processi <- c("cromo", "vegetale", "free-metal", "misto", "altri")

proc_vars <- processi %>%
  map(~ expr(ifelse(str_detect(tolower(tipo_processo_concia), !!.x), 1, 0))) %>%
  set_names(paste0("proc_", gsub("-", "_", processi)))

data <- data %>%
  mutate(!!!proc_vars)

data <- data %>%
  mutate(sensibilità_sostenibilita = rowSums(select(., starts_with("has_")), na.rm = TRUE)/10)

data %>%
  select(Nome, sensibilità_sostenibilita) %>%
  arrange(desc(sensibilità_sostenibilita))


data <- data %>%
  mutate(has_welfare_formazione = 
           ifelse(welfare == "Si" & formazione_ambientale == "Si", 1, 0))

data$sensibilità_sostenibilita_fac <-  factor(data$sensibilità_sostenibilita, ordered = TRUE)


#######################eda##################
#riepiloghi descrittivi

data %>%
  select(where(~ !is.character(.))) %>%
  summary()
#anno di fondazione copre una arco temporale di 40 anni dal 1970 al 2013
#n_dip è molto variabile, la media è di circa 42 però il min è 14 e il max è 115.
#Questo suggerisce la presenza sia di piccole imprese che di realtà più strutturate.
#lo stesso vale per il vol di  produzione che varia di circa 400.000 mq, tra la media e la mediana 
#non abbiam molta differenza --> no valori estremi. il 3 quartile ci dice che il 75% delle imprese
#ha vlm minore di 278709.
#anche per il fatturato abbiamo molta variabilità , il min è di meno di 5mln e il
#max è più di 26mln  con una differenza di più di 20mln
#comsumo acqua La maggior parte delle aziende ha un consumo d'acqua elevato o molto elevato. 
#Solo 3 rientrano nella fascia intermedia. Nessuna nella fascia più bassa.
#consumo energia Quasi tutte le aziende ha un consumo energetico molto elevato.
#Solo una nella fascia intermedia.
#gestione acque Tutte le aziende gestiscono le acque reflue esternamente.
#investimenti La maggior parte delle aziende fa investimenti elevati, 1 si trova nella
#fascia intermedia, 1 in quella bassa e 1 non ha rilasciato informazioni lasciandoci
#assumere che non faccia investimenti.
#La maggior parte offrono programmi di formazione ambientale e welfare.
#has iso 14001 la media è 67% quindi più della metà la possiede
#has lwg , la media è 1 ciò indica che tutte le aziende la possiedono
#has sost è pari a 2 indicandoci che 2 la possiedono
#has eco l media è del 11% ciò mostra che è psseduta da 1 azienda, cosi come has trc sc 410,
#uni en 16484, trc pc 412
#per quanto riguarda le certificazioni sociali:
#has iso 45001, la media è 11% ciò mostra che è psseduta da 1 azienda 
#lo stesso per unic codice condotta
#sa 8000 ha media 0 perche nessuno la possiede
#Si osserva una forte discrepanza tra cert ambentali e sociali che evidenziano 
#un disimpegno per le seconde.
#per quanto riguarda i processi più della metà ha pricesso di tipo cromo (63%)
#il 25 % ha processo vegetale e lo stesso per il processo free metal, il 50% ha processo misto
#un'azienda ha altri tipi.
#l'indice di sost è abbastanza variabile, da 0.1 a 0.6 indicando concerie che hanno 1 o 2 cert
# e concerie che le possiedono quasi tutte.
# welfare e formazione ha media 0.56 quindi più della metà hanno entrambi

#tabelle di contigenza
table(data$proc_cromo, data$consumo_acqua) 
#su 5 aziende che usano processo al cromo 3 hanno consumo fascia media e 2 fascia alta
#le 3 aziende che non lo usano hanno tutte consumo alto
table(data$proc_vegetale, data$consumo_acqua)#su 2 aziende che usano processo al vegetale entrambe hanno consumo alto
#tra le 6 aziende che non lo usano, 3 hanno consumo medio e 3 consumo alto 
table(data$proc_misto, data$consumo_acqua)
#su 4 aziende che usano processo al cromo 1 hanno consumo fascia media e 3 fascia alta
#tra le 4 aziende che non lo usano 2 ha consumo medio e 2 hanno consumo alto

#su 5 aziende che usano processo al cromo 1 hanno consumo fascia media e 4 fascia alta
#le 3 aziende che non lo usano hanno tutte consumo alto di energia
table(data$proc_cromo, data$consumo_energia)
#su 2 aziende che usano processo al vegetale entrambe hanno consumo di energia in fascia alta
#tra le 6 aziende che non lo usano, 1 ha consumo medio e 5 alto
table(data$proc_vegetale, data$consumo_energia)
#su 4 aziende che usano processo misto 1 ha consumo fascia media e 3 fascia alta
#le 4 aziende che non lo usano hanno tutte consumo alto
table(data$proc_misto, data$consumo_energia)
#questi risultai sono contro intuitivi però non significativi perche abbiamo un numero ridotto di oss
table(data$has_iso_14001, data$consumo_acqua)

table(data$has_esg_ecowadis, data$consumo_acqua)

table(data$has_iso_14001, data$consumo_energia)

table(data$has_esg_ecowadis, data$consumo_energia)

table(data$has_iso_14001, data$investimenti_ambientali)

table(data$has_esg_ecowadis, data$investimenti_ambientali)
#grafici a barre
data %>%
  ggplot(aes(certificazioni_ambientali))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione delle Certificazioni Ambientali")

data %>%
  ggplot(aes(certificazioni_sociali))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione delle Certificazioni Sociali")

data %>%
  group_by(tipo_processo_concia) %>%
  summarise(n = n())

data %>%
  group_by(gestione_acque_reflue) %>%
  summarise(n = n())

data %>%
  ggplot(aes(consumo_acqua))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione del Consumo di Acqua")

data %>%
  ggplot(aes(consumo_energia))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione del Consumo di Energia")

data %>%
  ggplot(aes(materie_prime))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione delle Materie Prime")

data %>%
  ggplot(aes(investimenti_ambientali))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione degli Investimenti Ambientali")

data %>%
  ggplot(aes(formazione_ambientale))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione della Formazione Ambientale")

data %>%
  ggplot(aes(welfare))+
  geom_bar(fill= "steelblue") +
  labs(title = "Distribuzione del Welfare")

data%>%
  ggplot( aes(Nome,sensibilità_sostenibilita_fac)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Distribuzione dell'Indice di Sostenibilità",x = "Nome Azienda", y = "Sensibilità Sostenibilità")


#relazione indice sostenibilità vs tipo di processo della concia
#attraverso un grafico a barre
data%>%
  group_by(tipo_processo_concia)%>%
  summarise(media_sost = mean(sensibilità_sostenibilita, na.rm = TRUE)) %>%
  arrange(media_sost)%>%
  ggplot(aes(tipo_processo_concia, media_sost))+
  geom_col(fill= 'darkviolet')+
  coord_flip()+
  labs(title= "Indice di sostenibilità medio per tipo di processo")

#indice di sostenbilita vs welfare e formazione
#attraverso un boxplot
data%>%
  ggplot(aes(x = factor(has_welfare_formazione, levels = c(0,1), labels = c("No Welfare/Formazione", "Sì Welfare/Formazione")),
             y = sensibilità_sostenibilita,
             fill = factor(has_welfare_formazione))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuzione dell'Indice di Sostenibilità per Welfare e Formazione",
       x = "Welfare e Formazione Ambientale",
       y = "Indice di Sostenibilità") +
  theme_minimal()

#relazione tra fatturato e indice di sostenibilita
data %>%
  ggplot(aes(x = sensibilità_sostenibilita, y = fatturato_annuo)) +
  geom_point(size = 3, color = 'black') +
  geom_smooth(method = "lm", se = FALSE, color = 'red') + # Linea di regressione
  labs(title = "Fatturato vs. Indice di Sostenibilità",
       x = "Indice di Sostenibilità",
       y = "Fatturato ") +
  theme_minimal()
#osservo che al crescere dell'indice di sost , aumenta il fatturato
 
data %>%
  ggplot(aes(x = sensibilità_sostenibilita, y = volume_produzione)) +
  geom_point(size = 3, color = 'black') +
  geom_smooth(method = "lm", se = FALSE, color = 'red') + # Linea di regressione
  labs(title = "Volume produzione vs. Indice di Sostenibilità",
       x = "Indice di Sostenibilità",
       y = "Volume di produzione") +
  theme_minimal()
data %>%
  ggplot(aes(x = volume_produzione, y = fatturato_annuo)) +
  geom_point(size = 3, color = 'black') +
  geom_smooth(method = "lm", se = FALSE, color = 'red') + # Linea di regressione
  labs(title = "Volume di produzione vs. fatturato",
       x = "Volume produzione",
       y = "Fatturato") +
  theme_minimal()
Dopo aver trattato questi dati, possiamo sostenere che i consumi delle risorse sono in linea con

```{r}
data%>%
  select(volume_produzione, consumo_energia, consumo_acqua)%>%
  arrange(volume_produzione)
in base a questa suddivisione dovrei apportare qualche modifica all'introduzione del capitolo 3.4 oppure lascio cosi com'è ? ##3.4 Analisi Descrittiva- Univariata
Completata la fase di pulizia, trasformazione e costruzione di variabili derivate, è ora possibile procedere con l’analisi descrittiva. Questo passaggio ha l’obiettivo di fornire una sintesi preliminare dei dati raccolti, consentendo di individuare tendenze, distribuzioni frequenti e possibili anomalie.

A seconda della natura dei dati (numeriche, ordinali o nominali), vengono adottati strumenti di analisi differenti. 
Le variabili ordinal

################## correlazione ##############
data %>%
  select(n_dipendenti, fatturato_annuo, sensibilità_sostenibilita) %>%
  cor(use = "pairwise.complete.obs")
#mostra correlazione tra num di dipendenti e fatturato 
#mentre non c'è tra n_dip e l'indice di sostenibilita 
# e soprattutto tra il fatturato e indiced di sost




### Relazione tra tipologia di processo e consumo idrico
```{r echo=TRUE}
barplot(table(data$proc_cromo, data$consumo_acqua), col= c(2,3))
```
Tra le 5 aziende che utilizzano il processo al cromo, 3 si collocano nella fascia media di consumo idrico, mentre 2 registrano consumi elevati. Al contrario, tutte e 3 le aziende che non adottano questo processo si trovano nella fascia di consumo più alta.

```{r echo=TRUE}
barplot(table(data$proc_vegetale, data$consumo_acqua), col = c(2,3))

```
Fra le 3 aziende che adottano la concia vegetale, 2 hanno un consumo idrico elevato e 1 medio. Tra le 3 che non la adottano, 2 presentano un consumo medio e 3 un consumo elevato.

```{r echo=TRUE}
barplot(table(data$proc_misto, data$consumo_acqua), col = c(2,3))

```
su 4 aziende che usano processo al cromo, 1 hanno consumo fascia media e 3 fascia alta
tra le 4 aziende che non lo usano 2 ha consumo medio e 2 hanno consumo alto

### Relazione tra processo e consumo energetico
```{r echo=TRUE}
barplot(table(data$proc_cromo, data$consumo_energia), col = c(2,3))
```
Quattro delle cinque aziende che usano il cromo registrano consumi energetici elevati, una si colloca nella fascia media. Le aziende che non lo adottano presentano tutte consumi elevati.

```{r echo=TRUE}
barplot(table(data$proc_vegetale, data$consumo_energia), col= c(2,3))

```
Tra le aziende che adottano la concia vegetale 2 mostrano consumi elevati e 1 medio. Tra quelle che non la adottano, hanno tutti consumo alti.

```{r}
barplot(table(data$proc_misto, data$consumo_energia), col = c(2,3))
```
Tra le aziende con processo misto, tre hanno un alto consumo energetico e una un consumo medio. Tutte le aziende che non usano questo processo mostrano consumi elevati.

### Relazione tra certificazioni e consumo
```{r}
barplot(table(data$has_iso_14001, data$consumo_acqua), col = c(2,3))
```

questi risultai sono contro intuitivi però non significativi perche abbiamo un numero ridotto di oss

```{r}
barplot(table(data$has_esg_ecowadis, data$consumo_acqua), col = c(2,3))
```

```{r}
barplot(table(data$has_iso_14001, data$consumo_energia), col = c(2,3))
```
```{r}
# Seleziona tutte le variabili dummy delle certificazioni e processi
cert_proc_vars <- data %>%
  select(starts_with("has_"), starts_with("proc_"))

# Crea la tabella riepilogativa con somma dei valori (quante aziende hanno ciascuna caratteristica)
summary_table <- cert_proc_vars %>%
  summarise(across(everything(), ~sum(. == 1))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variabile",
               values_to = "Numero_aziende")

# Ordina per numero di aziende decrescente (facoltativo)
summary_table <- summary_table %>%
  arrange(desc(Numero_aziende))

# Visualizza
print(summary_table)

```

```{r}
barplot(
  table(data$has_esg_ecowadis, data$consumo_energia), col = c(2,3)
)
```
```{r}
barplot(
  table(data$has_iso_14001, data$investimenti_ambientali), col = c(2,3))
```
```{r}
barplot(table(data$has_esg_ecowadis, data$investimenti_ambientali), col = c(2,3))
```


```{r}
data %>%
  select(-c(Nome, certificazioni_ambientali, 
            certificazioni_sociali, tipo_processo_concia, gestione_acque_reflue, materie_prime, formazione_ambientale, welfare))%>%
  summary()
```
```{r}
boxplot(data$n_dipendenti)
hist(data$n_dipendenti)
```

in base a questa suddivisione dovrei apportare qualche modifica all'introduzione del capitolo 3.4 oppure lascio cosi com'è ? ##3.4 Analisi Descrittiva- Univariata
  Completata la fase di pulizia, trasformazione e costruzione di variabili derivate, è ora possibile procedere con l’analisi descrittiva. Questo passaggio ha l’obiettivo di fornire una sintesi preliminare dei dati raccolti, consentendo di individuare tendenze, distribuzioni frequenti e possibili anomalie.

A seconda della natura dei dati (numeriche, ordinali o nominali), vengono adottati strumenti di analisi differenti. 
Le variabili ordinal


``{r echo=TRUE}
barplot(table(data$certificazioni_sociali, data$welfare), col= c(2,4))
```

```{r}
barplot(table(data$formazione_ambientale, data$investimenti_ambientali), col= c(2,4))

















