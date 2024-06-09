---
  title: "Narodziny w USA w latach 2000-2014 - analiza eksploracyjna"
author: "Aleksandra Jaworska"
output:
  html_document:
  toc: yes
toc_depth: 2
toc_float:
  collapsed: no
smooth_scroll: yes
code_folding: hide
pdf_document:
  toc: yes
toc_depth: '2'
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(fivethirtyeight)
library(tidyverse)
library(yarrr)
library(lubridate)
usb <- US_births_2000_2014
```

# Wprowadzenie

Celem projektu jest eksploatacja danych dotyczących narodzin w Stanach Zjednoczonych w latach 2000-2014. 


## Obróbka danych

Dane zostały bezpośrednio wczytane z pakietu fivethirtyeight. Zawierają 5479 obseracji i 6 zmiennych. 

```{r}
usb <- US_births_2000_2014
```

## Wykorzystane zmienne

W dalszej analizie wykorzystano następujące zmienne:  
  
  | Zmienna             | Opis          
| --------------------|:-------------------------------------------
  | *year*              | rok urodzenia
| *month*               | miesiąc urodzenia
| *date_of_month*       | dzień urodzenia
| *date*                | dokładna data urodzenia
| *day_of_week*         | skrót dnia tygodnia urodzenia
| *births*              | liczba urodzin przypisanych do danej daty


Statystyki podsumowujące wybrane zmienne przedstawiają się następująco:
  
  ```{r warning=FALSE}
# zamiana niektorych zmiennych na faktory i przypisanie polskich nazw 
usb$day_of_week <- factor(usb$day_of_week)
usb$date <- factor(usb$date)
usb$month <- factor(usb$month)
levels(usb$month) <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")
levels(usb$day_of_week) <- c("Niedz", "Pon", "Wt", "Śr", "Czw", "Pt", "Sob")

# ladna tabelka z podsumowaniem (funkcja kable z pakietu knitr)
knitr::kable(usb %>% select(year, month, date_of_month, date, day_of_week, births) %>% summary)
```

Z powyższej tabeli można odczyta m.in., że rekordy są kompletne, nie posiadają braków. Co więcej 
najmniejszą liczbą osób, których narodziny przypisane są do tej samej daty jest 5728, największą 16081, a 
średni wynik tego zestawienia to 11350. Nieuwzględnione statystyki z powyższej tabeli zostaną szczegółowo omówione 
w dalszej części analizy. 


# Analiza

Przeprowadzona analiza obejmowała następujące zagadnienia:
  
  * analizę czasową - m.in. liczbę urodzin w poszczególnych latach, miesiącach i dniach tygodnia; liczbę urodzin podczas nietypowych dat - 29 lutego i piątki 13, wpływ Walentynek na zwiększoną ilość urodzeń (po 9 miesiącch); 
* analizę przynależności - m.in. ilość urodzeń przypisanych do danego znaku zodiaku;  


## Analiza czasowa

Celem tej analizy jest sprawdzenie, czy można zaobserwować jakieś wzorce związane z czasem.

```{r warning = FALSE}

#Utworzenie nowej ramki wprowadzającej podział na lata przestępne i zsumowaną liczbę urodzeń z pogrupowanych lat.
usb_year_nbirths <- usb %>% group_by(year) %>% 
  summarise(urodzeni = sum(births), n =n(), mean = mean(births)) %>% mutate(czy_przestepny = ifelse(n == 366, "Tak", "Nie"))  
usb_year_nbirths
```

### Ilość osób urodzonych w konkretnym roku z podziałem na lata przestępne

```{r}
## Ilość osób urodzonych w konkretnym roku z podziałem na lata przestępne

  
  ggplot(aes(x = year, y = urodzeni/1E6)) + 
  geom_line(col = "darkgrey") + geom_point(pch = 22, cex = 3, aes(fill = czy_przestepny))+
  scale_x_continuous(breaks = 2000:2014)+
  theme_bw() + theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("#228B22", "#CD5C5C")) + 
  labs(fill="Czy rok jest przestępny?", 
       title = "Ilość osób urodzonych w konkretnym roku z podziałem na lata przestępne",
       subtitle = "(Wartości wyrażone w mln)",
       x = "Rok urodzenia",
       y = "Ilość narodzin") + geom_line(aes(x = year, y = mean))


usb_year_nbirths %>% ggplot(aes(x = year, y = urodzeni/1E6)) + 
  geom_col(aes(fill = czy_przestepny), width = 0.7) +
  scale_x_continuous(breaks = 2000:2014) + 
  scale_y_continuous(breaks = seq(0, 4.5, 0.5)) + 
  theme_bw() + theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("#228B22", "#CD5C5C")) + 
  labs(fill="Czy rok jest przestępny", 
       title = "Ilość osób urodzonych w konkretnym roku z podziałem na lata przestępne",
       subtitle = "(wartości wyrażone w mln)",
       x = "rok urodzenia",
       y = "ilość narodzin") 




```

Najwięcej osób urodziło się w roku 2007, jak można odczytać z wyżej umieszczonej tabeli dokładnie 4380784, jednak rok ten 
nie odznacza się znaczącą różnicą względem pozostałych lat. Najmniej z kolei urodziło się w 2012 i 2013 roku, odpowiednio 4000868 i 3973337 osób.
Dodatkowo rok przestępny nie wpływa w żaden sposób na liczbę urodzeń, co można zaobserwować z wyżej przedstawionego wykresu.

### Wpływ Walentynek (14 luty) na ilość urodzeń (odpowiednio po 9 miesiącach)

```{r, fig.width = 10, fig.height=10, warning = FALSE}
## Wpływ Walentynek (14 luty) na ilość urodzeń (odpowiednio po 9 miesiącach)

#Utworzenie nowej ramki danych wyodrębniającej urodzenia powalentynkowe  
valentines_day_result <- usb %>% group_by(month, date_of_month) %>% summarise(n = sum(births)) %>% mutate(k = ifelse(month == "Listopad" & date_of_month %in% 7:21, "red", "white"))

valentines_day_result %>% 
  ggplot(aes(x = date_of_month, y = n, fill = n)) + geom_col(aes(col = k), width = 0.8, lwd = 0.8)+ scale_color_manual(values = c("#3639BA", "white"))+
  facet_grid(rows = vars(month))  +
  theme_bw() +  scale_x_continuous(breaks = 1:31) + 
  labs(title = "Ilość urodzeń w dniach poszczególnych miesięcy",
       subtitle = "Z zaznaczonymi urodzeniami powalentynkowymi",
       x = "Dzień miesiąca", 
       y = "Urodzenia", 
       fill = "suma urodzeń w poszczególnych 
dniach miesięcy 
z lat 2000-2014") + guides(color = "none") + scale_fill_gradient(low = "#E74C3C", high = "#1E90FF") + 
  theme(strip.background = element_rect(fill = alpha('blue', 0.3))) 

```

Osoby poczęte 14 lutego szacunkowo powinny urodzić się 14 listopada, jednak dla elastyczności analizy, przedział ten 
poszerzony został o +/- 7 dni. Jak można zauważyć na wykresie, w podanych przedziałach czasowych nie kreuje się wyraźnie
zwiększona tendencja wzrostowa. Świadczy to o tym, iż Walentynki nie mają znaczącego wpływu na wzrost liczby urodzeń. 


### Liczba urodzeń z podziałem na miesiące

```{r warning = FALSE}
### Liczba urodzeń z podziałem na miesiące ###

usb %>% ggplot(aes(x = month, y = births/1E6)) +
  geom_col(fill = "orange", width = 0.5) +
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
  xlab("Miesiące") +
  ylab("Liczba w mln") +
  ggtitle("Liczba wszystkich urodzeń z podziałem na miesiące w USA w latach 2000-2014") +
  theme_bw()

```

W wyżej przedstawionym wykresie widać, że nie występuje znaczna różnica liczby urodzeń w każdym z miesięcy, 
największym wynikiem jest sierpień z liczbą 5540170, natomiast najmniejszą liczbą jest 4725693, 
które zostało odnotowane w Lutym.

### Liczba urodzeń, w danym dniu tygodnia

```{r fig.width = 8, fig.height=6, warning = FALSE}
## Liczba urodzeń z podziałem na dzień tygodnia 

usb %>% ggplot(aes(x = day_of_week, y = births/1E6)) +
  geom_col(aes(fill= day_of_week), width = 0.5, show.legend = F) +
  scale_y_continuous(breaks = seq(0, 11, 1)) +
  xlab("Dzień Tygodnia") +
  ylab("Liczba w mln") +
  ggtitle("Liczba wszystkich urodzeń z podziałem na dzień tygodnia w USA w latach 2000-2014") +
  theme_bw()
```

Najwięcej osób urodziło się we wtorek (10274874), najmniej natomiast urodziło się w niedzielę (5886889). Większość dni tygodnia ma zbliżoną do siebie liczbę urodzeń, jednak w weekend, liczba urodzeń jest mniejsza niż w pozostałe dni.

### Ilość osób urodzonych w piątek trzynastego

``` {r warning = FALSE}

## Tabela przedstawiająca ilość osób urodzonych w piątek trzynastego 

usb %>% filter(date_of_month == 13, day_of_week == "Pt") %>% 
  group_by(zodiac)  %>% 
  summarise(n = sum(births))  %>% ggplot(aes(x = zodiac, y = n)) + geom_line(group = 1, color = "darkgrey")+
  geom_point(pch = 21, fill = "#9040b3", cex = 4, col = "#9040b3") + labs(title = "Ilość osób urodzonych w piątek trzynastego",
                                                subtitle = "Z podziałem na odpowiadające datom znaki zodiaku",
                                                x = "Znak zodiaku", 
                                                y = "Suma urodzeń") + theme_bw() + scale_y_continuous(breaks = seq(0, 40000, 2000))
usb %>% filter(date_of_month == 13, day_of_week == "Pt") %>% 
  group_by(zodiac)  %>% 
  summarise(n = sum(births))  

Najwięcej osób urodzonych w piątek trzynastego jest spod znaku raka (ok. 37500), niewiele mniej spod znaku bliźniąt (ok. 36000) i barana (ok. 34000), 
te trzy znaki zodiaku wyraźnie wybijają się na tle pozostałych. 

Najmniej osób urodzonych w piątek trzynastego jest spod znaku skorpiona (ok. 11500) i niewiele więcej spod znaku ryb (ok. 12000). 

Dzięki tej analizie możemy więc zaobserwować, że statystycznie najwięcej piątków trzynastego występowało pomiędzy 21.03-19.04 oraz 20.05-22.07 (przedziały czasowe odpowiadające znakom zodiaku z największą 
sumą urodzeń w piątek trzynastego), a najmniej pomiędzy 23.10-21.11 oraz 19.02-20.03.


## Tabela zliczająca liczbę występujących piątków trzynastego
usb %>% filter(date_of_month == 13, day_of_week == "Pt") %>% group_by(month) %>% summarise(n = n())



ifelse(month == "Czerwiec" & date_of_month %in% 21:30 | month == "Lipiec" & date_of_month %in% 1:22
```ifelse(month == "Październik" & date_of_month %in% 23:31 | month == "Listopad" & date_of_month %in% 1:21, "skorpion"

Powyższa tabela pokazuje, że najwięcej osób urodziło się w Lipcu 2007 roku i liczba ta wynosiła 13228	, natomiast najmniej osób urodziło się w Kwietniu 2001 roku i dokładnie liczba ta wynosiła 10881.

### Ilość osób urodzonych 29 lutego, w różnych latach

``` {r warning = FALSE}

## Wykres pokazujący liczbę osób urodzonych 29 lutego, w różnych latach

usb %>% filter(month == "Luty", date_of_month == 29) %>% 
  group_by(year) %>% 
  ggplot(aes(x = year, y = births)) +
  geom_col(color = "black", fill = "orange", width = 3) +
  scale_x_continuous(breaks = seq(2000, 2012, 4)) +
  scale_y_continuous(breaks = seq(0, 12000, 1000)) +
  xlab("Rok") +
  ylab("Liczba") +
  ggtitle("Liczba urodzonych 29 lutego, w różnych latach") +
  theme_bw()

```

Z wykresu wynika, że najwięcej osób urodziło się 2000 roku z liczbą 11895, najmniej natomiast w 2004 roku z wynikiem 7301. W 3 na 4 podanych latach widać, że liczba osób urodzonych jest bardzo zbliżona do siebie i wacha się między 11, a 12 tysięcy osób, jednak w jednym pozostałym roku liczba urodzonych jest znacznie mniejsza od pozostałych.


## Analiza przynależności

W tej części sprawdzono przynależność do znaku zodiaku badanych, a także to, czy można zaobserwować związek pomiędzy posiadaniem danego znaku zodiaku a urodzeniem podczas nietypowej daty. 

### Przynależność do znaku zodiaku

```{r warning = FALSE}

#Utworzenie nowej kolumny z pdziałem na znaki zodiaku

usb <- usb %>% mutate(zodiac = ifelse(month == "Marzec" & date_of_month %in% 21:31 | month == "Kwiecień" & date_of_month %in% 1:19, "baran", 
                                      ifelse(month == "Kwiecień" & date_of_month %in% 20:30 | month == "Maj" & date_of_month %in% 1:20, "byk", 
                                             ifelse(month == "Maj" & date_of_month %in% 21:31 | month == "Czerwiec" & date_of_month %in% 1:20, "bliźnięta", 
                                                    ifelse(month == "Czerwiec" & date_of_month %in% 21:30 | month == "Lipiec" & date_of_month %in% 1:22, "rak", 
                                                           ifelse(month == "Lipiec" & date_of_month %in% 23:31 | month == "Sierpień" & date_of_month %in% 1:22, "lew", 
                                                                  ifelse(month == "Sierpień" & date_of_month %in% 23:31 | month == "Wrzesień" & date_of_month %in% 1:22, "panna", 
                                                                         ifelse(month == "Wrzesień" & date_of_month %in% 23:30 | month == "Październik" & date_of_month %in% 1:22, "waga", 
                                                                                ifelse(month == "Październik" & date_of_month %in% 23:31 | month == "Listopad" & date_of_month %in% 1:21, "skorpion", 
                                                                                       ifelse(month == "Listopad" & date_of_month %in% 22:30 | month == "Grudzień" & date_of_month %in% 1:21, "strzelec",  
                                                                                              ifelse(month == "Grudzień" & date_of_month %in% 22:31 | month == "Styczeń" & date_of_month %in% 1:19, "koziorożec", 
                                                                                                     ifelse(month == "Styczeń" & date_of_month %in% 20:31 | month == "Luty" & date_of_month %in% 1:18, "wodnik", "ryby"))))))))))))


#Wykres kolumnowy pokazujący udział procentowy poszczególnych znaków zodiaku

usb %>% group_by(zodiac) %>% summarise(n = sum(births), procent = round(sum(births)/62187024*100, 2)) %>% ggplot(aes(x = "", y = procent, fill= factor(zodiac))) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y", start=0) + geom_label(aes(label = procent, "%"), color="white", position = position_stack(vjust = 0.3), show.legend = F)+
  guides(fill = guide_legend(title = "znak zodiaku")) + theme_bw() + ggtitle("Procentowy udział znaków zodiaku") + ylab("") + xlab("")  

```

Do ramki danych dodano kolumnę, która za pomocą funkcji ifelse określa znak zodiaku dla danej daty urodzenia. Utworzono również wykres, na którym ukazano procentowy rozkład tych znków.  

Z wykresu wynika, iż najwięcej osób jest spod znaku raka (9%), a najmniej spod znaku koziorożca (7,5%). Różnice pomiędzy pozostałymi znakami zodiaku są znikome i oscylują w granicach części dziesiętnych.


# Wnioski

1. Analiza czasowa nie wykazuje żadnych wzorców związanych z czasem.
2. Nie występuje znaczna różnica między liczbą urodzeń względem każdego miesiąca.
3. W weekendy rodzi się mniej dzieci niż w dni powszednie. Z czego najmniej rodzi się w niedziele. Potencjalnie lekarze są bardziej sklonni do asystowania przy porodzie w dni powszednie niż w weekendy. 
4. W latach przestępnych nie występują żadne wzorce związane z czasem, natomiast jest widoczna znaczna różnica liczby urodzeń w roku 2004, względem pozostałych lat.
5. Przy znakach zodiaku również nie występuje żaden wzorzec związany z czasem, nie występuje znaczna różnica między ilośćią osób różnych znaków zodiaku, większość danych oscyluje w okolicach 8%.


# Co dalej?

W dalszych analizach możnaby uwzględnić:
  
  * Analiza płci, obliczyć której płci jest więcej (potrzeba skorzystania z dodatkowych danych), 
* Analiza przestrzenna, dotycząca miejsca urodzenia (również potrzeba skorzystania z dodatkowych danych),
* Analiza przynależności poszczególnych dat do chińskich znaków zodiaku (potrzeba utworzenia nowej kolumny z odpowiednimi warunkami), 

  
  
