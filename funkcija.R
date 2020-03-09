
library("ggplot2")
library("magrittr")

#set.seed(1)

#sliderji
max_knjig <- 10
utezi_za_knjige <- c(0.35, 0.23, 0.15, 0.1, 0.07, 0.05, 0.03, 0.02)


cas_odnasanja_k_knjig <- function(k){
  40 + 15*k
}


#GLAVNA FUNKCIJA, KI NAREDI SKUPNO TABELO
simulacija_prihodov <- function(t, parameter) { 
  
  # Dataframe, ki vsebuje čase prihodov in število prinešenih knjig
  stranke_prihodi_knjige <- data.frame(rexp(1, parameter), sample(c(1:8), size = 1, prob = utezi_za_knjige))
  names(stranke_prihodi_knjige) <- c("cas_vstopa","st_knjig")
  
  while(tail(stranke_prihodi_knjige[,1],1) < t) {
    cas_novega_skoka <- tail(stranke_prihodi_knjige[,1],1) + rexp(1, parameter)
    prinesene_knjige <- sample(c(1:8), size = 1, prob = utezi_za_knjige)
    stranke_prihodi_knjige[nrow(stranke_prihodi_knjige)+1,] <- c(cas_novega_skoka, prinesene_knjige)
  }
  
  
  # glavna_tabela
  cas_prvega_prihoda <- stranke_prihodi_knjige[1,1]
  st_prvih_knjig <- stranke_prihodi_knjige[1,2]
  
  if(st_prvih_knjig < max_knjig){
    pospravljanje = 0
  }else{
    pospravljanje = cas_odnasanja_k_knjig(st_prvih_knjig)
  }
  
  skupna_tabela <- data.frame("vrstni_red" = 1, 
                              "cas_prihoda" = cas_prvega_prihoda, 
                              "st_prinesenih_knjig" = st_prvih_knjig, 
                              "cas_zacetka_strezbe" = cas_prvega_prihoda, 
                              "dolzina_strezbe" = 3 * st_prvih_knjig + runif(1, min = 10, max = 30),
                              "cas_odhoda" = cas_prvega_prihoda + 3 * st_prvih_knjig + runif(1, min = 10, max = 30),
                              "st_strank_v_knjiznici" = 1, 
                              "skupno_stevilo_knjig" = st_prvih_knjig, 
                              "cas_knjiznicarja" = pospravljanje)
  
  
  # za vsako novo stranko posodobimo tabelo
  
  n <- 2
  
  while(n <= length(stranke_prihodi_knjige[,1])){
    
    #cas prihoda in število prinesenih knjig n-tega posameznika
    cas_trenutnega_prihoda <- stranke_prihodi_knjige[n,1]
    st_prinesenih_knjig <- stranke_prihodi_knjige[n,2]
    
    # skupno stevilo knjig in cas_za pospravljanje
    novo_stanje_knjig_pred_pospravljanjem <- skupna_tabela$skupno_stevilo_knjig[n-1] + st_prinesenih_knjig
    
    if(novo_stanje_knjig_pred_pospravljanjem >= max_knjig){
      st_knjig <- 0
      pospravljanje <- cas_odnasanja_k_knjig(novo_stanje_knjig_pred_pospravljanjem)
    }
    else{
      st_knjig <- novo_stanje_knjig_pred_pospravljanjem
      pospravljanje <- 0
    }
    
    # število ljudi v knjižnici / v vrsti 
    prejsnji_prihod <- stranke_prihodi_knjige[n - 1,1]
    stevilo_obravnavanih <- length(which((skupna_tabela$cas_odhoda >= prejsnji_prihod) & 
                                           (skupna_tabela$cas_odhoda < cas_trenutnega_prihoda)))
    st_strank_v_knjiznici <- skupna_tabela$st_strank_v_knjiznici[n-1] + 1 - stevilo_obravnavanih
    
    # začetek obravnave
    zacetek <- max(skupna_tabela$cas_odhoda[n-1] + skupna_tabela$cas_knjiznicarja[n-1], stranke_prihodi_knjige[n,1])
    
    # čas strežbe
    if(st_strank_v_knjiznici > 1){
      cas_obravnave <- 3 * st_prinesenih_knjig
    }else{
      dodaten_cas <- runif(1, min = 10, max = 30)
      cas_obravnave <- 3 * st_prinesenih_knjig + dodaten_cas
    }
    
    
    #dodajanje
    trenutni_obiskovalec <- c(n, cas_trenutnega_prihoda, st_prinesenih_knjig, zacetek, cas_obravnave, 
                              zacetek + cas_obravnave, st_strank_v_knjiznici, st_knjig, pospravljanje)
    
    skupna_tabela <- rbind(skupna_tabela, trenutni_obiskovalec)
    
    n <- n + 1
  }
  skupna_tabela
}






#ANALIZA

cas_opazovanja <- 1000
tabela <- simulacija_prihodov(cas_opazovanja, 0.02)
tabela_brez_zadnjega <- tabela[1:(length(tabela[,1]) - 1),]

glavna_tabela <- rbind(c(0,0,0,0,0,0,0,0,0,0), tabela_brez_zadnjega)

# graf, ki prikazuje, kako se spreminja število ljudi v knjižnici.
tabela_aux_casi1 <- data.frame(glavna_tabela$cas_prihoda[2:length(glavna_tabela$cas_prihoda)], 1)
names(tabela_aux_casi1) <- c("cas", "prihod/odhod")
tabela_aux_casi2 <- data.frame(glavna_tabela$cas_odhoda[2:length(glavna_tabela$cas_odhoda)], -1)
names(tabela_aux_casi2) <- c("cas", "prihod/odhod")
tabela_stevila_strank_tocna <- rbind(c(0,0), tabela_aux_casi1, tabela_aux_casi2)
tabela_stevila_strank_tocna <- tabela_stevila_strank_tocna[order(tabela_stevila_strank_tocna$`cas`),]
tabela_stevila_strank_tocna$stevilo_strank_v_knjiznici <- cumsum(tabela_stevila_strank_tocna$`prihod/odhod`)
tabela_stevila_strank_tocna$razlika <- c(diff(tabela_stevila_strank_tocna$cas),0)

ggplot(data = tabela_stevila_strank_tocna) +
  geom_step(mapping = aes(x = cas, y = stevilo_strank_v_knjiznici)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), cas_opazovanja/10))

# Koliko strank pride v enem dnevu?
stevilo_strank_v_celem_dnevu <- length(glavna_tabela[,1]) - 1


# Koliko časa je knjižničar brez dela?
tabela_brezdelja <- data.frame(glavna_tabela$cas_prihoda, glavna_tabela$cas_odhoda, glavna_tabela$cas_knjiznicarja)
tabela_brezdelja$cas_naslednjega_prihoda <- c(glavna_tabela$cas_prihoda[2:length(glavna_tabela$cas_prihoda)], cas_opazovanja)
names(tabela_brezdelja) <- c("cas_prihoda", "cas_odhoda", "cas_knjiznicarja", "cas_naslednjega_prihoda")
tabela_brezdelja$cas_brezdelja <- tabela_brezdelja$cas_naslednjega_prihoda - (tabela_brezdelja$cas_odhoda + tabela_brezdelja$cas_knjiznicarja)

cas_brezdelja <- sum(tabela_brezdelja$cas_brezdelja[tabela_brezdelja$cas_brezdelja >= 0])

# graf, ki ponazarja, kdaj je knjižničar brez dela
strnjena_tabela_brezdelja <- tabela_brezdelja[tabela_brezdelja$cas_brezdelja >= 0,]
aux1_brezdelje <- data.frame(strnjena_tabela_brezdelja$cas_odhoda + strnjena_tabela_brezdelja$cas_knjiznicarja, 0)
names(aux1_brezdelje) <- c("cas", "stanje")
aux2_brezdelje <- data.frame(strnjena_tabela_brezdelja$cas_naslednjega_prihoda, 1)
names(aux2_brezdelje) <- c("cas", "stanje")

tabela_brezdelje_graf <- rbind(aux1_brezdelje, aux2_brezdelje) 
tabela_brezdelje_graf <- tabela_brezdelje_graf[order(tabela_brezdelje_graf$cas),]
tabela_brezdelje_graf <- rbind(tabela_brezdelje_graf, c(cas_opazovanja, tabela_brezdelje_graf$stanje[length(tabela_brezdelje_graf)]))

ggplot(data = tabela_brezdelje_graf) + 
  geom_step(mapping = aes(x = cas, y = stanje)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), cas_opazovanja/10))


# graf, koliko časa knjižničar nima dela



# VPRAŠANJA:
# Do kdaj dela, če zaklene vrata ob šestih?
# Koliko knjig vrne v enem dnevu?
# Koliko časa ljudje čakajo v vrsti?
# Koliko časa čaka posamezen človek?

# POTREBNO SPREMENITI:
# skupni čas knjižničarja brez dela v odvisnosti časa
# Čas strežbe je v sekundah -> spremeni diskretno v zvezno porazdelitev
