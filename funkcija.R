# Documentacija:
# max_knjig ......................... po največ koliko vrnjenih knjigah knjižničar odnese vse knjige na police.
# utezi_za_knjige ................... verjetnosti, da posameznik prinese eno, dve... ali osem knjig
# cas_za_vracanje_ene_knjige ........ čas, da stranka izroči knjigo knjižničarju (knjiga v tistem trenutnku še vedno na šalterju)
# cas_za_izposojo_ene_knjige ........ čas, da knjižničar pofočka knjigo in jo izroči stranki
# trajanje_klica_rezervacije ........ trajanje klica, ko stranka pri knjižničarju naredi rezervacijo.
# cas_odnasanja_k_knjig ............. funkcija, ki vrne čas, da knjižničar pravilno razvrsti k knjig iz šalterja na police 
# 
# stranke_prihodi_knjige ............ tabela, v kateri so zabeleženi prihodi strank in število knjig, ki ji prinesejo
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

library("ggplot2")
library("magrittr")
library("dplyr")
library("rvest")

#set.seed(1)

#sliderji
max_knjig <- 10 
utezi_za_knjige <- c(0.35, 0.23, 0.15, 0.1, 0.07, 0.05, 0.03, 0.02)
cas_za_vracanje_ene_knjige <- 3
cas_za_izposojo_ene_knjige <- 3
povprecno_trajanje_klica_rezervacije <- 20 
intenziteta_prihodov <- 0.04
intenziteta_klicov <- 0.01

cas_odnasanja_k_knjig <- function(k){
  50 + 10*k
}


#GLAVNA FUNKCIJA, KI NAREDI SKUPNO TABELO
simulacija_prihodov <- function(t, parameter_prihodov, parameter_klicov) { 
  
  # tabela, ki vsebuje čase prihodov in število prinešenih knjig
  stranke_prihodi_knjige <- data.frame(rexp(1, parameter_prihodov), sample(c(1:8), size = 1, prob = utezi_za_knjige))
  names(stranke_prihodi_knjige) <- c("cas_vstopa","st_knjig")
  
  while(tail(stranke_prihodi_knjige[,1],1) < t) {
    cas_novega_skoka <- tail(stranke_prihodi_knjige[,1],1) + rexp(1, parameter_prihodov)
    prinesene_knjige <- sample(c(1:8), size = 1, prob = utezi_za_knjige)
    stranke_prihodi_knjige[nrow(stranke_prihodi_knjige) + 1,] <- c(cas_novega_skoka, prinesene_knjige)
  }
  
  
  # tabela, ki vsebuje case klicov rezervacij in trajanja teh klicov
  tabela_rezervacije_aux <- data.frame(rexp(1, parameter_klicov), povprecno_trajanje_klica_rezervacije * runif(1, min = 0.8, 1.25))
  names(tabela_rezervacije_aux) <- c("cas", "trajanje")
  while(tail(tabela_rezervacije_aux[,1],1) < t){
    cas_klica <- tail(tabela_rezervacije_aux$cas, 1) + rexp(1, parameter_klicov)
    trajanje_klica <- povprecno_trajanje_klica_rezervacije * runif(1, min = 0.8, 1.25))
tabela_rezervacije_aux[,length(tabela_rezervacije_aux[,1]) + 1] <- c(cas_klica, trajanje_klica)
  }
  tabela_rezervacije <- tabela_rezervacije_aux[1:length(tabela_rezervacije_aux[,1])-1,]
  
  
  
  
  # glavna_tabela
  cas_prvega_prihoda <- stranke_prihodi_knjige[1,1]
  st_prvih_knjig <- stranke_prihodi_knjige[1,2]
  
  if(st_prvih_knjig < max_knjig){
    pospravljanje = 0
  }else{
    pospravljanje = cas_odnasanja_k_knjig(st_prvih_knjig)
  }
  
  cas_strezbe <- cas_za_vracanje_ene_knjige * st_prvih_knjig
  
  
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
cas_obratovanja <- 3600
cas_opazovanja <- 2 * cas_obratovanja

tabela <- simulacija_prihodov(cas_obratovanja, intenziteta_prihodov, intenziteta_klicov)
tabela_brez_zadnjega <- tabela[1:(length(tabela[,1]) - 1),]


glavna_tabela <- rbind(c(0,0,0,0,0,0,0,0,0,0), tabela_brez_zadnjega)
glavna_tabela$cas_knjiznicarja[length(glavna_tabela$cas_knjiznicarja)] <- cas_odnasanja_k_knjig(tail(glavna_tabela$skupno_stevilo_knjig, 1))

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
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna[,1],1), cas_opazovanja/20))

# Koliko strank pride v enem dnevu?
stevilo_strank_v_celem_dnevu <- length(glavna_tabela[,1]) - 1


# Koliko časa je knjižničar brez dela?
tabela_brezdelja <- data.frame(glavna_tabela$cas_prihoda, glavna_tabela$cas_odhoda, glavna_tabela$cas_knjiznicarja)
tabela_brezdelja$cas_naslednjega_prihoda <- c(glavna_tabela$cas_prihoda[2:length(glavna_tabela$cas_prihoda)], cas_obratovanja)
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
tabela_brezdelje_graf <- rbind(tabela_brezdelje_graf, c(cas_obratovanja, tabela_brezdelje_graf$stanje[length(tabela_brezdelje_graf)]))

ggplot(data = tabela_brezdelje_graf) + 
  geom_step(mapping = aes(x = cas, y = stanje)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), cas_obratovanja/10))


# graf, koliko časa knjižničar nima dela
tabela_brezdelja2 <- tabela_brezdelja[tabela_brezdelja$cas_brezdelja >= 0,]
tabela_brezdelja2$vsota <- cumsum(tabela_brezdelja2$cas_brezdelja)

tabela_brezdelja2_aux1 <- data.frame(tabela_brezdelja2$cas_odhoda + tabela_brezdelja2$cas_knjiznicarja, tabela_brezdelja2$vsota)
tabela_brezdelja2_aux2 <- data.frame(tabela_brezdelja2$cas_naslednjega_prihoda, tabela_brezdelja2$vsota)
names(tabela_brezdelja2_aux1) <- c("cas_ko_konca", "vsota")
names(tabela_brezdelja2_aux2) <- c("cas_ko_konca", "vsota")
tabela_brezdelja2_aux1$vsota <- c(0,tabela_brezdelja2_aux1$vsota[1:length(tabela_brezdelja2_aux1$vsota)-1])
tabela_brezdelja2_graf <- rbind(tabela_brezdelja2_aux1, tabela_brezdelja2_aux2)
tabela_brezdelja2_graf <- tabela_brezdelja2_graf[order(tabela_brezdelja2_graf$cas_ko_konca),]
tabela_brezdelja2_graf[length(tabela_brezdelja2_graf$cas_ko_konca) + 1,] <- c(cas_obratovanja, tail(tabela_brezdelja2_graf$vsota, 1))

ggplot(data = tabela_brezdelja2_graf) + 
  geom_point(mapping = aes(x = cas_ko_konca, y = vsota)) +
  geom_line(mapping = aes(x = cas_ko_konca, y = vsota)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), cas_obratovanja/10))


# Do kdaj dela, če zaklene vrata ob casu_obratovanja?
odhod_knjiznicarja <- tail(glavna_tabela$cas_odhoda,1) + tail(glavna_tabela$cas_knjiznicarja, 1)

# Koliko knjig vrne v enem dnevu?
st_knjig_v_enem_dnevu <- sum(glavna_tabela$st_prinesenih_knjig)

# Koliko časa ljudje čakajo v vrsti?
tabela_cakanje <- glavna_tabela[,c(2,4)]
tabela_cakanje$cakanje <- tabela_cakanje$cas_zacetka_strezbe - tabela_cakanje$cas_prihoda
skupni_cas_cakanja <- sum(tabela_cakanje$cakanje)

# Koliko časa čaka posamezen človek?
ggplot(data = glavna_tabela) + 
  geom_line(aes(x = vrstni_red, y = cas_zacetka_strezbe - cas_prihoda))



# VPRAŠANJA:
# dodati še izposojo in svetovanje, rezervacije (telefonske / v živo)


# POTREBNO SPREMENITI:
