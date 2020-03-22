library(ggplot2)
max_knjig <- 10 
parameter_prihodov <- 0.04
parameter_klicov <- 0.004
t <- 10000
utezi_za_knjige <- c(0.35, 0.25, 0.15, 0.09, 0.07, 0.04, 0.03, 0.02)
verjetnost_za_izposojo <- c(0.35, 0.25, 0.15, 0.09, 0.07, 0.04, 0.03, 0.02)
cas_za_vracanje_ene_knjige <- 3
cas_za_izposojo_ene_knjige <- 3

prazna_tabela <-  data.frame("vrstni_red" = integer(), 
                             "cas_prihoda" = numeric(), 
                             "st_prinesenih_knjig" = integer(), 
                             "cas_zacetka_strezbe" = numeric(), 
                             "dolzina_strezbe" = numeric(),
                             "cas_odhoda" = numeric(),
                             "st_strank_v_knjiznici" = integer(), 
                             "skupno_stevilo_knjig" = integer(), 
                             "cas_knjiznicarja" = numeric(),
                             "vrsta_opravila" = factor(levels = c("VRACANJE", "VRACANJE IN IZPOSOJA", "IZPOSOJA", "KLIC")))



cas_odnasanja_k_knjig <- function(k){
  30 + 5*k
}

izracunaj_novo_stanje_knjig <- function(n){
  if (n >= max_knjig){
    a <- list(as.integer(0), cas_odnasanja_k_knjig(n))
  }else{
    a <- list(n, as.numeric(0))
  }
  a 
}

# funkcija, ki dobi čas n-tega prihoda iz tabele1 in doda v tabelo2 vse klice, ki se začnejo po odhodu n-1 stranke in pred prihodom n-te stranke
dodaj_zacetne_klice <- function(n, cas_klica, tabela1, tabela2){
  podatki_stranke <- tabela1[n,]
  cas_zadnje_vrnitve_knjiznicarja <- tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1)
  tabela_zacetnih <- prazna_tabela
  while((cas_klica < podatki_stranke$cas) && (cas_klica < t)){
    trajanje_zvonenja <- runif(1, min = 20, max = 50)
    # ločimo, če je klic prvi od odhoda zadnje stranke ali ne
    if (tail(tabela2$vrsta_opravila, 1) == "KLIC"){
      zaporedni_vnos <- length(tabela2[,1]) + length(prazna_tabela[,1]) + 1
      trajanje_pogovora <- runif(1, min = 20, max = 40)
      st_strank <- length(tabela1[tabela1$cas <= cas_klica, 1]) - length(tabela2[tabela2$vrsta_opravila != "KLIC", 1])
      stanje_knjig <- tail(tabela2$skupno_stevilo_knjig, 1)
      nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                                 st_strank, stanje_knjig, as.numeric(0), "KLIC")
      names(nova_vrstica) <- names(tabela_zacetnih)
      tabela_zacetnih <- rbind(tabela_zacetnih, nova_vrstica)
      cas_klica <- cas_klica  + trajanje_pogovora + rexp(1, rate = parameter_klicov)
    }else{
      # prvi klic za ta n
      if(cas_klica + trajanje_zvonenja >= cas_zadnje_vrnitve_knjiznicarja){
        zaporedni_vnos <- length(tabela2[,1]) + length(prazna_tabela[,1]) + 1
        zacetek <- max(cas_klica, cas_zadnje_vrnitve_knjiznicarja)
        trajanje_pogovora <- runif(1, min = 20, max = 40)
        st_strank <- length(tabela1[tabela1$cas <= cas_klica, 1]) - length(tabela2[as.character(tabela2$vrsta_opravila) != "KLIC", 1])
        stanje_knjig <- tail(tabela2$skupno_stevilo_knjig, 1)
        nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), zacetek, trajanje_pogovora, zacetek + trajanje_pogovora,
                                   st_strank, stanje_knjig, as.numeric(0), "KLIC")
        names(nova_vrstica) <- names(tabela2)
        tabela_zacetnih <- rbind(tabela_zacetnih, nova_vrstica)
        cas_klica <- cas_klica + zacetek - cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
      }else{
        cas_klica <- cas_klica + trajanje_zvonenja + rexp(1, rate = parameter_klicov)
      }
    }
  }
  list(tabela_zacetnih, cas_klica)
}

# funkcija, ki dobi čas n-tega prihoda iz tabele1 in doda v tabelo2 vse klice, ki se začnejo po prihodu n-te stranke in pred odhodom n-te stranke
dodaj_vmesne_klice <- function(n, cas_klica, tabela1, tabela2){
  tabela_vmesnih <- prazna_tabela
  podatki_stranke <- tabela1[n,]
  # Če v prejšnjem koraku nismo imeli nobenega klica, potem moramo upoštevati čas vračanja knjižničarja
  zacetek_strezbe <- max(podatki_stranke$cas, tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1))
  
  # trajanje strežbe, če ne bi bilo motenj
  opravilo <- as.character(podatki_stranke$vrsta_opravila)
  if (opravilo == "VRACANJE"){
    trajanje <- podatki_stranke$cas_za_vracanje
  }else if (opravilo == "IZPOSOJA"){
    trajanje <- sample(c(1:8), 1, prob = verjetnost_za_izposojo) * cas_za_izposojo_ene_knjige
  }else{
    trajanje <- podatki_stranke$cas_za_vracanje + sample(c(1:8), 1, prob = verjetnost_za_izposojo) * cas_za_izposojo_ene_knjige
  }
  
  cas_odhoda <- zacetek_strezbe + trajanje
  
  while ((cas_klica < cas_odhoda) && (cas_klica < t)){
    trajanje_zvonenja <- runif(1,min = 20, max = 40)
    #dodamo klic v skupno tabelo
    zaporedni_vnos <- length(tabela2[,1]) + 1
    trajanje_pogovora <- runif(1, min = 20, max = 40)
    stanje_knjig <- tail(tabela2$skupno_stevilo_knjig,1)
    st_strank <- length(tabela1[tabela1$cas <= cas_klica,1]) - length(tabela2[as.character(tabela2$vrsta_opravila) != "KLIC", 1])
    
    if (tail(tabela2$vrsta_opravila,1) == "KLIC"){
      zacetek_pogovora <- cas_klica
      nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), zacetek_pogovora, trajanje_pogovora, zacetek_pogovora + trajanje_pogovora,
                                 st_strank, stanje_knjig, as.numeric(0), "KLIC")
      names(nova_vrstica) <- names(tabela2)
      tabela2 <- rbind(tabela2, nova_vrstica)
      cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
      
      cas_odhoda <- cas_odhoda + trajanje_pogovora + 5 
    }else{
      zadnja_vrnitev_knjiznicarja <- tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1)
      if (cas_klica + trajanje_zvonenja >= zadnja_vrnitev_knjiznicarja){
        zacetek_pogovora <- max(cas_klica, zadnja_vrnitev_knjiznicarja)
        nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), zacetek_pogovora, trajanje_pogovora, zacetek_pogovora + trajanje_pogovora,
                                   st_strank, stanje_knjig, as.numeric(0), "KLIC")
        names(nova_vrstica) <- names(tabela2)
        tabela_vmesnih <- rbind(tabela_vmesnih, nova_vrstica)
        cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
        
        cas_odhoda <- cas_odhoda + trajanje_pogovora + 5 
      }else{
        cas_klica <- cas_klica + trajanje_zvonenja + rexp(1, rate = parameter_klicov)
      }
    }
  }
  list(tabela_vmesnih, cas_klica, cas_odhoda)
}


# funkcija, ki doda n-ti prihod v skupno tabelo (tabelo2)
prihod_n <- function(n, tabela1, tabela2, cas_odhoda){
  podatki_stranka <- tabela1[n,]
  
  zaporedni_vnos <- length(tabela2[,1]) + 1
  zacetek_strezbe <- max(podatki_stranka$cas, tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1))
  tabela_koncanih_strank <- tabela2[tabela2$cas_odhoda >= podatki_stranka$cas, ]
  st_strank <- length(tabela_koncanih_strank[as.character(tabela_koncanih_strank$vrsta_opravila) != "KLIC", 1]) + 1
  
  stanje_knjig_pred_pospravljanjem <- podatki_stranka$st_knjig + tail(tabela2$skupno_stevilo_knjig,1)
  if (stanje_knjig_pred_pospravljanjem >= max_knjig){
    novo_stanje_knjig <- as.integer(0)
    pospravljanje <- cas_odnasanja_k_knjig(stanje_knjig_pred_pospravljanjem)
  }else{
    novo_stanje_knjig <- stanje_knjig_pred_pospravljanjem
    pospravljanje <- as.numeric(0)
  }
  
  opravilo <- as.character(podatki_stranka$vrsta_opravila)
  
  nova_vrstica <- data.frame(zaporedni_vnos, podatki_stranka$cas, podatki_stranka$st_knjig, zacetek_strezbe, cas_odhoda - zacetek_strezbe, cas_odhoda,
                             st_strank, novo_stanje_knjig, pospravljanje, opravilo)
  names(nova_vrstica) <- names(prazna_tabela)
  nova_vrstica
}


########

ustvari_novo_vrsto_prihodov <- function(tabela){
  cas_novega_skoka <- tail(tabela[,1],1) + rexp(1, parameter_prihodov)
  
  opravilo <- as.character(sample(c("IZPOSOJA", "VRACANJE", "VRACANJE IN IZPOSOJA"), 1))
  if(opravilo == "VRACANJE IN IZPOSOJA"){
    prinesene_knjige <- sample(c(1:8), size = 1, prob = utezi_za_knjige)
    cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
  }else if(opravilo == "IZPOSOJA"){
    prinesene_knjige <- as.integer(0)
    cas_vracanja <- as.numeric(0)
  }else if(opravilo == "VRACANJE"){
    prinesene_knjige <- sample(c(1:8), size = 1, prob = utezi_za_knjige)
    cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
  }
  
  nova_vrsta <- data.frame(cas_novega_skoka, opravilo, prinesene_knjige, cas_vracanja)
  nova_vrsta 
}  






#######################
#  KONEC FUNCKCIJ
####################### 

# tabela, ki vsebuje čase prihodov in število prinešenih knjig
tabela_prihodov <- data.frame("cas" = numeric(), 
                              "vrsta_opravila" = factor(levels = c("VRACANJE", "VRACANJE IN IZPOSOJA", "IZPOSOJA")),
                              "st_knjig" = integer(),
                              "cas_za_vracanje" = numeric())


# dodamo prvo vrsto
cas_vstopa <- rexp(1, parameter_prihodov)
opravilo <- sample(c("VRACANJE", "VRACANJE IN IZPOSOJA", "IZPOSOJA"), 1)

if(opravilo == "VRACANJE IN IZPOSOJA"){
  prinesene_knjige <- sample(c(1:8), size = 1, prob = utezi_za_knjige)
  cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
} else if(opravilo == "IZPOSOJA"){
  prinesene_knjige <- 0
  cas_vracanja <- 0
}else if(opravilo == "VRACANJE"){
  prinesene_knjige <- sample(c(1:8), size = 1, prob = utezi_za_knjige)
  cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
}

prva_vrsta <- data.frame(cas_vstopa, opravilo, prinesene_knjige, cas_vracanja)
names(prva_vrsta) <- names(tabela_prihodov)
tabela_prihodov <- rbind(tabela_prihodov, prva_vrsta)

# Dodamo še vse preostale vrstice
s <- 1
repeat{
  s <- s + 1
  nova_vrsta <- ustvari_novo_vrsto_prihodov(tabela_prihodov)
  names(nova_vrsta) <- names(tabela_prihodov)
  tabela_prihodov <- rbind(tabela_prihodov, nova_vrsta)
  if (tail(tabela_prihodov[,1],1) > t){
    break
  }
}

#zadnjo prihod odstranimo, ker se zgodi, ko je knjižnica že zaprta
tabela_prihodov <- tabela_prihodov[1:length(tabela_prihodov[,1])-1,]


#############################################
# Na tem mestu imamo narejeno tabelo prihodov 
#############################################











skupna_tabela <- prazna_tabela

# V tabelo dodamo prvo stranko in vse klice pred njo ali med strežbo
prva_stranka <- tabela_prihodov[1,] 

cas_klica <- rexp(1, rate = parameter_klicov)
# klici, ki se začnejo pred prvim prihodom
while (cas_klica < prva_stranka$cas){
  zaporedni_vnos <- length(skupna_tabela[,1]) + 1
  trajanje_pogovora <- runif(1, min = 20, max = 40)
  nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                             as.integer(0), as.integer(0), as.numeric(0), "KLIC")
  names(nova_vrstica) <- names(skupna_tabela)
  skupna_tabela <- rbind(skupna_tabela, nova_vrstica)
  cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
}

# Začetek strežbe prve stranke
if (length(skupna_tabela[1,]) > 0){
  zacetek_strezbe <- max(prva_stranka$cas, tail(skupna_tabela$cas_odhoda, 1))
}else{
  zacetek_strezbe <- prva_stranka$cas
}
# trajanje strežbe, če ne bi bilo motenj
opravilo <- prva_stranka$vrsta_opravila
if (opravilo == "VRACANJE"){
  trajanje <- prva_stranka$cas_za_vracanje
}else if (opravilo == "IZPOSOJA"){
  trajanje <- sample(c(1:8), 1, prob = verjetnost_za_izposojo) * cas_za_izposojo_ene_knjige
}else{
  trajanje <- prva_stranka$cas_za_vracanje + sample(c(1:8), 1, prob = verjetnost_za_izposojo) * cas_za_izposojo_ene_knjige
}

konec_strezbe <- zacetek_strezbe + trajanje

#klici, ki se začnejo po prvem prihodu in pred prvim odhodom
while (cas_klica < konec_strezbe){
  zaporedni_vnos <- length(skupna_tabela[,1]) + 1
  trajanje_pogovora <- runif(1, min = 20, max = 40)
  nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                             as.integer(1), as.integer(0), as.numeric(0), "KLIC")
  names(nova_vrstica) <- names(skupna_tabela)
  skupna_tabela <- rbind(skupna_tabela, nova_vrstica)
  cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov) 
  
  # Ti klici vplivajo tudi na stranko, ki je trenutno v vrsti.
  konec_strezbe <- konec_strezbe + trajanje_pogovora + 5
}

# Sedaj dodamo še prvo stranko.
zaporedni_vnos <- length(skupna_tabela[,1]) + 1
if (prva_stranka$st_knjig >= max_knjig){
  novo_stanje_knjig <- as.integer(0)
  pospravljanje <- cas_odnasanja_k_knjig(prva_stranka$st_knjig)
}else{
  novo_stanje_knjig <- prva_stranka$st_knjig
  pospravljanje <- as.numeric(0)
}
opravilo <- as.character(prva_stranka$vrsta_opravila)

nova_vrstica <- data.frame(zaporedni_vnos, prva_stranka$cas, prva_stranka$st_knjig, zacetek_strezbe, konec_strezbe - zacetek_strezbe, konec_strezbe,
                           as.integer(1), novo_stanje_knjig, pospravljanje, opravilo)
names(nova_vrstica) <- names(skupna_tabela)
skupna_tabela <- rbind(skupna_tabela, nova_vrstica)


################### začetek tabele je narejen


################### sedaj dodamo še preostale vrstice


stevilo_prihodov <- length(tabela_prihodov[,1])
n <- 2

while (n <= stevilo_prihodov){
  stanje_po_novih <- dodaj_zacetne_klice(n, cas_klica, tabela_prihodov, skupna_tabela)
  skupna_tabela <- rbind(skupna_tabela, stanje_po_novih[[1]])
  cas_klica <- stanje_po_novih[[2]]
  
  tabela_po_vmesnih <- dodaj_vmesne_klice(n, cas_klica, tabela_prihodov, skupna_tabela)
  skupna_tabela <- rbind(skupna_tabela, tabela_po_vmesnih[[1]]) 
  cas_klica <- tabela_po_vmesnih[[2]]
  cas_odhoda <- tabela_po_vmesnih[[3]]
  
  nova_stranka <- prihod_n(n, tabela_prihodov, skupna_tabela, cas_odhoda)
  skupna_tabela <- rbind(skupna_tabela, nova_stranka)
  
  n <- n + 1
}
############### Sedaj so v tabeli zabeleženi vsi klici in vsi obiski do vključno zadnjega obiska. 
# Sedaj dodamo še klice po zadnjem prihodu in pred zaprtjem.


while (cas_klica < t){
  zaporedni_vnos <- length(skupna_tabela[,1]) + 1
  trajanje_pogovora <- runif(1, min = 20, max = 40)
  st_strank <- length(tabela_prihodov[,1]) - length(skupna_tabela[as.character(skupna_tabela$vrsta_opravila) != "KLIC",1])
  
  stanje_knjig <- tail(skupna_tabela$skupno_stevilo_knjig,1)
  nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                             st_strank, stanje_knjig, as.numeric(0), "KLIC")
  names(nova_vrstica) <- names(skupna_tabela)
  skupna_tabela <- rbind(skupna_tabela, nova_vrstica)
  cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
}

############################################################################################
# Osnovna tabela je narejena. Zdaj naredimo tabele, katere bomo uporabili pri vizualizaciji.
############################################################################################


# 1. tabela za graf, ki prikazuje, kako se spreminja število ljudi v knjižnici.
tabela_samo_strank <- skupna_tabela[skupna_tabela$vrsta_opravila != "KLIC",]

tabela_aux_casi1 <- data.frame(tabela_samo_strank$cas_prihoda[2:length(tabela_samo_strank$cas_prihoda)], 1)
names(tabela_aux_casi1) <- c("cas", "prihod/odhod")
tabela_aux_casi2 <- data.frame(tabela_samo_strank$cas_odhoda[2:length(tabela_samo_strank$cas_odhoda)], -1)
names(tabela_aux_casi2) <- c("cas", "prihod/odhod")
tabela_stevila_strank_tocna <- rbind(c(0,0), tabela_aux_casi1, tabela_aux_casi2)
tabela_stevila_strank_tocna <- tabela_stevila_strank_tocna[order(tabela_stevila_strank_tocna$`cas`),]
tabela_stevila_strank_tocna$stevilo_strank_v_knjiznici <- cumsum(tabela_stevila_strank_tocna$`prihod/odhod`)
tabela_stevila_strank_tocna$razlika <- c(diff(tabela_stevila_strank_tocna$cas),0)

# 2. Koliko strank pride v enem dnevu?
stevilo_strank_v_celem_dnevu <- length(skupna_tabela[,1])

# 3. Koliko časa je knjižničar brez dela?
tabela_brezdelja <- data.frame(skupna_tabela$cas_prihoda, skupna_tabela$cas_odhoda, skupna_tabela$cas_knjiznicarja)
tabela_brezdelja$cas_naslednjega_prihoda <- c(skupna_tabela$cas_prihoda[2:length(skupna_tabela$cas_prihoda)], t)
names(tabela_brezdelja) <- c("cas_prihoda", "cas_odhoda", "cas_knjiznicarja", "cas_naslednjega_prihoda")
tabela_brezdelja$cas_brezdelja <- tabela_brezdelja$cas_naslednjega_prihoda - (tabela_brezdelja$cas_odhoda + tabela_brezdelja$cas_knjiznicarja)

cas_brezdelja <- sum(tabela_brezdelja$cas_brezdelja[tabela_brezdelja$cas_brezdelja >= 0])

# 4. tabela za graf, ki ponazarja, kdaj je knjižničar brez dela
strnjena_tabela_brezdelja <- tabela_brezdelja[tabela_brezdelja$cas_brezdelja >= 0,]
aux1_brezdelje <- data.frame(strnjena_tabela_brezdelja$cas_odhoda + strnjena_tabela_brezdelja$cas_knjiznicarja, 0)
names(aux1_brezdelje) <- c("cas", "stanje")
aux2_brezdelje <- data.frame(strnjena_tabela_brezdelja$cas_naslednjega_prihoda, 1)
names(aux2_brezdelje) <- c("cas", "stanje")

tabela_brezdelje_graf <- rbind(aux1_brezdelje, aux2_brezdelje) 
tabela_brezdelje_graf <- tabela_brezdelje_graf[order(tabela_brezdelje_graf$cas),]
tabela_brezdelje_graf <- rbind(tabela_brezdelje_graf, c(t, tabela_brezdelje_graf$stanje[length(tabela_brezdelje_graf)]))

# 5. tabela za graf, koliko časa knjižničar nima dela
tabela_brezdelja2 <- tabela_brezdelja[tabela_brezdelja$cas_brezdelja >= 0,]
tabela_brezdelja2$vsota <- cumsum(tabela_brezdelja2$cas_brezdelja)

tabela_brezdelja2_aux1 <- data.frame(tabela_brezdelja2$cas_odhoda + tabela_brezdelja2$cas_knjiznicarja, tabela_brezdelja2$vsota)
tabela_brezdelja2_aux2 <- data.frame(tabela_brezdelja2$cas_naslednjega_prihoda, tabela_brezdelja2$vsota)
names(tabela_brezdelja2_aux1) <- c("cas_ko_konca", "vsota")
names(tabela_brezdelja2_aux2) <- c("cas_ko_konca", "vsota")
tabela_brezdelja2_aux1$vsota <- c(0,tabela_brezdelja2_aux1$vsota[1:length(tabela_brezdelja2_aux1$vsota)-1])
tabela_brezdelja2_graf <- rbind(tabela_brezdelja2_aux1, tabela_brezdelja2_aux2)
tabela_brezdelja2_graf <- tabela_brezdelja2_graf[order(tabela_brezdelja2_graf$cas_ko_konca),]
tabela_brezdelja2_graf[length(tabela_brezdelja2_graf$cas_ko_konca) + 1,] <- c(t, tail(tabela_brezdelja2_graf$vsota, 1))


# 6. Do kdaj dela, če zaklene vrata ob casu_obratovanja?
odhod_knjiznicarja <- tail(skupna_tabela$cas_odhoda,1) + tail(skupna_tabela$cas_knjiznicarja, 1)

# 7. Koliko knjig vrne v enem dnevu?
st_knjig_v_enem_dnevu <- sum(skupna_tabela$st_prinesenih_knjig)

# 8. Koliko časa ljudje čakajo v vrsti?
tabela_cakanje <- skupna_tabela[,c(2,4)]
tabela_cakanje$cakanje <- tabela_cakanje$cas_zacetka_strezbe - tabela_cakanje$cas_prihoda
skupni_cas_cakanja <- sum(tabela_cakanje$cakanje)



##############






#############################################################################################
# VIZUALIZACIJA
#############################################################################################

# Kakšno je število strank ob vsakem prihodu ali klicu?
ggplot(data = skupna_tabela) +
  geom_step(mapping = aes(x = vrstni_red, y = st_strank_v_knjiznici)) +
  scale_x_continuous(breaks=seq(0, tail(skupna_tabela$cas_odhoda,1), t/10))


# Število strank v knjižnici v vsakem trenutku.
ggplot(data = tabela_stevila_strank_tocna) +
  geom_step(mapping = aes(x = cas, y = stevilo_strank_v_knjiznici)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna[,1],1), t/10))

# graf, ki ponazarja, kdaj je knjižničar brez dela
ggplot(data = tabela_brezdelje_graf) + 
  geom_step(mapping = aes(x = cas, y = stanje)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), t/10))
 
# graf, koliko časa knjižničar nima dela 
ggplot(data = tabela_brezdelja2_graf) + 
 geom_point(mapping = aes(x = cas_ko_konca, y = vsota)) +
 geom_line(mapping = aes(x = cas_ko_konca, y = vsota)) +
 scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), t/10))
 
# Koliko časa čaka posamezen človek?
ggplot(data = skupna_tabela) +
  geom_line(aes(x = vrstni_red, y = cas_zacetka_strezbe - cas_prihoda))


# # VPRAŠANJA:
# 
# 
# # POTREBNO SPREMENITI:
# dolžina strežbe in cas knjižničarja v random uniform
# odstraniti as.character ali spremeniti 
# funkcija, ki izračuna novo stanje knjig

