library(dplyr)

cas_za_vracanje_ene_knjige <- 3
cas_za_izposojo_ene_knjige <- 3

prazna_tabela <-  data.frame("vrstni_red" = integer(), 
                             "cas_prihoda" = numeric(), 
                             "st_prinesenih_knjig" = integer(),
                             "st_izposojenih_knjig" = integer(),
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



############################################
# Funkcija, ki naredi tabelo_vseh prihodov
############################################

ustvari_novo_vrsto_prihodov <- function(tabela, parameter_prihodov, utezi_in, utezi_out, max_izposojenih_knjig){
  cas_novega_skoka <- tail(tabela[,1],1) + rexp(1, parameter_prihodov)
  
  opravilo <- as.character(sample(c("IZPOSOJA", "VRACANJE", "VRACANJE IN IZPOSOJA"), 1))
  if(opravilo == "VRACANJE IN IZPOSOJA"){
    prinesene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_in)
    cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
    izposojene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_out)
  }else if(opravilo == "IZPOSOJA"){
    prinesene_knjige <- as.integer(0)
    izposojene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_out)
    cas_vracanja <- as.numeric(0)
  }else if(opravilo == "VRACANJE"){
    prinesene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_in)
    cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
    izposojene_knjige <- as.integer(0)
  }
  
  nova_vrsta <- data.frame(cas_novega_skoka, opravilo, prinesene_knjige, cas_vracanja, izposojene_knjige)
  nova_vrsta 
}  



# funkcija, ki kliče funkcijo ustvari_novo_vrsto_prihodov in s tem ustvari tabelo prihodov
ustvari_tabelo_prihodov <- function(parameter_prihodov, utezi_in, utezi_out, t, max_izposojenih_knjig){
  tabela <- data.frame("cas" = numeric(), 
                       "vrsta_opravila" = factor(levels = c("VRACANJE", "VRACANJE IN IZPOSOJA", "IZPOSOJA")),
                       "st_knjig" = integer(),
                       "cas_za_vracanje" = numeric(),
                       "st_izposojenih_knjig" = integer())
  
  
  # dodamo prvo vrsto
  cas_vstopa <- rexp(1, parameter_prihodov)
  opravilo <- sample(c("VRACANJE", "VRACANJE IN IZPOSOJA", "IZPOSOJA"), 1)
  
  if(opravilo == "VRACANJE IN IZPOSOJA"){
    prinesene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_in)
    cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
    izposojene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_out)
  } else if(opravilo == "IZPOSOJA"){
    prinesene_knjige <- as.integer(0)
    cas_vracanja <- as.numeric(0)
    izposojene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_out)
  }else if(opravilo == "VRACANJE"){
    prinesene_knjige <- sample(c(1:max_izposojenih_knjig), size = 1, prob = utezi_in)
    cas_vracanja <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.8, max = 1.25) + 10
    izposojene_knjige <- as.integer(0)
  }
  
  prva_vrsta <- data.frame(cas_vstopa, opravilo, prinesene_knjige, cas_vracanja, izposojene_knjige)
  names(prva_vrsta) <- names(tabela)
  tabela <- rbind(tabela, prva_vrsta)
  
  # Dodamo še vse preostale vrstice
  while (tail(tabela[,1],1) < t) {
    nova_vrsta <- ustvari_novo_vrsto_prihodov(tabela, parameter_prihodov, utezi_in, utezi_out, max_izposojenih_knjig)
    names(nova_vrsta) <- names(tabela)
    tabela <- rbind(tabela, nova_vrsta)
  }
  tabela[1:length(tabela[,1])-1,]
}


#########################################
# Na tem mestu imamo tabelo vseh prihodov
#########################################










#funkcija, ki doda vse do vključno prvega prihoda
dodaj_prve_klice_in_prvi_prihod <- function(tabela_prihodov, parameter_klicov, max_knjig){
  tabela_v_nastajanju <- prazna_tabela
  prva_stranka <- tabela_prihodov[1,] 
  cas_klica <- rexp(1, rate = parameter_klicov)
  
  # klici, ki se začnejo pred prvim prihodom
  while (cas_klica < prva_stranka$cas){
    zaporedni_vnos <- length(tabela_v_nastajanju[,1]) + 1
    trajanje_pogovora <- runif(1, min = 20, max = 40)
    nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                               as.integer(0), as.integer(0), as.numeric(0), "KLIC")
    names(nova_vrstica) <- names(tabela_v_nastajanju)
    tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nova_vrstica)
    cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
  }
  
  # Začetek strežbe prve stranke
  if (length(tabela_v_nastajanju[1,]) > 0){
    zacetek_strezbe <- max(prva_stranka$cas, tail(tabela_v_nastajanju$cas_odhoda, 1))
  }else{
    zacetek_strezbe <- prva_stranka$cas
  }
  
  # trajanje strežbe, če ne bi bilo motenj
  opravilo <- prva_stranka$vrsta_opravila
  if (opravilo == "VRACANJE"){
    trajanje <- prva_stranka$cas_za_vracanje
  }else if (opravilo == "IZPOSOJA"){
    trajanje <- prva_stranka$st_izposojenih_knjig * cas_za_izposojo_ene_knjige * runif(1,min = 0.9, max = 1.1)
  }else{
    trajanje <- prva_stranka$cas_za_vracanje + prva_stranka$st_izposojenih_knjig * cas_za_izposojo_ene_knjige * runif(1,min = 0.9, max = 1.1)
  }
  
  konec_strezbe <- zacetek_strezbe + trajanje
  
  #klici, ki se začnejo po prvem prihodu in pred prvim odhodom
  while (cas_klica < konec_strezbe){
    zaporedni_vnos <- length(tabela_v_nastajanju[,1]) + 1
    trajanje_pogovora <- runif(1, min = 20, max = 40)
    nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                               as.integer(1), as.integer(0), as.numeric(0), "KLIC")
    names(nova_vrstica) <- names(tabela_v_nastajanju)
    tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nova_vrstica)
    cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov) 
    
    # Ti klici vplivajo tudi na stranko, ki je trenutno v vrsti.
    konec_strezbe <- konec_strezbe + trajanje_pogovora + 5
  }
  
  # Sedaj dodamo še prvo stranko.
  zaporedni_vnos <- length(tabela_v_nastajanju[,1]) + 1
  if (prva_stranka$st_knjig >= max_knjig){
    novo_stanje_knjig <- as.integer(0)
    pospravljanje <- cas_odnasanja_k_knjig(prva_stranka$st_knjig)
  }else{
    novo_stanje_knjig <- prva_stranka$st_knjig
    pospravljanje <- as.numeric(0)
  }
  opravilo <- as.character(prva_stranka$vrsta_opravila)
  
  nova_vrstica <- data.frame(zaporedni_vnos, prva_stranka$cas, prva_stranka$st_knjig, prva_stranka$st_izposojenih_knjig, zacetek_strezbe, konec_strezbe - zacetek_strezbe, konec_strezbe,
                             as.integer(1), novo_stanje_knjig, pospravljanje, opravilo)
  names(nova_vrstica) <- names(tabela_v_nastajanju)
  tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nova_vrstica)
  list(tabela_v_nastajanju, cas_klica)
}


dodaj_samo_prvi_prihod <- function(tabela_prihodov, max_knjig){
  n <- 1
  stranka_n <- tabela_prihodov[1,]
  cas_prihoda <- stranka_n$cas
  
  
  zacetek <- cas_prihoda
  prinesene_knjige <- as.integer(stranka_n$st_knjig)
  izposojene_knjige <- as.integer(stranka_n$st_izposojenih_knjig)
  opravilo <- stranka_n$vrsta_opravila    
  trajanje <- (prinesene_knjige * cas_za_izposojo_ene_knjige + izposojene_knjige * cas_za_izposojo_ene_knjige) * runif(1,min = 0.9, max = 1.1)
  
  if (prinesene_knjige >= max_knjig){
    novo_stanje_knjig <- as.integer(0)
    pospravljanje <- cas_odnasanja_k_knjig(prinesene_knjige)
  }else{
    novo_stanje_knjig <- prinesene_knjige
    pospravljanje <- as.numeric(0)
  }
  
  nov_prihod <- data.frame(n, cas_prihoda, prinesene_knjige, izposojene_knjige, zacetek, trajanje, zacetek + trajanje, as.integer(1),
                           novo_stanje_knjig, pospravljanje, opravilo)
  names(nov_prihod) <- names(tabela_v_nastajanju)
  tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nov_prihod)
  tabela_v_nastajanju
  
}







# funkcija, ki dobi čas n-tega prihoda iz tabele1 in doda v tabelo2 vse klice, ki se začnejo po odhodu n-1 stranke in pred prihodom n-te stranke
dodaj_zacetne_klice <- function(n, cas_klica, tabela1, tabela2, t, parameter_klicov){
  podatki_stranke <- tabela1[n,]
  cas_zadnje_vrnitve_knjiznicarja <- tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1)
  tabela_zacetnih <- prazna_tabela
  while(cas_klica < podatki_stranke$cas){
    trajanje_zvonenja <- runif(1, min = 20, max = 50)
    # ločimo, če je klic prvi od odhoda zadnje stranke ali ne
    if (tail(tabela2$vrsta_opravila, 1) == "KLIC"){
      zaporedni_vnos <- length(tabela2[,1]) + length(prazna_tabela[,1]) + 1
      trajanje_pogovora <- runif(1, min = 20, max = 40)
      st_strank <- length(tabela1[tabela1$cas <= cas_klica, 1]) - length(tabela2[tabela2$vrsta_opravila != "KLIC", 1])
      stanje_knjig <- tail(tabela2$skupno_stevilo_knjig, 1)
      nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
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
        nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), zacetek, trajanje_pogovora, zacetek + trajanje_pogovora,
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
dodaj_vmesne_klice <- function(n, cas_klica, tabela1, tabela2, verjetnost_za_izposojo, t, parameter_klicov, max_izposojenih_knjig){
  tabela_vmesnih <- prazna_tabela
  podatki_stranke <- tabela1[n,]
  # Če v prejšnjem koraku nismo imeli nobenega klica, potem moramo upoštevati čas vračanja knjižničarja
  zacetek_strezbe <- max(podatki_stranke$cas, tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1))
  
  # trajanje strežbe, če ne bi bilo motenj
  opravilo <- as.character(podatki_stranke$vrsta_opravila)
  if (opravilo == "VRACANJE"){
    trajanje <- podatki_stranke$cas_za_vracanje
  }else if (opravilo == "IZPOSOJA"){
    trajanje <- podatki_stranke$st_izposojenih_knjig * cas_za_izposojo_ene_knjige * runif(1,min = 0.9, max = 1.1)
  }else{
    trajanje <- podatki_stranke$cas_za_vracanje + podatki_stranke$st_izposojenih_knjig * cas_za_izposojo_ene_knjige * runif(1,min = 0.9, max = 1.1)
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
      nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), zacetek_pogovora, trajanje_pogovora, zacetek_pogovora + trajanje_pogovora,
                                 st_strank, stanje_knjig, as.numeric(0), "KLIC")
      names(nova_vrstica) <- names(tabela2)
      tabela2 <- rbind(tabela2, nova_vrstica)
      cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
      
      cas_odhoda <- cas_odhoda + trajanje_pogovora + 5 
    }else{
      zadnja_vrnitev_knjiznicarja <- tail(tabela2$cas_odhoda,1) + tail(tabela2$cas_knjiznicarja,1)
      if (cas_klica + trajanje_zvonenja >= zadnja_vrnitev_knjiznicarja){
        zacetek_pogovora <- max(cas_klica, zadnja_vrnitev_knjiznicarja)
        nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), zacetek_pogovora, trajanje_pogovora, zacetek_pogovora + trajanje_pogovora,
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
prihod_n <- function(n, tabela1, tabela2, cas_odhoda, max_knjig){
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
  
  st_izposojenih_knjig <- podatki_stranka$st_izposojenih_knjig
  
  nova_vrstica <- data.frame(zaporedni_vnos, podatki_stranka$cas, podatki_stranka$st_knjig, st_izposojenih_knjig, zacetek_strezbe, cas_odhoda - zacetek_strezbe, cas_odhoda,
                             st_strank, novo_stanje_knjig, pospravljanje, opravilo)
  names(nova_vrstica) <- names(prazna_tabela)
  nova_vrstica
}

dodaj_klice_na_koncu <- function(cas, tabela_v_nastajanju, tabela_prihodov, parameter_klicov, t){
  cas_klica <- cas
  while (cas_klica < t){
    zaporedni_vnos <- length(tabela_v_nastajanju[,1]) + 1
    trajanje_pogovora <- runif(1, min = 20, max = 40)
    st_strank <- length(tabela_prihodov[,1]) - length(tabela_v_nastajanju[as.character(tabela_v_nastajanju$vrsta_opravila) != "KLIC",1])
    
    stanje_knjig <- tail(tabela_v_nastajanju$skupno_stevilo_knjig,1)
    nova_vrstica <- data.frame(zaporedni_vnos, cas_klica, as.integer(0), as.integer(0), cas_klica, trajanje_pogovora, cas_klica + trajanje_pogovora,
                               st_strank, stanje_knjig, as.numeric(0), "KLIC")
    names(nova_vrstica) <- names(tabela_v_nastajanju)
    tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nova_vrstica)
    cas_klica <- cas_klica + trajanje_pogovora + rexp(1, rate = parameter_klicov)
  }
  tabela_v_nastajanju
}


dodaj_nov_kos <- function(n, cas_klica, tabela_prihodov, tabela_v_nastajanju, utezi_knjige_out, t, parameter_klicov, max_knjig){
  stanje_po_novih <- dodaj_zacetne_klice(n, cas_klica, tabela_prihodov, tabela_v_nastajanju, t, parameter_klicov)
  tabela_v_nastajanju <- rbind(tabela_v_nastajanju, stanje_po_novih[[1]])
  cas_klica <- stanje_po_novih[[2]]
  
  tabela_po_vmesnih <- dodaj_vmesne_klice(n, cas_klica, tabela_prihodov, tabela_v_nastajanju, utezi_knjige_out, t, parameter_klicov)
  tabela_v_nastajanju <- rbind(tabela_v_nastajanju, tabela_po_vmesnih[[1]]) 
  cas_klica <- tabela_po_vmesnih[[2]]
  cas_odhoda <- tabela_po_vmesnih[[3]]
  
  nova_stranka <- prihod_n(n, tabela_prihodov, tabela_v_nastajanju, cas_odhoda, max_knjig)
  tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nova_stranka)
  list(tabela_v_nastajanju, cas_klica)
}


dodaj_nov_kos_brez_klicov <- function(tabela_prihodov, tabela_v_nastajanju, max_knjig, n){
  stranka_n <- tabela_prihodov[n,]
  cas_prihoda <- stranka_n$cas
  prinesene_knjige <- stranka_n$st_knjig
  izposojene_knjige <- stranka_n$st_izposojenih_knjig
  opravilo <- stranka_n$vrsta_opravila
  trajanje <- prinesene_knjige * cas_za_vracanje_ene_knjige * runif(1,min = 0.9, max = 1.1) + izposojene_knjige * cas_za_izposojo_ene_knjige * runif(1,min = 0.9, max = 1.1)
  
  st_strank <- length(tabela_v_nastajanju[tabela_v_nastajanju$cas_odhoda >= cas_prihoda,1]) + 1
  zacetek <- max(cas_prihoda, tail(tabela_v_nastajanju$cas_odhoda,1) + tail(tabela_v_nastajanju$cas_knjiznicarja,1))
  st_knjig_pred_urejanjem <- prinesene_knjige + tail(tabela_v_nastajanju$skupno_stevilo_knjig, 1)
  
  if (st_knjig_pred_urejanjem >= max_knjig){
    pospravljanje <- st_knjig_pred_urejanjem
    novo_stevilo_knjig <- as.integer(0)
  }else{
    pospravljanje <- as.numeric(0)
    novo_stevilo_knjig <- st_knjig_pred_urejanjem
  }
  
  nov_prihod <- data.frame(n, cas_prihoda, prinesene_knjige, izposojene_knjige, zacetek, trajanje, zacetek + trajanje, st_strank,
                           novo_stevilo_knjig, pospravljanje, opravilo)
  names(nov_prihod) <- names(tabela_v_nastajanju)
  tabela_v_nastajanju <- rbind(tabela_v_nastajanju, nov_prihod)
  tabela_v_nastajanju
}

###########################
#  KONEC POMOŽNIH FUNCKCIJ
########################### 
















#### Glavna funkcija, s katero naredimo glavno tabelo, v kateri so vsi podatki o klicih, prihodih in odhodih.

ustvari_skupno_tabelo <- function(parameter_prihodov, parameter_klicov, utezi_knjige_in, utezi_knjige_out, t, max_knjig, klici_dovoljenje, max_izposojenih_knjig){
  
  tabela_prihodov <- ustvari_tabelo_prihodov(parameter_prihodov, utezi_knjige_in, utezi_knjige_out, t, max_izposojenih_knjig)
  tabela_v_nastajanju <-  prazna_tabela
  
  
  if (klici_dovoljenje == TRUE){
    # V tabelo dodamo prvo stranko in vse klice pred njo ali med strežbo
    seznam <- dodaj_prve_klice_in_prvi_prihod(tabela_prihodov, parameter_klicov, max_knjig)
    tabela_v_nastajanju <- seznam[[1]]
    cas_klica <- seznam[[2]]
    
    ################### sedaj dodamo še preostale vrstice
    stevilo_prihodov <- length(tabela_prihodov[,1])
    n <- 2
    
    while (n <= stevilo_prihodov){
      seznam <- dodaj_nov_kos(n, cas_klica, tabela_prihodov, tabela_v_nastajanju, utezi_knjige_out, t, parameter_klicov, max_knjig)
      tabela_v_nastajanju <- seznam[[1]]
      cas_klica <- seznam[[2]]
      
      n <- n + 1
    }
    
    ###Sedaj so v tabeli zabeleženi vsi klici in vsi obiski do vključno zadnjega obiska. Dodamo še klice po zadnjem prihodu in pred zaprtjem.
    tabela_v_nastajanju <- dodaj_klice_na_koncu(cas_klica, tabela_v_nastajanju, tabela_prihodov, parameter_klicov, t)
    
    
  }else{ 
    #dodajamo samo osebne prihode, klicov ni
    tabela <- dodaj_samo_prvi_prihod(tabela_prihodov, max_knjig)
    
    n <- 2
    stevilo_prihodov <- length(tabela_prihodov[,1])
    while (n <= stevilo_prihodov){
      tabela_v_nastajanju <- dodaj_nov_kos_brez_klicov(tabela_prihodov, tabela_v_nastajanju, max_knjig, n)
      n <- n + 1
    }
  }
  tabela_v_nastajanju
}








############################################################################################
# Osnovna tabela je narejena. 
############################################################################################


# skupna_tabela <- ustvari_skupno_tabelo(0.04, 0.004,c(0.35, 0.25, 0.15, 0.09, 0.07, 0.04, 0.03, 0.02),
#                                        utezi_knjige_out = c(0.35, 0.25, 0.15, 0.09, 0.07, 0.04, 0.03, 0.02),
#                                        t = 10000,
#                                        max_knjig = 10,
#                                        TRUE,
#                                        max_izposojenih_knjig = 8)



# # POTREBNO SPREMENITI:
# funkcija, ki izračuna novo stanje knjig
# dodaj graf po številu knjig
# dodaj tabelo za vračanje knjig

