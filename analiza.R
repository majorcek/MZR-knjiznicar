
# funkcije, ki jih uporabimo pri vizualizaciji


naredi_tabelo_stanja_strank <- function(tabela){
  tabela_samo_strank <- tabela[tabela$vrsta_opravila != "KLIC",]    #odstranimo klice
  
  # povzamemo vse dogodke
  tabela_aux_casi1 <- data.frame(tabela_samo_strank$cas_prihoda[2:length(tabela_samo_strank$cas_prihoda)], 1)
  names(tabela_aux_casi1) <- c("cas", "prihod/odhod")
  tabela_aux_casi2 <- data.frame(tabela_samo_strank$cas_odhoda[2:length(tabela_samo_strank$cas_odhoda)], -1)
  names(tabela_aux_casi2) <- c("cas", "prihod/odhod")
  
  tabela_stevila_strank_tocna <- rbind(c(0,0), tabela_aux_casi1, tabela_aux_casi2)
  tabela_stevila_strank_tocna <- tabela_stevila_strank_tocna[order(tabela_stevila_strank_tocna$`cas`),]
  tabela_stevila_strank_tocna$stevilo_strank_v_knjiznici <- cumsum(tabela_stevila_strank_tocna$`prihod/odhod`)
  tabela_stevila_strank_tocna$razlika <- c(diff(tabela_stevila_strank_tocna$cas),0)
  
  tabela_stevila_strank_tocna
}


naredi_tabelo_brez_dela <- function(tabela, cas_obratovanja){

  # 3. Koliko časa je knjižničar brez dela?
  t2 <- tabela[c(1,2,7,10)]
  t2 <- t2[order(t2$cas_prihoda),]
  t2$original_red <- c(1:length(t2[,1]))
  t2$sprejemljiv <- t2$vrstni_red - t2$original_red
  t2 <- t2[t2$sprejemljiv >= 0,]
  t2 <- t2[, c(2,3,4)]
  
  tabela_brezdelja <- t2[order(t2$cas_prihoda),]
  tabela_brezdelja$cas_naslednjega_prihoda <- c(t2$cas_prihoda[2:length(t2$cas_prihoda)], cas_obratovanja)
  tabela_brezdelja$cas_brezdelja <- tabela_brezdelja$cas_naslednjega_prihoda - (tabela_brezdelja$cas_odhoda + tabela_brezdelja$cas_knjiznicarja)
  
  #cas_brezdelja <- sum(tabela_brezdelja$cas_brezdelja[tabela_brezdelja$cas_brezdelja >= 0])
  
  strnjena <- tabela_brezdelja[tabela_brezdelja$cas_brezdelja > 0,]
  strnjena$skupen_cas <- cumsum(strnjena$cas_brezdelja) + strnjena$cas_prihoda[1]
  
  aux1 <- data.frame(c(0,strnjena$cas_prihoda, tail(strnjena$cas_naslednjega_prihoda,1)), c(0,strnjena$cas_prihoda[1],strnjena$skupen_cas))
  names(aux1) <- c("cas", "cas_brez_dela")
  aux2 <- data.frame(c(strnjena$cas_odhoda + strnjena$cas_knjiznicarja, cas_obratovanja), c(strnjena$cas_prihoda[1], strnjena$skupen_cas))
  names(aux2) <- c("cas", "cas_brez_dela")
  knjiznicar <- rbind(aux1, aux2)
  knjiznicar <- knjiznicar[order(knjiznicar$cas),]
  
  knjiznicar
}



# Koliko strank pride v enem dnevu?
pridobi_podatke <- function(tabela){
  stevilo_strank_v_celem_dnevu <- length(tabela[,1])
  odhod_knjiznicarja_domov <- tail(tabela$cas_odhoda,1) + tail(tabela$cas_knjiznicarja, 1)
  skupno_stevilo_prinesenih_knjig <- sum(tabela$st_prinesenih_knjig)
  skupni_cas_cakanja <- sum(tabela$cas_zacetka_strezbe - tabela$cas_prihoda)
  najdaljsi_cas_cakanja <- max(tabela$cas_zacetka_strezbe - tabela$cas_prihoda)
  najdalje_v_knjiznici <- max(tabela$cas_odhoda - tabela$cas_prihoda)

  c(stevilo_strank_v_celem_dnevu, odhod_knjiznicarja_domov, skupno_stevilo_prinesenih_knjig, skupni_cas_cakanja, najdaljsi_cas_cakanja, najdalje_v_knjiznici)
}


#Kakšna je porazdelitev deležev knjig pri izposoji in vračanju
vracanje_knjig <- function(tabela){
  tabela_vrnjenih <- tabela %>% group_by(st_prinesenih_knjig) %>% count()
  
  stevila1 <- data.frame(c(1:8))
  names(stevila1) <- c("st_prinesenih_knjig")
  tabela_vrnjenih <- merge(x = stevila1, y = tabela_vrnjenih, all.x = TRUE)
  
  tabela_vrnjenih[is.na(tabela_vrnjenih)] <- 0
  tabela_vrnjenih$delez <- tabela_vrnjenih$n / sum(tabela_vrnjenih$n)
  tabela_vrnjenih
}

izposoja_knjig <- function(tabela){
  tabela_izposojenih <- tabela %>% group_by(st_izposojenih_knjig) %>% count()
  
  stevila1 <- data.frame(c(1:8))
  names(stevila1) <- c("st_izposojenih_knjig")
  tabela_izposojenih <- merge(x = stevila1, y = tabela_izposojenih, all.x = TRUE)
  
  tabela_izposojenih[is.na(tabela_izposojenih)] <- 0
  tabela_izposojenih$delez <- tabela_izposojenih$n / sum(tabela_izposojenih$n)
  tabela_izposojenih
}
