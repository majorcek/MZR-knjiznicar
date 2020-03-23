#source("funkcija.R")


# funkcija, ki naredi vse tabela, katere uporabimo za vizualizacijo



naredi_tabelo_stanja_strank <- function(tabela){
  
  # 1. tabela za graf, ki prikazuje, kako se spreminja število ljudi v knjižnici.
  tabela_samo_strank <- tabela[tabela$vrsta_opravila != "KLIC",]
  
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


naredi_tabelo_brez_dela <- function(tabela){

  # 3. Koliko časa je knjižničar brez dela?
  tabela_brezdelja <- data.frame(tabela$cas_prihoda, tabela$cas_odhoda, tabela$cas_knjiznicarja)
  tabela_brezdelja$cas_naslednjega_prihoda <- c(tabela$cas_prihoda[2:length(tabela$cas_prihoda)], t)
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
  
  tabela_brezdelja2_graf
}



# # 2. Koliko strank pride v enem dnevu?
# stevilo_strank_v_celem_dnevu <- length(tabela[,1])
# 
# # 6. Do kdaj dela, če zaklene vrata ob casu_obratovanja?
# odhod_knjiznicarja <- tail(tabela$cas_odhoda,1) + tail(tabela$cas_knjiznicarja, 1)
# 
# # 7. Koliko knjig vrne v enem dnevu?
# st_knjig_v_enem_dnevu <- sum(tabela$st_prinesenih_knjig)
# 
# # 8. Koliko časa ljudje čakajo v vrsti?
# tabela_cakanje <- tabela[,c(2,4)]
# tabela_cakanje$cakanje <- tabela_cakanje$cas_zacetka_strezbe - tabela_cakanje$cas_prihoda
# skupni_cas_cakanja <- sum(tabela_cakanje$cakanje)
