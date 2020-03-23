
# VIZUALIZACIJA
#############################################################################################

#source("funkcija.R")
#source("analiza.R")



# Kakšno je število strank ob vsakem prihodu ali klicu?
graf1 <- ggplot(data = skupna_tabela) +
  geom_step(mapping = aes(x = cas_prihoda, y = st_strank_v_knjiznici)) +
  scale_x_continuous(breaks=seq(0, tail(skupna_tabela$cas_odhoda,1), t/10))


# Število strank v knjižnici v vsakem trenutku.
graf_2 <- ggplot(data = tabela_stevila_strank_tocna) +
  geom_step(mapping = aes(x = cas, y = stevilo_strank_v_knjiznici)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna[,1],1), t/10))


# graf, ki ponazarja, kdaj je knjižničar brez dela
graf3 <- ggplot(data = tabela_brezdelje_graf) + 
  geom_step(mapping = aes(x = cas, y = stanje)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), t/10))


# graf, koliko časa knjižničar nima dela 
graf4 <- ggplot(data = tabela_brezdelja2_graf) + 
  geom_point(mapping = aes(x = cas_ko_konca, y = vsota)) +
  geom_line(mapping = aes(x = cas_ko_konca, y = vsota)) +
  scale_x_continuous(breaks=seq(0, tail(tabela_stevila_strank_tocna$cas, 1), t/10))


# Koliko časa čaka posamezen človek?
graf5 <- ggplot(data = tabela_samo_strank) +
  geom_line(aes(x = vrstni_red, y = cas_zacetka_strezbe - cas_prihoda))


# Število klicev in število strank
graf6 <- ggplot(data = skupna_tabela) + 
  geom_bar(aes(vrsta_opravila), stat = "count")