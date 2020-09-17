library(rjson)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape)
library(plotly)
library(purrr)
library(viridis)

embs <- fromJSON(file = "embeddings.txt")

data <- rbind(
  embs$Borchev$Borchev_main, 
  embs$`Grant Pepoyan`$`Grant Pepoyan_main`, 
  embs$Galaev$Galaev_main, 
  embs$Berezin$Berezin_main, 
  embs$Voloshin$Voloshin_main, 
  embs$Roznov$Roznov_main, 
  embs$Nikitina$Nikitina_main, 
  embs$andron$andron_main, 
  embs$Nikitina2$Nikitina2_main
)

names_all <- c("Borchev_main", "Pepoyan_main", "Galaev_main", "Berezin_main", "Voloshin_main", "Roznov_main", 'Nikitina_main', 'andron_main', 'Nikitina2_main')



for(i in 1:9){
  
  for(j in 1:length(embs[[i]][[2]])){
    
    data <- rbind(data, embs[[i]][[2]][[j]])
    
    names_all <- c(names_all, names(embs[[i]][[2]])[j])
    
    
  }
  
  
}

data <- cbind(data, names_all)

data <- 
  data %>%
  as.data.frame() %>%
  mutate(names= names_all) %>%
  separate(names_all, into = c("mark", "add_info"))


for(k in 1:512){
  data[,k] = as.numeric(data[,k])
}


a <- melt(data, id.vars = c("mark", "names", "add_info"))

a <- 
  a %>%
  mutate(variable = as.factor(variable)) %>%
  filter(add_info %in% c("main", "rect", "cut")) %>%
  filter(mark %in% c("Borchev"))


#здесь просто можно посмотреть, как выглядят вектора
gg_all <- 
  ggplot() + 
  geom_point(data = a %>% filter(add_info != "main"), aes(x = as.numeric(variable), y = value, col = add_info)) + 
 # geom_line(data = a %>% filter(add_info != "main"), aes(x = as.numeric(variable), y = value, col = add_info)) + 
  geom_point(data = a %>% filter(add_info == "main"), aes(x = as.numeric(variable), y = value)) + 
 # geom_line(data = a %>% filter(add_info == "main"), aes(x = as.numeric(variable), y = value)) +  
  scale_x_continuous(breaks = seq(from = 1, to = 515, by = 20))

a <- melt(data, id.vars = c("mark", "names", "add_info"))

a <- 
  a %>%
  mutate(variable = as.factor(variable)) %>%
  filter(mark %in% c("Borchev")) %>%
  filter(add_info != "blur")

gg_grid <- 
  ggplot() + 
  geom_point(data = a %>% filter(add_info != "main"), aes(x = as.numeric(variable), y = value, col = add_info)) + 
  # geom_line(data = a %>% filter(add_info != "main"), aes(x = as.numeric(variable), y = value, col = add_info)) + 

  # geom_line(data = a %>% filter(add_info == "main"), aes(x = as.numeric(variable), y = value)) +  
  scale_x_continuous(breaks = seq(from = 1, to = 515, by = 20)) +
#  scale_y_continuous(limits = c(0,5)) + 
  facet_wrap(~add_info)  
  #geom_point(data = a %>% filter(add_info == "main"), aes(x = as.numeric(variable), y = value)) 

gg_grid <- gg_grid + geom_point(data = data.frame(x = as.numeric(a$variable), y = a$value, add = a$add_info) %>% filter(add == "main"),
                                aes(x = x, y = y))

gg_all

gg_grid

ggplotly(gg_all)
ggplotly(gg_grid)

#составим для Борчева матрицу расстояний
main <- 
  data %>%
  filter(mark == "Galaev") %>%
  filter(add_info == "main") %>%
  select(-c(add_info, mark, names)) %>%
  unlist() %>%
  unname()

gens <- 
  data %>%
  filter(mark == "Galaev") %>%
  filter(add_info != "main") %>%
  select(-c(add_info, mark, names))

dist <- t(apply(gens, 1, '-', main))

dist <-
  melt(dist) %>%
  mutate(X2 = as.numeric(as.factor(X2))) %>%
  mutate(X1 = as.factor(X1))

ggplot(data = dist) + 
  geom_point(aes(x = X2, y = value, col = X1))

data_to_hist <-
  dist %>%
  filter(X1 == 2) %>%
  select(value) %>%
  unlist() %>%
  unname()

hist(data_to_hist)

qqnorm(unlist(data_to_hist))

st <- shapiro.test(data_to_hist)


class_names <- data %>% select(mark) %>% unique() %>% unlist() %>% unname()

data <- 
  data %>%
  mutate(mark = ifelse(mark == "Pepoyan", "Pepoyan", ifelse(mark == "Grant", "Pepoyan", mark)))

for(name in class_names){
  
  log <- paste("----------------------------", name)
  
  write.table(log, "logs.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
  
  main <- 
    data %>%
    filter(mark == name) %>%
    filter(add_info == "main") %>%
    select(-c(add_info, mark, names)) %>%
    unlist() %>%
    unname()
  
  gens <- 
    data %>%
    filter(mark == name) %>%
    filter(add_info != "main") %>%
    select(-c(add_info, mark, names))
  
  dist <- t(apply(gens, 1, '-', main))
  
  dist <-
    melt(dist) %>%
    mutate(X2 = as.numeric(as.factor(X2))) %>%
    mutate(X1 = as.factor(X1))
  
  faces <- dist$X1 %>% unique() %>% as.numeric()
  
  for(face_id in faces){
  
    data_to_hist <-
      dist %>%
      filter(X1 == face_id) %>%
      select(value) %>%
      unlist() %>%
      unname()
  
    st <- shapiro.test(data_to_hist)
  
    log <- paste(name, face_id, "foto shapiro.test p-value: ", st$p.value)
  
    write.table(log, "logs.txt", append = TRUE)
  
  }
}


ggplot(data = dist, aes(sample = value, col = X1)) + 
  stat_qq() + 
  stat_qq_line()


# Произведем расчет главных компонент

dfpca <- 
  data %>%
  select(-c(mark, add_info, names))

pca <- 
  prcomp(dfpca, rank. = 2)


rot <- pca$rotation

pcaviz <- 
  data %>%
  select(mark) %>%
  cbind(pca$x)

ggplot(data = pcaviz) + geom_point(aes(x = PC1, y = PC2, col = mark)) + 
  scale_color_brewer(palette = "Paired")


pca <- 
  prcomp(dfpca, rank. = 3)

pcaviz <- 
  data %>%
  select(mark) %>%
  cbind(pca$x)

plot_ly(data = pcaviz, x=~PC1, y=~PC2, z=~PC3, type="scatter3d", mode="markers", color=~mark, colors = "Paired")


pca <- 
  dfpca <- 
  data %>%
  group_by(mark) %>%
  select(-c(mark, add_info, names)) %>%
  group_map(~ prcomp(.x, rank. = 2) %>% pluck(2))
