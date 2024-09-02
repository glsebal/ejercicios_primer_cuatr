df <- read_csv("C:/mis_datos/Trabajo_R.csv")
library(dplyr)
library(ggplot2)
library(readr)

ggplot(df, aes(x = `Publication Year`)) +
  geom_bar() +
  labs(title = "Número de Publicaciones por Año",
       x = "Año de Publicación",
       y = "Número de Publicaciones")

ggplot(df, aes(x = `Item Type`)) +
  geom_bar() +
  labs(title = "Distribución de Tipos de Publicaciones",
       x = "Tipo de Publicación",
       y = "Número de Publicaciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df %>% filter(!is.na(Language)), aes(x = Language)) +
  geom_bar() +
  labs(title = "Publicaciones por Idioma",
       x = "Idioma",
       y = "Número de Publicaciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


colnames(df)

year_counts <- df %>%
  group_by(`Publication Year`) %>%
  summarise(count = n())
ggplot(year_counts, aes(x = "", y = count, fill = `Publication Year`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Publication Year", title = "Distribution of Publication Year")


ggplot(year_counts, aes(x = "", y = count, fill = factor(`Publication Year`))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Publication Year", title = "Distribution of Publication Year") +
  scale_fill_manual(values = rainbow(n = nrow(year_counts)))


url_counts <- df %>%
  group_by(`Publication Year`) %>%
  summarise(count = n_distinct(Url))  

ggplot(url_counts, aes(x = reorder(`Publication Year`, -count), y = count, fill = `Publication Year`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Publication Year", y = "Number of Unique URLs", fill = "Publication Year", title = "Number of Unique URLs by Publication Year") +
  scale_fill_manual(values = rainbow(n = nrow(url_counts))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

url_counts <- df %>%
  group_by(`Publication Year`) %>%
  summarise(count = n_distinct(Url)) %>%
  mutate(`Publication Year` = factor(`Publication Year`))  

ggplot(url_counts, aes(x = reorder(`Publication Year`, -count), y = count, fill = `Publication Year`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Publication Year", y = "Number of Unique URLs", fill = "Publication Year", title = "Number of Unique URLs by Publication Year") +
  scale_fill_manual(values = rainbow(n = nlevels(url_counts$`Publication Year`))) +  # Usar nlevels para el número de colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


author_counts <- df %>%
  group_by(Author) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  

ggplot(author_counts, aes(x = reorder(Author, count), y = count, fill = Author)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Author", y = "Number of Publications", fill = "Author", title = "Number of Publications by Author") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


ggplot(author_counts, aes(x = reorder(Author, count), y = count, color = Author)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Author", y = "Number of Publications", color = "Author", title = "Number of Publications by Author") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


ggplot(author_counts, aes(x = reorder(Author, count), y = count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(x = "Author", y = "Number of Publications", title = "Number of Publications by Author") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
