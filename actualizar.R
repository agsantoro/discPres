library(dplyr)
library(rvest)
library(stringr)
library(uuid)
library(readr)
library(tidytext)
library(syuzhet)

urlBase = "https://www.casarosada.gob.ar/informacion/discursos"

# Borrar datos
pathDatos <- "data"
archivos <- list.files(pathDatos, full.names = TRUE)
sapply(archivos, unlink)

ultimaPagina = urlBase %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr('href') %>%
  as.data.frame() %>%
  setNames(c("links")) %>%
  dplyr::filter(substring(links,1,28)=="/informacion/discursos?start") %>%
  dplyr::mutate(links = substring(links,30, nchar(links)))

ultimaPagina = max(as.numeric(ultimaPagina$links))

urlBasePaginas = "https://www.casarosada.gob.ar/informacion/discursos?start="

paginas = seq(0,ultimaPagina,40)

linksDiscursos = c()

for (p in paginas) {
  linksDiscursosPagina =  
    read_html(paste0(urlBasePaginas,p)) %>% 
    html_nodes(".panel") %>%
    html_attr("href")
  linksDiscursos = c(linksDiscursos,linksDiscursosPagina)
  
}

linksDiscursos = paste0("https://www.casarosada.gob.ar",linksDiscursos)

dataBase = data.frame(
  fecha = as.character(),
  titulo = as.character(),
  link = as.character(),
  LinkTexto = as.character(),
  nPalabras = as.numeric(),
  linkVideo = as.character()
)

for (l in linksDiscursos) {
  
  pagina = read_html(l)
  
  fecha = pagina %>% html_nodes("time") %>% html_text()
  fecha = str_remove_all(fecha,"\\r\\n")
  fecha = trimws(fecha, c("left"))
  fecha = trimws(fecha, c("right"))
  
  titulo = pagina %>% html_nodes("strong") %>% html_text()
  titulo = titulo[1]
  titulo = str_replace_all(titulo,'\\"','"')
  
  texto = pagina %>% html_nodes("p") %>% html_text()
  texto = trimws(texto, c("left"))
  texto = trimws(texto, c("right"))
  texto = texto[nchar(texto)>2]
  texto = paste(texto, collapse = "")
  texto = str_replace_all(texto,'\\"','"')
  
  id = UUIDgenerate()
  path = paste0("data/",id,".txt")
  write_lines(texto,path)
  
  linkVideo = (pagina %>% html_nodes("iframe") %>% html_attr("src"))[1]
  
  if (is.null(linkVideo)==F) {
    linkVideo = str_replace(linkVideo,"embed/","watch?v=")
  } else {linkVideo = NA}
  
  nPalabras = length(strsplit(texto, "\\s+")[[1]])
  
  append = data.frame(
    fecha = fecha,
    titulo = titulo,
    link = l,
    linkTexto = path,
    nPalabras = nPalabras,
    linkVideo = linkVideo
  )
  
  dataBase = rbind(
    dataBase,
    append
  )
  
  print(paste("Agregado:",titulo))
}



mesesNombre = c(
  "enero",
  "febrero",
  "marzo",
  "abril",
  "mayo",
  "junio",
  "julio",
  "agosto",
  "septiembre",
  "octubre",
  "noviembre",
  "diciembre"
)

mesesNumero = str_pad(1:12,2,"left","0")

names(mesesNombre) = mesesNumero


for (m in 1:12) {
  dataBase$fecha <- gsub(mesesNombre[m], names(mesesNombre)[m], dataBase$fecha)
}

dataBase$fecha <- gsub(" de ", "-", dataBase$fecha)
dataBase$fecha <- sub("^\\D*", "", dataBase$fecha)
dataBase$fecha <- paste0(substring(dataBase$fecha,7,10),
                         "-",
                         substring(dataBase$fecha,4,5),
                         "-",
                         substring(dataBase$fecha,1,2))

dataBase$fecha[dataBase$fecha=="-000-11-30"] = "2016-11-30"


dataBaseSentiment = data.frame()


for (i in 1:nrow(dataBase)) {
  print(paste0("Procesando:", dataBase$titulo[i]))
  texto = read_lines(dataBase$linkTexto[i])
  texto = tolower(texto)
  texto = str_replace_all(texto,"http\\S*", "")
  texto = str_replace_all(texto,"[[:punct:]]", " ")
  texto = str_replace_all(texto,"[[:digit:]]", " ")
  texto = str_replace_all(texto,"[\\s]+", " ")
  texto = str_split(texto, " ")[[1]]
  texto = texto[nchar(texto)>2]
  texto = data.frame(token = texto)
  
  texto <- texto %>% anti_join(read.csv("files/spanishStop.txt") %>% setNames("token"))
  
  discurso = data.frame(
    fecha = dataBase$fecha[i],
    titulo = dataBase$titulo[i],
    texto = texto
  )
  
  discurso = discurso %>%
    group_by(fecha,titulo,token) %>%
    summarise(n=n())
  
  discurso$sentimiento = get_sentiment(
    discurso$token,
    method = "nrc",
    path_to_tagger = NULL,
    cl = NULL,
    language = "spanish",
    lexicon = NULL,
    regex = "[^A-Za-z']+"
  )
  
  discurso= cbind(
    discurso,
    get_nrc_sentiment(
      discurso$token,
      language = "spanish"
    )
    
  )
  
  dataBaseSentiment = rbind(
    dataBaseSentiment,
    discurso
  )
  
  
}




    

