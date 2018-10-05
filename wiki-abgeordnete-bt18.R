## Pakete laden ------------------------------

library(rvest)
library(stringr)
library(igraph)
library(dplyr)
library(htmltab)
library(pageviews)
library(lubridate)

## Funktionen definieren ------------------------------

## Get the labels aligned consistently around the edge of the circle
## for any n of nodes.
## This code borrows bits of ggplot2's polar_coord function
## start = offset from 12 o'clock in radians
## direction = 1 for clockwise; -1 for anti-clockwise.
## from https://stackoverflow.com/questions/23209802/placing-vertex-label-outside-a-circular-layout-in-igraph
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

# degrees to radians
# source: https://stackoverflow.com/questions/21402259/radian-measure-in-sin-in-r
degrees_to_radians<-function(degrees=45,minutes=30)
{
  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal<-minutes/60
  c.num<-degrees+decimal
  radians<-c.num*pi/180
  radians
}

linMap <- function(x, from, to)  (x - min(x)) / max(x - min(x)) * (to - from) + from


## Seite parsen, Links auslesen ---------------------

html <- read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(18._Wahlperiode)")

zellen <- html_nodes(html, css = ".wikitable td:nth-child(1) a")
links <- html_attr(zellen, name = "href")
links <- links[5:length(links)] # Die ersten vier Links sind irrelevant und werden ausgeschlossen


## Seiten herunterladen ---------------------

dir.create("artikel")
baseurl <- "http://de.wikipedia.org"
HTML <- list()
Fname <- paste0("artikel/", str_c(basename(links), ".html"))
URL <- str_c(baseurl, links)
# loop
for ( i in seq_along(links) ){
  url <- URL[i]
  fname <- Fname[i]
  if ( !file.exists(fname) ) download.file(url, fname)
  HTML[[i]] <- read_html(fname)
}


## Verknüpfungsmatrix erstellen ---------------------

connections <- data.frame(from=NULL, to=NULL)
# loop
for (i in seq_along(HTML)) {
  pslinks <- html_attr(
    html_nodes(HTML[[i]], xpath="//div/p/a"),
    "href")
  links_in_pslinks <- seq_along(links)[links %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks!=i]
  connections <- rbind(
    connections,
    data.frame(
      from=rep(i, length(links_in_pslinks)), 
      to=links_in_pslinks
    )
  )
}

names(connections) <- c("from", "to")

connections <- rbind(
  connections,
  data.frame(from=connections$to,
             to=connections$from)
)
connections <- connections[!duplicated(connections),]
connections[nrow(connections+1),] <- c(length(links), length(links)-1) # add artificial edge for last observation to get length of graph right


## Graphen erstellen, PageRank berechnen ---------------------

graph_wiki <- graph_from_edgelist(as.matrix(connections), directed = TRUE)
wiki_names <- basename(links) %>% sapply(URLdecode)
graph_wiki_pagerank <- page.rank(graph_wiki)$vector
wiki_pagerank_df <- data.frame(wiki_names, graph_wiki_pagerank, stringsAsFactors = FALSE) 
wiki_pagerank_df$order_original <- 1:nrow(wiki_pagerank_df)
wiki_pagerank_df <- arrange(wiki_pagerank_df, desc(graph_wiki_pagerank))

# Politiker in 10 batches nach PageRank sortieren
vec_list <- list()
for(i in 1:10) { 
  vec_list[[i]] <- seq(i,nrow(wiki_pagerank_df),10)
}
vec_order <- unlist(vec_list)
wiki_pagerank_df$rank <- rev(order(wiki_pagerank_df$graph_wiki_pagerank))
wiki_pagerank_df$order <- order(vec_order)
wiki_pagerank_df$wiki_names_vip <- wiki_pagerank_df$wiki_names
wiki_pagerank_df$wiki_names_vip[wiki_pagerank_df$rank > 10] <- ""
wiki_pagerank_df <- arrange(wiki_pagerank_df, order)

# Labels für Top 10 Politiker erstellen
starting_letter <- str_extract(wiki_pagerank_df$wiki_names_vip, "[:alpha:]")
wiki_pagerank_df$wiki_names_vip <- str_replace(wiki_pagerank_df$wiki_names_vip, "[:alpha:]+", paste0(starting_letter, ". ")) %>% str_replace_all("_", "")
wiki_pagerank_df$wiki_names_vip <- wiki_pagerank_df$wiki_names_vip %>% str_replace("H. -Peter", "H.-P. ") %>% str_replace("U. vonder", "U. v.d.  ") %>% str_replace("F. -Walter", "F.-W. ")


## Files in neuer Reihenfolge nach Batches sortiert einlesen, Graphen erneut erstellen ---------

filenames_ordered <- paste0("artikel/", sapply(wiki_pagerank_df$wiki_names, URLencode), ".html")
links_ordered <- links[wiki_pagerank_df$order_original]
files_parsed <- lapply(filenames_ordered, read_html)

# Verknüpfungsmatrix aufziehen, Graph erstellen
connections <- data.frame(from=NULL, to=NULL)
# loop
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath="//div/p/a"), 
    "href")
  links_in_pslinks <- seq_along(links_ordered)[links_ordered %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks!=i]
  connections <- rbind(
    connections,
    data.frame(
      from=rep(i, length(links_in_pslinks)), 
      to=links_in_pslinks
    )
  )
}

names(connections) <- c("from", "to")
connections <- rbind(
  connections,
  data.frame(from=connections$to,
             to=connections$from)
)
connections <- connections[!duplicated(connections),]
connections[nrow(connections+1),] <- c(length(links), length(links)-1)
graph_wiki <- graph_from_edgelist(as.matrix(connections), directed = TRUE)


## Plot generieren -----------------------
pdf(file="pageRankBundestagCircle.pdf", height=4, width=5.1, family="URWTimes")
par(oma=c(0,0,0,0) + .1)
par(mar=c(.5, .75,.5,1.2))
set.seed(42)
layout <- layout_in_circle(graph_wiki)
lab.locs <- radian.rescale(x=1:vcount(graph_wiki), direction=-1, start=0)
# plot
plot(graph_wiki, layout = layout, 
     vertex.size = linMap(sqrt(wiki_pagerank_df$graph_wiki_pagerank), 1, 20),
     vertex.label = wiki_pagerank_df$wiki_names_vip, 
     vertex.color = "blue",
     vertex.label.color = "black",
     vertex.label.dist = 4 + 1.3*cos(degrees_to_radians(1:vcount(graph_wiki)*(720/vcount(graph_wiki)))),
     vertex.label.degree = lab.locs,
     edge.arrow.size=.05, edge.color = rgb(.3, .3, .3, .2))
dev.off() 



## Tabelle scrapen -----------------------

html <- "https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(18._Wahlperiode)"
tab <- htmltab(html, which = 2, rm_nodata_cols = F)
tab[1:3, 1:4]

doc <- read_html(html)
nodes <- html_nodes(doc, xpath = "//a")



## Pageviews API abfragen -----------------------

merkel_views <- article_pageviews(project = "de.wikipedia", article = "Angela Merkel", user_type = "user", start = "2016090100", end = "2017070100")
schulz_views <- article_pageviews(project = "de.wikipedia", article = "Martin Schulz", user_type = "user", start = "2016090100", end = "2017070100")

plot(ymd(merkel_views$date), merkel_views$views, col = "black", type = "l")
lines(ymd(schulz_views$date), schulz_views$views, col = "darkgrey")




