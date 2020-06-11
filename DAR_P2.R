query_product.dat <- read.csv("query_product.csv",stringsAsFactors=FALSE)
product_descriptions.dat <- read.csv("product_descriptions.csv",stringsAsFactors=FALSE)

#bereken de beschrijvende statistieken
summary(query_product.dat$product_title)
summary(as.factor(query_product.dat$relevance))

unique_products <- query_product.dat[!duplicated(query_product.dat$product_uid), ]
nrow(unique_products)

unique_queries <- query_product.dat[!duplicated(query_product.dat$search_term), ]
nrow(unique_queries)

#laad de library voor het voorbewerken van de tekst
library(tau)

#feature 1: komen alle zoektermen voor in de productnaam?
# functie om te berekenen of alle querywoorden in de productnaam voorkomen
all.queryterms <- function (queries,docs) 
{
  n <- length(queries)
  feature <- vector(length=n)
  for(i in 1:n){
    query <- queries[i]
    document <- docs[i]
    a <- textcnt(query,method="string",n=1L)
    b <- textcnt(document,method="string",n=1L)
    c <- intersect(names(a), names(b))
    feature[i] <- as.numeric(length(a)==length(c))}
  feature
}
# bereken deze feature op de query_product tabel
allterms <- all.queryterms(query_product.dat$search_term,query_product.dat$product_title)
summary(allterms)
#feature 2: hoeveel procent van de zoektermen komen voor in de product beschrijving?
#voeg de productbeschrijving samen met de query_product tabel
description_merged <- merge (product_descriptions.dat, query_product.dat)
#functie om te berekenen hoeveel querywoorden er in de productnaam voorkomen
descriptiontermscount <- function (queries,docs) 
{
  n <- length(queries)
  feature <- vector(length=n)
  for(i in 1:n){
    query <- queries[i]
    document <- docs[i]
    a <- textcnt(query,method="string",n=1L)
    b <- textcnt(document,method="string",n=1L)
    c <- intersect(names(a), names(b))
    feature[i] <- (length(c)/length(a))}
  feature
}
descriptionterms <- descriptiontermscount(description_merged$search_term,description_merged$product_description)
summary(descriptionterms)
