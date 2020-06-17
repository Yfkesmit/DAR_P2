productQuery <- read.csv("query_product.csv",stringsAsFactors=FALSE, fileEncoding="ISO-8859-1")
productDescriptions <- read.csv("product_descriptions.csv",stringsAsFactors=FALSE, encoding = "ASCII")
#voeg de productbeschrijving samen met de query_product tabel
description_merged <- merge (productDescriptions, productQuery)

#bereken de beschrijvende statistieken
summary(productQuery$relevance)
summary(as.factor(productQuery$relevance))

unique_products <- productQuery[!duplicated(productQuery$product_uid), ]
nrow(unique_products)

unique_queries <- productQuery[!duplicated(productQuery$search_term), ]
nrow(unique_queries)


#preprocessing:
# Install these packages if not already installed
install.packages('readr')
install.packages("NLP")
install.packages('tm')
install.packages('SnowballC')

# Load the necessary libraries
library(readr)
library(NLP)
library(tm)
library(SnowballC)


# Create a corpus for data pre-processing
productQueryCorpus <- VCorpus(VectorSource(productQuery))
productDescriptionCorpus <- VCorpus(VectorSource(productDescriptions))
mergedQueryDescriptionCorpus <- VCorpus(VectorSource(description_merged))
# Normalize the text
productQueryCorpus_clean <- tm_map(productQueryCorpus, content_transformer(tolower))
productQueryCorpus_clean <- tm_map(productQueryCorpus_clean, removePunctuation)
productQueryCorpus_clean <- tm_map(productQueryCorpus_clean, content_transformer(removeWords), stopwords('english'))
productQueryCorpus_clean <- tm_map(productQueryCorpus_clean, stripWhitespace)

productDescriptionCorpus_clean <- tm_map(productDescriptionCorpus, content_transformer(tolower))
productDescriptionCorpus_clean <- tm_map(productDescriptionCorpus_clean, removePunctuation)
productDescriptionCorpus_clean <- tm_map(productDescriptionCorpus_clean, content_transformer(removeWords), stopwords('english'))
productDescriptionCorpus_clean <- tm_map(productDescriptionCorpus_clean, stripWhitespace)

mergedQueryDescriptionCorpus_clean <- tm_map(mergedQueryDescriptionCorpus, content_transformer(tolower))
mergedQueryDescriptionCorpus_clean <- tm_map(mergedQueryDescriptionCorpus_clean, removePunctuation)
mergedQueryDescriptionCorpus_clean <- tm_map(mergedQueryDescriptionCorpus_clean, content_transformer(removeWords), stopwords('english'))
mergedQueryDescriptionCorpus_clean <- tm_map(mergedQueryDescriptionCorpus_clean, stripWhitespace)


#features
#load some libraries
install.packages('qualV')
library(tau)
library(qualV)
#feature 1: komen alle zoektermen voor in de productnaam?
# functie om te berekenen of alle querywoorden in de productnaam voorkomen
feature1method <- function (queries,docs) 
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
allterms <- feature1method(productQueryCorpus_clean[[4]]$content,productQueryCorpus_clean[[3]]$content)
summary(allterms)

#feature 2: hoeveel procent van de zoektermen komen voor in de product beschrijving?
#functie om te berekenen hoeveel querywoorden er in de productnaam voorkomen
feature2method <- function (queries,docs) 
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
descriptionterms <- feature2method(mergedQueryDescriptionCorpus_clean[[5]]$content,mergedQueryDescriptionCorpus_clean[[2]]$content)
summary(descriptionterms)

#feature 3: hoe vaak komt de query voor in de data set?//basically Raw Frequency
feature3method <- function(queries)
{
  n <- length(queries)
  a <- as.data.frame(table(queries), stringsAsFactors = FALSE)
  feature <- vector(length = n)
  for (i in 1:n){
    query <- queries[i]
    b <- a[a$queries == query, ]
    feature[i] <- as.numeric(b[[2]])
  }
  feature
}
feature3 <- feature3method(productQueryCorpus_clean[[4]]$content)
summary(feature3)

#feature4: Welk percentage van de termen in de productnaam komen voor in de zoektermen?
feature4method <- function (queries,docs) 
{
  n <- length(queries)
  feature <- vector(length=n)
  for(i in 1:n){
    query <- queries[i]
    document <- docs[i]
    a <- textcnt(query,method="string",n=1L)
    b <- textcnt(document,method="string",n=1L)
    c <- intersect(names(a), names(b))
    feature[i] <- (length(c)/length(b))}
  feature
}
#first parameter: queries, second parameter: product name
feature4 <- feature4method(productQueryCorpus_clean[[4]]$content,productQueryCorpus_clean[[3]]$content)
summary(feature4)

#feature 5: lengte van de langste gemeenschappelijke substring tussen productnaam en query
feature5method <- function (queries, productnames)
{
  n <- length(queries)
  feature <- vector(length = n)
  for (i in 1:n){
    query <- queries[i]
    name <- productnames[i]
    a <- strsplit(query, " ")[[1]]
    b <- strsplit(name, " ")[[1]]
    lcs <- LCS(a, b)
    feature[i] <- (lcs$LLCS)
  }
  feature
}
feature5 <- feature5method(productQueryCorpus_clean[[4]]$content[1:50], productQueryCorpus_clean[[3]]$content[1:50])
summary(feature5)
