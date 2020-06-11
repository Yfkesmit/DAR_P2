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

