
BiocStyle::latex()

library(knitr)
opts_chunk$set(tidy=FALSE)

suppressPackageStartupMessages({
    library(hgu95av2.db)
})

ls("package:hgu95av2.db")

hgu95av2.db

columns(hgu95av2.db)

keytypes(hgu95av2.db)

head(keys(hgu95av2.db, keytype="SYMBOL"))

k <- head(keys(hgu95av2.db,keytype="PROBEID"))

select(hgu95av2.db, keys=k, columns=c("SYMBOL","GENENAME"), keytype="PROBEID")

k <- head(keys(hgu95av2.db,keytype="PROBEID"))

mapIds(hgu95av2.db, keys=k, column=c("GENENAME"), keytype="PROBEID")

library(org.Hs.eg.db)
columns(org.Hs.eg.db)

keytypes(org.Hs.eg.db)
uniKeys <- head(keys(org.Hs.eg.db, keytype="UNIPROT"))
cols <- c("SYMBOL", "PATH")
select(org.Hs.eg.db, keys=uniKeys, columns=cols, keytype="UNIPROT")

load(system.file("extdata", "resultTable.Rda", package="AnnotationDbi"))
head(resultTable)

annots <- select(org.Hs.eg.db, keys=rownames(resultTable),
                 columns=c("SYMBOL","GENENAME"), keytype="ENTREZID")
resultTable <- merge(resultTable, annots, by.x=0, by.y="ENTREZID")
head(resultTable)

library(GO.db)
GOIDs <- c("GO:0042254","GO:0044183")
select(GO.db, keys=GOIDs, columns="DEFINITION", keytype="GOID")

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
txdb
columns(txdb)
keytypes(txdb)
keys <- head(keys(txdb, keytype="GENEID"))
cols <- c("TXID", "TXSTART")
select(txdb, keys=keys, columns=cols, keytype="GENEID")

library(EnsDb.Hsapiens.v75)
edb <- EnsDb.Hsapiens.v75
edb

columns(edb)

keytypes(edb)

keys <- head(keys(edb, keytype="GENEID"))

select(edb, keys=keys, columns=c("TXID", "TXSEQSTART", "TXBIOTYPE"), 
       keytype="GENEID")

linkY <- keys(edb,
              filter=list(GeneBiotypeFilter("lincRNA"), SeqNameFilter("Y")))
length(linkY)

txs <- select(edb, keys=linkY, columns=c("TXID", "TXSEQSTART", "TXBIOTYPE"),
              keytype="GENEID")
nrow(txs)

txs <- select(edb, keys=list(GeneBiotypeFilter("lincRNA"), SeqNameFilter("Y")),
              columns=c("TXID", "TXSEQSTART", "TXBIOTYPE"))
nrow(txs)

sessionInfo()

