
BiocStyle::latex()

library(knitr)
opts_chunk$set(tidy=FALSE)

library(DBI)
library(org.Hs.eg.db)
library(AnnotationForge)
available.dbschemas()

options(continue=" ", prompt="R> ", width=72L)

library(hgu95av2.db)

ls("package:hgu95av2.db")

library(hgu95av2.db)
search()
hgu95av2_dbschema()
org.Hs.eg_dbschema()

qcdata = capture.output(hgu95av2())
head(qcdata, 20)

all_probes <- ls(hgu95av2ENTREZID)
length(all_probes)

set.seed(0xa1beef)
probes <- sample(all_probes, 5)
probes

hgu95av2ENTREZID[[probes[1]]]
hgu95av2ENTREZID$"31882_at"

syms <- unlist(mget(probes, hgu95av2SYMBOL))
syms

mget(probes, hgu95av2CHRLOC, ifnotfound=NA)[1:2]

unlist(mget(syms, revmap(hgu95av2SYMBOL)))

as.list(revmap(hgu95av2PATH)["00300"])

x <- hgu95av2PATH

revx <- hgu95av2PATH2PROBE
revx2 <- revmap(x, objName="PATH2PROBE")
revx2
identical(revx, revx2)

as.list(revx["00300"])

Term("GO:0000018")
Definition("GO:0000018")

rs = ls(revmap(org.Hs.egREFSEQ))[4:6]
EGs = mget(rs, revmap(org.Hs.egREFSEQ), ifnotfound=NA)

GOs = mget(unlist(EGs), org.Hs.egGO, ifnotfound=NA)
GOs

GOIDs = as.character(unique(sapply(GOs, names)))

Term(GOIDs)

head(toTable(hgu95av2GO[probes]))

toTable(x)[1:6, ]
toTable(revx)[1:6, ]

length(x)
length(revx)
allProbeSetIds <- keys(x)
allKEGGIds <- keys(revx)

junk <- Lkeys(x)

Llength(x)
junk <- Rkeys(x)

Rlength(x)

x = hgu95av2ENTREZID[1:10]

mappedkeys(x)
count.mappedkeys(x)

mappedLkeys(x)
count.mappedLkeys(x)

y = hgu95av2ENTREZID[isNA(hgu95av2ENTREZID)]
Lkeys(y)[1:4]

count.mappedLkeys(hgu95av2GO)
Llength(hgu95av2GO) - count.mappedLkeys(hgu95av2GO)
mappedLkeys(hgu95av2GO)[1]
toTable(hgu95av2GO["1000_at"])

x <- hgu95av2CHR
Rkeys(x)
chroms <- Rkeys(x)[23:24]
chroms
Rkeys(x) <- chroms
toTable(x)

z <- as.list(revmap(x)[chroms])
names(z)
z[["Y"]]

chrs = c("12","6")
mget(chrs, revmap(hgu95av2CHR[1:30]), ifnotfound=NA)

unlist(mget(chrs, revmap(hgu95av2CHR[1:30]), ifnotfound=NA))

unlist2(mget(chrs, revmap(hgu95av2CHR[1:30]), ifnotfound=NA))

x <- hgu95av2MAP
pbids <- c("38912_at", "41654_at", "907_at", "2053_at", "2054_g_at",
           "40781_at")
x <- subset(x, Lkeys=pbids, Rkeys="18q11.2")
toTable(x)

  pb2cyto <- as.character(x)
  pb2cyto[pbids]

  cyto2pb <- as.character(revmap(x))

  dim(hgu95av2ENTREZID)

  multi <- toggleProbes(hgu95av2ENTREZID, "all")

  dim(multi)

  multiOnly <- toggleProbes(multi, "multiple")

  dim(multiOnly)

  singleOnly <- toggleProbes(multiOnly, "single")

  dim(singleOnly)

  hasMultiProbes(multiOnly)
  hasSingleProbes(multiOnly)

  hasMultiProbes(singleOnly)
  hasSingleProbes(singleOnly)

org.Hs.eg_dbschema()

org.Hs.eg_dbconn()

query <- "SELECT gene_id FROM genes LIMIT 10;"
result = dbGetQuery(org.Hs.eg_dbconn(), query)
result

sql <- "SELECT gene_id, chromosome FROM genes AS g, chromosomes AS c WHERE g._id=c._id;"
dbGetQuery(org.Hs.eg_dbconn(),sql)[1:10,]

toTable(org.Hs.egCHR)[1:10,]

hgu95av2_dbschema()

hgu95av2ORGPKG

org.Hs.eg_dbschema()

orgDBLoc = system.file("extdata", "org.Hs.eg.sqlite", package="org.Hs.eg.db")
attachSQL = paste("ATTACH '", orgDBLoc, "' AS orgDB;", sep = "")
dbGetQuery(hgu95av2_dbconn(), attachSQL)

system.time({
SQL <- "SELECT DISTINCT probe_id,symbol FROM probes, orgDB.gene_info AS gi, orgDB.genes AS g, orgDB.go_bp AS bp WHERE bp._id=g._id AND gi._id=g._id AND probes.gene_id=g.gene_id AND bp.evidence IN ('IPI', 'IDA', 'IMP', 'IGI')"
zz <- dbGetQuery(hgu95av2_dbconn(), SQL)
})

dbGetQuery(hgu95av2_dbconn(), "DETACH orgDB"         )

sql <- "SELECT gene_id, start_location, end_location, cytogenetic_location FROM genes AS g, chromosome_locations AS c, cytogenetic_locations AS cy WHERE g._id=c._id AND g._id=cy._id"
dbGetQuery(org.Hs.eg_dbconn(),sql)[1:10,]

orgDBLoc = system.file("extdata", "org.Hs.eg.sqlite", package="org.Hs.eg.db")
attachSQL = paste("ATTACH '", orgDBLoc, "' AS orgDB;", sep = "")
dbGetQuery(hgu95av2_dbconn(), attachSQL)

goDBLoc = system.file("extdata", "GO.sqlite", package="GO.db")
attachSQL = paste("ATTACH '", goDBLoc, "' AS goDB;", sep = "")
dbGetQuery(hgu95av2_dbconn(), attachSQL)

SQL <- "SELECT DISTINCT p.probe_id, gi.symbol, gt.go_id, gt.definition
    FROM probes 
        AS p, orgDB.gene_info AS gi, orgDB.genes AS g, orgDB.go_bp 
        AS bp, goDB.go_term AS gt  
    WHERE bp._id=g._id AND gi._id=g._id AND p.gene_id=g.gene_id 
        AND bp.evidence IN ('IPI', 'IDA', 'IMP', 'IGI') AND gt.go_id=bp.go_id"
zz <- dbGetQuery(hgu95av2_dbconn(), SQL)

dbGetQuery(hgu95av2_dbconn(), "DETACH orgDB")
dbGetQuery(hgu95av2_dbconn(), "DETACH goDB")

sessionInfo()

