
getManyToOneStatus <- function(x){
    cols <- cols(x)
    testManyToOne <- function(c, x){
        k <- keys(x,"ENTREZID")
        res <- select(x, cols=c, keys=k, keytype="ENTREZID")
        if(length(unique(res[["ENTREZID"]])) <
           length(res[["ENTREZID"]])){
            return(TRUE)
        }else{
            return(FALSE)
        }
    }
    res <- unlist(lapply(cols, testManyToOne, x))
    names(res) <- cols
    res
}

require("org.Hs.eg.db")
require("org.Mm.eg.db")
require("org.At.tair.db")
require("org.Bt.eg.db")
require("org.Cf.eg.db")
require("org.Gg.eg.db")
require("org.Dm.eg.db")
require("org.Rn.eg.db")
require("org.Ce.eg.db")
require("org.Xl.eg.db")
require("org.Sc.sgd.db")
require("org.Ss.eg.db")
require("org.Dr.eg.db")
require("org.EcK12.eg.db")
require("org.EcSakai.eg.db")

pkgs <- c(org.Hs.eg.db,
          org.Mm.eg.db,

          org.Bt.eg.db,
          org.Cf.eg.db,
          org.Gg.eg.db,
          org.Dm.eg.db,
          org.Rn.eg.db,
          org.Ce.eg.db,
          org.Xl.eg.db,

          org.Ss.eg.db,
          org.Dr.eg.db,
          org.EcK12.eg.db,
          org.EcSakai.eg.db)

res <- lapply(pkgs, getManyToOneStatus)
many2Ones = res
save(many2Ones, file="many2Ones.Rda")

blackList <- sort(unlist(res), decreasing=TRUE)

blackList <- blackList[unique(names(blackList))]

blackList <- names(blackList[blackList])

save(blackList, file="manyToOneBlackList.Rda")

