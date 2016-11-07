library("dplyr")
library("org.Hs.eg.db")
library("org.Mm.eg.db")

#gene annotation
cat('Preparing the hg19 annotation df.....')
hg19_keys <- keys(org.Hs.eg.db,keytype="SYMBOL")
hg19_genes_annot <- AnnotationDbi::select(org.Hs.eg.db, keys=hg19_keys, columns=c("GENENAME","ALIAS", "ENSEMBL", "ENTREZID", "UNIPROT"), keytype="SYMBOL")
# hg19_genes_grpd <- hg19_genes_annot %>%
#   group_by(ENSEMBL) %>%
#   summarise(ALIAS = paste(unique(ALIAS),collapse=", "),
#             SYMBOL = paste(unique(SYMBOL),collapse=", "),
#             GENENAME = paste(unique(GENENAME),collapse=", "),
#             ENTREZID = paste(unique(ENTREZID),collapse=", "),
#             UNIPROT = paste(unique(UNIPROT),collapse=", "))
# hg19_genes_grpd <- as.data.frame(hg19_genes_grpd)
cat('Done \n\n')


#gene annotation
cat('Preparing the mm10 annotation df.....')
mm10_keys <- keys(org.Mm.eg.db,keytype="SYMBOL")
mm10_genes_annot <- AnnotationDbi::select(org.Mm.eg.db, keys=mm10_keys, columns=c("GENENAME","ALIAS", "ENSEMBL", "ENTREZID", "UNIPROT"), keytype="SYMBOL")
cat('Done \n\n')

#' @export
geneIdConverter <- function(genes, genome){
  if(genome == 'mm10'){
    res <- mm10_genes_annot %>%
      filter(SYMBOL %in% genes  | ENSEMBL %in% genes | ALIAS %in% genes |
               ENTREZID %in% genes | UNIPROT %in% genes )
  } else if (genome == 'hg19'){
  res <- hg19_genes_annot %>%
    filter(SYMBOL %in% genes  | ENSEMBL %in% genes | ALIAS %in% genes |
            ENTREZID %in% genes | UNIPROT %in% genes )
  } else {
    stop('genome name not recognized')
  }
  return(res)
}
