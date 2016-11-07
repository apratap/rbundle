counts_to_tpm <- function(counts, geneLength){
  geneLength_KB <- geneLength / 1e3
  
  apply(counts, 2, function(col){
    #normalize by gene length in KB
    reads_per_kb_gene <- col / geneLength_KB
    #normalizing for sequencing length // dividing by a 1e6 for convenience purpose
    norm_seq_depth <- sum(reads_per_kb_gene ) / 1e6
    # get to TPM
    tpm <- reads_per_kb_gene / norm_seq_depth    
  })
}

