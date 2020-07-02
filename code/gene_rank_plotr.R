gene_rank_plotr <- function(score_tab = NULL, gene_col = "gene", pval_col = "log_p_val", score_comp_col = "logFDR") {
  
  score_tab_named <- score_tab %>% 
    rename(gene = {{ gene_col }}, log_pval = {{ pval_col }}, comp_col = {{ score_comp_col}}) %>% 
    mutate(across(gene, as.character)) %>% 
    rowwise() %>% 
    mutate(comb_score = sum(log_pval > .$log_pval & comp_col > .$comp_col)) %>%
    ungroup() %>%
    mutate(comb_score = comb_score/n()) 
  
  x <- list(
    title = "-log10 GWAS P-Value"
  )
  
  y <- list(
    title = "-log10 SIGNET FDR"
  )
  
  plot <- plot_ly(score_tab_named,
                  x = ~log_pval,
                  y = ~comp_col,
                  text = ~gene,
                  color = ~comb_score,
                  hoverinfo = 'text',
                  type = "scatter") %>% 
    layout(xaxis = x, 
           yaxis = y
           )
}