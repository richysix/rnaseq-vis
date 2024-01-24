test_that("load gene ids file works", {
  gene_ids_file <- system.file("extdata", "gene-ids.txt", package = "rnaseqVis")
  gene_ids <- readr::read_tsv(gene_ids_file, col_names = FALSE, show_col_types = FALSE)
  expect_equal(load_gene_ids(gene_ids_file), gene_ids[[1]])
  expect_equal(load_gene_ids(NULL), NULL)
})
