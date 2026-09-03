if (file.exists(".Renviron")) readRenviron(".Renviron")
bucket <- Sys.getenv("S3_BUCKET")
keys <- c("networks/invoice_moves.rds", "networks/sap_snapshot_AR.rds",
          "networks/sap_snapshot_AP.rds", "networks/proveedores.rds")
b <- tryCatch(aws.s3::get_bucket_df(bucket = bucket, prefix = "networks/"),
              error = function(e) { cat("get_bucket_df failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(b)) {
  sub <- b[b$Key %in% keys, c("Key", "LastModified", "Size")]
  print(sub)
}
