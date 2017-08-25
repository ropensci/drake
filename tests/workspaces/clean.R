keep = c("clean.R", "README.md")
for(x in list.files(all.files = TRUE)){
  if(!(x %in% keep)){
    unlink(x, recursive = TRUE, force = TRUE)
  }
}
