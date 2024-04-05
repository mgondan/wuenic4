results = function(fname="afg", mask="%s.txt", path="out")
{
  if(!is.null(mask))
    fname = sprintf(mask, fname)
  
  if(!is.null(path))
    fname = file.path(path, fname)
  
  read.table(fname, sep='\t', header=TRUE, row.names=NULL)
}

oldres = function(ccode="afg", path="../wuenic39/out")
  results(ccode, mask="%s.txt", path=path)

newres = function(ccode="afg", path="out")
  results(ccode, mask="%s.txt", path=path)

diffcov = function(ccode="afg")
{
  old = oldres(ccode)
  new = newres(ccode)
  any(old$WUENIC != new$WUENIC)
}

diffall = function(path="out")
{
  l = list.files(path=path, pattern="^...\\.txt$")
  l = gsub("\\.txt$", "", l) # remove extension
  sapply(l, diffcov)
}

# ccode = "che"
# args = commandArgs(trailingOnly=TRUE)
# if(length(args))
#     ccode = tools::file_path_sans_ext(args[1])
# print(sprintf("%s: Difference %s", ccode, diffcov(ccode)))