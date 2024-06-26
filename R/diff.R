results = function(fname="jam", mask="%s.txt", path="out")
{
  if(!is.null(mask))
    fname = sprintf(mask, fname)
  
  if(!is.null(path))
    fname = file.path(path, fname)
  
  read.table(fname, sep='\t', header=TRUE, row.names=NULL)
}

oldres = function(ccode="jam", path="../wuenic39/out")
  results(ccode, mask="%s.txt", path=path)

newres = function(ccode="jam", path="out")
  results(ccode, mask="%s.txt", path=path)

diffcov = function(ccode="jam")
{
  old = oldres(ccode)
  old$Rule[which(old$Rule == "RMF")] = "RMF:"
  new = newres(ccode)

  # Country names are skipped because of problems with special characters
  # Comments are skipped because they are reordered and slightly reformatted.
  old = old[, !(names(old) %in% c("Country", "Comment", "X"))]
  new = new[, !(names(new) %in% c("Country", "Comment"))]
  if(any(dim(old) != dim(new)))
    return(TRUE)
  
  d1 = any(old != new, na.rm=TRUE)

  # In Irq, ChildrenInTarget exceeds INT_MAX, skip that one  
  old = old[, !(names(old) %in% c("ChildrenInTarget"))]
  new = new[, !(names(new) %in% c("ChildrenInTarget"))]
  d2 = any(is.na(old) != is.na(new))
  
  d1 | d2
}

diffall = function(path="out")
{
  l = list.files(path=path, pattern="^...\\.txt$")
  l = gsub("\\.txt$", "", l) # remove extension
  sapply(l, diffcov)
}

args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0)
{
    ccode = tools::file_path_sans_ext(args[1])
    print(sprintf("%s: Difference %s", ccode, diffcov(ccode)))
}

if(length(args) == 0)
    print(which(diffall()))