# A function to optimise java in order to be able to export big dataset to excel

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}   