"print.extmat" <-
function(x, ...) {
  info <- .Call(extmat_info, x)
  msg <- paste("extmat\nname: '",
               .Call(extmat_get_name, x),
               "'\ntype: ", c("signed char", "unsigned char",
                             "short int", "unsigned short int",
                             "int", "unsigned int",
                             "long long int", "unsigned long long int",
                             "float", "double", "user")[info[1]],
               "\nbytes per item: ", info[2],
               "\nbytes allocated: ", info[3],
               "\ndimensions: ", paste(dim(x), collapse=", "),
               "\nnumber of extra (slop) items:", info[4],
               if (as.logical(info[5])) {
                 "\nR internal pointer: "
               } else {
                 "\nForeign pointer: "
               },
               sprintf("0x%x", info[6]),
               "\nUse indexing, e.g. VAR[], to get the values as an R matrix.\n", sep="")
  cat(msg)
}

