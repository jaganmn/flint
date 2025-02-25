complex.runif <-
function (length.out = 0L, modulus = c(0, 1), argument = c(0, 2 * pi))
    complex(modulus  = runif(length.out,  modulus[1L],  modulus[2L]),
            argument = runif(length.out, argument[1L], argument[2L]))
