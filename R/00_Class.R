setClass("fmpz",
         slots = c(length = "numeric", x = "externalptr"))

setClass("fmpq",
         slots = c(length = "numeric", x = "externalptr"))

setClass("mag",
         slots = c(length = "numeric", x = "externalptr"))

setClass("arf",
         slots = c(length = "numeric", x = "externalptr"))

setClass("arb",
         slots = c(length = "numeric", x = "externalptr"))

setClass("acb",
         slots = c(length = "numeric", x = "externalptr"))
