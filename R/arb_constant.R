arb_const_pi <-
function (prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_const_pi, res, slong(prec))
    res
}

arb_const_log2 <-
function (prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_const_log2, res, slong(prec))
    res
}

arb_const_log10 <-
function (prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_const_log10, res, slong(prec))
    res
}

arb_const_e <-
function (prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_const_e, res, slong(prec))
    res
}
