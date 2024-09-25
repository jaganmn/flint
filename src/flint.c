#include "R_flint.h"

unsigned long long int R_flint_get_length(SEXP object)
{
	unsigned long long int value;
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	uconv(&value, (unsigned int *) INTEGER(length), 1);
	return value;
}

void R_flint_set_length(SEXP object, unsigned long long int value)
{
	/* 'object' is the value of R_do_new_object(class), which is the value  */
	/* of Rf_duplicate(class@prototype); class@prototype@length is INTSXP,  */
	/* so object@length **is** newly allocated.                             */
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	uconv(&value, (unsigned int *) INTEGER(length), 0);
	return;
}

void *R_flint_get_x(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_x);
	return R_ExternalPtrAddr(x);
}

void R_flint_set_x(SEXP object, void *p, R_CFinalizer_t f)
{
	/* 'object' is the value of R_do_new_object(class), which is the value  */
	/* of Rf_duplicate(class@prototype); class@prototype@x is EXTPTRSXP,    */
	/* so object@x **is not** newly allocated.                              */
	SEXP length = PROTECT(R_do_slot(object, R_flint_symbol_length)),
		x = PROTECT(R_MakeExternalPtrFn(p, R_NilValue, length));
	R_RegisterCFinalizer(x, f);
	R_do_slot_assign(object, R_flint_symbol_x, x);
	UNPROTECT(2);
	return;
}

const char *R_flint_get_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	return (i < 0) ? "" : R_flint_classes[i];
}

SEXP R_flint_bits(void)
{
	return Rf_ScalarInteger(FLINT_BITS);
}

SEXP R_flint_class(SEXP object)
{
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, 1));
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	SET_STRING_ELT(ans, 0, (i < 0) ? NA_STRING : Rf_mkChar(R_flint_classes[i]));
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_new(SEXP class)
{
	return newObject(CHAR(STRING_ELT(class, 0)));
}

SEXP R_flint_valid(SEXP object)
{
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	if (XLENGTH(length) != 2)
		return Rf_mkString("length of 'length' slot is not 2");
	int length0 = INTEGER(length)[0] == 0 && INTEGER(length)[1] == 0;
	SEXP x = R_do_slot(object, R_flint_symbol_x);
	if ((R_ExternalPtrAddr(x) == 0) != length0)
		return Rf_mkString((length0) ?
		                   "pointer field of 'x' slot is non-zero and length is zero" :
		                   "pointer field of 'x' slot is zero and length is non-zero");
	if (R_ExternalPtrProtected(x) != length)
		return Rf_mkString("protected field of 'x' slot is not 'length' slot");
	return Rf_ScalarLogical(1);
}

SEXP R_flint_length(SEXP object)
{
	SEXP ans;
	unsigned long long int n = R_flint_get_length(object);
	if (n <= INT_MAX) {
		ans = Rf_allocVector(INTSXP, 1);
		INTEGER(ans)[0] = (int) n;
	} else {
		ans = Rf_allocVector(REALSXP, 1);
		REAL(ans)[0] = (double) n;
		unsigned long long int n_ = (unsigned long long int) (double) n;
		if (n_ != n)
			Rf_warning("true length (%llu) is not exactly representable in double precision; returning an implementation-defined rounded length (%llu)",
			           n, n_);
	}
	return ans;
}
