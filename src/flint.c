#include "R_flint.h"

void R_flint_abort(void)
{
	Rf_error(_("caught exception in libflint"));
	return;
}

void *R_flint_get_pointer(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata);
	void *p = R_ExternalPtrAddr(x);
	return p;
}

unsigned long long int R_flint_get_length(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
	unsigned long long int n;
	uconv(&n, (unsigned int *) INTEGER(length), 1);
	return n;
}

const char *R_flint_get_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	return (i < 0) ? "" : R_flint_classes[i];
}

void R_flint_set(SEXP object,
                 void *p, unsigned long long int n, R_CFinalizer_t f)
{
	SEXP length = PROTECT(Rf_allocVector(INTSXP, 2));
	uconv(&n, (unsigned int *) INTEGER(length), 0);
	SEXP x = PROTECT(R_MakeExternalPtrFn(p, R_NilValue, length));
	R_RegisterCFinalizer(x, f);
	R_do_slot_assign(object, R_flint_symbol_dot_xdata, x);
	UNPROTECT(2);
	return;
}

SEXP R_flint_bits(void)
{
	return Rf_ScalarInteger(FLINT_BITS);
}

SEXP R_flint_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	SEXP ans = Rf_allocVector(STRSXP, 1);
	SET_STRING_ELT(ans, 0, (i < 0) ? NA_STRING : Rf_mkChar(R_flint_classes[i]));
	return ans;
}

SEXP R_flint_new(SEXP class)
{
	return newObject(CHAR(STRING_ELT(class, 0)));
}

SEXP R_flint_valid(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
#define INVALID(...) Rf_mkString(R_alloc_snprintf(255, __VA_ARGS__))
	if (TYPEOF(length) != INTSXP)
		return INVALID(_("type of protected field is not \"%s\""), "integer");
	if (XLENGTH(length) != 2)
		return INVALID(_("length of protected field is not %d"), 2);
	int length0 = INTEGER(length)[0] == 0 && INTEGER(length)[1] == 0;
	if ((R_ExternalPtrAddr(object) == 0) != length0)
		return INVALID((length0)
		               ? _("length is zero and pointer field is non-zero")
		               : _("length is non-zero and pointer field is zero"));
#undef INVALID
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
			Rf_warning(_("true length (%llu) is not exactly representable in double precision; returning an implementation-defined rounded length (%llu)"),
			           n, n_);
	}
	return ans;
}
