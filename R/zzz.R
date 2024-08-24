.onLoad <-
function (libname, pkgname)
	library.dynam("flint", pkgname, libname)

.onUnload <-
function (libpath)
	library.dynam.unload("flint", libpath)
