#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void run_quantile(double *In, double *Out,
			 const int *nIn, const int *nWin,
			 const double *Prob, const int *nProb,
			 const int *Type);

static const R_CMethodDef CEntries[] = {
    {"run_quantile", (DL_FUNC) &run_quantile, 7},
    {NULL, NULL, 0}
};

void R_init_diveMove(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
