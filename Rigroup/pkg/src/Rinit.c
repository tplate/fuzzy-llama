#include "Rigroup.h"

/* Automate using sed or something. */
#if _MSC_VER >= 1000
__declspec(dllexport)
#endif
    
    static const R_ExternalMethodDef R_ExtDef[] = {
	{"igroupFuns", (DL_FUNC)&igroupFuns, 1},
	{NULL, NULL, 0},
    };

void R_init_Rigroup(DllInfo *info)
{
  R_registerRoutines(info,NULL,NULL,NULL,R_ExtDef);
}
