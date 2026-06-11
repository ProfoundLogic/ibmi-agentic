     D*=====================================================================
     D* Menu Processing procedure prototypes
     D*=====================================================================
    C*====================================================================
    C* CrtDtaAraNm - Create the DTAARA name from the prompts
    C*====================================================================
     D CrtDtaAraNm     pr            10
     D  inClt#                        4    const
     D  inApp                         3    const
     D  inEnv                         1    const
     D  inRls                         4    const
    D*
    C*====================================================================
    C* LoadDtaAra - Load the DTAARA info to or from
    C*====================================================================
     D LoadDtaAra      pr                  extPgm('M0021C')
     D  type                          4    const
     D  dtaAra                       10    const
     D  dtaAraInfo                  450
     D  rtnCde                        4
