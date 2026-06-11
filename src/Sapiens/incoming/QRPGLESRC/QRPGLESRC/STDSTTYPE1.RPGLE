     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STDSTTYPE
     h*  Calls to:
     h*
     h*  Created: 04/19/21   C:53   Project WC2278
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fstdsl     if   e           k disk
     fstdsp     if   e             disk    rename(pds01:pds0p)

      /copy sPrAtrium

      * *ENTRY
     dstDstType1       pr                  extpgm('STDSTTYPE1')
     d                                9  0
     d                            32767
     d                                1

     dstDstType1       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strTyp          s             25
     d strDesc         s             40
     d strSts          s             10

     d ##key           s             10


      /Free

       Chain inRRN pds0p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlType':%trim(strTyp));
       BuildJSON(outJSON :'OutDtlDesc':%trim(strDesc));
       BuildJSON(outJSON :'OutDtlStatus':%trim(strSts));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Type
       strTyp = DStype;

       // Description
       strDesc = DSdesc;

       If DSstat = 'A';
          strSts = 'Active';
       Else;
          strSts = 'Inactive';
       Endif;

       endsr;
      /End-Free
