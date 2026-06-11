     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to Return Select box values for Agency Contacts
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTAGTCON
     h*  Calls to:
     h*
     h*  Created on 10/19/2021 by C:300 for WC2395
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Agent Contacts File
     fwma5p     if   e             disk    rename(pac01:pac0p)
     fwma5pcrt  if   e           k disk    rename(pac01:pac02)
     f                                     prefix(PR_)
      * Agent Producer File
     fwmapl     if   e           k disk    rename(pap01:pap00)
      * Distribution Types File
     fstdsl     if   e           k disk    rename(pds01:pds00)
      * Sub Category File
     fstdel     if   e           k disk    rename(pde01:pde00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutAgtCon02      pr                  ExtPgm('WTAGTCON02')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutAgtCon02      pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     d psds          esds                  extname(szq1p)

     d intProd         s              5  0
     d strDStype       s             25
     d strSBcat        s              5
     d strProd         s           2000
     d strDisTyp       s           2000
     d strSubCat       s           2000
     d strOutCont      s           6000

      /Free
       Chain inRRN pac0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       Exsr BuildData;

       outJSON = ' ';
       BuildJSON(outJSON :'Points1' :%trim(strProd));
       BuildJSON(outJSON :'Points2' :%trim(strDisTyp));
       BuildJSON(outJSON :'Points3' :%trim(strSubCat));
       BuildJSON(outJSON :'Points4' :%trim(strOutCont));

       RtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        Clear strProd;
        Clear strDisTyp;
        Clear strSubCat;
        Clear strOutCont;
        Setll (A5co#:A5Fein:A5Mod:A5seq#) pac02;
        Reade(n) (A5co#:A5Fein:A5Mod:A5seq#) pac02;
        Dow not %eof(wma5pcrt);
           Select;
           When PR_A5critID = 'Producer';
              strProd = %trim(strProd) + ',' + %trim(PR_A5critVal);

              // Get Producer Name
              intprod = %dec(%trim(PR_A5critVal):5:0);
              Chain (A5co#:A5Fein:A5Mod:intprod) pap00;
              If %found(wmapl);
                 strProd = %trim(strProd) + ' - ' + %trim(APfsn) +
                  ' ' + %trim(APlsn);
              Endif;

           When PR_A5critID = 'Distribution';
            //strDisTyp = %trim(strDisTyp) + ',' + %trim(PR_A5critVal);

              // Get Distribution Description
              strDStype = %trim(PR_A5critVal);
              Chain (A5co#:strDstype) pds00;
              If %found(stdsl);
                 strDisTyp = %trim(strDisTyp) + ',' + %trim(DSdesc);
              Endif;

           When PR_A5critID = 'Sub Category';
            //strSubCat = %trim(strSubCat) + ',' + %trim(PR_A5critVal);

              // Get Sub Category Description
              strSBcat = %trim(PR_A5critVal);
              Setll A5co# pde00;
              Reade A5co# pde00;
              Dow not %eof(stdel);
                 If DEsubc = strSBcat;
                    // Avoid Duplicate data
                    If %scan(%trim(DEdesc):strSubCat:1) = 0;
                       strSubCat = %trim(strSubCat) + ',' + %trim(DEdesc);
                    Endif;
                 Endif;
                 Reade A5co# pde00;
              Enddo;

           When PR_A5critID = 'Process Output Control';
              strOutCont = %trim(strOutCont) + ',' + %trim(PR_A5critVal);
           Endsl;

        Reade(n) (A5co#:A5Fein:A5Mod:A5seq#) pac02;
        Enddo;

        If strProd <> *blank;
           strProd = %subst(strProd:2);
        Endif;
        If strDisTyp <> *blank;
           strDisTyp = %subst(strDisTyp:2);
        Endif;
        If strSubCat <> *blank;
           strSubCat = %subst(strSubCat:2);
        Endif;
        If strOutCont <> *blank;
           strOutCont = %subst(strOutCont:2);
        Endif;

       endsr;

      /End-Free
