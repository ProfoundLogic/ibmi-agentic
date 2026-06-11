     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to Return Select box values for Agency Producers
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTAGTPROD
     h*  Calls to:
     h*
     h*  Created on 02/09/2022 by C:300 for WC2395
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Agent Producers File
     fwmapp     if   e             disk    rename(pap01:pap0p)
     fwmappcrt  if   e           k disk    rename(pap01:pap0pcr)
     f                                     prefix(CR_)
      * Distribution Types File
     fstdsl     if   e           k disk    rename(pds01:pds00)
      * Sub Category File
     fstdel     if   e           k disk    rename(pde01:pde00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutAgtpro04      pr                  ExtPgm('WTAGTPRO04')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutAgtPro04      pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     d psds          esds                  extname(szq1p)

     d intProd         s              5  0
     d strDStype       s             25
     d strSBcat        s              5
     d strDisTyp       s           2000
     d strSubCat       s           2000
     d strOutCont      s           6000

      /Free
       Chain inRRN pap0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       Exsr BuildData;

       outJSON = ' ';
       BuildJSON(outJSON :'Points1' :%trim(strDisTyp));
       BuildJSON(outJSON :'Points2' :%trim(strSubCat));
       BuildJSON(outJSON :'Points3' :%trim(strOutCont));

       RtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        Clear strDisTyp;
        Clear strSubCat;
        Clear strOutCont;
        Setll (APco#:APFein:APMod:APprod:APseq#) pap0pcr;
        Reade(n) (APco#:APFein:APMod:APprod:APseq#) pap0pcr;
        Dow not %eof(wmappcrt);
           Select;
           When CR_APcritID = 'Distribution';

              // Get Distribution Description
              strDStype = %trim(CR_APcritVal);
              Chain (APco#:strDstype) pds00;
              If %found(stdsl);
                 strDisTyp = %trim(strDisTyp) + ',' + %trim(DSdesc);
              Endif;

           When CR_APcritID = 'Sub Category';

              // Get Sub Category Description
              strSBcat = %trim(CR_APcritVal);
              Setll APco# pde00;
              Reade APco# pde00;
              Dow not %eof(stdel);
                 If DEsubc = strSBcat;
                    // Avoid Duplicate data
                    If %scan(%trim(DEdesc):strSubCat:1) = 0;
                       strSubCat = %trim(strSubCat) + ',' + %trim(DEdesc);
                    Endif;
                 Endif;
                 Reade APco# pde00;
              Enddo;

           When CR_APcritID = 'Process Output Control';
              strOutCont = %trim(strOutCont) + ',' + %trim(CR_APcritVal);
           Endsl;

        Reade(n) (APco#:APFein:APMod:APprod:APseq#) pap0pcr;
        Enddo;

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
