     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to Return EDI Release Level          y Producers
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: W0080R1
     h*  Calls to:
     h*
     h*  Created on 02/14/2022 by C:079 for WC2797
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
      * Claims File
     fwmcml1    if   e           k disk
      * EDI Control file
     fwmixl     if   e           k disk

      /copy sPrString
      /copy sPrAtrium

      *entry
     DGetEdiRelease    pr                  ExtPgm('W0080R101')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DGetEdiRelease    pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     d psds          esds                  extname(szq1p)

     d intCo#          s                   Like(CMco#)
     d intFnd          s                   Like(CMfnd)
     d intFyr          s                   Like(CMfYr)
     d intCase         s                   Like(CMcase)
     d strEdiRelease   s              1

       Exsr ParseParams;
       Exsr BuildData;

       outJSON = ' ';
       BuildJSON(outJSON :'EdiRelease' :strEdiRelease);

       RtnCode = ' ';
       Return;

       //===================================================================
       // Build Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        Clear strEdiRelease;
        Chain (intCo#:intFnd:intFyr:intCase) pcm01;
        If %Found(WMCML1);
           Chain (intCo#:CMjrSt) pix01;
           If %Found(WMIXL);
              strEdiRelease = IXrls;
           Endif;
        Endif;

       endsr;

       //===================================================================
       // Parse incoming parameter
       //===================================================================
       Begsr ParseParams;
          Monitor;
          IntCo# =  %Dec(ParseString(inGenParm:',':1):3:0);
          IntFnd =  %Dec(ParseString(inGenParm:',':2):3:0);
          IntFyr =  %Dec(ParseString(inGenParm:',':3):3:0);
          IntCase = %Dec(ParseString(inGenParm:',':4):7:0);
          On-Error;
          Endmon;
       endsr;
