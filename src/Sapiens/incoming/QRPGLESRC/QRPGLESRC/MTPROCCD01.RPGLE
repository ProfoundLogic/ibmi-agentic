     h**********************************************************************
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
      **********************************************************************
      * Created: 7/9/2019 C:079 Project: MC433
      **********************************************************************
      * Revisions:
      *
      **********************************************************************
     FFMPRP     IF   E             disk
      * Entry
     dGetPrCdDetail    pr                  ExtPgm('MTPROCCD01')
     d                                9  0
     d                            32767
     d                                1
     d                              512
     dGetPrCdDetail    pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1
     d inState                      512

      /copy sPrAtrium

      /free
       outJSON  = ' ';

       // Load detail for IDC Prior Transactional Files
       Exsr ProcessPrCd;

       RtnCode = ' ';
       Return;
       //==================================================================
       Begsr ProcessPrCd;
        Chain(n) inRRN ppr01;
        If %found(fmprp);
           BuildJSON(outJSON:'OutPrCdDtl':PRcpt4);
           BuildJSON(outJSON:'OutDescription1Dtl':PRdsc1);
           BuildJSON(outJSON:'OutDescription2Dtl':PRdsc2);
        Endif;
       Endsr;

       //==================================================================
      /end-free

