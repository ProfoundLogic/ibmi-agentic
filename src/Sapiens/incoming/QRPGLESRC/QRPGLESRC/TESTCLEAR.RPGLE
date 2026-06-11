     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     h dftactgrp(*no)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7231R - AUDIT REQUESTS
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM IS USED TO GENERATE AUDIT REQUEST FORMS TO BE   *
     H*  SENT OUT TO ONE OR MORE EMPLOYERS ON CANCELLATION AUDITS     *
     H*  OR END OF THE YEAR AUDITS.                                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    ACCOUNTING REPORT MENU                                     *
     H*    UPDATE CANCELLATION PROGRAM (W0164R)                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SRNAME                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:09                                             *
     H*  CREATED:    12/13/93              PROJECT: WC197             *
     H*****************************************************************
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     F*
     FWMFNL     IF   E           K DISK    Rename(pfn01:pfn00) prefix(f2:2)
     F
     d decCo#          s                   Like(F2co#)
     d decFnd          s                   Like(F2fnd)
     D groupRecord   e ds                  extname(wmfnp) prefix(f2:2)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
      /free
         decCo# = %Dec(qqCo#:3:0);
         decFnd = %Dec(qqFnd:3:0);
         Chain (777:777) pfn00;
         If %Found(wmfnl);
            Chain (decCo#:decFnd) pfn00;
            If Not %Found(wmfnl);
               Clear pfn00;
               Clear groupRecord;
            EndIf;
         EndIf;
         *inLR = *On;
      /end-free
