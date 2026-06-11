     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to default the quote expiration date.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 10/08/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)

      /copy sPrDate
      /copy sPrAtrium

      * *ENTRY
     DQuoteExpDate     pr                  ExtPgm('WTPOLQTE10')
     D                              512
     D                            32767
     D                                1
      *
     DQuoteExpDate     pi
     D inData                       512
     D outJSON                    32767
     D rtnCode                        1
      *
     D                 DS
     D  data                   1    512
     D  inCo#                  1      3  0
     D  inFnd                  4      6  0
     D  inDate                 7     20
      *
     d grpYear         s              3  0 inz
     d incepDate       s              8  0 inz
     d expDate         s              8  0 inz
     d dateStr         s             10    inz
     d dateStr2        s             10    inz
     d slash1          s              2  0 inz
     d slash2          s              2  0 inz
     d numberDate      s              8  0 inz
     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    inCo#
     C                   Kfld                    inFnd
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    inCo#
     C                   Kfld                    inFnd

      /Free

       data = inData;

       dateStr = %subst(inDate:1:%scan(' ':inDate:1)-1);

       If %scan('/':dateStr:1) <> 0;
          slash1 = %scan('/':dateStr:1);
          slash2 = %scan('/':dateStr:slash1+1);
          dateStr2 = %subst(dateStr:1:slash1-1) +
           %subst(dateStr:slash1+1:slash2-slash1-1) + %subst(dateStr:slash2+1);
       Else;
          dateStr2 = dateStr;
       Endif;

       numberDate = %int(%trim(dateStr2));

       incepDate = dtCheck(numberDate);

       If incepDate <> 0 and inCo# <> 0 and inFnd <> 0;

          Chain(n) keyWMFNL PFN00;

          If %found;

             Setgt keyWDF1L PF100;
             Readpe keyWDF1L PF100;

             Dow not %eof and F1Stpo > dtCYMD(incepDate);
                Readpe keyWDF1L PF100;
             Enddo;

             If not %eof;

                grpYear = F1Fyr;

                If FNCGFL = 'Y';
                   expDate = DateTo8(CvtToDate(incepDate) + %years(1));
                Else;
                   expDate = dtMDYY(F1Enpo);
                Endif;

             Endif;

          Endif;

       Endif;

       outJSON = ' ';

       BuildJSON(outJSON :'expirationDate' : %editw(expDate:'  /  /    '));
       BuildJSON(outJSON :'groupYear' : %char(grpYear));

       rtnCode = ' ';
       Return;
      /End-Free

