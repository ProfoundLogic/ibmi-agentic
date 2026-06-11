     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve A/R Transaction Summary - TAB - 2.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLARINQ
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmeap     if   e             disk    rename(pea01:pea0p)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwtavl     if   e           k disk    rename(pav01:pav01)
     fwtabl     if   e           k disk    include(pbs01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DPolicyInq04      pr                  ExtPgm('WTPOLARI04')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicyInq04      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d SrAddr          pr                  extpgm('SRADDR')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##odr                          1
     d ##nodr                         1
     d ##aodr                         1
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d numEdtCde       c                   const('N')
     d xxKey           s             10
     d paymentPlan     s             60
     d actstatus       s             60
     d userName        s             60
     d pgmName         s             60
     d polStatus       s             20
     d bilStatus       s             20
     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d x1              s              1  0
     d x2              s              1  0

     d arrMadDs        ds
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d arrMad                              OVERLAY(arrMadds)
     d                                     LIKE(##emn1) DIM(10)

     d gr2Addds        ds
     d gr2Add1                       50
     d gr2Add2                       50
     d gr2Add3                       50
     d gr2Add4                       50
     d gr2Add5                       50
     d gr2Add6                       50
     d gr2Add7                       50
     d gr2Add                              OVERLAY(gr2Addds)
     d                                     Like(gr2Add1) DIM(10)

     d* Load Transaction Detail detail info.
     d  LoadTrDtDtl    pr

     c* Employer Detail File
     c     keyWDELL1     Klist
     c                   Kfld                    EAco#
     c                   Kfld                    EAfnd
     c                   Kfld                    EAfyr
     c                   Kfld                    EAemp#
     c                   Kfld                    EAdiv

     c     keyTable      klist
     c                   kfld                    EAco#
     c                   kfld                    xxKey
      *
      *
      /Free
       Clear arrMad;
       Clear gr2Add;
       Chain(n) inRRN pea0p;
       Chain(n) keyWDELL1 pel01;
       LoadTrDtDtl();

       outJSON = ' ';

       BuildJSON(outJSON :'OutG2PaymentPlan' :%trimR(paymentPlan));
       BuildJSON(outJSON :'OutG2BilStatus' :%trimR(bilStatus));
       BuildJSON(outJSON :'OutG2PolStatus' :%trimR(polStatus));
       BuildJSON(outJSON :'OutG2ActStatus' :%trimR(actstatus));
       BuildJSON(outJSON :'Outgr2Add1' :%trimR(gr2Add1));
       BuildJSON(outJSON :'Outgr2Add2' :%trimR(gr2Add2));
       BuildJSON(outJSON :'Outgr2Add3' :%trimR(gr2Add3));
       BuildJSON(outJSON :'Outgr2Add4' :%trimR(gr2Add4));
       BuildJSON(outJSON :'Outgr2Add5' :%trimR(gr2Add5));
       BuildJSON(outJSON :'Outgr2Add6' :%trimR(gr2Add6));
       BuildJSON(outJSON :'Outgr2Add7' :%trimR(gr2Add7));


       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Load Transaction Detail detail info.
       //===================================================================
     P LoadTrDtDtl     B
     D LoadTrDtDtl     PI
      /free

       Clear xxKey;
       xxKey = %editc(ELbscd : 'X');
       Chain keyTable PBS01;
       paymentPlan = %trimr(xxKey) + ' ' + BSdesc;

       Select;
       When ELFAF = 'A';
         bilStatus  = 'Audited';
       When  ELFAF = 'E';
         bilStatus  = 'Estimated';
       When  ELFAF = 'R';
         bilStatus  = 'Revised';
       Endsl;

       polStatus = GetQuoteStsDesc(EAco#:EAfnd:EAemp#:EAdiv);

       Clear   AVdesc;
       Clear xxKey;
       xxKey = ELascd;
       Chain keyTable PAV01;
       actstatus  = %trimr(xxkey) + ' ' + AVdesc;

       ##co#  = %editc(EAco#:'X');
       ##fnd  = %editc(EAfnd:'X');
       ##emp# = %editc(EAemp#:'X');
       ##div  = %editc(EAdiv:'X');
       ##ODR  = ' ';
       ##NODR = ' ';
       ##AODR = ' ';
       ##emn1 = ' ';
       ##emn2 = ' ';
       ##emn4 = ' ';
       ##emn5 = ' ';
       ##emn6 = ' ';
       ##emn7 = ' ';

       SrAddr(##co#:##fnd:##emp#:##div:##odr:##nodr:##aodr:
        ##emn1:##emn2:##emn3:##emn4:##emn5:##emn6:##emn7);

       //LOAD MAD ARRAY AND FIND THE FIRST BLANK INDEX.
       x1 = 1;
       x2 = 1;
       If x1 > 0;
          Dow x1 < 8;
             If arrMad(x1) <> *blanks;
                gr2Add(x2) = arrMad(x1);
                x1 = x1 + 1;
                x2 = x2 + 1;
             Else;
                x1 = x1 + 1;
             Endif;
          Enddo;
       Endif;

      /end-free
     P LoadTrDtDtl     E





