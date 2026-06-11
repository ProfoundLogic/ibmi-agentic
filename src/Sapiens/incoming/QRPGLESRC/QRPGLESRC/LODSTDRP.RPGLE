     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')

     h/title LODSTDRP - Load driver program call types
     h*  Created on: 03/12/15 by C:279 for Project: DR337
      *---------------------------------------------------------------------
      * Rrvisions:
      *
      *  06/24/22 C:300 Project: WC2395
      *                 Load crteria for new Agency Maintenance Programs
      *
      *  08/06/21 C:300 Project: WC2196
      *                 Load crteria for new Group Configuration Programs
      *
      *
      *
      *---------------------------------------------------------------------
      *  Note: Please add comments to the top of Revisions.
      *---------------------------------------------------------------------

     fstdrl     uf a e           k disk    rename(pdr01:pdr00)

       //***  Begin Chg  ***  08/06/21  ***********************************
     d***** #ofEntry        c                   15
       //***  Begin Chg  ***  06/24/22  ***********************************
     d***** #ofEntry        c                   23
     d #ofEntry        c                   30
       //***  End Chg  ***  06/24/22  ***********************************
       //***  End Chg  ***  08/06/21  ***********************************
     d pgmDesc         s             21    dim(#ofEntry) ctdata perrcd(1)

     dpgmInfo          ds
     d pgm                           10a
     d   blank01                      1a
     d type                          10a

     d intCount        s              3  0

     d psds          esds                  extname(szq1p)

     d/copy sPrDate

      /free

       // main processing
       Exsr Process;

       *inlr = *On;

       //==================================================================
       // Process record in compile time array
       //==================================================================

       Begsr Process;

       // Writing record in file STDRP from Compile time array.
       intCount = 1;

       Dow #ofEntry >= intCount;
          pgmInfo = pgmDesc(intCount);

          clear pdr00;
          Chain pgm pdr00;

          DRtype = type;
          If not %found(STDRL);
             DRkey = pgm;
             DRipgm = q1Pgm;
             DRiusr = q1User;
             DRidt  = DateTo7(SystemDate);
             DRitim = TimeTo6(SystemTime);
             write pdr00;
          Else;
             DRpgm  = q1Pgm;
             DRuser = q1User;
             DRdt   = DateTo7(SystemDate);
             DRtime = TimeTo6(SystemTime);
             update pdr00;
          EndIf;
          Unlock STDRL;

          intCount = intCount + 1;
       EndDo;

       Endsr;

      /end-free

** pgm  **|** type **
WTCLMCMP   Claim
WTCLMCON   Claim
WTCLMDTL   Claim
WTCLMPRV   Claim
WTCLMRSV   Claim
WTCLMSUM   Claim
WTPOLARINQ Policy
WTPOLCONT  Policy
WTPOLCXRN  Policy
WTPOLDTL   Policy
WTPOLLMST  Policy
WTPOLQTE   Policy
WTPOLQTFR  Policy
WTPOLSUM   Policy
WTPOLTRACK Policy
WTGRPCFG   Group                                                                  ** 08/06/21
WTGRPPRI   Group                                                                  ** 08/06/21
WTGRPADMIN Group                                                                  ** 08/06/21
WTGRPPOLCG Group                                                                  ** 08/06/21
WTGRPCLMCG Group                                                                  ** 08/06/21
WTGRPSSI   Group                                                                  ** 08/06/21
WTGRPDATA  Group                                                                  ** 08/06/21
WTGRPSTRAT Group                                                                  ** 08/06/21
WTAGTASN   Agency                                                                 ** 06/24/22
WTAGTCFG   Agency                                                                 ** 06/24/22
WTAGTCON   Agency                                                                 ** 06/24/22
WTAGTPRI   Agency                                                                 ** 06/24/22
WTAGTPROD  Agency                                                                 ** 06/24/22
WTAGTRTFND Agency                                                                 ** 06/24/22
WTAGTSRVRT Agency                                                                 ** 06/24/22
