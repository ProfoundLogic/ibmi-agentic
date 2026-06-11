     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for A/R Batch Detail Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTARBTCH
     h*  Calls to:
     h*
     h*  Created on 06/07/2022 by C:342 for Project WC2675
     h**********************************************************************
     h* Revisions:
     h*
     h*                                                                *
     h*   11/07/23 C:300 Support: 98110                                *
     h*                  DSPF: Add Invoice # to A/R Batch Detail box   *
     h*                                                                *
     h**********************************************************************
     fWDEAP     if   e             disk    rename(PEA01:PEA0P)
     fWMEML     if   e           k disk
     FWDELL     IF   E           K DISK    RENAME(PEL01:PEL00)
     FWTAVL     IF   E           K DISK
     FWMPFL     IF   E           K DISK
      * employer details - old
     fwzexl     if   e           k disk    rename(pex01:pex01l)
     FWTABL     IF   E           K DISK
     F                                     INCLUDE(PSY01)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dWTARBTCH01       pr                  extpgm('WTARBTCH01')
     d                                9  0
     d                            32767
     d                                1

     dWTARBTCH01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

      // date data area
     D                 DS                  INZ
     D  ECA                    1     35  0
     D                                     DIM(5)                               CX DATES/FILE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS                  INZ
     D  EEF                    1     35  0
     D                                     DIM(5)                               EFF DATES
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
      *
     d CANCDT          s                   like(EMCAD1)
     d XXPFDT          s                   like(EMCAD1)
     d IX              s              1P 0 inz(*zeros)
     d AvKeyf          s             10A   inz(*blanks)
     d xxofnd          s                   like(EXofnd)
     d xxoemp          s                   like(EXoemp)
     d xxodiv          s                   like(EXodiv)
     d xxsytp          s             50    inz(*blanks)
     d xxacst          s             50    inz(*blanks)
     d xxpfin          s             50    inz(*blanks)
     d xxname          s             40    inz(*blanks)
     d xxvip           s              3    inz(*blanks)

     d KKKEY           s             10A   inz(*blanks)
     d KKco            s                   like(SYCO#)

     C     TBKY          KLIST
     C                   KFLD                    KKco
     C                   KFLD                    KKKEY
      /Free

       Chain inRRN PEA0P;
       outJSON = ' ';

       // Get System Type
       xxsytp = *blanks;
       KKco  = EACO#;
       KKKEY = EASYCD;
       Chain(n) TBKY PSY01;
       if %found and SYstat <> 'I';
          xxsytp = %trim(SYkey) + ' - '+ %trim(SYdesc);
       Endif;
       BuildJSON(outJSON :'OutDtlSysTyp':%trim(xxsytp));

       BuildJSON(outJSON :'OutDtlGroup':%trim(%char(EAFND)));
       BuildJSON(outJSON :'OutDtlEmployer':%trim(%char(EAEMP#)));
       BuildJSON(outJSON :'OutDtlDivision':%trim(%char(EADIV)));
       BuildJSON(outJSON :'OutDtlYear':%trim(%char(EAFYR)));

       // Prior Group, Employer No, Division
       xxofnd = 0;
       xxoemp = 0;
       xxodiv = 0;
       chain(n) (EAco#:EAfnd:EAemp#:EAdiv) pex01l;
       if %found(wzexl);
          xxofnd = EXofnd;
          xxoemp = EXoemp;
          xxodiv = EXodiv;
       endif;
       BuildJSON(outJSON :'OutDtlPriGroup':%trim(%char(xxofnd)));
       BuildJSON(outJSON :'OutDtlPriEmployer':%trim(%char(xxoemp)));
       BuildJSON(outJSON :'OutDtlPriDivision':%trim(%char(xxodiv)));

       BuildJSON(outJSON :'OutDtlChkNo':%trim(%char(EACHK#)));
       BuildJSON(outJSON :'OutDtlChkAmt':%editc(EACKAM:'N'));
       BuildJSON(outJSON :'OutDtlPostAmt':%editc(EAAMT:'N'));
       BuildJSON(outJSON :'OutDtlPostDate':
                         alphaUSA(cvtToDate(dtMDYY(EAPODT))));
       if EAarcd <> *blanks;
          BuildJSON(outJSON :'OutDtlARCode':%trim(EAarcd)+' - '+%trim(EAdesc));
       endif;
       if EAarcd = *blanks;
          BuildJSON(outJSON :'OutDtlARCode':' ');
       endif;

       //*** Begin Add ***** 11/07/23 *****************************
       BuildJSON(outJSON :'OutDtlInv#':%trim(%char(EAinv#)));
       //*** End   Add ***** 11/07/23 *****************************
       BuildJSON(outJSON :'OutDtlInvcID':%trim(%char(EAINVTYPID)));
       BuildJSON(outJSON :'OutDtlPayRefn' :%trim(%char(EAref#)));
       BuildJSON(outJSON :'OutDtlCbxLatePayment':%trim(EALATE));

       // Get Employer Name and Cancel Date, VIP status
       xxname = *blanks;
       Cancdt = *zeros;
       xxvip  = *blanks;
       if EAEMP# <> *zeros;
          Chain(n) (EACO#:EAFND:EAEMP#:EADIV) WMEML;
          If %found(WMEML);
             xxname = %trim(EMNAME);

             // Get cancellation date from wmemp
             IX = 5;
             Dow EEF(IX) = *zeros and IX >=1;
                IX -= 1;
             Enddo;
             Cancdt = ECA(IX);

             // Get VIP status from wmemp
             If EMVIP ='Y';
                xxvip = 'VIP';
             Endif;
          Endif;
       Endif;

       BuildJSON(outJSON :'OutDtlEmpName':%trim(xxname));
       BuildJSON(outJSON :'OutDtlCanDate':
                         alphaUSA(cvtToDate(dtMDYY(Cancdt))));
       BuildJSON(outJSON :'OutDtlVIP':xxvip);

       BuildJSON(outJSON :'OutDtlTransactionDate':
                         alphaUSA(cvtToDate(dtMDYY(EAtrdt))));

       xxacst = *Blanks;
       xxpfin = *Blanks;
       Chain(n) (EACO#:EAFND:EAEMP#:EADIV:EAFYR) PEL00;
       If %found(WDELL);
          // Get Activity Status
          If ELascd <> *blanks;
             AvKeyf = ELASCD;
             Chain(n) (ELCO#:AvKeyf) WTAVL;
             If %found(WTAVL);
                xxacst = %trim(AVkey) + ' - ' + %trim(AVdesc);
             Endif;
          Endif;

          // Get Premium Finance Co
          XXPFDT = ELPFDT;
          If ELpfin <> *blanks;
             SetGt (ELCO#:ELFND:ELPFIN:XXPFDT) WMPFL;
             ReadPE (ELCO#:ELFND:ELPFIN) WMPFL;
             If not %eof(WMPFL);
                xxpfin = %trim(ELpfin) + ' - ' + %trim(PFname);
             Endif;
          Endif;
       Endif;
       BuildJSON(outJSON :'OutDtlActSts':%trim(xxacst));
       BuildJSON(outJSON :'OutDtlPremFin':%trim(xxpfin));

       rtnCode = ' ';
       Return;

