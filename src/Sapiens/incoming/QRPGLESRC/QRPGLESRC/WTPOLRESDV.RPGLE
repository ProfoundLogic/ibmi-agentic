     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/TITLE WTPOLRESDV -  Rescind of Division - Mid term Endorsement
      *===============================================================*
      *  Created   : 05/23/12                                         *
      *  Programmer: C:191                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 10/15/24 C:320 Project: WC2984                                *
      *                Restore the original premium from WDEHP        *
      *                                                               *
      * 06/13/24 C:062 Support: 115758(133)                           *
      *                Resend invoices to IVC when invoice            *
      *                reinstated.                                    *
     H*                                                               *
     H* 06/12/24 C:062 Project: 146WKQ                                *
     H*                Don't create assessments billing if not S/R or *
     H*                marked as SR assessments.                      *
      *                                                               *
      * 02/26/24 C:340 Support: WC2956                                *
      *                Reset Flag - Cancellation/Reinstatement Process*
      *                             Control                           *
      *                                                               *
      * 09/15/22 C:344 Support: WC2880                                *                            *
      *                Add processing to handle situations where a    *                            *
      *                division is being reactivated that is on a     *                            *
      *                policy that is already cancelled/non-renewed.  *                            *
      *                The state reporting division will be checked   *
      *                to determine if the policy is currently under  *
      *                a cancellation (non-renew or otherwise).       *
      *                If the policy is cancelled then after          *
      *                rescinding the deletion of division cancel the *
      *                division with the state reporting division's   *
      *                information.
      *                                                               *
      * 06/29/22 C:340 Support: WC2854                                *                            *
      *                Invoice Cloud:  Retain Payment Ref#            *                            *
      *                                                               *                            *
      *  03/29/22 C:33  Support: 101661                               *
      *                 Load xxEmp#/xxDiv from WDELP                  *
      *
      *  08/07/19 C:136 Support: 80573                                *
      *                 Ensure QTKY and WA002R get a co# so that      *
      *                 Quote Tracking record is written              *
      *                                                               *
      * 07/28/14 C:302 SUPPORT: 85709                                 *
      *                XXCO# and XXFND not passed.  Get them from     *
      *                WMELP file.                                    *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     F*  Physical file definition (for maintaining record)
     FWMEMP     UF A E             DISK    rename(pem01:pem0p)
     FWDELP     IF   E             DISK    rename(pel01:pel0p)
     FWTMYP     IF   E             DISK    rename(pmy01:pmy0p)
     FWDELL     UF   E           K DISK    rename(pel01:pel00)
     F*** Begin Add *** 06/12/24 ***********************************   *****
     FWDF1L     IF   E           K DISK    rename(pf101:pf100)
     F*** End   Add *** 06/12/24 ***********************************   *****
     FWDF2L     IF   E           K DISK    rename(pf201:pf200)
     FWMENL3    IF A E           K DISK    rename(pen01:pen03)
     FWMFNL     IF   E           K DISK    rename(pfn01:pfn00)
     FWMDXL     IF   E           K DISK    rename(pdx01:pdx00)
     F** Cancellation Detail File
      *** Begin Chg *** 09/15/22 WC2880 *******************************
     F***WDCXL     UF   E           K DISK    rename(pcx01:pcx00)
     FWDCXL     UF A E           K DISK    rename(pcx01:pcx00)
      *** End   Chg *** 09/15/22 WC2880 *******************************
     F** Cancellation History File
     FWDXHL     IF A E           K DISK    rename(pxh01:pxh00)
     FWAQTL7    IF A E           K DISK    rename(pqt01:pqt07)
     F*  Employer Accounts Receivable
     FWMEAL14   UF A E           K DISK    rename(pea01:pea14)
     FWMA#L1    UF   E           K DISK
     F                                     RENAME(pea01:Pa#01)
     F*
     F*  Transaction Type Code Table  (WTTTP)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
     F*
     F* Audit Tracking Control File
     FWMXAL     UF   E           K DISK    rename(pxa01:pxa00)
     F*
     F* ADMIN/DARTS Control File
     FWMEXL     IF   E           K DISK    rename(pex01:pex00)
     F*
      *** Begin Add *** 09/15/22 WC2880 *******************************
     FWDCXL3    IF   E           K DISK    rename(pcx01:pcx03) prefix(c_)
      *** End   Add *** 09/15/22 WC2880 *******************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     FWDRRL     IF   E           K DISK
     FWDEHL1    IF   E           K DISK
      *** End Add ***  10/15/24  ***  WC2984  ************************
      *
      /copy sprDate
      //*** Begin Add *** 09/15/22 WC2880 *****************************
      /copy wpremplr
      //*** End   Add *** 09/15/22 WC2880 *****************************
     d*** Begin Add *** 06/13/24 ***********************************
     d/Copy sPrModule
     d/Copy wPrPmtSvc
     d*** End   Add *** 06/13/24 ***********************************
     D psds          esds                  extname(szq1p)
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANC DATES
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D
      //*** Begin Add *** 09/15/22 WC2880 ******************
       Dcl-S @sTRptDiv Packed(5:0) inz;
      //*** End   Add *** 09/15/22 WC2880 ******************
     d*** Begin Add *** 06/13/24 ***********************************
     d xxKey           s             10
     d isInvoiceCloud  s              1n   inz
     d inJson          s          32000    Varying
     d*** End   Add *** 06/13/24 ***********************************
     d*** Begin Add *** 06/12/24 *******************************************
     d isSRAs          s               n   Inz(*Off)
     d isSkipAR        s               n   Inz(*Off)
     d*** End   Add *** 06/12/24 *******************************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     d PCDays          S              3S 0
     d PPDays          S              3S 0
     d PRFact          S              5S 3
     d W@FrDt          S              7S 0
     d W@ToDt          S              7S 0
      *** End Add ***  10/15/24  ***  WC2984  ************************
      *
     C*=========================================================================
     C*  PLIST definitions
     C*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@EMRRN           9 0          PEM01 RRN
     C                   PARM                    @@ELRRN           9 0          PEL01 RRN
     C                   PARM                    @@MYRRN           9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0          ENDORSE DATE
     C                   PARM                    @@RESR           50            Rescind Reason
     C*
     C*  Agent/Producer retrieve program parameter list
     C     AGTPL         PLIST
     C                   PARM                    xxCO#                          Chg 08/07/19
     C                   PARM                    XXFND
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*=========================================================================
     C*  KLIST definitions
     C*=========================================================================
     C*
     C     EAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C                   KFLD                    XXARCD
     C                   KFLD                    XXPODT
      *
     C     QTKY          KLIST
     C                   KFLD                    xxCO#                          Chg 08/07/19
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKSTAT            1
     C                   KFLD                    KKAGT#
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*
     C* KEY LIST FOR THE GROUP AUXILIARY FILE
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C     DXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Cancellation Detail file
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
      *
     C     XAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      *  CANCELLATION HISTORY FILE
      *
     C     XHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXREQ#
      *
     C*=========================================================================
     C*  *LIKE DEFN statements
     C*=========================================================================
     C     *LIKE         DEFINE    EAARCD        XXARCD
     C     *LIKE         DEFINE    QTAGT#        KKAGT#
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EAPODT        ZZPODT
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA

     C                   EVAL      SYSDT = DateTo7(SystemDate())
     C                   EVAL      SYSTM = TimeTo6(SystemTime())
     C                   MOVE      *ON           FIRST             1
     C                   MOVE      'N'           XXRNEW            1

     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

        //*** Begin Add *** 06/13/24 ************************************
        // Determine if IVC active
        isInvoiceCloud = IsModEnabled('Invoice Cloud');
        //*** End   Add *** 06/13/24 ************************************
       //*** Begin Add *** 06/12/24 ****************************************
       // Check to see if SR and if SR Assessments is off
       isSRAs = *Off;
       xxKey = %EditC(ELbsCd:'X');
       Chain (xxCo#:xxKey) pbs01;
       If %Found(wtabl) And BSseFl = 'S';
          If ELsrAs = ' ';
             Chain (xxCo#:xxFnd) pf100;
             If %Found(wdf1l) And F1SRAs = 'Y';
                isSRAs = *On;
             EndIf;
          Else;
             isSRAs = *On;
          EndIf;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************

     C     @@EMrrn       CHAIN     pem0p                              99
     C     @@ELrrn       CHAIN(n)  pel0p                              99
      *
      *  07/28/14  Beg Add
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C***  Begin Add  ***  03/29/22 ********************
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C***  End   Add  ***  03/29/22 ********************
      *
      *  07/28/14  End Add
     C*
     C*  Get Group information
     C     FNKY          CHAIN     PFN00                              60
     C     F2KY          CHAIN     PF200                              60
     C*
     C     DXKY          CHAIN     pex00                              60
     C     DXKY          CHAIN     PDX00                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX00
     C                   ENDIF
     C*  Get Agent/Producer information
     C                   Z-ADD     *ZEROS        ##FYR
     C                   CALL      'WA002R'      AGTPL
     C                   EXSR      UpdResRec
       //*** Begin Add *** 06/13/24 ***********************************
       //
       // Send deleted/inactivated invoces to IVC
       If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd);
          FinalizeThirdPartyInvoicing(EAco#:EAfnd);
       Endif;
       //*** End   Add *** 06/13/24 ***********************************
     C                   SETON                                        LR
     C*=========================================================================
     C* Update data base record and after processing
     C*=========================================================================
     C     UpdResRec     BEGSR
     C*
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C*
     C*  Get the cancel date that corresponds to the most recent
     C*  effective date.
     C*
     C                   Z-ADD     1             I1                1 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
      *  Clear the cancel date
     C                   CLEAR                   CX(I1)
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   EVAL      EMDT = DateTo7(SystemDate())
     C                   EVAL      EMTIME = TimeTo6(SystemTime())
     C*
     C                   UPDATE    PEM0P
     C*
     C                   EXSR      WRTRESPEN
     C*
      *  Process renewal years
     C                   EXSR      SRRNEW
      *
      *** Begin Add *** 09/15/22 WC2880 *******************************
         // Handle situations where a division is being reactivated
         // that is on a policy that is already cancelled/non-renewed.

         CancellationDeletion();

      *** End   Add *** 09/15/22 WC2880 *******************************
     C                   ENDSR
     C*=========================================================================
     C* WRTRESPEN - Write data base record to Endorsement Print Workfile
     C*=========================================================================
     C     WRTRESPEN     BEGSR
     C*
     C                   CLEAR                   pen03
     C                   Z-ADD     *ZERO         ENSEQ#
     C                   MOVE      XXCO#         ENCO#
     C                   MOVE      XXFND         ENFND
     C                   MOVE      XXEMP#        ENEMP#
     C                   MOVE      XXDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   EVAL      ENEEDT = dtCymd(LDEEDT)
     C*
     C*  CHANGED EN**2 FIELDS TO EN**1 FIELDS
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C                   MOVEL     EMCNTY        ENCNT1
     C                   MOVE      F2CAR#        ENCAR1
     C*
     C     ELKY          CHAIN     PEL00                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60
     C*
     C                   MOVE      'Y'           ENRDDF
     C                   MOVEL(P)  DXDVDL        ENRMCD
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL(P)  ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C     @@MYRRN       CHAIN     PMY0P                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C*  CHANGED EN**1 FIELDS TO EN**2 FIELDS
     C                   MOVEL     *BLANKS       ENNAM2
     C                   MOVEL     *BLANKS       ENFDN2
     C*
     C                   MOVE      *BLANKS       ENANM1
     C                   MOVE      *BLANKS       ENANM2
     C*
     C                   MOVE      *BLANKS       ENAFN1
     C                   MOVE      *BLANKS       ENAFN2
     C*
     C                   MOVEL     *BLANKS       ENCON2
     C                   MOVEL     *BLANKS       ENATT2
     C                   MOVEL     *BLANKS       ENCN32
     C                   MOVE      *ZEROS        ENFEN2
     C                   MOVEL     *BLANKS       ENTOB2
     C                   MOVEL     *BLANKS       ENMA12
     C                   MOVEL     *BLANKS       ENMA22
     C                   MOVEL     *BLANKS       ENMCT2
     C                   MOVEL     *BLANKS       ENMST2
     C                   MOVEL     *BLANKS       ENMZP2
     C                   MOVEL     *BLANKS       ENPA12
     C                   MOVEL     *BLANKS       ENPA22
     C                   MOVEL     *BLANKS       ENPCT2
     C                   MOVEL     *BLANKS       ENPST2
     C                   MOVEL     *BLANKS       ENPZP2
     C                   MOVEL     *BLANKS       ENCNT2
     C                   Z-ADD     *ZEROS        ENCAR2
     C                   Z-ADD     *ZEROS        ENPFD2
     C*
     C                   EVAL      ENDT = DateTo7(SystemDate())
     C                   EVAL      ENTIME = TimeTo6(SystemTime())
     C*
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C*  Build up Before/After lines & write WMENP records.
     C*
     C     EMPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
      *
     C                   ELSE                                                   LOAD MAIL ADDR
      *
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       ENBLIN
      *
      *  Employer name
      *
     C                   MOVEL     EMNAME        ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENSEQ#
      *
      *  Employer federal name
      *
     C                   MOVEL     EMFDNM        ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENSEQ#
      *
      *  Employer address 1
      *
     C                   MOVEL     XXADR1        ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENSEQ#
      *
      *  Employer Address 2
      *
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *
      *  Employer city/ST/ZIP
      *
     C                   MOVEL     XXADR3        ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENSEQ#
      *
      * Reason for Rescission
      *
     C                   MOVEL     @@RESR        ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENSEQ#
      *
      *  Update Cancellation Detail
     C                   EXSR      UPDCX
      *
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     C     ELKY          CHAIN     PEL00                              60
     C     *IN60         IFEQ      *OFF
      *  Restore Original Premium on WDELP
     C                   EXSR      RSTPR
     C                   EndIf
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
      *
      *  Write CX History record
     C                   EXSR      WRTXH
      *
      *  Update QUOTE Tracking
     C                   EXSR      UPDQT
      *
      *  Restore A/R Billing records from WMXAP
     C                   EXSR      RSTAR
      *
      *
     C                   ENDSR
      *=========================================================================
      * WRTXH - Write CX History Record
      *=========================================================================
     C     WRTXH         BEGSR
      *
      *  Load WDXHP fields
      *
     C     XHKY          CHAIN     pxh00                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANK        XHRVFL
     C                   MOVEL     CXRSN         XHRSN
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C                   MOVE      CXERR         XHERR
     C                   Z-add     CXagt#        XHagt#                         Add 08/07/19
      *
     C                   EVAL      XHTRDT = DateTo7(SystemDate())
     C                   Z-ADD     CXCNC#        XHCNC#
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     C                   EVAL      XHDT = DateTo7(SystemDate())
     C                   EVAL      XHTIME = TimeTo6(SystemTime())
      *
     C                   WRITE     pxh00
     C                   ENDIF                                                   XHKY
      *
     C                   ENDSR                                                  WRTXH
      *=========================================================================
      * UPDCX - Update CX Detail Record
      *=========================================================================
     C     UPDCX         BEGSR
      *
     C                   MOVE      *HIVAL        XXREQ#
     C     CXKY1         SETGT     pcx00
     C     CXKY          READPE    pcx00                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     XXRNEW        IFEQ      'N'
     C     XXRNEW        OREQ      'Y'
     C     CXAFLT        ANDEQ     'Y'
     C     CXREIN        ANDNE     'Y'                                          NOT REINSTATED
      *
      *  Load WDCXP fields
      *
     C                   Z-ADD     CXCXDT        CXREDT
     C                   MOVE      *BLANK        CXRCRT
     C                   MOVE      *BLANKS       CXRDRT
     C                   MOVE      'Y'           CXREIN
     C                   MOVE      'N'           CXERR
     C                   Z-ADD     0             CXCFDT
     C                   Z-ADD     ##AGT#        CXAGT#
     C                   EVAL      CXRDAT = DateTo7(SystemDate())
      *
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
                         CXpdPr = 'Y';
      *** End Add *** 02/26/24 *** WC2956 **********************************
      *
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   EVAL      CXDT = DateTo7(SystemDate())
     C                   EVAL      CXTIME = TimeTo6(SystemTime())
      *
     C                   UPDATE    pcx00
     C                   ENDIF                                                  XXRNEW
      *
     C                   ENDIF                                                  CXKY
      *
     C                   ENDSR                                                  UPDCX
      *
      *
      *=========================================================================
      * UPDQT - Update QUOTE Tracking Record
      *=========================================================================
     C     UPDQT         BEGSR
      *
     C                   MOVE      'W'           KKSTAT
     C                   MOVE      ##AGT#        KKAGT#
     C     QTKY          SETGT     pqt07
     C     QTKY          READPE    pqt07                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     QTTYPE        IFEQ      'C'                                          CANCELLATION
     C     QTTYPE        OREQ      'T'                                          TRANSFER
     C     QTXFER        ANDEQ     'C'                                          XFER/CX
     C                   MOVE      'R'           QTTYPE
     C                   MOVE      *BLANKS       QTXFER
      *
     C                   EVAL      QTWKDT = DateTo7(SystemDate())
     C                   Z-ADD     QTWKDT        QTDT
     C                   EVAL      QTTIME = TimeTo6(SystemTime())
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   WRITE     pqt07
     C                   ENDIF                                                  QTTYPE
      *
     C                   ENDIF                                                  QTKY
      *
     C                   ENDSR                                                  UPDQT
      *
      *
      *=========================================================================
      * RSTAR - Restore A/R from WMAXP
      *=========================================================================
     C     RSTAR         BEGSR
      *
      *    Transfer any previously deleted
      *    a/r records to WMEAP determining new posting dates if necessary.
     C                   EVAL      SYSDT = DateTo7(SystemDate())
      *
      *      Read cancellation a/r detail file
     C     XAKY          CHAIN     pxa00                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  Get ZZPODT Date
      *
     C                   EXSR      GTZZDT
      *
      *      If posting date is greater than calc date then write record as
      *      is, otherwise write/update record with calc date as posting date
      *
     C     XAPODT        IFGT      ZZPODT
     C                   EXSR      HOLDAR
     C                   EXSR      WRTREC
     C                   DELETE    pxa00
      *
     C                   ELSE
      *
     C                   MOVE      XAARCD        XXARCD
     C                   Z-ADD     ZZPODT        XXPODT
     C     EAKY          CHAIN     pea14                              61
      *
     C     *IN61         IFEQ      *OFF
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   ADD       XAAMT         EAAMT
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   MOVE(P)   Q1PGM         EAPGM
     C                   Z-ADD     SYSDT         EATRDT
     C                   UPDATE    pea14
     C                   DELETE    pxa00
      *
     C                   ELSE
      *
     C                   Z-ADD     ZZPODT        XAPODT
     C                   EXSR      GETINV
     C                   EXSR      WRTREC
     C                   DELETE    pxa00
     C                   ENDIF                                                  *IN61=*OFF
      *
     C                   ENDIF                                                  XAPODT>ZZPODT
       //*** Begin Add *** 06/13/24 ****************************************
       If isInvoiceCloud and IcGroupOpt(XAco#:XAfnd) and EXdlAr = 'Y';

          inJson = '{"type":"billing",' +
             '"invoice":"'  + %Editc(XAninv:'X') + '",' +
             '"company":"'  + %Editc(XAco#:'X') + '",' +
             '"group":"'    + %Editc(XAfnd:'X') + '",' +
             '"employer":"' + %Editc(XAemp#:'X') + '",' +
             '"division":"' + %Editc(XAdiv:'X') + '",' +
             '"active":'    + '"true"' + ',' +
             '"delete":'    + '"false"}';

          ThirdPartyInvoicing(inJson);
       EndIf;
       //*** End   Add *** 06/13/24 ****************************************
      *
     C     XAKY          READE     pxa00                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  RSTAR
      *=========================================================================
      * GTZZDT - Get ZZPODT
      *=========================================================================
     C     GTZZDT        BEGSR
      *
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     XXCO#         #2CO#
     C                   Z-ADD     XXFND         #2FND
     C                   Z-ADD     XXEMP#        #2EMP#
     C                   Z-ADD     XXDIV         #2DIV
     C                   Z-ADD     XXFYR         #2FYR
      *
     C                   EVAL      #2DATE = dtMdyy(ELPTDT)
     C                   MOVEL     '*'           #2PLAN
     C                   CALL      'WA002R'
     C                   PARM                    #2CO#             3 0
     C                   PARM                    #2FND             3 0
     C                   PARM                    #2EMP#            9 0
     C                   PARM                    #2DIV             5 0
     C                   PARM                    #2FYR             3 0
     C                   PARM                    #2AGT#            5 0
     C                   PARM                    #2ANAM           60
     C                   PARM                    #2PROD            5 0
     C                   PARM                    #2PNAM           60
     C                   PARM                    #2AREA            3 0
     C                   PARM                    #2PHON            7 0
     C                   PARM                    #2PLAN            1
     C                   PARM                    #2DATE            8 0
      *
     C     #2PLAN        IFEQ      'A'
     C     DXKY          CHAIN     pex00                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EXAOID        XXDAYS            5 0
      *
      * POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C                   EVAL      #3DT = DateTo8(SystemDate())
     C                   CALL      'SRFUTR8'
     C                   PARM                    #3DT              8 0
     C                   PARM      XXDAYS        #3DAYS            5 0
     C                   PARM                    #3ERR             1
     C                   PARM                    #3TERM            1
      *
     C     #3ERR         IFNE      '1'
     C                   MOVE      #3DT          XXACDT            8 0
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  @@PLAN IFEQ
      *
      *  Determine post date for a/r (revised interim days + system dt)
     C                   EVAL      #3DT = DateTo8(SystemDate())
     C                   CALL      'SRFUTR8'
     C                   PARM                    #3DT              8 0
     C                   PARM      EXRVID        #3DAYS            5 0
     C                   PARM                    #3ERR             1
     C                   PARM                    #3TERM            1
      *
     C     #3ERR         IFNE      '1'
     C                   EVAL      ZZPODT = DtCymd(#3DT)
     C                   ENDIF
      *
     C     XXACDT        IFNE      *ZERO
     C                   EVAL      ZZPODT = DtCymd(XXACDT)
     C                   ENDIF                                                  XXACDT IFNE
      *
     C                   ENDSR                                                  GTZZDT
      *=========================================================================
      * SRRNEW - Process Renewal Years
      *=========================================================================
     C     SRRNEW        BEGSR
      *
     C                   MOVE      'Y'           XXRNEW            1
     C                   Z-ADD     XXFYR         SVFYR
     C     ELKY          SETGT     PEL00
     C     ELKY1         READE     PEL00                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELFYR         XXFYR
      *
      *  Update Cancellation Detail
      *
     C                   EXSR      UPDCX
      *
     C     CXAFLT        IFEQ      'Y'
      *
      *  Write CX History record
      *
     C                   EXSR      WRTXH
      *
      *  Restore A/R Billing records from WMXAP
      *
     C                   EXSR      RSTAR
      *
     C                   ENDIF                                                  CXAFLT
      *
     C     ELKY1         READE     PEL00                                  60
     C                   ENDDO                                                  ELKY
      *
     C                   Z-ADD     SVFYR         XXFYR
     C                   MOVE      'N'           XXRNEW            1
      *
     C                   ENDSR                                                  SRRNEW
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    XACO#         #ICO#
     C     *LIKE         DEFINE    XAFND         #IFND
     C     *LIKE         DEFINE    XAEMP#        #IEMP#
     C     *LIKE         DEFINE    XADIV         #IDIV
     C     *LIKE         DEFINE    XAFYR         #IFYR
     C     *LIKE         DEFINE    XAPODT        #IDT
     C     *LIKE         DEFINE    XANINV        #IINV#
     C*
     C     CLTID         IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      XACO#         #ICO#
     C                   PARM      XAFND         #IFND
     C                   PARM      XAEMP#        #IEMP#
     C                   PARM      XADIV         #IDIV
     C                   PARM      XAFYR         #IFYR
     C                   PARM      XAPODT        #IDT
     C     XANINV        PARM                    #IINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================
     C* Delete from A/R holding file when holding record was written today
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     A#KEY         KLIST
     C                   KFLD                    XACO#
     C                   KFLD                    XAFND
     C                   KFLD                    XAEMP#
     C                   KFLD                    XADIV
     C                   KFLD                    XAFYR
     C                   KFLD                    SYSDT
     C*
     C     A#KEY         CHAIN     pa#01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     XATRDT        IFEQ      EATRDT
     C     XAPODT        ANDEQ     EAPODT
     C     XAAMT         ANDEQ     EAAMT
     C     XANINV        ANDEQ     EANINV
     C                   DELETE    pa#01
     C                   LEAVE
     C                   ENDIF
     C*
     C     A#KEY         READE     pa#01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
      *================================================================
      *  WRTREC - writes record to a/r file
      *================================================================
     C     WRTREC        BEGSR
     C*
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XAFND         EAFND
     C                   MOVE      XAFYR         EAFYR
     C                   MOVE      XAEMP#        EAEMP#
     C                   MOVE      XADIV         EADIV
     C                   Z-ADD     XARDIV        EARDIV
     C                   MOVE      XAARCD        EAARCD
     C                   MOVE      XAARTY        EAARTY
     C                   MOVE      XADESC        EADESC
     C                   MOVE      XAAMT         EAAMT
     C                   MOVE      XAPODT        EAPODT
     C                   MOVE      XAEDIT        EAEDIT
     C                   MOVE      XASTAT        EASTAT
     C                   MOVE      XABUL1        EABUL1
     C                   MOVE      XACHK#        EACHK#
     C                   MOVE      XANINV        EANINV
     C                   MOVE      XALATE        EALATE
     C                   MOVE      XAFL01        EAFL01
     C                   MOVE      XAFL02        EAFL02
     C                   MOVE      XADT01        EADT01
     C                   MOVE      XADT02        EADT02
     C                   MOVE      XACTYP        EACTYP
     C                   MOVE      XACCTP        EACCTP
     C                   MOVE      XACNUM        EACNUM
     C                   MOVE      XACEXP        EACEXP
     C                   MOVE      XACNAM        EACNAM
      *
      *    Update these fields with reinstatement info
     C                   Z-ADD     SYSDT         EATRDT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   EVAL      EATIME = TimeTo6(SystemTime())
     C                   Z-ADD     SYSDT         EADT
      *
          //*** Beg Add ***** 06/29/22 ***************************
     C                   MOVE      XAref#        EAref#
          //*** End Add ***** 06/29/22 ***************************
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA14
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDSR                                                  WRTREC
      *** Begin Add *** 09/15/22 WC2880 *******************************

      *** Beg Add *** 10/15/24 *** WC2984 *****************************
      *================================================================
      *  RSTPR  - Restore Original Premium
      *================================================================
         BegSr RSTPR;

          // Get Policy Period in Days
          PPDays = 365;

          // Get Policy Covered in Days (From Policy Start Date till Cancellation)
          W@FrDt = ELPFDT;
          W@ToDt = CXCXDT;
          PCDays = %Diff (%Date(%Char(W@ToDt):*CYMD0) :
                          %Date(%Char(W@FrDt):*CYMD0) : *D);

          // Calculate Prorate Factor
          PRFact = %DecH(PCDays/PPDays:5:3);

          // Restore Original Premium from WDEHP

          If ELFAF <> ' ' and ELARSM = 'Y';
          // Calculate Tax Due based on Current Value & Prorate Factor
             If ELTXDU <> 0 ;
                ELTXDU = %DecH((ELTXDU/PRFact*ELPROR):9:0);
             EndIf;

             SetGT (XXCO#:XXFND:XXEMP#:ELDIV:XXFYR) WDEHL1;
             ReadPE (XXCO#:XXFND:XXEMP#:ELDIV:XXFYR) WDEHL1;
             DoW Not %EOF;
                 If EHFAF <> 'A';
                    ELANPR = EHWAPR ;                        // Est Annual Pre
                    EL1AAM = EHW1AS ;                        // 1st Assessment
                    EL2AAM = EHW2AS ;                        // 2nd Assessment
                    EL3AAM = EHW3AS ;                        // 3rd Assessment
                    EL4AAM = EHW4AS ;                        // 4th Assessment
                    EL5AAM = EHW5AS ;                        // 5th Assessment
                    EL6AAM = EHW6AS ;                        // 6th Assessment
                    EL7AAM = EHW7AS ;                        // 7th Assessment
                    ELSFAM = EHWSF  ;                        // State Tax
                    ELMFAM = EHWMF  ;                        // State Tax
                    Update PEL00;
                    Leave;
                 EndIf;
                 ReadPE (XXCO#:XXFND:XXEMP#:ELDIV:XXFYR) WDEHL1;
             EndDo;
          EndIf;

         EndSr;
      *** End Add ***  10/15/24  ***  WC2984  ************************
       //******************************************************************
       // PROCEDURE: Cancellation & Deletion Overlap on Divisions.
       //******************************************************************
       Dcl-proc CancellationDeletion;
       Dcl-S i1 zoned(1:0) inz;

       // Obtain State Reporting Division
       @sTRptDiv = GetStRptDv(xxCo#:xxFnd:xxEmp#);
       If @sTRptDiv <> xxDiv;
          //----------------------------------------------------------
          // Create current Div Rec on the Cancellation Detail File
          // for current policy year
          //----------------------------------------------------------
          Setgt (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv:*hival) pcx00;
          Readpe (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv) pcx00;
          // If the state reporting division is cancelled
          If not %eof(WDCXL) and CXrein <> 'Y';
             // Find the latest request # value from the
             // existing WDCXP record on the division
             Setgt (xxCo#:xxFnd:xxEmp#:xxDiv:xxFYR:*hival) pcx03;
             Readpe (xxCo#:xxFnd:xxEmp#:xxDiv:xxFYR) pcx03;
             If %eof(WDCXL3);
                Clear c_CXreq#;
             Endif;

             CXdiv = xxDiv;
             CXreq# = c_CXreq# + 1;
             CXdt = DateTo7(SystemDate());
             CXtime = TimeTo6(SystemTime());
             CXuser = Q1user;
             CXpgm = Q1pgm;
             CXidt = CXdt;
             CXitm = CXtime;
             CXiusr = CXuser;
             CXipgm = CXpgm;
             Write PCX00;

             // Update employer master file WMEMP with the cancellation date
             // CXCXDT. Get the cancel date that corresponds to the most
             // recent effective date.
             Chain @@EMrrn pem0p;
             i1 = %lookup(0:EF:1);

             If i1 <> 0;
                i1 = i1 - 1;
             Else;
                i1 = 5;
             Endif;

             // If index is not 0, set array element te Cancel Date
             If i1 <> 0;
                CX(i1) = CXcxdt;
             Endif;

             EMuser = Q1user;
             EMpgm = Q1pgm;
             EMdt = DateTo7(SystemDate());
             EMtime = TimeTo6(SystemTime());

             Update PEM0P;

             //----------------------------------------------------------
             // Create current Div Rec on the Cancellation Hist File for
             // current policy year
             //----------------------------------------------------------
             Setgt (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv:*hival) pxh00;
             Readpe (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv) pxh00;
             If not %eof(WDXHL);
                XHdiv = xxDiv;
                XHreq# = c_CXreq# + 1;
                XHdt = DateTo7(SystemDate());
                XHtime = TimeTo6(SystemTime());
                XHuser = Q1user;
                XHpgm = Q1pgm;
                Write pxh00;
             Endif;

             //----------------------------------------------------------
             // Retrieve Future Policy Years and create PCX00 and
             // PXH00 records for the curr Div on the renewal years
             //----------------------------------------------------------
             Setgt (xxCo#:xxFnd:xxEmp#:xxDiv:xxFYR) pel00;
             Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pel00;
             Dow not %eof(WDELL);
                xxFYR = ELfyr;
                // Find the latest request # value for the renewal
                // years existing WDELL record on the division
                Setgt (xxCo#:xxFnd:xxEmp#:xxDiv:xxFYR:*hival) pcx03;
                Readpe (xxCo#:xxFnd:xxEmp#:xxDiv:xxFYR) pcx03;
                If %eof(WDCXL3);
                   Clear c_CXreq#;
                Endif;

                // Create current Div Rec on the Cancellation Detail file
                // for subsequent policy year
                Setgt (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv:*hival) pcx00;
                Readpe (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv) pcx00;
                // Check state reporting division is cancelled
                If not %eof(WDCXL) and CXrein <> 'Y';
                   CXdiv = xxDiv;
                   CXreq# = c_CXreq# + 1;
                   CXdt = DateTo7(SystemDate());
                   CXtime = TimeTo6(SystemTime());
                   CXuser = Q1user;
                   CXpgm = Q1pgm;
                   CXidt = CXdt;
                   CXitm = CXtime;
                   CXiusr = CXuser;
                   CXipgm = CXpgm;
                   Write pcx00;
                Endif;

                // Create current Div Rec on the Cancellation History File
                // for subsequent policy year
                Setgt (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv:*hival) pxh00;
                Readpe (xxCo#:xxFnd:xxFYR:xxEmp#:@sTRptDiv) pxh00;
                If not %eof(WDXHL);
                   XHdiv = xxDiv;
                   XHreq# = c_CXreq# + 1;
                   XHdt = DateTo7(SystemDate());
                   XHtime = TimeTo6(SystemTime());
                   XHuser = Q1user;
                   XHpgm = Q1pgm;
                   Write PXH00;
                Endif;

                // Retrieve next policy year record
                Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pel00;
             Enddo;
          Endif;
       Endif;
       End-proc;
      *** End   Add *** 09/15/22 WC2880 *******************************
