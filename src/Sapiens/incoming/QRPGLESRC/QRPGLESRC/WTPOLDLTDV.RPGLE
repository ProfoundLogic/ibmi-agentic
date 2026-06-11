     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLDLTDV -  Deletion of Division - Mid term Endorsement
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO)
     H BNDDIR('WTBNDDIR')
     H ACTGRP(*CALLER)
     h**********************************************************************
    *===============================================================*
    *  Created   : 05/23/12                                         *
    *  Programmer: C:191                                            *
    *  Project   : WC2116                                           *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      *  12/13/24 C:65  Support: 118183 (Sunz)                        *
      *                 No need to set the "in processing" flag on    *
      *                 WDCXP records if called from PEO Import via   *
      *                 the W0051R30 program. Jobs immediately submit.*
      *                                                               *
      * 10/15/24 C:320 Project: WC2984                                *
      *                Update Employer Detail WDELP with prorated     *
      *                amounts for Revised Bill to pickup.            *
      *                Adjust A/R if the Division is Summarized.      *
      *                                                               *
      * 06/13/24 C:062 Support: 115758(133)                           *
      *                Send deletion or inactivation to IVC when      *
      *                invoices are removed                           *
      *                                                               *
      * 02/26/24 C:340 Support: WC2956                                *
      *                Reset Flag - Cancellation/Reinstatement Process*
      *                             Control                           *
      *                                                               *
      * 09/14/22 C:344 Support: WC2880                                *                            *
      *                Add processing to handle situations where a    *                            *
      *                division is being deleted off a policy that is *                            *
      *                already cancelled/non-renewed                  *                            *
      *                                                               *                            *
      * 06/29/22 C:340 Support: WC2854                                *                            *
      *                Invoice Cloud:  Retain Payment Ref#            *                            *
      *                                                               *                            *
      *  01/28/22 C:136 Support: 100356 (Sunz Only)                   *
      *                 Don't clear A/R Summary flag in SRRNEW        *
      *                                                               *
      *  01/11/22 C:65  Support: 100105 (CoveRisk)                    *
      *                 Use system date field for A/R removal check   *
      *                 as the xxDate field gets zero'd out during    *
      *                 quote tracking update.                        *
      *                                                               *
      *  01/22/20 C:136 Support: 86536                                *
      *                 Reimplement 150WBU                            *
      *                                                               *
      *  08/07/19 C:136 Support: 80573                                *
      *                 Ensure QTKY and WA002R get a co# so that      *
      *                 Quote Tracking record is written              *
      *                                                               *
      *  07/18/19 C:300 Support: 80918 (LUBA) (Removed 08/07/19)      *
      *                 QA V19R1 testing Mid Term End Deletion of Div *
      *                 1.Receive unexpected error -- Update or Delete*
      *                 in the file WDCXL without prior input operation
      *                 2.Write a new record in WDCXP when a Division *
      *                 is Deleted in Mid-Term Endosrsement           *
      *                 3.Correct sequence number for Renewal Policy  *
      *                 Year for writing new reords in WDCXP and WDXHP*
      *                                                               *
      *  11/01/18 C:291 Support: 72916  (Removed 08/07/19)            *
      *                 Deletion of Division on a Cancelled Policy:   *
      *                 Update WDCXP with new CX date and CX          *
      *                 transaction date.                             *
      *                 Add New WDXHP record with new Transaction     *
      *                 and Effective date and set revision flag.     *
    *                                                               *
      *  05/17/18 C:136 Support: 51055                                *
      *                 List address on before and after lines        *
      *                 for Delete Division endorsement. The after    *
      *                 line will list the reason and cx date.        *
    *                                                               *
      *  10/13/17 C:65  Project: 150WBU                               *
      *                 Do not clear a/r summary flag for client 150. *
    *                                                               *
    *  05/31/16 Support: 25606                                      *
    *           Add chain to Admin/DARTS for Delete Future A/R      *
    *           Flag                                                *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     F*  Physical file definition (for maintaining record)
     FWMEMP     UF A E             DISK    rename(pem01:pem0p)
     FWDELP     IF   E             DISK    rename(pel01:pel0p)
     FWTMYP     IF   E             DISK    rename(pmy01:pmy0p)
     FWDELL     UF   E           K DISK    rename(pel01:pel00)
     FWDF2L     IF   E           K DISK    rename(pf201:pf200)
     FWMENL3    IF A E           K DISK    rename(pen01:pen03)
     FWMFNL     IF   E           K DISK    rename(pfn01:pfn00)
     FWMDXL     IF   E           K DISK    rename(pdx01:pdx00)
     F** Cancellation Detail File
     FWDCXP     O    E             DISK    rename(pcx01:pcx0p)
     F*** Begin Chg *** 09/14/22 WC2880 *******************************
     F*****WDCXL     IF   E           K DISK    rename(pcx01:pcx00)
     FWDCXL     UF   E           K DISK    rename(pcx01:pcx00)
     F*** End   Chg *** 09/14/22 WC2880 *******************************
     F** Cancellation History File
     FWDXHP     O    E             DISK    rename(pxh01:pxh0p)
     FWAQTL7    IF A E           K DISK    rename(pqt01:pqt07)
     F*  Employer Accounts Receivable
     FWMEAL6    UF   E           K DISK    rename(pea01:pea06)
     FWMA#P     O  A E           K DISK    rename(pea01:pa#0p)
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
     FWMXAP     O    E             DISK
     F*
     F* ADMIN/DARTS Control File
     FWMEXL     IF   E           K DISK
     FWMIFL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      *** Begin Add ***  09/14/22 WC2880 *******************************
      * Cancellation History File
     FWDXHL     IF A E           K DISK    rename(pxh01:pxh00)
      *** End   Add ***  09/14/22 WC288O *******************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     FWDRRL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
      *** End Add ***  10/15/24  ***  WC2984  ************************
      *
      /copy sprDate
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
     D*** Begin Add ***  09/14/22 WC2880 *******************************
     D@eNEfDte         s              7s 0 inz
     D*** End   Add ***  09/14/22 WC2880 *******************************
     d*** Begin Add *** 06/13/24 ***********************************
     d isInvoiceCloud  s              1n   inz
     d inJson          s          32000    Varying
     d isInvSent       s              1n
     d*** End   Add *** 06/13/24 ***********************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     d PCDays          S              3S 0
     d PPDays          S              3S 0
     d PRFact          S              5S 3
     d W@FrDt          S              7S 0
     d W@ToDt          S              7S 0
      *** End Add ***  10/15/24  ***  WC2984  ************************
     D*** begin add ***  12/13/24  *****************************************
     d pgmName         s             10
     d pgmExists       s              1
     d isPEOImport     s               n   Inz(*Off)
     d ChkPgmStack     pr                  ExtPgm('SRCHKPGM')
     d   pgmName                     10
     d   pgmExists                    1
     D*** end add *****  12/13/24  *****************************************
      *
     C*=========================================================================
     C*  PLIST definitions
     C*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@EMRRN           9 0          PEM01 RRN
     C                   PARM                    @@ELRRN           9 0          PEL01 RRN
     C                   PARM                    @@MYRRN           9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0          ENDORSE DATE
     C                   PARM                    @@DELR           50            Delete Reason
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
     C*  Employer Accounts Receivable
     C     EAKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C                   KFLD                    XXKDAT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C*
     C*  Transaction Type Code Table
     C     TTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*  RATE SET CODE FILE
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  INSTALLMENT FEE FILE
     C     IFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXEFDT            7 0
     C*
     C     IFKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
      *
      *** Begin Add ***  09/14/22 WC2880 *******************************
     C     XHKY          KLIST
     C                   KFLD                    CXco#
     C                   KFLD                    CXfnd
     C                   KFLD                    CXfyr
     C                   KFLD                    CXemp#
     C                   KFLD                    CXdiv
     C                   KFLD                    CXreq#
      *** End   Add ***  09/14/22 WC2880 *******************************
      *
     C*=========================================================================
     C*  *LIKE DEFN statements
     C*=========================================================================
     C     *LIKE         DEFINE    QTAGT#        KKAGT#
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         XSRDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C     *LIKE         DEFINE    TTCO#         KKCO#
     C     *LIKE         DEFINE    TTKEY         KKKEY
     C     *LIKE         DEFINE    CXCXDT        XXKDAT
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*** begin add ***  12/13/24  *****************************************
       pgmName   = 'W0051R30';
       pgmExists = *Off;
       ChkPgmStack(pgmName:pgmExists);
       If pgmExists = *On;
          isPEOImport = *On;
       EndIf;
     C*** end add *****  12/13/24  *****************************************

     C*** begin add ***  10/13/17  *************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** end add *****  10/13/17  *************************************
     C                   EVAL      SYSDT = DateTo7(SystemDate())
     C                   EVAL      SYSTM = TimeTo6(SystemTime())
     C                   MOVE      *ON           FIRST             1
     C     @@EMrrn       CHAIN     pem0p                              99
     C     @@ELrrn       CHAIN(n)  pel0p                              99
     C*
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR

        //*** Begin Add *** 06/13/24 ************************************
        // Determine if IVC active
        isInvoiceCloud = IsModEnabled('Invoice Cloud');
        //*** End   Add *** 06/13/24 ************************************
     C*
     C*  Get Group information
     C     FNKY          CHAIN     PFN00                              60
     C     F2KY          CHAIN     PF200                              60
      *** beg add ***** 05/31/16 ****************
     C     FNKY          CHAIN     PEX01                              60
      *** beg add ***** 05/31/16 ****************
     C*
     C     DXKY          CHAIN     PDX00                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX00
     C                   ENDIF
     C*  Get Agent/Producer information
     C                   Z-ADD     *ZEROS        ##FYR
     C                   CALL      'WA002R'      AGTPL
     C                   EXSR      UpdDelRec
     C                   SETON                                        LR
     C*=========================================================================
     C* Update data base record and after processing
     C*=========================================================================
     C     UpdDelRec     BEGSR
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
     C*
      *** Beg Add *** 09/14/22 WC2880 ************************************
      * Set the Endorsement Effective Date Work Field
     C                   EVAL      @eNEfDte= dtCymd(@@ENDT)
      *** If cancel date in the array element is cancelled
      *** OR the endorsement effective date is less than the value in the
      *** cancel date array element then set the cancel date to the
      *** endorsement effective date.
      *** End Add *** 09/14/22 WC2880 ************************************
     C     CX(I1)        IFEQ      *ZERO
      *** Beg Add *** 09/14/22 WC2880 *********
     C     @eNEfDte      ORLT      CX(I1)
      *** End Add *** 09/14/22 WC2880 *********
     C*  Set the cancel date to the endorsement effective date.
     C                   EVAL      CX(I1) = dtCymd(@@ENDT)
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   EVAL      EMDT = DateTo7(SystemDate())
     C                   EVAL      EMTIME = TimeTo6(SystemTime())
     C*
     C                   UPDATE    PEM0P
     C*
     C                   EXSR      WRTDELPEN
     C*
      *  Process renewal years
     C                   EXSR      SRRNEW
     C                   ENDSR
     C*=========================================================================
     C* WRTDELPEN - Write data base record to Endorsement Print Workfile
     C*=========================================================================
     C     WRTDELPEN     BEGSR
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C*
     C     ENKY          SETGT     PEN03
     C     ENKY          READPE    PEN03                                  60
     C*
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C                   ENDIF
     C*
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
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
     C*
     C     ELKY          READE     PEL00                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60
     C*
     C                   MOVE      'Y'           ENDLDF
     C                   MOVEL(P)  DXDVDL        ENRMCD
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     @@MYRRN       CHAIN     PMY0P                              60
     C*
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
     C*
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C*
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C*
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C*
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   LOAD MAIL ADDR
     C*
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C*
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C*
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C*
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      *** beg chg ***** 05/17/18 **************************
     C***                   MOVE      *BLANKS       ENBLIN
     C****
     C****  Employer name
     C****
     C***                   MOVEL     EMNAME        ENALIN
     C***                   WRITE     pen03
     C***                   ADD       1             ENSEQ#
     C****
     C****  Employer federal name
     C****
     C***                   MOVEL     EMFDNM        ENALIN
     C***                   WRITE     pen03
     C***                   ADD       1             ENSEQ#
     C****
     C****  Employer address 1
     C****
     C***                   MOVEL     XXADR1        ENALIN
     C***                   WRITE     pen03
     C***                   ADD       1             ENSEQ#
     C****
     C****  Employer Address 2
     C****
     C***     XXADR2        IFNE      *BLANKS
     C***                   MOVEL     XXADR2        ENALIN
     C***                   WRITE     pen03
     C***                   ADD       1             ENSEQ#
     C***                   ENDIF
     C****
     C****  Employer city/ST/ZIP
     C****
     C***                   MOVEL     XXADR3        ENALIN
     C****
     C***                   WRITE     pen03
     C****
     C***                   ADD       1             ENSEQ#
     C****
     C**** Reason for deletion
     C****
     C***                   MOVEL     @@DELR        ENALIN
     C****
     C***                   WRITE     pen03
     C****
     C***                   ADD       1             ENSEQ#

      /Free
       // DBA Name
       ENblin = EMname;
       ENalin = EMname;
       Write pen03;
       ENseq# += 1;
       // Fed Name
       ENblin = EMfdnm;
       ENalin = EMfdnm;
       Write pen03;
       ENseq# += 1;
       // Add line 1
       ENblin = xxAdr1;
       ENalin = xxAdr1;
       Write pen03;
       ENseq# += 1;
       // Add line 2
       ENblin = xxAdr2;
       ENalin = xxAdr2;
       Write pen03;
       ENseq# += 1;
       // City St Zip
       ENblin = xxAdr3;
       ENalin = xxAdr3;
       Write pen03;
       ENseq# += 1;
       // Deletion date
       ENblin = *Blanks;
       ENalin = 'Deleted: ' + %Char(%Date(LDeedt:*Usa):*Usa);
       Write pen03;
       ENseq# += 1;
       // Deletion reason
       ENalin = @@DelR;
       Write pen03;
       ENseq# += 1;
      /End-Free
      *** end chg ***** 05/17/18 **************************
     C*
     C*  Write Cancellation Detail and History records

     C*  Get the next Request#
     C                   MOVE      *HIVAL        XXREQ#
      *** Begin Chg *** 09/14/22 WC2880 ***********************************
     C                   EXSR      SRREIN
      *** End   Chg *** 09/14/22 WC2880 ***********************************
     C*
     C     CXKY1         SETGT     PCX00
      *** Begin Chg *** 09/14/22 WC2880 *******************
     C***  CXKY          READPE    PCX00                                  60
     C     CXKY          READPE(N) PCX00                                  60
      *** End   Chg *** 09/14/22 WC2880 *******************
     C*
     C     *IN60         IFEQ      *OFF
     C     CXREQ#        ADD       1             XXREQ#
     C                   ELSE
     C                   Z-ADD     1             XXREQ#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Load WDCXP fields
     C*
     C                   MOVE      XXCO#         CXCO#
     C                   MOVE      XXFND         CXFND
     C                   MOVE      XXEMP#        CXEMP#
     C                   MOVE      XXDIV         CXDIV
     C                   MOVE      LDFYR         CXFYR
     C                   MOVE      XXREQ#        CXREQ#
     C                   EVAL      CXCXDT = dtCymd(LDEEDT)
     C                   MOVE      *BLANKS       CXRSN
     C                   MOVE      *BLANKS       CXCOM1
     C                   MOVE      *BLANKS       CXCOM2
     C                   MOVE      *BLANK        CXCCRT
     C                   MOVE      *BLANKS       CXCDRT
     C                   MOVE      'N'           CXREIN
     C                   MOVE      *ZEROS        CXREDT
     C                   MOVE      *ZEROS        CXRDAT
     C                   MOVE      *BLANK        CXRCRT
     C                   MOVE      *BLANKS       CXRDRT
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      *BLANK        CXREVL
     C                   MOVE      'Y'           CXCFFL
     C                   EVAL      CXCFDT = dtCymd(@@ENDT)
     C                   MOVE      *BLANK        CXCEMN
     C                   MOVE      *ZEROS        CXDOI#
     C                   MOVE      *ZEROS        CXCNC#
     C                   MOVE      *BLANK        CXERR
     C                   MOVE      *ZEROS        CXAMTD
     C                   MOVE      *ZEROS        CXRFEE
     C                   MOVE      *BLANKS       CXRFDS
     C                   MOVE      *BLANKS       CXCAP#
     C                   MOVE      *ZEROS        CXRPMY
     C                   EVAL      CXCFPD = DateTo7(systemDate())
     C                   MOVE      *BLANKS       CXCARR
     C                   MOVE      'Y'           CXENFL
     c                   Z-Add     ##Agt#        CXagt#                         Add 08/07/19
     C*
     C                   EVAL      CXDT = DateTo7(systemDate())
     C                   EVAL      CXCDAT = DateTo7(systemDate())
     C                   EVAL      XXDATE = DateTo7(systemDate())
     C                   EVAL      CXTIME = TimeTo6(SystemTime())
     C*
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
     C*** begin add ***  12/13/24  *****************************************
     C                   Clear                   CXPdpr
     C                   If        not IsPEOImport
     C*** end add *****  12/13/24  *****************************************
                         CXpdPr = 'Y';
     C*** begin add ***  12/13/24  *****************************************
     C                   Endif
     C*** end add *****  12/13/24  *****************************************
      *** End Add *** 02/26/24 *** WC2956 **********************************
     C*
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C*
     C                   MOVE      Q1USER        CXIUSR
     C                   MOVE      Q1PGM         CXIPGM
     C                   EVAL      CXITM = TimeTo6(SystemTime())
     C                   Z-ADD     CXDT          CXIDT
     C*  Load WDXHP fields
     C                   MOVE      XXCO#         XHCO#
     C                   MOVE      XXFND         XHFND
     C                   MOVE      XXEMP#        XHEMP#
     C                   MOVE      XXDIV         XHDIV
     C                   MOVE      LDFYR         XHFYR
     C                   MOVE      XXREQ#        XHREQ#
     C                   EVAL      XHCXDT = dtCymd(LDEEDT)
     C                   MOVE      *BLANKS       XHRSN
     C                   MOVE      *ZEROS        XHREDT
     C                   MOVE      *ZEROS        XHRDAT
     C                   MOVE      *ZEROS        XHCNC#
     C                   MOVE      *BLANK        XHERR
     C                   MOVE      'Y'           XHENFL
     c                   Z-add     ##Agt#        XHagt#                         Add 08/07/19
     C*
     C                   EVAL      XHDT = DateTo7(systemDate())
     C                   EVAL      XHCDAT = DateTo7(systemDate())
     C                   EVAL      XHTIME = TimeTo6(SystemTime())
     C*
     C                   MOVEL     Q1USER        XHUSER
     C                   MOVEL     Q1PGM         XHPGM
      *
     C                   MOVE      'W'           KKSTAT
     C                   MOVE      ##AGT#        KKAGT#
     C     QTKY          SETGT     PQT07
     C     QTKY          READPE    PQT07                                  60
      *
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     0             XXDATE            7 0
     C                   EVAL      QTWKDT = DateTo7(SystemDate())
     C                   EVAL      QTTIME = TimeTo6(SystemTime())
     C*
     C                   MOVE      'C'           QTTYPE
     C                   MOVE      Q1USER        QTUSER
     C
     C                   MOVE      Q1PGM         QTPGM
     C                   Z-ADD     QTWKDT        QTDT
     C*
     C                   MOVE      QTUSER        QTIUSR
     C                   MOVE      QTTIME        QTITM
     C                   MOVE      QTPGM         QTIPGM
     C                   MOVE      QTDT          QTIDT
     C*
     C                   WRITE     PQT07
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*  Write WDCXP and WDXHP records
     C*
     C                   WRITE     PCX0P
     C                   WRITE     PXH0P
     C     ELKY          CHAIN     PEL00                              60
     C*** begin chg ***  01/22/20  *************************************
     C*********          MOVE      *BLANKS       ELARSM
      *** Beg Del ***  10/15/24  ***  WC2984  ************************
     C***                If        cltID <> '150'
     C***                Eval      Elarsm = *Blanks
     C***                Endif
      *** Beg Del ***  10/15/24  ***  WC2984  ************************
     C*** end chg *****  01/22/20  *************************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     C                   EXSR      SRPRPR
      *** End Add ***  10/15/24  ***  WC2984  ************************
     C                   UPDATE    PEL00
     C*  Delete billings which are posted on or after the effective
     C*  date of cancellation (CXCXDT) if admin/DARTS flag = 'Y'
     C*
     C     EXDLAR        IFEQ      'Y'
     C                   EXSR      SRDBIL
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*================================================================
     C*  SRDBIL - For billings posted on or after cancellation date:
     C*           write record to WMXAP and then delete from WMEAP
     C*================================================================
     C     SRDBIL        BEGSR
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   MOVE      ELPFDT        XXEFDT
     C  N60IFKY          SETGT     PIF01
     C  N60IFKY1         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   ENDIF
     C     CXCXDT        IFNE      *ZERO
     C*** begin chg ***  01/11/22  *********************************************
     C*****CXCXDT        IFLT      XXDATE
     C*****              Z-ADD     XXDATE        XXKDAT
     C     CXCXDT        IFLT      sysDt
     C                   Z-ADD     sysDt         XXKDAT
     C*** end chg *****  01/11/22  *********************************************
     C                   ELSE
     C                   Z-ADD     CXCXDT        XXKDAT
     C                   ENDIF                                                  CXCXDT IFLT XXDATE
       //*** Begin Add *** 06/13/24 ***********************************
       //
       // Initialize A/R Sent Flag
       isInvSent = *Off;
       //*** End   Add *** 06/13/24 ***********************************
     C*
     C     EAKY1         SETLL     PEA06
     C     EAKY2         READE     PEA06                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAPODT        IFGE      CXCXDT
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'H'
     C*           Write record then delete
     C                   EXSR      SRWRIT
     C                   EXSR      HOLDAR
     C                   DELETE    PEA06
       //*** Begin Add *** 06/13/24 ****************************************
       ExSr DltInvIVC;
       //*** Begin Add *** 06/13/24 ****************************************
     C                   ENDIF                                                  EAARTY
     C     EAARCD        IFEQ      IFIFBL
     C*           Write record then delete
     C                   EXSR      SRWRIT
     C                   EXSR      HOLDAR
     C                   DELETE    PEA06
       //*** Begin Add *** 06/13/24 ****************************************
       ExSr DltInvIVC;
       //*** Begin Add *** 06/13/24 ****************************************
     C                   ENDIF                                                  EAARCD IFEQ
     C*
     C     EAARTY        IFEQ      'F'
     C                   MOVE      EACO#         KKCO#
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C*
     C  N60TTCTYP        IFEQ      'T'
     C     TTCTYP        OREQ      'M'
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C*           Write record then delete
     C                   EXSR      SRWRIT
     C                   EXSR      HOLDAR
     C                   DELETE    PEA06
       //*** Begin Add *** 06/13/24 ****************************************
       ExSr DltInvIVC;
       //*** Begin Add *** 06/13/24 ****************************************
     C                   ENDIF                                                  TTCTYP
     C*
     C                   ENDIF                                                  EAARTY = F
     C*
     C                   ENDIF                                                  EAPODT
     C*
     C     EAKY2         READE     PEA06                                  60
     C                   ENDDO                                                  *IN60
       //*** Begin Add *** 06/13/24 ***********************************
       //
       // Send deleted/inactivated invoces to IVC
       If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd) and isInvSent = *On;
          FinalizeThirdPartyInvoicing(EAco#:EAfnd);
       Endif;
       //*** End   Add *** 06/13/24 ***********************************
     C*
     C                   ENDIF                                                  CXCXDT
     C*
     C                   ENDSR                                                  SRDBIL
       //*** Begin Add *** 06/13/24 ****************************************
       //*==================================================================
       //* Delete invoices from IVC
       //*==================================================================
       BegSR DltInvIVC;

          // Remove or Inactivate Invoices
          If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd);
             inJson = '{"type":"billing",' +
               '"invoice":"'   + %Editc(EAninv:'X') + '",' +
               '"company":"'   + %Editc(EAco#:'X') + '",' +
               '"group":"'     + %Editc(EAfnd:'X') + '",' +
               '"employer":"'  + %Editc(EAemp#:'X') + '",' +
               '"division":"'  + %Editc(EAdiv:'X') + '",';

             // If IC flag Inactivate Invoices after Cx = Y then inactive
             // Otherwise delete
             If EXicCxInv = 'Y';
                inJson = inJson + '"active":' + '"false"' + ',' +
                '"delete":' + '"false"';
             Else;
                inJson = inJson + '"active":' + '"true"' + ',' +
                '"delete":' + '"true"';
             Endif;

             inJson = inJson + '}';
             ThirdPartyInvoicing(inJson);
             isInvSent = *On;
          EndIf;

       EndSR;
       //*** End   Add *** 06/13/24 ****************************************
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     EATRDT        IFLT      SYSDT
     C*
     C                   ADD       1             SYSTM
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1USER        EAUSER
     C                   Z-ADD     SYSDT         EADT
     C                   Z-ADD     SYSTM         EATIME
     C*
     C                   WRITE     pa#0p
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C*  SRWRIT - Write WMEAP record to WMAXP
     C*================================================================
     C     SRWRIT        BEGSR
     C*
     C                   MOVE      EACO#         XACO#
     C                   MOVE      EACO#         XACO#
     C                   MOVE      EAFND         XAFND
     C                   MOVE      EAFYR         XAFYR
     C                   MOVE      EAEMP#        XAEMP#
     C                   MOVE      EADIV         XADIV
     C                   Z-ADD     EARDIV        XARDIV
     C                   MOVE      EAARCD        XAARCD
     C                   MOVE      EAARTY        XAARTY
     C                   MOVE      EADESC        XADESC
     C                   MOVE      EAAMT         XAAMT
     C                   MOVE      EATRDT        XATRDT
     C                   MOVE      EAPODT        XAPODT
     C                   MOVE      EAEDIT        XAEDIT
     C                   MOVE      EASTAT        XASTAT
     C                   MOVE      EAUSER        XAUSER
     C                   MOVE      EAPGM         XAPGM
     C                   MOVE      EATIME        XATIME
     C                   MOVE      EADT          XADT
     C                   MOVE      EABUL1        XABUL1
     C                   MOVE      EACHK#        XACHK#
     C                   MOVE      EANINV        XANINV
     C                   MOVE      EALATE        XALATE
     C                   MOVE      EAFL01        XAFL01
     C                   MOVE      EAFL02        XAFL02
     C                   MOVE      EADT01        XADT01
     C                   MOVE      EADT02        XADT02
     C                   MOVE      EACCTP        XACCTP
     C                   MOVE      EACNUM        XACNUM
     C                   MOVE      EACEXP        XACEXP
     C                   MOVE      EACNAM        XACNAM
          //*** Beg Add ***** 06/29/22 ***************************
     C                   MOVE      EAref#        XAref#
          //*** End Add ***** 06/29/22 ***************************
     C*
     C*              Maintenance fields
     C                   MOVE      Q1USER        XAIUSR
     C                   MOVE      Q1PGM         XAIPGM
     C                   EVAL      XAIDT = DateTo7(SystemDate())
     C                   EVAL      XAITM = TimeTo6(SystemTime())
     C*
     C                   WRITE     PXA01
     C*
     C                   ENDSR                                                  SRWRIT
      *=========================================================================
      * SRRNEW - Process Renewal Years
      *=========================================================================
     C     SRRNEW        BEGSR
      *
     C                   Z-ADD     XXFYR         SVFYR             3 0
     C     ELKY          SETGT     PEL00
     C     ELKY1         READE     PEL00                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *** Begin Add *** 09/14/22 WC2880 ************************************
     C                   EVAL      xxFYR = ELfyr
      *** End   Add *** 09/14/22 WC2880 ************************************
      *
      *    Get Agent info
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM                    #1AGT#            5 0
     C                   PARM                    #1ANAM           60
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
      *
     C                   Z-Add     0             XXSYDT            7 0
     C                   Eval      xxSyDT = DateTo7(SystemDate())
      *
      *  Write Cancellation Detail and History records
      *  Get the next Request#
     C                   MOVE      *HIVAL        XXREQ#            5 0
      *** Begin Add *** 09/14/22 WC2880 *******************
     C                   EXSR      SRREIN
      *** End   Add *** 09/14/22 WC2880 *******************
     C     CXKY1         SETGT     PCX00
      *** Begin Chg *** 09/14/22 WC2880 *******************
     C**** CXKY          READPE    PCX00                                  60
     C     CXKY          READPE(N) PCX00                                  60
      *** End   Chg *** 09/14/22 WC2880 *******************
     C  N60CXREQ#        ADD       1             XXREQ#
     C   60              Z-ADD     1             XXREQ#
      *
      *  Load WDCXP fields
     C                   MOVE      ELCO#         CXCO#
     C                   MOVE      ELFND         CXFND
     C                   MOVE      ELEMP#        CXEMP#
     C                   MOVE      ELDIV         CXDIV
     C                   MOVE      ELFYR         CXFYR
     C                   MOVE      XXREQ#        CXREQ#
     C                   Z-ADD     ELPFDT        CXCXDT
     C                   Z-ADD     XXSYDT        CXCDAT
     C                   Z-ADD     #1AGT#        CXAGT#
     C                   MOVE      *BLANKS       CXRSN
     C                   MOVE      *BLANKS       CXCOM1
     C                   MOVE      *BLANKS       CXCOM2
     C                   MOVE      *ZEROS        CXAMTD
     C                   MOVE      'N'           CXCCRT
     C                   MOVE      *BLANKS       CXCDRT
     C                   MOVE      'N'           CXREIN
     C                   MOVE      'N'           CXRCRT
     C                   MOVE      *ZEROS        CXREDT
     C                   MOVE      *ZEROS        CXRDAT
     C                   MOVE      *BLANKS       CXRDRT
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      *BLANK        CXREVL
     C                   MOVE      'Y'           CXCFFL
     C                   Z-ADD     XXSYDT        CXCFDT
     C                   MOVE      *BLANK        CXCEMN
     C                   MOVE      *ZEROS        CXDOI#
     C                   MOVE      *ZEROS        CXCNC#
     C                   MOVE      *BLANK        CXERR
     C                   MOVE      *ZEROS        CXRFEE
     C                   MOVE      *BLANKS       CXRFDS
     C                   MOVE      *BLANKS       CXCAP#
     C                   MOVE      *ZEROS        CXRPMY
     C                   Z-ADD     XXSYDT        CXCFPD
     C                   MOVE      *BLANKS       CXCARR
     C                   MOVE      'Y'           CXENFL
      *
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
     C*** begin add ***  12/13/24  *****************************************
     C                   Clear                   CXPdpr
     C                   If        not IsPEOImport
     C*** end add *****  12/13/24  *****************************************
                         CXpdPr = 'Y';
     C*** begin add ***  12/13/24  *****************************************
     C                   Endif
     C*** end add *****  12/13/24  *****************************************
      *** End Add *** 02/26/24 *** WC2956 **********************************
      *
     C                   EVAL      CXDT = DateTo7(SystemDate())
     C                   EVAL      CXTIME = TimeTo6(SystemTime())
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
      *
     C                   MOVE      Q1USER        CXIUSR
     C                   MOVE      Q1PGM         CXIPGM
     C                   EVAL      CXITM = TimeTo6(SystemTime())
     C                   Z-ADD     CXDT          CXIDT
     C                   MOVE      'Y'           CXAFLT
      *
      *  Load WDXHP fields
     C                   MOVE      ELCO#         XHCO#
     C                   MOVE      ELFND         XHFND
     C                   MOVE      ELEMP#        XHEMP#
     C                   MOVE      ELDIV         XHDIV
     C                   MOVE      ELFYR         XHFYR
     C                   MOVE      XXREQ#        XHREQ#
     C                   Z-ADD     ELPFDT        XHCXDT
     C                   Z-ADD     XXSYDT        XHTRDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   MOVE      *BLANKS       XHRSN
     C                   MOVE      *ZEROS        XHREDT
     C                   MOVE      *ZEROS        XHRDAT
     C                   MOVE      *ZEROS        XHCNC#
     C                   MOVE      *BLANK        XHERR
     C                   MOVE      'Y'           XHENFL
     C                   Z-add     #1Agt#        XHagt#                         Add 08/07/19
     C                   Z-ADD     XXSYDT        XHDT
     C                   EVAL      XHCDAT= DateTo7(SystemDate())
     C                   EVAL      XHTIME= TimeTo6(SystemTime())
      *
     C                   MOVE      *BLANK        XHSTAT
     C                   MOVEL     Q1USER        XHUSER
     C                   MOVEL     Q1PGM         XHPGM
      *
      *  Write WDCXP and WDXHP records
     C                   WRITE     PCX0P
     C                   WRITE     PXH0P
     C*** begin chg ***  01/28/22  *************************************
     C*********          MOVE      *BLANKS       ELARSM
     C                   If        cltID <> '150'
     C                   Eval      Elarsm = *Blanks
     C                   Endif
     C*** end chg *****  01/28/22  *************************************
     C                   UPDATE    PEL00
      *
      *  Delete billings which are posted on or after the effective
      *  date of cancellation (CXCXDT) if admin/DARTS flag = 'Y'
     C     EXDLAR        IFEQ      'Y'
     C                   EXSR      SRDBIL
     C                   ENDIF
      *
     C     ELKY1         READE     PEL00                                  60
     C                   ENDDO                                                  ELKY
      *** Begin Add *** 09/14/22 WC2880 ***********************
     C                   EVAL      xxFYR = svFYR
      *** End   Add *** 09/14/22 WC2880 ***********************
      *
     C                   ENDSR                                                  SRRNEW
      *** Begin Add *** 09/14/22 WC2880 ***********************
      *=========================================================================
      * SRREIN - Update Existing CX Detail Records
      *=========================================================================
     C     SRREIN        BEGSR
       Setgt CXKY1 pcx00;
       Readpe CXKY pcx00;
          If not %eof(WDCXL) and CXrein <> 'Y';
             CXredt = CXcxdt;
             CXrein = 'Y';
             CXrdat = DateTo7(SystemDate());
             CXuser = Q1user;
             CXpgm = Q1pgm;
             CXdt = CXrdat;
             CXtime = TimeTo6(SystemTime());
             Update pcx00;
             Exsr WRTXH;
          Endif;
     C                   ENDSR                                                  SRRNEW
      *** End   Add *** 09/14/22 WC2880 ****************************
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
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     C*================================================================
     C*  SRPRPR - Calculate Prorated Premium based on Policy Coverage
     C*           (Start Date - Cancel Date)
     C*================================================================
        BegSr SRPRPR;
          // Get Policy Period in Days
          PPDays = 365;

          // Get Policy Covered in Days (From Policy Start Date till Cancellation)
          W@FrDt = ELPFDT;
          W@ToDt = CXCXDT;
          PCDays = %Diff (%Date(%Char(W@ToDt):*CYMD0) :
                          %Date(%Char(W@FrDt):*CYMD0) : *D);

          // Calculate Prorate Factor
          PRFact = %DecH(PCDays/PPDays:5:3);

          // Apply Prorate Factor to Premiums

          If ELFAF <> ' ' and ELARSM = 'Y';
             If ELPROR < 1 and ELPROR > 0;
                ELANPR = %DecH(ELANPR/ELPROR:9:0);              // Est Annual Pre
                EL1AAM = %DecH(EL1AAM/ELPROR:9:0);              // 1st Assessment
                EL2AAM = %DecH(EL2AAM/ELPROR:9:0);              // 2nd Assessment
                EL3AAM = %DecH(EL3AAM/ELPROR:9:0);              // 3rd Assessment
                EL4AAM = %DecH(EL4AAM/ELPROR:9:0);              // 4th Assessment
                EL5AAM = %DecH(EL5AAM/ELPROR:9:0);              // 5th Assessment
                EL6AAM = %DecH(EL6AAM/ELPROR:9:0);              // 6th Assessment
                EL7AAM = %DecH(EL7AAM/ELPROR:9:0);              // 7th Assessment
                ELTXDU = %DecH(ELTXDU/ELPROR:9:0);              // State Tax
             EndIf;
             ELANPR = %DecH(ELANPR*PRFact:9:0);                 // Est Annual Pre
             EL1AAM = %DecH(EL1AAM*PRFact:9:0);                 // 1st Assessment
             EL2AAM = %DecH(EL2AAM*PRFact:9:0);                 // 2nd Assessment
             EL3AAM = %DecH(EL3AAM*PRFact:9:0);                 // 3rd Assessment
             EL4AAM = %DecH(EL4AAM*PRFact:9:0);                 // 4th Assessment
             EL5AAM = %DecH(EL5AAM*PRFact:9:0);                 // 5th Assessment
             EL6AAM = %DecH(EL6AAM*PRFact:9:0);                 // 6th Assessment
             EL7AAM = %DecH(EL7AAM*PRFact:9:0);                 // 7th Assessment
             ELTXDU = %DecH(ELTXDU*PRFact:9:0);                 // State Tax

             // Get Safety Fee Prorate Flag
             Chain (XXCO#:XXFND:XXST) WDRRL;
             If %Found and RRSPFL = 'Y' and RREFDT <= ELPFDT;
                If ELPROR < 1 and ELPROR > 0;
                   ELSFAM = %DecH(ELSFAM/ELPROR:9:0);               // Safety Fee
                EndIf;
                ELSFAM = %DecH(ELSFAM*PRFact:9:0);               // Safety Fee
             EndIf;

             // Get Membership Fee Prorate Flag
             Chain (XXCO#:XXFND:XXFYR) WDF1L;
             If %Found and F1PMFL = 'Y';
                If ELPROR < 1 and ELPROR > 0;
                   ELMFAM = %DecH(ELMFAM/ELPROR:9:0);               // Safety Fee
                EndIf;
                ELMFAM = %DecH(ELMFAM*PRFact:9:0);              // Member Fee
             EndIf;

          EndIf;

        EndSr;
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
