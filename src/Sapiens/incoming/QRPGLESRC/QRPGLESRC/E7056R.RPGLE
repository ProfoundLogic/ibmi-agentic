     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7056R - EMP EXP MOD - CALCULATE/BUILD WORK FILE FOR
     H**              EXPERIENCE MODIFIER
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM CALCULATES AND BUILDS WORK FILE FOR        **
     H**     EXPERIENCE MODIFIER                                     **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7055C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    02/18/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/28/25 C:291 Support: 118943                                *
     H*                Fix Expected Loss Rate Calc done under WC2952. *
     H*                (Revision to original changes from 06/19/24)   *
     H*                                                               *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Exapand Expected Loss Rate to 8,3.             *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 07/22/10  C:246  SUPPORT:73562  REROFIT THE WC1502 GERT FTR  **
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 09/17/06  C:03   PROJECT: WC1502
     H*                  ADD GERT FACTOR
     H*
     H*                                                               *
     H* 10/27/04  C:03   PROJECT: 129WEU  SUPP44336
     H*                  CORRECT FILED POPULATION W/MA OR NY FOR
     H*                  MED 70% REDUCTION                            *
     H*
     H*                                                               *
     H* 08/28/04  C:03   PROJECT: 129WEU
     H*                  ADD CHANGES FOR MA ARAP                      *
     H*                  RMV 70% REDUCTION FOR NY & MA
     H*                                                               *
     H* 04/08/04  C:03   PROJECT: SUPP 29702
     H*                  CHG WORK FIELD FOR RATE FROM 5/4 TO 7/4      *
     H*                  TO HANDLE 7/0 RATE FIELD.
     H*                                                               *
     H* 09/23/01  C:03   PROJECT: SUPP EX071
     H*                  CALC FOR MIN 2 YR NOT CORRECT DUE TO YR INFO *
     H*                  IN THE XXFRDT AND XXTODT DATA STRUCTURES
     H*                                                               *
     H* 09/06/01  C:03   PROJECT: SUPP EX093 #28229                   *
     H*                  CORRECT TTL ON EXCESS CLM AMT                *
     H*                                                               *
     H* 08/25/01  C:03   PROJECT: WC853                               *
     H*                  CORRECT LOAD OF "B" REC EFDT.                *
     H*                                                               *
     H* 10/13/98  C:03   PROJECT: WC853                               *
     H*                  ADD CHNAGES FOR EXCESS MED CLAIMS            *
     H*                                                               *
     H* 10/10/98  C:58   PROJECT: WC853                               *
     H*                  Apply 30% reduction for excess loss          *
     H*                  if effective date > 01/01/98.                *
     H*                                                               *
     H* 09/02/98  C:90   PROJECT: WC853                               *
     H*                  MODIFIED TO VERSION 3 STANDARDS              *
     H*                                                               *
     H* 08/25/98  C:35   PROJECT: WC853                               *
     H*                  APPLY 30% REDUCTIION FOR NON-EXCESS LOSSES   *
     H*                  IF EFFECTIVE DATE > 01/01/98.                *
     H*                                                               *
     H* 08/12/98  C:90   PROJECT: WC853                               *
     H*                  MODIFIED TO VERSION 3 STANDARDS              *
     H*                                                               *
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO#                                       **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMACL1(PAC01): CO#/FUND                                  **
     H**    EXXTL (PXX01): CO/FND/EMP#/EFDT                          **
     H**    EMEML (PEM01): CO/FUND/EMP#                              **
     H**    EMEBL (PEB01): CO/FUND/EMP#/BATCH #                      **
     H**    EMEEL (PEE01): CO/FUND/EMP#/BATCH/ACC DT/CASE #          **
     H**    EMEXL4(PEX01): CO/ST/EFDT/CLAS                           **
     H**    EMMRL (PMR01): CO/RTCD/CLAS/EFDT                         **
     H**    EMEPL (PEP01): CO/FND/EMP#/BATCH#/CLAS                   **
     H**    EMWBL1(PWB01): CO/LOSS- SELECT STAT <> I (REPLACED)      **
     H**    EMWEL3(PWE01): CO/ST/EFDT/LOSS  SEL STAT <> I (WEIGHTING)**
     H**    EMBAL4(PBA01): CO/ST/EFDT/LOSS  SEL STAT <> I (BALLAST)  **
     H**    EMFNL (PFN01): CO/FND                                    **
     H**    SMORL (POR01): CO/PGM/PRT                                **
     H**    QSYSPRT                                                  **
     H****** 09/17/06  BGN ADD  *********
     H**    EMGTL3(PGT01): CO/ST/GERT       SEL STAT <> I (GERT)  **
     H****** 09/17/06  END ADD  *********
     H*****************************************************************
     H*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* Company Auxiliary Master File
     FEMACL1    IF   E           K DISK
     F*
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK
     F*
     F* Exp Mod Work File
     FEXXTL     UF A E           K DISK
     F*
     F* Employer Master Maintenance
     FEMEML     IF   E           K DISK
     F*
     F* Employer Batch Maintenance
     FEMEBL     IF   E           K DISK
     F*
     F* Excess/Non-Excess Claims Maintenance
     FEMEEL     IF   E           K DISK
     F*
     F* Expected Loss & Discount Ratio Master File
     FEMEXL6    IF   E           K DISK
     F*
     F* Manual Rate Master File
     FEMMRL2    IF   E           K DISK
     F*
     F* Employer Payroll Master File
     FEMEPL     IF   E           K DISK
     F*
     F* Weighing Table & Ballast Table Master File
     FEMWEL3    IF   E           K DISK
     FEMBAL4    IF   E           K DISK
     F*
     F* Fund Master File
     FEMFNL     IF   E           K DISK
     F*
     F* Output Routing File
     FSMORL     IF   E           K DISK
     F*
     F* Printer Output File
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOA)
     F*
     F****** 09/17/06  BGN ADD  *********
     F* GERT FACTOR Master File
     FEMGTL3    IF   E           K DISK
     F****** 09/17/06  END ADD  *********

     D*
     D* Table for Batch Years to be Processed
     D BT              S              3  0 DIM(9)
     D*
     D* Table for Batch Years to be Processed
     D YR              S              3  0 DIM(9) DESCEND
     D*
     D* Table for Manual Payroll by Batch
     D BP              S             11  0 DIM(9)
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* From Date Data Structure
     D                 DS
     D  XXFRDT                 1      7  0
     D*************** DEL 09/23/01 ********** 2   30XXFRYY
     D  XXFRYY                 1      3  0
     D  XXFRMN                 4      5  0
     D  XXFRDY                 6      7  0
     D*
     D* To Date Data Structure
     D                 DS
     D  XXTODT                 1      7  0
     D*************** DEL 09/23/01 ********** 2   30XXTOYR
     D  XXTOYR                 1      3  0
     D  XXTOMN                 4      5  0
     D  XXTODY                 6      7  0
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      *** Beg Add ***** 06/19/24 **********************************
      *** Beg Chg ***** 04/28/25 **********************************
     D*** xxLsRt          S              8  3
     D*** x2LsRt          S              8  3
     D xxLsRt          S              8  5
     D x2LsRt          S              8  5
      *** End Chg ***** 04/28/25 **********************************
      *** End Add ***** 06/19/24 **********************************

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EEYEAR        XXYEAR
     C     *LIKE         DEFINE    EEACDT        XXACDT
     C     *LIKE         DEFINE    EECASE        XXCASE
     C     *LIKE         DEFINE    XEFDT         XXEFDT
     C     *LIKE         DEFINE    XCLS          XXCLAS
     C     *LIKE         DEFINE    WEEFDT        X2EFDT
     C     *LIKE         DEFINE    WELOSS        XXLOSS
     C     *LIKE         DEFINE    XACDT         XXLODT
     C     *LIKE         DEFINE    XACDT         XXHIDT
     C     *LIKE         DEFINE    XYEAR         XXHIBT
     C     *LIKE         DEFINE    XYEAR         XXHIB1
     C     *LIKE         DEFINE    XTDRSC        XXDRSC
     C     *LIKE         DEFINE    XTST          XXST
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Company Auxiliary Master File
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the Experience Modifier Work File
     C*
     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXEFDT
     C*
     C     XXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     XXKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* Key List for the Employer Batch Master File
     C*
     C     EBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C*
     C* Key List for the Excess/Non-Excess Claims File
     C*
     C     EEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C*
     C     EEKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C                   KFLD                    XXACDT
     C                   KFLD                    XXCASE
     C*
     C* Key List for the Expected Loss & Discount Ratio Master File
     C*
     C     EXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXCLAS
     C                   KFLD                    X2EFDT
      *
      *
     C     EXKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXCLAS
      *
     C*
     C* Key List for the Manual Rate Master File
     C*
     C     MRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDRSC
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C* Key List for the Employer Payroll Master File
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C                   KFLD                    XXCLAS
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C*
     C* Key List for the Weighing & Ballast Value Master File
     C*
     C     WEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C                   KFLD                    XXLOSS
     C*
     C     BAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C                   KFLD                    XXLOSS
     C*
     C     BAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C*
     C****** 09/17/06  BGN ADD  *********
     C     GTKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C****** 09/17/06  END ADD  *********
     C*
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C**** BEGIN CHG **** 09/02/98 ************************************
     C******************** PARM           QQFND   30
     C******************** PARM           QQEMP#  90
     C******************** PARM           QQBAT1  30
     C******************** PARM           QQBAT2  30
     C******************** PARM           QQBAT3  30
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQBAT1            3
     C                   PARM                    QQBAT2            3
     C                   PARM                    QQBAT3            3
     C******************** PARM           QQBAT4  30
     C******************** PARM           QQBAT5  30
     C******************** PARM           QQBAT6  30
     C******************** PARM           QQBAT7  30
     C******************** PARM           QQBAT8  30
     C******************** PARM           QQBAT9  30
     C******************** PARM           QQEFDT  80        mdy
     C******************** PARM           QQRNDT  80        mdy
     C                   PARM                    QQBAT4            3
     C                   PARM                    QQBAT5            3
     C                   PARM                    QQBAT6            3
     C                   PARM                    QQBAT7            3
     C                   PARM                    QQBAT8            3
     C                   PARM                    QQBAT9            3
     C                   PARM                    QQEFDT            8             mdy
     C                   PARM                    QQRNDT            8             mdy
     C**** BEGIN CHG **** 09/02/98 ************************************
     C                   PARM                    QQQUE            10
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Initialize and Set-up
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   Z-ADD     0             BP
     C                   MOVE      QQBAT1        BT(1)
     C                   MOVE      QQBAT2        BT(2)
     C                   MOVE      QQBAT3        BT(3)
     C                   MOVE      QQBAT4        BT(4)
     C                   MOVE      QQBAT5        BT(5)
     C                   MOVE      QQBAT6        BT(6)
     C                   MOVE      QQBAT7        BT(7)
     C                   MOVE      QQBAT8        BT(8)
     C                   MOVE      QQBAT9        BT(9)
     C                   MOVE      QQRNDT        X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXRNDT            6 0
     C*
     C                   MOVE      QQEFDT        X3EFDT            8 0
     C*
     C                   Z-ADD     X3EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2EFDT
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Retrieve the Company Auxiliary Record
     C*
     C                   Z-ADD     0             XXFND
     C                   MOVE      QQFND         XXFND
     C     ACKEY         CHAIN     PAC01                              60
     C*
     C* Adjust multiple claim factor from company aux file
     C*
     C     ACMCF         DIV       100           XXMCFT            3 2
     C*
     C* Retrieve the Fund Record
     C*
     C                   Z-ADD     0             XXFND
     C                   MOVE      QQFND         XXFND
     C     FNKEY         CHAIN     PFN01                              55
     C     *IN55         IFEQ      '1'
     C                   MOVE      *BLANKS       FNNAME
     C                   END
     C                   MOVE      FNMST         XXST
     C*
     C* Print heading for exception report for employers that do not
     C* qualify for exp mod
     C*
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C                   MOVE      *BLANKS       XXTITL           56
     C                   MOVEL     ORTITL        XXTITL
     C                   TIME                    DSDTTM
     C*
      ***** Beg Del ***** 08/26/14 *******************************************
     C*************      EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *******************************************
     C                   EXCEPT    HEAD
     C*
     C* MAIN LOOP
     C*
     C* Use employer file as the driver file; if user has selected an
     C* employer read file based on the employer; otherwise read
     C* employer file by fund.
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C*
     C     XXEMP#        IFEQ      0
     C     EMKEY         CHAIN     PEM01                              50
     C                   ELSE
     C     EMKY1         CHAIN     PEM01                              50
     C                   END
     C*
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* Delete old work file records for this emp#, this effect date
     C                   EXSR      DELWK
     C                   EXSR      CLR
     C*
     C* get the loss information for this employer
     C                   EXSR      LOSS
     C                   EXSR      CLR
     C*
     C***** BGN DEL 09/06/01
     C********** ZTAAL     SUB  ZTPAL     ZTAEL
     C***** END DEL 09/06/01
     C*
     C* get the payroll information for this employer
     C                   EXSR      PREM
     C                   EXSR      CLR
     C*
     C* get the exp mod for this employer
     C                   EXSR      MOD
     C                   EXSR      CLR
     C*
     C* clear work fields used for totals
     C                   EXSR      TTL0
     C                   EXSR      CLR
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C     XXEMP#        IFEQ      0
     C     EMKEY         READE     PEM01                                  50
     C                   ELSE
     C     EMKY1         READE     PEM01                                  50
     C                   END
     C*
     C                   END
     C*
     C* End the Program
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C****************************************************************
     C*  DELWK - subroutine to delete old existing work file records
     C****************************************************************
     C*
     C* This subroutine will delete records fromthe work file for
     C* fund, emp#, eff date
     C*
     C     DELWK         BEGSR
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   MOVE      QQEFDT        ZZEFDT            8 0
     C*
     C                   Z-ADD     ZZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     XXKEY         CHAIN     PXX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXX01
     C     XXKEY         READE     PXX01                                  60
     C                   END
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*  LOSS -  subroutine to obtain loss information
     C****************************************************************
     C*
     C* This subroutine will calculate loss information for the emp#
     C*
     C     LOSS          BEGSR
     C*
     C***** BGN ADD 09/06/01 *********************************
     C                   Z-ADD     0             ZTAEL
     C***** END ADD 09/06/01 *********************************
     C                   Z-ADD     1             X                 3 0
     C                   DO        9             X
     C     BT(X)         IFNE      0
     C*
     C* get the excess/non-excess records for this employer/batch
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     BT(X)         XXYEAR
     C     EEKEY         CHAIN     PEE01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       XXOVR1            1
     C                   MOVE      *BLANKS       XXOVR2            1
     C* non-excess losses
     C     EEACDT        IFEQ      0
     C                   Z-ADD     EEAMT         XAAMT
     C                   Z-ADD     0             XPRAMT
     C                   Z-ADD     0             XAEAMT
     C* add to total primary losses
      **** BEGIN CHG **** 08/25/98 *********************************************
     C********** ZTPAL     ADD  EEAMT     ZTPAL
     C********** ZTAAL     ADD  EEAMT     ZTAAL
      *
     C**** BEGIN CHG **** 09/02/98 ************************************
     C********** QQEFDT    IFGE 980101
     C     XXEFDT        IFGE      980101
     C**** END   CHG **** 09/02/98 ************************************
      *
     C     EEMULT        IFEQ      '6'
     C***  BGN DEL 10/27/04 *****************************
     C***  BGN CHG 08/28/04 ******************************
     C********** ACST      ANDNE'MA'      DEL 10/27/04
     C********** ACST      ANDNE'NY'      DEL 10/2704
     C***  END CHG 08/28/04 ******************************
     C***  END DEL 10/27/04 *****************************
      *
     C**** BGN CHG **** 10/13/98 ************************************
     C                   MOVE      '6'           EESTAT
     C**** END CHG **** 10/13/98 ************************************
     C***  BGN CHG 10/27/04 ******************************
     C     ACST          IFNE      'MA'
     C     ACST          ANDNE     'NY'
     C***  BGN CHG 08/28/04 ******************************
     C     EEAMT         MULT(H)   .3            ZZAMT             9 0
      *
     C     EEAMT         IFGE      5000
     C     ZTAAL         ADD       ZZAMT         ZTAAL
     C***** BGN CHG 11/05/99 *********************************
     C*********  ZTPAL     ADD  1500      ZTPAL
     C     ZTPAL         ADD       ZZAMT         ZTPAL
     C***** END CHG 11/05/99 *********************************
     C**** BGN CHG **** 10/13/98 ************************************
     C                   Z-ADD     ZZAMT         XPRAMT
     C                   Z-ADD     1500          XAEAMT
     C**** END CHG **** 10/13/98 ************************************
     C***** BGN CHG 11/05/99 *********************************
     C                   Z-ADD     ZZAMT         XPRAMT
     C                   Z-ADD     0             XAEAMT
     C***** END CHG 11/05/99 *********************************
     C                   ELSE
     C     ZTAAL         ADD       ZZAMT         ZTAAL
     C     ZTPAL         ADD       ZZAMT         ZTPAL
     C**** BGN CHG **** 10/13/98 ************************************
     C                   Z-ADD     ZZAMT         XPRAMT
     C**** BGN CHG **** 09/06/01 ***********************************
     C*********DEL ****    Z-ADDZZAMT     XAEAMT *** DEL 09/06/01 ***
     C                   Z-ADD     0             XAEAMT
     C**** END CHG **** 09/06/01 ***********************************
     C**** END CHG **** 10/13/98 ************************************
     C                   ENDIF                                                  EEAMT IFGE 5000
     C**** BGN CHG **** 10/13/98 ************************************
     C                   Z-ADD     ZZAMT         XAAMT
     C**** END CHG **** 10/13/98 ************************************
      *
     C***  BGN CHG 10/27/04 ******************************
     C                   ELSE
     C     ZTAAL         ADD       EEAMT         ZTAAL
     C     ZTPAL         ADD       EEAMT         ZTPAL
     C                   Z-ADD     EEAMT         XPRAMT
     C                   Z-ADD     EEAMT         XAEAMT
     C                   Z-ADD     EEAMT         XPRAMT
     C                   Z-ADD     0             XAEAMT
     C                   END
     C***  END CHG 10/27/04 ******************************
      *
     C                   ELSE
     C     ZTAAL         ADD       EEAMT         ZTAAL
     C     ZTPAL         ADD       EEAMT         ZTPAL
     C**** BGN CHG **** 10/13/98 ************************************
     C                   Z-ADD     EEAMT         XPRAMT
     C                   Z-ADD     EEAMT         XAEAMT
     C**** END CHG **** 10/13/98 ************************************
     C**** BGN CHG **** 11/05/99 ************************************
     C                   Z-ADD     EEAMT         XPRAMT
     C                   Z-ADD     0             XAEAMT
     C**** END CHG **** 11/05/99 ************************************
     C                   ENDIF                                                  EEMULT IFEQ '6'
      *
     C                   ELSE
     C     ZTAAL         ADD       EEAMT         ZTAAL
     C     ZTPAL         ADD       EEAMT         ZTPAL
     C**** BGN CHG **** 10/13/98 ************************************
     C                   Z-ADD     EEAMT         XPRAMT
     C                   Z-ADD     EEAMT         XAEAMT
     C**** END CHG **** 10/13/98 ************************************
     C                   ENDIF                                                  QQEFDT IFGE 980101
      **** END   CHG **** 08/25/98 *********************************************
     C*
     C                   ELSE                                                   Acc dt else
     C*
     C* excess losses
     C                   Z-ADD     ACEXLS        X1EXLS
     C                   Z-ADD     ACPRLS        X1PRLS
     C     EEMULT        IFEQ      'Y'
     C                   Z-ADD     0             X1EXLS
     C                   Z-ADD     0             X1PRLS
     C     ACEXLS        MULT(H)   XXMCFT        X1EXLS            9 0
     C     ACPRLS        MULT(H)   XXMCFT        X1PRLS            9 0
     C                   END                                                    mult clm Y/N
     C                   Z-ADD     EEAMT         XXAMT            11 0
     C                   Z-ADD     0             X1AMT            11 0
     C                   Z-ADD     0             X2AMT            11 0
     C**** BEGIN ADD **** 10/10/98 ************************************
     C*
     C*    Calculate 30% reduction for excess loss
     C     XXEFDT        IFGE      980101
     C     EEMULT        ANDEQ     '6'
     C***  BGN CHG 08/28/04 ******************************
     C     ACST          ANDNE     'MA'
     C     ACST          ANDNE     'NY'
     C***  END CHG 08/28/04 ******************************
     C                   Z-ADD     X1PRLS        XXPRAM            9 0
     C     XXAMT         IFLE      X1PRLS
     C                   Z-ADD     XXAMT         XXPRAM
     C                   ENDIF                                                  .AMT < 1PRLS
     C*
     C                   MULT(H)   .3            XXPRAM
     C     XXAMT         SUB       X1PRLS        XXAEAM
     C**** BGN CHG **** 10/13/98 ************************************
     C*****************    Z-ADDXXAMT     XAAMT
     C     XXAMT         MULT(H)   .3            XAAMT
     C**** END CHG **** 10/13/98 ************************************
     C**** BGN CHG **** 11/05/98 ************************************
     C                   Z-ADD     XXAMT         XAAMT
     C**** END CHG **** 11/05/98 ************************************
     C                   Z-ADD     XXPRAM        XPRAMT
     C*
     C                   MULT(H)   .3            XXAEAM
     C                   Z-ADD     XXAEAM        XAEAMT
     C**** BGN CHG **** 10/13/98 ************************************
     C******************   ADD  XXPRAM    ZTPAL
     C******************   ADD  XXAMT     ZTAAL
     C                   ADD       XPRAMT        ZTPAL
     C                   ADD       XAAMT         ZTAAL
     C**** END CHG **** 10/13/98 ************************************
     C*
     C                   ELSE
     C**** END ADD ****** 10/10/98 ************************************
     C* PRIMARY LOSS IS A CONSTANT FIGUR; NO LONGER CALCULATED
     C*
     C                   Z-ADD     X1PRLS        XXPRAM            9 0
     C     XXAMT         IFLE      XXPRAM
     C                   Z-ADD     XXAMT         XXPRAM
     C                   END
     C     XXAMT         IFGT      X1EXLS
     C                   Z-ADD     X1EXLS        XXAMT
     C                   MOVE      '*'           XXOVR1
     C                   END
     C                   Z-ADD     0             XXAEAM
     C     XXAMT         SUB       XXPRAM        XXAEAM            9 0
     C* total excess losses
     C                   Z-ADD     XXAMT         XAAMT
     C                   Z-ADD     XXPRAM        XPRAMT
     C                   Z-ADD     XXAEAM        XAEAMT
     C* add to total
     C     ZTPAL         ADD       XXPRAM        ZTPAL
     C     ZTAAL         ADD       XXAMT         ZTAAL
     C*
     C**** BEGIN ADD **** 10/10/98 ************************************
     C                   ENDIF                                                  .EFDT>=980101
     C**** END ADD ****** 10/10/98 ************************************
     C                   END                                                    Acc dt = 0
     C*
     C* write the work record
     C*
     C***** BGN ADD 09/06/01 *********************************
     C     ZTAEL         ADD       XAEAMT        ZTAEL
     C***** END ADD 09/06/01 *********************************
     C                   Z-ADD     EETLCA        XNCLMS
     C                   Z-ADD     XXCO#         XCO#
     C                   Z-ADD     EEFND         XFUND
     C                   Z-ADD     EEEMP#        XEMP#
     C                   Z-ADD     EEYEAR        XYEAR
     C                   Z-ADD     EEACDT        XACDT
     C                   MOVEL     EECASE        XCASE
     C                   MOVE      EESTAT        XOFF
     C                   MOVE      QQEFDT        ZZEFDT
     C*
     C                   Z-ADD     ZZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XEFDT
     C*
     C                   MOVE      'A'           XREC
     C                   MOVE      XXOVR1        XOVR1
     C                   MOVE      XXOVR2        XOVR2
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XDT
     C*
     C                   Z-ADD     DSTIME        XTIME
     C                   MOVE      Q1USER        XUSER
     C                   MOVE      Q1PGM         XPGM
     C                   MOVE      Q1USER        XIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XIDT
     C*
     C                   Z-ADD     0             XFRDT
     C                   Z-ADD     0             XTODT
     C                   WRITE     PXX01
     C                   EXSR      CLR
     C*
     C* Get the next excess/non-excess record
     C*
     C     EEKEY         READE     PEE01                                  60
     C                   END                                                     *IN60 END
     C                   END                                                     BT,X = ' '
     C                   END                                                     ENDDO
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  PREM -  subroutine to obtain Payroll information
     C****************************************************************
     C*
     C* This subroutine will calculate loss information for the emp#
     C*
     C     PREM          BEGSR
     C*
     C* TOTAL UP EXPECTED PRIMARY LOSSES
     C                   Z-ADD     0             XXEPLS            9 0
     C*
     C                   DO        9             X
     C     BT(X)         IFNE      0
     C*
     C* get the payroll records for this employer
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     BT(X)         XXYEAR
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      EPDRSC        XXDRSC
     C* get expect loss/discount record
     C                   Z-ADD     EPCLAS        XXCLAS
      *
     C     EXKEY         SETGT     PEX01
     C     EXKEY2        READPE    PEX01                                  61
      *
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     0             EXLSRT
     C                   Z-ADD     0             EXDSRT
     C                   END
     C* convert excess loss rate & discount rate
      *** Beg Chg ***** 06/19/24 **************************************
     C***  EXLSRT        DIV(H)    10000         XXLSRT            5 4
      *** Beg Chg ***** 04/28/25 **************************************
       //***xxLsRt = EXLsRt;
       xxLsRt = EXLsRt/100;
      *** End Chg ***** 04/28/25 **************************************
      *** End Chg ***** 06/19/24 **************************************
     C     EXDSRT        DIV(H)    100           XXDSRT            3 2
     C*
     C* take payroll amt and multiple time the expected loss rate
     C* to find the expected losses, take this times the "D" ratio
     C* to find the primary amt
     C*
     C     XXLSRT        MULT(H)   EPAMT         XXEXLS            9 0
     C*
     C     EPCLAS        IFEQ      908
     C     EPCLAS        OREQ      909
     C     EPCLAS        OREQ      912
     C     EPCLAS        OREQ      913
     C                   Z-ADD     0             X2LSRT
      *** Beg Chg ***** 06/19/24 **************************************
     C***  EXLSRT        DIV(H)    100           X2LSRT            5 2
       x2LsRt = EXLsRt;
      *** End Chg ***** 06/19/24 **************************************
     C     X2LSRT        MULT(H)   EPAMT         XXEXLS
     C                   END
     C*
     C*
     C     XXDSRT        MULT(H)   XXEXLS        XXPRIM            9 0
     C* the expected excess is the excess loss less the primary loss
     C     XXEXLS        SUB       XXPRIM        XXEXEX            9 0
     C     XXEPLS        ADD       XXPRIM        XXEPLS            9 0
     C*
     C* neeD to get the manual payroll factor from table,
     C* get batch record from batch master file, and use the from
     C* date to get the manual rate
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     BT(X)         XXYEAR
     C     EBKEY         CHAIN     PEB01                              61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     0             EBFRDT
     C                   END
     C* we need to save the highest and lowest batch bates for the
     C* average minimum premium
     C     XXLODT        IFEQ      0
     C                   Z-ADD     EBFRDT        XXLODT
     C                   ELSE
     C     EBFRDT        IFLT      XXLODT
     C                   Z-ADD     EBFRDT        XXLODT
     C                   END
     C                   END
     C*
     C     XXHIDT        IFEQ      0
     C                   Z-ADD     EBTODT        XXHIDT
     C                   ELSE
     C     EBTODT        IFGT      XXHIDT
     C                   Z-ADD     EBTODT        XXHIDT
     C                   END
     C                   END
     C*
     C* get the manual rate record
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   Z-ADD     EPRDAT        XXEFDT
     C     EPRDAT        IFEQ      0
     C                   Z-ADD     EBFRDT        XXEFDT
     C                   Z-ADD     EBFRDT        EPRDAT
     C                   END
     C     MRKEY         SETGT     PMR01
     C                   READP     PMR01                                  61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     0             MRRATE
     C                   END
     C     MRCLAS        IFNE      EPCLAS
     C     MRDRSC        ORNE      XXDRSC
     C     MREFDT        ORGT      EPRDAT
     C                   Z-ADD     0             MRRATE
     C                   END
     C*
     C* correct value of manual rate, and find manual payroll
     C*
     C****BGN CHG 04/08/04
     C********** MRRATE    DIV  10000     XXRATE  54H
      *** Beg Chg *****04/28/25 *****************************
     C***  MRRATE        DIV(H)    10000         XXRATE            7 4
     C     MRRATE        DIV(H)    100           XXRATE            7 4
      *** End Chg *****04/28/25 *****************************
     C****BGN CHG 04/08/04
     C     EPAMT         MULT(H)   XXRATE        XXMPRM            9 0
     C* take manual payroll and accumulate by batch
     C     BP(X)         ADD       XXMPRM        BP(X)
     C     ZTEXEX        ADD       XXEXEX        ZTEXEX
     C     ZTEXLS        ADD       XXEXLS        ZTEXLS
     C* write the work record
     C                   Z-ADD     XXCO#         XCO#
     C                   Z-ADD     EMFND         XFUND
     C                   Z-ADD     EMEMP#        XEMP#
     C                   Z-ADD     EBYEAR        XYEAR
     C***** BGN ADD  08/25/01 ***********************************
     C******************** MOVE QQEFDT    XXEFDT           DLT 08/25/01
     C*
     C******************** Z-ADDXXEFDT    SRDTI            DLT 08/25/01
     C******************** CALL 'SRCVTDAT'DTCYMD           DLT 08/25/01
     C******************** MOVELSRDTO     XEFDT            DLT 08/25/01
     C*
     C                   MOVE      QQEFDT        ZZEFDT
     C*
     C                   Z-ADD     ZZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XEFDT
     C***** END ADD  08/25/01 ***********************************
     C                   MOVE      'B'           XREC
     C                   Z-ADD     EPCLAS        XCLS
     C                   Z-ADD     MRRATE        XMNRT
     C                   Z-ADD     EPAMT         XPREM
     C                   Z-ADD     EXLSRT        XELRT
     C                   Z-ADD     XXEXLS        XEXLS
     C                   Z-ADD     EXDSRT        XDRATO
     C                   Z-ADD     XXPRIM        XPRIM
     C                   Z-ADD     XXEXEX        XEXEX
     C                   Z-ADD     XXMPRM        XMPRM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XDT
     C*
     C                   Z-ADD     DSTIME        XTIME
     C                   MOVE      Q1USER        XUSER
     C                   MOVE      Q1PGM         XPGM
     C                   MOVE      Q1USER        XIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XIDT
     C*
     C                   MOVE      XXST          XTST
     C                   MOVE      XXDRSC        XTDRSC
     C                   Z-ADD     EPRDAT        XRDAT
     C                   WRITE     PXX01
     C                   EXSR      CLR
     C*
     C* get next payroll record
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     BT(X)         XXYEAR
     C     EPKY1         READE     PEP01                                  60
     C                   END                                                     *in60 end
     C*
     C* save the last two batch numbers
     C*
     C                   Z-ADD     XXHIBT        XXHIB1
     C                   Z-ADD     BT(X)         XXHIBT
     C*
     C                   END                                                     BT,X = ' '
     C                   END                                                     ENDDO
     C*
     C                   Z-ADD     0             YR
     C                   MOVEA     BT            YR
     C                   SORTA     YR
     C                   Z-ADD     0             XXHIB1
     C                   Z-ADD     0             XXHIBT
     C                   Z-ADD     YR(1)         XXHIBT                           TO DT YR
     C                   Z-ADD     YR(2)         XXHIB1                           FRM DT YR
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  CLR -  subroutine to clear work file fields
     C****************************************************************
     C*
     C* This subroutine will clear work file fields
     C*
     C     CLR           BEGSR
     C*
     C                   Z-ADD     0             XFUND
     C                   Z-ADD     0             XEMP#
     C                   Z-ADD     0             XEFDT
     C                   Z-ADD     0             XYEAR
     C                   MOVE      *BLANKS       XREC
     C                   Z-ADD     0             XACDT
     C                   MOVE      *BLANKS       XCASE
     C                   MOVE      *BLANKS       XOFF
     C                   Z-ADD     0             XAAMT
     C                   Z-ADD     0             XPRAMT
     C                   Z-ADD     0             XAEAMT
     C                   Z-ADD     0             XNCLMS
     C                   MOVE      *BLANKS       XOVR1
     C                   MOVE      *BLANKS       XOVR2
     C                   Z-ADD     0             XCLS
     C                   Z-ADD     0             XMNRT
     C                   Z-ADD     0             XPREM
     C                   Z-ADD     0             XELRT
     C                   Z-ADD     0             XEXLS
     C                   Z-ADD     0             XDRATO
     C                   Z-ADD     0             XPRIM
     C                   Z-ADD     0             XEXEX
     C                   Z-ADD     0             XMPRM
     C                   Z-ADD     0             XTPAL
     C                   Z-ADD     0             XTAAL
     C                   Z-ADD     0             XTAEL
     C                   Z-ADD     0             XTEXEX
     C                   Z-ADD     0             XTEXLS
     C                   Z-ADD     0             XTBALL
     C                   Z-ADD     0             XTAELW
     C                   Z-ADD     0             XTWVWV
     C                   Z-ADD     0             XTEXEW
     C                   Z-ADD     0             XTDENM
     C                   Z-ADD     0             XTNUM
     C                   Z-ADD     0             XTMOD
     C                   Z-ADD     0             XTERTO
     C                   Z-ADD     0             XTMODA
     C                   Z-ADD     0             XTM2YR
     C                   Z-ADD     0             XTMALY
     C                   Z-ADD     0             XTM2RT
     C                   Z-ADD     0             XTMALR
     C                   MOVE      *BLANK        XTOVR3
     C                   MOVE      *BLANKS       XUSER
     C                   MOVE      *BLANKS       XPGM
     C                   Z-ADD     0             XTIME
     C                   Z-ADD     0             XDT
     C                   MOVE      *BLANKS       XIUSR
     C                   Z-ADD     0             XIDT
     C                   Z-ADD     0             XRDAT
     C                   Z-ADD     0             XFRDT
     C                   Z-ADD     0             XTODT
     C                   Z-ADD     0             XTRVAL
     C                   Z-ADD     0             XTSURF
     C                   Z-ADD     0             XTEPLS
     C                   MOVE      *BLANK        XTST
     C                   MOVE      *BLANK        XTDRSC
     C*
     C****** 09/17/06  BGN ADD  *********
     C                   Z-ADD     0             XTGERT
     C                   Z-ADD     0             XTGMOD
     C                   Z-ADD     0             XTAGT#
     C                   MOVE      *BLANKS       XTAGNM
     C                   Z-ADD     0             XTPROD
     C                   MOVE      *BLANKS       XTPRNM
     C****** 09/17/06  END ADD  *********
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  MOD -  subroutine to calculate modifier
     C****************************************************************
     C*
     C* This subroutine will calculate experience mod
     C*
     C     MOD           BEGSR
     C*
     C* take the expexted losses and retrieve the appropiate ballast
     C* and weighing value from file
     C*
     C                   Z-ADD     ZTEXLS        XXLOSS
     C*
     C     BAKEY         SETGT     PBA01
     C     BAPRV         TAG
     C                   READP     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     BAST          ORNE      XXST
     C     BACO#         ORNE      XXCO#
     C     BAEFDT        ORGT      X2EFDT
     C                   Z-ADD     0             BABALL
     C                   ELSE
     C     BALOSS        IFGT      XXLOSS
     C                   GOTO      BAPRV
     C                   END
     C                   END
     C                   Z-ADD     BABALL        XXBALL
     C*
     C* CHECK FOR STATES NORTH CAROLINA, SOUTH CAROLINA, FLORIDA
     C*
     C** NORTH CAROLINA
     C*
     C* EFF DT 1/1/94  CHG 2.20 TO 2.70
     C*
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 2.70) / EXP + (700 * 2.70))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 2.70)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 2.70
     C*
     C     ACST          IFEQ      'NC'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   2.70          X2                                |
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   2.70          X4                                 |
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "NC" END
     C*
     C* SOUTH CAROLINA
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 4.35) / EXP + (700 * 4.35))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 4.35)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 4.35
     C*
     C     ACST          IFEQ      'SC'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   4.35          X2
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   4.35          X4
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "SC" END
     C*
     C*
     C* LOUSIANA
     C*
     C* EFF DT 1/1/94  CHG 6.30 TO 6.65
     C*
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 6.65) / EXP + (700 * 6.65))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 6.65)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 6.65
     C*
     C     ACST          IFEQ      'LA'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   6.65          X2                              |
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   6.65          X4                              |
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "LA" END
     C* TEXAS
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 4.35) / EXP + (700 * 4.35))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 4.35)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 4.35
     C*
     C     ACST          IFEQ      'TX'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   4.35          X2
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   4.35          X4
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "TX" END
     C*
     C* FLORIDA - IF EFF DT OF MOD IS >= 10-01-92
     C*
     C* EFF DT 1/1/94  CHG 5.00 TO 5.60
     C*
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 5.60) / EXP + (700 * 5.60))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 5.60)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 5.60
     C*
     C     ACST          IFEQ      'FL'
     C     X2EFDT        IFGE      921001
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C     BAEFDT        ORGT      X2EFDT
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   5.60          X2                              |
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   5.60          X4                              |
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     EFF DT
     C                   END                                                     "FL" END
     C*
     C* GEORGIA
     C*
     C* EFF DT 1/1/91
     C*
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 2.50) / EXP + (700 * 2.50))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 2.50)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 2.50
     C*
     C     ACST          IFEQ      'GA'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   2.50          X2
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   2.50          X4
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "GA" END
     C*
     C* MASSACHUSETTS
     C*
     C* EFF DT 1/1/92
     C*
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 7.00) / EXP + (700 * 7.00))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 7.00)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 7.00
     C*
     C     ACST          IFEQ      'MA'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   7.00          X2
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   7.00          X4
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "MA" END
     C*
     C* NEW HAMPSHIRE
     C*
     C* EFF DT 1/1/94
     C*
     C* BALL=(.10 * EXLS) +((2500 * EXLS * 4.55) / EXP + (700 * 4.55))
     C* X1= (.10 * EXPEXTED LOSS)
     C* X2= (EXP LOSS + ( 700 * 4.55)
     C* X3= EXLS / X2                     DO THE DIVIDE FIRST
     C* X4= X3 * 2500 * 4.55
     C*
     C     ACST          IFEQ      'NH'
     C     BAKEY1        READE     PBA01                                  61
     C     *IN61         IFEQ      '1'
     C     ZTEXLS        ORGT      BALOSS
     C                   Z-ADD     0             X1                6 0
     C                   Z-ADD     0             X2               11 0
     C                   Z-ADD     0             X3               11 9
     C                   Z-ADD     0             X4                6 0
     C     ZTEXLS        MULT(H)   .10           X1
     C     700           MULT(H)   4.55          X2
     C     ZTEXLS        ADD       X2            X2
     C     ZTEXLS        DIV(H)    X2            X3
     C     2500          MULT(H)   4.55          X4
     C     X4            MULT(H)   X3            X4
     C     X4            ADD       X1            XXBALL            6 0
     C                   Z-ADD     XXBALL        BABALL
     C                   END                                                     EXP < TABLE
     C                   END                                                     "NH" END
     C*
     C* convert ballast value, find our total denominator
     C*
     C                   Z-ADD     XXBALL        BABALL
     C                   Z-ADD     BABALL        ZTBALL
     C*
     C     ZTBALL        ADD       ZTEXLS        ZTDENM
     C*
     C* prepare weighting value
     C*
     C     WEKEY         SETGT     PWE01
     C     WEPRV         TAG
     C                   READP     PWE01                                  61
     C     *IN61         IFEQ      '1'
     C     WECO#         ORNE      XXCO#
     C     WEST          ORNE      XXST
     C     WEEFDT        ORGT      X2EFDT
     C                   Z-ADD     0             WEWEIG
     C                   ELSE
     C     WELOSS        IFGT      XXLOSS
     C                   GOTO      WEPRV
     C                   END
     C                   END
     C     WEWEIG        IFEQ      0
     C     BABALL        ANDEQ     0
     C                   Z-ADD     1             WEWEIG
     C                   Z-ADD     WEWEIG        ZTWVWV
     C                   ELSE
     C     WEWEIG        DIV(H)    100           ZTWVWV
     C                   END                                                    end weigh
     C*
     C     ZTAEL         MULT(H)   ZTWVWV        ZTAELW
     C*
     C* take  1-"w", times the expected excess, add the ttl primary,
     C* ballast value actual excess times "w", and the expected excess
     C* times 1-"w" to find total numerator
     C*
     C                   Z-ADD     0             XXWK              9 4
     C     1             SUB       ZTWVWV        XXWK
     C     XXWK          MULT(H)   ZTEXEX        ZTEXEW
     C                   Z-ADD     ZTPAL         ZTNUM
     C     ZTNUM         ADD       ZTBALL        ZTNUM
     C     ZTNUM         ADD       ZTAELW        ZTNUM
     C     ZTNUM         ADD       ZTEXEW        ZTNUM
     C*
     C* determine experience modifier
     C     ZTDENM        IFEQ      0
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    NOPREM
     C                   Z-ADD     0             ZTMOD
     C                   ELSE
     C     ZTNUM         DIV(H)    ZTDENM        ZTMOD
     C                   END
     C*
     C     ZTEXLS        IFEQ      0
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    NOLOSS
     C                   Z-ADD     0             ZTERTO
     C                   ELSE
     C     ZTAAL         DIV(H)    ZTEXLS        ZTERTO
     C                   END
     C*
     C* check the value of the mod
     C*
     C*  execute special rule for FL Exp Mod
     C*
     C     ACST          IFEQ      'FL'
     C*
     C     ZTMOD         IFGT      1
     C     ZTERTO        ANDLT     1
     C                   EXSR      MOD1
     C                   ELSE
     C     ZTMOD         IFLT      1
     C     ZTERTO        ANDGT     1
     C                   EXSR      MOD0
     C                   END
     C                   END
     C                   END
     C*
     C* BEGIN ADDITIONAL CALC'S IF STATE = MA
     C*
     C     ACST          IFEQ      'MA'
     C                   EXSR      EXPMA
     C                   END
     C*
     C* Retrieve batch record for the LOW two year date
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     XXHIB1        XXYEAR
     C                   Z-ADD     0             XXFRDT
     C     EBKEY         CHAIN     PEB01                              62
     C     *IN62         IFEQ      '1'
     C                   Z-ADD     0             XXFRDT
     C                   ELSE
     C                   Z-ADD     EBFRDT        XXFRDT
     C                   END
     C*
     C* Retrieve the batch record for the HIGH two year date
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     XXHIBT        XXYEAR
     C                   Z-ADD     0             XXTODT
     C     EBKEY         CHAIN     PEB01                              62
     C     *IN62         IFEQ      '1'
     C                   Z-ADD     0             XXTODT
     C                   ELSE
     C                   Z-ADD     EBTODT        XXTODT
     C                   END
     C*
     C* Find the difference in the 2 dates in terms of months
     C                   EXSR      MNTHS
     C*
     C* take the difference & divide by 24, then multiply times the
     C* company 2 year total
     C*
     C     XXMNTH        DIV(H)    24            X1WK              5 4
     C     X1WK          MULT(H)   ACPR2Y        ZZWK              9 0
     C* find the actual manual premium for our two years (high & low)
     C                   Z-ADD     0             ZZWK1             9 0
     C                   Z-ADD     1             X
     C                   DO        9             X
     C     BT(X)         IFEQ      XXHIBT
     C     BT(X)         OREQ      XXHIB1
     C     ZZWK1         ADD       BP(X)         ZZWK1
     C                   END
     C                   END                                                      end do
     C*
     C* move information to hold buckets
     C                   MOVE      *BLANKS       XTOVR3
     C                   Z-ADD     ZZWK1         ZTM2YR
     C                   Z-ADD     0             ZTM2RT
     C     XXMNTH        DIV(H)    24            ZTM2RT
     C     ZZWK1         IFLT      ZZWK
     C                   MOVE      '*'           XTOVR3
     C                   END
     C*
     C* if we did not meet the 2 year minimum, check to see if it
     C* meets the all years minimum
     C*
     C                   Z-ADD     XXLODT        XXFRDT
     C                   Z-ADD     XXHIDT        XXTODT
     C                   EXSR      MNTHS
     C*
     C     XXMNTH        DIV(H)    12            X1WK
     C     X1WK          MULT(H)   ACPRAV        ZZWK
     C                   Z-ADD     0             ZZWK1             9 0
     C                   Z-ADD     1             X
     C                   DO        9             X
     C     ZZWK1         ADD       BP(X)         ZZWK1
     C                   END                                                      end do
     C*
     C                   Z-ADD     ZZWK1         ZTMALY
     C     XXMNTH        DIV(H)    12            ZTMALR
     C     XTOVR3        IFEQ      '*'
     C     ZZWK1         IFLT      ZZWK
     C                   MOVE      '*'           XTOVR3
     C                   ELSE
     C                   MOVE      *BLANKS       XTOVR3
     C                   END
     C                   END
     C*
     C* write the work file record
     C                   Z-ADD     XXCO#         XCO#
     C                   Z-ADD     EMFND         XFUND
     C                   Z-ADD     EMEMP#        XEMP#
     C                   Z-ADD     0             XYEAR
     C                   MOVE      QQEFDT        ZZEFDT
     C*
     C                   Z-ADD     ZZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XEFDT
     C*
     C                   MOVE      'C'           XREC
     C                   Z-ADD     ZTPAL         XTPAL
     C                   Z-ADD     ZTAAL         XTAAL
     C                   Z-ADD     ZTAEL         XTAEL
     C                   Z-ADD     ZTEXEX        XTEXEX
     C                   Z-ADD     ZTEXLS        XTEXLS
     C                   Z-ADD     ZTBALL        XTBALL
     C                   Z-ADD     ZTAELW        XTAELW
     C                   Z-ADD     ZTWVWV        XTWVWV
     C                   Z-ADD     ZTEXEW        XTEXEW
     C                   Z-ADD     ZTDENM        XTDENM
     C                   Z-ADD     ZTNUM         XTNUM
     C                   Z-ADD     0             XTMOD
     C                   Z-ADD     0             XTERTO
     C                   Z-ADD     0             XTMODA
     C                   Z-ADD     ZTMOD         XTMOD
     C                   Z-ADD     ZTERTO        XTERTO
     C                   Z-ADD     ZTMODA        XTMODA
     C                   Z-ADD     ZTMALR        XTMALR
     C                   Z-ADD     ZTM2RT        XTM2RT
     C                   Z-ADD     ZTM2YR        XTM2YR
     C                   Z-ADD     ZTMALY        XTMALY
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XDT
     C*
     C                   Z-ADD     DSTIME        XTIME
     C                   MOVE      Q1USER        XUSER
     C                   MOVE      Q1PGM         XPGM
     C                   MOVE      Q1USER        XIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XIDT
     C*
     C     XXRVAL        MULT(H)   1             XTRVAL
     C     XXSURF        MULT(H)   1             XTSURF
     C                   Z-ADD     XXEPLS        XTEPLS
     C*
     C****** 09/17/06  BGN ADD  *********
     C*
     C                   EXSR      GERT
     C                   EXSR      AGT
     C*
     C****** 09/17/06  END ADD  *********
     C*
     C*
     C                   WRITE     PXX01
     C                   EXSR      CLR
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  MOD1  - subroutine to calulate mods > 1.00
     C****************************************************************
     C*
     C* This subroutine will calculate months between two dates
     C*
     C     MOD1          BEGSR
     C*
     C                   Z-ADD     0             ZTMODA
     C     ZTERTO        IFLT      .833
     C                   Z-ADD     1.00          ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C*
     C                   Z-ADD     0             XXADJ            11 4
     C     ZTMOD         SUB       1             XXADJ
     C*
     C     ZTERTO        IFLT      .84
     C     XXADJ         MULT(H)   .1            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .86
     C     XXADJ         MULT(H)   .2            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .88
     C     XXADJ         MULT(H)   .3            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .90
     C     XXADJ         MULT(H)   .4            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .92
     C     XXADJ         MULT(H)   .5            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .94
     C     XXADJ         MULT(H)   .6            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .96
     C     XXADJ         MULT(H)   .7            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C     ZTERTO        IFLT      .98
     C     XXADJ         MULT(H)   .8            ZTMODA
     C                   ADD       1             ZTMODA
     C                   GOTO      MOD1E
     C                   END
     C*
     C     XXADJ         MULT(H)   .9            ZTMODA
     C                   ADD       1             ZTMODA
     C*
     C     MOD1E         TAG
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  MOD0  - subroutine to calulate mods < 1.00
     C****************************************************************
     C*
     C* This subroutine will calculate months between two dates
     C*
     C     MOD0          BEGSR
     C*
     C                   Z-ADD     0             ZTMODA
     C     ZTERTO        IFGT      1.167
     C                   Z-ADD     1.00          ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C*
     C                   Z-ADD     0             XXADJ            11 4
     C     1             SUB       ZTMOD         XXADJ
     C*
     C     ZTERTO        IFGE      1.16                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .1            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.14                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .2            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.12                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .3            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.10                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .4            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.08                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .5            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.06                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .6            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.04                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .7            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.02                                         LJQ 2-24-92
     C     XXADJ         MULT(H)   .8            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C     ZTERTO        IFGE      1.001                                        LJQ 2-24-92
     C     XXADJ         MULT(H)   .9            ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C                   GOTO      MOD0E
     C                   END
     C*
     C     XXADJ         MULT      1             ZTMODA
     C     1             SUB       ZTMODA        ZTMODA
     C*
     C     MOD0E         TAG
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  TTL0  - subroutine to clear total fields
     C****************************************************************
     C*
     C* This subroutine will initialize total fields
     C*
     C     TTL0          BEGSR
     C*
     C                   Z-ADD     0             ZTPAL            11 0
     C                   Z-ADD     0             ZTAAL            11 0
     C                   Z-ADD     0             ZTAEL            11 0
     C                   Z-ADD     0             ZTEXEX           11 0
     C                   Z-ADD     0             ZTEXLS           11 0
     C                   Z-ADD     0             ZTBALL           11 0
     C                   Z-ADD     0             ZTAELW           11 0
     C                   Z-ADD     0             ZTWVWV            3 2
     C                   Z-ADD     0             ZTEXEW           11 0
     C                   Z-ADD     0             ZTDENM           11 0
     C                   Z-ADD     0             ZTNUM            11 0
     C                   Z-ADD     0             ZTMOD             5 4
     C                   Z-ADD     0             ZTERTO            7 4
     C                   Z-ADD     0             ZTMODA            5 4
     C                   Z-ADD     0             ZTM2YR           11 0
     C                   Z-ADD     0             ZTMALY           11 0
     C                   Z-ADD     0             ZTM2RT           11 4
     C                   Z-ADD     0             ZTMALR           11 4
     C                   Z-ADD     0             XXLODT
     C                   Z-ADD     0             XXHIDT
     C                   Z-ADD     0             XXHIBT
     C                   Z-ADD     0             XXHIB1
     C                   Z-ADD     0             BP
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  MNTHS - subroutine to number of months between dates
     C****************************************************************
     C*
     C* This subroutine will calculate months between two dates
     C*
     C     MNTHS         BEGSR
     C*
     C                   Z-ADD     0             XXMNTH            3 0
     C                   Z-ADD     0             Z1WK              3 0
     C* find number of years between dates
     C     XXTOYR        SUB       XXFRYY        XXMNTH
     C* find months between dates
     C     XXTOMN        SUB       XXFRMN        Z1WK
     C* convert years to months
     C     XXMNTH        MULT      12            XXMNTH
     C* add up months
     C     Z1WK          ADD       XXMNTH        XXMNTH
     C                   ADD       1             XXMNTH
     C*
     C                   ENDSR
     C*
     C****************************************************************
     C*  EXPMA - subroutine FOR MA ADDITIONAL CALCS
     C****************************************************************
     C*
     C* This subroutine will calculate months between two dates
     C*
     C     EXPMA         BEGSR
     C*
     C                   Z-ADD     0             XXRVAL            5 4
     C                   Z-ADD     0             XXSURF            5 4
     C* CALCULATE 'R' VALUE  - CAPPED AT 2.0
     C*
     C* R = ((.5 - .5*W) * AP)/(M * EP) + ((.5 + .5*W) * A)/(M * E)
     C* R1 = .5 * W       W = WEIGHTING VALUE (XTWVWV)
     C* R2 = .5 - R1 = (5 - (.5*W))
     C* R3 =  R2 * AP = ((.5 - (.5*W)) * AP) AP = ACT PRI LOSS (XTPAL)
     C* R4 = M * EP  M = EXP MOD, EP=EXPT PRI LOSS  (XTMOD XXEPLS)
     C* R5 = .5 + R1 = (.5 + (.5 * W))
     C* R6 =  R5 * A         A = ACT OSS (XTAAL)
     C* R7 =  M * E
     C* R8 = R6 / R7
     C* R9 = R3 / R4
     C* R10 = R8 + R9
     C*
     C                   Z-ADD     0             R1                5 4
     C                   Z-ADD     0             R2                5 4
     C                   Z-ADD     0             R3               15 4
     C                   Z-ADD     0             R4               13 4
     C                   Z-ADD     0             R5                7 4
     C                   Z-ADD     0             R6               13 4
     C                   Z-ADD     0             R7               13 4
     C                   Z-ADD     0             R8                5 4
     C                   Z-ADD     0             R9                5 4
     C*
     C     .50           MULT(H)   ZTWVWV        R1
     C     .5            SUB       R1            R2
     C     R2            MULT(H)   ZTPAL         R3
     C     ZTMOD         MULT      XXEPLS        R4
     C     .5            ADD       R1            R5
     C     R5            MULT(H)   ZTAAL         R6
     C     ZTMOD         MULT(H)   ZTEXLS        R7
     C     R7            IFNE      0
     C     R6            DIV(H)    R7            R8
     C                   ELSE
     C     R6            MULT      1             R8
     C                   END
     C     R4            IFNE      0
     C     R3            DIV(H)    R4            R9
     C                   ELSE
     C     R3            MULT      1             R9
     C                   END
     C     R8            ADD       R9            XXRVAL
     C     XXRVAL        IFGT      2.0
     C                   Z-ADD     2.0           XXRVAL
     C                   END
     C     XXRVAL        MULT(H)   1             XTRVAL
     C*
     C* CALCULATE SURCH IF R > 1
     C*
     C* CALCULATE SURCHARGE FACTOR      ! = TO THE POWER OF
     C* POWER OF 1.25 = SQRT(SQRT(X)) * X
     C* POWER OF  .25 = SQRT(X) * X
     C*
     C* S = 1 + ((.08 * E * ((R-1)!1.25)) / (E+3)!.5
     C*
     C* S1 = XTRVAL - 1
     C* S2 = SQRT(S1)
     C* S2 = SQRT(S2)
     C* S3 = S1 * S2
     C* S4 = E / 1000   E = TTL EXP LOSS (XTEXLS)
     C* S5 =  S4 * S3
     C* S6 = S5 * .08
     C* S7 = S4 + 3
     C* S8 = SQRT(S7)
     C* S9 = S7 * S8
     C* S10= S6 / S9
     C* XXSURF = 1 + S10
     C*
     C                   Z-ADD     0             S1                5 4
     C                   Z-ADD     0             S2                5 4
     C                   Z-ADD     0             S3                5 4
     C                   Z-ADD     0             S4               13 4
     C                   Z-ADD     0             S5               13 4
     C                   Z-ADD     0             S6               13 4
     C                   Z-ADD     0             S7               13 4
     C                   Z-ADD     0             S8               13 4
     C                   Z-ADD     0             S9               13 4
     C                   Z-ADD     0             S10               5 4
     C*
     C     XTRVAL        IFGT      1
     C     XTRVAL        SUB       1             S1
     C***** 08/28/04  BGN CHG ************************
     C*****************    SQRT S1        S2        H
     C*****************    SQRT S2        S2        H
     C*********  S1        MULT S2        S3        H
     C* CALL SUB PROGRAM TO CAL THE VALUES OF S1 !1.25
     C*
     C                   MOVE      *BLANK        P1                1            RTN CODE
     C                   Z-ADD     S1            P2                5 4          S1 VALUE
     C                   Z-ADD     1.25          P3                5 4          EXPONENT
     C                   Z-ADD     0             P4                5 4          RESULT
     C                   CALL      'E7056S'
     C                   PARM                    P1
     C                   PARM                    P2
     C                   PARM                    P3
     C                   PARM                    P4
     C     P1            IFEQ      ' '
     C                   Z-ADD     P4            S3
     C                   ELSE
     C                   Z-ADD     0             S3
     C                   EXCEPT    MAERR
     C                   END
     C***** 08/28/04  END CHG ************************
     C***  BGN CHG 08/28/04 ******************************
     C     ZTEXLS        IFGT      40000
     C     40000         DIV(H)    1000          S4
     C                   ELSE
     C***  END CHG 08/28/04 ******************************
     C     ZTEXLS        DIV(H)    1000          S4
     C***  BGN CHG 08/28/04 ******************************
     C                   END
     C***  END CHG 08/28/04 ******************************
     C     S3            MULT(H)   S4            S5
     C     .08           MULT(H)   S5            S6
     C     3             ADD       S4            S7
     C                   SQRT(H)   S7            S8
     C***  BGN CHG 08/28/04 ******************************
     C********   S7        MULT S8        S9        H
     C                   Z-ADD     S8            S9
     C***  END CHG 08/28/04 ******************************
     C     S9            IFNE      0
     C     S6            DIV(H)    S9            S10
     C                   ELSE
     C     S6            MULT(H)   1             S10
     C                   END
     C     S10           ADD       1             XXSURF
     C***** 08/28/04  BGN CHG ************************
     C*******    XXSURF    IFGT 1.49
     C*******              Z-ADD1.49      XXSURF
     C*******              END
     C***** 08/28/04  END CHG ************************
     C                   END
     C*
     C                   ENDSR
     C*
     C*
     C****** 09/17/06  BGN ADD  *********
     C*
     C****************************************************************
     C*  GERT - subroutine GERT FACTOR
     C****************************************************************
     C*
     C*  CAP = 1 + (.00005) * ((EXP LOSS) + (2 * EXP LOSS)/GERT))
     C*  WHRE EXPECTED LOSS = XTEXLS WITHIN THIS PGM
     C*
     C     GERT          BEGSR
     C*
     C                   Z-ADD     0             XTGERT
     C                   Z-ADD     0             XTGMOD
     C     GTKEY         SETGT     PGT01
     C                   READP     PGT01                                  61
     C     *IN61         IFEQ      '1'
     C     GTST          ORNE      XXST
     C     GTCO#         ORNE      XXCO#
     C     GTEFDT        ORGT      X2EFDT
     C                   Z-ADD     0             XTGERT
     C                   GOTO      ENDGRT
     C                   ELSE
     C                   Z-ADD     GTGERT        XTGERT
     C                   END
     C*
     C                   Z-ADD     0             ZZGERT           17 6
     C     XTEXLS        MULT      2             ZZGERT
     C     ZZGERT        DIV       XTGERT        ZZGERT
     C                   ADD       XTEXLS        ZZGERT
     C                   MULT      .00005        ZZGERT
     C                   ADD       1             ZZGERT
     C     ZZGERT        MULT(H)   1             XTGMOD
     C*
     C     ENDGRT        TAG
     C                   ENDSR
     C*===============================================================
     C*
     C****************************************************************
     C*  AGT  - subroutine TO GET AGENT & PRODUCER NAMES
     C****************************************************************
     C*
     C*
     C     AGT           BEGSR
     C*
     C                   ENDSR
     C*===============================================================
     C*
     C****** 09/17/06  END ADD  *********
     C*
     C*
     O***************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *******************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O*  MLS 1/19/88  ADD INDICATORS
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *******************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/19/88  ADD INDICATORS
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              79
      ***** Beg Del ***** 08/26/14 *******************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *******************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1990'
     O****    E  160           ROUTE
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     80
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL              68
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           1
     O                                           09 'FUND:'
     O                       XXFND         Z     13
     O                       FNNAME              74
     O          E            HEAD           2
     O                                           13 'RUN DATE:'
     O                       XXRNDT              22 '  /  /  '
     O          E            NOPREM         1
     O                       EMEMP#        Z     09
     O                                           33 '** NO PREMIUM FOUND'
     O                                           45 'FOR ACCOUNT'
     O          E            NOLOSS         1
     O                       EMEMP#        Z     09
     O                                           35 '** NO EXPECTED LOSSES'
     O                                           53 'FOUND FOR ACCOUNT'
     O          E            MAERR          1
     O                       EMEMP#        Z     09
     O                                           35 '** ERROR ON MA ARAP *'
     O                                           53 '*                '
