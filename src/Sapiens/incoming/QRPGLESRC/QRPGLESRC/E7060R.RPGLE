     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7060R - EMP EXP MOD - PRINT EXPERIENCE MODIFICATION
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS THE EXP MOD                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7055C   from calculation program (print flag = y)      **
     H** or  E7061C   from menu                                      **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    02/26/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                              *
     H** MODIFICATIONS:                                               *
     H*                                                               *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Exapand Expected Loss Rate to 8,3.             *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 08/09/10  C:246  SUPPORT:73562  PRINT GERT MOD.               *
     H*                                                               *
     H* 07/22/10  C:246  SUPPORT:73562  REROFIT THE WC1502 GERT FTR  **
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 09/17/06  C:03   PROJECT: WC1502
     H*                  ADD GERT FACTOR
     H**                                                              *
     H**      12/09/03  C:003 SUPPORT: EX129  40291                   *
     H**                ADD FND TO ACKEY FOR MA PROCESSING           **
     H**                USE EMACL1
     H**                                                             **
     H**      10/18/02  C:136 SUPPORT: EX093  34579                  **
     H**                CHANGE RATE FIELD FOR FILE CHANGE            **
     H**                                                             **
     H**                                                             **
     H**      10/01/01  C:136  PROJ: WC577                           **
     H**                EXP_MOD REWRITE - CHG 'FUND' TO 'GRP'        **
     H**                                                             **
     H**      10/13/98  C:03   PROJ: WC853                           **
     H**                ADDED ARRAY FOR NON-EXCESS MED ONLY CLMS     **
     H**                                                             **
     H**      09/21/98  C:03   PROJ: WC853                           **
     H**                CORRECTED ARRAY LOAD FOR NON-EXCESS CASES    **
     H**                                                             **
     H**      08/12/98  C:90   PROJ: WC853                           **
     H**                MODIFIED TO VERSION 3 STANDARDS              **
     H**                                                             **
     H**      11-07-94  C:30   WC356                                 **
     H**                       CO# PASSED AS PARM.                   **
     H**                                                             **
     H**      08-02-94   003   WC356                                 **
     H**                       1. ADD PROCESSING FOR MA.  NEED TO    **
     H**                          GET THE COMP AUX TO DETERMINE      **
     H**                          WHETHER TO PRINT "MA" ADDITIONAL   **
     H**                          FIELDS.
     H**                                                             **
     H**       11-13-91  LJQ   CHANGED OUTPUT TO EXCLUDE PRINTING    **
     H**                       IF EMPLOYER IS INACTIVE OR MOD FLAG   **
     H**                       IS EQUAL TO "N".                      **
     H**                       PROJECT# MD005    CLIENT# = 999       **
     H**                                                             **
     H**       11-04-91  LJQ   CHANGED OUTPUT TO INCLUDE MESSAGE FOR **
     H**                       CLAIMS WHICH EXCESS EXCESS LIMIT      **
     H**                       XOVR1 = "*"                           **
     H**                       PROJECT# MD007    CLIENT# = 999       **
     H**                                                             **
     H**       9-19-91   LJQ   CHANGED FOR RATE DATE FOR BSI5  FILE  **
     H**                       ADDED EFFECT DATE FOR RATE            **
     H**                       PROJECT # AIZ (WORK COMP) CLIENT# = 3 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**     80                 NEW Employer                         **
     H**     81                 Print Total Losses Detail Line       **
     H**     50                 Chain - Work File                    **
     H**     60                 Chain - General Usage                **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO#                                       **
     H**    EMUPL (PUP01): USER                                      **
     H**    EXXTL (PXX01): CO/FND/EMP#/EFDT                          **
     H**    EMEML (PEM01): CO/FUND/EMP#                              **
     H**    EMEBL (PEB01): CO/FUND/EMP#/BATCH #                      **
     H**    EMFNL (PFN01): CO/FND                                    **
     H**    SMORL (POR01): CO/PGM/PRT                                **
     H**    QSYSPRT1                                                 **
     H**    EMACL (PAC01): CO#                                       **
     H*****************************************************************
     H*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* Company Auxiliary Master File
     F*** BGN CHG 12/09/03
     F******EMACL   IF  E           K        DISK
     FEMACL1    IF   E           K DISK
     F*** BGN CHG 12/09/03
     F*
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK
     F*
     F* Exp Mod Work File
     FEXXTL     IF   E           K DISK
     F*
     F* Employer Master Maintenance
     FEMEML     IF   E           K DISK
     F*
     F* Employer Batch Maintenance
     FEMEBL     IF   E           K DISK
     F*
     F* Fund Master File
     FEMFNL     IF   E           K DISK
     F*
     F* Output Routing File
     FSMORL     IF   E           K DISK
     F*
     F* Printer Output File
     F*
     FQSYSPRT1  O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)

     D*
     D* Array for Batch Years to be Processed
     D BT              S              3  0 DIM(9)
     D*
     D* Array for Actual Losses
     D AC              S             11  2 DIM(4)
     D*
     D* Array for Non-Excess Losses
     D NE              S             11  2 DIM(4)
     D********* BGN ADD  10/13/98 ********************************
     D*
     D* Array for Non-Excess Losses
     D N2              S             11  2 DIM(4)
     D********* BGN ADD  10/13/98 ********************************
     D*
     D* Array for Excess Losses
     D EX              S             11  2 DIM(4)
     D*
     D* Array for Payroll - Expected Losses
     D BA              S             11  2 DIM(4)
     D*
     D* Array for Expected Total
     D EL              S             11  2 DIM(4)
     D*
     D* Array for Print Messages
     D NT              S             70    DIM(2) CTDATA PERRCD(1)
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Data Structure for Line and Page Number
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      *** Beg Add ***** 06/19/24 **********************************
     D MOD             S              4  3
     D GMOD            S              4  3
     D MNRT            S              7  4
     D ELRT            S              7  4
      *** End Add ***** 06/19/24 **********************************

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        XXEMP2
     C     *LIKE         DEFINE    XYEAR         XXYEAR
     C     *LIKE         DEFINE    XEFDT         XXEFDT
     C     *LIKE         DEFINE    XXPAGE        ZZPAGE
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
     C*** BGN CHG 12/09/03
     C                   KFLD                    XXFND
     C*** END CHG 12/09/03
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
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9            from emp#
     C                   PARM                    QQBAT1            3
     C                   PARM                    QQBAT2            3
     C                   PARM                    QQBAT3            3
     C                   PARM                    QQBAT4            3
     C                   PARM                    QQBAT5            3
     C                   PARM                    QQBAT6            3
     C                   PARM                    QQBAT7            3
     C                   PARM                    QQBAT8            3
     C                   PARM                    QQBAT9            3
     C                   PARM                    QQEFDT            8             mdy
     C                   PARM                    QQRNDT            8             mdy
     C                   PARM                    QQQUE            10
     C                   PARM                    QQEMP2            9             to emp#
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
     C                   MOVE      QQEFDT        X2EFDT            8 0
     C*
     C                   Z-ADD     X2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1EFDT            6 0
     C*
     C                   MOVE      QQEFDT        ZZEFDT            8 0
     C*
     C                   Z-ADD     ZZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQEMP2        XXEMP2
     C                   MOVE      QQFND         XXFND
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
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Retrieve the Fund Record
     C*
     C                   Z-ADD     0             XXFND
     C                   MOVE      QQFND         XXFND
     C     FNKEY         CHAIN     PFN01                              55
     C     *IN55         IFEQ      '1'
     C                   MOVE      *BLANKS       FNNAME
     C                   END
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C                   MOVE      '1'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C                   MOVE      *BLANKS       XXTITL           60
     C                   MOVEL     ORTITL        XXTITL
     C                   TIME                    DSDTTM
     C*
      ***** Beg Del ***** 08/26/14 *******************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *******************************************
     C*
     C* Clear work fields
     C*
     C                   EXSR      CLR
     C*
     C* MAIN LOOP - Read the Work File
     C*
     C     XXEMP#        IFEQ      0
     C     XXKY1         CHAIN     PXX01                              50
     C                   ELSE
     C     XXKY2         CHAIN     PXX01                              50
     C                   END
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* Process records with effective date = requested effective date
     C*
     C     XEFDT         IFEQ      XXEFDT
     C*
     C* Retrieve Employer Record if employer has changed (*in80)
     C*
     C     *IN80         IFNE      '1'
     C                   Z-ADD     XFUND         XXFND
     C                   Z-ADD     XEMP#         XXEMP#
     C                   MOVE      *BLANKS       XXENAM           30
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM
     C                   ELSE
     C                   MOVEL     *BLANKS       EMSEMP
     C                   END                                                    *IN60 END
     C*
     C     EMMOD         IFEQ      'N'
     C     EMSTAT        OREQ      'I'
     C                   GOTO      NOPRNT
     C                   END
     C*
     C* Print Loss Heading
     C*
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    LSHD
     C                   MOVE      '1'           *IN80
     C*
     C* save current year being processed for the new employer
     C*
     C                   Z-ADD     XYEAR         HOLDYR            3 0
     C                   END                                                    *IN80 <> 1
     C*
     C* Process "A" Type records - Losses
     C*
     C     XREC          IFEQ      'A'
     C*
     C* Batch yr changed; print total for yr and get new batch yr rec
     C*
     C     HOLDYR        IFNE      XYEAR
     C                   EXSR      GETBT
     C                   END
     C*
     C* Check for new page
     C*
     C     XXLINE        IFGE      56
     C     *INOA         OREQ      '1'
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    LSHD
     C                   END
     C*
     C     XACDT         IFEQ      0
     C* Non-Excess Claim
     C*** BGN CHG 09/21/98 *****************************************
     C***                  Z-ADDXAAMT     NE,1
     C***                  Z-ADDXAAMT     NE,2
     C***                  Z-ADDXNCLMS    NE,4
     C*** BGN CHG 10/13/98 *****************************************
     C     XOFF          IFEQ      '6'
     C                   Z-ADD     XAAMT         N2(1)
     C                   Z-ADD     XPRAMT        N2(2)
     C                   Z-ADD     XNCLMS        N2(4)
     C*** END CHG 10/13/98 *****************************************
     C*** ADD CHG 10/13/98 *****************************************
     C******************** ADD  XAAMT     NE,1
     C******************** ADD  XAAMT     NE,2
     C******************** ADD  XNCLMS    NE,4
     C                   ELSE
     C                   Z-ADD     XAAMT         NE(1)
     C                   Z-ADD     XPRAMT        NE(2)
     C                   Z-ADD     XNCLMS        NE(4)
     C*** END CHG 10/13/98 *****************************************
     C*** END CHG 09/21/98 *****************************************
     C*** BGN CHG 10/13/98 *****************************************
     C                   END
     C*** END CHG 10/13/98 *****************************************
     C     XAAMT         ADD       AC(1)         AC(1)
     C     XAAMT         ADD       AC(2)         AC(2)
     C     XNCLMS        ADD       AC(4)         AC(4)
     C*
     C                   ELSE
     C* Excess Claim
     C                   Z-ADD     XACDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C     XAAMT         ADD       AC(1)         AC(1)
     C     XPRAMT        ADD       AC(2)         AC(2)
     C     XAEAMT        ADD       AC(3)         AC(3)
     C     AC(4)         ADD       1             AC(4)
     C     XAAMT         ADD       EX(1)         EX(1)
     C     XPRAMT        ADD       EX(2)         EX(2)
     C     XAEAMT        ADD       EX(3)         EX(3)
     C     EX(4)         ADD       1             EX(4)
     C                   Z-ADD     XAAMT         AAMT             11 2
     C                   Z-ADD     XPRAMT        PRAMT            11 2
     C                   Z-ADD     XAEAMT        AEAMT            11 2
     C*
     C                   MOVE      *BLANKS       XXSTAT            5
     C     XOFF          IFEQ      'F'
     C                   MOVEL     'FINAL'       XXSTAT
     C                   ELSE
     C                   MOVEL     'OPEN'        XXSTAT
     C                   END
     C*
     C* Print excess claim detail line
     C*
     C                   EXCEPT    LSEX
     C     XOVR1         IFEQ      '*'
     C                   MOVE      *BLANKS       XXFLG1
     C                   MOVEL     'YES'         XXFLG1
     C                   Z-ADD     XAAMT         XXFAMT           11 2
     C                   END
     C*
     C                   END                                                    ACDT =  " "
     C                   END                                                    REC = A
     C*
     C* Process "B" Type records - Payroll
     C*
     C     XREC          IFEQ      'B'
     C*
     C* print total for yr and get new batch yr rec
     C*
     C     *IN81         IFNE      '1'
     C                   EXSR      GETBT
     C*
     C* Check for new page
     C*
     C     XXLINE        IFGE      56
     C     *INOA         OREQ      '1'
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    LSHD
     C                   END
     C*
     C* PRINT LOSS TOTAL LINE
     C*
     C                   Z-ADD     AC(4)         TLCLM             3 0
     C                   EXCEPT    LSTL
     C*
     C     XXFLG1        IFEQ      'YES'
     C*
     C* Check for new page
     C*
     C     XXLINE        IFGE      56
     C     *INOA         OREQ      '1'
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    EXHD
     C                   END
     C                   EXCEPT    EXNOTE
     C                   MOVE      *BLANKS       XXFLG1
     C                   END
     C* PRINT NEW PAGE FOR PAYROLL
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    EXHD
     C                   Z-ADD     XYEAR         HOLDYR
     C                   MOVE      '1'           *IN81
     C                   END                                                    *IN81 END
     C*
     C* CHECK FOR NEW BATCH YEAR
     C*
     C     XYEAR         IFNE      HOLDYR
     C                   EXSR      GETB2
     C                   END
     C*
     C* Check for new page
     C*
     C     XXLINE        IFGE      56
     C     *INOA         OREQ      '1'
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    EXHD
     C                   END
     C*
     C* PROCESS RECORD
     C*
     C     XPREM         ADD       EL(1)         EL(1)
     C     XEXLS         ADD       EL(2)         EL(2)
     C     XPRIM         ADD       EL(3)         EL(3)
     C     XEXEX         ADD       EL(4)         EL(4)
     C     XPREM         ADD       BA(1)         BA(1)
     C     XEXLS         ADD       BA(2)         BA(2)
     C     XPRIM         ADD       BA(3)         BA(3)
     C     XEXEX         ADD       BA(4)         BA(4)
     C*
     C* MOVE FILE FIELDS TO OUTPUT FIELDS
     C*
      *** BEG CHG **** 10/18/02 **********************************************
     C********** XMNRT     MULT .0001     MNRT    54
     C********** XELRT     MULT .0001     ELRT    54
      *** Beg Chg ***** 06/19/24 ******************************************
     C***  XMNRT         MULT      .0001         MNRT              7 4
     C***  XELRT         MULT      .0001         ELRT              7 4
       MNRT = XMNRT/100;
       ELRT = XELRT/100;
      *** End Chg ***** 06/19/24 ******************************************
      *** END CHG **** 10/18/02 **********************************************
     C     XDRATO        MULT      .01           DRATO             3 2
     C                   Z-ADD     XPREM         PREM             11 2
     C                   Z-ADD     XEXLS         EXLS             11 2
     C                   Z-ADD     XPRIM         PRIM             11 2
     C                   Z-ADD     XEXEX         EXEX             11 2
     C*
     C                   Z-ADD     XRDAT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RDAT              6 0
     C*
     C* PRINT DETAIL LINE FOR EXPECTED LOSSES
     C*
     C                   EXCEPT    EXDT
     C                   END                                                    REC = B
     C*
     C* Process "C" Type records - EXPERIENCE MOD
     C*
     C     XREC          IFEQ      'C'
     C*
     C* print total for yr and get new batch yr rec
     C*
     C                   EXSR      GETB2
     C*
     C* Check for new page
     C*
     C     XXLINE        IFGE      56
     C     *INOA         OREQ      '1'
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    EXHD
     C                   END
     C*
     C* PRINT LOSS TOTAL LINE
     C*
     C                   EXCEPT    EXTL
     C*
     C* MOVE FILE FIELDS TO OUTPUT FIELDS
     C*
     C     1             SUB       XTWVWV        ZZWVWV            3 2
     C                   Z-ADD     XTPAL         TPAL             11 2
     C                   Z-ADD     XTBALL        TBALL            11 2
     C                   Z-ADD     XTEXLS        TEXLS            11 2
     C                   Z-ADD     XTWVWV        TWVWV             3 2
     C                   Z-ADD     XTAELW        TAELW            11 2
     C                   Z-ADD     XTEXEW        TEXEW            11 2
     C                   Z-ADD     XTNUM         TNUM             11 2
     C                   Z-ADD     XTDENM        TDENM            11 2
     C                   Z-ADD     XTM2YR        TM2YR            11 2
     C                   Z-ADD     XTMALY        TMALY            11 2
     C     XTMALR        MULT(H)   1             MALR              3 2
     C     XTM2RT        MULT(H)   1             M2RT              3 2
     C     XTOVR3        IFEQ      '*'
      *** Beg Chg ***** 06/19/24 ***************************************
     C***                Z-ADD     1.00          MOD               3 2
       Mod = 1.000;
      *** End Chg ***** 06/19/24 ***************************************
     C                   MOVE      *BLANKS       NOTE             75
     C                   MOVEL     NT(1)         NOTE
     C                   ELSE
     C     XTMODA        IFNE      0
     C     XTMODA        MULT(H)   1             MOD
     C                   MOVE      *BLANKS       NOTE
     C                   MOVEL     NT(2)         NOTE
     C                   ELSE
     C     XTMOD         MULT(H)   1             MOD
     C                   END
     C                   END
     C*
     C*** 09/17/06  BGN ADD ***************************
     C*
      *** Beg Chg ***** 06/19/24 ***************************************
     C***                Z-ADD     0.00          GMOD              3 2
       GMod = 0.000;
      *** End Chg ***** 06/19/24 ***************************************
     C     XTGMOD        MULT(H)   1             GMOD
     C*** 09/17/06  BGN ADD ***************************
     C*
     C* PRINT EXP MOD PAGE
     C*
     C     ZZPAGE        ADD       1             ZZPAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    EXPM
     C                   MOVE      *BLANKS       NOTE
     C*
     C*** BGN CHG 12/09/03
     C                   Z-ADD     XFUND         XXFND
     C*** END CHG 12/09/03
     C     ACKEY         CHAIN     PAC01                              60
     C     ACST          IFEQ      'MA'
     C                   Z-ADD     0             RVAL              3 2
     C                   Z-ADD     0             SURF              3 2
     C     XTRVAL        MULT(H)   1             RVAL
     C     XTSURF        MULT(H)   1             SURF
     C                   EXCEPT    MA1
     C                   END
     C*
     C                   EXSR      CLR
     C                   END                                                    REC = C
     C                   ELSE
     C*
     C* Effective date <> requested effective date-clear work feilds
     C*
     C     NOPRNT        TAG
     C*
     C                   EXSR      CLR
     C                   END                                                    EFF DT END
     C*
     C* Retrieve next work file record
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C     XXEMP#        IFEQ      0
     C     XXKY1         READE     PXX01                                  50
     C                   ELSE
     C     XXEMP2        IFNE      0
     C     XXKY1         READE     PXX01                                  50
     C                   ELSE
     C     XXKY2         READE     PXX01                                  50
     C                   END
     C                   END
     C*
     C     XXEMP2        IFNE      0
     C     XEMP#         IFGT      XXEMP2
     C                   GOTO      ENDIT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C* End the Program
     C*
     C     ENDIT         TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C***************************************************************
     C* CLR - Subroutine to Clear Work Fields
     C***************************************************************
     C*
     C     CLR           BEGSR
     C*
     C                   MOVE      '0'           *IN80
     C                   MOVE      '0'           *IN81
     C                   MOVE      *BLANKS       XXFLG1            3
     C                   Z-ADD     0             ZZPAGE
     C                   Z-ADD     0             AC
     C                   Z-ADD     0             NE
     C                   Z-ADD     0             EX
     C                   Z-ADD     0             BA
     C                   Z-ADD     0             EL
     C*
     C                   ENDSR
     C*
     C*
     C***************************************************************
     C* GETBT - Subroutine to RETRIEVE BATCH YEAR RECORD
     C***************************************************************
     C*
     C     GETBT         BEGSR
     C*
     C                   Z-ADD     XFUND         XXFND
     C                   Z-ADD     XEMP#         XXEMP#
     C                   Z-ADD     HOLDYR        XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             EBFRDT
     C                   Z-ADD     0             EBTODT
     C                   END
     C*
     C* Flip dates:  yy/mm/dd to mm/dd/yy
     C*
     C                   Z-ADD     EBFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     EBTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C* add to total arrays
     C*
     C     NE(1)         ADD       EX(1)         BA(1)
     C     NE(2)         ADD       EX(2)         BA(2)
     C     NE(3)         ADD       EX(3)         BA(3)
     C     NE(4)         ADD       EX(4)         BA(4)
     C*** BGN CHG 10/13/98 *****************************************
     C                   ADD       N2(1)         BA(1)
     C                   ADD       N2(2)         BA(2)
     C                   ADD       N2(3)         BA(3)
     C                   ADD       N2(4)         BA(4)
     C*** END CHG 10/13/98 *****************************************
     C*
     C* print non-excess total line
     C*
     C                   Z-ADD     EX(4)         ECLM              3 0
     C                   Z-ADD     NE(4)         NCLM              3 0
     C*** BGN CHG 10/13/98 *****************************************
     C                   Z-ADD     N2(4)         NCL2              3 0
     C*** END CHG 10/13/98 *****************************************
     C                   EXCEPT    LSNE
     C*
     C* print total line for batch year
     C*
     C                   Z-ADD     BA(4)         YRCLM             3 0
     C                   EXCEPT    LSYR
     C*
     C* re-initialize total arrays
     C*
     C                   Z-ADD     0             NE
     C                   Z-ADD     0             EX
     C                   Z-ADD     0             BA
     C*** BGN CHG 10/13/98 *****************************************
     C                   Z-ADD     0             N2
     C*** END CHG 10/13/98 *****************************************
     C                   Z-ADD     XYEAR         HOLDYR
     C*
     C                   ENDSR
     C*
     C***************************************************************
     C* GETB2 - Subroutine to RETRIEVE BATCH YEAR RECORD
     C***************************************************************
     C*
     C     GETB2         BEGSR
     C*
     C                   Z-ADD     XFUND         XXFND
     C                   Z-ADD     XEMP#         XXEMP#
     C                   Z-ADD     HOLDYR        XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             EBFRDT
     C                   Z-ADD     0             EBTODT
     C                   END
     C*
     C* Flip dates:  yy/mm/dd to mm/dd/yy
     C*
     C                   Z-ADD     EBFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     EBTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT
     C*
     C* print total line for batch year
     C*
     C                   EXCEPT    EXYR
     C*
     C* re-initialize total arrays
     C*
     C                   Z-ADD     0             BA
     C                   Z-ADD     XYEAR         HOLDYR
     C*
     C                   ENDSR
     C*
     O***************************************************************
     OQSYSPRT1  E            ROUTE          1 06
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
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O**************************************************************
     O* MAIN Heading
     O**************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    118
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              86
     O                                          108 'PAGE:'
     O                       ZZPAGE        Z    118
     O          E            HEAD           2
      *** BEG CHG **** 10/01/01 ************************************************
     O*********************************  05 'FUND:'
     O                                           04 'GRP:'
      *** END CHG **** 10/01/01 ************************************************
     O                       XXFND         Z     09
     O                       FNNAME              70
     O          E            HEAD           0
     O                                           16 'EFFECTIVE DATE'
     O                                           37 'PROMULGATION DATE'
     O                                           49 'EMPLOYER'
     O                                          100 'STATE EMP#'
     O          E            HEAD           1
     O                                           16 'EFFECTIVE DATE'
     O                                           37 'PROMULGATION DATE'
     O                                           49 'EMPLOYER'
     O                                          100 'STATE EMP#'
     O          E            HEAD           2
     O                       X1EFDT              13 '  /  /  '
     O                       XXRNDT              32 '  /  /  '
     O                       XEMP#         Z     44
     O                       EMNAME              86
     O                       EMSEMP             105
     O**************************************************************
     O* Page 1 - Actual Losses (Excess and Non-Excess)
     O**************************************************************
     O          E            LSHD           0
     O                                           14 'ACTUAL LOSSES'
     O          E            LSHD           2
     O                                           14 'ACTUAL LOSSES'
     O          E            LSHD           2
     O                                           19 'CASE NUMBER'
     O                                           35 'ACC DATE'
     O                                           48 'DISP'
     O                                           72 'ACTUAL'
     O                                           95 'PRIMARY'
     O                                          118 'EXCESS'
     O**************************************************************
     O* Page 1 - Excess Claims Detail Line
     O**************************************************************
     O          E            LSEX           1
     O                       XCASE               18
     O                       XXACDT              35 '  /  /  '
     O                       XXSTAT              49
     O                       AAMT                72 '  ,   ,   , 0 .  '
     O                       XOVR1               74
     O                       PRAMT               95 '  ,   ,   , 0 .  '
     O                       XOVR2               97
     O                       AEAMT              118 '  ,   ,   , 0 .  '
     O**************************************************************
     O* Page 1 - Non-Excess Claims Detail Line
     O**************************************************************
     O          E            LSNE        1  1
     O                                           42 'EXCESS CASES:'
     O                       ECLM                49 ' 0 '
     O                       EX(1)               72 '  ,   ,   , 0 .  '
     O                       EX(2)               95 '  ,   ,   , 0 .  '
     O                       EX(3)              118 '  ,   ,   , 0 .  '
     O          E            LSNE           1
     O                                           42 'NON-EXCESS CASES:'
     O                       NCLM                49 ' 0 '
     O                       NE(1)               72 '  ,   ,   , 0 .  '
     O                       NE(2)               95 '  ,   ,   , 0 .  '
     O                       NE(3)              118 '  ,   ,   , 0 .  '
     C*** ADD CHG 10/13/98 *****************************************
     O          E            LSNE           1
     O                                           42 'MED-NON-EXCESS CASES:'
     O                       NCL2                49 ' 0 '
     O                       N2(1)               72 '  ,   ,   , 0 .  '
     O                       N2(2)               95 '  ,   ,   , 0 .  '
     O                       N2(3)              118 '  ,   ,   , 0 .  '
     C*** END CHG 10/13/98 *****************************************
     O**************************************************************
     O* Page 1 - Batch Year Total
     O**************************************************************
     O          E            LSYR           2
     O                                           22 'POLICY DATES:'
     O                       XXFRDT              31 '  /  /  '
     O                                           33 '-'
     O                       XXTODT              42 '  /  /  '
     O                       YRCLM               49 ' 0 '
     O                       BA(1)               72 '  ,   ,   , 0 .  '
     O                       BA(2)               95 '  ,   ,   , 0 .  '
     O                       BA(3)              118 '  ,   ,   , 0 .  '
     O**************************************************************
     O* Page 1 - Total Actual Losses
     O**************************************************************
     O          E            LSTL        1  2
     O                                           42 'TOTAL ACTUAL:'
     O                       TLCLM               49 ' 0 '
     O                       AC(1)               72 '  ,   ,   , 0 .  '
     O                       AC(2)               95 '  ,   ,   , 0 .  '
     O                       AC(3)              118 '  ,   ,   , 0 .  '
     O**************************************************************
     O* Page 1 - NOTE FOR CLAIM OVER STATE ACCIDENT LIMIT
     O**************************************************************
     O          E            EXNOTE      2  1
     O                                           30 '* CLAIM EXCEEDS STATE'
     O                                           48 'ACCIDENT LIMIT OF'
     O                       XXFAMT              67 '  ,   ,   , 0 .  '
     O                                           69 '.'
     O          E            EXNOTE         2
     O                                           28 'PRIMARY VALUE BASED'
     O                                           39 'ON ACTUAL.'
     O**************************************************************
     O* Page 2 - Expected Losses - Heading
     O**************************************************************
     O          E            EXHD           0
     O                                           16 'EXPECTED LOSSES'
     O          E            EXHD           1
     O                                           16 'EXPECTED LOSSES'
     O          E            EXHD           2
     O                                            7 'CLASS'
     O                                           15 'MAN RT'
     C*** BGN CHG  LJQ  9 -19-91 **********************************
     O                                           25 'EFF DT'
     C*** END CHG  LJQ  9 -19-91 **********************************
     O                                           44 'PAYROLL'
     O                                           53 'EL RATE'
     O                                           71 'EXP LOSSES'
     O                                           79 'D RATIO'
     O                                           96 'PRIMARY'
     O                                          115 'EXP EXCESS'
     O**************************************************************
     O* Page 2 - Expected Losses - Detail line
     O**************************************************************
     O          E            EXDT           1
     O                       XCLS          Z      7
      *** BEG CHG **** 10/18/02 ***********************************************
     O************************ MNRT      15 '0.    '
     O                       MNRT                16 '0  .    '
      *** END CHG **** 10/18/02 ***********************************************
     O                       RDAT                25 '  /  /  '
     O                       PREM                44 '  ,   ,   , 0 .  '
      *** BEG CHG **** 10/18/02 ***********************************************
     O************************ ELRT      52 '0.    '
     O                       ELRT                53 '0  .    '
      *** END CHG **** 10/18/02 ***********************************************
     O                       EXLS                71 '  ,   ,   , 0 .  '
     O                       DRATO               77 '0.  '
     O                       PRIM                96 '  ,   ,   , 0 .  '
     O                       EXEX               115 '  ,   ,   , 0 .  '
     O**************************************************************
     O* Page 2 - Expected Losses - Batch Year Totals
     O**************************************************************
     O          E            EXYR        1  1
     O                                           14 'POLICY DATES:'
     O                       XXFRDT              24 '  /  /  '
     O                                           26 '-'
     O                       XXTODT              35 '  /  /  '
     O          E            EXYR           2
     O                       BA(1)               44 '  ,   ,   , 0 .  '
     O                       BA(2)               71 '  ,   ,   , 0 .  '
     O                       BA(3)               96 '  ,   ,   , 0 .  '
     O                       BA(4)              115 '  ,   ,   , 0 .  '
     O**************************************************************
     O* Page 2 - Expected Losses TOTALS
     O**************************************************************
     O          E            EXTL        1  1
     O                                           23 'EXPECTED TOTAL'
     O          E            EXTL           2
     O                       EL(1)               44 '  ,   ,   , 0 .  '
     O                       EL(2)               71 '  ,   ,   , 0 .  '
     O                       EL(3)               96 '  ,   ,   , 0 .  '
     O                       EL(4)              115 '  ,   ,   , 0 .  '
     O**************************************************************
     O* Page 3 - Experience mod Total Page
     O**************************************************************
     O          E            EXPM           0
     O                                           24 'EXPERIENCE MODIFICATION'
     O          E            EXPM           3
     O                                           24 'EXPERIENCE MODIFICATION'
     O          E            EXPM           2
     O                                           17 'PRIMARY ACTUAL:'
     O                       TPAL                47 '  ,   ,   , 0 .  '
     O                                           68 'EXPECTED LOSSES:'
     O                       TEXLS               89 '  ,   ,   , 0 .  '
     O          E            EXPM           2
     O                                           20 'STABILIZING VALUE:'
     O                       TBALL               47 '  ,   ,   , 0 .  '
     O                                           70 'STABILIZING VALUE:'
     O                       TBALL               89 '  ,   ,   , 0 .  '
     O          E            EXPM           2
     O                                           16 'ACTUAL EXCESS:'
     O                       TWVWV               27 '0 .  '
     O                       TAELW               47 '  ,   ,   , 0 .  '
     O          E            EXPM           2
     O                                           18 'EXPECTED EXCESS:'
     O                       ZZWVWV              27 '0 .  '
     O                       TEXEW               47 '  ,   ,   , 0 .  '
     O          E            EXPM           2
     O                                           17 'RATABLE LOSSES:'
     O                       TNUM                47 '  ,   ,   , 0 .  '
     O                       TDENM               89 '  ,   ,   , 0 .  '
     O          E            EXPM           2
     O                                           26 'EXPERIENCE MODIFICATION:'
      *** Beg Chg ***** 06/19/24 ***************************************
     O***                    MOD                 47 '0 .  '
     O                       MOD                 47 '0 .   '
      *** End Chg ***** 06/19/24 ***************************************
     O          E            EXPM           2
     O                       NOTE                75
     O*** 09/17/06  BGN ADD ***************************
     O****    E  2             EXPM
     O****                               26 'EXPERIENCE MODIFICATION '
     O****                               33 '(GERT):'
     O****                     GMOD      47 '0 .  '
     O*** 09/17/06  END ADD ***************************
     O*** 08/09/10  BGN ADD ***************************
     O          E            EXPM           2
     O                                           26 'EXPERIENCE MODIFICATION '
     O                                           33 '(GERT):'
      *** Beg Chg ***** 06/19/24 ***************************************
     O***                    GMOD                47 '0 .  '
     O                       GMOD                47 '0 .   '
      *** End Chg ***** 06/19/24 ***************************************
     O*** 08/09/10  END ADD ***************************
     O          E            EXPM        2  2
     O                                           17 'TWO YEAR MANUAL'
     O                                           26 'PREMIUM:'
     O                       TM2YR               47 '  ,   ,   , 0 .  '
     O                                           61 'PRO-RATA:'
     O                       M2RT                67 '0 .  '
     O          E            EXPM           2
     O                                           23 'TOTAL MANUAL PREMIUM:'
     O                       TMALY               47 '  ,   ,   , 0 .  '
     O                                           61 'PRO-RATA:'
     O                       MALR                67 '0 .  '
     O*
     O          E            MA1            2
     O                                           23 'WEIGHTED TEST RATIO: '
     O                       RVAL                47 '0 .  '
     O*
     O          E            MA1            2
     O                                           23 'SURCHARGE FACTOR:    '
     O                       SURF                47 '0 .  '
     O* MESSAGES FOR PRINTING EXPERIENCE MOD
**
THIS EMPLOYER DOES NOT MEET MINIMUM PREMIUM REQUIREMENTS.
THE SPECIAL RULE TO ADJUST EXPERIENCE MODIFIERS HAS BEEN INVOKED.
