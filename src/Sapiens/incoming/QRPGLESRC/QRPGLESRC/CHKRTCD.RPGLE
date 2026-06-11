     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CHKRTCD - LIST ALL POLICIES BY STATE
     H*****************************************************************
     H**                                                              *
     H** PROGRAM DESCRIPTION:                                         *
     H**    THIS PROGRAM WILL LIST HOW MANY POLICIES ARE ACTIVE AND   *
     H**    INACTIVE IN EACH STATE.                                   *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    BATCH                                                     *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**                                                              *
     H** PROGRAMMER: C:65                                             *
     H** CREATED:    08/17/99         PROJECT: PDS                    *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  7/16/10 C:09  PROJECT SUPP / SEQ # 70414                     *
     H*                Correct program logic to use the correct date  *
     H*                format when comparing dates.  Also, check      *
     H*                Cancellation status based upon the Monthend    *
     H*                Date.  Add some program comments regarding     *
     H*                ELFAF = ' '.
     H*                                                               *
     H* 10/30/09 C:09  PROJECT SUPP / SEQ # 70414                     *
     H*                Apply program updates to properly calculate    *
     H*                policy count and policy premium.               *
     H*                                                               *
     H* 05/20/03 C:132 PROJECT WC577                                  *
     H*                ADD Premium Total Amounts to report            *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
      ***** BEG ADD ***** 10/30/09 **************************************
     FWMFNL     IF   E           K DISK
      ***** BEG ADD ***** 10/30/09 **************************************
     FWDELL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWXR9P     O    E             DISK
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOF)
      *****************************************************************

     D*
     D ST              S              2    DIM(54) CTDATA PERRCD(27)
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D STTOTS          DS                  OCCURS(54)
     D  TAST                   1      2
     D  TOTA                   3     13  0
     D  TOTI                  14     24  0
      ***** BEG ADD ***** 05/20/03 **************************************
     D  TOTAPR                25     39  2
     D  TOTIPR                40     54  2
      ***** END ADD ***** 05/20/03 **************************************

     C*****************************************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      ***** BEG ADD ***** 10/30/09 **************************************
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
      ***** END ADD ***** 10/30/09 **************************************
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
      ***** BEG ADD ***** 10/30/09 **************************************
     C                   KFLD                    FNFND
      ***** END ADD ***** 10/30/09 **************************************
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
      ***** BEG ADD ***** 10/30/09 **************************************
     C     *LIKE         DEFINE    ELPTDT        SVPTDT
     C     *LIKE         DEFINE    ELPTDT        XXMED2
      ***** END ADD ***** 10/30/09 **************************************
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCLT             3
     C                   PARM                    QQCO#             3
      ***** BEG ADD ***** 10/30/09 **************************************
     C                   PARM                    QQMEDT            6
      ***** END ADD ***** 10/30/09 **************************************
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
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
      ***** BEG ADD ***** 10/30/09 **************************************
     C                   MOVE      QQMEDT        XXMEDT            6 0
      ***** END ADD ***** 10/30/09 **************************************
     C*
      *  INITIALIZE FIELDS BASED ON *ENTRY PARMS
      *
     C                   MOVE      QQCO#         XXCO#
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   TIME                    DSDTTM
     C*
      ***** BEG ADD ***** 10/30/09 **************************************
     C                   Z-ADD     XXMEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXMED8            8 0
     C                   Z-ADD     XXMED8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMED2
      ***** END ADD ***** 10/30/09 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C     1             DO        54            W                 2 0
     C     W             OCCUR     STTOTS
     C                   MOVE      ST(W)         TAST
     C                   Z-ADD     0             TOTA
     C                   Z-ADD     0             TOTI
      ***** BEG ADD ***** 05/20/03 **************************************
     C                   Z-ADD     0             TOTAPR
     C                   Z-ADD     0             TOTIPR
      ***** END ADD ***** 05/20/03 **************************************
     C                   ENDDO
     C*
      ***** BEG ADD ***** 10/30/09 **************************************
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         DOWEQ     *OFF
     C     FNTEST        IFEQ      'Y'
     C                   GOTO      NXTFND
     C                   ENDIF
      ***** END ADD ***** 10/30/09 **************************************
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C*
      ***** BEG ADD ***** 10/30/09 **************************************
     C                   Z-ADD     *ZEROS        SVPTDT
     C     ELKY          SETGT     PEL01
     C     NXTEL         TAG
     C     ELKY          READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPTDT        SVPTDT
      *
      * A policy can indicate a status of "Not billed" (ELFAF = Blank) when
      * it is rolled from Quote to Live -or- when a Renewal is run.
      * The Renewal program clears all premium figures, therefore, it
      * would be misleading to have a count and no premium.  The Quote Transfer
      * sets the status to "Not Billed" but does not clear the Quote Premium.
      * Once again, Quote Premium is not a "final" premium figure in Live so
      * we should skip "Not Billed" policy years
      *
     C     ELFAF         IFEQ      ' '
     C                   GOTO      NXTEL
     C                   ENDIF
      ***** END ADD ***** 10/30/09 **************************************
     C*
      *  CHECK IF EMPLOYER IS ACTIVE
      *
     C                   MOVE      *OFF          *IN42
     C                   Z-ADD     5             X                 3 0
     C                   DO        5
     C     EF(X)         IFNE      *ZEROS
     C     CX(X)         IFNE      *ZEROS
      ***** BEG CHG ***** 07/16/10 **************************************
     C***********CX,X******ANDLEXXDATE
     C     CX(X)         ANDLE     XXMED2
      ***** END CHG ***** 07/16/10 **************************************
     C                   MOVE      *ON           *IN42
     C                   LEAVE
     C                   ENDIF                                                  CX,X IFNE
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  EF,X IFNE
     C                   ENDDO                                                  DO 5
      *
      ***** BEG ADD ***** 05/20/03 **************************************
     C     ELFAF         IFEQ      'A'                                          AUDITED ?
     C                   Z-ADD     ELAAPR        XXPREM           11 2          AUDIT PREM
     C                   ELSE
     C                   Z-ADD     ELANPR        XXPREM                         AUDIT PREM
     C                   ENDIF
      ***** END ADD ***** 05/20/03 **************************************
     C*
      ***** BEG CHG ***** 10/30/09 **************************************
     C***********ELPTDT    IFLT XXDATE
      ***** BEG CHG ***** 07/16/10 **************************************
     C***********SVPTDT****IFLT XXMEDT
     C     SVPTDT        IFLT      XXMED2
      ***** END CHG ***** 07/16/10 **************************************
      ***** END CHG ***** 10/30/09 **************************************
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST              2
     C*
     C                   EXSR      PLACE
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     NXTFND        TAG
     C     FNKY          READE     PFN01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      PRINT
     C*
     C                   MOVE      *ON           *INLR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO PRINT THE BILL AND LIST EMPLOYER ON REPORT.
      *
     C     PRINT         BEGSR
     C*
     C                   Z-ADD     0             XTI              11 0
     C                   Z-ADD     0             XTA              11 0
     C*
     C                   EXCEPT    HEAD
     C*
     C     1             DO        54            W                 2 0
     C     W             OCCUR     STTOTS
     C*
     C     TOTI          IFNE      *ZERO
     C     TOTA          ORNE      *ZERO
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
      ***** BEG ADD ***** 05/20/03 **************************************
      *  ACCUMULATE ACTIVE AND INACTIVE PREMIUM
     C                   ADD       TOTIPR        XTIPRM           15 2
     C                   ADD       TOTAPR        XTAPRM           15 2
     C     TOTIPR        ADD       TOTAPR        TOTALS           15 2
      ***** END ADD ***** 05/20/03 **************************************
     C                   ADD       TOTI          XTI
     C                   ADD       TOTA          XTA
     C     TOTI          ADD       TOTA          TOTAL            11 0
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  TOTI IFNE
     C*
     C                   ENDDO
     C*
      ***** BEG ADD ***** 05/20/03 **************************************
     C     XTIPRM        ADD       XTAPRM        TOTALG           15 2
      ***** END ADD ***** 05/20/03 **************************************
     C     XTI           ADD       XTA           TOTAL
     C                   EXCEPT    DET2
     C*
     C                   ENDSR                                                  PRINT SR
     C*
      *****************************************************************
      *
      *  SUBROUTINE TO PLACE THE POLICY INTO THE PROPER STATE COUNT.
      *
     C     PLACE         BEGSR
     C*
     C     1             DO        54            W
     C     W             OCCUR     STTOTS
     C     TAST          IFEQ      XXST
     C                   LEAVE
     C                   ENDIF                                                  TAST IFEQ
     C                   ENDDO                                                  DO W:1-52
     C*
     C     *IN42         IFEQ      *ON
     C                   ADD       1             TOTI
      ***** BEG ADD ***** 05/20/03 **************************************
     C                   ADD       XXPREM        TOTIPR           15 2
      ***** END ADD ***** 05/20/03 **************************************
     C                   ELSE
     C                   ADD       1             TOTA
      ***** BEG ADD ***** 05/20/03 **************************************
     C                   ADD       XXPREM        TOTAPR           15 2
      ***** END ADD ***** 05/20/03 **************************************
     C                   ENDIF
     C*
     C                   CLEAR                   PR901
     C*
     C                   Z-ADD     ELCO#         R9CO#
     C                   Z-ADD     ELFND         R9FND
     C                   Z-ADD     ELFYR         R9FYR
     C                   Z-ADD     ELEMP#        R9EMP#
     C                   Z-ADD     ELDIV         R9DIV
     C                   Z-ADD     XXPREM        R9PREM
     C                   MOVE      TAST          R9ST
     C     *IN42         IFEQ      *ON
     C                   MOVE      'I'           R9STAT
     C                   ELSE
     C                   MOVE      'A'           R9STAT
     C                   ENDIF
     C*
     C                   WRITE     PR901
     C*
     C                   ENDSR                                                  PLACE SR
     C*
      *****************************************************************
     C*
     OQSYSPRT2  E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           66 'CLIENT:'
     O                       QQCLT               71
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      ***** BEG ADD ***** 05/20/03 **************************************
      * ADDED NEW HEADINGS, STATE PREMIUM TOTALS & FINAL TOTAL PREMIUM
      ***** END ADD ***** 05/20/03 **************************************
     O          E            HEAD           1
     O                                           21 'ACTIVE'
     O                                           41 'ACTIVE'
     O                                           67 'INACTIVE'
     O                                           87 'INACTIVE'
     O                                          100 'TOTAL'
     O                                          121 'TOTAL'
     O          E            HEAD           2
     O                                           07 'STATE'
     O                                           22 'POLICIES'
     O                                           42 'PREMIUM'
     O                                           67 'POLICIES'
     O                                           86 'PREMIUM'
     O                                          102 'POLICIES'
     O                                          122 'PREMIUM'
     O          E            DET            1
     O                       TAST                05
     O                       TOTA          1     22
     O                       TOTAPR        1B    42
     O                       TOTI          1     67
     O                       TOTIPR        1B    87
     O                       TOTAL         1    102
     O                       TOTALS        1B   122
     O          E            DET2        1  1
     O                                           08 'TOTAL:'
     O                       XTA           1     22
     O                       XTAPRM        1     42
     O                       XTI           1     67
     O                       XTIPRM        1     87
     O                       TOTAL         1    102
     O                       TOTALG        1    122
     O*
**  ST TABLE
AKALARAZCACOCTDCDEFLGAHIIAIDILINKSKYLAMAMDMEMIMNMOMSMT
NCNDNENHNJNMNVNYOHOKORPAPRRISCSDTNTXUTVAVIVTWAWIWVWYZZ
