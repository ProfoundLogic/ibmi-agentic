     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H**
     H/TITLE E8020R - ROLL WORK COMP MANUAL RATES TO EXP MOD FILE
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM WILL READ WORK COMP PAYROLL RATE FILE AND  **
     H**     ROLL THE MANUAL RATES TO THE EXP MOD MANUAL RATE FILE   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E8020C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: LORI J. QUINN                                 **
     H**   CREATED:    08/13/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**  06/17/24  C:344 Project: WC2952                            **
     H**            Increase Rate field to zone 8.3.                 **
     H**                                                             **
     H**  10/12/99  C:03  PROJECT:  SUPPORT                          **
     H**            CHANGED LOGICAL TO LOAD RATES BASED ON RATE CODE **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT:  WC853                            **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add field Rate Set Code.  Also, the pgm now will **
     H**            rollover all or 1 Rate Set Code based on PARM.   **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**     *IN                                                     **
     H**      08     DEMAND MODE                                     **
     H**      09     BATCH  MODE                                     **
     H**      50     DOWEQ                                           **
     H**      60     NO HIT ON FILE                                  **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    EMMRL1(PWB01): CO/RTCD/CLASS/EFF DATE                   **
     H**    WMPRL1   (PPR01): CO/RTCD/CLASS/EFF DATE                 **
     H*****************************************************************
     H*
     F*
     F* Company Master File
     F*
     FSMCOL     IF   E           K DISK
     F*
     F* Manual Rate Master File
     F***** BGN CHG 10/12/99 *************************************
     F**EMMRL   UF  E           K        DISK                      A
     FEMMRL1    UF A E           K DISK
     F***** END CHG 10/12/99 *************************************
     F*
     F* WC Payroll Class File
     FWMPRL1    IF   E           K DISK

     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST MANUAL RATE FILE
     C*
     C     MRKY          KLIST
     C                   KFLD                    XXCO#
     F***** BGN CHG 10/12/99 *************************************
     C*****************    KFLD           XXST
     C                   KFLD                    XXRTCD
     F***** END CHG 10/12/99 *************************************
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C*   WORK COMP PAYROLL CLASS CODE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQRSC
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQRSC             3
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
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    MRCO#         XXCO#
     C     *LIKE         DEFINE    MRST          XXST
     C     *LIKE         DEFINE    MRCLAS        XXCLAS
     C     *LIKE         DEFINE    MREFDT        XXEFDT
     C     *LIKE         DEFINE    MRRATE        XXRATE
     C     *LIKE         DEFINE    MRDRSC        XXDRSC
     F***** BGN CHG 10/12/99 *************************************
     C     *LIKE         DEFINE    MRDRSC        XXRTCD
     F***** END CHG 10/12/99 *************************************
     C*
     C* SET UP AND INITIALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C     QQRSC         IFEQ      *BLANK
     C                   MOVE      *ON           *IN55                          READ ALL RSC
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C****   READ WORK COMP PAYROLL CLASS FILE
     C*
     C     PRKY          SETLL     PPR01
     C*
     C   55              READ      PPR01                                  50
     C  N55PRKY          READE     PPR01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* CHECK EXP MOD MANUAL RATE FILE; IF IT ALREADY EXISTS-UPDATE;
     C*   ELSE WRITE NEW RECORD
     C*
     C                   MOVE      COST          XXST                           STATE
     C                   Z-ADD     PRCLAS        XXCLAS                         CLASS CODE
     C                   Z-ADD     PREFDT        XXEFDT
     F***** BGN CHG 10/12/99 *************************************
     C                   MOVEL     PRRTCD        XXRTCD
     F***** END CHG 10/12/99 *************************************
     C                   TIME                    DSDTTM
     C*
     C     MRKY          CHAIN     PMR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*** Beg Chg *** 06/17/24 *********************************************
     C***  PRRT          MULT      100           XXRATE
     C***                Z-ADD     XXRATE        MRRATE
     C                   Z-Add     PRrt          MRrate
     C*** End Chg *** 06/17/24 *********************************************
     C                   MOVE      PRRTCD        MRDRSC
     C                   MOVE      Q1USER        MRUSER
     C                   MOVE      Q1PGM         MRPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C*
     C                   Z-ADD     DSTIME        MRTIME
     C*
     C                   UPDATE    PMR01
     C*
     C                   ELSE
     C*
     C***Begg Chg *** 06/17/24 *********************************************
     C***  PRRT          MULT      100           XXRATE
     C***                Z-ADD     XXRATE        MRRATE
     C                   Z-Add     PRrt          MRrate
     C*** End Chg *** 06/17/24 *********************************************
     C                   Z-ADD     XXCO#         MRCO#
     C                   MOVE      PRRTCD        MRDRSC
     C                   MOVEL     COST          MRST
     C                   Z-ADD     PRCLAS        MRCLAS
     C                   Z-ADD     PREFDT        MREFDT
     C                   MOVE      PRSTAT        MRSTAT
     C                   MOVE      Q1USER        MRIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRIDT
     C*
     C                   MOVE      Q1USER        MRUSER
     C                   MOVE      Q1PGM         MRPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C*
     C                   Z-ADD     DSTIME        MRTIME
     C                   WRITE     PMR01
     C                   END
     C*
     C   55              READ      PPR01                                  50
     C  N55PRKY          READE     PPR01                                  50
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
