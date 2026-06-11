     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7009R2 - Supervisors EOMB Pend Report - Output Workfile
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM BUILDS THE WORKFILE FOR THE SUPERVISORS    **
     H**     EOMB PEND REPORT, WHICH HAVE A TRANS PAYMENTS OLDER     **
     H**     THAN  X  NUMBER OF DAYS ENTERED IN W7008R.              **
     H**                                                             **
     H**  CREATED: 03/10/95  C:28  PROJ:68AEZ                         *
     H**                                                             **
     H**   CALLED FROM: W7009C1                                      **
     H**   CALLS TO:    NONE                                         **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/25/98 C:94 PROJECT: 48MBV                                 *
     H*                MAKE PGM YR2000 COMPLIANT                      *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     11   -      CONDITION FIRST TIME THROUGH PROGRAM          *
     H*     60   -      GENERAL CHAINING                              *
     H*****************************************************************
     H** FILE USAGE:                                                  *
     H*                                                               *
     H*     WMCML1 (PCM01) CO#/FND/FYR/CASE                           *
     H*     WARPL8 (PRP01) CO#/TRDT            SELECT- STAT='P'       *
     H*     SMCOL (PCO01): CO#                                        *
     H*     SMORL  (POR01) ORPNAM,ORFIL#                              *
     H*****************************************************************
     H*
     FWMCML1    IF   E           K DISK
     F* * * Claims File
     F*
     FWARPL8    IF   E           K DISK
     F* * * Request Payment Trans File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWXRLL     IF A E           K DISK
     F* * * Work File Output
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D**
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*         *LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    RLSORT        XXSORT
     C     *LIKE         DEFINE    RLSEQ#        XXSEQ#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  REQUEST PAYMENT FILE - PARTIAL
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  WORKFILE
     C*
     C     RLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSORT
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEG CHG *** 11/25/98 ***************************************
     C                   PARM                    QQDTE             7
     C*********            PARM           QQDTE   6
     C*** END CHG *** 11/25/98 ***************************************
     C                   PARM                    QQCDE             2
     C                   PARM                    QDAYS             5
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
     C                   MOVE      QQCO#         XXCO#
     C*** BEG CHG *** 11/25/98 ****************************************
     C                   MOVE      QQDTE         XXMDY             7 0
     C                   Z-ADD     XXMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXYMD             8 0
     C**********           MOVE QQDTE     XXMDY   60
     C**********           Z-ADDXXMDY     SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXYMD   80
     C**********           MOVELXXYMD     XXYYMM  50
     C**********           MOVELSRDTO     XXYMD   70
     C**********           MOVELXXYMD     XXYYMM  40
     C*** END CHG *** 11/25/98 ***************************************
     C                   MOVE      QDAYS         DAYS              5 0
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  READ PAYMENTS REQUEST FILE AS PRIMARY
     C*
     C     RPKY          CHAIN     PRP01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*   VERIFY THAT THE RECORD IS A PENDED FSMS RECORD FOR THE
     C*    DATE KEYED ON THE PROMPT SCREEN.
     C*
     C     RPEOB#        IFNE      0
     C     RPSTAT        ANDEQ     'P'
     C*
     C     QQCDE         IFNE      *BLANKS
     C     RPSTS         IFNE      QQCDE
     C                   GOTO      NEXT
     C                   END
     C                   END
     C*
     C**    CALL PGM TO CALCULATE DAYS SINCE PAYMENT WAS PENDING
     C**
     C     RPTRDT        IFNE      *ZERO
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         RPMDY             8 0
     C*** BEG CHG *** 11/25/98 ***************************************
     C                   Z-ADD     RPMDY         QQDOB             8 0
     C**********           MOVE RPMDY     QQDOB   6
     C*** END CHG *** 11/25/98 ***************************************
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      ' '           QQERR             1
     C*** BEG DEL *** 11/25/98 ***************************************
     C**********           MOVE '1'       QQTERM  1
     C*** END DEL *** 11/25/98 ***************************************
     C*** BEG CHG *** 11/25/98 ***************************************
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQDOB
     C                   PARM                    XXYMD
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C**********           CALL 'SRDDIF'
     C**********           PARM           QQDOB
     C**********           PARM           QQDTE
     C**********           PARM           QQDAYS
     C**********           PARM           QQERR
     C**********           PARM           QQTERM
     C*** END CHG *** 11/25/98 ***************************************
     C                   MOVE      QQDAYS        XXDAYS            5 0
     C                   END
     C     XXDAYS        IFGE      DAYS
     C*
     C*  READ CLAIM FILE
     C*
     C                   Z-ADD     RPFND         XXFND
     C                   Z-ADD     RPFYR         XXFYR
     C                   Z-ADD     RPCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  WRITE THE WORKFILE RECORD
     C*
     C                   MOVEL     CMSUPR        XXSORT
     C                   MOVEL     RPSEQ#        XXSEQ#
     C     RLKY          CHAIN     PRL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      XXCO#         RLCO#
     C                   MOVE      XXFND         RLFND
     C                   MOVE      XXFYR         RLFYR
     C                   MOVE      XXSORT        RLSORT
     C                   MOVE      XXCASE        RLCASE
     C                   MOVE      XXSEQ#        RLSEQ#
     C                   MOVE      RPTRDT        RLTRDT
     C                   MOVE      RPAMT         RLAMT
     C                   MOVE      RPBR2         RLBR2
     C                   MOVE      RPBR7         RLBR7
     C                   MOVE      RPFRDT        RLFRDT
     C                   MOVE      RPTODT        RLTODT
     C                   MOVE      RPTRCD        RLTRCD
     C                   MOVE      RPTYPE        RLTYPE
     C                   MOVE      RPRETP        RLRETP
     C                   MOVE      RPRECD        RLRECD
     C                   MOVE      RPPNAM        RLPNAM
     C                   MOVE      RPSTS         RLSTS
     C                   MOVE      RPSENT        RLSENT
     C                   MOVE      RPDSEQ        RLDSEQ
     C                   MOVE      RPPGTP        RLPGTP
     C                   MOVE      RPRQST        RLRQST
     C                   MOVE      RPLTCD        RLLTCD
     C                   WRITE     PRL01
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     NEXT          TAG
     C     RPKY          READE     PRP01                                  60
     C                   END
     C*
     C**
     C                   MOVE      '1'           *INLR
     C**
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKY          CHAIN     SMCOL                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*
