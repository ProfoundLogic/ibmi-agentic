     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8612R - Days Lost - Update Program
     H*
     H*****************************************************************
     H*
     H* CREATED BY:   C58       New for V3
     H* PROJECT:      WC739     08/15/97
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/05/05 C:15 Support: WC082/43373                            *
     H*               Chgd code to load the batch date into CMLSDT.   *
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H* 04/18/02 C:15  Support: WC065/31816                           *
     H*                Added code to clear XXLOST to prevent the pgm  *
     H*                from calculating incorrect numbers.            *
     H*                                                               *
     H* 03/07/00 C:15  Support: WC048/21944                           *
     H*                Chgd code to calculate the correct # of comp   *
     H*                days paid if more than one pmt is made on a    *
     H*                claim for a particular day.                    *
     H*****************************************************************
     H* INDICATOR USAGE:
     H*
     H*        60  Payment detail: end-of-file and loop control
     H*        61  Lost time workfile: record not found
     H*        62  Claim master: record not found
     H*
     H*        65  Lost time workfile: EOF/loop control
     H*
     H*
     H*
     H*****************************************************************
     F*       Files
     F*________________________________________________________________
     F*      Payment Detail File - by transaction date
     FWDPAL9    IF   E           K DISK
     F*________________________________________________________________
     F*      Lost days work file - Co, fund FYR, case
     FWXLDL     UF A E           K DISK
     F*________________________________________________________________
     F*      Claim master file - Co, fund FYR, case
     FWMCML1    UF   E           K DISK
     F*________________________________________________________________
     F*      User parameters
     FWMUPL     IF   E           K DISK

     D*****************************************************************
     D*________________________________________________________________
     D*      Tables and Arrays
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*

     C*________________________________________________________________
     C*================================================================
     C*
     C*          MAINLINE
     C*
     C*================================================================
     C*   Start reading the payment detail
     C     PAKY          CHAIN     PPA01                              60
     C*
     C*   Process all the compensation payments
     C     *IN60         DOWEQ     *OFF
     C     PARETP        IFEQ      'C'
     C*
     C*   Determine if already in the lost time workfile
     C                   MOVE      PAFND         XXFND
     C                   MOVE      PAFYR         XXFYR
     C                   MOVE      PACASE        XXCASE
     C     LDKY          CHAIN     PLD01                              61
     C*
     C*   If not, check claim master date of 'lost time'
     C*
     C     CMKY          CHAIN(N)  PCM01                              62
     C     *IN61         IFEQ      *ON
     C*
     C*   Claim was updated, skip it
     C     CMLSDT        CABEQ     XXBDAT        SKIP                            Done - skip
     C*
     C*   Not updated, build workfile record
     C                   EXSR      SRBLDW                                        Build new
     C*
     C                   ENDIF                                                   61 = *ON
     C*
     C*   Update workfile fields
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C*
     C*   Calculate days lost
     C                   EXSR      SRDAYS
     C*
     C*   Put workfile record
     C     *IN61         IFEQ      *ON
     C                   WRITE     PLD01
     C                   ELSE                                                    X 61 = *ON
     C                   UPDATE    PLD01
     C                   ENDIF                                                   61 = *ON
     C*
     C                   ENDIF                                                  PARETP = C
     C*
     C*   Read next payment
     C     SKIP          TAG
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C*
     C*   Read the workfile to update all claim master records.
     C                   EXSR      SRUCLM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C/EJECT
     C**************************************************************************
     C*   SRDAYS - Calculate workfile days lost -- LDLOST
     C**************************************************************************
     C*
     C     SRDAYS        BEGSR
     C*
     C*** Begin add *** 04/18/02 **************************************
     C                   Z-ADD     0             XXLOST
     C*** End add ***** 04/18/02 **************************************
     C*
     C*    Calculate days in weeks
     C     PAWKS         IFNE      *ZERO
     C     CMDAYS        IFNE      *ZERO
     C     PAWKS         MULT      CMDAYS        XXLOST
     C                   ELSE
     C     PAWKS         MULT      7             XXLOST
     C                   ENDIF                                                   CMDAYS ne 0
     C                   ENDIF                                                   PAWKS ne 0
     C*
     C*    Add days
     C                   ADD       PADAYS        XXLOST
     C*
     C*    Post according to Transaction
     C     PATYPE        IFEQ      'A'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'P'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     *ZERO
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
     C*** BEGIN CHG *** 03/07/00 **************************************
     C******************** Z-ADDXXLOST    LDLOST
     C                   ADD       XXLOST        LDLOST
     C*** END CHG ***** 03/07/00 **************************************
     C*
     C                   ELSE                                                   x TYPE = ...
     C*
     C*** BEGIN CHG *** 03/07/00 **************************************
     C******************** Z-SUBXXLOST    LDLOST
     C                   SUB       XXLOST        LDLOST
     C*** END CHG ***** 03/07/00 **************************************
     C*
     C                   ENDIF                                                  PATYPE = ...
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C*   SRUCLM - Update the claim master.
     C**************************************************************************
     C     SRUCLM        BEGSR
     C*
     C     LDKY2         SETLL     PLD01
     C     LDKY2         READE     PLD01                                  65
     C*
     C*   Process the whole file
     C     *IN65         DOWEQ     *OFF
     C*
     C*   Get Claim master
     C                   MOVE      LDFND         XXFND
     C                   MOVE      LDFYR         XXFYR
     C                   MOVE      LDCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              62
     C*
     C*   Update claim master
     C     *IN62         IFEQ      *OFF
     C                   ADD       LDLOST        CMLOST
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*** Begin chg *** 10/05/05 *************************************
     C******************** MOVELSRDTO     CMLSDT
     C                   MOVE      XXBDAT        CMLSDT
     C*** End chg ***** 10/05/05 *************************************
     C*
     C                   UPDATE    PCM01
     C                   ENDIF                                                    *IN62=OFF
     C*
     C*   Read next work record
     C     LDKY2         READE     PLD01                                  65
     C*
     C                   ENDDO                                                    *IN65 DOW
     C*
     C     ENUCLM        ENDSR
     C*
     C**************************************************************************
     C*   SRBLDW - Build new workfile record
     C**************************************************************************
     C     SRBLDW        BEGSR
     C*
     C                   Z-ADD     PACO#         LDCO#
     C                   Z-ADD     PAFND         LDFND
     C                   Z-ADD     PAFYR         LDFYR
     C                   Z-ADD     PACASE        LDCASE
     C*
     C                   MOVEL     Q1PGM         LDIPGM
     C                   MOVEL     Q1USER        LDIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LDITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDIDT
     C*
     C                   Z-ADD     *ZERO         LDLOST
     C*
     C     ENBLDW        ENDSR
     C*
     C**************************************************************************
     C*   Program Initialization - *INZSR
     C**************************************************************************
     C     *INZSR        BEGSR
     C*
     C*** Define Parameter Lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQBDAT            8
     C                   PARM                    QQRTCD            1
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
     C*________________________________________________________________
     C*   Define Key Lists
     C*________________________________________________________________
     C*          WMUPL - User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*________________________________________________________________
     C*          WMCML - Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*________________________________________________________________
     C*          WDPAL9 - Payment Detail File
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTRDT
     C*________________________________________________________________
     C*          WXLDL  - Lost time workfile
     C*
     C     LDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*________________________________________________________________
     C*          WXLDL  - Lost time workfile to reread
     C*
     C     LDKY2         KLIST
     C                   KFLD                    XXCO#
     C*________________________________________________________________
     C*   Define work fields
     C*
     C     *LIKE         DEFINE    QQCALL        QSCALL
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C     *LIKE         DEFINE    PATRDT        XXBDAT
     C     *LIKE         DEFINE    CMLOST        XXLOST
     C*
     C     *LIKE         DEFINE    Q1PGM         QQPGM
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    Q1JOBN        QQJOB#
     C     *LIKE         DEFINE    Q1USER        QQUSER
     C     *LIKE         DEFINE    Q1JOB         QQJOB
     C*
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      QQCO#         XXCO#
     C*
     C*   Convert batch date to CYYMMDD
     C                   MOVE      QQBDAT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBDAT
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C*   Retrieve the User Parameter File
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   ENDSR
