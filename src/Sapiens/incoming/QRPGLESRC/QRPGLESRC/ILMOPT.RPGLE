     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *  Description:                                                   *
      *
      *  This API will return the current status of the optical purge
      *  process.  Included will be last completed purge date. Date range
      *  of last purge (from/to date), Current status (C-completed,
      *  I-In process) and a return code for error(s).
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  Parms to retrieve information.
      *
      *  SEND BACK       :  See description above.
      *  - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   5/16/02  C:02  Project:                             *
      *                                                                 *
      * *****************************************************************
     H** REVISIONS:
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**                  01/26/04    THE OPTICAL LIBRARY NAME
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     F*
     FSMOCLA    IF   E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************
      *

     D EMSG            S             79    DIM(1) CTDATA PERRCD(1)
      *
     D QQFILL          DS
     D  XXFIL1                 1    150
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D IMGPRM          DS
     D  QQLCD                  1      8
     D  QQLFD                  9     16
     D  QQLTD                 17     24
     D  QQSTAT                25     25
     D  QQRTCD                26     27
     D  QQEMSG                28    106
     D  QQVOLI               107    112
     D  QQOFLD               113    120
     D  QQOSBD               121    132
     D  QQFIL1               133    256
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     I*
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
     C*
     C*   KEY LIST FOR
     C*
      *
     C***        *LIKE     DEFN XXXX      XXXX
     C*
      * ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    IMGPRM
     C****************     PARM           QQLCD   8        01
     C***                  PARM           QQLFD   8        02
     C***                  PARM           QQLTD   8        03
     C***                  PARM           QQSTAT  1        04
     C***                  PARM           QQRTCD  2        05
     C***                  PARM           QQEMSG 79        06
     C***                  PARM           QQFILL150        07
      *-------------------------------------------------------------------
     C***        Q1PRMS    IFGT 15
      *-------------------------------------------------------------------
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *
     C     DMDY82        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*** 01/26/04 ** END ADD *************************************8
     C*
     C*** DEL 1/26/04 ***  MOVE 'G'       QQOSYS  1
     C*** DEL 1/26/04 ***  MOVEL'OPTMLB01'QQLIBN 10
     C*
      *
      * GET TODAY'S DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XDCYMD            7 0             dt in cymd
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XDMDY8           80            Dt in MDY8
     C*
      *
     C     OCKY          CHAIN     POC01                              62
     C     *IN62         IFEQ      *OFF
     C*
     C*** convert and put OCLDEL into the QQLCD field
     C*** Convert and put OCPFRM into the QQLFD, and OCPTO into QQLTD
     C*** If ALL flags are "Y" (OCDELF, OCSELF, OCCPYF) then the process is
     C*** in COMPLETED STATUS (QQSTAT = "C") else QQSTAT = I (in-Process).
     C*
     C                   Z-ADD     OCLDEL        SRDTI
     C                   CALL      'SRCVTDAT'    DMDY82
     C                   MOVEL     SRDTO         QQLCD                          Dt in MDY8
     C*
     C*
     C                   Z-ADD     OCPFRM        SRDTI
     C                   CALL      'SRCVTDAT'    DMDY82
     C                   MOVEL     SRDTO         QQLFD                          Dt in MDY8
     C*
     C*
     C                   Z-ADD     OCPTO         SRDTI
     C                   CALL      'SRCVTDAT'    DMDY82
     C                   MOVEL     SRDTO         QQLTD                          Dt in MDY8
     C*
     C     OCDELF        IFEQ      'Y'
     C     OCSELF        ANDEQ     'Y'
     C     OCCPYF        ANDEQ     'Y'
     C                   MOVEL     'C'           QQSTAT
     C                   ELSE
     C                   MOVEL     'I'           QQSTAT
     C                   ENDIF
     C*
     C*
     C                   MOVEL     OCVOLI        QQVOLI
     C                   MOVEL     OCOFLD        QQOFLD
     C                   MOVEL     OCOSBD        QQOSBD
     C*
     C***
     C                   ELSE
     C*** Error Occured, No OC record.  Put A RTNCD Value ("01") and
     C*** Error Message into QQERMS....
      *
     C                   MOVEA     EMSG(1)       QQEMSG
     C                   MOVEL     '01'          QQRTCD
     C*
     C                   ENDIF
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
** EMSG
Optical Control Record was not found.        RtnCode-01
