     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H* * *  SCOCUPR
      *               CHECK AND UPDATE THE OPTICAL PROCESS INFO IN THE
      *               OPTICAL CONTROL FILE
     F*
     F*     RTN CODES:
     F*                 1 -  Can't perform Select Process
     F*                 2 -  Can't perform Copy Process
     F*                 3 -  Can't perform Delete Process
     F*
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**           01/26/04   THE OPTICAL LIBRARY NAME
     F*
     F*
     F*
     FSMOCLA    UF   E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************
      *

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     OCKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *
     C     *ENTRY        PLIST
     C                   PARM                    PROCES            1            S,C,D (SEL,CPY,DEL)
     C                   PARM                    BFRAFT            1            B,A (BEFORE,AFTER)
     C                   PARM                    RTNCDE            1
     C                   PARM                    SLFRM             8            Sel From Date ymd
     C                   PARM                    SLTO              8            Sel To Date   ymd
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
     C** CURRENT DATE/TIME
     C*
     C                   TIME                    DSDTTM
      *
     C     OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      *OFF
     C*
     C     PROCES        CASEQ     'S'           SELECT
     C     PROCES        CASEQ     'C'           COPY
     C     PROCES        CASEQ     'D'           DELETE
     C                   ENDCS
      *
      * Update the OC record with Process tracking values
     C                   UPDATE    POC01
      *
     C                   ELSE                                                   NO HIT ON SMOCP
     C*** ERROR
     C                   ENDIF                                                  END *IN62 = 0
      *
     C                   MOVE      *ON           *INLR
      ***--------------------------------------------------------------***
      *** SELECT SUBROUTINE                                            ***
      ***--------------------------------------------------------------***
      *
     C     SELECT        BEGSR
      *
     C                   SELECT
     C     BFRAFT        WHENEQ    'B'
      **
     *** FOR THE BEFORE SELECT
     ***   If Delete process is done, and copy process hasn't started
     ***   mark the flags, select has started, copy not done, Del not done.
      **
     C     OCCPYF        IFEQ      'Y'
     C     OCDELF        ANDEQ     'Y'
     C     OCSELF        ANDNE     'S'
     C     OCCPYF        OREQ      'N'
     C     OCDELF        ANDEQ     'N'
     C     OCSELF        ANDNE     'S'
     C                   MOVEL     'S'           OCSELF
     C                   MOVEL     'N'           OCCPYF
     C                   MOVEL     'N'           OCDELF
     C                   ELSE
      *** ERROR CANT RUN PROCESS
     C                   MOVEL     '1'           RTNCDE
     C                   ENDIF
      *
     C     BFRAFT        WHENEQ    'A'
      *
     C                   MOVEL     'Y'           OCSELF
      *** Today's Date as select completion Date
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OCLSEL
     C*
      *** Put From and To Date used on Selection Screen
      *
     C*
     C                   MOVE      SLFRM         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OCPFRM
     C*
     C                   MOVE      SLTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OCPTO
     C*
     C                   OTHER
     C*** ERROR IF NOT "B" OR "A"
      *
     C                   ENDSL
      *
     C     ENDSEL        ENDSR
      *
      ***--------------------------------------------------------------***
      *** COPY   SUBROUTINE                                            ***
      ***--------------------------------------------------------------***
      *
     C     COPY          BEGSR
      *
      *
     C                   SELECT
     C     BFRAFT        WHENEQ    'B'
      **
     *** FOR THE BEFORE COPY
     ***   If Select process is done, and copy process hasn't completed
     ***   mark the flags, Copy has started
      **
     C     OCSELF        IFEQ      'Y'
     C     OCCPYF        ANDEQ     'N'
     C     OCSELF        OREQ      'Y'
     C     OCCPYF        ANDEQ     'S'
     C                   MOVEL     'S'           OCCPYF
     C                   ELSE
      *** ERROR CANT RUN PROCESS
     C                   MOVEL     '2'           RTNCDE
     C                   ENDIF
      *
     C     BFRAFT        WHENEQ    'A'
      *
     C                   MOVEL     'Y'           OCCPYF
      *** Today's Date as Copy completion Date
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OCLCPY
     C*
     C                   OTHER
     C*** ERROR IF NOT "B" OR "A"
      *
     C                   ENDSL
      *
      *
     C     ENDCPY        ENDSR
      *
      ***--------------------------------------------------------------***
      *** DELETE SUBROUTINE                                            ***
      ***--------------------------------------------------------------***
      *
     C     DELETE        BEGSR
      *
      *
     C                   SELECT
     C     BFRAFT        WHENEQ    'B'
      **
     *** FOR THE BEFORE DELETE
     ***   If Select process is done, and copy process is done, and Delete
     ***   process is either not started, or still processing, mark flag as
     ***   started for Deletion.
      **
     C     OCSELF        IFEQ      'Y'
     C     OCCPYF        ANDEQ     'Y'
     C     OCDELF        ANDEQ     'N'
     C     OCSELF        OREQ      'Y'
     C     OCCPYF        ANDEQ     'Y'
     C     OCDELF        ANDEQ     'S'
     C                   MOVEL     'S'           OCDELF
     C                   ELSE
      *** ERROR CANT RUN PROCESS
     C                   MOVEL     '3'           RTNCDE
     C                   ENDIF
      *
     C     BFRAFT        WHENEQ    'A'
      *
     C                   MOVEL     'Y'           OCDELF
      *** Today's Date as Copy completion Date
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OCLDEL
     C*
     C                   OTHER
     C*** ERROR IF NOT "B" OR "A"
      *
     C                   ENDSL
      *
      *
     C     ENDDEL        ENDSR
      *
