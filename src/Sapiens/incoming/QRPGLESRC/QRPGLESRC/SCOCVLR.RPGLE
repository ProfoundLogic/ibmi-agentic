     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* * *  SCOCUPR Validates the Optical Purge Status.
     H*****************************************************************
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
     FSMOCLA    IF   E           K DISK
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
      *
     C     *ENTRY        PLIST
     C                   PARM                    PROCES            1            S,C,D (SEL,CPY,DEL)
     C                   PARM                    RTNCDE            1
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
      **
     *** Selection Validation
     ***   If Delete process is done, and copy process hasn't started
     ***   mark the flags, select has started, copy not done, Del not done.
      **
     C     OCCPYF        IFEQ      'Y'
     C     OCDELF        ANDEQ     'Y'
     C     OCSELF        ANDNE     'S'
     C     OCCPYF        OREQ      'N'
     C     OCDELF        ANDEQ     'N'
     C     OCSELF        ANDNE     'S'
     C                   MOVEL     *BLANK        RTNCDE
     C                   ELSE
      *** ERROR CANT RUN PROCESS
     C                   MOVEL     '1'           RTNCDE
     C                   ENDIF
      *
      *
     C     ENDSEL        ENDSR
      *
      ***--------------------------------------------------------------***
      *** COPY   SUBROUTINE                                            ***
      ***--------------------------------------------------------------***
      *
     C     COPY          BEGSR
      **
     *** Copy Validation
     ***   If Select process is done, and copy process hasn't completed
     ***   mark the flags, Copy has started
      **
     C     OCSELF        IFEQ      'Y'
     C     OCCPYF        ANDEQ     'N'
     C     OCSELF        OREQ      'Y'
     C     OCCPYF        ANDEQ     'S'
     C                   MOVEL     *BLANK        RTNCDE
     C                   ELSE
      *** ERROR CANT RUN PROCESS
     C                   MOVEL     '2'           RTNCDE
     C                   ENDIF
      *
     C     ENDCPY        ENDSR
      *
      ***--------------------------------------------------------------***
      *** DELETE SUBROUTINE                                            ***
      ***--------------------------------------------------------------***
      *
     C     DELETE        BEGSR
      **
     *** Delete Validation
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
     C                   MOVEL     *BLANK        RTNCDE
     C                   ELSE
      *** ERROR CANT RUN PROCESS
     C                   MOVEL     '3'           RTNCDE
     C                   ENDIF
      *
     C     ENDDEL        ENDSR
      *
