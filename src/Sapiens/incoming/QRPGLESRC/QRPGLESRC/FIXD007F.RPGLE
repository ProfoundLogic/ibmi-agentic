      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007F - Report payment with pv1
      *                   but no reserve/trans code
      *****************************************************************
      *
      * DESCRIPTION:  This identifies payments
      *               with a PV1 and no reserve/trans codes
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2    IF   E             DISK    PREFIX(x) RENAME(BADPV2:BADPVR)
     FBADPV3    IF   E           K DISK
     FWDPALB    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D XXCO#           S              3  0 INZ(777)
     D TOTCNT          S              5  0 INZ
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     P3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    xPAFND
     C                   KFLD                    xPAFYR
     C                   KFLD                    xPACASE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    xPAFND
     C                   KFLD                    xPAFYR
     C                   KFLD                    xPACASE
     C                   KFLD                    xPACHK#
     C                   KFLD                    xPASEQ#

     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD

     C                   READ      BADPVR
     C                   DOW       NOT %EOF(BADPV2)
     C     P3KY          CHAIN     BADPV3
     C                   IF        %FOUND(BADPV3)
     C     PAKY1         CHAIN     WDPALB
     C                   IF        PABR2 <> *BLANKS and
     C                             PATRCD = *BLANKS AND PATYPE = *BLANKS AND
     C                             PARECD = *BLANKS AND PARETP = *BLANKS
     C                   EVAL      TOTCNT =  TOTCNT + 1
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C                   ENDIF
     C                   READ      BADPVR
     C                   ENDDO

     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   EVAL      *INLR = *ON
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           75 'Check for payments'
     O                                         +001 'with pv1 and no'
     O                                         +001 'trans codes'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD007F'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           27 'PV1'
     O                                           60 'TRANS CODE'
     O          E            DET            1
     O                       PAFND         4      3
     O                       PAFYR         4      9
     O                       PACASE        4     20
     O                       PABR2               26
     O                       PATRCD              61
     O          E            TOT1        2
     O                       TOTCNT        Z     26
