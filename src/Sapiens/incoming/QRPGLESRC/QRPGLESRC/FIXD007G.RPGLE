      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007G - Report payment requests with pv1
      *                   but no reserve/trans code
      *****************************************************************
      *
      * DESCRIPTION:  This identifies payment requests
      *               with a PV1 and no reserve/trans codes.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2RL  IF   E             DISK    PREFIX(x) RENAME(BADPV2R:BADPVR)
     FBADPV3    IF   E           K DISK
     FWARPL     IF   E           K DISK
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
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    xRPFND
     C                   KFLD                    xRPFYR
     C                   KFLD                    xRPCASE
     C                   KFLD                    xRPSEQ#

     C     P3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    xRPFND
     C                   KFLD                    xRPFYR
     C                   KFLD                    xRPCASE


     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD

     C                   READ      BADPVR
     C                   DOW       NOT %EOF(BADPV2RL)
     C     P3KY          CHAIN     BADPV3
     C                   IF        %FOUND(BADPV3)
     C     RPKY1         CHAIN     WARPL
     C                   IF        RPBR2 <> *BLANKS and
     C                             RPTRCD = *BLANKS AND RPTYPE = *BLANKS AND
     C                             RPRECD = *BLANKS AND RPRETP = *BLANKS
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
     O                                           75 'Check for payment requests'
     O                                         +001 'with pv1 and no'
     O                                         +001 'trans codes'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD007G'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           27 'PV1'
     O                                           60 'TRANS CODE'
     O          E            DET
     O                       RPFND         4      3
     O                       RPFYR         4      9
     O                       RPCASE        4     20
     O                       RPBR2               26
     O                       RPTRCD              61
     O          E            TOT1        2
     O                       TOTCNT        Z     26
