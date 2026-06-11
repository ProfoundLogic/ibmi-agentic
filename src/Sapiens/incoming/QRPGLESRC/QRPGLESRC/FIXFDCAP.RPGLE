     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXFDCAP - UPDATE BODY PARTS IN FDCAP FILE WITH 1997 BODY PARTS
     FFDCAP     UF   E           K DISK

     D*
     D*   USED TO UPDATE DATE AND TIME
     D*
     D*   INFORMATION
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDT                   7     12  0

     I*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQTCO#            3
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
     C                   MOVE      QQTCO#        XXTCO#            3 0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C*
     C*** PROCEDURE CODE FILE
     C                   READ      PCA01                                  60
     C     *IN60         DOWEQ     '0'
     C     CAPART        IFNE      *BLANKS
     C*
     C                   SELECT
     C*
     C*****************************************************************
     C*          CONVERSION OF BODY PARTS
     C*
     C*          ORIGINAL           NEW PART
     C*          01                 03
     C*          02                 03
     C*          03                 04
     C*          04                 04
     C*          05                 03
     C*          06                 01
     C*          7A                 07
     C*          7B                 07
     C*          7C                 07
     C*          7D                 08
     C*          7E                 08
     C*          7F                 08
     C*          7G                 07
     C****************************************************************
     C*
     C     CAPART        WHENGE    '01'
     C     CAPART        ANDLE     '02'
     C                   MOVEL     '03'          CAPART
     C     CAPART        WHENEQ    '03'
     C                   MOVEL     '04'          CAPART
     C     CAPART        WHENEQ    '05'
     C                   MOVEL     '03'          CAPART
     C     CAPART        WHENEQ    '06'
     C                   MOVEL     '01'          CAPART
     C     CAPART        WHENGE    '7A'
     C     CAPART        ANDLE     '7C'
     C                   MOVEL     '07'          CAPART
     C     CAPART        WHENGE    '7D'
     C     CAPART        ANDLE     '7F'
     C                   MOVEL     '08'          CAPART
     C     CAPART        WHENEQ    '7G'
     C                   MOVEL     '07'          CAPART
     C                   ENDSL
     C*
     C                   MOVEL     DSTIME        CATIME
     C                   MOVEL     DSDT          CADT
     C                   MOVEL(P)  'COOPERB'     CAUSER
     C                   MOVEL(P)  'FIXFDCAP'    CAPGM
     C*
     C                   ENDIF                                                   CAPART
     C                   UPDATE    PCA01
     C                   READ      PCA01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
