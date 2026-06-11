     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXCAP_EOP UPDATE FDCAP FILE FOR INCORRECT DENIED AMOUNTS PARTS
     H* OHIO FEE SCHEDULE CALCULATED THE DENIED AMOUNT INCORRECTLY
     H* IT LEFT THE VALUE BLANK WHEN IT SHOULD HAVE HELD THE AMOUNT
     H* CHARGED.
     H*
     FFDCAP     UF   E           K DISK

     D*
     D*
     D XXPGM           C                   CONST('FIXCAP_EOP')
     D*
     D*   USED TO UPDATE DATE AND TIME
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
     C     CAEOMB        IFEQ      '097'
     C     CAEOMB        OREQ      '099'
     C*
     C     CATOTD        IFEQ      *ZERO
     C     CATOTP        ANDEQ     *ZERO
     C     CATOTC        SUB       CANDIS        CATOTD
     C     CATOTD        SUB       CAOTHR        CATOTD
     C*
     C                   MOVEL     DSTIME        CATIME
     C                   MOVEL     DSDT          CADT
     C                   MOVEL(P)  'COOPERB'     CAUSER
     C                   MOVEL(P)  XXPGM         CAPGM
     C*
     C                   UPDATE    PCA01
     C                   ENDIF                                                  ENDCATOTD
     C                   ENDIF                                                  ENDCAEOMB
     C                   READ      PCA01                                  60
     C                   ENDDO                                                  ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
