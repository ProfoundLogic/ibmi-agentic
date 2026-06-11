     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFMDIP2 - CONVERT ICD9 TO FILE FORMAT
     FFMDIPTAP  IF   F   84        DISK
     FFMDIP     O    E             DISK

     D*
     D                 DS
     D  XXDP                   1      1
     D  XXE                    2      2
     D  XXCDEE                 2      6
     D  XIFLD                  1     84
     D  XILOST                 1      1
     D  XIICD9                 3      7
     D  XIDSC1                12     35
     D  XIGRP                 54     56
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     F*

     IFMDIPTAP  AA  60
     I                                  1   84  DIFLD

     C*
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
     C                   READ      FMDIPTAP                               50
     C     *IN50         DOWEQ     '0'
     C                   MOVEL     DIFLD         XIFLD
     C     XXE           IFEQ      'E'
     C                   MOVE      XXCDEE        DIICD9
     C                   ELSE
     C                   MOVE      XIICD9        DIICD9
     C                   END
     C                   MOVEL     XIDSC1        DIDSC1
     C                   MOVE      XIGRP         DIGRP
     C                   MOVE      XILOST        DILOST
     C                   MOVE      *BLANKS       DIDSC2
     C                   MOVE      'A'           DISTAT
     C                   MOVEL     'PDSPGMR'     DIUSER
     C                   MOVEL     'CVTFMDIP'    DIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DIDT
     C*
     C                   Z-ADD     DSTIME        DITIME
     C                   MOVEL     'PDSPGMR'     DIIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DIIDT
     C*
     C                   MOVE      *BLANKS       DIFL1
     C                   MOVE      *BLANKS       DIFL2
     C                   MOVE      *BLANKS       DIFL3
     C                   MOVE      *BLANKS       DIFL4
     C                   MOVE      *BLANKS       DIFL5
     C                   MOVE      *BLANKS       DIFL6
     C                   MOVE      *BLANKS       DIFL7
     C                   MOVE      *BLANKS       DIFL8
     C                   MOVE      *BLANKS       DIFL9
     C                   MOVE      *BLANKS       DIFL10
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVE      *BLANKS       DIDS2
     C                   MOVE      *BLANKS       DIDS3
     C                   MOVE      *BLANKS       DIDS4
     C                   MOVE      *BLANKS       DIDS5
     C                   WRITE     PDI01
     C                   READ      FMDIPTAP                               50
     C                   END
     C*
     C                   MOVE      '1'           *INLR
