     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTHCIA-CONVERT HCIA DATA FROM TAPE TO FILE FORMAT
     F*
     F* LOS BY DIAGNOSIS
     FUMLDPTAP  IF   F   48        DISK
     F*
     F* LOS BY OPERATION
     FUMLOPTAP  IF   F   48        DISK
     F*
     F* LENGTH OF STAY
     FUMLSP     O    E             DISK

     D*
     D* HCIA LOS BY DIAG FILE DATA STRUCTURE
     D                 DS
     D  XDFLD                  1     48
     D  XDRGN                  1      1
     D  XDGRP                  2      4
     D  XDCAT                  5      5
     D  XDAGE                  6      6
     D  XDTPAT                 9     16
     D  XDASTY                17     19
     D  XDVARI                20     23
     D  XD10P                 24     25
     D  XD10P1                26     26
     D  XD25P                 27     28
     D  XD25P1                29     29
     D  XD50P                 30     31
     D  XD50P1                32     32
     D  XD75P                 33     34
     D  XD75P1                35     35
     D  XD90P                 36     37
     D  XD90P1                38     38
     D  XD95P                 39     40
     D  XD95P1                41     41
     D  XD99P                 42     43
     D  XD99P1                44     44
     D*
     D* HCIA LOS BY OPER FILE DATA STRUCTURE
     D                 DS
     D  XOFLD                  1     48
     D  XORGN                  1      1
     D  XOGRP                  2      4
     D  XOCAT                  5      5
     D  XOAGE                  6      6
     D  XOTPAT                 9     16
     D  XOASTY                17     19
     D  XOVARI                20     23
     D  XO10P                 24     25
     D  XO10P1                26     26
     D  XO25P                 27     28
     D  XO25P1                29     29
     D  XO50P                 30     31
     D  XO50P1                32     32
     D  XO75P                 33     34
     D  XO75P1                35     35
     D  XO90P                 36     37
     D  XO90P1                38     38
     D  XO95P                 39     40
     D  XO95P1                41     41
     D  XO99P                 42     43
     D  XO99P1                44     44
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     F*

     I*
     IUMLDPTAP  AB  61
     I                                  1   48  LDFLD
     I*
     IUMLOPTAP  AC  62
     I                                  1   48  LOFLD

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQEFDT            8            MMDDYY ORDER
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
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C*
     C* WRITE TO THE PDS LOS BY DIAG FILE (UMLSP)
     C                   READ      UMLDPTAP                               51
     C     *IN51         DOWEQ     '0'
     C                   MOVEL     LDFLD         XDFLD
     C                   MOVE      XDRGN         LSRGN
     C                   MOVE      XDGRP         LSGRP
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSEFDT
     C*
     C                   MOVE      XDCAT         LSCAT
     C                   MOVE      XDAGE         LSAGE
     C                   MOVE      XDTPAT        LSTPAT
     C                   MOVE      XDASTY        LSASTY
     C                   MOVE      XDVARI        LSVARI
     C                   MOVE      XD10P         LS10P
     C                   MOVE      XD10P1        LS10P1
     C                   MOVE      XD25P         LS25P
     C                   MOVE      XD25P1        LS25P1
     C                   MOVE      XD50P         LS50P
     C                   MOVE      XD50P1        LS50P1
     C                   MOVE      XD75P         LS75P
     C                   MOVE      XD75P1        LS75P1
     C                   MOVE      XD90P         LS90P
     C                   MOVE      XD90P1        LS90P1
     C                   MOVE      XD95P         LS95P
     C                   MOVE      XD95P1        LS95P1
     C                   MOVE      XD99P         LS99P
     C                   MOVE      XD99P1        LS99P1
     C                   MOVE      'A'           LSSTAT
     C                   MOVE      *BLANKS       LSBLK1
     C                   MOVE      *BLANKS       LSBLK2
     C                   MOVEL     'PDSPGMR'     LSUSER
     C                   MOVEL     'CVTHCIA '    LSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSDT
     C*
     C                   Z-ADD     DSTIME        LSTIME
     C                   MOVEL     'PDSPGMR'     LSIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSIDT
     C*
     C                   MOVE      'D'           LSLOST
     C                   WRITE     PLS01
     C                   READ      UMLDPTAP                               51
     C                   END
     C*
     C* WRITE TO THE PDS LOS BY OPER FILE (UMLSP)
     C                   READ      UMLOPTAP                               52
     C     *IN52         DOWEQ     '0'
     C                   MOVEL     LOFLD         XOFLD
     C                   MOVE      XORGN         LSRGN
     C                   MOVE      XOGRP         LSGRP
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSEFDT
     C*
     C                   MOVE      XOCAT         LSCAT
     C                   MOVE      XOAGE         LSAGE
     C                   MOVE      XOTPAT        LSTPAT
     C                   MOVE      XOASTY        LSASTY
     C                   MOVE      XOVARI        LSVARI
     C                   MOVE      XO10P         LS10P
     C                   MOVE      XO10P1        LS10P1
     C                   MOVE      XO25P         LS25P
     C                   MOVE      XO25P1        LS25P1
     C                   MOVE      XO50P         LS50P
     C                   MOVE      XO50P1        LS50P1
     C                   MOVE      XO75P         LS75P
     C                   MOVE      XO75P1        LS75P1
     C                   MOVE      XO90P         LS90P
     C                   MOVE      XO90P1        LS90P1
     C                   MOVE      XO95P         LS95P
     C                   MOVE      XO95P1        LS95P1
     C                   MOVE      XO99P         LS99P
     C                   MOVE      XO99P1        LS99P1
     C                   MOVE      'A'           LSSTAT
     C                   MOVEL     'PDSPGMR'     LSUSER
     C                   MOVEL     'CVTHCIA '    LSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSDT
     C*
     C                   Z-ADD     DSTIME        LSTIME
     C                   MOVEL     'PDSPGMR'     LSIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSIDT
     C*
     C                   MOVE      'O'           LSLOST
     C                   WRITE     PLS01
     C                   READ      UMLOPTAP                               52
     C                   END
     C*
     C                   MOVE      '1'           *INLR
