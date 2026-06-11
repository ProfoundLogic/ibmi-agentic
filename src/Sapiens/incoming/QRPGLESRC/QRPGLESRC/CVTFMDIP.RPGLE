     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE CV1FMDIP-CONVERT ICD9 TO FILE FORMAT
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  06/15/95  C:33  PROJECT# FL006                              *
     H*                  REVISE TO LOAD ACTUAL VALUE (W/DECIMAL      *
     H*                  POINT) INTO DIDS1(RIGHT-ADJUST)             *
     H*  02/21/96  C:32  PROJECT# MC131                              *
     H*                  CHG TO LOAD NEW LOS DIAG OR OPER GRP -DILGRP*
     H****************************************************************
     FFMDIPTAP  IF   F   84        DISK
     FFMDIP     O    E             DISK

     D*
     D                 DS
     D  XXDP                   1      1
     D  XXE                    2      2
     D  XXCDEE                 2      6
     D  XXFLD                  1     84
     D  XXLOST                 1      1
     D  XXCODE                 3      7
     D  XXCAT                 10     10
     D  XXDESC                12     35
     D  XXABGP                54     58
     D*
     D*
     D***  ICD-9 CODE WITH 2 BEFORE AND 3 AFTER THE DECIMAL
     D*
     D                 DS
     D  XXTMP1                 1      6
     D  XXCD12                 1      2
     D  XXPER1                 3      3
     D  XXCD35                 4      6
     D*
     D***  ICD-9 CODE WITH 3 BEFORE AND 2 AFTER THE DECIMAL
     D*
     D                 DS
     D  XXTMP2                 1      6
     D  XXCD13                 1      3
     D  XXPER2                 4      4
     D  XXCD45                 5      6
     D*
     D***  ICD-9 CODE WITH 4 BEFORE AND 1 AFTER THE DECIMAL
     D*
     D                 DS
     D  XXTMP3                 1      6
     D  XXCD14                 1      4
     D  XXPER3                 5      5
     D  XXCD55                 6      6
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
     C                   MOVEL     DIFLD         XXFLD
     C     XXE           IFEQ      'E'
     C                   MOVE      XXCDEE        DIICD9
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVEL     DIICD9        XXTMP3
     C     XXPER3        IFEQ      *BLANKS
     C                   MOVEL     XXCDEE        DIDS1
     C                   ELSE
     C                   MOVEL     DIICD9        XXCD14
     C                   MOVE      '.'           XXPER3
     C                   MOVE      DIICD9        XXCD55
     C                   MOVEL     XXTMP3        DIDS1
     C                   END
     C                   ELSE
     C                   MOVE      XXCODE        DIICD9
     C*
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVEL     DIICD9        XXTMP2
     C     XXPER2        IFEQ      *BLANKS
     C                   MOVEL     DIICD9        DIDS1
     C                   ELSE
     C                   MOVEL     DIICD9        XXCD13
     C                   MOVE      '.'           XXPER2
     C                   MOVE      DIICD9        XXCD45
     C                   MOVEL     XXTMP2        DIDS1
     C                   END
     C*
     C                   END
     C                   MOVEL     XXDESC        DIDSC1
     C                   Z-ADD     *ZEROS        DIGRP
     C                   MOVEL(P)  XXABGP        DILGRP
     C                   MOVE      XXLOST        DILOST
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
     C                   MOVE      *BLANKS       DIDS2
     C                   MOVE      *BLANKS       DIDS3
     C                   MOVE      *BLANKS       DIDS4
     C                   MOVE      *BLANKS       DIDS5
     C                   WRITE     PDI01
     C                   MOVE      *BLANKS       XXE
     C                   READ      FMDIPTAP                               50
     C                   END
     C*
     C                   MOVE      '1'           *INLR
