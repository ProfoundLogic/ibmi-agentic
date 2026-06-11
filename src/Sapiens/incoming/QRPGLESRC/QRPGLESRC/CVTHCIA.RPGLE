     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE CVTHCIA-CONVERT HCIA DATA FROM TAPE TO FILE FORMAT
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                               *
     H* 05/20/09 C:33  Project: WC1770                                *
     H*                Ensure MC values are kept in sync with system  *
     H*                 values.                                       *
     H*                                                              *
     H*  06/30/05  C:79  PROJECT: 133MBZ25456                        *
     H*                  ADD LEADING BLANK TO DIDS1                  *
     H*                                                              *
     H*  06/06/01  C:79  SUPPORT: MC025 25456                        *
     H*                  REMOVE LEADING BLANK FROM DIDS1             *
     H*                                                              *
     H*  05/23/01  C:62  PROJECT# 96MDY                              *
     H*                  MAKING CHANGES TO DEAL WITH FLAT FILE       *
     H*                                                              *
     H*  02/21/96  C:32  PROJECT# MC131                              *
     H*                  CHG TO LOAD NEW LOS DIAG OR OPER GRP -LSLGRP*
     H****************************************************************
     F*
     F* ICD-9-CM FILE
     F*** BEGIN CHG *** 05/23/01 **************************************
     F**********FMDIPTAPIF  F      84            DISK
     FFMDIPTAP  IF   E             DISK
     F*** END   CHG *** 05/23/01 **************************************
     FFMDIP     O    E             DISK
     F*** BEGIN ADD *** 05/23/01 **************************************
     F                                     RENAME(PDI01:PDI02)
     F*** END   ADD *** 05/23/01 **************************************
     F***  Begin Add  ***  05/20/09  **************************************
     F                                     INFDS(DIFDS)
     F***  End   Add  ***  05/20/09  **************************************
     F*
     F* LOS BY DIAGNOSIS
     F*** BEGIN CHG *** 05/23/01 **************************************
     F**********UMLDPTAPIF  F      48            DISK
     FUMLDPTAP  IF   E             DISK
     F*** END   CHG *** 05/23/01 **************************************
     F*
     F* LOS BY OPERATION
     F*** BEGIN CHG *** 05/23/01 **************************************
     F**********UMLOPTAPIF  F      48            DISK
     FUMLOPTAP  IF   E             DISK
     F*** END   CHG *** 05/23/01 **************************************
     F*
     F* LENGTH OF STAY
     FUMLSP     O    E             DISK
     F*

     D*** BEGIN DEL *** 05/23/01 **************************************
     D**********FMDIPTAPAA  60
     D**********                              1  84 DIFLD
     D*
     D**********UMLDPTAPAB  61
     D**********                              1  48 LDFLD
     D*
     D**********UMLOPTAPAC  62
     D**********                              1  48 LOFLD
     D*** END   DEL *** 05/23/01 **************************************
     D*
     D* HCIA ICD-9-CM FILE DATA STRUCTURE
     D                 DS
     D*** BEGIN ADD *** 05/23/01 **************************************
     D  XXDATA                 1     84
     D*** END   ADD *** 05/23/01 **************************************
     D  XXDP                   1      1
     D  XXE                    2      2
     D  XXCDEE                 2      6
     D  XIFLD                  1     84
     D  XILOST                 1      1
     D  XIICD9                 3      7
     D  XIDSC1                12     35
     D  XIGRP                 54     58
     D*** BEGIN ADD *** 05/23/01 **************************************
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
     D*** END   ADD *** 05/23/01 **************************************
     D*
     D* HCIA LOS BY DIAG FILE DATA STRUCTURE
     D                 DS
     D  XDFLD                  1     48
     D  XDRGN                  1      1
     D  XDGRP                  4      8
     D  XDCAT                  9      9
     D  XDAGE                 10     10
     D  XDTPAT                11     18
     D  XDASTY                19     21
     D  XDVARI                22     25
     D  XD10P                 26     27
     D  XD10P1                28     28
     D  XD25P                 29     30
     D  XD25P1                31     31
     D  XD50P                 32     33
     D  XD50P1                34     34
     D  XD75P                 35     36
     D  XD75P1                37     37
     D  XD90P                 38     39
     D  XD90P1                40     40
     D  XD95P                 41     42
     D  XD95P1                43     43
     D  XD99P                 44     45
     D  XD99P1                46     46
     D*
     D* HCIA LOS BY OPER FILE DATA STRUCTURE
     D                 DS
     D  XOFLD                  1     48
     D  XORGN                  1      1
     D  XOGRP                  4      8
     D  XOCAT                  9      9
     D  XOAGE                 10     10
     D  XOTPAT                11     18
     D  XOASTY                19     21
     D  XOVARI                22     25
     D  XO10P                 26     27
     D  XO10P1                28     28
     D  XO25P                 29     30
     D  XO25P1                31     31
     D  XO50P                 32     33
     D  XO50P1                34     34
     D  XO75P                 35     36
     D  XO75P1                37     37
     D  XO90P                 38     39
     D  XO90P1                40     40
     D  XO95P                 41     42
     D  XO95P1                43     43
     D  XO99P                 44     45
     D  XO99P1                46     46
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D***  Begin Add  ***  05/20/09  **************************************
     D DIFDS           DS
     D  DIRRN                397    400B 0

     I***  End   Add  ***  05/20/09  **************************************

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
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C*
     C* WRITE TO THE PDS ICD-9-CM FILE (FMDIP)
     C                   READ      FMDIPTAP                               50
     C     *IN50         DOWEQ     '0'
     C*** BEGIN ADD *** 05/23/01 **************************************
     C                   MOVE      DIDATA        XXDATA
     C*** END   ADD *** 05/23/01 **************************************
     C*                    MOVELDIFLD     XIFLD
     C     XXE           IFEQ      'E'
     C                   MOVE      XXCDEE        DIICD9
     C*** BEGIN ADD *** 05/23/01 **************************************
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVEL     DIICD9        XXTMP3
     C     XXPER3        IFEQ      *BLANKS
     C                   MOVEL     XXCDEE        DIDS1
     C                   ELSE
     C                   MOVEL     DIICD9        XXCD14
     C                   MOVE      '.'           XXPER3
     C                   MOVE      DIICD9        XXCD55
     C                   MOVEL     XXTMP3        DIDS1
     C                   ENDIF
     C*** END   ADD *** 05/23/01 **************************************
     C                   ELSE
     C                   MOVE      XIICD9        DIICD9
     C*** BEGIN ADD *** 05/23/01 **************************************
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVEL     DIICD9        XXTMP2
     C     XXPER2        IFEQ      *BLANKS
     C                   MOVEL     DIICD9        DIDS1
     C                   ELSE
     C                   MOVEL     DIICD9        XXCD13
     C                   MOVE      '.'           XXPER2
     C                   MOVE      DIICD9        XXCD45
     C                   MOVEL     XXTMP2        DIDS1
     C                   ENDIF
     C*** END   ADD *** 05/23/01 **************************************
     C                   END
     C*** BEG   ADD *** 06/30/05 **************************************
     C     XXDP          IFEQ      'O'
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVEL(P)  DIICD9        XXTMP1
     C*
     C     XXPER1        IFEQ      *BLANKS
     C                   MOVEL     DIICD9        DIDS1
     C                   ELSE
     C                   MOVEL     DIICD9        XXCD12
     C                   MOVE      '.'           XXPER1
     C                   MOVE      DIICD9        XXCD35
     C                   MOVEL     XXTMP1        DIDS1
     C                   ENDIF
     C*
     C     ' '           CAT       DIICD9        DIICD9
     C                   ENDIF
     C*** END   ADD *** 06/30/05 **************************************
     C                   MOVEL     XIDSC1        DIDSC1
     C                   Z-ADD     *ZEROS        DIGRP
     C                   MOVEL(P)  XIGRP         DILGRP
     C                   MOVE      XILOST        DILOST
     C                   MOVE      *BLANKS       DIDSC2
     C                   MOVE      'A'           DISTAT
     C                   MOVEL     'PDSPGMR'     DIUSER
     C                   MOVEL     'CVTHCIA '    DIPGM
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
     C*** BEGIN DEL *** 05/23/01 **************************************
     C**********           MOVE *BLANKS   DIDS1
     C*** END   DEL *** 05/23/01 **************************************
     C*
     C*** BEG ADD *** 06/06/01 ****************************************
     C*** BEG DEL *** 06/30/05 ****************************************
     C*********
     C*********  1         SUBSTDIDS1:1   XXDS1   1
     C*********
     C*********  XXDS1     IFEQ ' '
     C*********            MOVE *BLANKS   ZZDS1  10
     C*********  6         SUBSTDIDS1:2   ZZDS1
     C*********            MOVE ZZDS1     DIDS1     P
     C*********            ENDIF
     C*** END DEL *** 06/30/05 ****************************************
     C*** END ADD *** 06/06/01 ****************************************
     C*
     C                   MOVE      *BLANKS       DIDS2
     C                   MOVE      *BLANKS       DIDS3
     C                   MOVE      *BLANKS       DIDS4
     C                   MOVE      *BLANKS       DIDS5
     C*** BEGIN CHG *** 05/23/01 **************************************
     C**********           WRITEPDI01
     C                   WRITE     PDI02
     C*** END   CHG *** 05/23/01 **************************************
     C***  Begin Add  ***  05/20/09  **************************************
     C                   FEOD      FMDIP
     C                   EXSR      UPDSYS
     C***  End   Add  ***  05/20/09  **************************************
     C                   READ      FMDIPTAP                               50
     C                   ENDDO
     C*
     C* WRITE TO THE PDS LOS BY DIAG FILE (UMLSP)
     C                   READ      UMLDPTAP                               51
     C     *IN51         DOWEQ     '0'
     C*** BEGIN CHG *** 05/23/01 **************************************
     C**********           MOVELLDFLD     XDFLD
     C                   MOVEL     LDDATA        XDFLD
     C*** END   CHG *** 05/23/01 **************************************
     C                   MOVE      XDRGN         LSRGN
     C                   Z-ADD     *ZEROS        LSGRP
     C                   MOVEL(P)  XDGRP         LSLGRP
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
     C                   ENDDO
     C*
     C* WRITE TO THE PDS LOS BY OPER FILE (UMLSP)
     C                   READ      UMLOPTAP                               52
     C     *IN52         DOWEQ     '0'
     C*** BEGIN CHG *** 05/23/01 **************************************
     C**********           MOVELLOFLD     XOFLD
     C                   MOVEL     LODATA        XOFLD
     C*** END   CHG *** 05/23/01 **************************************
     C                   MOVE      XORGN         LSRGN
     C                   Z-ADD     *ZEROS        LSGRP
     C                   MOVEL(P)  XOGRP         LSLGRP
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
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
     C***  Begin Add  ***  05/20/09  **************************************
     C*====================================================================
     C* Update the system values
     C*====================================================================
     C     UPDSYS        BEGSR
     C*
     C                   CALL      'FA210R'
     C                   PARM      DIRRN         ##RRN             9 0
     C                   PARM      'U'           ##MODE            1
     C                   PARM      'MC'          ##FROM            2
     C                   PARM      'SY'          ##TO              2
     C*
     C                   ENDSR
     C***  End   Add  ***  05/20/09  **************************************
