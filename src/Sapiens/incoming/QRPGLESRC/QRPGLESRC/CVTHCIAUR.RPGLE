     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H****************************************************************
     H/TITLE CVTHCIAUR - SOLUCIENT ICD9 UPLOAD PROCESS - UPLOAD PGM
     H*
     H****************************************************************
     H* CREATED 12/05/01 BY C:62 FOR PROJECT:91MFB
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  06/14/17 C:053 Support: 46716
     H*                 Correct program not passing rrn value
     H*                                                              *
     H*  06/06/11 C:198  Support: 76863                              *
     H*                 -change managed care status to 'A'  in case  *
     H*                 of new V code.                               *
     H*                                                              *
     H*  01/21/11 C:198  Project: WC1879 Priss 142                   *
     H*                 -do not touch Managed Care status when a     *
     H*                 managed care code becomes CMS excluded       *
     H*                 code                                         *
     H*                                                              *
     H*  11/11/10 C:198 Project:WC1819 Priss 120                     *
     H*                 Fixed the upload of DIDS1 in case of CMS as  *
     H*                 well as Mananged Care.                       *
     H*                                                              *
     H*  11/01/10 C:251 PROJECT:133MJY                               *
     H*                 Modify to account for new position of        *
     H*                 operative code in file layout                *
     H*                                                              *
     H*  05/27/10 C:79 PROJECT:WC1770                                *
     H*                Ensure MC values are kept in sync with system *
     H*                values.                                       *
     H*                                                              *
     H*  05/18/10 C:198 PROJECT:WC1879                               *
     H*                Added 1 more parameter to distinguish         *
     H*                where it is called from.                      *
     H*                                                              *
     H*  10/01/03 C:79 PROJECT:81MUW                                 *
     H*                REMOVE REV 9/22/02. DATA FMT HAS CHANGED      *
     H*                                                              *
     H*  09/22/02 C:62 PROJECT:81MUK                                 *
     H*                NEED TO ADJUST THE OPERATIVE CODES SINCE IT   *
     H*                HAS A LEADING BLANK.                          *
     H*                                                              *
     H****************************************************************
     FFMDIPTAP  IF   E             DISK
     F*** Beg Chg *** 05/18/10 **********************************
     F*****FMDIP   O   E                    DISK
     FFMDIL     UF A E           K DISK
     F*** End Chg *** 05/18/10 **********************************
     F                                     RENAME(PDI01:PDI02)
     F***  Begin Add  ***  05/27/10  **************************************
     F                                     INFDS(DIFDS)
     F***  End   Add  ***  05/27/10  **************************************
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* HCIA ICD-9-CM FILE DATA STRUCTURE
     D                 DS
     D  XXDATA                 1     84
     D  XXDP                   1      1
     D  XXE                    2      2
     D  XXCDEE                 2      6
     D  XIFLD                  1     84
     D  XILOST                 1      1
     D  XIICD9                 3      7
     D*** Beg Add *** 11/01/10 *********************************
     D  XXBLK                  2      3
     D  X1ICD9                 4      7
     D*** End Add *** 11/01/10 *********************************
     D  XIDSC1                12     35
     D  XIGRP                 54     58
     D*
     D*** Beg Add *** 05/18/10 **********************************
     D                 DS
     D  XXCMS                  1     84
     D  XXCOD                  1      5
     D  XXDESC                 7     84
     D*** End Add *** 05/18/10 **********************************
     D***  ICD-9 CODE WITH 2 BEFORE AND 3 AFTER THE DECIMAL
     D*
     D                 DS
     D*** BEGIN CHG *** 09/22/02 **************************************
     D*********                               1   6 XXTMP1
     D*********                               1   2 XXCD12
     D*********                               3   3 XXPER1
     D*********                               4   6 XXCD35
     D*** BEGIN CHG *** 10/01/03 **************************************
     D**********                              1   6 XXTMP1
     D**********                              1   3 XXCD12
     D**********                              4   4 XXPER1
     D**********                              5   6 XXCD35
     D  XXTMP1                 1      6
     D  XXCD12                 1      2
     D  XXPER1                 3      3
     D  XXCD35                 4      6
     D*** END   CHG *** 10/01/03 **************************************
     D*** END   CHG *** 09/22/02 **************************************
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
     D***  Begin Add  ***  05/27/10  **************************************
     D DIFDS           DS
     D  DIRRN                397    400B 0

     I***  End   Add  ***  05/27/10  **************************************

     C*
     C**** Beg Add *** 11/11/10 ********************************
     C     *LIKE         DEFINE    DIDS1         SVDS1
     C**** End Add *** 11/11/10 ********************************
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
     C*** Beg Add *** 05/18/10 ************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CMS             1
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9            5
     C                   KFLD                    KKLOST            1
     C*
     C                   READ      PDI02                                  95
     C     *IN95         DOWEQ     *OFF
     C     @@CMS         IFEQ      'Y'
     C*
     C     DICMS         IFNE      ' '
     C                   MOVEL     'E'           DICMS
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     'I'           DISTAT
     C*
     C                   ENDIF                                                  @@CMS = 'Y'
     C*
     C                   UPDATE    PDI02
     C                   READ      PDI02                                  95
     C                   ENDDO
     C*** End Add *** 05/18/10 ************************************
     C* WRITE TO THE PDS ICD-9-CM FILE (FMDIP)
     C                   READ      FMDIPTAP                               50
     C     *IN50         DOWEQ     '0'
     C*
     C*** Beg Add *** 05/18/10 ************************************
     C     @@CMS         IFEQ      'Y'
     C                   MOVEL     DIDATA        XXCMS
     C                   MOVEL     XXCOD         KKICD9
     C                   MOVEL     'D'           KKLOST
     C                   ELSE
     C*** End Add *** 05/18/10 ************************************
     C*** Beg Del *** 11/11/10 ************************************
     C*****                MOVE DIDATA    XXDATA
     C*****                MOVE XILOST    DILOST
     C*                    MOVELDIFLD     XIFLD
     C*****      XXE       IFEQ 'E'
     C*****                MOVE XXCDEE    DIICD9
     C*****                MOVE *BLANKS   DIDS1
     C*****                MOVELDIICD9    XXTMP3
     C*****      XXPER3    IFEQ *BLANKS
     C*****                MOVELXXCDEE    DIDS1
     C*****                ELSE
     C*****                MOVELDIICD9    XXCD14
     C*****                MOVE '.'       XXPER3
     C*****                MOVE DIICD9    XXCD55
     C*****                MOVELXXTMP3    DIDS1
     C*****                ENDIF
     C*
     C*****                ELSE
     C*
     C*****      DILOST    IFEQ 'D'
     C*
     C*****                MOVE XIICD9    DIICD9
     C*****                MOVE *BLANKS   DIDS1
     C*****                MOVELDIICD9    XXTMP2
     C*****      XXPER2    IFEQ *BLANKS
     C*****                MOVELDIICD9    DIDS1
     C*****                ELSE
     C*****                MOVELDIICD9    XXCD13
     C*****                MOVE '.'       XXPER2
     C*****                MOVE DIICD9    XXCD45
     C*****                MOVELXXTMP2    DIDS1
     C*****                ENDIF
     C*
     C*****                ELSE
     C*** Beg Add *** 11/01/10 *********************************
     C*****                MOVE *BLANKS   DIICD9
     C*****      XXBLK     IFEQ *BLANKS
     C*****                MOVELX1ICD9    DIICD9
     C*****                ELSE
     C*** End Add *** 11/01/10 *********************************
     C*****                MOVE XIICD9    DIICD9
     C*** Beg Add *** 11/01/10 *********************************
     C*****                ENDIF
     C*** End Add *** 11/01/10 *********************************
     C*****                MOVE *BLANKS   DIDS1
     C*****                MOVELDIICD9    XXTMP1
     C*****      XXPER1    IFEQ *BLANKS
     C*****                MOVELDIICD9    DIDS1
     C*****                ELSE
     C*****                MOVELDIICD9    XXCD12
     C*****                MOVE '.'       XXPER1
     C*****                MOVE DIICD9    XXCD35
     C*****                MOVELXXTMP1    DIDS1
     C*****                ENDIF
     C*** BEGIN CHG *** 09/22/02 **************************************
     C*************        MOVELDIICD9    XXICD4  4
     C*** BEGIN CHG *** 10/01/03 **************************************
     C*************        MOVE DIICD9    XXICD4  4
     C*****                MOVELDIICD9    XXICD4  4
     C*** END   CHG *** 10/01/03 **************************************
     C*** END   CHG *** 09/22/02 **************************************
     C*****                MOVE XXICD4    DIICD9    P
     C*****                ENDIF
     C*
     C*****                ENDIF
     C*
     C*** End Del *** 11/11/10 ************************************
     C*** Beg Add *** 11/11/10 ************************************
     C                   EXSR      MCSR1
     C                   MOVEL     DIDS1         SVDS1
     C*** End Add *** 11/11/10 ************************************
     C*** Beg Add *** 05/18/10 ************************************
     C                   MOVEL     DIICD9        KKICD9
     C                   MOVEL     DILOST        KKLOST
     C                   ENDIF                                                  @@CMS = 'Y'
     C*
     C     DIKY          CHAIN     PDI02                              95
     C     *IN95         IFEQ      *ON
     C     @@CMS         IFNE      'Y'
     C*** End Add *** 05/18/10 ************************************
     C*** Beg Del *** 11/11/10 ************************************
     C*****                MOVELXIDSC1    DIDSC1
     C*****                Z-ADD*ZEROS    DIGRP
     C*****                MOVELXIGRP     DILGRP    P
     C*****                MOVE *BLANKS   DIDSC2
     C*****                MOVE 'A'       DISTAT
     C*** Beg Del *** 05/18/10 ************************************
     C*****                MOVELQ1USER    DIUSER
     C*****                MOVELQ1PGM     DIPGM
     C*****                TIME           DSDTTM
     C*
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     DIDT
     C*
     C*****                Z-ADDDSTIME    DITIME
     C*****                MOVELQ1USER    DIIUSR
     C*
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     DIIDT
     C*** End Del *** 05/18/10 ************************************
     C*
     C*****                MOVE *BLANKS   DIFL1
     C*****                MOVE *BLANKS   DIFL2
     C*****                MOVE *BLANKS   DIFL3
     C*****                MOVE *BLANKS   DIFL4
     C*****                MOVE *BLANKS   DIFL5
     C*****                MOVE *BLANKS   DIFL6
     C*****                MOVE *BLANKS   DIFL7
     C*****                MOVE *BLANKS   DIFL8
     C*****                MOVE *BLANKS   DIFL9
     C*****                MOVE *BLANKS   DIFL10
     C*
     C*****      1         SUBSTDIDS1:1   XXDS1   1
     C*
     C*****      XXDS1     IFEQ ' '
     C*****                MOVE *BLANKS   ZZDS1  10
     C*****      6         SUBSTDIDS1:2   ZZDS1
     C*****                MOVE ZZDS1     DIDS1     P
     C*****                ENDIF
     C*
     C*****                MOVE *BLANKS   DIDS2
     C*****                MOVE *BLANKS   DIDS3
     C*****                MOVE *BLANKS   DIDS4
     C*****                MOVE *BLANKS   DIDS5
     C*
     C*** End Del *** 11/11/10 ************************************
     C*** Beg Add *** 11/11/10 ************************************
     C                   MOVEL     SVDS1         DIDS1
     C                   EXSR      MCSR2
     C*** End Add *** 11/11/10 ************************************
     C*** Beg Add *** 05/18/10 *******************************************
     C                   MOVE      ' '           DICMS
     C                   ELSE                                                    @@CMS <> 'Y'
     C*** chain on FMDIP to see if record already exists else write it
     C                   CLEAR                   PDI02
     C                   MOVEL     KKICD9        DIICD9
     C                   MOVEL     'D'           DILOST
     C*** Beg Add *** 11/11/10 ************************************
     C                   MOVEL     DIICD9        DIDS1
     C*** End Add *** 11/11/10 ************************************
     C                   EXSR      UPDCMS
     C*
     C                   ENDIF                                                   @@CMS <> 'Y'
     C*
     C                   EXSR      UPDMNT
     C* Update initial maintenance fields
     C                   MOVEL     Q1USER        DIIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DIIDT
     C*** End Add *** 05/18/10 *******************************************
     C                   WRITE     PDI02
     C*** BEG Add *** 05/18/10 *******************************************
     C                   ELSE                                                   *IN95 = *ON
     C***
     C     @@CMS         IFNE      'Y'
     C     DICMS         IFEQ      ' '
     C                   MOVE      ' '           DICMS
     C                   ENDIF
     C*** Beg Add *** 11/11/10 ************************************
     C                   EXSR      MCSR1
     C                   EXSR      MCSR2
     C*** End Add *** 11/11/10 ************************************
     C*** Beg Del *** 11/11/10 ************************************
     C*****                MOVELXIDSC1    DIDSC1
     C*****                Z-ADD*ZEROS    DIGRP
     C*****                MOVELXIGRP     DILGRP    P
     C*****                MOVE *BLANKS   DIDSC2
     C*****                MOVE 'A'       DISTAT
     C*** End Del *** 11/11/10 ************************************
     C                   ELSE                                                   @@CMS <> 'Y'
     C                   EXSR      UPDCMS
     C                   ENDIF                                                  @@CMS <> 'Y'
     C*
     C                   EXSR      UPDMNT
     C*
     C                   UPDATE    PDI02
     C*
     C                   ENDIF                                                  *IN95 = *ON
     C*** End Add *** 05/18/10 *******************************************
     C*** Beg Add *** 06/14/17  *******************************************
     C                   Z-ADD     DIRRN         ##RRN
     C*** End Add *** 06/14/17  *******************************************
     C*
     C***  Begin Add  ***  05/27/10  **************************************
     C*** Beg Chg *** 05/18/10  *******************************************
     C*****                FEOD FMDIP
     C                   FEOD      FMDIL
     C*** End Chg *** 05/18/10  *******************************************
     C                   EXSR      UPDSYS
     C***  End   Add  ***  05/27/10  **************************************
     C                   READ      FMDIPTAP                               50
     C*
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C***  Begin Add  ***  05/27/10  **************************************
     C*====================================================================
     C* Update the system values
     C*====================================================================
     C     UPDSYS        BEGSR
     C*
     C                   CALL      'FA210R'
     C*** Beg Add *** 06/14/17  *******************************************
     C****************   PARM      DIRRN         ##RRN             9 0
     C                   PARM                    ##RRN             9 0
     C*** End Add *** 06/14/17  *******************************************
     C                   PARM      'U'           ##MODE            1
     C                   PARM      'MC'          ##FROM            2
     C                   PARM      'SY'          ##TO              2
     C*
     C                   ENDSR
     C***  End   Add  ***  05/27/10  **************************************
     C*** Beg Add *** 05/18/10 ********************************************
     C*====================================================================
     C* UPDCMS - update CMS status field
     C*====================================================================
     C     UPDCMS        BEGSR
     C*
     C                   MOVEL     XXDESC        DIDSC1
     C                   SUBST     DIICD9:1      XXFIRS            1
     C     XXFIRS        IFEQ      'V'
     C                   MOVE      'E'           DICMS
     C*** Beg Add *** 01/21/11 ********************************************
     C     *IN95         IFEQ      *ON
     C*** End Add *** 01/21/11 ********************************************
     C*** Beg Chg *** 06/06/11 ********************************************
     C******               MOVE 'I'       DISTAT
     C                   MOVE      'A'           DISTAT
     C*** End Chg *** 06/06/11 ********************************************
     C*** Beg Add *** 01/21/11 ********************************************
     C                   ENDIF                                                  *in95 = *on
     C*** End Add *** 01/21/11 ********************************************
     C                   ELSE
     C                   MOVE      'I'           DICMS
     C                   MOVE      'A'           DISTAT
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================
     C* UPDMNT - update Maintenance fields
     C*====================================================================
     C     UPDMNT        BEGSR
     C*
     C                   MOVEL     Q1USER        DIUSER
     C                   MOVEL     Q1PGM         DIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DIDT
     C*
     C                   Z-ADD     DSTIME        DITIME
     C*
     C                   ENDSR
     C*** End Add *** 05/18/10 ********************************************
     C*** Beg Add *** 11/11/10 ********************************************
     C*====================================================================
     C* MCSR1  - SR for Managed care
     C*====================================================================
     C     MCSR1         BEGSR
     C***
     C                   MOVE      DIDATA        XXDATA
     C                   MOVE      XILOST        DILOST
     C*                    MOVELDIFLD     XIFLD
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
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     DILOST        IFEQ      'D'
     C*
     C                   MOVE      XIICD9        DIICD9
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
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       DIICD9
     C     XXBLK         IFEQ      *BLANKS
     C                   MOVEL     X1ICD9        DIICD9
     C                   ELSE
     C                   MOVE      XIICD9        DIICD9
     C                   ENDIF
     C                   MOVE      *BLANKS       DIDS1
     C                   MOVEL     DIICD9        XXTMP1
     C     XXPER1        IFEQ      *BLANKS
     C                   MOVEL     DIICD9        DIDS1
     C                   ELSE
     C                   MOVEL     DIICD9        XXCD12
     C                   MOVE      '.'           XXPER1
     C                   MOVE      DIICD9        XXCD35
     C                   MOVEL     XXTMP1        DIDS1
     C                   ENDIF
     C                   MOVEL     DIICD9        XXICD4            4
     C                   MOVE(P)   XXICD4        DIICD9
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* MCSR2  - SR for Managed care
     C*====================================================================
     C     MCSR2         BEGSR
     C*
     C                   MOVEL     XIDSC1        DIDSC1
     C                   Z-ADD     *ZEROS        DIGRP
     C                   MOVEL(P)  XIGRP         DILGRP
     C                   MOVE      *BLANKS       DIDSC2
     C                   MOVE      'A'           DISTAT
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
     C*
     C     1             SUBST     DIDS1:1       XXDS1             1
     C*
     C     XXDS1         IFEQ      ' '
     C                   MOVE      *BLANKS       ZZDS1            10
     C     6             SUBST     DIDS1:2       ZZDS1
     C                   MOVE(P)   ZZDS1         DIDS1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DIDS2
     C                   MOVE      *BLANKS       DIDS3
     C                   MOVE      *BLANKS       DIDS4
     C                   MOVE      *BLANKS       DIDS5
     C*
     C                   ENDSR
     C*** End Add *** 11/11/10 ***********************************
