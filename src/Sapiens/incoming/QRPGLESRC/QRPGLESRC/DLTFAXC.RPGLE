     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE DLTFAXC - DELETE INCOMING FAX (FILE MBR AND DOCID)
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
     FSAIFL     UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXLFM                  1     30
     D  QQLIB                  1     10
     D  QQFIL                 11     20
     D  QQMBR                 21     30
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FC6             C                   CONST('                    -
     D                                        ')

     C*
     C* * * KEY TO INCOMING FAX FILE * * *
     C     IFKEY         KLIST
     C                   KFLD                    QQMBR
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           63
     C                   PARM                    QQLFM            30
     C*
     C*
     C                   MOVEL     QQLFM         XXLFM
     C*
     C                   TIME                    DSDTTM
     C*
      *================================================================
     C*
     C                   CALL      'DLTFXM'
     C                   PARM                    QQLIB
     C                   PARM                    QQFIL
     C                   PARM                    QQMBR
     C*
     C*
     C                   MOVEL     *BLANKS       XXFOLD           80
     C                   MOVEL     QQFOLD        XXFOLD
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     *BLANKS       QQUNCN          255
     C                   MOVEL     *BLANKS       QQIFSP            1
      ***** END ADD ***** 10/17/11 ***********************************
     C                   CALL      'DLDLOC'
     C                   PARM                    QQDCID
     C                   PARM                    XXFOLD           80
     C                   PARM                    QQRTC2            2
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    QQUNCN
     C                   PARM                    QQIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C     IFKEY         DELETE    PIF01                              60
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
