     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMFBL1    UF A E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     F*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D QSD             C                   CONST('SUBDIREC.001')

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFLNM            8              FOLDER NAME
     C*
     C*
     C     FBKY1         KLIST
     C                   KFLD                    QQFLNM
     C*
     C*
     C     FBKY1         CHAIN     PFB01                              60
     C*** SET DEFAULT VALUES FOR INITIAL IMAGE SETUP
     C                   MOVEL     QQFLNM        FBCIF
     C                   MOVEL     QSD           FBCISF
     C                   MOVE      5000          FBM#DS
     C                   MOVE      *ZERO         FBC#DS
     C                   MOVEL     'DFU   '      FBIUSR
     C                   MOVEL     'INSTIMG2'    FBIPGM
     C                   MOVE      000001        FBITIM
     C                   MOVE      010101        FBIDT
     C                   MOVEL     'DFU   '      FBLUSR
     C                   MOVEL     'INSTIMG2'    FBLPGM
     C                   MOVE      000001        FBLTIM
     C                   MOVE      010101        FBLDT
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    PFB01
     C                   ELSE
     C                   WRITE     PFB01
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVE      *ON           *INLR
