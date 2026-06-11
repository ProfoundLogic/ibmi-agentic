     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE SCVWPDF1 - VIEW PDF DOCS - SPECIAL PROCESSING
     H*
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FSMVDP     UF   E           K DISK
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    #DAFP             1
     C                   PARM                    #DUSRI            8
     C                   PARM                    #DSYSN            8
     C                   PARM                    #DFLR             8
     C                   PARM                    #DNBR             7
     C                   PARM                    #ADNAM            8
     C                   PARM                    #ADSYS            8
     C                   PARM                    #CURU            10
     C                   PARM                    #TOC              1
     C*
     C*** FILE WILL BE AT 1ST RCD ON OPEN
     C*
     C                   READ      PVD01                                  67
     C     *IN67         IFEQ      *OFF
     C                   MOVEL     VDAFP         #DAFP
     C                   MOVEL     VDUSRI        #DUSRI
     C                   MOVEL     VDSYSN        #DSYSN
     C                   MOVEL     VDFLR         #DFLR
     C                   MOVE      VDNBR         #DNBR
     C                   MOVE      *BLANKS       #TOC
     C*
     C                   ADD       1             VDNBR
     C                   UPDATE    PVD01
     C                   ENDIF
     C                   MOVEL     *BLANKS       #ADNAM
     C                   MOVEL     *BLANKS       #ADSYS
     C*
     C     ENDPGM        TAG
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
