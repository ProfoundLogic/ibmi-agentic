     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE SCVWDOC1 - VIEW AFP DOCS - SPECIAL PROCESSING
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
     FQAOKL02A  IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     I*
     IWOSFMT01
     I              WOS1DDEN                    S1DDEN
     I              WOS1DDGN                    S1DDGN
     I              WOS1USRP                    S1USRP
     I              WOS1MASK                    S1MASK
     I              WOS1INDU                    S1INDU
     I              WOS1RCVP                    S1RCVP
     I              WOS1CVRP                    S1CVRP
     I              WOS1MLNT                    S1MLNT
     I              WOS1RSV                     S1RSV
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
     C     VDAFP         IFEQ      'Y'
     C                   ADD       1             VDNBR
     C                   UPDATE    PVD01
     C                   ENDIF
     C     #CURU         CHAIN     WOSFMT01                           66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     S1DDEN        #ADNAM
     C                   MOVEL     S1DDGN        #ADSYS
     C                   ELSE
     C                   MOVEL     *BLANKS       #ADNAM
     C                   MOVEL     *BLANKS       #ADSYS
     C                   MOVE      '2'           #TOC
     C                   GOTO      ENDPGM
     C                   ENDIF
     C                   ELSE                                                    NO HIT
     C                   MOVE      '1'           #TOC
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*
     C     ENDPGM        TAG
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
