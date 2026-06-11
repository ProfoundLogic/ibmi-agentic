     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0041R - FACILITY/HOSPITAL INFORMATION
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    ALLOWS USERS TO MAINTAIN FACILITY/HOSPITAL INFORMATION     *
     H*    (STORED IN NMHSP).                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MANAGED CARE MENU                                          *
     H*                                                               *
     H*  CREATED:                                                     *
     H*    09/29/97  C:77  PROJECT 87MAU                              *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  11/07/03 C:79  PROJ:MC256
     H*                 ADD DISPLAY MODE
     H*
     H*  11/02/98 C:62  PROJECT: MC145
     H*                 ADDED PARM TO DETERMINE THE MODE
     H*
     H*  09/28/98 C:62  PROJECT: MC098 14017
     H*                 CHANGED WMUPL TO SMUPL.
     H*
     H*  06/26/98 C:94  PROJECT: MC145
     H*                 INSTALLED CAPABILITY FOR F14=DARTS
     H*
     H*  01/15/98 C:31  Project: 60MBS
     H*                 Anthem Network
     H*                 1. Correct date logic
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H* SCR02:                                                        *
     H* SCR03:                                                        *
     H*****************************************************************
     FF0041F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*** BEGIN CHG *** 09/28/98 **************************************
     F**********WMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
     F*** END   CHG *** 09/28/98 **************************************
     F*
     FFMPDL     IF   E           K DISK
     F*
     FNMHSL1    UF A E           K DISK
     F*
     FSMCOL     IF   E           K DISK

     D*** BEG CHG *** 11/07/03 ****************************************
     D******************* UA      1   2 10               MAINTENENCE MODE
     D UA              S             10    DIM(3) CTDATA PERRCD(1)              MAINTENENCE MODE
     D*** END CHG *** 11/07/03 ****************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D OPTNM           C                   CONST('Facility/Hospital-
     D                                      Information')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     ALFKY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 11/02/98 **************************************
     C                   PARM                    QQMODE            1
     C*** END   ADD *** 11/02/98 **************************************
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
     C     *LIKE         DEFINE    HSFEIN        XXFEIN
     C     *LIKE         DEFINE    HSFMOD        XXMOD
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C*
     C*** BEG ADD *** 06/26/98 ***************************************
      *
      *  WCMS / WCMC System Control flags assigned
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C*** END ADD *** 06/26/98 ***************************************
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C*
     C                   ELSE
     C                   MOVEL     UPCO#         XXCO#
     C*
     C                   ENDIF                                                  *IN60 = '1'
     C*
     C     TOP           TAG
     C*
     C                   MOVE      Q1USER        SCUSER
     C*** BEGIN ADD *** 11/02/98 **************************************
     C*
     C* SET THE SCREEN MODE BASED UPON *IN09 (DISPLAY(09)/CHANGE(N09)).
     C*
     C     QQMODE        COMP      'D'                                    09
     C*** END   ADD *** 11/02/98 **************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* DISPLAY PROVIDER INFORMATION
     C*
     C                   Z-ADD     XXFEIN        SCFEIN
     C                   Z-ADD     XXMOD         SCMOD
     C*
     C* GET THE NAME OF THE PROVIDER
     C*
     C     ALFKY         CHAIN     PPD01                              60
     C                   MOVEL     PDNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     ALFKY         SETGT     PHS01
     C     ALFKY         READPE    PHS01                                  60
     C     *IN60         IFEQ      '1'
     C*
     C* IF NEW RECORD, SAVE FEIN AND MOD
     C                   Z-ADD     XXFEIN        HSFEIN
     C                   MOVEL     XXMOD         HSFMOD
     C*
     C                   MOVEL     UA(1)         SCMODE
     C                   MOVEL     *BLANKS       SCGPN2
     C                   MOVEL     *BLANKS       SCCOM3
     C                   Z-ADD     *ZERO         SCICUB
     C                   Z-ADD     *ZERO         SCCCUB
     C                   Z-ADD     *ZERO         SCPCUB
     C                   Z-ADD     *ZERO         SCMLSB
     C                   Z-ADD     *ZERO         SCORTB
     C                   Z-ADD     *ZERO         SCPEDB
     C                   Z-ADD     *ZERO         SCOBB
     C                   Z-ADD     *ZERO         SCGYNB
     C                   Z-ADD     *ZERO         SCNURB
     C                   Z-ADD     *ZERO         SCOT1B
     C                   Z-ADD     *ZERO         SCOT2B
     C                   Z-ADD     *ZERO         SCOT3B
     C*
     C                   ELSE
     C*** BEG ADD *** 11/07/03 ****************************************
     C     QQMODE        IFNE      'D'
     C*** BEG ADD *** 11/07/03 ****************************************
     C                   MOVEL     UA(2)         SCMODE
     C*** BEG ADD *** 11/07/03 ****************************************
     C                   ELSE
     C                   MOVEL     UA(3)         SCMODE
     C                   ENDIF
     C*** BEG ADD *** 11/07/03 ****************************************
     C                   MOVEL     HSGPN2        SCGPN2
     C                   MOVEL     HSCOM3        SCCOM3
     C                   Z-ADD     HSICUB        SCICUB
     C                   Z-ADD     HSCCUB        SCCCUB
     C                   Z-ADD     HSPCUB        SCPCUB
     C                   Z-ADD     HSMLSB        SCMLSB
     C                   Z-ADD     HSORTB        SCORTB
     C                   Z-ADD     HSPEDB        SCPEDB
     C                   Z-ADD     HSOBB         SCOBB
     C                   Z-ADD     HSGYNB        SCGYNB
     C                   Z-ADD     HSNURB        SCNURB
     C                   Z-ADD     HSOT1B        SCOT1B
     C                   Z-ADD     HSOT2B        SCOT2B
     C                   Z-ADD     HSOT3B        SCOT3B
     C*
     C                   ENDIF                                                  *IN60 = '1'
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF='SCR01'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  Q2RECF='SCR02'
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   ENDIF                                                  Q2RECF='SCR03'
     C*
     C*****************************************************************
     C* PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C*
     C************
     C* F3=EXIT  *
     C************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  *INKC = '1'
     C*
     C***************
     C* F12=CANCEL  *
     C***************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'L'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  *INKL = '1'
     C*
     C*** BEG ADD *** 06/26/98 ***************************************
      **********************************************************************
      *  KNDO - F14=DARTS
      **********************************************************************
     C     *INKN         IFEQ      '1'
     C*
     C                   MOVE      SCFEIN        #@FEIN
     C                   MOVE      SCMOD         #@MOD
     C*
     C                   CALL      'FA706R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCDK        #@WCDK            1
     C                   PARM                    #@FEIN            9
     C                   PARM                    #@MOD             3
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD *** 06/26/98 ***************************************
     C*****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C**************************************
     C* PROCESS SCREEN REQUEST - SCREEN 1  *
     C**************************************
     C     SCR01T        TAG
     C*
     C* VALIDATE SCREEN DATA
     C*
     C*
     C* STORE ENTERED/ALTERED DATA
     C*
     C                   MOVEL     SCGPN2        HSGPN2
     C                   MOVEL     SCCOM3        HSCOM3
     C                   Z-ADD     SCICUB        HSICUB
     C                   Z-ADD     SCCCUB        HSCCUB
     C                   Z-ADD     SCPCUB        HSPCUB
     C                   Z-ADD     SCMLSB        HSMLSB
     C                   Z-ADD     SCORTB        HSORTB
     C                   Z-ADD     SCPEDB        HSPEDB
     C                   Z-ADD     SCOBB         HSOBB
     C                   Z-ADD     SCGYNB        HSGYNB
     C                   Z-ADD     SCNURB        HSNURB
     C                   Z-ADD     SCOT1B        HSOT1B
     C                   Z-ADD     SCOT2B        HSOT2B
     C                   Z-ADD     SCOT3B        HSOT3B
     C*
     C* LOAD SCREEN 2 DATA FROM FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         SCSPHY
     C                   Z-ADD     *ZERO         SCSTPC
     C                   Z-ADD     *ZERO         SCNURS
     C                   Z-ADD     *ZERO         SCAGPC
     C                   MOVEL     *BLANKS       SCLIC#
     C                   Z-ADD     *ZERO         SCLEXP
     C                   Z-ADD     *ZERO         SCMPED
     C                   MOVEL     *BLANKS       SCBN#
     C                   Z-ADD     *ZERO         SCBNDT
     C                   MOVEL     *BLANKS       SCMCA#
     C                   MOVEL     *BLANKS       SCCM21
     C                   MOVEL     *BLANKS       SCCM22
     C*
     C                   ELSE
     C                   Z-ADD     HSSPHY        SCSPHY
     C                   Z-ADD     HSSTPC        SCSTPC
     C                   Z-ADD     HSNURS        SCNURS
     C                   Z-ADD     HSAGPC        SCAGPC
     C                   MOVEL     HSLIC#        SCLIC#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDHSLEXP    SCLEXP
     C                   Z-ADD     HSLEXP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCLEXP
     C*** END CHG *** 01/15/98 ****************************************
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDHSMPED    SCMPED
     C                   Z-ADD     HSMPED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCMPED
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVEL     HSBN#         SCBN#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDHSBNDT    SCBNDT
     C                   Z-ADD     HSBNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCBNDT
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVEL     HSMCA#        SCMCA#
     C                   MOVEL     HSCM21        SCCM21
     C                   MOVEL     HSCM22        SCCM22
     C*
     C                   ENDIF                                                  *IN60 = '1'
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C**************************************
     C* PROCESS SCREEN REQUEST - SCREEN 2  *
     C**************************************
     C     SCR02T        TAG
     C*
     C* VALIDATE SCREEN DATA
     C*** BEGIN ADD *** 01/15/98 **************************************
     C     SCLEXP        IFNE      0
     C                   Z-ADD     SCLEXP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLEXP
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  LEXP <> 0
     C     SCMPED        IFNE      0
     C                   Z-ADD     SCMPED        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMPED
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  MPED <> 0
     C     SCBNDT        IFNE      0
     C                   Z-ADD     SCBNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBNDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  BNDT <> 0
     C*** END ADD *** 01/15/98 ****************************************
     C*
     C* STORE ENTERED/ALTERED DATA
     C*
     C                   Z-ADD     SCSPHY        HSSPHY
     C                   Z-ADD     SCSTPC        HSSTPC
     C                   Z-ADD     SCNURS        HSNURS
     C                   Z-ADD     SCAGPC        HSAGPC
     C                   MOVEL     SCLIC#        HSLIC#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCLEXP    HSLEXP
     C                   Z-ADD     SCLEXP        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         HSLEXP
     C*** END CHG *** 01/15/98 ****************************************
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCMPED    HSMPED
     C                   Z-ADD     SCMPED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         HSMPED
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVEL     SCBN#         HSBN#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCBNDT    HSBNDT
     C                   Z-ADD     SCBNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         HSBNDT
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVEL     SCMCA#        HSMCA#
     C                   MOVEL     SCCM21        HSCM21
     C                   MOVEL     SCCM22        HSCM22
     C*
     C* LOAD SCREEN 3 DATA FROM FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       SCCEOL
     C                   MOVEL     *BLANKS       SCCEOF
     C                   MOVEL     *BLANKS       SCCEOM
     C                   MOVEL     *BLANKS       SCCFOL
     C                   MOVEL     *BLANKS       SCCFOF
     C                   MOVEL     *BLANKS       SCCFOM
     C                   MOVEL     *BLANKS       SCCM11
     C                   MOVEL     *BLANKS       SCLIAC
     C                   Z-ADD     *ZERO         SCLIM1
     C                   Z-ADD     *ZERO         SCLIM2
     C                   Z-ADD     *ZERO         SCLIM3
     C                   MOVEL     *BLANKS       SCPOL#
     C                   Z-ADD     *ZERO         SCPEDT
     C                   Z-ADD     *ZERO         SCDEC
     C*
     C                   ELSE
     C                   MOVEL     HSCEOL        SCCEOL
     C                   MOVEL     HSCEOF        SCCEOF
     C                   MOVEL     HSCEOM        SCCEOM
     C                   MOVEL     HSCFOL        SCCFOL
     C                   MOVEL     HSCFOF        SCCFOF
     C                   MOVEL     HSCFOM        SCCFOM
     C                   MOVEL     HSCM11        SCCM11
     C                   MOVEL     HSLIAC        SCLIAC
     C                   Z-ADD     HSLIM1        SCLIM1
     C                   Z-ADD     HSLIM2        SCLIM2
     C                   Z-ADD     HSLIM3        SCLIM3
     C                   MOVEL     HSPOL#        SCPOL#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDHSPEDT    SCPEDT
     C                   Z-ADD     HSPEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCPEDT
     C*** END CHG *** 01/15/98 ****************************************
     C                   Z-ADD     HSDEC         SCDEC
     C*
     C                   ENDIF                                                  *IN60 = '1'
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C**************************************
     C* PROCESS SCREEN REQUEST - SCREEN 3  *
     C**************************************
     C     SCR03T        TAG
     C*
     C* VALIDATE SCREEN 3 DATA
     C*** BEGIN ADD *** 01/15/98 **************************************
     C     SCPEDT        IFNE      0
     C                   Z-ADD     SCPEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPEDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        74
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  PEDT <> 0
     C*** END ADD *** 01/15/98 ****************************************
     C*
     C*
     C* STORE ENTERED/ALTERED DATA
     C*
     C                   MOVEL     SCCEOL        HSCEOL
     C                   MOVEL     SCCEOF        HSCEOF
     C                   MOVEL     SCCEOM        HSCEOM
     C                   MOVEL     SCCFOL        HSCFOL
     C                   MOVEL     SCCFOF        HSCFOF
     C                   MOVEL     SCCFOM        HSCFOM
     C                   MOVEL     SCCM11        HSCM11
     C                   MOVEL     SCLIAC        HSLIAC
     C                   Z-ADD     SCLIM1        HSLIM1
     C                   Z-ADD     SCLIM2        HSLIM2
     C                   Z-ADD     SCLIM3        HSLIM3
     C                   MOVEL     SCPOL#        HSPOL#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCPEDT    HSPEDT
     C                   Z-ADD     SCPEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         HSPEDT
     C*** END CHG *** 01/15/98 ****************************************
     C                   Z-ADD     SCDEC         HSDEC
     C*
     C* WRITE DATA TO FILE
     C*
     C                   MOVEL     'F0041R'      HSPGM
     C                   MOVEL     Q1USER        HSUSER
     C                   Z-ADD     DSTIME        HSTIME
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDDSDATE    HSDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         HSDT
     C*** END CHG *** 01/15/98 ****************************************
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVEL     'F0040R'      HSIPGM
     C*** BEGIN ADD *** 01/15/98 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         HSIDT
     C*** END ADD *** 01/15/98 ****************************************
     C                   Z-ADD     DSTIME        HSITIM
     C                   WRITE     PHS01
     C*
     C                   ELSE
     C                   UPDATE    PHS01
     C*
     C                   ENDIF                                                  *IN60 = '1'
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  MODE TABLE
-- Add --
- Change -
-Display-
