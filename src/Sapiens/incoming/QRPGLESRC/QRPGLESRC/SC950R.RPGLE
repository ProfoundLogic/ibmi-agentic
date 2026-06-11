     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC950R - PROMPT FOR DARTS EXTRACT
     H*****************************************************************
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      XXXXXXX - SUBMITTER                                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H* 02/18/16  C:53  Project: 114IWR
     H*                 Code for Opt to Delete DARTS/attachments
     H*                                                               *
     H* 01/09/13  C:282  PROJECT: 114WGD                              *
     H*                  Shifted the F12 key to record format level   *
     H*                  in dispaly file. (Tranform Item 1872)        *
     H*                  Only Recompile the program.                  *
     H*                                                               *
     H* 12/27/09  C:002  PROJECT: 135INT                              *
     H*                  *** DARTS EXTRACT PROMPRT                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  20,24  INVALID DATE                                          *
     H*  21     TO DATE LESS THAN FROM DATE                           *
     H*  22,25  ONLY ONE DATE ENTERED                                 *
     H*  23     POSITION CURSOR TO APPROPRIATE DATE FIELD             *
     H*  26/27 - FLAG ERROR
     H*  60     GENERAL CHAIN                                         *
     H*****************************************************************
     FSC950F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSDEHL     IF   E           K DISK
     F*

     D****
     D**
     D****
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D************
     D                 DS
     D  GR                     1     42
     D                                     DIM(14)
     D  SCGR01                 1      3
     D  SCGR02                 4      6
     D  SCGR03                 7      9
     D  SCGR04                10     12
     D  SCGR05                13     15
     D  SCGR06                16     18
     D  SCGR07                19     21
     D  SCGR08                22     24
     D  SCGR09                25     27
     D  SCGR10                28     30
     D  SCGR11                31     33
     D  SCGR12                34     36
     D  SCGR13                37     39
     D  SCGR14                40     42
     D                 DS
     D  CF                     1     14
     D                                     DIM(14)
     D  SCCF01                 1      1
     D  SCCF02                 2      2
     D  SCCF03                 3      3
     D  SCCF04                 4      4
     D  SCCF05                 5      5
     D  SCCF06                 6      6
     D  SCCF07                 7      7
     D  SCCF08                 8      8
     D  SCCF09                 9      9
     D  SCCF10                10     10
     D  SCCF11                11     11
     D  SCCF12                12     12
     D  SCCF13                13     13
     D  SCCF14                14     14
     D                 DS
     D  PF                     1     14
     D                                     DIM(14)
     D  SCPF01                 1      1
     D  SCPF02                 2      2
     D  SCPF03                 3      3
     D  SCPF04                 4      4
     D  SCPF05                 5      5
     D  SCPF06                 6      6
     D  SCPF07                 7      7
     D  SCPF08                 8      8
     D  SCPF09                 9      9
     D  SCPF10                10     10
     D  SCPF11                11     11
     D  SCPF12                12     12
     D  SCPF13                13     13
     D  SCPF14                14     14
     D*** Begin Add *** 09/20/17 ****************************
     D                 DS
     D  AF                     1     14
     D                                     DIM(14)
     D  SCAF01                 1      1
     D  SCAF02                 2      2
     D  SCAF03                 3      3
     D  SCAF04                 4      4
     D  SCAF05                 5      5
     D  SCAF06                 6      6
     D  SCAF07                 7      7
     D  SCAF08                 8      8
     D  SCAF09                 9      9
     D  SCAF10                10     10
     D  SCAF11                11     11
     D  SCAF12                12     12
     D  SCAF13                13     13
     D  SCAF14                14     14
     D*** End   Add *** 09/20/17 ****************************
     D                 DS
     D  FD                     1    112  0
     D                                     DIM(14)
     D  SCFD01                 1      8  0
     D  SCFD02                 9     16  0
     D  SCFD03                17     24  0
     D  SCFD04                25     32  0
     D  SCFD05                33     40  0
     D  SCFD06                41     48  0
     D  SCFD07                49     56  0
     D  SCFD08                57     64  0
     D  SCFD09                65     72  0
     D  SCFD10                73     80  0
     D  SCFD11                81     88  0
     D  SCFD12                89     96  0
     D  SCFD13                97    104  0
     D  SCFD14               105    112  0
     D**
     D                 DS
     D  TO                     1    112  0
     D                                     DIM(14)
     D  SCTO01                 1      8  0
     D  SCTO02                 9     16  0
     D  SCTO03                17     24  0
     D  SCTO04                25     32  0
     D  SCTO05                33     40  0
     D  SCTO06                41     48  0
     D  SCTO07                49     56  0
     D  SCTO08                57     64  0
     D  SCTO09                65     72  0
     D  SCTO10                73     80  0
     D  SCTO11                81     88  0
     D  SCTO12                89     96  0
     D  SCTO13                97    104  0
     D  SCTO14               105    112  0
     D************
     D**
     D                 DS
     D  CP                     1    308
     D                                     DIM(14)
     D  SCCP01                 1     22
     D  SCCP02                23     44
     D  SCCP03                45     66
     D  SCCP04                67     88
     D  SCCP05                89    110
     D  SCCP06               111    132
     D  SCCP07               133    154
     D  SCCP08               155    176
     D  SCCP09               177    198
     D  SCCP10               199    220
     D  SCCP11               221    242
     D  SCCP12               243    264
     D  SCCP13               265    286
     D  SCCP14               287    308
     D************
     D                 DS
     D  PP                     1    308
     D                                     DIM(14)
     D  SCPP01                 1     22
     D  SCPP02                23     44
     D  SCPP03                45     66
     D  SCPP04                67     88
     D  SCPP05                89    110
     D  SCPP06               111    132
     D  SCPP07               133    154
     D  SCPP08               155    176
     D  SCPP09               177    198
     D  SCPP10               199    220
     D  SCPP11               221    242
     D  SCPP12               243    264
     D  SCPP13               265    286
     D  SCPP14               287    308
     *************
     D*** Begin Add *** 09/20/17 ****************************
     D                 DS
     D  AP                     1    308
     D                                     DIM(14)
     D  SCAP01                 1     22
     D  SCAP02                23     44
     D  SCAP03                45     66
     D  SCAP04                67     88
     D  SCAP05                89    110
     D  SCAP06               111    132
     D  SCAP07               133    154
     D  SCAP08               155    176
     D  SCAP09               177    198
     D  SCAP10               199    220
     D  SCAP11               221    242
     D  SCAP12               243    264
     D  SCAP13               265    286
     D  SCAP14               287    308
     D*** End   Add *** 09/20/17 ****************************

     C*
     C* KEY List For The Compnay Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*
     C* KEY List For The GROUP FILE
     C*
     C     GRKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    ##GRP
     C*
     C* KEY List For The User Parameter Master File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     *LIKE         DEFINE    UPDT          XXFDAT
     C     *LIKE         DEFINE    UPDT          XXTDAT
     C*
     C     STRT01        TAG                                                     INITIALIZE THESE
     C                   MOVEA     *ZEROS        FD
     C                   MOVEA     *ZEROS        TO
     C                   MOVEA     *BLANKS       CP
     C                   MOVEA     *BLANKS       PP
     C                   MOVEA     *BLANKS       CF
     C                   MOVEA     *BLANKS       PF
     C                   MOVEA     *BLANKS       GR
     C*
     C*  Get The User Parameter File
     C*
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C*
     C*  Get the Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*                    MOVE Q1USER    SCUSER
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   ZZZZ the Record Format Info For First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C*
     C*   Process All Command KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=*ON
     C*
     C*
     C     *INKH         IFEQ      *ON
     C*** DISPLAY THE HISTORY INFO *** (SCREEN 2?)
     *** <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
     **** GET THE HISTORY INFO TO DISPLAY AND LOAD SCREEN 2 HERE ***
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C                   MOVEA     *ZEROS        FD
     C                   MOVEA     *ZEROS        TO
     **
     C                   MOVEA     *BLANKS       CP
     C                   MOVEA     *BLANKS       PP
     C                   MOVEA     *BLANKS       CF
     C                   MOVEA     *BLANKS       PF
     C                   MOVEA     *BLANKS       GR
     C                   GOTO      SCR02T
     C                   ENDIF                                                  *INKC=*ON
     C*
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01  '     Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*********************
     C*  Process SCR01
     C*********************
     *********  1. GROUP MUST BE VALID *************************
     C*
     C                   MOVE      SCGRP         ##GRP             3 0
     C     GRKY          CHAIN     PFN01                              29
     C     *IN29         CABEQ     *ON           STRT
     C*
     C*
     C*
     C*   Both dates must be entered
     C*
     C                   SELECT
     C*
     C     SCFDAT        WHENEQ    *ZERO
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C*
     C     SCTDAT        WHENEQ    *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C*   Validate the Date Range Entered
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C     SRERR         CABEQ     '1'           STRT                     20
     C*
     C                   Z-ADD     SCTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*   The dates must be in ascending order.
     C*
     C     XXTDAT        IFLT      XXFDAT
     C*
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCDATT IFLT SCDATF
     *** VALIDATE THE FLAGS ENTERED.
     ***     1) CLAIMS AND/OR POLICY MUST BE SELECTED
     ***        A. INVALID ENTRY (NOT Y OR N)
     ***        B. MUST HAVE ONE (OR MORE) = "Y"   (BOTH CAN NOT BE "N")
     C*
     C     SCCLMF        IFNE      'Y'
     C     SCCLMF        ANDNE     'N'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     ***
     C*
     C     SCPOLF        IFNE      'Y'
     C     SCPOLF        ANDNE     'N'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*** Begin Add *** 09/20/17 ***************************************
     C*
     C* Process Agency
     C*
     C     SCAGTF        IFNE      'Y'
     C     SCAGTF        ANDNE     'N'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C*** End   Add *** 09/20/17 ***************************************
     ***
     ***
     C*
     C     SCCLMF        IFNE      'Y'
     C     SCPOLF        ANDNE     'Y'
     C*** Begin Add *** 09/20/17 ***************************************
     C     SCAGTF        ANDNE     'Y'
     C*** End   Add *** 09/20/17 ***************************************
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     ***
     C*
     C* Call the CLP to Submit the List
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVEL     SCFDAT        QQFRDT            8
     C                   MOVEL     SCTDAT        QQTODT            8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVEL     SCCLMF        QQCLMF            1
     C                   MOVEL     SCPOLF        QQPOLF            1
     C*** Begin Add *** 09/20/17 ******************************************
     C                   MOVEL     SCAGTF        QQAGTF            1
     C*** End   Add *** 09/20/17 ******************************************
     C                   MOVE      XXFDAT        QQFCYD            7
     C                   MOVE      XXTDAT        QQTCYD            7
     C*** Begin Add *** 09/20/17 ******************************************
     C**********         MOVEL     SCGRP         QQGRP             3
     C                   MOVE      SCGRP         QQGRP             3
     C*** End   Add *** 09/20/17 ******************************************
     C*
     C                   CALL      'SC950C'
     C                   PARM                    QQGRP
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQCLMF
     C                   PARM                    QQPOLF
     C*** Begin Add *** 09/20/17 ******************************************
     C                   PARM                    QQAGTF
     C*** End   Add *** 09/20/17 ******************************************
     C                   PARM                    QQFCYD
     C                   PARM                    QQTCYD
     C*** Beg Add *** 02/18/16 *************************************
     C                   PARM      'N'           QQDLT             1
     C*** End Add *** 02/18/16 *************************************
     C******************** PARM           QQOUTQ  ************************
     C*******
     C                   GOTO      TERM
     C*** SCR02
     C     SCR02T        TAG
     C* * NOW GO BACK TO SCR01
     C                   MOVEA     *ZEROS        FD
     C                   MOVEA     *ZEROS        TO
     C                   MOVEA     *BLANKS       CP
     C                   MOVEA     *BLANKS       PP
     C                   MOVEA     *BLANKS       CF
     C                   MOVEA     *BLANKS       PF
     ***
     C*** LOAD THE SCREEN USING READP ON TRANSDATE/TIME TO SHOW MOST RECENT
     *****  FIRST
     ***
     **
     C     *HIVAL        SETGT     PEH01
     C                   DO        14            X                 2 0
     C                   READP     PEH01                                  68
     C     *IN68         IFEQ      *OFF
     C                   MOVE      EHFRDT        FD(X)
     C                   MOVE      EHTODT        TO(X)
     C                   MOVEL     EHCLMF        CF(X)
     C                   MOVEL     EHPOLF        PF(X)
     C*** Begin Add *** 09/20/17 ********************************
     C                   MOVEL     EHAGTF        AF(X)
     C*** End   Add *** 09/20/17 ********************************
     **** DO PATH DIFFERENTLY HERE ** EXTRACT THE MAIN PORTION??
     C                   MOVEL     EHCPTH        CP(X)
     C                   MOVEL     EHPPTH        PP(X)
     C*** Begin Add *** 09/20/17 ********************************
     C                   MOVEL     EHAPTH        AP(X)
     C*** End   Add *** 09/20/17 ********************************
     C                   MOVEL     EHGRP         GR(X)
     ****
     C**8
     C                   ELSE
     C                   MOVE      *ZEROS        FD(X)
     C                   MOVE      *ZEROS        TO(X)
     C                   MOVEL     *BLANK        CF(X)
     C                   MOVEL     *BLANK        PF(X)
     C*** Begin Add *** 09/20/17 ********************************
     C                   MOVEL     *BLANK        AF(X)
     C*** End   Add *** 09/20/17 ********************************
     **** DO PATH DIFFERENTLY HERE ** EXTRACT THE MAIN PORTION??
     C                   MOVEL     *BLANKS       CP(X)
     C                   MOVEL     *BLANKS       PP(X)
     C*** Begin Add *** 09/20/17 ********************************
     C                   MOVEL     *BLANKS       AP(X)
     C*** End   Add *** 09/20/17 ********************************
     C                   MOVEL     *BLANKS       GR(X)
     ***
     C                   ENDIF
     C                   ENDDO
     **
     **
     C                   GOTO      STRT
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
