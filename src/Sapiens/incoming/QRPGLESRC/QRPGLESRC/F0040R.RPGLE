     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0040R - PROFESSIONAL/PHYSICIAN INFORMATION
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    ALLOWS USERS TO MAINTAIN PROFESSIONAL/PHYSICIAN INFORMATION*
     H*    (STORED IN NMPHP).                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MANAGED CARE MENU                                          *
     H*                                                               *
     H*  CREATED:                                                     *
     H*    09/29/97  C:77  PROJECT 87MAU                              *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20 C:212  Project : SY420
     H*                  Mask SSN
     H*
     H*  11/07/03 C:79  PROJ:MC256
     H*                 ADD DISPLAY MODE
     H*
     H*  07/16/99 C:62  PROJECT: MC145
     H*                 ADDED SEARCHES FOR FEIN/MOD FIELDS
     H*
     H*  11/02/98 C:62  PROJECT: MC145
     H*                 ADDED PARM TO DETERMINE THE MODE
     H*
     H*  10/16/98 C:62  PROJECT: MC098 14429
     H*                 SWAP OUT WMUPL WITH SMUPL.
     H*
     H*  06/26/98 C:94  PROJECT: MC145
     H*                 INSTALLED CAPABILITY FOR F14=DARTS
     H*
     H*  01/15/98 C:31  Project: 60MBS
     H*                 Anthem Network
     H*                 1. Correct date logic
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  *IN60 - GENERAL CHAIN                                        *
     H*****************************************************************
     FF0040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*** BEGIN CHG *** 10/16/98 **************************************
     F**********WMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
     F*** END   CHG *** 10/16/98 **************************************
     F*
     FFMPDL     IF   E           K DISK
     F*
     FNMPHL1    UF A E           K DISK
     F*
     FSMCOL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*** BEG CHG *** 11/07/03 ****************************************
     D******************* UA      1   2 10               MAINTENENCE MODE
     D UA              S             10    DIM(3) CTDATA PERRCD(1)              MAINTENENCE MODE
     D*** END CHG *** 11/07/03 ****************************************
     D*** BEGIN ADD *** 07/16/99 **************************************
     D FNF             S             10    DIM(5) CTDATA PERRCD(1)              FEIN FIELDS
     D MDF             S             10    DIM(5) CTDATA PERRCD(1)              MOD FIELDS
     D*** END   ADD *** 07/16/99 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 07/16/99 **************************************
     D                 DS
     D  FN                     1     45  0
     D                                     DIM(5)                               FEIN
     D  SCHF#1                 1      9  0
     D  SCHF#2                10     18  0
     D  SCHF#3                19     27  0
     D  SCHF#4                28     36  0
     D  SCHF#5                37     45  0
     D                 DS
     D  MD                     1     15  0
     D                                     DIM(5)                               MOD
     D  SCHFM1                 1      3  0
     D  SCHFM2                 4      6  0
     D  SCHFM3                 7      9  0
     D  SCHFM4                10     12  0
     D  SCHFM5                13     15  0
     D                 DS
     D  NM                     1    175
     D                                     DIM(5)                               NAME
     D  SCHSP1                 1     35
     D  SCHSP2                36     70
     D  SCHSP3                71    105
     D  SCHSP4               106    140
     D  SCHSP5               141    175
     D*** END   ADD *** 07/16/99 **************************************
     D OPTNM           C                   CONST('Professional Phys-
     D                                     ician Information')
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
     C     *LIKE         DEFINE    PHFEIN        XXFEIN
     C     *LIKE         DEFINE    PHFMOD        XXMOD
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
     C                   MOVEL     XXMOD         SCMOD
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
     C     ALFKY         SETGT     PPH01
     C     ALFKY         READPE    PPH01                                  60
     C     *IN60         IFEQ      '1'
     C*
     C* IF NEW RECORD, SAVE FEIN AND MOD
     C                   Z-ADD     XXFEIN        PHFEIN
     C                   MOVEL     XXMOD         PHFMOD
     C*
     C                   MOVEL     UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCPRSG
     C                   MOVEL     *BLANKS       SCSS#
     C                   MOVEL     *BLANKS       SCGPN1
     C                   MOVEL     *BLANKS       SCGPN2
     C                   MOVEL     *BLANKS       SCOFNM
     C                   MOVEL     *BLANKS       SCOML
     C                   MOVEL     *BLANKS       SCOMF
     C                   Z-ADD     *ZERO         SCMONO
     C                   Z-ADD     *ZERO         SCMONC
     C                   Z-ADD     *ZERO         SCTUEO
     C                   Z-ADD     *ZERO         SCTUEC
     C                   Z-ADD     *ZERO         SCWEDO
     C                   Z-ADD     *ZERO         SCWEDC
     C                   Z-ADD     *ZERO         SCTHUO
     C                   Z-ADD     *ZERO         SCTHUC
     C                   Z-ADD     *ZERO         SCFRIO
     C                   Z-ADD     *ZERO         SCFRIC
     C                   Z-ADD     *ZERO         SCSATO
     C                   Z-ADD     *ZERO         SCSATC
     C                   Z-ADD     *ZERO         SCSUNO
     C                   Z-ADD     *ZERO         SCSUNC
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
     C                   MOVE      PHPRSG        SCPRSG
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     PHSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'PHSS#':PHSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL     PHGPN1        SCGPN1
     C                   MOVEL     PHGPN2        SCGPN2
     C                   MOVEL     PHOFNM        SCOFNM
     C                   MOVEL     PHOML         SCOML
     C                   MOVEL     PHOMF         SCOMF
     C                   Z-ADD     PHMONO        SCMONO
     C                   Z-ADD     PHMONC        SCMONC
     C                   Z-ADD     PHTUEO        SCTUEO
     C                   Z-ADD     PHTUEC        SCTUEC
     C                   Z-ADD     PHWEDO        SCWEDO
     C                   Z-ADD     PHWEDC        SCWEDC
     C                   Z-ADD     PHTHUO        SCTHUO
     C                   Z-ADD     PHTHUC        SCTHUC
     C                   Z-ADD     PHFRIO        SCFRIO
     C                   Z-ADD     PHFRIC        SCFRIC
     C                   Z-ADD     PHSATO        SCSATO
     C                   Z-ADD     PHSATC        SCSATC
     C                   Z-ADD     PHSUNO        SCSUNO
     C                   Z-ADD     PHSUNC        SCSUNC
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
     C*** BEGIN ADD *** 07/16/99 **************************************
     C***************
     C* F10=SEARCH  *
     C***************
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C                   EXSR      KJDO
     C*
     C                   ENDIF
     C*** END   ADD *** 07/16/99 **************************************
     C*
     C***************
     C* F12=CANCEL  *
     C***************
     C     *INKL         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C*
     C                   MOVE      'L'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
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
     C**********************************************************************
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
     C*** BEGIN ADD *** 04/29/98 **************************************
     C                   SETOFF                                       71
     C*
     C     SCPRSG        IFNE      'S'
     C     SCPRSG        ANDNE     'G'
     C     SCPRSG        ANDNE     ' '
     C*
     C                   SETON                                        71
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*** END   ADD *** 04/29/98 **************************************
     C*
     C* SAVE ENTERED/ALTERED VALUES
     C*
     C                   MOVE      SCPRSG        PHPRSG
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     SCSS#         PHSS#
      /Free
       PHss# = UnMaskData('SSN':'PHSS#':scSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL     SCGPN1        PHGPN1
     C                   MOVEL     SCGPN2        PHGPN2
     C                   MOVEL     SCOFNM        PHOFNM
     C                   MOVEL     SCOML         PHOML
     C                   MOVEL     SCOMF         PHOMF
     C                   Z-ADD     SCMONO        PHMONO
     C                   Z-ADD     SCMONC        PHMONC
     C                   Z-ADD     SCTUEO        PHTUEO
     C                   Z-ADD     SCTUEC        PHTUEC
     C                   Z-ADD     SCWEDO        PHWEDO
     C                   Z-ADD     SCWEDC        PHWEDC
     C                   Z-ADD     SCTHUO        PHTHUO
     C                   Z-ADD     SCTHUC        PHTHUC
     C                   Z-ADD     SCFRIO        PHFRIO
     C                   Z-ADD     SCFRIC        PHFRIC
     C                   Z-ADD     SCSATO        PHSATO
     C                   Z-ADD     SCSATC        PHSATC
     C                   Z-ADD     SCSUNO        PHSUNO
     C                   Z-ADD     SCSUNC        PHSUNC
     C*
     C* LOAD SCREEN 2 DATA FROM FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       SCHSP1
     C                   MOVEL     *BLANKS       SCHSP2
     C                   MOVEL     *BLANKS       SCHSP3
     C                   MOVEL     *BLANKS       SCHSP4
     C                   MOVEL     *BLANKS       SCHSP5
     C                   Z-ADD     *ZERO         SCHF#1
     C                   Z-ADD     *ZERO         SCHF#2
     C                   Z-ADD     *ZERO         SCHF#3
     C                   Z-ADD     *ZERO         SCHF#4
     C                   Z-ADD     *ZERO         SCHF#5
     C                   Z-ADD     *ZERO         SCHFM1
     C                   Z-ADD     *ZERO         SCHFM2
     C                   Z-ADD     *ZERO         SCHFM3
     C                   Z-ADD     *ZERO         SCHFM4
     C                   Z-ADD     *ZERO         SCHFM5
     C                   MOVE      *BLANKS       SCPS1
     C                   MOVE      *BLANKS       SCPS2
     C                   MOVE      *BLANKS       SCPS3
     C                   MOVE      *BLANKS       SCPS4
     C                   MOVE      *BLANKS       SCPS5
     C                   MOVEL     *BLANKS       SCCOM1
     C                   MOVEL     *BLANKS       SCCOM2
     C                   MOVEL     *BLANKS       SCDRL#
     C                   Z-ADD     *ZERO         SCDRLE
     C                   Z-ADD     *ZERO         SCDRYL
     C                   MOVEL     *BLANKS       SCDEA#
     C                   Z-ADD     *ZERO         SCDEAE
     C                   MOVE      *BLANKS       SCIME
     C                   MOVEL     *BLANKS       SCIMC1
     C                   MOVEL     *BLANKS       SCIMC2
     C                   MOVEL     *BLANKS       SCPHYG
     C                   MOVEL     *BLANKS       SCICT1
     C                   MOVEL     *BLANKS       SCICT2
     C                   MOVE      *BLANKS       SCPAYN
     C*
     C                   ELSE
     C                   MOVEL     PHHSP1        SCHSP1
     C                   MOVEL     PHHSP2        SCHSP2
     C                   MOVEL     PHHSP3        SCHSP3
     C                   MOVEL     PHHSP4        SCHSP4
     C                   MOVEL     PHHSP5        SCHSP5
     C                   Z-ADD     PHHF#1        SCHF#1
     C                   Z-ADD     PHHF#2        SCHF#2
     C                   Z-ADD     PHHF#3        SCHF#3
     C                   Z-ADD     PHHF#4        SCHF#4
     C                   Z-ADD     PHHF#5        SCHF#5
     C                   Z-ADD     PHHFM1        SCHFM1
     C                   Z-ADD     PHHFM2        SCHFM2
     C                   Z-ADD     PHHFM3        SCHFM3
     C                   Z-ADD     PHHFM4        SCHFM4
     C                   Z-ADD     PHHFM5        SCHFM5
     C                   MOVE      PHPS1         SCPS1
     C                   MOVE      PHPS2         SCPS2
     C                   MOVE      PHPS3         SCPS3
     C                   MOVE      PHPS4         SCPS4
     C                   MOVE      PHPS5         SCPS5
     C                   MOVEL     PHCOM1        SCCOM1
     C                   MOVEL     PHCOM2        SCCOM2
     C                   MOVEL     PHDRL#        SCDRL#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDPHDRLE    SCDRLE
     C                   Z-ADD     PHDRLE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDRLE
     C*** END CHG *** 01/15/98 ****************************************
     C                   Z-ADD     PHDRYL        SCDRYL
     C                   MOVEL     PHDEA#        SCDEA#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDPHDEAE    SCDEAE
     C                   Z-ADD     PHDEAE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDEAE
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVE      PHIME         SCIME
     C                   MOVEL     PHIMC1        SCIMC1
     C                   MOVEL     PHIMC2        SCIMC2
     C                   MOVEL     PHPHYG        SCPHYG
     C                   MOVEL     PHICT1        SCICT1
     C                   MOVEL     PHICT2        SCICT2
     C                   MOVE      PHPAYN        SCPAYN
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
     C     SCDRLE        IFNE      0
     C                   Z-ADD     SCDRLE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDRLE
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  DRLE <> 0
     C     SCDEAE        IFNE      0
     C                   Z-ADD     SCDEAE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDEAE
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  DEAE <> 0
     C*** END ADD *** 01/15/98 ****************************************
     C*
     C*
     C* SAVE ENTERED/ALTERED DATA
     C*
     C                   MOVEL     SCHSP1        PHHSP1
     C                   MOVEL     SCHSP2        PHHSP2
     C                   MOVEL     SCHSP3        PHHSP3
     C                   MOVEL     SCHSP4        PHHSP4
     C                   MOVEL     SCHSP5        PHHSP5
     C                   Z-ADD     SCHF#1        PHHF#1
     C                   Z-ADD     SCHF#2        PHHF#2
     C                   Z-ADD     SCHF#3        PHHF#3
     C                   Z-ADD     SCHF#4        PHHF#4
     C                   Z-ADD     SCHF#5        PHHF#5
     C                   Z-ADD     SCHFM1        PHHFM1
     C                   Z-ADD     SCHFM2        PHHFM2
     C                   Z-ADD     SCHFM3        PHHFM3
     C                   Z-ADD     SCHFM4        PHHFM4
     C                   Z-ADD     SCHFM5        PHHFM5
     C                   MOVE      SCPS1         PHPS1
     C                   MOVE      SCPS2         PHPS2
     C                   MOVE      SCPS3         PHPS3
     C                   MOVE      SCPS4         PHPS4
     C                   MOVE      SCPS5         PHPS5
     C                   MOVEL     SCCOM1        PHCOM1
     C                   MOVEL     SCCOM2        PHCOM2
     C                   MOVEL     SCDRL#        PHDRL#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCDRLE    PHDRLE
     C                   Z-ADD     SCDRLE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PHDRLE
     C*** END CHG *** 01/15/98 ****************************************
     C                   Z-ADD     SCDRYL        PHDRYL
     C                   MOVEL     SCDEA#        PHDEA#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCDEAE    PHDEAE
     C                   Z-ADD     SCDEAE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PHDEAE
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVE      SCIME         PHIME
     C                   MOVEL     SCIMC1        PHIMC1
     C                   MOVEL     SCIMC2        PHIMC2
     C                   MOVEL     SCPHYG        PHPHYG
     C                   MOVEL     SCICT1        PHICT1
     C                   MOVEL     SCICT2        PHICT2
     C                   MOVE      SCPAYN        PHPAYN
     C*
     C* LOAD SCREEN 3 DATA FROM FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         SCDOB
     C                   MOVEL     *BLANKS       SCMEDS
     C                   Z-ADD     *ZERO         SCYRGR
     C                   MOVEL     *BLANKS       SCDEG
     C                   MOVEL     *BLANKS       SCSSCD
     C                   MOVEL     *BLANKS       SCRES
     C                   MOVEL     *BLANKS       SCBC1
     C                   MOVEL     *BLANKS       SCBC2
     C                   MOVEL     *BLANKS       SCELI1
     C                   MOVEL     *BLANKS       SCELI2
     C                   MOVEL     *BLANKS       SCPLC
     C                   Z-ADD     *ZERO         SCLIM1
     C                   Z-ADD     *ZERO         SCLIM2
     C                   Z-ADD     *ZERO         SCLIM3
     C                   MOVEL     *BLANKS       SCPOL#
     C                   Z-ADD     *ZERO         SCEXDT
     C                   Z-ADD     *ZERO         SCDED
     C*
     C                   ELSE
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDPHDOB     SCDOB
     C                   Z-ADD     PHDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDOB
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVEL     PHMEDS        SCMEDS
     C                   Z-ADD     PHYRGR        SCYRGR
     C                   MOVEL     PHDEG         SCDEG
     C                   MOVEL     PHSSCD        SCSSCD
     C                   MOVEL     PHRES         SCRES
     C                   MOVEL     PHBC1         SCBC1
     C                   MOVEL     PHBC2         SCBC2
     C                   MOVEL     PHELI1        SCELI1
     C                   MOVEL     PHELI2        SCELI2
     C                   MOVEL     PHPLC         SCPLC
     C                   Z-ADD     PHLIM1        SCLIM1
     C                   Z-ADD     PHLIM2        SCLIM2
     C                   Z-ADD     PHLIM3        SCLIM3
     C                   MOVEL     PHPOL#        SCPOL#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDPHEXDT    SCEXDT
     C                   Z-ADD     PHEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCEXDT
     C*** END CHG *** 01/15/98 ****************************************
     C                   Z-ADD     PHDED         SCDED
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
     C     SCDOB         IFNE      0
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDOB
     C     SRERR         IFEQ      '1'
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  DOB  <> 0
     C     SCEXDT        IFNE      0
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        74
     C                   GOTO      STRT
     C                   ENDIF                                                  ERR = '1'
     C                   ENDIF                                                  EXDT <> 0
     C*** END ADD *** 01/15/98 ****************************************
     C*
     C*
     C* SAVE ENTERED/ALTERED DATA
     C*
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCDOB     PHDOB
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PHDOB
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVEL     SCMEDS        PHMEDS
     C                   Z-ADD     SCYRGR        PHYRGR
     C                   MOVEL     SCDEG         PHDEG
     C                   MOVEL     SCSSCD        PHSSCD
     C                   MOVEL     SCRES         PHRES
     C                   MOVEL     SCBC1         PHBC1
     C                   MOVEL     SCBC2         PHBC2
     C                   MOVEL     SCELI1        PHELI1
     C                   MOVEL     SCELI2        PHELI2
     C                   MOVEL     SCPLC         PHPLC
     C                   Z-ADD     SCLIM1        PHLIM1
     C                   Z-ADD     SCLIM2        PHLIM2
     C                   Z-ADD     SCLIM3        PHLIM3
     C                   MOVEL     SCPOL#        PHPOL#
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDSCEXDT    PHEXDT
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PHEXDT
     C*** END CHG *** 01/15/98 ****************************************
     C                   Z-ADD     SCDED         PHDED
     C*
     C* WRITE DATA TO FILE
     C*
     C                   MOVEL     'F0040R'      PHPGM
     C                   MOVEL     Q1USER        PHUSER
     C                   Z-ADD     DSTIME        PHTIME
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********             Z-ADDDSDATE    PHDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PHDT
     C*** END CHG *** 01/15/98 ****************************************
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVEL     'F0040R'      PHIPGM
     C*** BEGIN ADD *** 01/15/98 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PHIDT
     C*** END ADD *** 01/15/98 ****************************************
     C                   Z-ADD     DSTIME        PHITIM
     C                   WRITE     PPH01
     C*
     C                   ELSE
     C                   UPDATE    PPH01
     C*
     C                   ENDIF
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 07/16/99 **************************************
     C*****************************************************************
     C* KJDO - F10=SEARCH
     C*****************************************************************
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C     HFLD          LOOKUP    FNF(X)                                 99
     C  N99HFLD          LOOKUP    MDF(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'F0032R'      ##PGM            10
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C                   SETOFF                                       16
     C*
     C     ##ERR         IFEQ      *ON
     C*
     C                   CALL      'FA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'F0032R'      ##FNM            10
     C                   PARM      'F0032Q'      ##TNM            10
     C*
     C     ##RTN         IFEQ      'A'
     C*
     C     32            ADD       X             Z                 2 0
     C                   MOVEL     *ON           *IN(Z)
     C                   SETON                                        31
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      'Y'           ##1N4             1            TYPE 1 & 4 ONLY
     C*
     C                   ELSE
     C*
     C                   CALL      'F0032Q'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      'Y'           ##1N4             1            TYPE 1 & 4 ONLY
     C*
     C                   ENDIF
     C*
     C     ##FEIN        IFNE      *BLANKS
     C*
     C                   MOVE      ##FEIN        FN(X)
     C                   MOVE      ##MOD         MD(X)
     C*
     C                   MOVE      ##FEIN        XXFEIN
     C                   MOVE      ##MOD         XXMOD
     C*
     C     ALFKY         CHAIN     PPD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  PDNAME        NM(X)
     C*
     C                   ENDIF
     C*
     C     32            ADD       X             Z                 2 0
     C                   MOVEL     *ON           *IN(Z)
     C                   SETON                                        32
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     32            ADD       X             Z                 2 0
     C                   MOVEL     *ON           *IN(Z)
     C                   SETON                                        31
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSR
     C*** END   ADD *** 07/16/99 **************************************
**  MODE TABLE
-- Add --
- Change -
-Display-
**  FEIN FIELDS
SCHF#1
SCHF#2
SCHF#3
SCHF#4
SCHF#5
**  MODE FIELDS
SCHFM1
SCHFM2
SCHFM3
SCHFM4
SCHFM5
