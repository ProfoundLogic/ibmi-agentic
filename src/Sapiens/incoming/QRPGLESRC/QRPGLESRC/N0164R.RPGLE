     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
      * *****************************************************************
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H/TITLE N0164R - A/R INQUIRY - SCREEN PROGRAM
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  12/18/24 C:342 Project : MC447 Phase 2                      **
     H*                 1.F10 Search disabled on the Group Field     **
     H*                 DSPF Changes:                                **
     H*                 1.Changed Existing Group field into Typeahead**
     H*                                                              **
     H*  06/09/20 C:212 Project : SY420                              **
     H*                 Mask SSN                                     **
     H*                                                              **
     H*  05/19/00 C:62  PROJECT: MC172                               **
     H*                 ADDED ADDITIONAL PARM TO CALL TO F3040R.     **
     H*                                                              **
     H*  04/30/99 C:94  PROJECT: MC201                               **
     H*                 CHANGE NDTML TO NDTML1                       **
     H*                                                              **
     H*  01/22/99 C:94  PROJECT: MC048  SEQ# 16162                   **
     H*                 ACTIVATE F12= CANCEL                         **
     H*                                                              **
     H*  01/14/99 C:94  PROJECT: 25MKL                               **
     H*                 CHANGED FAEOL3 TO FAEOLG AND CHANGED KLIST   **
     H*                                                              **
     H*  01/07/99 C:94  PROJECT: MC152                               **
     H*                 CHANGED LENGTH OF INV# FROM 5 TO 9           **
     H*                                                              **
     H*  12/17/98 C:94  PROJECT: MC152                               **
     H*                 CHANGED PROGRAM TO USE EOCERT INSTEAD OF     **
     H*                 PBCERT.  ALSO FIXED CHANGE VIEW WHERE DATE   **
     H*                 WAS STILL THERE WHEN YOU CHANGE VIEWS        **
     H*                                                              **
     H*  04/24/98 C:90  PROJ:MC181                                   **
     H*                 SCR01 - CHANGED TO MEET VERSION 4 STANDARDS  **
     H*                 COMBINED ALL SEARCHES INTO F10 SEARCH        **
     H*                                                              **
     H*  05/04/95 C:20  REQ/PROJ:68MAP                               **
     H*                 1. CHANGE CALL TO N0030 OR N0035 TO N0040C   **
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL **** 04/24/98 *************************************
     F*********FFMCMLA  IF  E           K        DISK
     F******F            PCM01                             KRENAMEPCM02
     F*** END   DEL **** 04/24/98 *************************************
     FFMSML     IF   E           K DISK
     FNMARL7    IF   E           K DISK
     FNMARL6    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     F*** BEG CHG *** 04/30/99 ***************************************
     FNDTML1    IF   E           K DISK
     F**********NDTML   IF  E           K        DISK
     F*** END CHG *** 04/30/99 ***************************************
     F*** BEG CHG *** 01/14/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F*** BEGIN ADD *** 09/29/99 *************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 09/29/99 *************************************
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/14/99 ***************************************
     F*** BEGIN ADD **** 04/24/98 *************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     F*** END   ADD **** 04/24/98 *************************************
     F*
     FN0164F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR02S:REC1)
     F                                     SFILE(SCR03S:REC2)
     F                                     INFDS(FIDS01)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*** BEGIN DEL *** 09/29/99 **************************************
     D**********          PTL     1   1 40               PGM TITLES
     D*** END   DEL *** 09/29/99 **************************************
     D*** BEGIN ADD *** 09/29/99 **************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 09/29/99 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SCFRDT                 1      8  0
     D  XXDY                   3      4  0
     D                 DS
     D  XXDTTM                 1     12  0
     D  XXDT                   1      6  0
     D  XXTIME                 7     12  0
     D*** BEG ADD *** 12/17/98 ***************************************
     D                 DS
     D  XXTDT3                 1     10
     D  XXMTH                  1      2
     D  XXSLSH                 3      3
     D  XXDAY                  4      5
     D  X1SLSH                 6      6
     D  XXYEAR                 7     10
     D*** END ADD *** 12/17/98 ***************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   COMPANY FILE                   (SMCOL)
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   SUBSCRIBER FILE                (NMSML)
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#            5 0
     C*
     C*  USER PARAMETER FILE             (FMUPL)
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM DEMOGRAPHICS FILE         (FMCDL)
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER FILE               (FMCML1)
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#                         ADD 11/08/93
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
      **** BEGIN DEL **** 04/24/98 ************************************
     C********** CMKEY2    KLIST
     C******************** KFLD           XXSS#
     C******************** KFLD           XXACDT  70
      **** END   DEL **** 04/24/98 ************************************
     C*
     C*  A/R DETAIL FILE                 (NMARL7)
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     ARKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C                   KFLD                    XXFRDT
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C*
     C     ARKEY3        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C                   KFLD                    XXEOB#
     C*
     C*  TIME/MATERIALS FILE
     C*
     C     TMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    AREOB#
     C*
     C*  EOMB DETAIL FILE
     C*
     C*** BEG CHG *** 01/14/99 ***************************************
     C     EOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    AREOB#
     C*
     C     EOKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X1EOB#
     C********** EOKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           AREOB#
     C*
     C********** EOKY1     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           X1EOB#
     C*** END CHG *** 01/14/99 ***************************************
     C*
     C*  PRE-CERT FILE
     C*
     C*** BEG DEL *** 12/17/98 ****************************************
     C********** PBKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           ARINV#
     C**********           KFLD           AREOB#
     C*** END DEL *** 12/17/98 ****************************************
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
     C     *LIKE         DEFINE    AREOB#        XXEOB#
     C     *LIKE         DEFINE    AREOB#        X1EOB#
     C     *LIKE         DEFINE    CMACDT        XXTODT
     C     *LIKE         DEFINE    CMACDT        XXFRDT
     C     *LIKE         DEFINE    ARTRDT        HLINDT
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        COMP      '2'                                    08    CALLED BY PGM
     C*
     C***  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   Z-ADD     0             REC1              4 0
     C                   Z-ADD     0             REC2              4 0
     C                   Z-ADD     0             REC2HL            4 0
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCFRDT
     C                   Z-ADD     *ZEROS        SCTODT
     C*
     C                   Z-ADD     UPSUB#        SCSUB
     C                   Z-ADD     UPFND         SCFND
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C                   MOVE      SCTODT        SCFRDT
     C                   MOVE      01            XXDY
     C                   MOVE      '0'           INIT              1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C***  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVEA     '111'         *IN(1)
     C  N04              WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVEA     '0000'        *IN(1)
     C                   END                                                    ENDEQ SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVEA     '111'         *IN(1)
     C  N04              WRITE     SCR03
     C                   EXFMT     SCR03C
     C                   MOVEA     '0000'        *IN(1)
     C                   END                                                    ENDEQ SCR03
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*************************************
     C*   F3 = EXIT                       *
     C*************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C*************************************
     C*   F5 = RESTART                    *
     C*************************************
     C*** IF RESTART IS REQUESTED, CLEAR SCR01 FIELDS FIRST
     C*
     C     *INKE         IFEQ      '1'
     C*
     C                   MOVEA     '111'         *IN(21)
     C                   WRITE     SCR02C                                       CLEAR SUBFILE 1
     C                   WRITE     SCR03C                                       CLEAR SUBFILE 2
     C                   MOVEA     '000'         *IN(21)
     C*
     C                   MOVE      '0'           INIT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C                   MOVE      SCTODT        SCFRDT
     C                   MOVE      01            XXDY
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKE
      *
      **** BEGIN CHG **** 04/24/98 ************************************
      *
     C**********************************
     C*   F10=SEARCH                   *
     C**********************************
      *
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
      *
     C**********************************
     C*   SUBSCRIBER SEARCH            *
     C**********************************
      *
     C     HFLD          IFEQ      'SCSUB'
     C                   CALL      'F0055R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     30
      *
     C     QQRRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  HFLD SCSUB
      *********************************
     C*   GROUP SEARCH                *
      *********************************
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          IFEQ      'SCFND'
     C*****              MOVE      QQRRNS        QQRRNF
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           QQCALL            1
     C******INKC         PARM                    QQKC              1
     C******INKL         PARM                    QQKL              1
     C*****              PARM                    QQRRNF            9 0
      *****
     C******INKC         CABEQ     *ON           TERM
      *****
     C******INKL         CABEQ     *ON           STRT                     31
      *****
     C*****QQRRNF        CHAIN     PFN02                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN32
     C*****              ENDIF                                                  IFEQ *IN60
      *****
     C*****              ENDIF                                                  HFLD SCFND
     C******** End Del ***** 12/18/24 ***************************
      *********************************
     C*   YEAR & CASE SEARCH          *
      *********************************
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C                   MOVE      SCSUB         QQSUB#
     C                   MOVE      SCFND         QQFND
     C                   CALL      'F0269R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM      0             QQFYR             3 0
     C                   PARM      0             QQCASE            7 0
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8 0
     C                   PARM                    QQRTCD            1
      *
     C     QQFYR         IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCFYR'       STRT                     32    PC AT SCFYR
     C                   ENDIF                                                  IFEQ QQFYR
     C     QQCASE        IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCCASE'      STRT                     33    PC AT SCCASE
     C                   ENDIF                                                  IFEQ QQCASE
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN34
     C                   MOVE      QQSUB#        SCSUB
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   ENDIF                                                  IFEQ '*'
      *
     C                   ENDIF                                                  HFLD SCFYR
      *
     C     HFLD          IFEQ      'SCFRDT'
     C     HFLD          OREQ      'SCTODT'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  HFLD SCFRDT
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  IFEQ *INKJ
     C**********************************
     C**********   COMMAND 06 = SUB SEARCH      *
     C**********************************
     C********** *INKF     IFEQ '1'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** QQRTC     OREQ '*'
     C********** SCSUB     OREQ *ZERO
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END B/*/0
     C******************** END                             END *INKF
     C*************************************
     C***********   F7 = GROUP SEARCH               *
     C*************************************
     C*
     C********** *INKG     IFEQ '1'
     C*
     C******************** CALL 'F0508R'
     C******************** PARM           QQPRDS 40
     C******************** PARM           QQCALL  1
     C******************** PARM           SCSUB
     C******************** PARM           SCFND
     C******************** PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR98
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C********** QQRTCD    OREQ '*'
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             ENDIF *INKG
     C*************************************
     C*   F8 = CASE # SEARCH              *
     C*************************************
     C*
     C********** *INKH     IFEQ '1'
     C*
     C******************** MOVE *BLANKS   XXSS#
     C******************** MOVE SCFND     XXFND
     C******************** CALL 'F0570R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCFYR
     C******************** PARM           SCCASE
     C******************** PARM           XXSS#
     C******************** PARM           QQRTCD
     C******************** PARM           XXFND   30       REQ. FOR SRC
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR98
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCFYR     OREQ *ZERO
     C******************** GOTO STRT
     C******************** END                             ENDIF 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** GOTO SCR01T
     C******************** END                             ENDIF '*'
     C*
     C******************** END                             ENDIF *INKH
     C*************************************
     C*********  COMMAND 9 - ALPHA SEARCH         *
     C*************************************
     C*
     C********** *INKI     IFEQ '1'
     C*
     C******************** MOVE *BLANK    XXSS#   9
     C******************** Z-ADD*ZEROS    XXACDT
     C******************** CALL 'F0578R1'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           XXSS#
     C******************** PARM           XXACDT
     C******************** PARM           SCSUB
     C******************** PARM           SCFND
     C******************** PARM           SCFYR
     C******************** PARM           SCCASE
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCFYR     OREQ *ZEROS
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE UPCO#     QQCO#   3
     C******************** MOVE SCSUB     QQSUB#  5
     C******************** MOVE SCFND     QQFND   3
     C******************** CALL 'F0006R2'
     C******************** PARM           QQCO#
     C******************** PARM           QQSUB#
     C******************** PARM           QQFND
     C******************** PARM           QQRTCD
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             ENDIF *INKI
     C*************************************
     C***********  COMMAND 11 - SS#/ACC DATE SEARCH *
     C*************************************
     C********** *INKK     IFEQ '1'
     C*
     C******************** MOVE *BLANK    XXSS#   9
     C******************** Z-ADD*ZEROS    XXACDT
     C******************** CALL 'F0573R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           XXSS#
     C******************** PARM           XXACDT
     C******************** PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR98
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** XXSS#     OREQ *BLANK
     C******************** GOTO STRT
     C******************** END                             ENDIF 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** Z-ADDXXACDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     XXACDT
     C********** CMKEY2    CHAINPCM02                60
     C********** *IN60     IFEQ '0'
     C******************** MOVE CMSUB#    SCSUB
     C******************** MOVE CMFND     SCFND
     C******************** MOVE CMFYR     SCFYR
     C******************** MOVE CMCASE    SCCASE
     C******************** GOTO SCR01T
     C******************** END                             ENDIF *IN60
     C******************** END                             ENDIF '*'
     C*
     C******************** END                             ENDIF *INKK
     C*
     C*** BEG CHG *** 01/22/99 ****************************************
     C*************************************
     C**********   F12 = PREVIOUS                  *
     C*************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      TERM
     C                   END                                                    ENDIF SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C*
     C                   MOVEA     '111'         *IN(21)
     C                   WRITE     SCR02C                                       CLEAR SUBFILE 1
     C                   MOVEA     '000'         *IN(21)
     C*
     C                   MOVE      '0'           INIT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR02
     C*
     C                   END                                                    ENDIF *INKL
     C********** *INKL     IFEQ '1'
     C*
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** GOTO TERM
     C******************** END                             ENDIF SCR01
     C*
     C********** Q2RECF    IFEQ 'SCR02'
     C********** Q2RECF    OREQ 'SCR02C'
     C********** Q2RECF    OREQ 'SCR02S'
     C*
     C******************** MOVEA'111'     *IN,21
     C******************** WRITESCR02C                     CLEAR SUBFILE 1
     C******************** MOVEA'000'     *IN,21
     C*
     C******************** MOVE '0'       INIT
     C*
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             ENDIF SCR02
     C*
     C******************** END                             ENDIF *INKL
     C*** END CHG *** 01/22/99 ****************************************
     C*************************************
     C*********   COMMAND 13 = CHANGE USER PARMS  *
     C*************************************
     C********** *INKM     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** CALL 'F0006R'
     C******************** WRITESCR98
     C*
     C**********  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** MOVE *ZERO     UPFND
     C******************** END
     C******************** GOTO TOP
     C******************** END
     C*
     C******************** END                             ENDIF *INKM
     C****
      **** END   CHG **** 04/24/98 ************************************
     C                   GOTO      STRT
     C                   END                                                    END CMDKEY
     C*****************************************************************
     C*
     C***  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C**************************************
     C*    PROCESS REQUEST SCREEN - SCR01  *
     C**************************************
     C     SCR01T        TAG
     C*
     C                   Z-ADD     SCSUB         XXSUB#
     C*
     C*** SUBSCRIBER VALIDATION
     C*
     C     SMKEY         CHAIN     PSM01                              25        INVALID SUBS
     C     *IN25         IFEQ      '1'
     C                   MOVE      *BLANK        SCSBNM
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*** CASE VALIDATION
     C                   Z-ADD     SCFND         XXFND             3 0
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C                   Z-ADD     SCCASE        XXCASE            7 0
     C     CMKEY         CHAIN     PCM01                              21        INVALID CASE
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*** FROM DATE VALIDATION
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         CABEQ     '1'           STRT                     22    INVALID FRDT
     C*
     C*** TO DATE VALIDATION
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         CABEQ     '1'           STRT                     23    INVALID TODT
     C*
     C*** VALIDATE THE FROM DATE IS LE TODATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C     XXTODT        CABLT     XXFRDT        STRT                   24      TODT B4 FRDT
     C*
     C*** LOAD SCR02 DATA
     C*
     C* * * *  SS#/ ACCIDENT DATE
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C* * * *  LAST,FIRST NAME SUBPROGRAM FOR CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C                   MOVEL     ' '           SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        SCCLNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   END                                                    ENDIF *IN60
     C***       SUBROUTINE TO GET AND LOAD THE ACCOUNTS RECEIVABLE DETAIL
     C***       SUBFILE.
     C*
     C     INIT          IFNE      '1'                                          SETLL ONLY 1ST
     C     ARKEY         CHAIN     PAR01                              53
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      ARINV#        HLINV#            9 0
     C**********           MOVE ARINV#    HLINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C                   Z-ADD     0             REC1              4 0
     C                   Z-ADD     0             SCPBAL
     C                   Z-ADD     0             SCIAMT
     C                   Z-ADD     0             SCPAMT
     C                   Z-ADD     0             SCNAMT
     C                   Z-ADD     0             SCVAMT
     C                   Z-ADD     0             SCAAMT
     C                   Z-ADD     0             SCADUE
     C                   Z-ADD     0             XXREC1            5 0
     C                   EXSR      ARLOAD
     C                   MOVE      '1'           INIT              1
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF                         GOTO DISPLAY
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*************************************
     C*   PROCESS REQUEST SCREEN - SCR02  *
     C*************************************
     C     SCR02T        TAG
     C*
     C* VALIDATE THE SECLECTION FIELD IS EITHER A '5' OR BLANKS IF IT IS
     C* ANYTHING ELSE SEND ERROR MESSAGE
     C*
     C                   EXSR      EDIT2
     C*
     C* CHAIN TO THE SUBFILE TO PROCESS A SELECTION OF '5'
     C*
     C                   MOVE      REC1          SREC1             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT2S2        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR02S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCSEL         IFEQ      '5'
     C                   Z-ADD     SCINV#        XXINV#
     C                   Z-ADD     K             SRCD#1
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR02S
     C                   MOVE      '0'           *IN39
     C*
     C* CHAIN TO A/R FILE W/INV#
     C*
     C     CHGVEW        TAG
     C     ARKEY2        CHAIN     PAR02                              53
     C     ARREPO        TAG
     C                   Z-ADD     0             XXREC2            5 0
     C                   MOVE      '1'           *IN23                          CLEAR SUBFILE
     C                   WRITE     SCR03C                                       CLEAR SUBFILE
     C                   MOVE      '0'           *IN23
     C                   Z-ADD     0             REC2
     C*
     C* GET INVOICE DETAIL INFORMATION
     C*
     C     ARROLL        TAG
     C                   EXSR      ARDETL
     C     TMRTRN        TAG
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   MOVEA     '111'         *IN(1)                         SUBFILE DISPLAY
     C                   WRITE     SCR03                                        SUBFILE DISPLAY
     C                   EXFMT     SCR03C
     C                   MOVEA     '0000'        *IN(1)
     C*
     C* WE WILL HAVE TO DO ALL SCR03 NORMAL PROCESSING HERE INSTEAD OF OTHER
     C* PLACES BEACUSE WE ARE IN LOOP LOOKING AT SELECTION FIELD FROM SCR02
     C*
     C* VERIFY IF REPOSITION WAS SELECTED
     C*
     C     SCEOB1        IFNE      *ZEROS
     C                   Z-ADD     SCEOB1        XXEOB#
     C                   Z-ADD     *ZEROS        SCEOB1
     C     ARKEY3        SETLL     PAR02
     C     ARKEY         READE     PAR02                                  53
     C                   GOTO      ARREPO
     C                   END                                                    END SCEOB1
     C*
     C* VERIFY IF FUNCTION KEY WAS PRESSED FORM SCR03
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C*
     C* F11 = CHG VEIW
     C*
     C     *INKK         IFEQ      '1'
     C*
     C     *IN39         IFEQ      '0'
     C                   MOVE      '1'           *IN39
     C                   ELSE
     C                   MOVE      '0'           *IN39
     C                   END
     C*
     C                   GOTO      CHGVEW
     C*
     C                   END                                                    END *INKK
     C*
     C* F12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT3S3        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR03S                             75
     C     *IN75         IFEQ      '0'
     C                   MOVE      *BLANKS       SCSE1
     C                   MOVE      '0'           *IN24
     C                   MOVE      '0'           *IN25
     C                   UPDATE    SCR03S
     C                   GOTO      NXT3S3
     C                   END                                                    END *IN75
     C*
     C                   GOTO      F12TAG
     C                   END                                                    ENDIF *INKL
     C*
     C* ROLLUP
     C*
     C     *IN33         IFEQ      '1'
     C                   GOTO      ARROLL
     C                   END
     C*
     C                   END                                                    END *IN11
     C*
     C* CHAIN TO THE SUBFILE TO PROCESS A SELECTION OF '6'
     C*
     C                   MOVE      REC2          SREC2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT3S1        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR03S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCSE1         IFEQ      '6'
     C     SCTM          ANDEQ     '*'
     C                   Z-ADD     K             SRCD#
     C                   MOVE      *BLANKS       SCSE1
     C                   MOVE      '0'           *IN24
     C                   MOVE      '0'           *IN25
     C                   UPDATE    SCR03S
     C*
     C* CALL T/M MAINTENANCE IN INQUIRY MODE
     C*
     C                   MOVE      SCEOB#        X1EOB#
     C*
     C     EOKY1         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 09/29/99 **************************************
     C*
     C**********           MOVE *BLANKS   Q1PRDS 40
     C**********           MOVELPTL,1     Q1PRDS
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C**********           MOVE UPCO#     Q1CO#   3
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C**********           MOVE SCSUB     Q1SUB#  5
     C**********           MOVE SCFND     Q1FND   3
     C**********           MOVE EOFYR     Q1FYR   3
     C**********           MOVE EOCASE    Q1CASE  7
     C**********           MOVE SCEOB#    Q1EOB#  9
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C**********           MOVE SCSYS     Q1SYS   1
     C**********           CALL 'N0040C'
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C**********           PARM           Q1PRDS
     C**********           PARM '3'       Q1CALL  1
     C**********           PARM           Q1CO#
     C**********           PARM           Q1SUB#
     C**********           PARM           Q1FND
     C**********           PARM           Q1FYR
     C**********           PARM           Q1CASE
     C**********           PARM           Q1EOB#
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C**********           PARM           Q1SYS
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C**********
     C                   CALL      'F3040R'
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C                   PARM      'Y'           ##EOB             1
     C*** END   CHG *** 09/29/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C                   END                                                    END *IN60
     C*
     C                   ELSE                                                   ELSE SCSE1
     C     SCSE1         IFNE      *BLANKS
     C                   MOVE      '1'           *IN25
     C                   MOVE      '0'           *IN24
     C                   UPDATE    SCR03S
     C                   MOVE      SREC2         REC2
     C                   Z-ADD     K             SRCD#
     C                   GOTO      TMRTRN
     C                   ELSE
     C                   MOVE      '0'           *IN24
     C                   MOVE      '0'           *IN25
     C                   UPDATE    SCR03S
     C                   END
     C*
     C                   END                                                    END SCSE1 '6'
     C*
     C* PROCESS THE NEXT SUBFILE RECORD
     C*
     C                   GOTO      NXT3S1
     C                   END                                                    END *IN75
     C                   GOTO      TMRTRN
     C*
     C                   END                                                    END SCSEL '5'
     C*
     C* PROCESS THE NEXT SUBFILE RECORD
     C*
     C                   GOTO      NXT2S2
     C                   END                                                    END *IN75
     C*
     C* RE-DISPLAY SCR02
     C*
     C     F12TAG        TAG
     C                   MOVE      SREC1         REC1
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      * ACCOUNTS RECEIVABLE LOAD TO SFL **SUBROUTINE**       A R L O A D *
      * SUMMARY SECTIONS ONLY                                            *
      *==================================================================*
     C*
     C     ARLOAD        BEGSR
     C                   Z-ADD     0             SCINAM
     C                   Z-ADD     0             SCPAAM
     C                   Z-ADD     0             SCAJAM
     C                   Z-ADD     0             SCTDUE
     C                   Z-ADD     0             X                 5 0
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
     C     *IN53         DOWEQ     '0'
     C     ARTRDT        ANDLE     XXTODT
     C     X             ANDLT     99999
     C*
     C*** CREATE HEADING A/R INFO ***
     C*
     C*  A/R TRANSACTION DATE COMPARED TO SCREEN FROM DATE
     C*
     C     ARTRDT        IFLT      XXFRDT
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         SCPBAL
     C                   ELSE
     C                   ADD       ARAMT         SCPBAL
     C                   END
     C                   END
     C*
     C*  A/R TRANSACTION DATE WITHIN DATE RANGE
     C*
     C     ARTRDT        IFGE      XXFRDT
     C     ARTRDT        ANDLE     XXTODT
     C*
     C*** HOLD THE FIRST INVOICE THAT FALLS WITHIN THE RANGE TO USE
     C*** IN SETLL/READE IN ARDETL SUBROUTINE
     C     HOLD          IFEQ      'N'
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C**********           Z-ADDARINV#    XXINV#  50
     C*** END CHG *** XX/XX/XX ***************************************
     C                   MOVE      'Y'           HOLD              1
     C                   END
     C*
     C     ARTRTP        IFEQ      'P'
     C                   ADD       ARAMT         SCPAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'A'
     C                   ADD       ARAMT         SCAAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'I'
     C                   ADD       ARAMT         SCIAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'V'
     C                   ADD       ARAMT         SCVAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'S'
     C                   ADD       ARAMT         SCSAMT
     C                   END
     C*
     C     ARTRTP        IFEQ      'N'
     C                   ADD       ARAMT         SCNAMT
     C                   END
     C*
     C                   END                                                    END GE/LE
     C*
     C*  ADD TO INVOICE SUMMARY BUCKETS REGARDLESS OF DATE
     C*
     C     ARTRTP        IFEQ      'P'
     C                   ADD       ARAMT         SCPAAM
     C                   END
     C*
     C     ARTRTP        IFEQ      'A'
     C                   ADD       ARAMT         SCAJAM
     C                   END
     C*
     C     ARTRTP        IFEQ      'I'
     C                   ADD       ARAMT         SCINAM
     C     ARTRDT        IFLT      HLINDT
     C     HLINDT        OREQ      *ZERO
     C                   Z-ADD     ARTRDT        HLINDT
     C                   END
     C                   END
     C*
     C                   ADD       1             X
     C*
     C     ARKEY         READE     PAR01                                  53
     C*
     C*  IF EOF OR INV# CHANGES, WRITE INVOICE SUMMARY RECORD (SCR02S)
     C*
     C     *IN53         IFEQ      '1'
     C     ARINV#        ORNE      HLINV#
     C                   Z-ADD     HLINV#        SCINV#
     C                   Z-ADD     HLINDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINDT
     C     SCINAM        SUB       SCPAAM        SCTDUE
     C                   ADD       SCAJAM        SCTDUE
     C     HLINDT        IFLT      XXFRDT
     C                   ADD       1             REC2HL
     C                   END
     C                   ADD       1             REC1
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     REC1          XXREC1
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   WRITE     SCR02S
     C                   Z-ADD     0             SCINAM
     C                   Z-ADD     0             SCPAAM
     C                   Z-ADD     0             SCAJAM
     C                   Z-ADD     0             SCTDUE
     C                   Z-ADD     0             SCINDT
     C                   Z-ADD     0             HLINDT
     C                   Z-ADD     ARINV#        HLINV#
     C*  RESET COUNTER TO ELIMINATE AN EXTRA RECORD ON SUMMARY LINE
     C                   Z-ADD     0             X
     C                   END                                                    END EQ *IN53
     C*
     C                   END                                                    END DOWEQ 53
     C*
     C*** CALCULATE CURRENT ACTIVITY DUE
     C*
     C     SCPBAL        SUB       SCPAMT        SCADUE
     C                   ADD       SCAAMT        SCADUE
     C                   ADD       SCIAMT        SCADUE
     C                   ADD       SCVAMT        SCADUE
     C                   ADD       SCSAMT        SCADUE
     C                   ADD       SCNAMT        SCADUE
     C*
     C*** IF AT LEAST ONE RECORD WAS READ THEN DISPLAY THE SUMMARY LINE
     C*
     C     X             IFGT      0
     C                   Z-ADD     HLINDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINDT
     C                   Z-ADD     HLINV#        SCINV#
     C     SCINAM        SUB       SCPAAM        SCTDUE
     C                   ADD       SCAJAM        SCTDUE
     C                   ADD       1             REC1
     C                   WRITE     SCR02S
     C                   Z-ADD     0             SCINAM
     C                   Z-ADD     0             SCPAAM
     C                   Z-ADD     0             SCAJAM
     C                   Z-ADD     0             SCTDUE
     C                   Z-ADD     0             SCINDT
     C                   Z-ADD     0             HLINDT
     C                   Z-ADD     ARINV#        HLINV#
     C                   END                                                    END X GT 0
     C*
     C*** CHECK IF NO RECORDS HAVE BEEN WRITTEN TO SFL, WRITE ONE BLANK
     C*
     C     REC1          IFLT      1
     C                   MOVE      *ZERO         SCINDT
     C                   MOVE      *ZERO         SCINV#
     C                   MOVE      *ZERO         SCINAM
     C                   MOVE      *BLANK        SCPAAM
     C                   MOVE      *BLANK        SCAJAM
     C                   MOVE      *BLANK        SCTDUE
     C                   ADD       1             REC1
     C                   Z-ADD     0             REC2HL
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     REC1          XXREC1
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   WRITE     SCR02S
     C                   END
     C*
     C                   ADD       1             REC2HL
     C*
     C                   Z-ADD     XXREC1        SRCD#1
     C*
     C                   ENDSR
      *==================================================================*
      * ACCOUNTS RECEIVABLE LOAD TO SFL **SUBROUTINE**       A R D E T L *
      *==================================================================*
     C*
     C     ARDETL        BEGSR
     C                   Z-ADD     0             X                 5 0
     C*
     C                   MOVE      'Y'           PCFL1             1
     C*
     C     *IN53         DOWEQ     '0'
     C     X             ANDLT     10
     C*
     C     ARINV#        IFEQ      XXINV#
     C                   MOVE      AREOB#        SCEOB#
     C     AREOB#        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       SCEOB#
     C                   END
     C                   Z-ADD     AREOB#        SCEOB
     C                   MOVE      ARSYS         SCSYS
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTDT3
     C*** BEG ADD *** 12/17/98 **************************************
     C                   MOVE      '/'           XXSLSH
     C                   MOVE      '/'           X1SLSH
     C                   MOVEL     SCTDT3        XXMTH
     C                   MOVE      SCTDT3        X1TDT3            6
     C                   MOVEL     X1TDT3        XXDAY
     C                   MOVE      X1TDT3        XXYEAR
     C  N39              MOVEL(P)  SCEOB#        SCDATA
     C  N39              MOVE      XXTDT3        SCDATA
     C*** END ADD *** 12/17/98 **************************************
     C                   Z-ADD     ARPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPDT3
     C                   MOVEL     ARTRCD        SCTCD3
     C                   MOVEL     ARTRTP        SCTRTP
     C                   MOVEL     ARDESC        SCDSC3
     C                   Z-ADD     ARAMT         SCAMT3
     C                   Z-ADD     ARCK#         SCCHK3
     C                   MOVE      *BLANKS       SCSE1
     C                   MOVE      '0'           *IN24
     C                   MOVE      '0'           *IN25
     C*
     C* WE MUST CHAIN TO EOMB FILE TO GET CORRECT CASE # SO WE CAN CHAIN
     C* TO T/M FILE
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* SEE IF EOMB HAS TIME/MATERIALS CHARGES PROCESSED AGAINST IT
     C*
     C     TMKEY         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           SCTM
     C                   ELSE
     C                   MOVE      *BLANKS       SCTM
     C                   END                                                    END TMKEY
     C*** BEG ADD *** 12/17/98 ***************************************
     C     *IN39         IFEQ      '1'
     C                   MOVE      *BLANKS       SCDATA
     C                   MOVEL     EOCERT        SCDATA
     C                   ENDIF
     C*** END ADD *** 12/17/98 ***************************************
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCTM
     C*** BEG ADD *** 12/17/98 ***************************************
     C     *IN39         IFEQ      '1'
     C                   MOVE      *BLANKS       SCDATA
     C                   ENDIF
     C*** END ADD *** 12/17/98 ***************************************
     C                   END                                                    END EOKEY
     C*
     C* GET THE PRE-CERT NUMBER IF TO BE DISPLAYED
     C*
     C*** BEG DEL *** 12/17/98 ***************************************
     C********** *IN39     IFEQ '1'
     C**********           MOVE *BLANKS   SCEOB#
     C********** PBKEY     CHAINPPB01                60
     C********** *IN60     IFEQ '0'
     C**********           MOVELPBCERT    SCEOB#
     C**********           ELSE
     C**********           MOVE *BLANKS   SCEOB#
     C**********           END                             END PBKEY
     C**********           END                             END *IN39
     C*** END DEL *** 12/17/98 ***************************************
     C*
     C                   ADD       1             X
     C                   ADD       1             REC2
     C*
     C     PCFL1         IFEQ      'Y'
     C                   Z-ADD     REC2          XXREC2
     C                   MOVE      'N'           PCFL1
     C                   END
     C*
     C                   WRITE     SCR03S
     C                   END                                                    END EQ XXINV#
     C*
     C     ARKEY2        READE     PAR02                                  53
     C                   END                                                    END DOWEQ 53
     C*
     C*  CONDITION THE ROLLUP FUNCTION
     C*
     C     *IN53         IFEQ      '0'
     C     ARTRDT        IFLE      XXTODT
     C                   MOVE      '1'           *IN93
     C                   ELSE                                                   ELSE 0 *IN53
     C                   MOVE      '0'           *IN93
     C                   END                                                    END 0 *IN53
     C*
     C                   ELSE                                                   ELSE 0 *IN53
     C                   MOVE      '0'           *IN93
     C                   END                                                    END 0 *IN53
     C*
     C     REC2          IFLT      1
     C                   MOVE      *ZERO         SCEOB#
     C                   MOVE      *BLANK        SCSYS
     C                   MOVE      *ZERO         SCTDT3
     C                   MOVE      *ZERO         SCPDT3
     C                   MOVE      *BLANK        SCTCD3
     C                   MOVE      *BLANK        SCTRTP
     C                   MOVE      *BLANK        SCDSC3
     C                   MOVE      *ZERO         SCAMT3
     C                   MOVE      *ZERO         SCCHK3
     C                   ADD       1             REC2
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL1         IFEQ      'Y'
     C                   Z-ADD     REC2          XXREC2
     C                   MOVE      'N'           PCFL1
     C                   END
     C*
     C                   WRITE     SCR03S
     C                   END
     C*
     C                   Z-ADD     XXREC2        SRCD#
     C*
     C                   ENDSR
     C*****************************************************************
     C* E D I T 2  - SUBROUTINE TO EDIT SCR02 SCREEN DATA
     C*****************************************************************
     C     EDIT2         BEGSR
     C*
     C                   MOVE      REC1          SREC1             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT2S1        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR02S                             75
     C     *IN75         IFEQ      '0'
     C*
     C* SCSEL MUST BE A 5 OR BLANKS ELSE SEND ERROR MESSAGE
     C*
     C     SCSEL         IFNE      '5'
     C     SCSEL         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR02S
     C                   MOVE      SREC1         REC1
     C                   Z-ADD     K             SRCD#1
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END                                                    END SCSEL
     C*
     C     *IN20         IFEQ      '0'
     C                   UPDATE    SCR02S
     C                   END
     C*
     C                   GOTO      NXT2S1
     C                   END                                                    END *IN75
     C                   MOVE      SREC1         REC1
     C*
     C                   ENDSR
     C**  PTL - PROGRAM TITLES (PASSED IN Q1PRDS)
     C**    TIME/MATERIALS MAINTENANCE
