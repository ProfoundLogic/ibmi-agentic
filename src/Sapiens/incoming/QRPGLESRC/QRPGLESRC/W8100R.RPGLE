     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: W8100R                                            *
     H*                                                               *
     H*    DESCRIPTION: PROMPT FOR LEGION DATA DOWNLOAD               *
     H*                                                               *
     H*    CREATED BY:  C:33   03/28/96   PROJECT# 65WAY              *
     H*    REWRITE V3:  C:33   03/26/98   PROJECT# 65WCG              *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      W8100C - SUBMITTER                                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H*  03/21/00  C:33  PROJECT: 65WCG   WA065/22000                 *
     H*                  CALCULATE JULIAN DATE CORRECTLY.             *
     H*****************************************************************
     FW8100F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
     FWMFNL     IF   E           K DISK

     D*
     D OPTNM           C                   CONST('Legion Data Download')
     D*
     D* TO DATE
     D                 DS
     D  XXDT                   1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXCC                   5      6  0
     D  XXYY                   7      8  0
     D  XXCY                   5      8  0
     D                 DS
     D  ZZDT                   1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZCC                   5      6  0
     D  ZZYY                   7      8  0
     D  ZZCY                   5      8  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    FNDT          XXFDT
     C     *LIKE         DEFINE    FNDT          XXTDT
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*_______________________________________________________________
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
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C*_______________________________________________________________
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        ZZDT
     C                   Z-ADD     1             ZZDD
     C                   SUB       1             ZZMM
     C*
     C                   Z-ADD     ZZDT          SCFRDT
     C*
     C                   SELECT
     C     ZZMM          WHENEQ    4
     C     ZZMM          OREQ      6
     C     ZZMM          OREQ      9
     C     ZZMM          OREQ      11
     C                   Z-ADD     30            ZZDD
     C     ZZMM          WHENEQ    2
     C                   Z-ADD     28            ZZDD
     C                   OTHER
     C                   Z-ADD     31            ZZDD
     C                   ENDSL
     C*
     C                   Z-ADD     ZZDT          SCTODT
     C*
     C                   MOVE      'N'           SCTPFL
     C*_______________________________________________________________
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         UPCO#
     C                   Z-ADD     *ZERO         UPFND
     C                   ENDIF
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C*
     C*  GET COMPANY NAME
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       CONAME
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET GROUP NAME
     C*
     C                   Z-ADD     XXFND         SCFND
     C                   MOVE      *BLANKS       SCFDNM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFDNM
     C*
     C*  Load the Record Format Information for the First Time
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*_______________________________________________________________
     C*
     C*  Display Screen Formats
     C*
     C     STRT          TAG
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*  Process All Command Keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*===============================================================
     C*  F3 = EXIT
     C*===============================================================
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   SELECT
     C*
     C*===============================================================
     C*  F13 = CHG USER PARMS
     C*===============================================================
     C*
     C     *INKM         WHENEQ    *ON
     C*
     C                   CALL      'W0006R'
     C*
     C                   GOTO      TOP
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C     SCR01T        TAG
     C*
     C*  Validate the Entered "FROM DATE"
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     20
     C*
     C*  Validate the Entered "TO DATE"
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
     C*
     C*  Validate the Entered Dates to Verify Ascending Order
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C     XXFDT         CABGT     XXTDT         STRT                 22
     C*
     C*  Copy to tape must be 'Y' or 'N'
     C*
     C     SCTPFL        IFNE      'Y'
     C     SCTPFL        ANDNE     'N'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTPFL<>Y/N
     C*
     C*  DEVICE MUST BE VALID, SAVE TO TAPE IS 'Y'
     C*
     C     SCTPFL        IFEQ      'Y'
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    SCDEV
     C                   PARM      '*DEVD'       ##TYPE            7
     C                   PARM                    ##ERR             1
     C*
     C                   MOVE      ##ERR         *IN24
     C     *IN24         CABEQ     *ON           STRT
     C*
     C                   ENDIF                                                  SCTPFL=Y
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*_______________________________________________________________
     C*
     C*  Get the 'TO' Julian Date
     C*
     C                   Z-ADD     SCTODT        XXDT
     C                   Z-ADD     12            ZZMM
     C                   Z-ADD     31            ZZDD
     C*
     C***  BEGIN CHG  ***  03/21/00  *********************************
     C**********
     C********** XXYY      IFNE *ZERO
     C********** XXYY      SUB  1         ZZYY
     C**********           ELSE
     C**********           Z-ADD99        ZZYY
     C**********           ENDIF                           XXYY<>*ZERO
     C     XXCY          SUB       1             ZZCY
     C***  END   CHG  ***  03/21/00  *********************************
     C*
     C*  Call sub-program SRDDIF8 to Calc Julian Date
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      ZZDT          SRFRDC            8 0
     C                   PARM      XXDT          SRTODC            8 0
     C                   PARM      *ZEROS        SRDAYS            5 0
     C                   PARM      *BLANK        SRERR             1
     C*
     C                   Z-ADD     SRDAYS        XXJDT             3 0
     C*
     C*  Call the CLP to Submit the Program for creating tape workfiles
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      SCFRDT        ##FDT             8
     C                   MOVE      SCTODT        ##TDT             8
     C                   MOVE      XXJDT         ##JDT             3
     C*
     C                   CALL      'W8100C'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FDT
     C                   PARM                    ##TDT
     C                   PARM                    ##JDT
     C                   PARM      SCTPFL        ##TPFL            1
     C                   PARM      SCDEV         ##DEV            10
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
