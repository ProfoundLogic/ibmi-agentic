     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0054R - ENDORSEMENT FORM INFO MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                ENDORSEMENT FORM INFO FOR A GROUP/STATE.       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH GROUPS                                  *
     H*  CALLS TO:                                                    *
     H*    W0054R02 - ENDORSEMENT FORM INFO DATA SCREENS              *
     H*                                                               *
     H*  CREATED: 12/03/97  PROGRAMMER: C:57  PROJECT: CM027          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 02/15/08 C:190 Project: WC1425                                *
     H*                Change the title of this option from           *
     H*                "Endorsement Forms Information" to             *
     H*                "Endors/Policy Form Info.                      *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  30 - Called as a search                                      *
     H*  31 - Called as a search w/YR                                 *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  93 - Used to display CPF6A7C; Option invalid                 *
     H*  94 - Used to display CPF5204; Function Key invalid           *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0054F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E             DISK
     FWMNXL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PNX01:DBRCDF)
     FWMNXP     IF   E             DISK
     F                                     RENAME(PNX01:PFRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              COLUMN HEADINGS
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D**************BEG CHG****02/15/08*******************************
     D OPTNM           C                   CONST('Endors/Policy Form -
     D                                     Info')
     D**********    'Endorsement Forms I- C         OPTNM
     D*********     'nformation'
     D**************END CHG****02/15/08*******************************
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0054R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     C*
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    FNNAME        KKNAME
     C     *LIKE         DEFINE    NXCO#         XXCO#
     C     *LIKE         DEFINE    NXST          XXST
     C     *LIKE         DEFINE    NXEFDT        XXEFDT
     C     *LIKE         DEFINE    SCEFDT        XXEFD2
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     KEY1C         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C     KEY1S         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSSTAT<>*BLANKS
     C*
     C                   ENDSR                                                  CHKSSM
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*===============================================================
     C* CVTDT2 - SUBROUTINE TO CONVERT DATE FROM MDY TO CYMD
     C*===============================================================
     C     CVTDT2        BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT2
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK2
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C*
     C                   ENDSR                                                  INZSST
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C     KEYINZ        BEGSR
     C*
     C  N32              MOVE      *LOVAL        XXST
     C                   MOVE      *HIVAL        XXEFDT
     C*
     C                   ENDSR                                                  KEYINZ
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C     KEYSET        BEGSR
     C*
     C  N32              MOVE      SCPST1        XXST
     C*
     C     SCPST2        IFEQ      *BLANKS
     C                   MOVE      *HIVAL        XXEFDT
     C*
     C                   ELSE
     C*
     C                   MOVE      SCPST2        XXEFD2
     C*
     C                   Z-ADD     XXEFD2        SRDTI
     C                   EXSR      CVTDT2
     C                   MOVEL     SRDTO         XXEFDT
     C                   ENDIF                                                  SCPST2 EQ 0
     C*
     C                   ENDSR                                                  KEYSET
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C     KEYSFL        BEGSR
     C*
     C  N32              MOVE      SCST          XXST
     C                   MOVE      SCEFDT        XXEFD2
     C*
     C                   Z-ADD     XXEFD2        SRDTI
     C                   EXSR      CVTDT2
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   ENDSR                                                  KEYSFL
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      NXST          SCST
     C*
     C                   Z-ADD     NXEFDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      *BLANKS       SCDEL
     C                   MOVE      NXSTAT        SCSTAT
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     @@CALL        COMP      '3'                                    31    SEARCH W/YR
     C*
     C   31              MOVEL     '2'           @@CALL
     C*
     C     @@ST          IFNE      *BLANKS
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  @@ST IFNE
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C                   MOVE      FNCO#         XXCO#
     C                   MOVE      FNCO#         ##CO#
     C                   MOVE      FNFND         SCFND
     C                   MOVE      FNNAME        SCFNAM
     C                   MOVE      *BLANKS       SCPST1
     C                   MOVE      *BLANKS       SCPST2
     C                   MOVE      @@ST          SCPSTS
     C                   MOVE      @@ST          XXST
     C                   Z-ADD     0             @@RRN
     C*
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C     READ          BEGSR
     C*
     C   21
     CANN32KEY1C         READE     DBRCDF                                 95
     C   21
     CAN 32KEY1S         READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C     READP         BEGSR
     C*
     C   21
     CANN32KEY1C         READPE    DBRCDF                                 95
     C   21
     CAN 32KEY1S         READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL     DELETD        SCDEL
     C*
     C                   ENDSR                                                  SF1UPD
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     NXSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
     C*===============================================================
** Column Headings
Opt  State   Eff Date  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
