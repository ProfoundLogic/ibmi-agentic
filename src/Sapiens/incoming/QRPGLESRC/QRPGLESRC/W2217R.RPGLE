     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2217R - WORK WITH COUNTY CODES SELECTIONS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL COUNTY *
     H*                CODES SELECTED FOR A REPORT.                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    VARIOUS                                                    *
     H*  CALLS TO:                                                    *
     H*    W2217R02 - COUNTY CODE SELECTION UPDATE (OPT 20)           *
     H*                                                               *
     H*  CREATED: 05/17/98  PROGRAMMER: C:33  PROJECT: WC701          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*****************************************************************
     FW2217F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PFRCDF)
     FWTABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCN01:DBRCDF)
     FWTCNL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCN01:DBRCDF2)
     FWACNL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('County Code Selectio-
     D                                     ns')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W2217R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D SF1PAG          C                   CONST(11)
     D VIEWMX          C                   CONST(2)
     D INFOMX          C                   CONST(1)

     C*
     C     *LIKE         DEFINE    CNDESC        KKDESC
     C     *LIKE         DEFINE    CNKEY         KKKEY
     C     *LIKE         DEFINE    CNKEY         @@KEY
     C     *LIKE         DEFINE    CNDESC        U1PDSC
     C     *LIKE         DEFINE    CNDESC        U2PDSC
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
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
     C                   KFLD                    ##CO#
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDESC
     C*
     C     CNKY          KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@ST              2
     C                   PARM                    @@RTCD            1
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSST          ORNE      *BLANKS
     C     SSDESC        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
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
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     XXST          SSST
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C     Q1PRMS        IFGE      4
     C                   MOVEL     @@ST          XXST
     C                   ENDIF                                                  Q1PRMS>=4
     C*
     C     XXST          CAT       '  ':0        XXKEY             4
     C   21              MOVEL(P)  XXKEY         KKKEY
     C   22              MOVEL     *LOVAL        KKDESC
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     SCPST1        CAT       SCPSTA:0      XXKEY
     C                   MOVEL(P)  XXKEY         KKKEY
     C*
     C     *IN22         WHENEQ    *ON
     C*
     C                   MOVEL(P)  SCPST2        KKDESC
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCKEY         KKKEY
     C   22              MOVEL     SCDESC        KKDESC
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CNKEY         SCKEY
     C*
     C                   MOVEL     CNKEY         SCST
     C     2             SUBST     CNKEY:3       SCCODE
     C                   MOVEL     CNDESC        SCDESC
     C*
     C                   MOVEL     SCST          XXDATA            7
     C                   MOVE      SCCODE        XXDATA
     C*
     C   21              MOVEL(P)  XXDATA        SCDATA
     C   21              MOVE      SCDESC        SCDATA
     C*
     C   22              MOVEL(P)  SCDESC        SCDATA
     C   22              MOVE      XXDATA        SCDATA
     C*
     C                   MOVEL     CNSTAT        SCSTAT
     C*
     C     CNKY          CHAIN     PCN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           SCSLCT
     C                   ELSE
     C                   MOVE      *BLANKS       SCSLCT
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C     *LOVAL        SETLL     PCN01
     C                   READ      PCN01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     *INKC         OREQ      *ON
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   MOVE      *OFF          @@RTCD
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FIELDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  CNDESC        U1PDSC
     C     LO:UP         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSST          IFNE      *BLANKS
     C*
     C     2             SUBST     CNKEY:1       XXST              2
     C*
     C     SSST          IFNE      XXST
     C     SSST          ANDNE     *BLANKS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSST<>XXST
     C*
     C                   ENDIF                                                  SSST<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CNSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
** Headers - HD
St  Code  Description                     Sts Selected
Description                      St  Code Sts Selected
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1055=Display                NNN
12020=Change Selection Stat YNY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
