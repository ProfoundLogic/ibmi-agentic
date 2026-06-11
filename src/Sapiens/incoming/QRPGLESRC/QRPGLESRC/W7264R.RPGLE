     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7264R - WORK WITH CLAIM TRANSFERS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY TRANSFERS WHICH HAVE *
     H*                BEEN EXECUTED.                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W7264R02                                                   *
     H*****************************************************************
     F*  02/11/10 C:237: SUPP SEQ71581
     F*                  Subset on W7264R/F-Work with Claim Transfers
     H*  CREATED: 05/02/00  PROGRAMMER: C:53  PROJECT: WC836          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  00/00/00 C:00  PROJECT:
     H*****************************************************************
     H*
     FW7264F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWACTP     IF   E             DISK
     F                                     RENAME(PCT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWACTL5    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCT01:DBRCDF)
     F*  Secondary logical file for view #2
     FWACTL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCT01:DBRCDF2)
     F*  Secondary logical file for view #3
     FWACTL1    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCT01:DBRCDF3)
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     FWMCMP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D DATA            S              1    DIM(70)                                            ion
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*DBFDSE      DS
     D*                                    B 397 4000DBRRNE
     D*
     D                 DS
     D  SCDATA                 1     70
     D  XSFND                 15     17
     D  XSXCAS                21     31
     D  XSCASE                35     45
     D  XSUSER                50     59
     D*** V1 I2
     D  XSXEMP                35     49
     D  XSEMP#                54     68
     D*** V2 I1
     D  X2XCAS                 7     17
     D  X2CASE                22     32
     D  X2TRDT                37     46
     D  X2USER                51     60
     D*** V2 I2
     D  X2XEMP                22     36
     D  X2EMP#                40     54
     D  X3TRDT                60     69
     D*** V3 I1
     D  X3CASE                 7     17
     D  X3XCAS                22     32
     D  X4TRDT                37     46
     D  X3USER                51     60
     D*** V3 I2
     D  X3XEMP                22     36
     D  X3EMP#                40     54
     D  X5TRDT                60     69
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Claim Transfers')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W7264R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(12)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C* EMPLOYER SEARCH
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    XACO#             3
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C*
     C     ACDPL3        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      *BLANKS       ##SORT            2
     C                   PARM      *ZEROS        ##RRN2
     C*** NO PASS          PARM           ##FYR   30
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEYR          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKFND
     C                   KFLD                    KKXFYR
     C                   KFLD                    KKXCAS
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKXFYR
     C                   KFLD                    KKXCAS
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FOR SUBSET
     C*
     C     EMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Date format parameter lists
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CTTRDT        KKTRDT
     C     *LIKE         DEFINE    CTCO#         KKCO#
     C     *LIKE         DEFINE    CTFND         KKFND
     C     *LIKE         DEFINE    CTXFYR        KKXFYR
     C     *LIKE         DEFINE    CTXCAS        KKXCAS
     C     *LIKE         DEFINE    CTFYR         KKFYR
     C     *LIKE         DEFINE    CTCASE        KKCASE
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    CTDIV         NNDIV
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
B01  C     SSFND         IFNE      *ZEROS
     C*** BEG ADD ***  02/11/10  ************************************
     C     SSYEAR        ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C     SSEMP#        ORNE      *ZEROS
     C     SSDIV         ORNE      *BLANKS
     C*** END ADD ***  02/11/10  ************************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDBF - Convert database fields to screen format
     C*================================================================
     C     CVTDBF        BEGSR
     C                   Z-ADD     CTTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTTRDT           10
     C*
     C                   MOVEL     CTFND         ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTFND             3
     C*
     C                   MOVEL     CTXFYR        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTXFYR            3
     C*
     C                   MOVEL     CTXCAS        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTXCAS            7
     C*
     C                   MOVEL     CTFYR         ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTFYR             3
     C*
     C                   MOVEL     CTCASE        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTCASE            7
     C*
     C                   MOVEL     CTXEMP        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTXEMP            9
     C*
     C                   MOVEL     CTXDIV        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTXDIV            5
     C*
     C                   MOVEL     CTEMP#        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTEMP#            9
     C*
     C                   MOVEL     CTDIV         ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTDIV             5
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*** BEG ADD ***  02/11/10  ************************************
     C                   EXSR      OTHFKS
     C*** END ADD ***  02/11/10  ************************************
     C*** BEG DEL ***  02/11/10  ************************************
     C***        SSFND     IFEQ *ZEROS
     C***                  MOVE *ON       *IN80
     C***                  ENDIF
     C***
     C***        SSYEAR    IFEQ *ZEROS
     C***                  MOVE *ON       *IN80
     C***                  ENDIF
     C***
     C***        SSEMP#    IFEQ *ZEROS
     C***        SSDIV     ANDNE*BLANKS
     C***                  MOVE *ON       *IN80
     C***                  ENDIF
     C***
      * PROCESS SEARCHES
      ***
     C***        *INKJ     IFEQ *ON
     C***                  SELEC
     C*
     C***        HFLD      WHEQ 'SSFND'
     C***                  MOVE ##CO#     XACO#
     C***                  MOVE *ZERO     XAFND
     C***                  MOVE *ZERO     XAFYR
     C***                  MOVE '2'       ##MODE
     C***                  CALL 'W0010R'  ACDPL2       99
     C***                  MOVE XAFND     SSFND
     C*
     C***        HFLD      WHEQ 'SSCASE'
     C***                  CALL 'W0070R'  ACDPL3       99
     C***        ##RRN     CHAINWMCMP                60
     C***                  MOVE CMCASE    SSCASE
     C*
     C***        HFLD      WHEQ 'SSEMP#'
     C***        *IN34     ANDEQ*OFF
     C*
     C***        HFLD      OREQ 'SSDIV'
     C***        *IN35     ANDEQ*OFF
     C***                  MOVE '2'       ##MODE
     C***                  Z-ADD*ZEROS    ##RRN3  90
     C***                  CALL 'W0050R'  ACDPL1       99
     C***        ##RRN     CHAINPEM0P                60
     C***        *IN60     IFEQ *OFF
     C***                  MOVE EMFND     SSFND
     C***                  MOVE EMEMP#    SSEMP#
     C*
     C***        EMDIV     IFNE *ZERO
     C***                  MOVELEMDIV     ##NUM     P
     C***                  MOVEL*BLANKS   ##MASK    P
     C***                  CALL CHGNUM    CHGPL
     C***                  MOVEL##NUM     SSDIV
     C***                  ELSE
     C***                  MOVE '0'       SSDIV
     C***                  ENDIF                           EMDIV<>*ZERO
     C*
E01  C***                  END
     C*
     C***                  OTHER
     C***                  SETON                         90
     C***                  ENDSL
     C*
     C***                  END                             *INKJ
     C*** END DEL ***  02/11/10  ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     UPXFND        IFEQ      'Y'
     C                   MOVE      *ON           *IN31
     C                   MOVEL     UPFND         SSFND
     C                   ENDIF
     C*
     C     UPXEMP        IFEQ      'Y'
     C                   MOVE      *ON           *IN32
     C                   MOVEL     UPDIV         ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         SSDIV
     C                   MOVEL     UPEMP#        SSEMP#
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *HIVAL        KKTRDT
     C                   MOVEL     *LOVAL        KKFND
     C                   MOVEL     *LOVAL        KKXFYR
     C                   MOVEL     *LOVAL        KKXCAS
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      *LOVAL        KKTRDT
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKXFYR
     C                   MOVE      *LOVAL        KKXCAS
     C                   MOVE      *LOVAL        KKFYR
     C                   MOVE      *LOVAL        KKCASE
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     SCPST1        IFNE      *BLANKS
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C                   ENDIF
     C*
     C     SCPS1A        IFNE      *BLANKS
     C                   MOVE      SCPS1A        KKFND
     C                   ENDIF
     C*
     C     *IN22         WHENEQ    *ON
     C     SCPST2        IFNE      *BLANKS
     C                   MOVE      SCPST2        KKFND
     C                   ENDIF
     C*
     C     SCPS2A        IFNE      *BLANKS
     C                   MOVE      SCPS2A        KKXFYR
     C                   ENDIF
     C*
     C     SCPS2B        IFNE      *BLANKS
     C                   MOVE      SCPS2B        KKXCAS
     C                   ENDIF
     C*
     C     *IN23         WHENEQ    *ON
     C     SCPST3        IFNE      *BLANKS
     C                   MOVE      SCPST3        KKFND
     C                   ENDIF
     C*
     C     SCPS3A        IFNE      *BLANKS
     C                   MOVE      SCPS3A        KKFYR
     C                   ENDIF
     C*
     C     SCPS3B        IFNE      *BLANKS
     C                   MOVE      SCPS3B        KKCASE
     C                   ENDIF
     C*
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCTRDT        KKTRDT
     C                   MOVEL     SCFND         KKFND                          MJF
     C                   MOVEL     SCXFYR        KKXFYR
     C                   MOVEL     SCXCAS        KKXCAS
     C                   MOVEL     SCFYR         KKFYR
     C                   MOVEL     SCCASE        KKCASE
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CTTRDT        SCTRDT
     C                   MOVE      CTXFYR        SCXFYR
     C                   MOVE      CTXCAS        SCXCAS
     C                   MOVE      CTFYR         SCFYR
     C                   MOVE      CTCASE        SCCASE
     C                   MOVE      CTFND         SCFND
     C*** CONVERT FIELDS
     C                   EXSR      CVTDBF
     C*
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  XTTRDT        SCDATA
     C                   MOVE(P)   XTFND         XSFND
     C                   MOVEL(P)  XTXFYR        XSXCAS
     C                   MOVE      XTXCAS        XSXCAS
     C*
     C     INFO#         IFEQ      1
     C                   MOVEL(P)  XTFYR         XSCASE
     C                   MOVE      XTCASE        XSCASE
     C                   MOVEL(P)  CTUSER        XSUSER
     C                   ENDIF
     C*
     C     INFO#         IFEQ      2
     C                   MOVEL(P)  XTXEMP        XSXEMP
     C                   MOVE      XTXDIV        XSXEMP
     C                   MOVEL(P)  XTEMP#        XSEMP#
     C                   MOVE      XTDIV         XSEMP#
     C                   ENDIF
     C*
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  XTFND         SCDATA
     C                   MOVEL(P)  XTXFYR        X2XCAS
     C                   MOVE      XTXCAS        X2XCAS
     C*
     C     INFO#         IFEQ      1
     C                   MOVEL(P)  XTFYR         X2CASE
     C                   MOVE      XTCASE        X2CASE
     C                   MOVEL(P)  XTTRDT        X2TRDT
     C                   MOVEL(P)  CTUSER        X2USER
     C                   ENDIF
     C*
     C     INFO#         IFEQ      2
     C                   MOVEL(P)  XTXEMP        X2XEMP
     C                   MOVE      XTXDIV        X2XEMP
     C                   MOVEL(P)  XTEMP#        X2EMP#
     C                   MOVE      XTDIV         X2EMP#
     C                   MOVEL(P)  XTTRDT        X3TRDT
     C                   ENDIF
     C*
     C     VIEW#         WHENEQ    3
     C                   MOVEL(P)  XTFND         SCDATA
     C                   MOVEL(P)  XTFYR         X3CASE
     C                   MOVE      XTCASE        X3CASE
     C*
     C     INFO#         IFEQ      1
     C                   MOVEL(P)  XTFYR         X3XCAS
     C                   MOVE      XTCASE        X3XCAS
     C                   MOVEL(P)  XTTRDT        X4TRDT
     C                   MOVEL(P)  CTUSER        X3USER
     C                   ENDIF
     C*
     C     INFO#         IFEQ      2
     C                   MOVEL(P)  XTXEMP        X3XEMP
     C                   MOVE      XTXDIV        X3XEMP
     C                   MOVEL(P)  XTEMP#        X3EMP#
     C                   MOVE      XTDIV         X3EMP#
     C                   MOVEL(P)  XTTRDT        X5TRDT
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*** BEG ADD ***  02/11/10  ************************************
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C*** END ADD ***  02/11/10  ************************************
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     Q1USER        CHAIN     PUP01                              60
     C                   MOVE      UPCO#         KKCO#
     C                   MOVE      UPFND         KKFND
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEYR          READE     DBRCDF                                 95
     C   22KEYR          READE     DBRCDF2                                95
     C   23KEYR          READE     DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEYR          READPE    DBRCDF                                 95
     C   22KEYR          READPE    DBRCDF2                                95
     C   23KEYR          READPE    DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   MOVE      SSDIV         NNDIV
B01  C                   SELECT
     C*
     C     SSFND         WHENNE    0
     C     CTFND         ANDNE     SSFND
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSYEAR        WHENNE    0
     C     CTXFYR        ANDNE     SSYEAR
     C     CTFYR         ANDNE     SSYEAR
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSCASE        WHENNE    0
     C     CTXCAS        ANDNE     SSCASE
     C     CTCASE        ANDNE     SSCASE
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSEMP#        WHENNE    0
     C     CTEMP#        ANDNE     SSEMP#
     C     CTXEMP        ANDNE     SSEMP#
     C                   MOVE      *OFF          MEETSS
     C*
     C*** DIV must be one associated with the EMP.
     C*
     C     NNDIV         WHENNE    *ZEROS
     C     CTEMP#        ANDEQ     SSEMP#
     C     CTDIV         ANDNE     NNDIV
     C     NNDIV         ORNE      *ZEROS
     C     CTXEMP        ANDEQ     SSEMP#
     C     CTXDIV        ANDNE     NNDIV
     C                   MOVE      *OFF          MEETSS
     C*
E01  C                   ENDSL
     C                   ENDSR
     C*** BEG ADD ***  02/11/10  ************************************
     C*===============================================================
     C* KJDO   - SEARCH SUBROUTINE
     C*===============================================================
     C     KJDO          BEGSR
      * PROCESS SEARCHES
      *
     C     *INKJ         IFEQ      *ON
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSFND'
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      *ZERO         XAFND
     C                   MOVE      *ZERO         XAFYR
     C                   MOVE      '2'           ##MODE
     C                   CALL      'W0010R'      ACDPL2                 99
     C                   MOVE      XAFND         SSFND
      *
     C     SSFND         IFNE      *ZEROS
     C                   SETON                                        71
     C                   ELSE
     C                   SETON                                        70
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SSCASE'
     C                   CALL      'W0070R'      ACDPL3                 99
     C     ##RRN         CHAIN     WMCMP                              60
     C                   MOVE      CMCASE        SSCASE
      *
     C     SSCASE        IFNE      *ZEROS
     C                   SETON                                        73
     C                   ELSE
     C                   SETON                                        72
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SSEMP#'
     C     *IN34         ANDEQ     *OFF
     C*
     C     HFLD          OREQ      'SSDIV'
     C     *IN35         ANDEQ     *OFF
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     *ZEROS        ##RRN3            9 0
     C                   CALL      'W0050R'      ACDPL1                 99
     C     ##RRN         CHAIN     PEM0P                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SSFND
     C                   MOVE      EMEMP#        SSEMP#
     C*
     C     EMDIV         IFNE      *ZERO
     C                   MOVEL(P)  EMDIV         ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         SSDIV
     C                   ELSE
     C                   MOVE      '0'           SSDIV
     C                   ENDIF                                                  EMDIV<>*ZERO
      *
     C     SSEMP#        IFNE      *ZEROS
     C                   SETON                                        70
     C                   ELSE
     C                   SETON                                        73
     C                   ENDIF
     C*
     C                   END
     C*
     C                   OTHER
     C                   SETON                                            90
     C                   ENDSL
      *
     C                   END                                                    *INKJ
      *
     C     SSFND         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C     SSYEAR        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C     SSEMP#        IFEQ      *ZEROS
     C     SSDIV         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN80
     C                   ENDIF
      *
     C                   ENDSR
     C*** END ADD ***  02/11/10  ************************************
     C*===============================================================
** Column headings
Opt  Transfer dt   Grp    From claim      To claim    Requester
Opt  Grp    From claim       To claim    Transfer dt   Requester
Opt  Grp      To claim     From claim    Transfer dt   Requester
Opt  Transfer dt   Grp    From claim     From employer        To employer
Opt  Grp    From claim      From employer       To employer     Transfer dt
Opt  Grp      To claim      From employer       To employer     Transfer dt
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change info           YYN
211F11=Change view          YYN
212F12=Cancel               NNN
217F17=Subset               YYY
3
