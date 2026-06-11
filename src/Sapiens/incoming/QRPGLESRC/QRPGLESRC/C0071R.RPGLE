     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE C0071R - WORK WITH CLAIMS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY CLAIMS               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    C0071R02                                                   *
     H*                                                               *
     H*  CREATED: 02/08/05  PROGRAMMER: C:53  PROJECT: PC011          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  00/00/00 C:00  PROJECT:
     H*****************************************************************
     H*
     H*
     FC0071F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FCMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FCMCML4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCM01:DBRCDF)
     F*  Secondary logical file for view #2
     FCMCML5    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCM01:DBRCDF2)
     F*  Secondary logical file for view #3
     FCMCML6    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCM01:DBRCDF3)
     FCMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(20) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(20) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D*** V1
     D  SCDATA                 1     70
     D  XSFIN                  1     24
     D  XSPOL#                27     40
     D  XSCLM#                43     54
     D  XSLSDT                57     66
     D  XSSTAT                70     70
     D*** V2
     D  X2FIN                 15     39
     D  X2LSDT                42     51
     D  X2POL#                54     66
     D*** V3
     D  X3POL#                 1     13
     D  X3FIN                 16     40
     D  X3CLM#                43     54
     D  X3LSDT                57     66
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCCLM#                 4     15
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Claims')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('C0071R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(10)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CLM#
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
     C                   PARM                    ##CONM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEYR          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFIN
     C                   KFLD                    KKPOL#
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKLSDT
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKFIN
     C                   KFLD                    KKLSDT
     C                   KFLD                    KKPOL#
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPOL#
     C                   KFLD                    KKFIN
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKLSDT
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMFIN         KKFIN
     C     *LIKE         DEFINE    CMPOL#        KKPOL#
     C     *LIKE         DEFINE    CMCLM#        KKCLM#
     C     *LIKE         DEFINE    CMLSDT        KKLSDT
     C     *LIKE         DEFINE    CMFIN         U1PDSC
     C     *LIKE         DEFINE    CMFIN         U2PDSC
     C     *LIKE         DEFINE    CMPOL#        ##POL#
     C     *LIKE         DEFINE    CMCLM#        ##CLM#
     C     *LIKE         DEFINE    CMCLM#        @@CLM#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSPOL#        IFGT      *BLANKS
     C     SSCLM#        ORGT      *BLANKS
     C     SSFIN         ORGT      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CLM#        IFNE      *BLANKS
     C                   MOVE      @@CLM#        SSCLM#
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKFIN
     C                   MOVE      *LOVAL        KKPOL#
     C                   MOVE      *LOVAL        KKCLM#
     C                   MOVE      *LOVAL        KKLSDT
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      *LOVAL        KKFIN
     C                   MOVE      *LOVAL        KKPOL#
     C                   MOVE      *LOVAL        KKCLM#
     C                   MOVE      *LOVAL        KKLSDT
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     SCPST1        IFNE      *BLANKS
     C                   MOVEL(P)  SCPST1        KKFIN
     C                   ENDIF
     C*
     C     *IN22         WHENEQ    *ON
     C     SCPST2        IFNE      *BLANKS
     C                   MOVEL(P)  SCPST2        KKCLM#
     C                   ENDIF
     C*
     C     *IN23         WHENEQ    *ON
     C     SCPST3        IFNE      *BLANKS
     C                   MOVEL(P)  SCPST3        KKPOL#
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCCO#         KKCO#
     C                   MOVEL     SCFIN         KKFIN
     C                   MOVEL     SCPOL#        KKPOL#
     C                   MOVEL     SCCLM#        KKCLM#
     C                   MOVEL     SCLSDT        KKLSDT
     C                   ENDSR
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SSCLM#'
     C*
     C*   CLAIMANT SEARCH
     C*
     C                   MOVE      SSPOL#        ##POL#
     C                   MOVE      SSCLM#        ##CLM#
     C                   CALL      'C0700R'
     C                   PARM                    ##PRDS           40
     C                   PARM                    ##CALL            1
     C                   PARM                    ##POL#
     C                   PARM                    ##CLM#
     C                   PARM                    ##RTC             1
     C                   MOVE      ##POL#        SSPOL#
     C                   MOVE      ##CLM#        SSCLM#
     C*
     C     ##RTC         IFEQ      'B'
     C     ##CLM#        OREQ      *BLANK
     C     ##POL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SSPOL#'
     C*
     C                   MOVE      *BLANK        ##POL#
     C                   MOVE      'C'           ##CALL
     C                   CALL      'C0531R'
     C                   PARM                    ##PRDS
     C                   PARM                    ##CALL
     C                   PARM                    ##POL#
     C                   PARM                    ##RTC
     C*
     C                   MOVE      ##POL#        SSPOL#
     C*
     C     ##RTC         IFEQ      'B'
     C     ##POL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SSFIN'
     C*
     C                   CALL      'C0530R'
     C                   PARM                    ##PRDS           40
     C                   PARM                    ##CALL            1
     C                   PARM                    ##POL#
     C                   PARM                    ##RTC             1
     C                   MOVE      ##POL#        SSPOL#
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CMCO#         SCCO#
     C                   MOVEL     CMFIN         SCFIN
     C                   MOVEL     CMPOL#        SCPOL#
     C                   MOVEL     CMCLM#        SCCLM#
     C                   MOVE      CMLSDT        SCLSDT
     C*
     C                   Z-ADD     CMLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTLSDT           10
     C                   MOVE      *BLANKS       SCDATA
     C*
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  SCFIN         XSFIN
     C                   MOVEL(P)  SCPOL#        XSPOL#
     C                   MOVEL(P)  SCCLM#        XSCLM#
     C                   MOVEL     XTLSDT        XSLSDT
     C*
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  SCCLM#        SCDATA
     C                   MOVEL(P)  SCFIN         X2FIN
     C                   MOVEL     XTLSDT        X2LSDT
     C                   MOVEL(P)  SCPOL#        X2POL#
     C*
     C     VIEW#         WHENEQ    3
     C                   MOVEL(P)  SCPOL#        SCDATA
     C                   MOVEL(P)  SCFIN         X3FIN
     C                   MOVEL(P)  SCCLM#        X3CLM#
     C                   MOVEL     XTLSDT        X3LSDT
     C                   ENDSL
     C*
     C                   MOVE      CMSTAT        XSSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '16'          OPT16
     C     SCSEL         CASEQ     '51'          OPT51
     C     SCSEL         CASEQ     '52'          OPT52
     C     SCSEL         CASEQ     '55'          OPT55
     C     SCSEL         CASEQ     '60'          OPT60
     C*******    SCSEL     CASEQ'61'      OPT61
     C     SCSEL         CASEQ     '62'          OPT62
     C                   ENDCS
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT7          BEGSR
     C*
     C     @@CLM#        IFNE      *BLANKS
     C                   MOVEL(P)  'C0077R'      #FNAME           10
     C                   MOVEL(P)  'C0077Q'      #TNAME           10
     C*
     C                   CALL      'SA711C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C     ##RTN         IFEQ      'A'
     C                   SETON                                        90
     C                   ELSE
     C*
     C                   CALL      'C0077Q'
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCCLM#        ##CLM#
     C                   ENDIF                                                  ##RTN = 'A'
     C                   ELSE
     C*
     C                   CALL      'C0077R'                             99
     C                   PARM      ' '           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCCLM#        ##CLM#
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT12         BEGSR
     C*
     C                   CALL      'CA004R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      ##CALL        ##MODE
     C                   PARM      SCPOL#        ##POL#
     C                   PARM      SCCLM#        ##CLM#
     C                   PARM      SCPRDS        ##PRDS
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT14         BEGSR
     C*
     C*** ESTABLISH KEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      KKCO#         QCCO#
     C                   MOVE      SCCLM#        QCCLM#
     C*
     C*** CALL REMARKS PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'C0200R'      XXPGM            10
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT16         BEGSR
     C*
     C                   MOVE      '2'           QXCALL            1
     C                   MOVE      SCCLM#        ##CLM#
     C                   MOVE      *BLANKS       ##CLID
     C                   MOVE      *BLANKS       ##RTC
     C                   CALL      'C0270R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM                    QXCALL            1
     C                   PARM                    ##CLM#
     C                   PARM                    ##CLID            5
     C                   PARM                    ##RTC             1
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C******     OPT17     BEGSR
     C*
     C*
     C******               ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C******     OPT18     BEGSR
     C*
     C*
     C******               ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT51         BEGSR
     C*
     C                   CALL      'C0210R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCCLM#        ##CLM#
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT52         BEGSR
     C*
     C                   CALL      'C0410R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCCLM#        ##CLM#
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT55         BEGSR
     C*
     C                   CALL      'C0711R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM                    ##RTC             1
     C                   PARM      SCCLM#        ##CLM#
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT60         BEGSR
     C*
     C                   CALL      'C0428R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL
     C                   PARM      SCCLM#        ##CLM#
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C****       OPT61     BEGSR
     C*
     C*
     C****                 ENDSR
     C*===============================================================
     C* OPT
     C*===============================================================
     C     OPT62         BEGSR
     C*
     C                   CALL      'C0704R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM                    ##CALL
     C                   PARM      SCPOL#        ##POL#
     C                   PARM      SCCLM#        ##CLM#
     C                   PARM                    ##RTC
     C*
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
     C                   MOVE      @@CALL        ##CALL
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C                   MOVE      UPCO#         KKCO#
      *
     C* CHECK TO SEE IF C0077Q IS ON THE CALL STACK.
      *
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'C0077Q'      XXPGM            10
     C                   PARM      *BLANKS       XXERR             1
     C*
     C     XXERR         IFEQ      '1'
     C*********  Q1PGM     IFEQ 'C0071Q'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '07'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  WCFL09=Y
      *
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
     C                   MOVEL     SCCLM#        @@CLM#
     C                   ENDSR
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   SELECT
     C*
     C     SSCLM#        WHENNE    *BLANKS
     C     CMCLM#        ANDNE     SSCLM#
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSPOL#        WHENNE    *BLANKS
     C     CMPOL#        ANDNE     SSPOL#
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSFIN         WHENNE    *BLANKS
     C     LO:UP         XLATE(P)  CMFIN         U1PDSC
     C     LO:UP         XLATE(P)  SSFIN         U2PDSC
     C                   EXSR      STRING
     C*
     C                   ENDSL
     C*
     C                   ENDSR
** Column headings
Opt  Insured Name              Policy #        Claim #       Loss Date    St
Opt  Claim #       Insured Name               Loss Date   Policy #        St
Opt  Policy #       Insured Name               Claim #       Loss Date    St
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Claimant info          YYY
11212=Reserves              YYY
11414=DARTS                 YYN
11616=Suit info             YYN
15151=Check request         YYN
15252=Man check/adjustments YYN
15555=Payment request srch  YYN
16060=Payment history       YYN
16262=Claim status history  YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYN
212F12=Cancel               NNY
217F17=Subset               YYY
3
