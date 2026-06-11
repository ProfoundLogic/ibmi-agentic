     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*  F8047R1 - Work with OptaComp Errors                          *
     H*                                                               *
     H*  Called from: Menu                                            *
     H*  Calls to   : F8047R01 - Upload Bill Information              *
     H*                                                               *
     H*****************************************************************
     H*  Created on 05/26/2013 by c:282 Project 114WJC                *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     fF8047F1   CF   E             WORKSTN
      /If Defined (WEB)
     f                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     f                                     SFILE(SCR01S:SFRRN1)
     f                                     SFILE(SCR02S:SFRRN2)
     f                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     fFAU1P     IF   E             DISK
     f                                     RENAME(PU101:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     fFAU1L11   IF   E           K DISK
     f                                     INFDS(DBFDS)
     f                                     RENAME(PU101:DBRCDF)
      *
      *  Secondary logical file definition (for displaying items)
     fFAU1L12   IF   E           K DISK
     f                                     INFDS(DBFDS2)
     f                                     RENAME(PU101:DBRCDF2)
      *
     fFAU1L13   IF   E           K DISK
     f                                     INFDS(DBFDS3)
     f                                     RENAME(PU101:DBRCDF3)
      *
     fFAU1L14   IF   E           K DISK
     f                                     INFDS(DBFDS4)
     f                                     RENAME(PU101:DBRCDF4)
      *
     fNMNWL     IF   E           K DISK
     fNMNWP     IF   E             DISK
     f                                     RENAME(PNW01:PNW02)
      /COPY SCOPYWW01D
      *

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
      *
     d HD              S             79    DIM(4) CTDATA PERRCD(1)              Column headings
     d OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     d OFD             S             35    DIM(14) ALT(OF)
      *
     d DBFDSA          DS
     d  DBRRNA               397    400B 0
      *
     d*================================================================
     d*  User defined data structures
     d*================================================================
      *
     d                 DS
     d  DATA                   1     70
     d  XXMTH                  1      2
     d  SLS1                   3      3
     d  XXDAY                  4      5
     d  SLS2                   6      6
     d  XXYR                   7     10
     d  XXBIL#                15     29
     d  XXNAM                 33     58
     d  XXERR                 63     63
     d  XXEST                 70     70
     d                 DS
     d  XXULDT                 1      8
     d  XMTH                   1      2
     d  XDAY                   3      4
     d  XYR                    5      8
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     d OPTNM           C                   CONST('Upload Bills')
     d SLMSGI          C                   CONST('MC49998')
     d LO              C                   CONST('abcdefghijklmnopqrs-
     d                                     tuvwxyz')
     d HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ')
     d SLMSGF          C                   CONST('FSMESG')
     d ACDPGM          C                   CONST('F8047R01')
     d COMAPI          C                   CONST('FA007R')
     d USPAPI          C                   CONST('F0006R')
     d CHGNUM          C                   CONST('SA020R')
      *
      *
     d ERRMNT          C                   CONST('F2550R')
      *
     d INFOMX          C                   CONST(1)
      *
      *
      *
      *             3                     C         VIEWMX
     d SLASH           C                   CONST('/')
      *
      *
     d SF1PAG          C                   CONST(12)
     d DELETE          C                   CONST('*** DELETED ***-
     d                                      DELETED *** D-
     d                                     ELETED ***')
     d TRNSFR          C                   CONST('**TRANSFERRED*T-
     d                                     RANSFERRED*TRA-
     d                                     NSFERRED**')
      *================================================================
      *  PLIST definitions
      *================================================================
     c     *ENTRY        PLIST
     c                   PARM                    @@CALL            1
     c                   PARM      *INKC         @@KC              1
     c                   PARM      *INKL         @@KL              1
     c                   PARM                    @@RRN             9 0
     c                   PARM                    @@NTWK            5
      *
      *  Add/change/display program parameter list
      *
     c     ACDPL         PLIST
     c     *INKC         PARM                    ##KC              1
     c     *INKL         PARM                    ##KL              1
     c                   PARM                    ##MODE            1
     c                   PARM      SCDBRR        ##RRN             9 0
     c                   PARM                    ##CALL            1
     c                   PARM      *BLANKS       ##RTCD            1
      *
      *  Change numeric field to character field.
      *
     c     CNUMPL        PLIST
     c                   PARM                    ##NUM            80
     c                   PARM      *BLANKS       ##MASK           10
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     c     COMPL         PLIST
     c                   PARM                    ##CO#             3 0
     c                   PARM                    ##FND             3 0
     c                   PARM                    ##CONM           40
     c                   PARM                    ##FNAM           40
     c                   PARM                    ##SUB#            5 0
     c                   PARM                    ##FYR             3 0
      *
     c     DTCYMD        PLIST
     c                   PARM                    SRDTI
     c                   PARM      *BLANKS       SRFFMT
     c                   PARM      '*CYMD'       SRTFMT
     c                   PARM                    SRDTO
     c                   PARM                    SRERR
      *
     c     DTYYMD        PLIST
     c                   PARM                    SRDTI
     c                   PARM      '*MDYY'       SRFFMT
     c                   PARM      '*YYMD'       SRTFMT
     c                   PARM                    SRDTO
     c                   PARM                    SRERR
      *================================================================
      *  KLIST definitions
      *================================================================
     c     KEY1          KLIST
     c                   KFLD                    KKTRID
      *
     c     KEY2          KLIST
     c                   KFLD                    KKCLM#
     c                   KFLD                    KKTRID
      *
     c     KEY3          KLIST
     c                   KFLD                    KKCLSN
     c                   KFLD                    KKCFSN
      *
     c     NWKY          KLIST
     c                   KFLD                    ##CO#
     c                   KFLD                    KKNWCD
      *
      *
     c     KEY4          KLIST
     c                   KFLD                    KKULDT
     c                   KFLD                    KKULTM
     c                   KFLD                    KKTRID
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     c     *LIKE         DEFINE    U1TRID        KKTRID
     c     *LIKE         DEFINE    U1CLM#        KKCLM#
     c     *LIKE         DEFINE    U1CLSN        KKCLSN
     c     *LIKE         DEFINE    U1CFSN        KKCFSN
     c     *LIKE         DEFINE    U1CLSN        U1PDSC
     c     *LIKE         DEFINE    U1CLSN        U2PDSC
     c     *LIKE         DEFINE    NWNWCD        KKNWCD
      *
      *
     c     *LIKE         DEFINE    U1ULDT        KKULDT
     c     *LIKE         DEFINE    U1ULTM        KKULTM
     c     *LIKE         DEFINE    U1TRID        ##TRID
      *
      *
     c/COPY SCOPYWW01C
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     c     CHKSSM        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       ssSS# = UnMaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
      *
     c     SSCLSN        IFNE      *BLANKS
     c     SSSS#         ORNE      *BLANKS
     c     SSPRFL        ORNE      *BLANKS
     c     SSDERR        ORNE      *BLANKS
     c     SSULDT        ORNE      *ZEROS
     c     SSSTAT        ORNE      *BLANKS
     c                   SETON                                        11
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     c     ERRCK1        BEGSR
      *
     c     *IN90         IFEQ      *OFF
     c     SCSEL         IFEQ      '4'
     c     SCPRFL        IFEQ      'X'
     c                   MOVEL     '99'          SCSEL
     c                   ELSE
     c                   SETON                                        9071
     c                   ENDIF
     c                   ENDIF
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     c     ERRCK2        BEGSR
      *
     c     *INKJ         IFEQ      *ON
     c                   EXSR      KJDO
     c                   ELSE
     c     SSPRFL        IFNE      'X'
     c     SSPRFL        ANDNE     ' '
     c                   SETON                                        8990
     c                   ENDIF
     c     *IN90         IFEQ      *OFF
     c     SSDERR        IFNE      'D'
     c     SSDERR        ANDNE     'P'
     c     SSDERR        ANDNE     'S'
     c     SSDERR        ANDNE     ' '
     c                   SETON                                        8890
     c                   ENDIF
     c                   ENDIF
      *
     c     *IN90         IFEQ      *OFF
     c     SSNTWK        ANDNE     *BLANKS
     c                   MOVEL(P)  SSNTWK        KKNWCD
     c     NWKY          CHAIN     PNW01                              99
     c     *IN99         IFEQ      *ON
     c                   CLEAR                   SCNWDS
     c                   SETON                                        8790
     c                   ELSE
     c                   MOVEL(P)  NWNAME        SCNWDS
     c                   ENDIF
     c                   ENDIF
      *
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     c     INZSST        BEGSR
      *
     c     Q1PRMS        IFGT      4
     c                   MOVEL(P)  @@NTWK        SSNTWK
     c                   MOVEL(P)  SSNTWK        KKNWCD
     c     NWKY          CHAIN     PNW01                              99
     c     *IN99         IFEQ      *OFF
     c                   MOVEL(P)  NWNAME        SCNWDS
     c                   ELSE
     c                   CLEAR                   SCNWDS
     c                   ENDIF
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     c     KEYINZ        BEGSR
      *
     c                   MOVEL(P)  *BLANKS       KKTRID
     c                   MOVEL(P)  *BLANKS       KKCLM#
     c                   MOVEL(P)  *BLANKS       KKCLSN
     c                   MOVEL(P)  *BLANKS       KKCFSN
      *
      *
     c                   Z-ADD     *HIVAL        KKULDT
     c                   Z-ADD     *ZEROS        KKULTM
     c                   ENDSR
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     c     KEYSET        BEGSR
      *
     c   21              MOVEL(P)  SCPST1        KKTRID
     c   22              MOVEL(P)  SCPST1        KKTRID
     c   22              MOVEL(P)  SCPST2        KKCLM#
     c   23              MOVEL(P)  SCPST3        KKCLSN
     c   23              MOVEL(P)  SCPSTF        KKCFSN
      *
      *
     c     *IN24         IFEQ      *ON
      *
      * Convert date
      *
     c                   MOVE      SCPST4        SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         KKULDT
     c                   Z-ADD     *HIVAL        KKULTM
      *
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     c     KEYSFL        BEGSR
      *
     c                   MOVEL(P)  SCTRID        KKTRID
     c                   MOVEL(P)  SCCLM#        KKCLM#
     c                   MOVEL(P)  SCCLSN        KKCLSN
     c                   MOVEL(P)  SCCFSN        KKCFSN
      *
      *
     c                   Z-ADD     SCULDT        KKULDT
     c                   Z-ADD     SCULTM        KKULTM
      *
      *
      *
     c                   ENDSR
      *================================================================
      * KJDO - F10=SEARCH
      *================================================================
     c     KJDO          BEGSR
      *
     c                   SELECT
     c     HFLD          WHENEQ    'SSNTWK'
      *
     c                   CALL      'N2325R'
     c                   PARM      '2'           ##CALL            1
     c     *INKC         PARM      '0'           ##KC              1
     c     *INKL         PARM      '0'           ##KL              1
     c                   PARM      *ZEROS        ##RRN             9 0
      *
     c     *INKL         IFEQ      *ON
     c     ##RRN         OREQ      *ZEROS
     c                   SETON                                        31
     c                   ELSE
     c     ##RRN         CHAIN     PNW02                              99
     c     *IN99         IFEQ      *OFF
     c                   MOVEL     NWNWCD        SSNTWK
     c                   MOVEL     NWNAME        SCNWDS
     c                   ENDIF                                                  *IN99 = Off
     c                   ENDIF
      *
     c                   ENDSL
      *
     c                   SETON                                        90
      *
     c                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     c     MVDBSC        BEGSR
      *
B01  c     UPDLIN        IFEQ      *OFF
      *
     c                   MOVEL(P)  U1TRID        SCTRID
     c                   MOVEL(P)  U1CLM#        SCCLM#
     c                   MOVEL(P)  U1CLSN        SCCLSN
     c                   MOVEL(P)  U1CFSN        SCCFSN
     c                   MOVEL(P)  U1PRFL        SCPRFL
      *
     c                   CALL      'SRNAME'
     c                   PARM      U1CLSN        ##LAST           56
     c                   PARM      U1CFSN        ##FRST           15
     c                   PARM      '1'           ##TERM            1
      *
     c                   MOVEL     ##LAST        XXNAME           33
      *
     c   21              MOVEL(P)  U1CLM#        XXDATA           54
     c   21              MOVE      XXNAME        XXDATA
     c   21              MOVEL(P)  U1TRID        SCDATA
     c   21              MOVE      XXDATA        SCDATA
      *
     c   22              MOVEL(P)  U1CLM#        XXDATA
     c   22              MOVE      XXNAME        XXDATA
     c   22              MOVEL(P)  XXDATA        SCDATA
     c   22              MOVE      U1TRID        SCDATA
      *
     c   23              MOVEL(P)  XXNAME        XXDATA
     c   23              MOVE      U1CLM#        XXDATA
     c   23              MOVEL(P)  XXDATA        SCDATA
     c   23              MOVE      U1TRID        SCDATA
      *
      *
B02  c     *IN24         IFEQ      *ON
      *
     c                   MOVEL(P)  XXNAME        XXDATA
     c                   MOVE      U1CLM#        XXDATA
      *
      * Convert date
      *
     c                   Z-ADD     U1ULDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTMDYY
     c                   MOVEL     SRDTO         XXULDT
      *
     c                   MOVEL(P)  U1TRID        SCTRID
      *
B03  c     XXULDT        IFNE      *BLANKS
      *
     c                   Z-ADD     U1ULDT        SCULDT
     c                   Z-ADD     U1ULTM        SCULTM
      *
     c                   MOVE      XMTH          XXMTH
     c                   MOVE      SLASH         SLS1
     c                   MOVE      XDAY          XXDAY
     c                   MOVE      SLASH         SLS2
     c                   MOVE      XYR           XXYR
      *
X03  c                   ELSE
      *
     c                   Z-ADD     *ZEROS        SCULDT
     c                   Z-ADD     *ZEROS        SCULTM
      *
     c                   MOVE      *BLANKS       XXMTH
     c                   MOVE      *BLANKS       SLS1
     c                   MOVE      *BLANKS       XXDAY
     c                   MOVE      *BLANKS       SLS2
     c                   MOVE      *BLANKS       XXYR
      *
E03  c                   ENDIF
      *
     c                   MOVEL     U1TRID        XXBIL#
     c                   MOVEL     XXNAME        XXNAM
     c                   MOVE      U1PRFL        XXERR
     c                   MOVE      U1ESTS        XXEST
      *
     c                   MOVE      *BLANKS       SCDATA
     c                   MOVEL     DATA          SCDATA
      *
X02  c                   ELSE
      *
      *
     c                   MOVEL     U1PRFL        SCERR
      *
      *
     c                   MOVE      U1DLRE        SCDERR
      *
      *
E02  c                   ENDIF
      *
X01  c                   ELSE
     c                   MOVEL     *OFF          UPDLIN
E01  c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * OPT7 - Work with errors
      *================================================================
      *
     c     OPT7          BEGSR
      *
     c     CLTID         IFEQ      '133'
     c                   MOVEL     *OFF          UPDLIN
     c                   MOVE      'C'           ##MODE
     c                   MOVE      ' '           ##CALL
     c                   CALL      ERRMNT        ACDPL                  99
      *
     c                   MOVEL     *ON           UPDLIN
      *
     c                   ENDIF
      *
     c                   ENDSR
      *
      *================================================================
      * OPT99 - Delete Bill
      *================================================================
     c     OPT99         BEGSR
      *
     c                   MOVEL     *OFF          UPDLIN
     c                   MOVE      'C'           ##MODE
     c                   MOVE      'D'           ##CALL
     c                   CALL      ACDPGM        ACDPL                  99
     c     *INKL         IFEQ      *OFF
     c     *INKC         ANDEQ     *OFF
      *
     c                   MOVEL(P)  DELETE        SCDATA
     c                   MOVEL     'Y'           SCPRFL
     c                   MOVEL     *ON           UPDLIN
      *
     c                   ENDIF
     c                   MOVEL     *BLANKS       ##CALL
      *
     c                   ENDSR
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     c     OTHFKS        BEGSR
     c                   ENDSR
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     c     OTHOPT        BEGSR
      *
     c     SCSEL         CASEQ     '7'           OPT7
      *
     c     SCSEL         CASEQ     '99'          OPT99
     c                   ENDCS
      *
     c                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     c     PGMEND        BEGSR
      *
     c                   SETON                                        LR
      *
     c                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     c     PGMSTR        BEGSR
     c                   MOVEL     *OFF          UPDLIN            1
      *
      *
     c     CLTID         IFEQ      '133'
      *
     c                   Z-ADD     4             VIEWMX            1 0
     c                   Z-ADD     4             VIEW#
      *
     c                   ELSE
      *
     c                   Z-ADD     1             VIEWMX
     c                   Z-ADD     1             VIEW#
      *
     c                   MOVE      '1'           XXOPFN
     c                   MOVE      'NNN'         XXMIS
     c                   MOVE      '07'          XXNMBR
      *
     c                   EXSR      UPDOFA
      *
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * READ   - Read next record from file
      *================================================================
     c     READ          BEGSR
      *
     c   21              READ      DBRCDF                                 95
     c   22              READ      DBRCDF2                                95
     c   23              READ      DBRCDF3                                95
      *
      *
     c   24              READ      DBRCDF4                                95
      *
      *
     c                   ENDSR
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     c     READP         BEGSR
      *
     c   21              READP     DBRCDF                                 95
     c   22              READP     DBRCDF2                                95
     c   23              READP     DBRCDF3                                95
      *
      *
     c   24              READP     DBRCDF4                                95
      *
      *
     c                   ENDSR
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     c     SETGT         BEGSR
      *
     c   21KEY1          SETGT     DBRCDF                             96
     c   22KEY2          SETGT     DBRCDF2                            96
     c   23KEY3          SETGT     DBRCDF3                            96
      *
      *
     c   24KEY4          SETGT     DBRCDF4                            96
      *
      *
     c                   ENDSR
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     c     SETLL         BEGSR
      *
     c   21KEY1          SETLL     DBRCDF                             96
     c   22KEY2          SETLL     DBRCDF2                            96
     c   23KEY3          SETLL     DBRCDF3                            96
      *
      *
     c   24KEY4          SETLL     DBRCDF4                            96
      *
      *
      *
     c                   ENDSR
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     c     SF1UPD        BEGSR
     c                   ENDSR
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     c     SLCTRD        BEGSR
     c                   ENDSR
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
     c     STRING        BEGSR
      *
     c                   CALL      'QCLSCAN'
     c                   PARM                    U1PDSC
     c                   PARM      30            STRLEN            3 0
     c                   PARM      1             START             3 0
     c                   PARM                    U2PDSC
     c                   PARM      30            PATLEN            3 0
     c                   PARM      '0'           TRANS             1
     c                   PARM      '1'           TRIM              1
     c                   PARM      ' '           WILD              1
     c                   PARM                    SCNRES            3 0
      *
     c     SCNRES        IFEQ      0
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF                                                  SCNRES=0
      *
     c                   ENDSR                                                  STRING
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     c     SUBSET        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       ssSS# = UnMaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
      *
B01  c     SSCLSN        IFNE      *BLANKS
     c     LO:HI         XLATE(P)  U1CLSN        U1PDSC
     c     LO:HI         XLATE(P)  SSCLSN        U2PDSC
     c                   EXSR      STRING
E01  c                   ENDIF                                                  SSPDSC<>*BLANKS
      *
     c     SSSS#         IFNE      *BLANKS
     c     SSSS#         ANDNE     U1SS#
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF
      *
     c     SSPRFL        IFNE      *BLANKS
     c     SSPRFL        IFNE      U1PRFL
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF
     c                   ENDIF
      *
     c     SSDERR        IFNE      *BLANKS
     c                   MOVE(P)   U1DLRE        XXDERR            1
     c     SSDERR        IFNE      XXDERR
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF
     c                   ENDIF
      *
     c     SSNTWK        IFNE      *BLANKS
     c     SSNTWK        ANDNE     U1NTWK
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF
      *
      *
      * SUBSET BY UPLOAD DATE
      *
     c     SSULDT        IFNE      *ZEROS
      *
      * Convert date
     c                   MOVE      SSULDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         WKULDT            7 0
      *
     c     WKULDT        IFNE      U1ULDT
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF
      *
     c                   ENDIF
      *
      * SUBSET BY STATUS
      *
     c     SSSTAT        IFNE      *BLANKS
      *
     c     SSSTAT        IFNE      U1ESTS
     c                   MOVE      *OFF          MEETSS
     c                   ENDIF
      *
     c                   ENDIF
     c*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       ssSS# = MaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     c*
     c                   ENDSR
** Column headings
Opt Bill#           Claim#               Claimant name                    Errs
Opt Claim#               Claimant Name                     Bill#          Errs
Opt Claimant name                     Claim#               Bill#          Errs
Opt Upload Date   Bill#             Claimant Name               Errs   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1077=WW Bills with Error stsYYY
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   NNN
211F11=Chg View             YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
