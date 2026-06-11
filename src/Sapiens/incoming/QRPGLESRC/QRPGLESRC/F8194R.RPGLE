     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*  F8194R - Work with 837 Bills with Errors                     *
     H*                                                               *
     H*  Called from: Menu                                            *
     H*  Calls to   : F8194R01 - 837 Bills With Errors                *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/30/2017 by c:79 Project 114MJB                 *
     H*****************************************************************
     H*  Revisions:
     H*
     H*  06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*
     H*  04/05/19 C:079 Support: 114/78417
     H*                 Add Ins ID and Suffix to the main view.
     H*****************************************************************
     H*
     H*  Indicators Used:
     H*****************************************************************
     FF8194F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFACLMP    IF   E             DISK    RENAME(PFACLM:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFACLML6   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFACLM:DBRCDF)
     F*
     FNMNWL     IF   E           K DISK
     FNMNWP     IF   E             DISK
     F                                     RENAME(PNW01:PNW02)
     F*
     FFALINL    UF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

      /COPY SCOPYWW01D
     D*
     D*** Beg Chg *** 04/05/19 ***********************************
     D*** HD              S             79    DIM(1) CTDATA PERRCD(1)           Column headings
     D HD              S             90    DIM(1) CTDATA PERRCD(1)              Column headings
     D*** End Chg *** 04/05/19 ***********************************
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*
     D*
     D                 DS
     D  DATA                   1     70
     D  XXMTH                  1      2
     D  SLS1                   3      3
     D  XXDAY                  4      5
     D  SLS2                   6      6
     D  XXYR                   7     10
     D  XXBIL#                15     29
     D  XXNAM                 33     58
     D  XXERR                 63     63
     D  XXEST                 70     70
     D                 DS
     D  XXULDT                 1      8
     D  XMTH                   1      2
     D  XDAY                   3      4
     D  XYR                    5      8
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('837 Bills With Errors')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F8194R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D CHGNUM          C                   CONST('SA020R')
     D*
     D*
     D ERRMNT          C                   CONST('F2550R')
     D*
     D*
     D INFOMX          C                   CONST(1)
     D*
     D*
     D viewmx          c                   const(1)
     D SLASH           C                   CONST('/')
     D*
     D*
     D SF1PAG          C                   CONST(12)
     D DELETE          C                   CONST('*** DELETED ***-
     D                                      DELETED *** D-
     D                                     ELETED ***')
     D TRNSFR          C                   CONST('**TRANSFERRED*T-
     D                                     RANSFERRED*TRA-
     D                                     NSFERRED**')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@NTWK            5
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CALL            1
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C*  Change numeric field to character field.
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKPATLSN
     C                   KFLD                    KKPATFSN
     C                   KFLD                    KKPACCT
     C*
     C     NWKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNWCD
     C*
     C     LINKY         KLIST
     C                   KFLD                    KKICNTL
     C                   KFLD                    KKGCTL
     C                   KFLD                    KKTSCNTL
     C                   KFLD                    KKBHTTID
     C                   KFLD                    KKPACCT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LINICNTL      KKICNTL
     C     *LIKE         DEFINE    LINGCTL       KKGCTL
     C     *LIKE         DEFINE    LINTSCNTL     KKTSCNTL
     C     *LIKE         DEFINE    LINBHTTID     KKBHTTID
     C     *LIKE         DEFINE    CLMPACCT      KKPACCT
     C     *LIKE         DEFINE    CLMPATLSN     KKPATLSN
     C     *LIKE         DEFINE    CLMPATFSN     KKPATFSN
     C*
     C     *LIKE         DEFINE    CLMPATLSN     U1PDSC
     C     *LIKE         DEFINE    CLMPATLSN     U2PDSC
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C*
     C*
     C     *LIKE         DEFINE    CLMULDT       KKULDT
     C     *LIKE         DEFINE    CLMULTM       KKULTM
     C     *LIKE         DEFINE    CLMPACCT      ##TRID
     C*
     C     *LIKE         DEFINE    CLMPACCT      K1TRID
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /free
       ssSS# = UnMaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     SSCLSN        IFNE      *BLANKS
     C     SSSS#         ORNE      *BLANKS
     C*** Beg Del *** 04/05/19 ***********************************
     C****
     C**** SSPRFL        ORNE      *BLANKS
     C**** SSDERR        ORNE      *BLANKS
     C**** SSULDT        ORNE      *ZEROS
     C**** SSSTAT        ORNE      *BLANKS
     C*** End Del *** 04/05/19 ***********************************
     C                   SETON                                        11
     C                   ENDIF
      ***** Beg Add *** 06/09/20 *******************************************
      /free
       ssSS# = MaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '6'
     C     SCERR         ANDEQ     'R'
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         IFEQ      '4'
     C     SCPRFL        IFEQ      'E'
     C     SCPRFL        OREQ      'R'
     C                   MOVEL     '99'          SCSEL
     C                   ELSE
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         IFEQ      '6'
     C     SCPRFL        ANDNE     'E'
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      KJDO
     C*** Beg Del *** 04/05/19 *******************************************
     C***
     C***                ELSE
     C***  SSPRFL        IFNE      'E'
     C***  SSPRFL        ANDNE     'R'
     C***  SSPRFL        ANDNE     ' '
     C***                SETON                                        8990
     C***                ENDIF
     C***  *IN90         IFEQ      *OFF
     C***  SSDERR        IFNE      'D'
     C***  SSDERR        ANDNE     'P'
     C***  SSDERR        ANDNE     'S'
     C***  SSDERR        ANDNE     ' '
     C***                SETON                                        8890
     C***                ENDIF
     C***                ENDIF
     C***
     C***  *IN90         IFEQ      *OFF
     C***  SSNTWK        ANDNE     *BLANKS
     C***                MOVEL(P)  SSNTWK        KKNWCD
     C***  NWKY          CHAIN     PNW01                              99
     C***  *IN99         IFEQ      *ON
     C***                CLEAR                   SCNWDS
     C***                SETON                                        8790
     C***                ELSE
     C***                MOVEL(P)  NWNAME        SCNWDS
     C***                ENDIF
     C***                ENDIF
     C*** End Del *** 04/05/19 *******************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*** Beg Del *** 04/05/19 *****************************************
     C****
     C**** Q1PRMS        IFGT      4
     C****               MOVEL(P)  @@NTWK        SSNTWK
     C****               MOVEL(P)  SSNTWK        KKNWCD
     C**** NWKY          CHAIN     PNW01                              99
     C**** *IN99         IFEQ      *OFF
     C****               MOVEL(P)  NWNAME        SCNWDS
     C****               ELSE
     C****               CLEAR                   SCNWDS
     C****               ENDIF
     C****               ENDIF
     C*** End Del *** 04/05/19 *****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       KKPATLSN
     C                   MOVEL(P)  *BLANKS       KKPATFSN
     C                   MOVEL(P)  *BLANKS       KKPACCT
     C*
     C*
     C                   Z-ADD     *HIVAL        KKULDT
     C                   Z-ADD     *ZEROS        KKULTM
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL(P)  SCPST1        KKPATLSN
     C   21              MOVEL(P)  SCPST2        KKPATFSN
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCTRID        KKPACCT
     C                   MOVEL(P)  SCCLSN        KKPATLSN
     C                   MOVEL(P)  SCCFSN        KKPATFSN
     C*
     C                   Z-ADD     SCULDT        KKULDT
     C                   Z-ADD     SCULTM        KKULTM
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*** Beg Del *** 04/05/19 ****************************************
     C****
     C**** HFLD          WHENEQ    'SSNTWK'
     C****
     C****               CALL      'N2325R'
     C****               PARM      '2'           ##CALL            1
     C**** *INKC         PARM      '0'           ##KC              1
     C**** *INKL         PARM      '0'           ##KL              1
     C****               PARM      *ZEROS        ##RRN             9 0
     C****
     C**** *INKL         IFEQ      *ON
     C**** ##RRN         OREQ      *ZEROS
     C****               SETON                                        31
     C****               ELSE
     C**** ##RRN         CHAIN     PNW02                              99
     C**** *IN99         IFEQ      *OFF
     C****               MOVEL     NWNWCD        SSNTWK
     C****               MOVEL     NWNAME        SCNWDS
     C****               ENDIF                                                  *IN99 = Off
     C****               ENDIF
     C*** End Del *** 04/05/19 ****************************************
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
B01  C     UPDLIN        IFEQ      *OFF
     C*
     C                   MOVEL(P)  CLMPACCT      SCTRID
     C*
     C     CLMPATCLM#    IFNE      *BLANKS
     C                   MOVEL(P)  CLMPATCLM#    SCCLM#
     C*
     C                   ELSE
     C                   MOVEL(P)  CLMSUBSPI     SCCLM#
     C*
     C                   ENDIF
     C*
     C     CLMsubIrc     IFNE      '18'
     C                   MOVEL(P)  CLMpatLsN     SCCLSN
     C                   MOVEL(P)  CLMpatFsN     SCCFSN
     C*
     C                   ELSE
     C                   MOVEL(P)  CLMsubLsn     SCCLSN
     C                   MOVEL(P)  CLMsubFsn     SCCFSN
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  CLMPRFL       SCPRFL
     C*
     C     CLMsubIrc     IFNE      '18'
     C                   MOVEL(P)  CLMPATLSN     XXLSN            56
     C                   MOVEL(P)  CLMPATFSN     XXFSN            15
     C*
     C                   ELSE
     C                   MOVEL(P)  CLMSUBLSN     XXLSN            56
     C                   MOVEL(P)  CLMSUBFSN     XXFSN            15
     C*
     C                   ENDIF
     C*
     C                   CALL      'SRNAME'
     C                   PARM      XXLSN         ##LAST           56
     C                   PARM      XXFSN         ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL     ##LAST        XXNAME           33
     C*
     C*** Beg Del *** 04/05/19 *******************************************
     C****
     C**** CLMPATCLM#    IFNE      *BLANKS
     C****               MOVEL     CLMPATCLM#    XXCLM#           25
     C****
     C****               ELSE
     C****               MOVEL     CLMSUBSPI     XXCLM#           25
     C****
     C****               ENDIF
     C*** End Del *** 04/05/19 *******************************************
     C*** Beg Add *** 04/05/19 *******************************************
     C                   MOVEL     CLMINSID      XXINSID          20
     C                   MOVEL     CLMPASFX      XXPASFX           3
     C*** End Add *** 04/05/19 *******************************************
     C*
     C                   MOVEL     CLMPACCT      XXPACCT          20
     C*
     C*** Beg Chg *** 04/05/19 *******************************************
     C***21              MOVEL(P)  XXNAME        XXDATA           54
     C***21              MOVE      XXCLM#        XXDATA
     C***21              MOVEL(P)  XXDATA        SCDATA
     C***21              MOVE      XXPACCT       SCDATA
     C   21              MOVEL(P)  XXNAME        XXDATA           49
     C   21              MOVE      XXPACCT       XXDATA
     C   21              MOVEL(P)  XXDATA        SCDATA
     C   21              MOVEL(P)  XXINSID       X1DATA           23
     C   21              MOVE      XXPASFX       X1DATA
     C   21              MOVE      X1DATA        SCDATA
     C*** End Chg *** 04/05/19 *******************************************
     C*
     C                   MOVEL     CLMPRFL       SCERR
     C*
X01  C                   ELSE
     C                   MOVEL     *OFF          UPDLIN
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Transfer to PDS
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   MOVEL     *OFF          UPDLIN
     C                   MOVE      'C'           ##MODE
     C                   MOVE      ' '           ##CALL
     C                   CALL      ACDPGM        ACDPL                  99
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C*
     C     ##RTCD        IFNE      *BLANKS
     C***********        MOVEL     ##RTCD        SCDERR
     C                   ELSE
     C                   MOVEL(P)  TRNSFR        SCDATA
     C                   MOVEL     'Y'           SCPRFL
     C                   MOVEL     *ON           UPDLIN
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVEL     *BLANKS       ##CALL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OPT7 - Work with errors
     C*================================================================
     C*
     C     OPT7          BEGSR
     C*
     C     CLTID         IFEQ      '133'
     C                   MOVEL     *OFF          UPDLIN
     C                   MOVE      'C'           ##MODE
     C                   MOVE      ' '           ##CALL
     C                   CALL      ERRMNT        ACDPL                  99
     C*
     C                   MOVEL     *ON           UPDLIN
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*
     C*================================================================
     C* OPT99 - Delete Bill
     C*================================================================
     C     OPT99         BEGSR
     C*
     C                   MOVEL     *OFF          UPDLIN
     C                   MOVE      'C'           ##MODE
     C                   MOVE      'D'           ##CALL
     C                   CALL      ACDPGM        ACDPL                  99
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  DELETE        SCDATA
     C                   MOVEL     'Y'           SCPRFL
     C                   MOVEL     *ON           UPDLIN
     C*
     C*** Soft delete FALINP records
     C*
     C     LINKY         CHAIN     PFALIN                             99
     C*
     C                   DOW       *IN99 = *OFF
     C*
     C*****Add Field *** EVAL      LINPRFL = 'D'
     C                   UPDATE    PFALIN
     C*
     C     LINKY         READE     PFALIN                                 99
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       ##CALL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C*
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C*
     C*
     C     SCSEL         CASEQ     '99'          OPT99
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL     *OFF          UPDLIN            1
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21              READ      DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21              READP     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
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
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSCLSN        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  CLMPATLSN     U1PDSC
     C     LO:HI         XLATE(P)  SSCLSN        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /free
       ssSS# = UnMaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     SSSS#         IFNE      *BLANKS
     C     SSSS#         ANDNE     CLMSS#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /free
       ssSS# = MaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C*** Beg Del *** 04/05/19 ***************************************
     C*****
     C*****SSPRFL        IFNE      *BLANKS
     C*****SSPRFL        IFNE      CLMPRFL
     C*****              MOVE      *OFF          MEETSS
     C*****              ENDIF
     C*****              ENDIF
     C*** End Del *** 04/05/19 ***************************************
     C*
     C* SUBSET BY UPLOAD DATE
     C*
     C*** Beg Del *** 04/05/19 ***************************************
     C****
     C**** SSULDT        IFNE      *ZEROS
     C****
     C**Convert date
     C****               MOVE      SSULDT        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYMD
     C****               MOVEL     SRDTO         WKULDT            7 0
     C****
     C**** WKULDT        IFNE      CLMULDT
     C****               MOVE      *OFF          MEETSS
     C****               ENDIF
     C****
     C****               ENDIF
     C*** End Del *** 04/05/19 ***************************************
     C*
     C                   ENDSR
** Column headings
Opt Claimant name                Acct#                    Sender ID        Suffix   Errs
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1066=Transfer to Prov. BillsYNN                                    04/20/10
1077=Error Status Maint     NNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   NNN
211F11=Chg View             YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
