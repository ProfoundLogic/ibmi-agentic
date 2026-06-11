     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*  F8079R - Work with Ancillary Bills with Errors               *
     H*                                                               *
     H*  Called from: Menu                                            *
     H*  Calls to   : F8079R01 - Ancillary Bill Information           *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/05/2011 by c:198 for Project 45MHZ             *
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
     FF8079F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAC1P     IF   E             DISK
     F                                     RENAME(PC101:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAC1L3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PC101:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFAC1L4    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PC101:DBRCDF2)
     FFAC1L6    IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

      /COPY SCOPYWW01D
     D*
     D HD              S             79    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  SS#IN                  1      9
     D  SS#1IN                 1      3
     D  SS#2IN                 4      5
     D  SS#3IN                 6      9
     D                 DS
     D  SS#OU                  1     11
     D  SS#1OU                 1      3
     D  HYPH1                  4      4    INZ('-')
     D  SS#2OU                 5      6
     D  HYPH2                  7      7    INZ('-')
     D  SS#3OU                 8     11
     D                 DS
     D  DATIN                  1      8
     D  DAT1IN                 1      2
     D  DAT2IN                 3      4
     D  DAT3IN                 5      8
     D                 DS
     D  DATOU                  1     10
     D  DAT1OU                 1      2
     D  SLSH1                  3      3    INZ('/')
     D  DAT2OU                 4      5
     D  SLSH2                  6      6    INZ('/')
     D  DAT3OU                 7     10
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Ancillary Bills with-
     D                                      Errors')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F8079R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(11)
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
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
     C     KEY1          KLIST
     C                   KFLD                    KKBLID
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKSSN
     C                   KFLD                    KKDOI
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    C1AUTH        KKBLID
     C     *LIKE         DEFINE    C1PSS#        KKSSN
     C     *LIKE         DEFINE    C1ACDT        KKDOI
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       SSSSN = UnMaskData('SSN':'SSSSN':SSSSN);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     SSSSN         IFNE      *BLANKS
     C     SSPRFL        ORNE      *BLANKS
     C                   SETON                                        11
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CNTERR - Count Errors
     C*================================================================
     C     CNTERR        BEGSR
     C*
     C                   CLEAR                   SCECNT
     C                   CLEAR                   SCPCNT
     C*
     C     *LOVAL        SETLL     PC101
     C                   READ      PC101                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C     C1PRFL        WHENEQ    'E'
     C                   ADD       1             SCECNT
     C     C1PRFL        WHENEQ    'P'
     C                   ADD       1             SCPCNT
     C                   ENDSL
     C*
     C                   READ      PC101                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         IFEQ      '4'
     C     SCPRFL        IFEQ      'E'
     C     SCPRFL        OREQ      'P'
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
     C     SCPRFL        ANDNE     'P'
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
     C     SSPRFL        IFNE      'E'
     C     SSPRFL        ANDNE     'P'
     C     SSPRFL        ANDNE     ' '
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       KKBLID
     C                   MOVEL(P)  *BLANKS       KKSSN
     C                   MOVEL(P)  *BLANKS       KKDOI
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKBLID
     C     *IN22         WHENEQ    *ON
      ***** Beg Add *** 06/09/20 *******************************************
     C***                MOVEL(P)  SCPST2        KKSSN
      /Free
       kkSSN = UnMaskData('SSN':'SCPST2':SCPST2);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   MOVE(P)   SCPST3        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         KKDOI
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCBLID        KKBLID
     C                   MOVEL(P)  SCSSN         KKSSN
     C                   MOVEL(P)  SCDOI         KKDOI
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     UPDLIN        IFEQ      *OFF
     C*          SFRRN1    ORGT 1
     C*
     C                   MOVEL(P)  C1AUTH        SCBLID
     C                   MOVEL(P)  C1PSS#        SCSSN
     C                   MOVEL(P)  C1ACDT        SCDOI
     C                   MOVEL(P)  C1PRFL        SCPRFL
     C*
     C                   MOVEL     C1PSS#        SS#IN
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     SS#1IN        SS#1OU
     C***                MOVEL     SS#2IN        SS#2OU
     C***                MOVEL     SS#3IN        SS#3OU
      /Free
       SS#OU = DisplaySS#(MaskData('SSN':'C1PSS#':C1PSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   MOVEL     C1ACDT        XXDATE            8
     C                   MOVE      XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         DATIN
     C                   MOVEL     DAT1IN        DAT1OU
     C                   MOVEL     DAT2IN        DAT2OU
     C                   MOVEL     DAT3IN        DAT3OU
     C*
     C                   MOVEL(P)  SS#OU         XXSSID           23
     C                   MOVE      DATOU         XXSSID
     C*
     C   21              MOVEL(P)  C1AUTH        SCDATA
     C   21              MOVE      XXSSID        SCDATA
     C*
     C   22              MOVEL(P)  XXSSID        SCDATA
     C   22              MOVE      C1AUTH        SCDATA
     C*
     C                   MOVEL     C1PRFL        SCERR
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *OFF          UPDLIN
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
     C                   MOVEL     'P'           SCPRFL
     C                   MOVEL     'P'           SCERR
     C                   ELSE
     C                   MOVEL(P)  TRNSFR        SCDATA
     C                   MOVEL     'Y'           SCPRFL
     C                   ENDIF
     C                   MOVEL     *ON           UPDLIN
     C*
     C                   ENDIF
     C                   MOVEL     *BLANKS       ##CALL
     C                   EXSR      CNTERR
     C*
     C                   ENDSR
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
     C                   ENDIF
     C                   MOVEL     *BLANKS       ##CALL
     C                   EXSR      CNTERR
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
     C                   MOVEL     *OFF          UPDLIN            1
     C                   EXSR      CNTERR
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       SSSSN = UnMaskData('SSN':'SSSSN':SSSSN);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C     SSSSN         IFNE      *BLANKS
     C     SSSSN         ANDNE     C1PSS#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSPRFL        IFNE      *BLANKS
     C     SSPRFL        IFNE      C1PRFL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt Doc ID                         SS#     Injury Date  Type
Opt     SS#     Injury Date    Doc ID                   Type
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1066=Transfer to Prov. BillsYNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   NNN
211F11=Chg View             YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
