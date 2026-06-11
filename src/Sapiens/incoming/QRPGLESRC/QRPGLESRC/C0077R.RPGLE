     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H/TITLE C0077R - WORK WITH CLAIMANTS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY CLAIMANTS            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    C0077R02                                                   *
     H*                                                               *
     H*  CREATED: 02/08/05  PROGRAMMER: C:53  PROJECT: PC011          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  00/00/00 C:00  PROJECT:
     H*****************************************************************
     H*
     FC0077F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FCMCLP     IF   E             DISK
     F                                     RENAME(PCL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FCMCLL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCL01:DBRCDF)
     F*  Secondary logical file for view #2
     FCMCLL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCL01:DBRCDF2)
     FCMUPL     IF   E           K DISK
     FCMCMP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D*** V1
     D  SCDATA                 1     70
     D  XSCNAM                 1     30
     D  XSCLID                33     46
     D  XSCLM#                49     60
     D*** V2
     D  X2CLM#                 1     12
     D  X2CLID                17     21
     D  X2CNAM                33     64
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCCLM#                 4     15
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Claimants')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('C0077R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CLM#           12
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@CLM#        ##CLM#           12
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEYR          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCNAM
     C                   KFLD                    KKCLID
     C                   KFLD                    KKCLM#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKCLID
     C                   KFLD                    KKCNAM
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CLCO#         KKCO#
     C     *LIKE         DEFINE    CLCNAM        KKCNAM
     C     *LIKE         DEFINE    CLCLID        KKCLID
     C     *LIKE         DEFINE    CLCLM#        KKCLM#
     C     *LIKE         DEFINE    CLCNAM        U1PDSC
     C     *LIKE         DEFINE    CLCNAM        U2PDSC
     C     *LIKE         DEFINE    CLCLM#        QQCLM#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSCLM#        IFGT      *BLANKS
     C     SSCNAM        ORGT      *BLANKS
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
     C     Q1PRMS        IFGT      4
     C     @@RRN         IFNE      *ZEROS
     C     @@RRN         CHAIN     CMCMP                              60
     C                   MOVEL(P)  CMCLM#        SSCLM#
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*   CLAIMANT SEARCH
     C*
     C     HFLD          WHENEQ    'SSCLM#'
     C***?????             MOVE SSPOL#    QQPOL#
     C                   MOVE      SSCLM#        QQCLM#
     C                   CALL      'C0700R'
     C                   PARM      SCPRDS        QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQCLM#
     C                   PARM                    QQRTC             1
     C****?????            MOVE QQPOL#    SSPOL#
     C                   MOVE      QQCLM#        SSCLM#
     C*
     C     QQRTC         IFEQ      'B'
     C     QQCLM#        OREQ      *BLANK
     C     QQPOL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SSCNAM'
     C*****???
     C*****???             CALL 'C0530R'
     C*****???             PARM           QQPRDS 40
     C*****???             PARM           QQCALL  1
     C*****???             PARM           QQPOL#
     C*****???             PARM           QQRTC   1
     C*****???             MOVE QQPOL#    SSPOL#
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCNAM
     C                   MOVE      *LOVAL        KKCLID
     C                   MOVE      *LOVAL        KKCLM#
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      *LOVAL        KKCNAM
     C                   MOVE      *LOVAL        KKCLID
     C                   MOVE      *LOVAL        KKCLM#
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     SCPST1        IFNE      *BLANKS
     C                   MOVEL(P)  SCPST1        KKCNAM
     C                   ENDIF
     C*
     C     *IN22         WHENEQ    *ON
     C     SCPST2        IFNE      *BLANKS
     C                   MOVEL(P)  SCPST2        KKCLM#
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
     C                   MOVEL     SCCNAM        KKCNAM
     C                   MOVEL     SCCLID        KKCLID
     C                   MOVEL     SCCLM#        KKCLM#
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     CLCO#         SCCO#
     C                   MOVEL     CLCNAM        SCCNAM
     C                   MOVEL     CLCLID        SCCLID
     C                   MOVEL     CLCLM#        SCCLM#
     C*
     C                   MOVE      *BLANKS       SCDATA
     C*
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  SCCNAM        XSCNAM
     C                   MOVEL(P)  SCCLM#        XSCLM#
     C                   MOVEL(P)  SCCLID        XSCLID
     C*
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  SCCNAM        X2CNAM
     C                   MOVEL(P)  SCCLM#        X2CLM#
     C                   MOVEL(P)  SCCLID        X2CLID
     C*
     C                   ENDSL
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
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '14'          OPT14
     C                   ENDCS
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
     C     OPT7          BEGSR
     C*
     C* IF SUBSETTED WE MAY BE COMING FROM C0071R
     C* IF SO, CREATE A COPY OF F0269R IN QTEMP TO AVOID RECURSIVE CALL
     C*
     C     @@RRN         IFNE      *ZEROS
     C                   MOVEL(P)  'C0071R'      #FNAME           10
     C                   MOVEL(P)  'C0071Q'      #TNAME           10
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
     C                   CALL      'C0071Q'
     C                   PARM      '1'           @@CALL            1
     C                   PARM      SCCLM#        @@CLM#           12
     C                   ENDIF                                                  ##RTN = 'A'
     C                   ELSE
      *
     C* CHECK TO SEE IF C0071Q IS ON THE CALL STACK.
      *
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'C0071R'      XXPGM            10
     C                   PARM      *BLANKS       XXERR             1
     C*
     C     XXERR         IFEQ      '1'
     C                   MOVE      '1'           @@CALL            1
     C                   ELSE
     C                   MOVE      ' '           @@CALL            1
     C                   ENDIF
     C*
     C                   CALL      'C0071R'
     C                   PARM                    @@CALL            1
     C                   PARM      SCCLM#        @@CLM#           12
     C                   ENDIF
     C*
     C**                   PARM SCDBRR    @@RRN   90
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
     C     Q1USER        CHAIN     PUP01                              60
     C                   MOVE      UPCO#         KKCO#
      *
     C* CHECK TO SEE IF C0071R IS ON THE CALL STACK.
      *
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'C0071R'      XXPGM            10
     C                   PARM      *BLANKS       XXERR             1
     C*
     C     XXERR         IFNE      '1'
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '06'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  WCFL09=Y
      *
      *
     C* CHECK TO SEE IF C0071Q IS ON THE CALL STACK.
      *
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'C0071Q'      XXPGM            10
     C                   PARM      *BLANKS       XXERR             1
     C*
     C     XXERR         IFEQ      '1'
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
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEYR          READPE    DBRCDF                                 95
     C   22KEYR          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
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
     C     CLCLM#        ANDNE     SSCLM#
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSCNAM        WHENNE    *BLANKS
     C     LO:UP         XLATE(P)  CLCNAM        U1PDSC
     C     LO:UP         XLATE(P)  SSCNAM        U2PDSC
     C                   EXSR      STRING
     C*
     C                   ENDSL
     C*
     C                   ENDSR
** Column headings
Opt  Claimant Name                   Claimant ID     Claim #
Opt  Claim #         Claimant ID     Claimant Name
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1077=Claims info            YYY
11414=DARTS                 YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
