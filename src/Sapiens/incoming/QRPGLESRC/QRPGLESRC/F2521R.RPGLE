     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W2521R - DRG FEDERAL TABLE LISTING
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR DRG FEDERAL TABLE LISTING              *
     H*                                                               *
     H* CALLED BY: F0601R                                             *
     H*                                                               *
     H* CALLS TO:  F7042C                                             *
     H*                                                               *
     H* CREATED: 06/04/2001                                           *
     H*                                                               *
     H* 06/04/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disabled option 5 and function 17 for GS
     H*                                                               *
     H* 11/04/02 C: 79 PROJECT: FSMNT                                 *
     H*                CHANGE SUBFILE PAGE SIZE SO F21 DISPLAYS       *
     H*                                                               *
     H* 09/25/02 C: 79 PROJECT: CA001                                 *
     H*                USE GENERIC FILE FOR SUBFILE NOT STATE SPECIFIC*
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SPFRCD CANNOT BE GREATER THAN SPTOCD FOR RANGE PRT  *
     H*  72       STATUS MUST BE 'A','I', OR ' ' ON SUBSET
     H*                                                               *
     H*****************************************************************
     H*
     FF2521F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*** BEG ADD *** 09/25/02 ****************************************
     F*  Physical file definition (for refreshing subfile record)
     FFTFEP     IF   E             DISK
     F                                     RENAME(PFE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTFEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFE01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFTFEL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PFE01:DBRCDF2)
     F*** END ADD *** 09/25/02 ****************************************
     F*
     F*** BEG DEL *** 09/25/02 ****************************************
     F**********
     F***Physical file definition (for refreshing subfile record)
     F**FTFEPPA*IF  E                    DISK
     F**********  PFE01                             KRENAMEPFRCDF
     F**********
     F***Primary logical file definition (for displaying items)
     F**FTFELPA*IF  E           K        DISK
     F**********                                    KINFDS DBFDS
     F**********  PFE01                             KRENAMEDBRCDF
     F**********
     F***Secondary logical file definition (for displaying items)
     F**FTFEL1PAIF  E           K        DISK
     F**********                                    KINFDS DBFDS2
     F**********  PFE01                             KRENAMEDBRCDF2
     F*** END DEL *** 09/25/02 ****************************************
     FSTABL     IF   E           K DISK
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
     D  VIEW1                  1     74
     D  V1DRG#                 1      5  0
     D  V1DESC                 8     47
     D  V1RWT1                50     51
     D  V1DECP                52     52
     D  V1RWT2                53     56
     D  V1STAT                63     63
     D*
     D                 DS
     D  VIEW2                  1     74
     D  V2DESC                 1     40
     D  V2DRG#                42     47  0
     D  V2RWT1                50     51
     D  V2DECP                52     52
     D  V2RWT2                53     56
     D  V2STAT                63     63
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('DRG Federal Register-
     D                                      Table')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2521R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKDRG#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKDESC
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FEDRG#        KKDRG#
     C     *LIKE         DEFINE    FEDESC        KKDESC
     C     *LIKE         DEFINE    FEDESC        U1PDSC
     C     *LIKE         DEFINE    FEDESC        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSDESC        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
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
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCKP - Error checking for the prompt screen
     C*================================================================
     C     ERRCKP        BEGSR
     C*
     C     SPFRCD        IFGT      SPTOCD
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C   21              MOVE      *LOVAL        KKDRG#
     C   22              MOVE      *LOVAL        KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL     SCPST1        KKDRG#
     C   22              MOVEL(P)  SCPST2        KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              Z-ADD     SCDRG#        KKDRG#
     C   22              MOVEL(P)  SCDESC        KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C                   MOVEL(P)  *BLANKS       SPFRCD
     C                   MOVEL(P)  *BLANKS       SPTOCD
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   EXSR      ERRCKP
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     ##CO#         #1CO
     C                   CALL      'F7042C'
     C                   PARM                    #1CO              3
     C                   PARM                    SPFRCD
     C                   PARM                    SPTOCD
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C                   PARM      @@ST          ##ST
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
      *
      * DISPLAY STATE CODE AND STATE NAME IN HEADER
      *
     C                   MOVEL(P)  @@ST          KKST
     C                   MOVEL     @@ST          SCST
      *
     C     STKY          CHAIN     PST01                              99
      *
     C                   MOVEL(P)  STNAME        SCSTNM
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     FEDRG#        SCDRG#
     C                   MOVEL     FEDESC        SCDESC
     C                   Z-ADD     FERLWT        SCRLWT
     C                   MOVEL     FESTAT        SCSTAT
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   Z-ADD     SCDRG#        V1DRG#
     C                   MOVEL     SCDESC        V1DESC
     C                   MOVEL     SCRLWT        V1RWT1
     C                   MOVE      '.'           V1DECP
     C                   MOVE      SCRLWT        V1RWT2
     C                   MOVEL     SCSTAT        V1STAT
     C                   MOVEL     VIEW1         SCDATA
     C     *IN22         WHENEQ    *ON
     C                   Z-ADD     SCDRG#        V2DRG#
     C                   MOVEL     SCDESC        V2DESC
     C                   MOVEL     SCRLWT        V2RWT1
     C                   MOVE      '.'           V2DECP
     C                   MOVE      SCRLWT        V2RWT2
     C                   MOVEL     SCSTAT        V2STAT
     C                   MOVEL     VIEW2         SCDATA
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C*** Beg Add *** MC367 *** 11/19/12 *****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/19/12 *****************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
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
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSDESC        IFNE      *BLANKS
     C     LO:UP         XLATE(P)  FEDESC        U1PDSC
     C     LO:UP         XLATE(P)  SSDESC        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     FESTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt   DRG#  Description                                Weight   Status
Opt   Description                               DRG#   Weight   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YNY
217F17=Subset               YYY
221F21=Print List           YYN
3
