     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*
     H/TITLE W2522R - ASC Regions/Groups
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR ASC REGIONS/GROUPS                      *
     H*                                                               *
     H* CALLED BY: F0601R                                             *
     H*                                                               *
     H* CALLS TO: F2522R01, F7046C                                    *
     H*                                                               *
     H* CREATED: 06/22/2001                                           *
     H*                                                               *
     H* 06/22/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 09/28/04 C:79 PROJ: TX026                                     *
     H*               USE OVERRIDE FILES                              *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       DATE MUST BE VALID IN PRINT SCREEN                  *
     H*  72       DATE MUST BE VALID IN SUBSET SCREEN                 *
     H*  73       STATUS MUST BE 'A','I' OR ' ' IN SUBSET SCREEN      *
     H*                                                               *
     H*****************************************************************
     FF2522F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     F*** BEG CHG *** 09/28/04 ****************************************
     F**** FAAGPPA IF  E                    DISK
     FFAAGP     IF   E             DISK
     F*** END CHG *** 09/28/04 ****************************************
     F                                     RENAME(PAG01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEG CHG *** 09/28/04 ****************************************
     F**** FAAGLPA IF  E           K        DISK
     FFAAGL     IF   E           K DISK
     F*** END CHG *** 09/28/04 ****************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAG01:DBRCDF)
     F*  Secondary logical file definition (for displaying items)
     F*** BEG CHG *** 09/28/04 ****************************************
     F**** FAAGL1PAIF  E           K        DISK
     FFAAGL1    IF   E           K DISK
     F*** END CHG *** 09/28/04 ****************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAG01:LOG2)
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
     D  VIEW1                  1     43
     D  V1GRP                  4      5
     D  V1RGN                 11     13
     D  V1EFDT                17     26
     D  V1ALOW                30     37
     D  V1STAT                43     43
     D*
     D                 DS
     D  VIEW2                  1     43
     D  V2EFDT                 1     10
     D  V2RGN                 17     19
     D  V2GRP                 25     26
     D  V2ALOW                30     37
     D  V2STAT                43     43
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ASC Regions/Groups')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2522R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)

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
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
     C     V1KY          KLIST
     C                   KFLD                    KKGRP
     C                   KFLD                    KKRGN
     C                   KFLD                    KKEFDT
      *
     C     V2KY          KLIST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKRGN
     C                   KFLD                    KKGRP
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    AGRGN         KKRGN
     C     *LIKE         DEFINE    AGGRP         KKGRP
     C     *LIKE         DEFINE    AGEFDT        KKEFDT
     C     *LIKE         DEFINE    AGEFDT        S1EFDT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
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
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        72  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C*
     C                   ENDIF                                                  IF SRERR = 1
     C                   ENDIF                                                  IF 90 OFF
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCKP - Error checking for the print prompt screen
     C*================================================================
     C     ERRCKP        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SPEFDT        ANDNE     *ZEROS
     C                   MOVEL     SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SPEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C                   ENDIF                                                  IF SRERR 1
     C                   ENDIF                                                  IF 90 OFF
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
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKRGN
     C                   MOVE      *LOVAL        KKGRP
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL(P)  SCPST1        KKGRP
     C   21              MOVEL(P)  SCPSTA        KKRGN
     C*
     C   22              MOVEL(P)  SCPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C   22              MOVEL(P)  SCPSTB        KKRGN
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   MOVEL(P)  SCGRP         KKGRP
     C                   MOVEL(P)  SCRGN         KKRGN
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   Z-ADD     *ZEROS        SPGRP
     C                   Z-ADD     *ZEROS        SPRGN
     C                   Z-ADD     *ZEROS        SPEFDT
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   EXSR      ERRCKP
     C*
     C                   MOVE      SPGRP         #1GRP
     C                   MOVE      SPRGN         #1RGN
     C*
     C                   MOVE      SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         #1EFDT            7
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     ##CO#         #1CO
     C                   CALL      'F7046C'
     C                   PARM                    #1CO              3
     C                   PARM                    #1GRP             2
     C                   PARM                    #1RGN             3
     C                   PARM                    #1EFDT            7
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
      *
     C                   Z-ADD     AGEFDT        SCEFDT
     C                   Z-ADD     AGGRP         SCGRP
     C                   Z-ADD     AGRGN         SCRGN
     C                   Z-ADD     AGALOW        SCALOW
     C                   MOVE      AGSTAT        SCSTAT
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
      *
     C                   MOVEL     AGGRP         V1GRP
     C                   MOVEL     AGRGN         V1RGN
      *
     C                   Z-ADD     AGEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C                   MOVEL(P)  ##NUM         V1EFDT
      *
     C                   MOVEL(P)  AGALOW        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'X,XXX.XX'    ##MASK
     C                   MOVEL(P)  ##NUM         V1ALOW
      *
     C                   MOVEL     AGSTAT        V1STAT
     C                   MOVEL     VIEW1         SCDATA
      *
     C     *IN22         WHENEQ    *ON
      *
     C                   Z-ADD     AGEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK
     C                   MOVEL(P)  ##NUM         V2EFDT
      *
     C                   MOVEL     AGRGN         V2RGN
     C                   MOVEL     AGGRP         V2GRP
      *
     C                   MOVEL(P)  AGALOW        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'X,XXX.XX'    ##MASK
     C                   MOVEL(P)  ##NUM         V2ALOW
      *
     C                   MOVEL     SCSTAT        V2STAT
     C                   MOVEL     VIEW2         SCDATA
     C                   ENDSL
      *
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      LOG2                                   95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     LOG2                                   95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21V1KY          SETGT     DBRCDF                             96
     C   22V2KY          SETGT     LOG2                               96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21V1KY          SETLL     DBRCDF                             96
     C   22V2KY          SETLL     LOG2                               96
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
     C                   Z-ADD     SCDBRR        @@RRN
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     AGEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     AGSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Group  Region    Eff. Date  Allowance   Status
Opt   Eff. Date   Region  Group  Allowance   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
12020=Change status         YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YNY
217F17=Subset               YYY
221F21=Print list           YYN
3
