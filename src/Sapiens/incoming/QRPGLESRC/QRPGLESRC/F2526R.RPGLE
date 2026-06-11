     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*
     H/TITLE F2526R - ASC by Providers
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H* WORK WITH ASC BY PROVIDER                                     *
     H*                                                               *
     H* CALLED BY: F0601R                                             *
     H* CALLS TO : F2526R01, F7056C                                   *
     H*                                                               *
     H*****************************************************************
     H* Created 08/01/2001 by C:62 for Project:114MCG                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 04/01/02 C:79 PROJ: 114MCG                                    *
     H*               CORRECT THE DISPLAY OF THE ALLOWANCE FIELD      *
     H*                                                               *
     H* 11/18/08 C:198 PROJECT PA046                                  *
     H*                Added NPI# after the group# on the screen      *
     H*                SCR01.                                         *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       GENERAL READ/CHAIN                                  *
     H*  71       DATE MUST BE VALID IN PRINT SCREEN                  *
     H*  72       DATE MUST BE VALID IN SUBSET SCREEN                 *
     H*  73       STATUS MUST BE 'A','I' OR ' ' IN SUBSET SCREEN      *
     H*                                                               *
     H*****************************************************************
     FF2526F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAAPPPA   IF   E             DISK
     F                                     RENAME(PAP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAAPLPA   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAP01:DBRCDF)
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
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ASC by Providers')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2526R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
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
     C                   KFLD                    KKMED#
     C                   KFLD                    KKGRP
     C                   KFLD                    KKEFDT
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    APMED#        KKMED#
     C     *LIKE         DEFINE    APGRP         KKGRP
     C     *LIKE         DEFINE    APEFDT        KKEFDT
     C     *LIKE         DEFINE    APEFDT        S1EFDT
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
     C                   SETON                                        72  90
     C                   ELSE
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
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
     C                   MOVE      *LOVAL        KKMED#
     C                   MOVE      *LOVAL        KKGRP
     C                   MOVE      *LOVAL        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL(P)  SCPST1        KKMED#
     C   21              Z-ADD     SCPSTG        KKGRP
     C   21              Z-ADD     *LOVAL        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL(P)  SCMED#        KKMED#
     C   21              Z-ADD     SCGRP         KKGRP
     C   21              Z-ADD     SCEFDT        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
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
     C                   MOVE      SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         #1EFDT
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     ##CO#         #1CO
     C*
     C                   CALL      'F7056C'
     C                   PARM                    #1CO              3
     C                   PARM                    #1EFDT            7
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C                   PARM      @@ST          ##ST
     C*
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
     C                   MOVEL(P)  APMED#        SCMED#
     C                   Z-ADD     APGRP         SCGRP
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   MOVEL(P)  APNPI#        SCNPI#
     C*** END ADD *** 11/18/08 ****************************************
     C                   Z-ADD     APMSA         SCMSA
     C                   Z-ADD     APEFDT        SCEFDT
     C*
     C                   Z-ADD     APEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD2
     C*
     C                   Z-ADD     APALOW        SCALOW
     C                   MOVE(P)   APSTAT        SCSTAT
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21V1KY          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21V1KY          SETLL     DBRCDF                             96
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
     C*
     C     S1EFDT        IFNE      APEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     APSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Medicare#     Group  NPI#             MSA     Eff. Date   Allowance  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change view          NNN
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
