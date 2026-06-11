     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H/define CONVERT
     H*
     H/TITLE F2450R - WW Outcome/Savings Codes
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY:                                                   *
     H*                                                               *
     H*  CALLS TO:  F2450R01 - MT Outcome/Savings Code                *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*     05/04/98 C:61 PROJECT 98MAC                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               * *
     H*  01/02/01 C:62 PROJECT:MCMNT         BASE ONLY                * *
     H*                -ADD SUBSET LOGIC.                             * *
     H*                                                               * *
     H*  09/11/00 C:62 PROJECT:MC205                                  * *
     H*                -ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.* *
     H*                -ADD F21=PRINT LIST                            * *
     H*                                                               *
     H*  XX/XX/XX C:XX PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
      *
     FF2450F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FFTOTP     IF   E             DISK
     F                                     RENAME(POT01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FFTOTL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POT01:DBRCDF)
      *
      *  Secondary logical file definition (for displaying items)
     FFTOTL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(POT01:DBRCDF2)
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Outcome/Savings Code-
     D                                     s')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2450R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(11)
     D*** BEGIN ADD *** 09/11/00 **************************************
     D FLAG            C                   CONST('   1     ')

     I*** END   ADD *** 09/11/00 **************************************
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FTOTP
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          FTOTP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     KEY           KLIST
     C                   KFLD                    KKCO#
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDESC
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    OTCO#         KKCO#
     C     *LIKE         DEFINE    OTCODE        KKCODE
     C     *LIKE         DEFINE    OTDESC        KKDESC
      *
      *
     C/COPY SCOPYWW01C
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C*** BEGIN ADD *** 01/02/01 **************************************
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*** END   ADD *** 01/02/01 **************************************
     C                   ENDSR
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
      * Refresh WW screen after delete
      *
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C*
     C*** BEGIN ADD *** 01/02/01 **************************************
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*** END   ADD *** 01/02/01 **************************************
     C*
     C                   ENDSR
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C*
     C*** BEGIN ADD *** 01/02/01 **************************************
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*** END   ADD *** 01/02/01 **************************************
     C*
     C                   ENDSR
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL(P)  *LOVAL        KKCODE
     C                   MOVEL(P)  *LOVAL        KKDESC
     C                   ENDSR
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
      *
     C   21              MOVEL(P)  SCPST1        KKCODE
     C   21              MOVEL(P)  *BLANKS       SCPST1
      *
     C   22              MOVEL(P)  SCPST2        KKDESC
     C   22              MOVEL(P)  *BLANKS       SCPST2
      *
     C                   ENDSR
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
      *
     C   21              MOVEL(P)  SCCODE        KKCODE
      *
     C   22              MOVEL(P)  SCDESC        KKDESC
      *
     C                   ENDSR
      *
      *
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         ##CO
     C                   MOVEL(P)  FLAG          ##FLAG
     C*
     C                   CALL      'F7050C'
     C                   PARM                    ##CO              3
     C                   PARM                    ##FLAG            9
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/11/00 **************************************
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL(P)  OTCODE        SCCODE
     C                   MOVEL(P)  OTDESC        SCDESC
     C                   MOVEL(P)  OTRTVL        SCRTVL
     C                   Z-ADD     OTSVRT        SCSVRT
     C                   Z-ADD     OTPERC        SCPERC
     C                   MOVEL(P)  OTSTAT        SCSTAT
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  OTCODE        SCDATA
     C     30            SUBST(P)  OTDESC        XXDESC           30
     C                   MOVE      XXDESC        SCDATA
      *
     C     VIEW#         WHENEQ    2
     C     30            SUBST(P)  OTDESC        XXDESC           30
     C                   MOVEL(P)  XXDESC        SCDATA
     C                   MOVE      OTCODE        SCDATA
      *
     C                   OTHER
     C                   MOVEL(P)  *BLANKS       SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *
      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C*** BEGIN ADD *** 09/11/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 09/11/00 **************************************
      *
     C                   ENDSR
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
      *
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      * Initialize
      *
     C                   Z-ADD     ##CO#         KKCO#
      *
     C                   ENDSR
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C                   ENDSR
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C                   ENDSR
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C                   ENDSR
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   Z-ADD     SCDBRR        @@RRN
      *
     C                   ENDSR
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
     C*
     C*** BEGIN ADD *** 01/02/01 **************************************
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     OTSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 01/02/01 **************************************
     C*
     C                   ENDSR
** Column headings
Opt Code        Description                    Value     Rate      %   Status
Opt Description                     Code       Value     Rate      %   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NYY
217F17=Subset               YYY
221F21=Print list           YYN
3
