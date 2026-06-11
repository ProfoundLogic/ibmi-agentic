     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               * *
     H*  08/08/02 C:62 PROJECT FS244                                  * *
     H*                LOADING EFFECTIVE DATE                         * *
     H*                                                               * *
     H*  12/26/00 C:62 PROJECT:GU099 25559                            * *
     H*                -CHANGED F20=CHANGE STATUS TO 20=CHANGE STATUS.* *
     H*                                                               * *
     H*  09/11/00 C:62 PROJECT:MC205                                  * *
     H*                -ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.* *
     H*                -ADD F21=PRINT LIST                            * *
     H*                                                               *
     H*****************************************************************
     FF2405F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMGZP     IF   E             DISK
     F                                     RENAME(PGZ01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 08/08/02 **************************************
     F**********FMGZL1  IF  E           K        DISK
     FFMGZL3    IF   E           K DISK
     F*** END   CHG *** 08/08/02 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PGZ01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMGZL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PGZ01:DBRCDF2)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*** BEGIN ADD *** 08/08/02 **************************************
     D                 DS
     D  XXDATE                 1     10
     D  XXDAY                  1      2
     D  SLASH1                 3      3    INZ('/')
     D  XXMNTH                 4      5
     D  SLASH2                 6      6    INZ('/')
     D  XXYEAR                 7     10
     D*** END   ADD *** 08/08/02 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Zip Areas')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2405R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEGIN ADD *** 09/11/00 **************************************
     D FLAG            C                   CONST('     1   ')

     I*** END   ADD *** 09/11/00 **************************************

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
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEGIN CHG *** 08/08/02 **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END   CHG *** 08/08/02 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   KFLD                    KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDESC
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   KFLD                    KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C     *LIKE         DEFINE    GZDESC        KKDESC
     C     *LIKE         DEFINE    GZDESC        U1PDSC
     C     *LIKE         DEFINE    GZDESC        U2PDSC
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     *LIKE         DEFINE    GZEFDT        KKEFDT
     C     *LIKE         DEFINE    GZEFDT        XXEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     SSEFDT        ORNE      *ZEROS
     C*** END   ADD *** 08/08/02 **************************************
B01  C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
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
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     SSEFDT        IFNE      *ZEROS
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        8890
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKZIPA
     C                   MOVE      *LOVAL        KKDESC
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   MOVE      *LOVAL        KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL(P)  SCPST1        KKZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     *IN21         IFEQ      *ON
     C                   Z-ADD     SCPED1        SRDTI
     C                   ENDIF
     C*** END   ADD *** 08/08/02 **************************************
     C   22              MOVEL(P)  SCPST2        KKDESC
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     *IN22         IFEQ      *ON
     C                   Z-ADD     SCPED2        SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL(P)  SCZIPA        KKZIPA
     C   22              MOVEL(P)  SCDESC        KKDESC
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   Z-ADD     SCEFDT        KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C                   ENDSR
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
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     GZZIPA        SCZIPA
     C                   MOVEL     GZDESC        SCDESC
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   Z-ADD     GZEFDT        SCEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C*** BEGIN CHG *** 08/08/02 **************************************
     C***21*****           MOVELGZZIPA    SCDATA    P
     C***21*****           MOVE GZDESC    SCDATA
     C*
     C***22*****           MOVELGZDESC    SCDATA    P
     C***22*****           MOVE GZZIPA    SCDATA
     C                   Z-ADD     GZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYDATE            8
     C*
     C                   RESET                   XXDATE
     C*
     C     2             SUBST(P)  YYDATE:1      XXDAY
     C     2             SUBST(P)  YYDATE:3      XXMNTH
     C     4             SUBST(P)  YYDATE:5      XXYEAR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  GZZIPA        XXDAT1           15
     C                   MOVE      XXDATE        XXDAT1
     C                   MOVEL(P)  XXDAT1        SCDATA
     C                   MOVE      GZDESC        SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  GZDESC        XXDAT2           52
     C                   MOVE      XXDATE        XXDAT2
     C                   MOVEL(P)  XXDAT2        SCDATA
     C                   MOVE      GZZIPA        SCDATA
     C                   ENDSL
     C*** END   CHG *** 08/08/02 **************************************
     C*
     C                   MOVEL     GZFRZP        SCFRZP
     C                   MOVEL     GZTOZP        SCTOZP
     C                   MOVEL     GZSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** BEGIN ADD *** 09/11/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 09/11/00 **************************************
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
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
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
     C                   MOVEL     SCDBRR        @@RRN
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
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  GZDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     SSEFDT        IFNE      *ZEROS
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     XXEFDT        IFNE      GZEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     GZSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Area     Date    Description                                Covered  Sts
Description                                  Date    Area   Covered  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
