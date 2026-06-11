     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*******************************************************************
     H/TITLE F2310R - WORK WITH U&C CODES
     H*****************************************************************
     H*  REVISIONS:
     H*                                                                 *
     H*  09/11/00 C:62 PROJECT:MC205                                    *
     H*                ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.   *
     H*                                                                 *
     H*  05/25/00 C:62 PROJECT:MC205                                    *
     H*                ADD F21=PRINT LIST                               *
     H*                                                                 *
     H*****************************************************************
     FF2310F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMRCP     IF   E             DISK
     F                                     RENAME(PRC01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMRCL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRC01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMRCL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PRC01:DBRCDF2)
     FFMGZP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  XXDATA                 1     22
     D  RCZIPA                 1      3
     D  RCCPT4                 6     10
     D  XXDAY                 13     14
     D  XXSL1                 15     15
     D  XXMNTH                16     17
     D  XXSL2                 18     18
     D  XXYEAR                19     22
     D*
     D                 DS
     D  XXEFDT                 1      8  0
     D  XXDY                   1      2  0
     D  XXMTH                  3      4  0
     D  XXYR                   5      8  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('R&C Codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2310R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)

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
     C*
     C*  Search parameter list.
     C*
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*  Converts the date from MDYY to CYMD.
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDSC1
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    RCZIPA        KKZIPA
     C     *LIKE         DEFINE    RCCPT4        KKCPT4
     C     *LIKE         DEFINE    RCEFDT        KKEFDT
     C     *LIKE         DEFINE    RCEFDT        S1EFDT
     C     *LIKE         DEFINE    RCDSC1        KKDSC1
     C     *LIKE         DEFINE    RCDSC1        U1PDSC
     C     *LIKE         DEFINE    RCDSC1        U2PDSC
     C*** BEGIN ADD *** 09/11/00 **************************************
     C     *LIKE         DEFINE    RCEFDT        ZZEFDT
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSZIPA        IFNE      *BLANKS
B01  C     SSEFDT        ORNE      *ZEROS
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        ORNE      *BLANKS
     C     SSTOCD        ORNE      *BLANKS
     C*** END   ADD *** 05/25/00 **************************************
B01  C     SSDESC        ORNE      *BLANKS
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
     C  N90*INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        IFGT      SSTOCD
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/00 **************************************
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
     C                   SETON                                        8890
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*================================================================
     C* ERRCKP - Error checking for the prompt screen
     C*================================================================
     C     ERRCKP        BEGSR
     C*
     C     SPFRCD        IFGT      SPTOCD
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SPEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZEFDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/11/00 **************************************
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
     C                   MOVE      *LOVAL        KKCPT4
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKDSC1
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH SUBSET SCREEN.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSZIPA'
     C*
     C                   CALL      'F2405R'      SCHPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        7190
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGZ01                              99
     C*
     C                   MOVEL     GZZIPA        SSZIPA
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKZIPA
     C   21              MOVEL     SCPSTC        KKCPT4
     C*
     C   21              Z-ADD     SCPSTD        SRDTI
     C   21              CALL      'SRCVTDAT'    DTCYMD
     C   21              MOVEL     SCPSTD        KKEFDT
     C*
     C   22              MOVEL     SCPST2        KKDSC1
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCZIPA        KKZIPA
     C   21              MOVEL     SCCPT4        KKCPT4
     C   21              MOVE      SCEFDT        KKEFDT
     C   22              MOVEL     SCDSC1        KKDSC1
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/25/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*** BEGIN ADD *** 09/11/00 **************************************
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
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   MOVE      ##CO#         ##CO
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   MOVE      ZZEFDT        ##EFDT
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   CALL      'F7035C'
     C                   PARM                    ##CO              3
     C*** BEGIN CHG *** 09/11/00 **************************************
     C**********           PARM SSFRCD    ##FROM  5
     C**********           PARM SSTOCD    ##TO    5
     C                   PARM      SPFRCD        ##FROM            5
     C                   PARM      SPTOCD        ##TO              5
     C*** END   CHG *** 09/11/00 **************************************
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   PARM                    ##EFDT            7
     C*** END   ADD *** 09/11/00 **************************************
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 05/25/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     RCZIPA        SCZIPA
     C                   MOVEL     RCCPT4        SCCPT4
     C                   MOVE      RCEFDT        SCEFDT
     C                   MOVEL     RCDSC1        SCDSC1
     C*
     C                   Z-ADD     RCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      XXDY          XXDAY
     C                   MOVEL     '/'           XXSL1
     C                   MOVE      XXMTH         XXMNTH
     C                   MOVEL     '/'           XXSL2
     C                   MOVE      XXYR          XXYEAR
     C*
     C                   MOVEL(P)  RCDSC1        XXDSC1           46
     C*
     C   21              MOVEL(P)  XXDATA        SCDATA
     C   21              MOVE      XXDSC1        SCDATA
     C*
     C   22              MOVEL(P)  XXDSC1        SCDATA
     C   22              MOVE      XXDATA        SCDATA
     C*
     C                   MOVEL     RCSTAT        SCSTAT
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
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 05/25/00 **************************************
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
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
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
     C                   PARM      50            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      50            PATLEN            3 0
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
     C     SSZIPA        IFNE      *BLANKS
     C     SSZIPA        ANDNE     RCZIPA
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     RCEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  RCDSC1        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        IFNE      *BLANKS
     C     SSFRCD        ANDGT     RCCPT4
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSTOCD        IFNE      *BLANKS
     C     SSTOCD        ANDLT     RCCPT4
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/00 **************************************
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     RCSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Zip  Proc    Eff. Date  Description                                    Sts
Description                                     Zip  Proc    Eff. Date Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
