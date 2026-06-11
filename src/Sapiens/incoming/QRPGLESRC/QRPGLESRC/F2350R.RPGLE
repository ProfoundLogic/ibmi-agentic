     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  05/11/18 C:180 PROJECT:114MJS                                * *
     H*                -RECOMPILE ONLY.                               * *
     H*                                                               * *
     H*  02/25/02 C:62 PROJECT:114MBH                                 * *
     H*                -CHANGE SCSTRN WITH SCSTRG.                    * *
     H*                                                               * *
     H*  09/11/00 C:62 PROJECT:MC205                                  * *
     H*                -ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.* *
     H*                -ADD F21=PRINT LIST                            * *
     H*                                                               *
     H*  08/26/99 C:62 Proj: 25MMG                                    *
     H*                ADD ADDITIONAL VIEW FOR OLD NDC VALUE          *
     H*                                                               *
     H*****************************************************************
     FF2350F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMRXP     IF   E             DISK
     F                                     RENAME(PRX01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMRXL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRX01:DBRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMRXL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PRX01:DBRCDF2)
     F*** BEGIN ADD *** 08/26/99 **************************************
     F*
     F*  Primary logical file definition (for displaying items)
     FFMRXL2    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PRX01:DBRCDF3)
     F*** END   ADD *** 08/26/99 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('NDC Drug Codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2350R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D*** BEGIN ADD *** 09/11/00 **************************************
     D FLAG            C                   CONST('      1  ')

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
     C*
     C*  CONVERTS DATE FROM MDYY TO CYMD
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
     C                   KFLD                    KKNDC
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNAME
     C*** BEGIN ADD *** 08/26/99 **************************************
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKONDC
     C                   KFLD                    KKNDC
     C*** END   ADD *** 08/26/99 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    RXNDC         KKNDC
     C     *LIKE         DEFINE    RXNAME        KKNAME
     C*** BEGIN ADD *** 08/26/99 **************************************
     C     *LIKE         DEFINE    RXONDC        KKONDC
     C*** END   ADD *** 08/26/99 **************************************
     C     *LIKE         DEFINE    RXAWPD        S1AWPD
     C     *LIKE         DEFINE    RXNAME        U1PDSC
     C     *LIKE         DEFINE    RXNAME        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSNAME        IFNE      *BLANKS
B01  C     SSAWPD        ORNE      *ZEROS
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
     C     *IN90         IFEQ      *OFF
     C     SSAWPD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSAWPD
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        8890
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1AWPD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
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
     C   21              MOVE      *LOVAL        KKNDC
     C   22              MOVE      *LOVAL        KKNAME
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23              MOVE      *LOVAL        KKONDC
     C   23              MOVE      *LOVAL        KKNDC
     C*** END   ADD *** 08/26/99 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKNDC
     C   22              MOVEL     SCPST2        KKNAME
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23              MOVEL     SCPST3        KKONDC
     C   23              MOVEL     *BLANKS       KKNDC
     C*** END   ADD *** 08/26/99 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCNDC         KKNDC
     C   22              MOVEL     SCNAME        KKNAME
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23              MOVEL     SCONDC        KKONDC
     C   23              MOVEL     SCNDC         KKNDC
     C*** END   ADD *** 08/26/99 **************************************
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
     C                   MOVEL     RXNDC         SCNDC
     C*** BEGIN ADD *** 08/26/99 **************************************
     C                   MOVEL     RXONDC        SCONDC
     C*** END   ADD *** 08/26/99 **************************************
     C                   MOVEL     RXNAME        SCNAME
     C*
     C                   MOVEL     RXNAME        XXNAME           30
     C*
     C   21              MOVEL(P)  RXNDC         SCDATA
     C   21              MOVE      XXNAME        SCDATA
     C*
     C   22              MOVEL(P)  XXNAME        SCDATA
     C   22              MOVE      RXNDC         SCDATA
     C*** BEGIN ADD *** 08/26/99 **************************************
     C*
     C   23              MOVEL(P)  RXONDC        SCDATA
     C   23              MOVE      XXNAME        SCDATA
     C*** END   ADD *** 08/26/99 **************************************
     C*
     C*** BEGIN CHG *** 02/25/02 **************************************
     C**********           MOVELRXSTRN    SCSTRN
     C                   MOVEL     RXSTRG        SCSTRG
     C*** END   CHG *** 02/25/02 **************************************
     C                   MOVEL     RXFORM        SCFORM
     C*
     C                   Z-ADD     RXAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAWPD
     C*
     C                   MOVEL     RXSTAT        SCSTAT
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
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C*** BEGIN ADD *** 09/11/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C*** END   ADD *** 09/11/00 **************************************
     C                   CAS                     ENTRDO
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
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23KEY1          READE     DBRCDF3                                95
     C*** END   ADD *** 08/26/99 **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23KEY1          READPE    DBRCDF3                                95
     C*** END   ADD *** 08/26/99 **************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23KEY4          SETGT     DBRCDF3                            96
     C*** END   ADD *** 08/26/99 **************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*** BEGIN ADD *** 08/26/99 **************************************
     C   23KEY4          SETLL     DBRCDF3                            96
     C*** END   ADD *** 08/26/99 **************************************
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
     C                   PARM      75            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      75            PATLEN            3 0
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
B01  C     SSNAME        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  RXNAME        U1PDSC
     C     LO:HI         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSAWPD        IFNE      *ZEROS
     C     S1AWPD        ANDNE     RXAWPD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     RXSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
NDC code       Description                   Form Strength   Eff. date Sts
Description                       NDC code   Form Strength   Eff. date Sts
ONDC code      Description                   Form Strength   Eff. date Sts
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
