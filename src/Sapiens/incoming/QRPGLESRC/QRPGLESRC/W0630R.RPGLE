     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0630R - WORK WITH PAYROLL CLASS CODES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                PAYROLL CLASS CODES BY CLASS CODE OR           *
     H*                DESCRIPTION.  EACH CLASS CODE CAN BE ACCESSED  *
     H*                FOR UPDATE, DISPLAY OR SELECTION DEPENDING UPON*
     H*                THE CALLING PROGRAM.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*    W0060R02 - EMPLOYER PAYROLL INFORMATION                    *
     H*  CALLS TO:                                                    *
     H*    W0630R02 - PAYROLL CLASS CODE INFORMATION (OPT 2/5, F6)    *
     H*                                                               *
     H*  CREATED:   /  /    PROGRAMMER: C:    PROJECT:                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/17/24 C:344 Project: WC2952                                *
     H*                Expand scRT field in display file.             *
     H*                                                               *
     H* 04/13/07 C:33  Project: WC1233  WA136/53121                   *
     H*                Allow for proper RRN to be used when using the *
     H*                 date passed in.                               *
     H*                                                               *
     H* 12/08/05 C:61  SUPPORT WA136 50235 WC1233                     *
     H*                Correct paging when subset by effective date.  *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109AU}                                        *
     H*                Add new opt 90=Expanded Description            *
     H*                                                               *
     H* 12/16/04 C:132 PROJECT WC1233                                 *
     H*                Add Parm for incoming date, display only one   *
     H*                Class Code if Search Call and date is passed   *
     H*                into program.                                  *
     H*                                                               *
     H* 10/08/03 C:61  PROJECT WC1160                                 *
     H*                Add subset by Class code.                      *
     H*                                                               *
     H* 08/29/03 C:61  SUPPORT WA099 38724                            *
     H*                Pass rating state with call to W0650R          *
     H*                WW Payroll Class Extensions.                   *
     H*                                                               *
     H*  08/15/01  C:90  PROJECT: WC1035                              *
     H*                  Expand rate field from 5.2 to 7.2            *
     H*                                                               *
     H*  03/05/00  C:118 PROJECT: WC906                               *
     H*                  ADD A NEW OPTION 3=COPY                      *
     H*                  MODIFY DISPLAY FOR 2ND LINE OF OPTIONS.      *
     H*                                                               *
     H*  11/16/99  C:90  PROJECT: WC945                               *
     H*                  ADDED STATE PARM TO CALL TO PAYROLL CLASS    *
     H*                  EXTENSION MAINTENANCE.                       *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  ADD OPTION 7 TO PULL UP PAYROLL CLASS        *
     H*                  DESCRIPTION EXTENSION.                       *
     H*                                                               *
     H*  01/28/98  C:33  PROJECT: WC577                               *
     H*                  ALLOW SCROLLING FOR CLASS CODES THAT HAVE THE*
     H*                  SAME DESCRIPTION.                            *
     H*****************************************************************
     FW0630F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMPRL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPR01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWMPRL5    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPR01:DBRCDF2)
      *
      * Rate Set Code
     FWMRSL     IF   E           K DISK
      *
      *
     FWMPRL1    IF   E           K DISK
     F                                     INFDS(PRFDS1)
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             75    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(20) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(20) ALT(OF)
     D*
     D*
     D                 DS
     D  XXDATE                 1     10
     D  XXDAY                  1      2
     D  XXHYP1                 3      3
     D  XXMTH                  4      5
     D  XXHYP2                 6      6
     D  XXYEAR                 7     10
     D                 DS
     D  XXEFDT                 1      8  0
     D  X1DAY                  1      2  0
     D  X1MTH                  3      4  0
     D  X1YEAR                 5      8  0
     D*
     D PRFDS1          DS
     D  PRRRN1               397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Payroll Class Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0630R02')
     D EXTPGM          C                   CONST('W0630R90')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
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
     C                   PARM                    @@RTCD            3
     C                   PARM                    @@VIEW            1 0
     C                   PARM                    @@DATE            8
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
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
      * Rate Set Code
      *
     C     RSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    PRRTCD
      *
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKRTCD
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKRTCD
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKEFDT
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKRTCD
     C                   KFLD                    KKDESC
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKEFDT
     C*
     C     KEY5          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKRTCD
     C                   KFLD                    KKCLAS
      *
      * Payroll Class Code
      *
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1RTCD
     C                   KFLD                    K1CLAS
     C                   KFLD                    K1EFDT
      *
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1RTCD
     C                   KFLD                    K1CLAS
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PRRTCD        K1RTCD
     C     *LIKE         DEFINE    PRCLAS        K1CLAS
     C     *LIKE         DEFINE    PREFDT        K1EFDT
     C     *LIKE         DEFINE    PRCLAS        KKCLAS
     C     *LIKE         DEFINE    PRDESC        U1PDSC
     C     *LIKE         DEFINE    PRDESC        U2PDSC
     C     *LIKE         DEFINE    PRDESC        KKDESC
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    PRRTCD        KKRTCD
     C     *LIKE         DEFINE    PREFDT        SSDATE
     C     *LIKE         DEFINE    PRCLAS        SVCLAS
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSDESC        ORNE      *BLANKS
B01  C     SSRTCD        ORNE      *BLANKS
     C     SSCLAS        ORNE      *ZERO
B01  C     SSEFDT        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM MDYY TO CYMD
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
      *
      *
      *
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSEFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSDATE
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        7190
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ELSE
     C*
     C                   CLEAR                   SSDATE
     C*
     C                   ENDIF                                                  SSEFDT<>*ZEROS
     C*
     C  N90SSSTAT        IFNE      ' '
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
     C                   CLEAR                   SSRTCD
     C                   CLEAR                   SSCLAS
     C                   CLEAR                   SSDESC
     C                   CLEAR                   SSEFDT
     C                   CLEAR                   SSDATE
     C                   CLEAR                   SSSTAT
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C   33              MOVEL     @@RTCD        SSRTCD
     C*
     C                   ENDSR
     C*================================================================
     C* KQDO - Subroutine for F16
     C*================================================================
     C     KQDO          BEGSR
     C*
     C                   MOVEL     'R'           ##MODE
     C                   CLEAR                   ##RRN
     C*
     C                   CALL      ACDPGM        ACDPL1
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      @@RTCD        KKRTCD
     C                   MOVE      *LOVAL        KKCLAS
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C  N33              MOVEL     SCPST1        KKRTCD
     C*
     C   21              MOVEL     SCPST2        KKCLAS
     C*
     C*  21                MOVE SCPSTD    XXEFDT
     C*
     C   21              MOVE      SCPSTD        SRDTI
     C   21              EXSR      CVTDT
     C   21              MOVEL     SRDTO         KKEFDT
     C*
     C   22              MOVEL     SCPST3        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCRTCD        KKRTCD
     C*
     C                   MOVEL     SCCLAS        KKCLAS
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   MOVEL     SCDESC        KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     PRSTAT        SCSTAT
     C                   MOVE      PRCLAS        SCCLAS
     C                   MOVE      PREFDT        SCEFDT
     C                   MOVE      PRDESC        SCDESC
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      X1DAY         XXDAY
     C                   MOVEL     '/'           XXHYP1
     C                   MOVE      X1MTH         XXMTH
     C                   MOVEL     '/'           XXHYP2
     C                   MOVE      X1YEAR        XXYEAR
     C*
     C                   MOVEL     PRCLAS        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCLAS            5
     C*
     C                   MOVEL(P)  XXCLAS        XXDATA           17
     C                   MOVE      XXDATE        XXDATA
     C*
     C   21              MOVEL(P)  XXDATA        SCDATA
     C   21              MOVE      PRDESC        SCDATA
     C*
     C   22              MOVEL(P)  PRDESC        SCDATA
     C   22              MOVE      XXDATA        SCDATA
     C*
     C                   MOVEL     PRRT          SCRT
     C                   MOVEL     PRPCF         SCPCF
     C                   MOVEL     PRLHCD        SCLHCD
     C                   MOVEL     PRRTCD        SCRTCD
     C*
     C                   ENDSR
     C*================================================================
     C* OPT3 - SUBROUTINE FOR OPTION 3
     C*================================================================
     C* OPTION 3=COPY
     C*
     C     OPT03         BEGSR
     C*
     C                   MOVEL     'P'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT07 - SUBROUTINE FOR OPTION 7
     C*================================================================
     C*
     C* OPTION 07=EXTENSIONS
     C*
     C     OPT07         BEGSR
     C*
      *
      *  Get rating state
      *
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSJRST        #1ST
     C   60              MOVEL(P)  *BLANKS       #1ST
      *
     C                   CALL      'W0650R'                             99
     C     *INKC         PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      @@CALL        #1CALL            1
     C                   PARM      *ZERO         #1RRN             9 0
     C                   PARM      ##CO#         #1CO#             3 0
     C                   PARM      SCCLAS        #1CLAS            5 0
     C                   PARM                    #1ST              2
      *
     C                   ENDSR
      *
      *
     C*================================================================
     C* OPT16 - SUBROUTINE FOR OPTION 16
     C*================================================================
     C*
     C* OPTION 16=NEW RATE
     C*
     C     OPT16         BEGSR
     C*
     C                   MOVEL     'R'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
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
     C*====================================================================
     C* OPT90 - Subroutine for Option 90
     C*====================================================================
     C*
     C* Option 90=Expanded Description
     C*
     C     OPT90         BEGSR
     C*
     C                   MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      EXTPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKQ         CASEQ     *ON           KQDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT07
     C     SCSEL         CASEQ     '16'          OPT16
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '90'          OPT90
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
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     Q1PRMS        IFGT      5
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>6
      *
     C                   MOVE      *OFF          *IN31
     C     Q1PRMS        IFGT      6
     C     @@DATE        ANDGT     *ZEROS
     C                   MOVE      @@DATE        ZZDATE            8 0
     C*
     C                   Z-ADD     ZZDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##DATE            7 0
     C*
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   Z-ADD     *ZEROS        ##DATE
     C                   ENDIF                                                  Q1PRMS>6
     C*
     C     @@RTCD        IFNE      *BLANKS
     C*
     C                   MOVE      @@RTCD        KKRTCD
     C                   MOVE      @@RTCD        SCPRCD
     C                   SETON                                        33
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C     *IN33         IFEQ      *OFF
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ELSE
     C*
     C   21KEY2          READE     DBRCDF                                 95
     C   22KEY2          READE     DBRCDF2                                95
      *
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C     *IN33         IFEQ      *OFF
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ELSE
     C*
     C   21KEY2          READPE    DBRCDF                                 95
     C   22KEY2          READPE    DBRCDF2                                95
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY3          SETGT     DBRCDF                             96
     C   22KEY4          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY3          SETLL     DBRCDF                             96
     C   22KEY4          SETLL     DBRCDF2                            96
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
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  PRDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     PRSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSRTCD        IFNE      *BLANKS
     C     SSRTCD        ANDNE     PRRTCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCLAS        IFNE      *ZERO
     C     SSCLAS        ANDNE     PRCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     SSDATE        ANDNE     PREFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEFDT<>*ZEROS
     C*
      *
     C     *IN31         IFEQ      *ON
     C     MEETSS        ANDEQ     *ON
      *
     C     PRCLAS        IFEQ      SVCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     MEETSS        IFEQ      *ON
     C                   MOVEL     PRRTCD        K1RTCD
     C                   MOVEL     PRCLAS        K1CLAS
     C                   Z-ADD     ##DATE        K1EFDT
     C     PRKY          SETGT     PPR01                              60
     C  N60PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   Z-ADD     PRRRN1        DBRRN
     C     *IN22         WHENEQ    *ON
     C                   Z-ADD     PRRRN1        DBRRN2
     C                   ENDSL
     C                   ENDIF
     C   60              MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   MOVEL     PRCLAS        SVCLAS
     C                   ENDIF
      *
     C*
     C                   ENDSR
** Column headings
Rtcd Class  Eff. date  Description                         Rate Cap L&H Sts
Rtcd Description                    Class  Eff. date       Rate Cap L&H Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
1077=Extensions             YYN
11616=New rate              YNN
12020=Change status         YNN
19090=Exp Desc              YYN                            06/24/05
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
216F16=New rates            YNN
217F17=Subset               YYY
3
                                                           Future Use
                                                           Future Use
                                                           Future Use
                                                           Future Use
