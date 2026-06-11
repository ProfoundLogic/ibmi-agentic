     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: 168        PROJECT: 93WFP  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/27/12 C:279 SUPPORT: WA099 79746                          *
     H*                 MODIFIED TO CORRECT SUBSET FUNCTIONALITY.     *
     H*                                                               *
     H*  04/17/12 C:136 SUPPORT: WA099 79746                          *
     H*                 FIX SORTING                                   *
     H*                                                               *
     H*  01/13/05 C:168  PROJECT: 93WFP                               *
     H*                  Initial creation, Maintain the Agent Contact *
     H*                  type                                         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW2287F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTAYP     IF   E             DISK
     F                                     RENAME(PAY01:PFRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWTAYL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAY01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWTAYL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAY01:DBRCDF2)
      /COPY SCOPYWW01D
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D***PY SCOPYWW01E
     D***PY SCOPYWW01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Agent Contact Type')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2287R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@KEY
     C                   PARM                    @@VIEW            1 0
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDESC
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    AYTYPE        KKKEY
     C     *LIKE         DEFINE    AYTYPE        @@KEY
     C******** Beg Chg ****** 04/27/12 ********************************
     C********   *LIKE     DEFN AYTYPE    U1PDSC
     C     *LIKE         DEFINE    AYDESC        U1PDSC
     C******** End Chg ****** 04/27/12 ********************************
     C     *LIKE         DEFINE    AYDESC        U2PDSC
     C     *LIKE         DEFINE    AYDESC        KKDESC
     C/COPY SCOPYWW01C
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSDESC        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C   21              MOVE      *LOVAL        KKKEY
     C   22              MOVE      *LOVAL        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKKEY
     C   22              MOVEL     SCPST2        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C   21              MOVEL     SCTYPE        KKKEY
     C   22              MOVEL     SCDESC        KKDESC
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     AYSTAT        SCSTAT
     C                   MOVEL(P)  AYTYPE        SCTYPE
     C                   MOVEL(P)  AYDESC        SCDESC
     C                   MOVEL(P)  *BLANKS       SCDATA
     C   21SCDATA        CAT       AYTYPE:5      SCDATA
     C   21              MOVEL(P)  AYDESC        ##DESC           35
     C   21              MOVE      ##DESC        SCDATA
     C   22              MOVEL(P)  AYDESC        ##DESC           35
     C   22              MOVEL     ##DESC        SCDATA
     C   22              MOVEL(P)  AYTYPE        ##TYP1           12
     C   22              MOVE      ##TYP1        SCDATA
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 (CHG STATUS)
     C*================================================================
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     Q1PRMS        IFGT      4
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>4
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
     C                   MOVEL     SCTYPE        @@KEY
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C     STRING        BEGSR
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
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C                   ENDSR                                                  STRING
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
B01  C     SSDESC        IFNE      *BLANKS
     C******** Beg Chg ****** 04/27/12 ********************************
     C********   LO:HI     XLATEAYTYPE    U1PDSC    P
     C     LO:HI         XLATE(P)  AYDESC        U1PDSC
     C******** End Chg ****** 04/27/12 ********************************
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     AYSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSR
** Column headings
 Contact Type   Description                             Status
 Description                      Contact Type          Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
