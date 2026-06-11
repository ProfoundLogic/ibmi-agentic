     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H*  W/W Auto Cash Dissolve A/R Categories                       *
     H****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W0010R61                                        *
     H*                                                               *
     H*  CALLS TO:    W0010R63                                        *
     H*                                                               *
     H*  CREATED: 03/30/05  PROGRAMMER: C:136 PROJECT: WC1179         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/03/09 C:136 PROJECT: WC1663                                *
     H*                CHANGE VERBIAGE IN COLUMN HEADER               *
    F*                                                               *
     F* 01/07/09  C:33  Project: WC1769    Priss# 10                  *
    F*                 Ensure all categories are loaded, including   *
     H*                  newly entered.                               *
     H*                                                               *
     H* 03/16/06 C:168 Support: WA139 51238                           *
     H*                Change to send the @@mode var to pgm W0010R63  *
     H*                                                               *
     H* 01/20/06 C:136 Support: WA129 50989                           *
     H*                Load an empty hierarchy if one doesn't exist   *
     H*                so that a user can set up a new group.         *
     H*****************************************************************
     FW0010F62  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDCDP     UF   E             DISK
     F                                     RENAME(PCD01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDCDL     UF   E           K DISK
     F                                     RENAME(PCD01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*  CO/FND/CTYP
     FWDCDL1    IF   E           K DISK
     F*  Group Master file
     FWMFNL     IF   E           K DISK
     F*  Category Table File
     FWTCTL     IF   E           K DISK
     F*    A/R Code Table
     FWTABL     IF   E           K DISK
      /COPY SCOPYWW02D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D ZER             S              1    DIM(3)                               Column headings
     D UND             S              1    DIM(99)                              UNDERLINE
     D***PY SCOPYWW02E
     D***PY SCOPYWW02I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Auto Cash Dissolve')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D DELETD          C                   CONST('*Deleted')
     D XXIFEE          C                   CONST('INSTALLMENT FEES')
     D XXDTAX          C                   CONST('DEPOSIT TAX')
      *** BEG ADD ***** 01/20/06 **************************************
     D LOADUP          C                   CONST('FIXWC1179B')
      *** END ADD ***** 01/20/06 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@FND             3 0
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
     C*  File for changing values for display
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   PARM                    EDTDEC            3 0
     C                   PARM                    EDTHOU            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     READKY        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C     PAGEKY        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKHIER
     C*
     C*  A/R Category Table Key
     C*
     C     CTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    CDCTYP
     C*
     C*  A/R Category Table Key
     C*
     C     TTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY            10
     C*
     C*  Group Key
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C*  Cash Dissolve Key
     C*
     C     CDKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCTYP
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CDFND         KKFND
     C     *LIKE         DEFINE    CDHIER        KKHIER
     C     *LIKE         DEFINE    CDCTYP        KKCTYP
     C     *LIKE         DEFINE    CDHIER        SVHIER
     C     *LIKE         DEFINE    CDNBCD        S2NBCD
     C     *LIKE         DEFINE    CDCPCD        S2CPCD
     C     *LIKE         DEFINE    CDCTYP        S2CTYP
     C*
     C/COPY SCOPYWW02C
     C*================================================================
     C* ERRCKA - Error check all subfile records (entire screen)
     C*================================================================
     C     ERRCKA        BEGSR
      *
     C     XXSQTT        IFNE      XXCNTT
     C                   SETON                                        72  90
     C                   MOVE      *OFF          XUPDT#            1
     C                   ENDIF
      *
     C                   Z-ADD     0             XXCNT             3 0
     C                   Z-ADD     0             XXCNTT            5 0
     C                   Z-ADD     0             XXSQTT            5 0
      *
     C                   ENDSR
     C*================================================================
     C* ERRCKC - Error check control record
     C*================================================================
     C     ERRCKC        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      *
      *   NEW BUSINESS PAYMENT TRANSFER CODE MUST EQUAL CATEGORY AND BE VALID
      *   IF A HIERARCHY VALUE IS ASSIGNED, EXCLUDING 'P' CATEGORY
      *
     C                   Z-ADD     0             XXSEQ#            3 0
     C     SCSEQ#        IFNE      *BLANK
     C                   MOVE      SCSEQ#        XXTMP1            3
     C     ' '           CHECKR    XXTMP1:3      Z                 2 0
     C     3             SUB       Z             Z1                2 0
     C                   MOVEL(P)  *BLANKS       SCSEQ#
     C                   CAT(P)    XXTMP1:Z1     SCSEQ#
     C                   MOVE      SCSEQ#        XXSEQ#
     C                   ENDIF
      *
     C     XXSEQ#        IFNE      *ZERO
     C                   ADD       1             XXCNT
     C                   ADD       XXCNT         XXCNTT
     C                   ADD       XXSEQ#        XXSQTT
     C                   ENDIF
      *
     C                   MOVE      SCHIER        XXHIER            3 0
     C     XXSEQ#        IFNE      XXHIER
     C     SCSEQ#        ANDNE     *BLANK
     C                   MOVE      *ON           XUPDT#
     C                   ENDIF
      *
     C     SVNBCD        IFNE      SCNBCD
     C     SVCPCD        ORNE      SCCPCD
     C                   MOVE      *ON           XUPDTC            1
     C                   ENDIF
      *
     C     *IN90         IFNE      *ON
     C     SCCTYP        ANDNE     'P'
      *
     C     XXSEQ#        IFNE      *ZERO
     C     SCHIER        ORNE      *ZERO
     C     SCHIER        ANDLT     10000
      *
     C                   MOVEL     SCNBCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTSTAT        ORNE      'A'
     C     TTCTYP        ORNE      SCCTYP
     C                   SETON                                        73  90
     C                   ENDIF
      *
     C     TTTYPE        IFNE      'P'
     C     TTTYPE        ANDNE     'Q'
     C     TTTYPE        ANDNE     'I'
     C                   SETON                                        73  90
     C                   ENDIF
      *
      *   CASH PAYMENT CODE MUST BE VALID AND EQUAL THE CATEGORY
      *   IF A HIERARCHY VALUE IS ASSIGNED, EXCLUDING 'P' CATEGORY
      *   IF A CASH CODE IS ASSIGNED.
      *
     C     SCCPCD        IFNE      *BLANKS
     C                   MOVEL     SCCPCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTSTAT        ORNE      'A'
     C     TTCTYP        ORNE      SCCTYP
     C                   SETON                                        74  90
     C                   ENDIF
      *
     C     TTTYPE        IFNE      'P'
     C     TTTYPE        ANDNE     'Q'
     C     TTTYPE        ANDNE     'I'
     C                   SETON                                        74  90
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  NO A/R CODES ARE TO BE ATTACHED TO PREMIUM CATEGORY ON THIS SCREEN
      *
     C     *IN90         IFNE      *ON
     C     SCCTYP        ANDEQ     'P'
      *
     C     SCNBCD        IFNE      *BLANK
     C     SCCPCD        ORNE      *BLANK
     C                   SETON                                        75  90
     C                   ENDIF
      *
     C                   ENDIF
      *
      * MAKE UNDERLINES ON A/R CODES DISPLAY PROPERLY AFTER ERROR CHECK
      *
     C                   Z-ADD     SFRRN1        Y
     C     UND(Y)        IFEQ      'N'
     C                   MOVE      *ON           *IN61
     C                   ELSE
     C                   MOVE      *OFF          *IN61
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     @@FND         KKFND
     C                   MOVE      *LOVAL        KKCTYP
     C                   MOVE      *LOVAL        KKHIER
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCHIER        KKHIER
     C                   MOVE      SCCTYP        KKCTYP
     C                   ENDSR
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10=SEARCH
     C*===============================================================
     C     KJDO          BEGSR
      *
      *  Search for A/R code
      *
     C     HRRN          CHAIN     SCR01S                             60
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCNBCD'
     C                   MOVE      *BLANKS       @@KEY
     C                   CALL      'W2161R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@KEY            10
     C     @@KEY         IFGT      *BLANKS
     C                   MOVEL     @@KEY         SCNBCD
     C                   MOVE      *OFF          *IN07
     C                   MOVE      *ON           *IN22                          CURSOR-NXT FLD
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCPCD'
     C                   MOVE      *BLANKS       @@KEY
     C                   CALL      'W2161R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@KEY            10
     C     @@KEY         IFGT      *BLANKS
     C                   MOVEL     @@KEY         SCCPCD
     C                   ENDIF
     C                   ENDSL
     C*
     C                   Z-ADD     HRRN          SCRRN1
     C*
      *  CLEAR THE SEARCH MESSAGE
     C*
     C                   CALL      'SRRMVMSG'
     C                   WRITE     SCR02
     C                   MOVE      *ON           *IN08                          01/19/05
     C                   UPDATE    SCR01S
     C                   MOVE      *ON           *IN07
     C                   MOVE      *OFF          *IN08                          01/19/05
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   CLEAR                   SCSEQ#
     C                   MOVE      ' '           SCCHGR
     C                   MOVE      *ON           *IN08
     C                   Z-ADD     CDHIER        SCHIER
      *
      *  ZERO SUPPRESSION ON SCDISP
      *
     C                   MOVE      CDHIER        SCDISP
     C                   MOVEA     SCDISP        ZER
     C                   DO        3             X                 1 0
     C     ZER(X)        IFEQ      '0'
     C                   MOVE      *BLANK        ZER(X)
     C                   ENDIF
     C     ZER(X)        IFGT      '0'
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     ZER           SCDISP
      *
     C                   MOVEL     CDCTYP        SCCTYP
     C     CTKY          CHAIN     PCT01                              60
     C                   MOVEL     CTDESC        SCDESC
     C                   MOVEL     CDNBCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C     CDCTYP        IFEQ      'D'
     C     TTTYPE        ANDEQ     'Q'
     C                   MOVEL     XXDTAX        SCDESC
     C                   ENDIF
     C     XXIFCT        IFEQ      1
     C     CDCTYP        ANDEQ     'F'
     C                   MOVEL(P)  XXIFEE        SCDESC
     C                   Z-ADD     2             XXIFCT            1 0
     C                   ENDIF
     C                   MOVEL     CDNBCD        SCNBCD
     C                   MOVEL     CDCPCD        SCCPCD
     C                   MOVEL     CDNBCD        SVNBCD
     C                   MOVEL     CDCPCD        SVCPCD
      *
     C     SCCTYP        IFEQ      'P'
     C                   MOVE      'N'           UND(Y)
     C                   ADD       1             Y
     C                   MOVE      *ON           *IN61
     C                   ELSE
     C                   MOVE      'Y'           UND(Y)
     C                   ADD       1             Y
     C                   MOVE      *OFF          *IN61
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   MOVE      'P'           KKCTYP
     C     CDKY1         CHAIN(N)  PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDHIER        ANDLT     10000
     C                   CALL      'W0010R63'                           99
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
      *** BEG  CHG 03/16/06 *******************************************
     C                   PARM      @@MODE        ##MODE            1
      *** END  CHG 03/16/06 *******************************************
     C                   PARM      @@FND         ##FND             3 0
     C                   ENDIF
     C                   ENDIF
      *
      *  RESET UNUSED CDHIER SO THAT 99000 LIMIT IS NOT EXCEEDED
      *
     C                   MOVE      *ON           FIRST
     C                   MOVE      *LOVAL        KKHIER
     C     PAGEKY        SETLL     DBRCDF
     C     FNKY          READE     DBRCDF                                 60
     C     *IN60         DOWEQ     *OFF
     C     CDHIER        IFGE      10000
     C     FIRST         IFEQ      *ON
     C     CDHIER        SUB       10000         XXDIF             5 0
     C                   MOVE      *OFF          FIRST             1
     C                   ENDIF
     C     CDHIER        SUB       XXDIF         CDHIER
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C     FNKY          READE     DBRCDF                                 60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVE      FNFND         SC1FND
     C                   Z-ADD     1             Y                 2 0
     C                   Z-ADD     1             XXIFCT
      *** BEG ADD ***** 01/20/06 *************************************
     C***  Begin Del  ***  01/07/09  **************************************
     C********** FNKY      SETLLPCD01                    60
     C********** *IN60     IFEQ *OFF
     C***  End   Del  ***  01/07/09  **************************************
     C                   CALL      LOADUP
     C                   PARM                    ##CO#
     C                   PARM                    FNFND
     C***  Begin Del  ***  01/07/09  **************************************
     C**********           ENDIF
     C***  End   Del  ***  01/07/09  **************************************
      *** END ADD ***** 01/20/06 *************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C     READKY        READE     DBRCDF                                 95
     C** 95                MOVE *ON       *IN09
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C     READKY        READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C     PAGEKY        SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C     PAGEKY        SETLL     DBRCDF                             96
     C     *IN96         IFEQ      *ON
     C                   Z-ADD     *ZEROS        DBRRN
     C                   ENDIF                                                  *IN96=*ON
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
      *
     C     XUPDT#        IFEQ      *ON
     C     XUPDTC        OREQ      *ON
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     *IN60         IFEQ      *OFF
      *
     C     XUPDT#        IFEQ      *ON
     C*
     C     SCSEQ#        IFNE      *BLANK
     C     SCSEQ#        ANDNE     '  0'
     C                   CLEAR                   CDHIER
     C                   MOVE      SCSEQ#        CDHIER
     C                   ENDIF
      *
     C     SCSEQ#        IFEQ      '  0'
     C     SCSEQ#        OREQ      *BLANKS
     C                   MOVE      CDCTYP        S2CTYP
     C                   MOVE      CDNBCD        S2NBCD
     C                   MOVE      CDCPCD        S2CPCD
     C                   Z-ADD     *HIVAL        KKHIER
     C     PAGEKY        SETGT     DBRCDF
     C     READKY        READPE    DBRCDF                                 60
     C                   MOVE      '000'         CDHIER
     C                   ADD       1000          CDHIER
     C                   MOVE      S2CTYP        CDCTYP
     C                   MOVE      S2NBCD        CDNBCD
     C                   MOVE      S2CPCD        CDCPCD
     C                   ENDIF
     C*
     C                   ENDIF                                                  XUPDT#
     C*
     C     SCNBCD        IFNE      SVNBCD
     C                   MOVE      SCNBCD        CDNBCD
     C                   ENDIF
     C*
     C     SCCPCD        IFNE      SVCPCD
     C                   MOVE      SCCPCD        CDCPCD
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        CDUSER
     C                   MOVEL     Q1PGM         CDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDT
     C                   MOVEL     DSTIME        CDTIME
     C                   UPDATE    PFRCDF
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  XUPDT#
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
     C*================================================================
** Column headings


** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1055=Display                NNN
12020=Change Status         NNN
203F3=Exit                  YNN
205F5=Refresh               YNN
210F10=Search               YNN
212F12=Cancel               YNN
217F17=Subset               NNN
3
