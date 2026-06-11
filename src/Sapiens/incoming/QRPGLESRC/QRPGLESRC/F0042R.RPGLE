     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0042R - WORK WITH PROVIDER STATE INFORMATION C:62 03/26/98  *
     H*                                                               *
     H*  CALLED FROM: F0032R01                                        *
     H*                                                               *
     H*  CALLS TO   : N0037R                                          *
     H*               F0042R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/24/04 C:079 PROJECT:MA012                                 *
     H*                Z-ADD Fee Sched Value to screen when state=MA  *
     H*                                                               *
     H*  01/08/02 C:79 Project: WV004                                 *
     H*                1. Correct paging issue                        *
     H*                2. Add SCAM03                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0042F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMP2P     IF   E             DISK    USROPN
     F                                     RENAME(PP201:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMP2L1    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS)
     F                                     RENAME(PP201:DBRCDF)
     FFMPDP     IF   E             DISK
     FFMP1L     IF   E           K DISK
     FFMSIL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HDS             S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D CD              S              1    DIM(13)                              CODE
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  ##CMD1                 1     56
     D  ##ST1                 38     39
     D                 DS
     D  ##CMD2                 1     56
     D  ##ST2                 40     41
     D                 DS
     D  P2SV01                 1     11
     D  SCSVC2                 1      3
     D  SCSVC1                 4      8
     D  SCSVC3                 9     11
     D*** BEG ADD *** 01/08/02 ****************************************
     D                 DS
     D  XXAM01                 1     10
     D  XXAMT1                 1      7
     D  XXDOT                  8      8
     D  XXDEC                  9     10
     D                 DS
     D  ZZAM01                 1     10
     D  ZZAMT1                 1      3
     D  ZZDOT                  4      4
     D  ZZDEC                  5     10
     D*** END ADD *** 01/08/02 ****************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider State Inf-
     D                                     ormation')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0042R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D DL              C                   CONST('** DELETED **')
     D PAY             C                   CONST('Pay')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FMP2P
     C                   PARM                    @@RRN2            9 0          FMPDP
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          FMP2P
     C                   PARM      @@RRN2        ##RRN2            9 0          FMPDP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            3 0
     C                   PARM                    ##FYR             3 0
     C*
     C*  Convert CYMD to MDYY.
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Search parameter list.
     C*
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   KFLD                    KKSV01
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C     SIKY          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKPRTY
     C                   KFLD                    KKSPCD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    P2FEIN        KKFEIN
     C     *LIKE         DEFINE    P2MOD         KKMOD
     C     *LIKE         DEFINE    P2EFDT        KKEFDT
     C     *LIKE         DEFINE    P2EFDT        XXEFDT
     C     *LIKE         DEFINE    SIST          KKST
     C     *LIKE         DEFINE    SIPRTY        KKPRTY
     C     *LIKE         DEFINE    SISPCD        KKSPCD
     C*** BEG ADD *** 01/08/02 ****************************************
     C     *LIKE         DEFINE    P2SV01        KKSV01
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   MOVEL     *OFF          STOP              1
     C*
     C                   SELECT
     C*
     C     @@CALL        WHENEQ    'L'
     C                   MOVEL     '1'           @@CALL
     C                   MOVEL     *ON           STOP
     C*
     C     @@CALL        WHENEQ    'M'
     C                   MOVEL     ' '           @@CALL
     C                   MOVEL     *ON           STOP
     C*
     C                   ENDSL
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
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
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C   17              MOVEL(P)  HDS(1)        SCHDL2
     C  N17              MOVEL(P)  HDS(2)        SCHDL2
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKEFDT
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   MOVE      *BLANKS       KKSV01
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH SUBSET SCREEN.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        XXEFDT
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCEFDT        KKEFDT
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   MOVE      SCSV01        KKSV01
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     P2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD2
     C*
     C                   MOVEA     *BLANKS       CD
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEA     SCSVC1        CD(5)
     C*
     C                   ELSE
     C*
     C*** BEG CHG *** 01/08/02 ****************************************
     C******************** MOVEASCSVC1    CD,1
     C******************** MOVEASCSVC2    CD,7
     C******************** MOVEASCSVC3    CD,11
     C                   MOVEA     SCSVC1        CD(5)
     C                   MOVEA     SCSVC2        CD(1)
     C                   MOVEA     SCSVC3        CD(11)
     C*** END CHG *** 01/08/02 ****************************************
     C*
     C                   ENDIF
     C*
     C                   MOVEA     CD            SCCODE
     C*
     C                   MOVE      P2EFDT        SCEFDT
     C*** BEG DEL *** 01/08/02 ****************************************
     C******************** MOVE P2AM01    SCAM01
     C*** END DEL *** 01/08/02 ****************************************
     C*** BEG ADD *** 01/08/02 ****************************************
     C*** BEG ADD *** 07/24/04 ****************************************
     C     SCST          IFEQ      'MA'
     C     P2AM01        MULT      10000         P2AM01
     C                   ENDIF
     C*** END ADD *** 07/24/04 ****************************************
     C                   MOVE      P2AM01        HDAM01            9
     C                   MOVEL     HDAM01        XXAMT1
     C                   MOVE      HDAM01        XXDEC
     C                   MOVEL     HDAM01        ZZAMT1
     C                   MOVE      HDAM01        ZZDEC
     C*
     C     *IN34         IFEQ      *ON
     C                   MOVE      XXAM01        SCAM01
     C                   ELSE
     C                   MOVE      ZZAM01        SCAM01
     C                   ENDIF
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   MOVEL     P2FL01        SCFL01
     C                   MOVE      P2AM02        SCAM02
     C   17              MOVEL     P2FL02        SCFL02
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   MOVE(P)   P2SV01        SCSV01
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPNOVR - Subroutine that opens and overrides FMP2P/FMP2L1
     C*================================================================
     C     OPNOVR        BEGSR
     C*
     C* Checks to see if files are open, if they are then close them. .
     C*
     C     OPNFL1        IFEQ      '1'
     C*
     C                   CLOSE     FMP2P
     C                   CLOSE     FMP2L1
     C                   MOVEL     ' '           OPNFL1
     C*
     C                   ENDIF
     C*
     C* Checks to see if the files have overrides, if so it deletes it.
     C*
     C     OVRFL1        IFEQ      '1'
     C*
     C* FMP2P
     C*
     C                   Z-ADD     56            LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   MOVEL     ' '           OVRFL1
     C*
     C* FMP2L1
     C*
     C                   Z-ADD     56            LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C                   MOVEL     ' '           OVRFL1
     C*
     C                   ENDIF
     C*
     C* If files have no overrides, then the overrides are established.
     C*
     C     OVRFL1        IFEQ      ' '
     C*
     C* FMP2P
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        ##CMD1
     C                   MOVEL     SCST          ##ST1
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    ##CMD1
     C                   PARM                    LENGTH
     C*
     C*
     C* FMP2L1
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(4)        ##CMD2
     C                   MOVEL     SCST          ##ST2
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    ##CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     '1'           OVRFL1
     C*
     C                   ENDIF
     C*
     C* If the files are closed then they are openned.
     C*
     C     OPNFL1        IFEQ      ' '
     C*
     C                   OPEN      FMP2P
     C                   OPEN      FMP2L1
     C                   MOVEL     '1'           OPNFL1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C* CLOSES AND DELETES OVERRIDES OF FMP2P AND FMP2L1.
     C*
     C                   EXSR      OPNOVR
     C*
     C* CHECKS TO SEE IF CONDITIONS ARE MET FOR CALLING N0037R.
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     PDPARN        ANDEQ     'P'
     C     STOP          ANDEQ     *OFF
     C*
     C                   MOVE      PDFEIN        ##FEIN
     C                   MOVE      PDMOD         ##MOD
     C*
     C                   CALL      'N0037R'
     C                   PARM      @@CALL        ##CALL            1                        t
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      *BLANKS       ##NWCD            5
     C                   PARM      *BLANKS       ##CNT#           15
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C* OPENS AND SETS OVERRIDES OF FMP2P AND FMP2L1.
     C*
     C                   EXSR      OPNOVR
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   MOVE      '.'           XXDOT
     C                   MOVE      '.'           ZZDOT
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   Z-ADD     8             SF1PAG            5 0
     C                   MOVEL     ' '           OPNFL1            1
     C                   MOVEL     ' '           OVRFL1            1
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     @@RRN2        CHAIN     PPD01                              99
     C*
     C                   MOVE      PDFEIN        KKFEIN
     C                   MOVE      PDMOD         KKMOD
     C*
     C                   MOVE      PDFEIN        SCFEIN
     C                   MOVE      PDMOD         SCMOD
     C                   MOVEL     PDNAME        SCPNAM
     C                   MOVEL     PDTYPE        SCPTYP
     C                   MOVEL     PDTYPE        KKPRTY
     C                   MOVEL     PDSPCD        SCSPCD
     C                   MOVEL     PDSPCD        KKSPCD
     C*
     C     KEY1          CHAIN     PP101                              99
     C*
     C                   MOVEL     P1ST          SCST
     C                   MOVEL     P1ST          KKST
     C*
     C                   SELECT
     C*
     C     SCPTYP        WHENEQ    '1'
     C*
     C                   SETON                                        16
     C*
     C     SCPTYP        WHENEQ    '4'
     C*
     C                   SETOFF                                       16
     C*** BEG ADD *** 08/05/99 ****************************************
     C                   MOVE      *BLANKS       KKSPCD
     C*** END ADD *** 08/05/99 ****************************************
     C*
     C                   ENDSL
     C*
     C     SIKY          CHAIN     PSI01                              99
     C*
     C   99              SETOFF                                       17
     C*
     C  N99SIFL02        IFEQ      'Y'
     C*
     C                   MOVEL     PAY           SCPAY
     C                   SETON                                        17
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       17
     C*
     C                   ENDIF
     C*
     C     *IN17         IFEQ      *OFF
     C*
     C                   MOVEL     *BLANKS       SCPAY
     C*
     C                   ENDIF
     C*
     C* OPEN AND SET OVERRIDES OF FMP2P AND FMP2L1.
     C*
     C                   EXSR      OPNOVR
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C     P1ST          IFEQ      'WV'
     C                   SETON                                        34
     C                   ELSE
     C                   SETOFF                                       34
     C                   ENDIF
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C     KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C     KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C     KEY2          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C     KEY2          SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DL            SCCODE
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
** Column headings
   Date         Code          value      Type   Discount    Lesser
   Date         Code          value      Type   Discount
** Column headings

** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
** QCMDEXC TABLE
DLTOVR FILE(FMP2P)
OVRDBF FILE(FMP2P) TOFILE(*LIBL/FMP2P  )
DLTOVR FILE(FMP2L1)
OVRDBF FILE(FMP2L1) TOFILE(*LIBL/FMP2L1  )
