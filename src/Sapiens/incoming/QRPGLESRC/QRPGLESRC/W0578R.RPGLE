     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
     H*****************************************************************
     H/TITLE W0578R - Work with Address History
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY THE ADDRESS HISTORY  *
     H*                W/W SCREEN.                                    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0578R02                                                   *
     H*                                                               *
     H*  CREATED: 06/29/07  PROGRAMMER: C:183 PROJECT: 114WOC         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*****************************************************************
     H*
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     FW0578F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDAHP     IF   E             DISK
     F                                     RENAME(PAH01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDAHL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAH01:DBRCDF)
     F*  Secondary logical file for view #2
     FWDAHL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAH01:DBRCDF2)
     F*  Secondary logical file for view #3
     FWDAHL2    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PAH01:DBRCDF3)
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     FWMCMP     IF   E             DISK
     D*
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D DATA            S              1    DIM(70)                                            ion
      *
     D/COPY SPRSTRING
     D/COPY SPRDATE
     D/COPY SPRCOMPANY
     D/COPY SPRMESSAGE
     D/COPY SPRRUNCMD
     D/COPY SPRWRKWTH
     D/COPY WPRUSRPRMS
     D/COPY WPRAGENT
     D/COPY WPRAR
     D/COPY WPRCHAPTER
     D/COPY WPROFFICE
     D/COPY WPRREGION
     D/COPY WPRPRODUCE
      *
     D                 DS
     D  SCDATA                       70
     D  @NAME1                 1     48
     D  @DT1                  50     59
     D  @TIME1                61     68
     D*** V1 I2
     D  @FND1                 50     52
     D  @FYR1                 54     56
     D  @CASE1                58     64
     D*** V2 I1
     D  @DT2                   1     10
     D  @TIME2                12     19
     D  @NAME2                21     68
     D*** V2 I2
     D  @FND2                 21     23
     D  @FYR2                 25     27
     D  @CASE2                29     35
     D*** V3 I1
     D  @USER3                 1     10
     D  @DT3                  12     21
     D  @NAME3                23     70
     D*** V3 I2
     D  @FND3                 23     25
     D  @FYR3                 27     29
     D  @CASE3                31     37
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Claimant/Add''''l Address')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0578R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(10)
     D*
     D*================================================================
     D*  Stand-alone variable declaration
     D*================================================================
     D @SCDT           S              8  0
     D @@SCDT          S               D   DATFMT(*USA)
     D @KKDT           S               D
     D @SCTIME         S              8
     D @SCUSER         S             10
     D KKCO#           S                   LIKE(AHCO#)
     D KKFND           S                   LIKE(AHFND)
     D KKFYR           S                   LIKE(AHFYR)
     D KKCASE          S                   LIKE(AHCASE)
     D KKDT            S                   LIKE(AHDT)
     D KKTIME          S                   LIKE(AHTIME)
     D KKUSER          S                   LIKE(AHUSER)
     D KKFSN           S                   LIKE(AHFSN)
     D KKLSN           S                   LIKE(AHLSN)
     D KKMID           S                   LIKE(AHMID)
     D KKBAFL          S                   LIKE(AHBAFL)
     D U1PDSC          S                   LIKE(AHLSN)
     D U2PDSC          S                   LIKE(AHLSN)
     D UP              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D LO              C                   'abcdefghijklmnopqrstuvwxyz'
     D*@NAME           S             48
      *
     C/COPY SCOPYWW41C
      *
     C     *LIKE         DEFINE    SCDATA        @SCDATA
     C     *LIKE         DEFINE    SCDATA        @SCDATA1
     C     *LIKE         DEFINE    SCDATA        @SCDATA2
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
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
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C* EMPLOYER SEARCH
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    XACO#             3
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C*
     C     ACDPL3        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      *BLANKS       ##SORT            2
     C                   PARM      *ZEROS        ##RRN2
     C*** NO PASS          PARM           ##FYR   30
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEYR          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C                   KFLD                    KKBAFL
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKBAFL
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKUSER
     C                   KFLD                    KKDT
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKBAFL
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FOR SUBSET
     C*
     C*    EMKY1         KLIST
     C*                  KFLD                    KKCO#
     C*                  KFLD                    SSFND
     C*                  KFLD                    SSEMP#
     C*                  KFLD                    KKDIV             5 0
     C*
     C*  Date format parameter lists
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY1       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *Blanks       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*    DTCYMD        PLIST
     C*                  PARM                    SRDTI
     C*                  PARM      *BLANKS       SRFFMT
     C*                  PARM      '*CYMD'       SRTFMT
     C*                  PARM                    SRDTO
     C*                  PARM                    SRERR
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*    *LIKE         DEFINE    AHCO#         KKCO#
     C*    *LIKE         DEFINE    AHFND         KKFND
     C*    *LIKE         DEFINE    AHFYR         KKFYR
     C*    *LIKE         DEFINE    AHCASE        KKCASE
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
B01  C     SSFND         IFNE      *ZEROS
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
     C     SSFND         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C     SSYEAR        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
      * PROCESS SEARCHES
      *
     C     *INKJ         IFEQ      *ON
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSFND'
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      *ZERO         XAFND
     C                   MOVE      *ZERO         XAFYR
     C                   MOVE      '2'           ##MODE
     C                   CALL      'W0010R'      ACDPL2                 99
     C                   MOVE      XAFND         SSFND
     C*
     C     HFLD          WHENEQ    'SSCASE'
     C                   CALL      'W0070R'      ACDPL3                 99
     C     ##RRN         CHAIN     WMCMP                              60
     C                   MOVE      CMCASE        SSCASE
     C*
     C                   OTHER
     C                   SETON                                            90
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*                  EVAL      KKCO# = *LOVAL
     C                   EVAL      KKDT = *HIVAL
     C                   EVAL      KKTIME = *HIVAL
     C                   EVAL      KKFND = *LOVAL
     C                   EVAL      KKFYR = *LOVAL
     C                   EVAL      KKCASE = *LOVAL
     C                   EVAL      KKLSN = *LOVAL
     C                   EVAL      KKFSN = *LOVAL
     C                   EVAL      KKMID = *LOVAL
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C                   EVAL      KKCO# = SCCO#
     C                   EVAL      KKLSN = *LOVAL
     C                   EVAL      KKFSN = *LOVAL
     C                   EVAL      KKMID = *LOVAL
     C                   EVAL      KKDT = *HIVAL
     C                   EVAL      KKTIME = *HIVAL
     C                   EVAL      KKUSER = *LOVAL
     C                   EVAL      KKBAFL = *LOVAL
      *
     C                   SELECT
     C                   WHEN      *IN21 = *ON
     C                   EVAL      KKLSN = SCPST1
     C                   EVAL      KKFSN = SCPS1A
     C                   EVAL      KKMID = SCPS1B
      *
     C                   WHEN      *IN22 = *ON
     C                   Z-ADD     0             @@KKDT            8 0
     C                   MOVE      SCPST2        @@KKDT
     C                   EVAL      SRDTI = @@KKDT
     C                   CALL      'SRCVTDAT'    DTMDYY1
     C                   IF        SRDTO = *BLANKS
     C                   MOVEL     *HIVAL        KKDT
     C                   ELSE
     C                   MOVEL     SRDTO         KKDT
     C                   ENDIF
      *
     C                   WHEN      *IN23 = *ON
     C                   MOVE      SCPST3        KKUSER
     C                   Z-ADD     0             @@KKDT            8 0
     C                   MOVE      SCPS3A        @@KKDT
     C                   EVAL      SRDTI = @@KKDT
     C                   CALL      'SRCVTDAT'    DTMDYY1
     C                   IF        SRDTO = *BLANKS
     C                   MOVEL     *HIVAL        KKDT
     C                   ELSE
     C                   MOVEL     SRDTO         KKDT
     C                   ENDIF
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   EVAL      KKCO# = SCCO#
     C                   EVAL      KKFND = SCFND                                MJF
     C                   EVAL      KKFYR = SCFYR
     C                   EVAL      KKCASE = SCCASE
     C                   EVAL      KKFSN = SCFSN
     C                   EVAL      KKLSN = SCLSN
     C                   EVAL      KKMID = SCMID
     C                   EVAL      KKUSER = SCUSER
     C                   EVAL      KKDT = SCDT
     C                   EVAL      KKTIME = SCTIME
     C                   EVAL      KKBAFL = SCBAFL
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   EVAL      SCCO# = AHCO#
     C                   EVAL      SCFND = AHFND
     C                   EVAL      SCFYR = AHFYR
     C                   EVAL      SCCASE = AHCASE
     C                   EVAL      SCDT = AHDT
     C                   EVAL      SCTIME = AHTIME
     C                   EVAL      SCUSER = AHUSER
     C                   EVAL      SCFSN = AHFSN
     C                   EVAL      SCLSN = AHLSN
     C                   EVAL      SCMID = AHMID
     C                   EVAL      SCBAFL= AHBAFL
      *
     C                   EVAL      @SCDT = DTMDYY(AHDT)
     C                   MOVE      @SCDT         @@SCDT
      *
     C                   EVAL      @SCTIME = %EDITW(AHTIME:'  :  :  ')
     C                   IF        %SUBST(@SCTIME:1:1) = ' '
     C                   EVAL      %SUBST(@SCTIME:1:1) = '0'
     C                   ENDIF
      *
     C                   EVAL      SCDATA = *BLANKS
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   IF        AHFSN = *BLANKS
     C                   EVAL      @NAME1 = %TRIM(AHLSN)
     C                   ELSE
     C                   EVAL      @NAME1 = %TRIM(AHLSN) + ', ' +%TRIM(AHFSN) +
     C                             ' ' + %TRIM(AHMID)
     C                   ENDIF
      *
     C                   IF        INFO# = 1
     C                   MOVE      @@SCDT        @DT1
     C                   EVAL      @TIME1 = @SCTIME
     C                   ELSEIF    INFO# = 2
     C                   EVAL      @FND1 = %EDITC(SCFND:'X')
     C                   EVAL      @FYR1 = %EDITC(SCFYR:'X')
     C                   EVAL      @CASE1 = %EDITC(SCCASE:'X')
     C                   ENDIF
      *
     C     VIEW#         WHENEQ    2
     C                   MOVE      @@SCDT        @DT2
     C                   EVAL      @TIME2 = @SCTIME
      *
     C                   IF        INFO# = 1
     C                   IF        AHFSN = *BLANKS
     C                   EVAL      @NAME2 = %TRIM(AHLSN)
     C                   ELSE
     C                   EVAL      @NAME2 = %TRIM(AHLSN) + ', ' +%TRIM(AHFSN) +
     C                             ' ' + %TRIM(AHMID)
     C                   ENDIF
      *
     C                   ELSEIF    INFO# = 2
     C                   EVAL      @FND2 = %EDITC(SCFND:'X')
     C                   EVAL      @FYR2 = %EDITC(SCFYR:'X')
     C                   EVAL      @CASE2= %EDITC(SCCASE:'X')
     C                   ENDIF
      *
     C     VIEW#         WHENEQ    3
     C                   EVAL      @USER3 = SCUSER
     C                   MOVE      @@SCDT        @DT3
      *
     C                   IF        INFO# = 1
     C                   IF        AHFSN = *BLANKS
     C                   EVAL      @NAME3 = %TRIM(AHLSN)
     C                   ELSE
     C                   EVAL      @NAME3 = %TRIM(AHLSN) + ', ' +%TRIM(AHFSN) +
     C                             ' ' + %TRIM(AHMID)
     C                   ENDIF
     C                   ELSEIF    INFO# = 2
     C                   EVAL      @FND3 = %EDITC(SCFND:'X')
     C                   EVAL      @FYR3 = %EDITC(SCFYR:'X')
     C                   EVAL      @CASE3= %EDITC(SCCASE:'X')
     C                   ENDIF
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
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
     C                   EVAL      *INLR = *ON
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C                   EVAL      *IN95 = *OFF
     C                   DOW       *IN95 = *OFF
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C                   IF        AHBAFL = 'B'
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C                   EVAL      *IN95 = *OFF
     C                   DOW       *IN95 = *OFF
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
     C                   IF        AHBAFL = 'B'
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
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
B01  C*                  SELECT
     C*
     C     SSFSN         IFNE      *BLANKS
     C                   EVAL      U1PDSC = %XLATE(LO:UP:AHFSN)
     C                   EVAL      U2PDSC = %XLATE(LO:UP:SSFSN)
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C     SSLSN         IFNE      *BLANKS
     C                   EVAL      U1PDSC = %XLATE(LO:UP:AHLSN)
     C                   EVAL      U2PDSC = %XLATE(LO:UP:SSLSN)
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C     SSMID         IFNE      *BLANKS
     C                   EVAL      U1PDSC = %XLATE(LO:UP:AHMID)
     C                   EVAL      U2PDSC = %XLATE(LO:UP:SSMID)
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C     SSDT          IFNE      0
     C                   MOVE      SSDT          @@@KKDT           8 0
     C                   EVAL      SRDTI = @@@KKDT
     C                   CALL      'SRCVTDAT'    DTMDYY1
     C                   IF        SRDTO = *BLANKS
     C                   MOVEL     *HIVAL        @SSDT             7 0
     C                   ELSE
     C                   MOVEL     SRDTO         @SSDT             7 0
     C                   ENDIF
     C     AHDT          IFNE      @SSDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSTIME        IFNE      0
     C     AHTIME        ANDNE     SSTIME
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSUSER        IFNE      *BLANKS
     C                   EVAL      U1PDSC = %XLATE(LO:UP:AHFSN)
     C                   EVAL      U2PDSC = %XLATE(LO:UP:SSFSN)
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C     SSFND         IFNE      0
     C     AHFND         ANDNE     SSFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSYEAR        IFNE      0
     C     AHFYR         ANDNE     SSYEAR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCASE        IFNE      0
     C     AHCASE        ANDNE     SSCASE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
E01  C*                  ENDSL
     C                   ENDSR
** Column headings
Opt  Name                                             Date       Time
Opt  Date       Time     Name
Opt  User       Date       Name
Opt  Name                                             Fnd Fyr Case
Opt  Date       Time     Fnd Fyr Case
Opt  User       Date       Fnd Fyr Case
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change info           YYN
211F11=Change view          YYN
212F12=Cancel               NNN
217F17=Subset               YYY
3
