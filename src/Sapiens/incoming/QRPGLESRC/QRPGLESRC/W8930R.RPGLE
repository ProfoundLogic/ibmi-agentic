     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8930R - W/W DATA WAREHOUSE EXTRACT CONTROL FILE
     H*****************************************************************
    H* (c) 2010 StoneRiver, Inc. All rights Reserved.                *
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This Program Displays the Extract Control File Fields for     *
     H* Update of Last Extract Date and Frequency Codes. Updates      *
     H* Control File at end of Program. Changes are stored in Arrays  *
     H* an written to File at LR time. Updates Maint fields           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*  CREATED: 07/18/01  C:132  PROJECT: WC924                     *
     H*  IN BASE: xx/xx/xx  C:132  PROJECT:                           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 01/28/10  C:XXX  Project: SY215                               *
     F*           C:189  Retrieve brand Name
     F*                                                               *
     H* 11/12/01  C:132 PROJECT WC924                                 *
     H*               - Change Description on Table Element for File  *
     H*                 WHEXP TO "Exposure by Policy Number"          *
     H*                                                               *
     H*****************************************************************
     H*
     FW8930F2   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FW8930F    CF   E             WORKSTN INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:RRN)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWHXCP     UF   E             DISK
      /COPY SCOPYMT01D
     D*
     D FIL             S             40    DIM(16) CTDATA PERRCD(1)             File Names
     D MSG             S             40    DIM(10) CTDATA PERRCD(1)
     D*
     D***************************************************************
     D* D A T A     S T R U C T U R E S                             *
     D***************************************************************
     D*
     D* Frequency Codes
     D                 DS
     D  FRQ                    1     16
     D                                     DIM(16)                              Extrct Frequency
     D  XCFR01                 1      1
     D  XCFR02                 2      2
     D  XCFR03                 3      3
     D  XCFR04                 4      4
     D  XCFR05                 5      5
     D  XCFR06                 6      6
     D  XCFR07                 7      7
     D  XCFR08                 8      8
     D  XCFR09                 9      9
     D  XCFR10                10     10
     D  XCFR11                11     11
     D  XCFR12                12     12
     D  XCFR13                13     13
     D  XCFR14                14     14
     D  XCFR15                15     15
     D  XCFR16                16     16
     D* Last Extract Dates
     D                 DS
     D  LST                    1    112  0
     D                                     DIM(16)                              Last Extract Dte
     D  XCLD01                 1      7  0
     D  XCLD02                 8     14  0
     D  XCLD03                15     21  0
     D  XCLD04                22     28  0
     D  XCLD05                29     35  0
     D  XCLD06                36     42  0
     D  XCLD07                43     49  0
     D  XCLD08                50     56  0
     D  XCLD09                57     63  0
     D  XCLD10                64     70  0
     D  XCLD11                71     77  0
     D  XCLD12                78     84  0
     D  XCLD13                85     91  0
     D  XCLD14                92     98  0
     D  XCLD15                99    105  0
     D  XCLD16               106    112  0
     D* Next Extract Dates
     D                 DS
     D  NXT                    1    112  0
     D                                     DIM(16)                              Next Extract Dte
     D  XCND01                 1      7  0
     D  XCND02                 8     14  0
     D  XCND03                15     21  0
     D  XCND04                22     28  0
     D  XCND05                29     35  0
     D  XCND06                36     42  0
     D  XCND07                43     49  0
     D  XCND08                50     56  0
     D  XCND09                57     63  0
     D  XCND10                64     70  0
     D  XCND11                71     77  0
     D  XCND12                78     84  0
     D  XCND13                85     91  0
     D  XCND14                92     98  0
     D  XCND15                99    105  0
     D  XCND16               106    112  0
     D*
      * Work & Output Dates Data Structure
     D                 DS
     D  OTDATE                 1      8  0
     D  ##OTMM                 1      2  0
     D  ##OTDD                 3      4  1
     D  ##OTCC                 5      6  0
     D  ##OTYY                 7      8  0
      *
      * Input Dates Data Sructure
      * WRKDTE 7 Position input Date, INDATE is 6 Position input Date
     D                 DS
     D  WRKDTE                 1      7  0
     D  INDATE                 2      7  0
     D  ##INCC                 1      1  0
     D  ##INYY                 2      3  0
     D  ##INMM                 4      5  0
     D  ##INDD                 6      7  0
      *
      * Edited Dates Data Sructure
      * EDTDTE 10 Position Edit Date
     D                 DS
     D  EDTDTE                 1     10
     D  ##ETMM                 1      2  0
     D  ##ET01                 3      3
     D  ##ETDD                 4      5  0
     D  ##ET02                 6      6
     D  ##ETCC                 7      8  0
     D  ##ETYY                 9     10  0
     D*
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***         DS
     D***                                     1  140DSDTTM
     D***                                     1   60DSTIME
     D***                                     7  140DSDATE
     D*** END   DEL *** 01/28/10 ********************************      ******
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D*** PSDS      ESDSSZQ1P
     D*
     D*** END   DEL *** 01/28/10 ********************************      ******
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('W/W Data Warehouse F-
     D                                     ile Dates')
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***           'Progressive Data So- C         PDSNAM
     D***           'lutions, Inc.'
     D*** END   DEL *** 01/28/10 ********************************      ******
     D*
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)

     I*** BEGIN ADD *** 01/28/10 **************************************
     I*
     I***PY SCOPYMT01I
     I*
     I*** END ADD ***** 01/28/10 **************************************
      *

     C*================================================================
     C*  MAIN  SECTION
     C*================================================================
     C*
     C     BEGIN         TAG
     C*
     C     *IN03         IFEQ      *ON
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C     #SCRID        IFEQ      *ZERO
     C     #SCRID        OREQ      *BLANK
     C                   EXSR      DSPFM1
     C                   ENDIF
     C*
      * If CMD Key 12 is Pressed & is Screen 2, Clear Screen and            "
      * REDISPLAY Subfile Screen One. If NOT Screen 2, Exit Pgm
     C     *IN12         IFEQ      *ON
     C     #SCRID        IFEQ      TWO
     C                   MOVE      ONE           #SCRID
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   EXFMT     SCR01C
     C                   GOTO      BEGIN
     C                   ELSE
     C                   GOTO      ENDIT
     C                   ENDIF
     C                   ENDIF
      *
      *  If CMD Key 03 was pressed, then exit the program
     C     *IN03         IFEQ      *OFF
      *
     C     #SCRID        IFEQ      TWO
     C                   EXSR      PRCFM2
     C                   GOTO      BEGIN
     C                   ENDIF
      *
     C     #SCRID        IFEQ      ONE
     C                   EXSR      PRCFM1
     C                   GOTO      BEGIN
     C                   ENDIF
      *
     C                   END
      *
     C                   GOTO      BEGIN
      *
      ***  ENDIT TAG ***           END OF CALCULATIONS HERE  *******
     C     ENDIT         TAG
      *
     C                   UPDATE    PXC01
     C                   SETON                                        LR
      *
     C********************************************************************
     C* DSPFM1 - Load & Display Subfile Format in Screen W8930F          *
     C********************************************************************
      *
     C     DSPFM1        BEGSR
      *
     C                   MOVE      *OFF          *IN12
     C                   MOVE      ONE           #SCRID
     C********             MOVELQ1USER    USERID
     C                   WRITE     SCR01
     C     RRN2          CHAIN     PXC01                              41
      *
     C     *IN41         IFEQ      *OFF
     C                   Z-ADD     *ZEROS        X
     C                   Z-ADD     XCCO#         XXCO#
     C                   MOVE      XCCO#         SCCO#
     C                   Z-ADD     *ZEROS        RRN
     C     X             DOUEQ     16
     C                   ADD       1             X
     C                   ADD       1             RRN
     C                   MOVEL     FIL(X)        SCFIL
     C                   SUBST     FIL(X):15     SCDES
     C                   MOVE      FRQ(X)        SCFRQ
      *
      * If Dates are Zero - output BLANKS on Screen
     C     LST(X)        IFGT      *ZEROS
     C                   Z-ADD     LST(X)        WRKDTE
     C                   EXSR      CHGDTE
     C                   MOVE      EDTDTE        SCLST
     C                   Z-ADD     NXT(X)        WRKDTE
     C                   EXSR      CHGDTE
     C                   MOVE      EDTDTE        SCNXT
     C                   ELSE
     C                   MOVE      *BLANKS       SCLST
     C                   MOVE      *BLANKS       SCNXT
     C                   ENDIF
      *
     C                   WRITE     SCR01S
     C                   ENDDO
     C                   ENDIF
      *
     C                   EXFMT     SCR01C
      **** End DSPFM1 ****
     C                   ENDSR
      *
     C********************************************************************
     C* DSPFM2 - Display Data Format in Screen W8930F2                   *
     C********************************************************************
      *
     C     DSPFM2        BEGSR
      *
     C                   MOVE      *OFF          *IN12
     C                   MOVE      TWO           #SCRID
     C                   MOVE      XCCO#         #SCCO#
     C                   CLEAR                   ERRMSG
     C*
     C                   Z-ADD     RRN           X
     C                   MOVEL     FIL(X)        #WKA07            7
     C     #WKA07        IFEQ      SCFIL
     C                   MOVE      SCFIL         #SCNAM
     C                   MOVE      FRQ(X)        #SCFRQ
     C                   MOVE      FRQ(X)        #SVFRQ
     C                   SUBST     FIL(X):15     #SCDES
     C                   MOVEL     SCLST         #SCLST
     C                   MOVEL     SCNXT         #SCNXT
     C                   ENDIF
     C*
     C     XCDT          IFNE      *ZEROS
     C                   Z-ADD     XCDT          WRKDTE
     C                   EXSR      CHGDTE
     C                   ELSE
     C                   Z-ADD     Q1DT          WRKDTE
     C                   EXSR      CHGDTE
     C                   ENDIF
     C                   MOVE      EDTDTE        #SCDT
     C                   Z-ADD     WRKDTE        SVMDTE                         SAVE MAINT DTE
     C*
     C     XCUSER        IFGT      *BLANKS
     C                   MOVEL     XCUSER        #SCUSR
     C                   ELSE
     C                   MOVEL     Q1USER        #SCUSR
     C                   ENDIF
     C                   MOVEL     #SCUSR        SVMUSR                         SAVE USERID
     C*
     C                   MOVE      ERRMSG        ERMSG2
     C*
     C                   EXFMT     SCR01F
     C*
     C                   ENDSR
     C*================================================================
     C* PRCFM1 - Load data fields from Screen ONE to Screen TWO
     C*================================================================
     C     PRCFM1        BEGSR
     C*
     C                   MOVE      *ON           *IN51
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *OFF          *IN51
     C                   EXSR      DSPFM2
     C                   LEAVE
     C                   ENDIF
     C                   READC     SCR01S                                 50
     C*
     C                   ENDDO
     C*
     C     *IN51         IFEQ      *ON
     C                   MOVE      *OFF          *IN51
     C                   EXFMT     SCR01C
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PRCFM2 - Load data fields from screen Two to Screen One
     C*================================================================
     C     PRCFM2        BEGSR
      *
     C                   CLEAR                   ERMSG2
      *
     C     #SCNEW        IFGT      *ZEROS
     C                   Z-ADD     #SCNEW        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         LST(X)
     C                   Z-ADD     XXDATE        XCDT
     C                   MOVEL     SRDTO         WRKDTE
     C                   EXSR      CHGDTE
     C                   MOVE      EDTDTE        SCLST
     C                   ELSE
     C                   MOVEL     MSG(2)        ERMSG2
     C                   MOVE      *ON           *IN82
     C                   ENDIF                                                  SRERR = *OFF
      *
     C                   MOVEL     Q1USER        XCUSER
     C                   MOVEL     Q1PGM         XCPGM
     C                   ENDIF                                                  #SCNEW > ZERO
     C*
     C* Check for Invalid Frequency Code
     C     #SCFRQ        IFNE      'D'
     C     #SCFRQ        ANDNE     'W'
     C     #SCFRQ        ANDNE     'M'
     C     #SCFRQ        ANDNE     'S'
     C     #SCFRQ        ANDNE     'Y'
     C     #SCFRQ        ANDNE     'I'
     C     #SCFRQ        ANDNE     ' '
     C                   MOVEL     MSG(1)        ERMSG2
     C                   MOVE      *ON           *IN81
     C                   ENDIF
     C*
     C* If Have Error Code, ReDisplay 2nd Menu
     C     ERMSG2        IFGT      *BLANKS
     C                   MOVE      *ON           *IN80
     C                   EXFMT     SCR01F
     C                   CLEAR                   ERMSG2
     C                   MOVE      *OFF          *IN80
     C                   MOVE      *OFF          *IN81
     C                   MOVE      *OFF          *IN82
     C                   GOTO      ENDPF2
     C                   ENDIF
     C*
     C     #SCFRQ        IFNE      #SVFRQ
     C                   MOVE      #SCFRQ        FRQ(X)
     C                   MOVE      #SCFRQ        SCFRQ
     C                   MOVE      XXDATE        XCDT
     C                   MOVE      Q1USER        XCUSER
     C                   ENDIF
     C*
     C     #SCNEW        IFGT      *ZEROS
     C     #SCFRQ        ORNE      #SVFRQ
     C                   MOVE      *BLANKS       #SCNEW
     C                   ENDIF
      *
     C                   MOVE      ONE           #SCRID
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   EXFMT     SCR01C
      *
     C     ENDPF2        TAG
     C                   ENDSR
      *
     C*================================================================
     C* SUBCLR - Clear the Screen Fields
     C*================================================================
     C     SUBCLR        BEGSR
     C                   Z-ADD     *ZEROS        RRN
     C                   MOVE      *BLANKS       #SCNAM
     C                   MOVE      *BLANKS       #SCDES
     C                   MOVE      *BLANKS       #SCLST
     C                   MOVE      *BLANKS       #SCNXT
     C                   MOVE      *BLANKS       #SCFRQ
     C                   MOVE      *BLANKS       #SCNEW
     C                   MOVE      ONE           #SCRID
     C*
     C* CLEAR SUBFILE AND REDISPLAY IF COMMAND 12 KEY PRESSED
     C                   MOVE      *ON           *IN41
     C                   MOVE      *BLANK        #SCRID
     C                   MOVE      *BLANK        SCSEL
     C                   CLEAR                   SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN41
     C                   MOVE      *OFF          *IN12
     C*
     C                   ENDSR
      *
     C*================================================================
     C* CHGDTE - Move Date Fields and check for valid dates
     C*================================================================
     C     CHGDTE        BEGSR
      *
      *  NOW CHECK DATE FOR VALID INPUT - IF NOT REDISPLAY WITH ERR MSG
      *
     C                   CLEAR                   ERRMSG
     C                   Z-ADD     *ZEROS        ##ETMM
     C                   Z-ADD     *ZEROS        ##ETDD
     C                   Z-ADD     *ZEROS        ##ETCC
     C                   Z-ADD     *ZEROS        ##ETYY
     C     WRKDTE        IFLE      *ZEROS
     C                   Z-ADD     *ZEROS        OTDATE
     C                   GOTO      ENDDTE
     C                   ENDIF
     C*
     C                   Z-ADD     WRKDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   Z-ADD     WRKDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         OTDATE
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         OTDATE
     C                   MOVE      MSG(2)        ERRMSG
     C                   ENDIF                                                  SRERR
      *
      *
     C                   MOVE      ##OTMM        ##ETMM
     C                   MOVE      ##OTDD        ##ETDD
     C                   MOVE      ##OTCC        ##ETCC
     C                   MOVE      ##OTYY        ##ETYY
      *
     C     ENDDTE        TAG
      *
     C                   ENDSR
     C*================================================================
     C* *INZSR - Initialize the subset fields
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C                   MOVE      '1'           ONE               1
     C                   MOVE      '2'           TWO               1
     C***  Begin Chg  ***  01/28/10  **************************************
     C******               MOVE PDSNAM    #SNAME
     C*****                MOVE PDSNAM    SCNAME
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'I'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVE      #BRND1        #SNAME
     C                   MOVE      #BRND1        SCNAME
     C***  END   Chg  ***  01/28/10  **************************************
     C                   MOVE      '/'           ##ET01
     C                   MOVE      '/'           ##ET02
     C     *LIKE         DEFINE    #SCFRQ        #SVFRQ
     C                   Z-ADD     *ZEROS        WRKDTE
     C                   Z-ADD     *ZEROS        OTDATE
     C                   Z-ADD     *ZEROS        RRN               9 0
     C                   Z-ADD     *ZEROS        X                 3 0
     C                   Z-ADD     1             RRN2              3 0
     C                   MOVE      *BLANKS       ERRMSG           40
     C*
     C     *LIKE         DEFINE    XCLD01        XXDATE
     C     *LIKE         DEFINE    XCCO#         XXCO#
     C     *LIKE         DEFINE    XCDT          SVMDTE
     C     *LIKE         DEFINE    XCUSER        SVMUSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDSR
     C*================================================================
** File Names   FIL                    |
WHPOP      1  Policy Information
WHPAP      2  Policy Agent Information
WHEXP      3  Exposure by Policy Number
WHCLP      4  Claims by Claim Number
WHPRP      5  Premium Information
WHLOP      6  Location Information
WHDPP      7  Department Information
WHCNP      8  County Information
WHOFP      9  Office Information
WHFNP     10  Group Information
WHUPP     11  User Parm Information
WHAGP     12  Agent Information
WHAPP     13  Agent Producer Infomation
WHARP     14  A/R Detail Information
WHACP     15  Agent Commission Informtn
WHCTP     16  Claim Transfers
**           ERROR MSGS
INVALID-Frequency is not D,W,M,S,Y or I   01
Invalid Date - Please Re-Enter Date       02
                                          03
                                          04
                                          05
                                          06
                                          07
                                          08
                                          09
                                          10
