     C****************************************************************
     C*  Revisions:
     C*
     C*  07/20/20 C:079  Project: WC2749- Add sixth screen
     C*
     C*  01/28/10 C:249  Project: SY215 - Change Brand Name
     C*           C:189  Retrieve brand name
     C*  10/27/05 C:136  Project: WC1191
     C*                  Expanded to five screens
     C*  03/10/99  C:69  Added logic in case F3 is pressed in a other
     C*                  function program.
     C*  02/19/98  C:69  Changed delete routine to execute the PGMEND
     C*                  subroutine before doing a return.
     C*  12/03/97  C:69  When F12 was pressed on SCR02 or SCR03, the
     C*                  data base record and screen fields needed to
     C*                  be refreshed.
     C*  10/24/97  C:69  Needed to set off *INKL at beginning of pgm
     C****************************************************************
     C*
     C*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
     C*
     C*  09 - Reserved for maintenance/inquiry conditions
     C*  71 - These indicators are to used as error indicators for
     C* thru  the various screens. They will set off after
     C*  89 - displaying each input capible screen.
     C*  90 - Used to general purpose error indicator
     C*  99 - Used when chaining to file being updated
     C*
     C****************************************************************
     C*
     C*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
     C*
     C*  FINISH - Final processing for file update/display
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*  PREADD - Pre-processing before add process
     C*  PRECHG - Pre-processing before change process
     C*  PREDLT - Pre-processing before delete process
     C*  PREDSP - Pre-processing before display process
     C*  XSCR02 - Display second screen and edit check
     C*  XSCR03 - Display third screen and edit check
     C*  XSCR04 - Display fourth screen and edit check
     C*  XSCR05 - Display fifth screen and edit check
     C*  XSCR06 - Display sixth screen and edit check
     C*
     C****************************************************************
     C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
     C*
     C*  DLTPRE - Used for any processing before delete occurs
     C*  DSPLYR - After displaying information
     C*  ERRCK1 - Used to determine errors on the first screen
     C*  ERRCK2 - Used to determine errors on the second screen
     C*  ERRCK3 - Used to determine errors on the third screen
     C*  ERRCK4 - Used to determine errors on the fourth screen
     C*  ERRCK5 - Used to determine errors on the fifth screen
     C*  MVDBSC - Used to move data base fields to screen fields
     C*  MVSCDB - Used to move screen fields to data base fields
     C*  OTHERR - Used for processing other modes
     C*  PGMEND - Used for any end of program processing
     C*  PGMSTR - Used for any start of program processing
     C*  PREOTH - Pre-processing for other modes
     C*  UPDATR - Used to update data base record
     C*  WRITER - Used to write data base record
     C*
     C****************************************************************
      /if not Defined(CONVERT)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
     D FL6             S             78    DIM(14)                              Function lines
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D MODEA           C                   CONST('-- Add --')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display-')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** PDSNAM       C                   CONST('Progressive Data So-
     D***                                  lutions, Inc.')
     D pdsnam          c                   const('Stoneriver, Inc.')
     D*** END CHG   ********* 01/28/10 **************************************
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
      /Endif
     D***  End   Add  ***  01/28/10  **************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVEL     Q1USER        SCUSER           10
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *OFF          OUTFLG            1
     C                   MOVE      *OFF          DSPTYP            1
B01  C     @@MODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09
X01  C                   ELSE
     C                   MOVE      *OFF          *IN09
E01  C                   ENDIF
     C                   MOVE      *OFF          CPYFLG            1
     C     @@MODE        IFEQ      'P'
     C                   MOVE      *ON           CPYFLG
     C                   ENDIF
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   EXSR      PGMSTR
     C*
B01  C     *INKC         DOUEQ     *ON
     C     *INKL         OREQ      *ON
     C     OUTFLG        OREQ      *ON
     C                   EXSR      REFRSH
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B02  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR01
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C   KL              SETON                                        90
B04  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B04  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK1
E04  C                   ENDIF
B04  C     *IN90         IFEQ      *OFF
B05  C     #OFSCR        IFEQ      1
     C                   EXSR      FINISH
X05  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR02
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDDO
E01  C                   ENDDO
     C                   EXSR      PGMEND
     C*
     C                   RETURN
     C*================================================================
     C*  FINISH - Final processing for file update/display
     C*================================================================
     C     FINISH        BEGSR
     C     @@MODE        CASEQ     'A'           WRITER
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C     @@MODE        CASEQ     'P'           WRITER
     C                   CAS                     OTHERR
     C                   ENDCS
B01  C     @@MODE        IFNE      'A'
     C                   MOVE      *ON           OUTFLG
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*================================================================
     C     GETCOF        BEGSR
     C                   CALL      COMAPI        COMPL                  99
B01  C     *IN99         IFEQ      *ON
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         MOVEL(P)  PDSNAM        ##DATA
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval      ##data = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
E01  C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C                   ENDSR
     C*================================================================
     C*  GETDTM - Gets date/time before display screen and saves
     C*================================================================
     C     GETDTM        BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SAVETM            6 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SAVEDT            7 0
     C                   ENDSR
     C*================================================================
     C*  GETPGT - Get program title for display heading
     C*================================================================
     C     GETPGT        BEGSR
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   ENDSR
     C*================================================================
     C*  KEDO   - Process F5; refresh
     C*================================================================
     C     KEDO          BEGSR
     C                   EXSR      REFRSH
     C                   MOVE      *ON           *IN90
     C                   ENDSR
     C*================================================================
     C*  PREADD - Pre-processing before add process
     C*================================================================
     C     PREADD        BEGSR
     C     CPYFLG        IFEQ      *OFF
     C                   CLEAR                   DBRCDF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDIF
     C                   MOVEL(P)  MODEA         SCMODE
     C                   EXSR      MVDBSC
     C                   ENDSR
     C*================================================================
     C*  PRECHG - Pre-processing before change process
     C*================================================================
     C     PRECHG        BEGSR
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVEL(P)  MODEC         SCMODE
     C                   EXSR      MVDBSC
     C                   ENDSR
     C*================================================================
     C*  PREDLT - Pre-processing before delete process
     C*================================================================
     C     PREDLT        BEGSR
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   EXSR      DLTPRE
     C     @@RRN         DELETE    DBRCDF                             99
     C                   EXSR      PGMEND
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C*  PREDSP - Pre-processing before display process
     C*================================================================
     C     PREDSP        BEGSR
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVEL(P)  MODED         SCMODE
     C                   EXSR      MVDBSC
     C                   ENDSR
     C*================================================================
     C*  REFRSH - Refresh screen values and date/time (1st time in/F5)
     C*================================================================
     C     REFRSH        BEGSR
     C     @@MODE        CASEQ     'A'           PREADD
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C     @@MODE        CASEQ     'E'           PREDLT
     C     @@MODE        CASEQ     'P'           PREADD
     C                   CAS                     PREOTH
     C                   ENDCS
     C                   EXSR      GETDTM
     C                   ENDSR
     C*================================================================
     C*  REFRS2 - Refresh screen values and date/time (on screen chg)
     C*================================================================
     C     REFRS2        BEGSR
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
     C                   EXSR      GETDTM
     C                   ENDSR
     C*================================================================
     C*  XSCR02 - Display second screen and edit check
     C*================================================================
     C     XSCR02        BEGSR
     C                   EXSR      REFRS2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR02
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C   KL              SETON                                        90
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B03  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK2
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
B04  C     #OFSCR        IFEQ      2
     C                   EXSR      FINISH
X04  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR03
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   EXSR      REFRS2
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  XSCR03 - Display third screen and edit check
     C*================================================================
     C     XSCR03        BEGSR
     C                   EXSR      REFRS2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl3
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR03
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C   KL              SETON                                        90
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B03  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK3
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
B04  C     #OFSCR        IFEQ      3
     C                   EXSR      FINISH
X04  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR04
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   EXSR      REFRS2
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  XSCR04 - Display fourth screen and edit check
     C*================================================================
     C     XSCR04        BEGSR
     C                   EXSR      REFRS2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl4
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR04
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C   KL              SETON                                        90
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B03  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK4
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
B04  C     #OFSCR        IFEQ      4
     C                   EXSR      FINISH
X04  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR05
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   EXSR      REFRS2
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  XSCR05 - Display fifth screen and edit check
     C*================================================================
     C     XSCR05        BEGSR
     C                   EXSR      REFRS2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl5
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR05
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C   KL              SETON                                        90
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B03  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK5
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
B04  C     #OFSCR        IFEQ      5
     C                   EXSR      FINISH
X04  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR06
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   EXSR      REFRS2
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  XSCR06 - Display sixth screen and edit check
     C*================================================================
     C     XSCR06        BEGSR
     C                   EXSR      REFRS2
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl6
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR06
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C   KL              SETON                                        90
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B03  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK6
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      FINISH
E03  C                   ENDIF
E02  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   EXSR      REFRS2
E01  C                   ENDIF
     C                   ENDSR
