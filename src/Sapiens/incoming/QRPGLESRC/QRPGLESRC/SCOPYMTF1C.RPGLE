     C****************************************************************
     C*  Revisions:
     C*
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
     C*  BLDLNS - Builds options and functions lines
     C*  CHGCDA - Update values in the function array
     C*  CHKVFK - Check for valid function key being pressed
     C*  FINISH - Final processing for file update/display
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*  KYDO   - Subroutine for F24=More Keys
     C*  PREADD - Pre-processing before add process
     C*  PRECHG - Pre-processing before change process
     C*  PREDLT - Pre-processing before delete process
     C*  PREDSP - Pre-processing before display process
     C*  XSCR02 - Display second screen and edit check
     C*  XSCR03 - Display third screen and edit check
     C*
     C****************************************************************
     C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
     C*
     C*  DLTPRE - Used for any processing before delete occurs
     C*  DSPLYR - After displaying information
     C*  ERRCK1 - Used to determine errors on the first screen
     C*  ERRCK2 - Used to determine errors on the second screen
     C*  ERRCK3 - Used to determine errors on the third screen
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   Z-ADD     2             #FKL1             1 0
     C                   Z-ADD     2             #FKL2             1 0
     C                   Z-ADD     2             #FKL3             1 0
     C                   Z-ADD     2             #FKL4             1 0
     C                   Z-ADD     2             #FKL5             1 0
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
     C                   EXSR      BLDLNS
     C*
B01  C     *INKC         DOUEQ     *ON
     C     *INKL         OREQ      *ON
     C     OUTFLG        OREQ      *ON
     C                   EXSR      REFRSH
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
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
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVEA     VF1           VF
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEA     VF2           VF
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEA     VF3           VF
     C     Q2RECF        WHENEQ    'SCR04'
     C                   MOVEA     VF4           VF
     C     Q2RECF        WHENEQ    'SCR05'
     C                   MOVEA     VF5           VF
     C                   ENDSL
     C*
     C                   EXSR      CHKVFK
     C*
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *IN90         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C     *INKY         CASEQ     *ON           KYDO                     90
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
     C* BLDLNS - Builds options and functions lines
     C*================================================================
     C     BLDLNS        BEGSR
     C*
     C                   SUB       FL1#1         FL1#1             3 0
     C                   SUB       FL1#2         FL1#2             3 0
     C                   SUB       FL2#1         FL2#1             3 0
     C                   SUB       FL2#2         FL2#2             3 0
     C                   SUB       FL3#1         FL3#1             3 0
     C                   SUB       FL3#2         FL3#2             3 0
     C                   SUB       FL4#1         FL4#1             3 0
     C                   SUB       FL4#2         FL4#2             3 0
     C                   SUB       FL5#1         FL5#1             3 0
     C                   SUB       FL5#2         FL5#2             3 0
     C                   MOVEA(P)  SF            ##SF
     C                   MOVEA(P)  SFD           ##SFD
     C*
     C     1             DO        #OFSCR        XXSCR#            1 0
     C*
     C                   MOVE      XXSCR#        ##SCR#
     C*
     C                   SELECT
     C     XXSCR#        WHENEQ    1
     C                   Z-ADD     #FKL1         ###LNS
     C     XXSCR#        WHENEQ    2
     C                   Z-ADD     #FKL2         ###LNS
     C     XXSCR#        WHENEQ    3
     C                   Z-ADD     #FKL3         ###LNS
     C     XXSCR#        WHENEQ    4
     C                   Z-ADD     #FKL4         ###LNS
     C     XXSCR#        WHENEQ    5
     C                   Z-ADD     #FKL5         ###LNS
     C                   ENDSL
     C*
     C                   CALL      'SRMNTFNC'                           99
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM                    ##SF            100
     C                   PARM                    ##SFD
     C                   PARM                    FL
     C                   PARM                    VF
     C                   PARM                    ##SCR#            1
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM                    ###LNS            1 0
     C*
     C                   SELECT
     C     XXSCR#        WHENEQ    1
     C                   MOVEA     FL            FL1
     C                   MOVEA     VF            VF1
     C                   MOVEL     'SCR01'       Q2RECF
     C     XXSCR#        WHENEQ    2
     C                   MOVEA     FL            FL2
     C                   MOVEA     VF            VF2
     C                   MOVEL     'SCR02'       Q2RECF
     C     XXSCR#        WHENEQ    3
     C                   MOVEA     FL            FL3
     C                   MOVEA     VF            VF3
     C                   MOVEL     'SCR03'       Q2RECF
     C     XXSCR#        WHENEQ    4
     C                   MOVEA     FL            FL4
     C                   MOVEA     VF            VF4
     C                   MOVEL     'SCR04'       Q2RECF
     C     XXSCR#        WHENEQ    5
     C                   MOVEA     FL            FL5
     C                   MOVEA     VF            VF5
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ENDSL
      *
     C                   EXSR      KYDO
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
     C*===============================================================
     C* CHGCDA - Update values in the function array
     C*===============================================================
     C     CHGCDA        BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 3 0
     C*
     C*  Go through array looking for a match
     C*
B01  C     FOUND         DOWEQ     *OFF
     C     SF(I)         ANDNE     '*'
     C*
B02  C     SF(I)         IFEQ      XXSCRN
     C                   MOVEL     SFD(I)        DUMNUM            2
B03  C     XXNMBR        IFEQ      DUMNUM
     C                   MOVEL(P)  SFD(I)        DUMOFD           30
     C                   MOVE      XXCDA         DUMOFD
     C                   MOVEL     DUMOFD        SFD(I)
     C                   MOVE      *ON           FOUND
E03  C                   ENDIF                                                  ZZNUM=DUMNUM
E02  C                   ENDIF                                                  OF,I=XXSCRN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
     C*
E01  C                   ENDDO                                                  FOUND DOWEQ *OFF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* CHKVFK - Check for valid function key being pressed
     C*================================================================
     C     CHKVFK        BEGSR
      *
     C                   SUB       SF#           SF#               3 0
     C                   TESTB     '1'           Q2IAID                   99
      *
B01  C     *IN99         IFEQ      *OFF                                         Function pressd
      *
     C                   TESTB     '7'           Q2IAID                   99
     C   99              ADD       1             SF#
     C                   TESTB     '6'           Q2IAID                   99
     C   99              ADD       2             SF#
     C                   TESTB     '5'           Q2IAID                   99
     C   99              ADD       4             SF#
     C                   TESTB     '4'           Q2IAID                   99
     C   99              ADD       8             SF#
     C                   TESTB     '0'           Q2IAID                   99
     C   99              ADD       12            SF#
      *
B02  C     VF(SF#)       IFEQ      *OFF
     C                   SETON                                        90  95
E02  C                   ENDIF
      *
E01  C                   ENDIF
      *
     C                   ENDSR
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
     C                   MOVEL(P)  PDSNAM        ##DATA
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
      *===============================================================
      * KYDO - SUBROUTINE FOR F24=More Keys
      *===============================================================
     C     KYDO          BEGSR
      *
     C                   SELECT
      *__________
      *
     C     Q2RECF        WHENEQ    'SCR01'
     C     #FKL1         IFEQ      1
     C                   ADD       1             FL1#1
     C                   ELSE
     C     FL1#2         ADD       1             FL1#1
     C                   ENDIF
B01  C     FL1#1         IFGT      12
     C     FL1(FL1#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL1#1
E01  C                   END
     C     FL1#1         ADD       1             FL1#2
     C                   MOVEL     FL1(FL1#1)    SC1FK1           78
     C                   MOVEL     FL1(FL1#2)    SC1FK2           78
      *__________
      *
     C     Q2RECF        WHENEQ    'SCR02'
     C     #FKL2         IFEQ      1
     C                   ADD       1             FL2#1
     C                   ELSE
     C     FL2#2         ADD       1             FL2#1
     C                   ENDIF
     C     FL2#2         ADD       1             FL2#1
B01  C     FL2#1         IFGT      12
     C     FL2(FL2#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL2#1
E01  C                   END
     C     FL2#1         ADD       1             FL2#2
     C                   MOVEL     FL2(FL2#1)    SC2FK1           78
     C                   MOVEL     FL2(FL2#2)    SC2FK2           78
      *__________
      *
     C     Q2RECF        WHENEQ    'SCR03'
     C     #FKL3         IFEQ      1
     C                   ADD       1             FL3#1
     C                   ELSE
     C     FL3#2         ADD       1             FL3#1
     C                   ENDIF
     C     FL3#2         ADD       1             FL3#1
B01  C     FL3#1         IFGT      12
     C     FL3(FL3#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL3#1
E01  C                   END
     C     FL3#1         ADD       1             FL3#2
     C                   MOVEL     FL3(FL3#1)    SC3FK1           78
     C                   MOVEL     FL3(FL3#2)    SC3FK2           78
      *__________
      *
     C     Q2RECF        WHENEQ    'SCR04'
     C     #FKL4         IFEQ      1
     C                   ADD       1             FL4#1
     C                   ELSE
     C     FL4#2         ADD       1             FL4#1
     C                   ENDIF
     C     FL4#2         ADD       1             FL4#1
B01  C     FL4#1         IFGT      12
     C     FL4(FL4#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL4#1
E01  C                   END
     C     FL4#1         ADD       1             FL4#2
     C                   MOVEL     FL4(FL4#1)    SC4FK1           78
     C                   MOVEL     FL4(FL4#2)    SC4FK2           78
      *__________
      *
     C     Q2RECF        WHENEQ    'SCR05'
     C     #FKL5         IFEQ      1
     C                   ADD       1             FL5#1
     C                   ELSE
     C     FL5#2         ADD       1             FL5#1
     C                   ENDIF
     C     FL5#2         ADD       1             FL5#1
B01  C     FL5#1         IFGT      12
     C     FL5(FL5#1)    OREQ      *BLANKS
     C                   Z-ADD     1             FL5#1
E01  C                   END
     C     FL5#1         ADD       1             FL5#2
     C                   MOVEL     FL5(FL5#1)    SC5FK1           78
     C                   MOVEL     FL5(FL5#2)    SC5FK2           78
      *__________
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KYDO
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
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
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
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
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
      /Free
       //******************************************************************;
       // Clear function key lines for web display;
       //******************************************************************;
       Begsr ClearFKeys;
      /If Defined (WEB)
       Clear sc1FK1;
       Clear sc1FK2;
       Clear sc2FK1;
       Clear sc2FK2;
       Clear sc3FK1;
       Clear sc3FK2;
      /Endif
       Endsr;
      /End-free
