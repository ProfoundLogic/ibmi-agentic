     C****************************************************************
     C*
     C*  Revisions:
     C*
     C*  10/15/03 C:136 Support (WC1159)
     C*                 Prevent all subfile records from being lit up.
     C*
     C*  04/24/01  C:61  Execute KEYSFL prior to UPDAT of SCR01S in
     C*                  PGUPDO.
     C*
     C****************************************************************
     C*
     C*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
     C*
     C*  01 - Used on the PAGEDOWN keyword
     C*  02 - Used on the PAGEUP keyword
     C*  03 - Used to condition SFLCLR
     C* N03 - Used to condition SFLDSPCTL
     C*  04 - Used to condition SFLDSP
     C*  05 - Used to condition DSPATR(RI) on the selection field
     C* N06 - Used to condition DSPATR(PC) on the position to field
     C*  07 - Used to condition DSPATR(PC) on the selection field
     C*  08 - Used to condition SFLNXTCHG
     C*  09 - Reserved for maintenance/inquiry conditions
     C* N10 - Used to condition SFLEND (on message subfile)
     C*  12 - Used to condition SFLDROP/SFLFOLD
     C*  13 - Used to condition SFLCLR (on confirm delete)
     C* N13 - Used to condition SFLDSPCTL (on confirm delete)
     C*  14 - Used to condition SFLDSP (on confirm delete)
     C*  15 - Used to condition SFLEND (on confirm delete)
     C*  20 - Used for the F11 toggle indicator
     C*  71 - These indicators are to used as error indicators for
     C* thru  the various screens. They will set off after
     C*  89 - displaying each input capible screen.
     C*  90 - Used as general purpose error indicator
     C*  91 - Used to display CPF5203; roll up/down past last record
     C*  92 - Used to condition SFLMSGID for more than one opt 1
     C*  95 - Used to condition SFLEND; also used to indicate EOF and
     C*       BOF on the file being read.
     C*  99 - Used for misc purpose; error on calls, on CHAINs
     C*
     C****************************************************************
     C*
     C*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
     C*
     C*  BLDLNS - Builds options and functions lines
     C*  CHKERR - Checks position of blank on selection; checks for
     C*           processing on deleted item.
     C*  CHKSFM - Check subfile mode for fold/truncate; if mode = '0'
     C*           then mode is fold, otherwise truncated
     C*  CLRGET - Clears subfile; get new set of records
     C*  CLRSF1 - Clears subfile; initializes relative record fields
     C*  DLTEXE - Displays confirm delete subfile; processes records
     C*  ENTRDO - Processes Enter key
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*  GETPGT - Get program titles for all screens
     C*  GETRDB - Gets a page of records (backwards)
     C*  GETRDS - Gets a page of records
     C*  KCDO   - Process F3; end of program
     C*  KEDO   - Process F5; refreshes subfile
     C*  KFDO   - Process F6; add records
     C*  KIDO   - Process F9; change info
     C*  KKDO   - Process F11; change view
     C*  KLDO   - Process F12; cancel program
     C*  KMDO   - Process F13; user parameters
     C*  KXDO   - Process F23; more options
     C*  KYDO   - Process F24; more keys
     C*  LOADSF - Load item into subfile
     C*  OPT1   - Process option 1; select item
     C*  OPT2   - Process option 2; change item
     C*  OPT4   - Process option 4; delete item
     C*  OPT5   - Process option 5; display item
     C*  PGDNDO - Process page down; get another page in subfile
     C*  PGUPDO - Process page up; get page for subfile
     C*  REFRSR - Refreshes the item in the subfile from data base
     C*  SF1CHK - Read subfile changes; checks for errors
     C*  SF1EXE - Process subfile changes
     C*  UPDSF1 - Updates subfile record when item is deleted
     C*
     C****************************************************************
     C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
     C*
     C*  ERRCKA - Used to determine errors for the entire subfile
     C*  ERRCK1 - Used to determine errors on the subfile selection
     C*  KEYINZ - Used to initialize key values when program starts
     C*  KEYSET - Used to set key values when using position to
     C*  MVDBSC - Used to move data base fields to screen fields
     C*  OTHFKS - Used to process other function keys
     C*  OTHOPT - Used to process other options
     C*  PGMEND - Used for any end of program processing
     C*  PGMSTR - Used for any start of program processing
     C*  READ   - Used to read next record from file
     C*  SETLL  - Used to set lower limits on file
     C*  SF1UPD - Used to set subfile fields when item is deleted.
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
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   Z-ADD     1             INFO#             1 0
     C                   Z-ADD     1             VIEW#             1 0
     C                   MOVE      *OFF          OUTFLG            1
     C                   MOVE      *OFF          AUTORF            1
     C                   MOVE      *OFF          SELMOD            1
     C                   MOVEL     Q1USER        SCUSER
     C                   MOVE      *OFF          *IN09
B01  C     Q1PRMS        IFGT      3
B02  C     @@MODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09
E02  C                   ENDIF
E01  C                   ENDIF
     C                   EXSR      GETHD#
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   EXSR      KEYINZ
     C                   EXSR      PGMSTR
     C                   EXSR      BLDLNS
     C                   EXSR      CLRGET
     C                   CALL      'SRRMVMSG'                           99
     C*
B01  C     *INKC         DOUEQ     *ON
     C     OUTFLG        OREQ      *ON
     C                   EXSR      GETDTM
     C  N90              WRITE     MSGSFLC
     C  N90              WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
B02  C     *INKC         IFEQ      *OFF
     C                   EXSR      CHKVFK
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      CHKSFM
B04  C     Q2IAID        IFEQ      '1'
     C                   EXSR      ENTRDO
X04  C                   ELSE
     C     *IN01         CASEQ     *ON           PGDNDO
     C     *IN02         CASEQ     *ON           PGUPDO
     C     *INKE         CASEQ     *ON           KEDO
     C     *INKL         CASEQ     *ON           KLDO
     C     *INKM         CASEQ     *ON           KMDO
     C     *INKX         CASEQ     *ON           KXDO
     C     *INKY         CASEQ     *ON           KYDO
     C                   CAS                     OTHFKS
     C                   ENDCS
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
B02  C     *INKC         IFEQ      *ON
     C     SELMOD        ANDEQ     *ON
     C                   MOVE      *OFF          *INKC
E02  C                   ENDIF
E01  C                   ENDDO
     C                   EXSR      PGMEND
     C                   EXSR      KCDO
     C*
     C                   RETURN
     C*================================================================
     C*  BLDLNS - Builds options and functions lines
     C*================================================================
     C     BLDLNS        BEGSR
     C                   SUB       OL#1          OL#1              3 0
     C                   SUB       OL#2          OL#2              3 0
     C                   SUB       FL#1          FL#1              3 0
     C                   SUB       FL#2          FL#2              3 0
     C                   MOVEA(P)  OF            ##OF
     C                   MOVEA(P)  OFD           ##OFD
     C                   MOVE      *BLANKS       ##WWMO
B01  C     Q1PRMS        IFGT      2
     C                   MOVE      @@CALL        ##WWMO
E01  C                   ENDIF
     C                   CALL      'SROPTFNC'                           99
     C                   PARM                    ##WWMO            1
     C                   PARM                    ##OF            100
     C                   PARM                    ##OFD
     C                   PARM                    OL
     C                   PARM                    FL
     C                   PARM                    VO
     C                   PARM                    VF
     C                   PARM      Q1PGM         ##PGM            10

     *******************************************************************
     ******** Add by Profound Logic for Dyamic Function Keys ***********
     C                   Eval      AV_KEYS_DS.ARRAY = fl
     C                   Eval      AV_KEYS =  AV_KEYS_DS.KEYS
     *******************************************************************
     C                   EXSR      KXDO
     C                   EXSR      KYDO
     C                   ENDSR
     C*================================================================
     C*  CHKERR - Checks position of blank on selection; checks for
     C*           processing on deleted item.
     C*================================================================
     C     CHKERR        BEGSR
     C     ' '           CHECK     SCSEL         L                 1 0
     C                   SUBST(P)  SCSEL:L       SCSEL
     C                   MOVEL     SCSEL         TEST1L            1
     C                   MOVE      SCSEL         TEST1R            1
     C                   SUB       OF#           OF#               3 0
B01  C     TEST1R        IFEQ      *BLANK
     C                   MOVE      TEST1L        OF#
X01  C                   ELSE
     C                   MOVE      SCSEL         OF#
E01  C                   ENDIF
B01  C     VO(OF#)       IFEQ      *OFF
     C                   SETON                                        9093
E01  C                   ENDIF
B01  C     *IN90         IFEQ      *OFF
     C     SCDBRR        ANDEQ     *ZEROS
     C                   SETON                                        9093
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  CHKSFM - Check subfile mode for fold/truncate; if mode = '0'
     C*           then mode is fold, otherwise truncated
     C*================================================================
     C     CHKSFM        BEGSR
B01  C     SFLMOD        IFEQ      '0'
     C                   MOVE      *OFF          *IN12
X01  C                   ELSE
     C                   MOVE      *ON           *IN12
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  CHKVFK - Check for valid function key being pressed
     C*================================================================
     C     CHKVFK        BEGSR
     C                   SUB       OF#           OF#
     C                   TESTB     '1'           Q2IAID                   99
B01  C     *IN99         IFEQ      *OFF                                         Function pressd
     C                   TESTB     '7'           Q2IAID                   99
     C   99              ADD       1             OF#
     C                   TESTB     '6'           Q2IAID                   99
     C   99              ADD       2             OF#
     C                   TESTB     '5'           Q2IAID                   99
     C   99              ADD       4             OF#
     C                   TESTB     '4'           Q2IAID                   99
     C   99              ADD       8             OF#
     C                   TESTB     '0'           Q2IAID                   99
     C   99              ADD       12            OF#
B02  C     VF(OF#)       IFEQ      *OFF
     C                   SETON                                        9094
E02  C                   END
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  CLRGET - Clears subfile; get new set of records
     C*================================================================
     C     CLRGET        BEGSR
     C                   EXSR      CLRSF1
     C                   EXSR      GETRDS
     C                   ENDSR
     C*================================================================
     C*  CLRSF1 - Clear subfile; initializes relative record fields
     C*================================================================
     C     CLRSF1        BEGSR
     C                   SUB       LASTRR        LASTRR
     C                   SUB       SFRRN1        SFRRN1               04
     C                   SETON                                        03
     C                   WRITE     SCR01C
     C                   SETOFF                                       03
     C                   ENDSR
     C*================================================================
     C*  DLTEXE - Displays confirm delete subfile; processes records
     C*================================================================
     C     DLTEXE        BEGSR
     C                   clear                   AV_KEYS
     C                   eval      AV_KEYS = 'F12=Cancel'
     C                   Z-ADD     1             SCRRN2
      /If not Defined (WEB)
     C                   WRITE     SCR02
      /Endif
     C                   EXFMT     SCR02C
B01  C     *INKL         IFEQ      *OFF
     C                   READC     SCR02S                                 99
B02  C     *IN99         DOWEQ     *OFF
     C                   MOVE      'E'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   EXSR      UPDSF1
     C                   READC     SCR02S                                 99
E02  C                   ENDDO
E01  C                   ENDIF
     C                   exsr      BldLns
     C                   ENDSR
     C*================================================================
     C*  ENTRDO - Process the Enter key
     C*================================================================
     C     ENTRDO        BEGSR
B01  C     SCPST1        IFNE      *BLANKS
     C                   EXSR      KEYSET
     C                   EXSR      CLRGET
     C                   SETON                                        06
X01  C                   ELSE
     C                   EXSR      ERRCKC
B02  C     *IN90         IFEQ      *OFF
B03  C     *IN04         IFEQ      *ON
     C                   EXSR      SF1CHK
B04  C     *IN90         IFEQ      *OFF
     C                   SETON                                        06
     C                   EXSR      SF1EXE
B05  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B06  C     *IN14         IFEQ      *ON
     C                   EXSR      DLTEXE
     C                   MOVE      *ON           *IN90
E06  C                   ENDIF
E05  C                   ENDIF
     C                   SUB       SFRRN2        SFRRN2               14
     C                   SETON                                        13
     C                   WRITE     SCR02C
     C                   SETOFF                                       13
E04  C                   ENDIF
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCKA
B04  C     *IN90         IFEQ      *OFF
     C                   EXSR      SF1PRS
B05  C     SCOUNT        IFEQ      *ZERO
     C     VF(12)        ANDEQ     *ON
     C     SCOUNT        ORGT      *ZERO
     C     SL1FLG        ANDEQ     *ON
     C                   MOVE      *ON           OUTFLG
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
B02  C     *IN90         IFEQ      *OFF
     C     OUTFLG        ANDEQ     *OFF
     C     AUTORF        ANDEQ     *ON
     C                   EXSR      KEDO
     C                   MOVE      *OFF          AUTORF
E02  C                   ENDIF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  GETHD# - Get the heading line number; move heading to screen
     C*================================================================
     C     GETHD#        BEGSR
     C                   MOVEL(P)  HD(1)         SCHDL1
     C                   ENDSR
     C*================================================================
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*================================================================
     C     GETCOF        BEGSR
     C                   CALL      COMAPI        COMPL                  99
B01  C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  PDSNAM        ##DATA
     C                   CALL      'SRCENTER'                           99
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
     C*  GETPGT - Get program titles for all screens
     C*================================================================
     C     GETPGT        BEGSR
     C     WWCONS        CAT(P)    OPTNM:1       ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C     CDCONS        CAT(P)    OPTNM:1       ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRD2
     C                   ENDSR
     C*================================================================
     C*  GETRDB - Gets a page of records (backwards)
     C*================================================================
     C     GETRDB        BEGSR
     C                   SUB       @RCDS         @RCDS             3 0          RCDS GOTTEN
     C                   MOVE      *OFF          FIRSTI            1
     C                   CALL      'SRSRCHMG'                           99
B01  C     *IN95         DOWEQ     *OFF
     C     @RCDS         ANDLE     SF1PAG
     C                   ADD       1             @RCDS
B02  C     @RCDS         IFLE      SF1PAG
     C     SF1PAG        SUB       @RCDS         SFRRN1               04
     C                   ADD       1             SFRRN1               04
B03  C     FIRSTI        IFEQ      *OFF
     C                   EXSR      CLRSF1
     C     SF1PAG        SUB       @RCDS         SFRRN1               04
     C                   ADD       1             SFRRN1               04
     C                   MOVE      *ON           FIRSTI
     C                   Z-ADD     SFRRN1        LASTRR
E03  C                   ENDIF
     C                   EXSR      LOADSF
E02  C                   ENDIF
     C                   EXSR      READP
E01  C                   ENDDO
     C                   Z-ADD     SFRRN1        SCRRN1
     C                   ENDSR
     C*================================================================
     C*  GETRDS - Gets all records
     C*================================================================
     C     GETRDS        BEGSR
B01  C     SFRRN1        IFEQ      *ZEROS
     C                   EXSR      SETLL
B02  C     *IN96         IFEQ      *OFF
     C                   EXSR      READ
X02  C                   ELSE
     C                   EXSR      READP
E02  C                   ENDIF
E01  C                   ENDIF
     C                   MOVE      *OFF          FIRSTI            1
     C                   CALL      'SRSRCHMG'                           99
B01  C     *IN95         DOWEQ     *OFF
     C                   ADD       1             SFRRN1            4 004
B02  C     FIRSTI        IFEQ      *OFF
     C                   MOVE      *ON           FIRSTI
     C                   Z-ADD     SFRRN1        SCRRN1
E02  C                   ENDIF
     C                   EXSR      LOADSF
     C                   EXSR      READ
E01  C                   ENDDO
     C                   Z-ADD     SFRRN1        LASTRR            4 0
B01  C     *IN09         IFEQ      *OFF
     C     SFRRN1        DIV       SF1PAG        RES               3 0
     C                   MVR                     RMDR              3 0
B02  C     RMDR          IFNE      *ZERO
     C     SF1PAG        SUB       RMDR          BLKS              3 0
     C                   EXSR      PUTBLK
X02  C                   ELSE
B03  C     SFRRN1        IFEQ      *ZERO
     C                   Z-ADD     1             SCRRN1
     C                   Z-ADD     SF1PAG        BLKS
     C                   EXSR      PUTBLK
E03  C                   ENDIF
E02  C                   ENDIF
     C                   MOVE      *OFF          *IN95
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  KCDO   - Process F3; end of program
     C*================================================================
     C     KCDO          BEGSR
B01  C     ##WWMO        IFEQ      ' '
     C     ##WWMO        OREQ      '1'
B02  C     Q1PRMS        IFLE      3
     C                   CALL      'QCMDEXC'
     C                   PARM      RCLCMD        ##CMD            14
     C                   PARM      14            ##CMDL           15 5
     C                   MOVE      *ON           *INLR
E02  C                   ENDIF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  KEDO   - Process F5; refreshes subfile
     C*================================================================
     C     KEDO          BEGSR
     C                   EXSR      CLRGET
     C                   ENDSR
     C*================================================================
     C*  KLDO   - Process F12; cancel program
     C*================================================================
     C     KLDO          BEGSR
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C*  KMDO   - Process F13; user parameters
     C*================================================================
     C     KMDO          BEGSR
     C                   CALL      USPAPI                               99
     C                   EXSR      GETCOF
     C                   EXSR      KEYINZ
     C                   EXSR      PGMSTR
     C                   EXSR      CLRGET
     C                   ENDSR
     C*================================================================
     C*  KXDO   - Process F23; more options
     C*================================================================
     C     KXDO          BEGSR
     C     OL#2          ADD       1             OL#1
B01  C     OL#1          IFGT      12
     C     OL(OL#1)      OREQ      *BLANKS
     C                   Z-ADD     1             OL#1
E01  C                   END
     C     OL#1          ADD       1             OL#2
     C                   MOVEL     OL(OL#1)      SCOPL1
     C                   MOVEL     OL(OL#2)      SCOPL2
     C                   ENDSR
     C*================================================================
     C*  KYDO   - Process F24; more keys
     C*================================================================
     C     KYDO          BEGSR
     C     FL#2          ADD       1             FL#1
B01  C     FL#1          IFGT      12
     C     FL(FL#1)      OREQ      *BLANKS
     C                   Z-ADD     1             FL#1
E01  C                   END
     C     FL#1          ADD       1             FL#2
     C                   MOVEL     FL(FL#1)      SCFKL1
     C                   MOVEL     FL(FL#2)      SCFKL2
     C                   ENDSR
     C*================================================================
     C*  LOADSF - Load item into subfile
     C*================================================================
     C     LOADSF        BEGSR
     C                   SETOFF                                       050708
     C                   Z-ADD     DBRRN         SCDBRR
     C                   EXSR      MVDBSC
     C                   MOVE      *BLANKS       SCSEL
     C                   WRITE     SCR01S
     C                   ENDSR
     C*================================================================
     C*  OPT2   - Process option 2; change item
     C*================================================================
     C     OPT2          BEGSR
     C                   MOVE      'C'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR
     C*================================================================
     C*  OPT4   - Process option 4; delete item
     C*================================================================
     C     OPT4          BEGSR
     C                   Z-ADD     SFRRN1        SCSF1R
     C                   ADD       1             SFRRN2            4 014
     C                   WRITE     SCR02S
     C                   ENDSR
     C*================================================================
     C*  OPT5   - Process option 5; display item
     C*================================================================
     C     OPT5          BEGSR
     C                   MOVE      'D'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR
     C*================================================================
     C*  PGDNDO - Process page down; get another page in subfile
     C*================================================================
     C     PGDNDO        BEGSR
     C                   Z-ADD     SF1PAG        BLKS
     C     LASTRR        ADD       1             SCRRN1
     C                   EXSR      PUTBLK
     C                   ENDSR
     C*================================================================
     C*  PGUPDO - Process page up; get page for subfile
     C*================================================================
     C     PGUPDO        BEGSR
B01  C     *IN04         IFEQ      *ON
     C     1             CHAIN     SCR01S                             99
     C***** BEG ADD ****** 04/24/01 **********************************
     C                   EXSR      KEYSFL
     C***** END ADD ****** 04/24/01 **********************************
     C                   SETON                                        08
     C                   UPDATE    SCR01S
     C***** BEG DEL ****** 04/24/01 **********************************
     C*********            EXSR KEYSFL
     C***** END DEL ****** 04/24/01 **********************************
X01  C                   ELSE
     C                   EXSR      KEYINZ
E01  C                   ENDIF
     C                   EXSR      SETLL
     C                   EXSR      READP
B01  C     *IN95         IFEQ      *ON
     C                   SETON                                        9091
     C                   SETOFF                                       95
X01  C                   ELSE
     C                   EXSR      GETRDB
B02  C                   SELECT
S01  C     @RCDS         WHENEQ    0
     C                   SETON                                        9091
B03  C     LASTRR        IFGE      SF1PAG
     C   95              SETOFF                                       95
E03  C                   ENDIF
S02  C     @RCDS         WHENGT    SF1PAG
     C   95              SETOFF                                       95
S03  C                   OTHER
     C                   EXSR      KEYINZ
     C                   EXSR      CLRGET
E02  C                   ENDSL
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  REFRSR - Refreshes the item in the subfile from data base
     C*================================================================
     C     PUTBLK        BEGSR
     C                   Z-ADD     LASTRR        SFRRN1
B01  C     1             DO        BLKS
     C                   ADD       1             SFRRN1               04
     C                   CLEAR                   SCR01S
     C                   WRITE     SCR01S
E01  C                   ENDDO
     C                   Z-ADD     SFRRN1        LASTRR
     C                   ENDSR
     C*================================================================
     C*  REFRSR - Refreshes the item in the subfile from data base
     C*================================================================
     C     REFRSR        BEGSR
B01  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     SCDBRR        CHAIN     PFRCDF                             99
B02  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVDBSC
E02  C                   ENDIF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  SF1CHK - Read subfile changes; checks for errors
     C*================================================================
     C     SF1CHK        BEGSR
     C                   SUB       COUNT1        COUNT1            3 0
      *** BEG DEL ***** 10/15/03 **************************************
     C*******************  MOVEA'00000'   *IN,71
     C*******************  MOVEA'00000'   *IN,76
     C*******************  MOVEA'00000'   *IN,81
     C*******************  MOVEA'0000'    *IN,86
     C*******************  MOVEA'00'      *IN,92
      *** END DEL ***** 10/15/03 **************************************
     C                   READC     SCR01S                                 99
B01  C     *IN99         DOWEQ     *OFF
      *** BEG ADD ***** 10/15/03 **************************************
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '0000'        *IN(86)
     C                   MOVEA     '00'          *IN(92)
      *** END ADD ***** 10/15/03 **************************************
     C                   SETOFF                                       050708
B02  C     *IN90         IFEQ      *OFF
B03  C     SCSEL         IFNE      *BLANKS
     C                   EXSR      CHKERR
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCK1
B04  C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '1'
     C                   ADD       1             COUNT1
E04  C                   ENDIF
E03  C                   ENDIF
B03  C     COUNT1        IFGT      1
     C                   SETON                                        9092
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *ON
     C                   SETON                                        0507
     C                   Z-ADD     SFRRN1        SCRRN1
E03  C                   ENDIF
      *** BEG ADD ***** 010/15/03 **********************************
     C                   MOVEA     *IN           ER                             SAVE INDICATORS
      *** END ADD ***** 010/15/03 **********************************
E02  C                   ENDIF
     C                   SETON                                        08
     C                   UPDATE    SCR01S
     C                   READC     SCR01S                                 99
E01  C                   ENDDO
      *** BEG ADD ***** 010/15/03 **********************************
     C                   MOVEA     ER            *IN                            RESTORE INDICATORS
      *** END ADD ***** 010/15/03 **********************************
     C                   ENDSR
     C*================================================================
     C*  SF1EXE - Process subfile changes
     C*================================================================
     C     SF1EXE        BEGSR
     C                   SUB       SCOUNT        SCOUNT            5 0
     C                   MOVE      *OFF          SL1FLG            1
     C                   READC     SCR01S                                 99
B01  C     *IN99         DOWEQ     *OFF
     C                   SETOFF                                       0708
B02  C     SCSEL         IFNE      *BLANKS
     C                   ADD       1             SCOUNT
     C     SCSEL         CASEQ     '2 '          OPT2
     C     SCSEL         CASEQ     '4 '          OPT4
     C     SCSEL         CASEQ     '5 '          OPT5
     C                   CAS                     OTHOPT
     C                   ENDCS
B03  C     SCSEL         IFNE      '4 '
     C                   MOVE      *BLANKS       SCSEL
     C                   Z-ADD     SFRRN1        SCRRN1
     C                   EXSR      REFRSR
E03  C                   ENDIF
E02  C                   ENDIF
     C                   SETON                                        08
     C                   UPDATE    SCR01S
     C  NKC
     CANNKL              READC     SCR01S                                 99
     C  NKC
E01  CANNKL              ENDDO
     C                   ENDSR
     C*================================================================
     C*  SF1PRS - Process subfile for data base writes and updates
     C*================================================================
     C     SF1PRS        BEGSR
     C                   READC     SCR01S                                 99
B01  C     *IN99         DOWEQ     *OFF
B02  C     SCDBRR        IFEQ      *ZEROS
     C                   EXSR      WRITER
X02  C                   ELSE
     C                   EXSR      UPDATR
E02  C                   ENDIF
     C  NKC
     CANNKL              READC     SCR01S                                 99
     C  NKC
E01  CANNKL              ENDDO
     C                   ENDSR
     C*================================================================
     C*  UPDSF1 - Updates subfile record when item is deleted
     C*================================================================
     C     UPDSF1        BEGSR
     C     SCSF1R        CHAIN     SCR01S                             99
B01  C     *IN99         IFEQ      *OFF
     C                   CLEAR                   SCR01S
     C                   EXSR      SF1UPD
E01  C                   ENDIF
     C                   UPDATE    SCR01S
     C                   ENDSR
