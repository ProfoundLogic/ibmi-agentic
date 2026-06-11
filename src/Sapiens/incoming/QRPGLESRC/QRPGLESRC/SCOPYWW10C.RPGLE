     C****************************************************************
     C*  Revisions:
     C*
     C*  05/11/04  C:136 Chain to physical file record in SF1EXE to
     C*                  update physical file fields before executing
     C*                  options.
     C*
     C*  02/11/04  C:33  Increase # of option lines.
     C*
     C*  11/07/03  C:33  When F5 is pressed, stay on same page.
     C*
     C*  06/17/03  C:61  Chain to physical file record prior ERRCK1.
     C*
     C*  09/27/01  C:61  Execute KEYSFL prior to UPDAT of SCR01S.
     C*
     C*  03/02/00  C:69  When Page up is pressed and the subfile is
     C*                  empty, do not do the READC added on 1/21.
     C*  01/21/00  C:69  When Page up is pressed and BOF is reached,
     C*                  make sure that options on the first subfile
     C*                  item are checked.
     C*  06/01/99  C:69  When F23/F24 is pressed, make sure that the
     C*                  current subfile page is shown, instead of last
     C*  04/16/99  C:69  When subfile has items that had been selected,
     C*                  and F5 is pressed and items no longer appear
     C*                  in the list, the program would not exit via
     C*                  the Enter key.
     C*  04/05/99  C:69  When positioning to beyond EOF, the READP was
     C*                  not checking the subset. Also, the Searching..
     C*                  message was removed when paging up.
     C*  08/23/98  C:69  Added subroutine UPDOFA for making changes to
     C*                  the option/function array.
     C*  01/07/98  C:69  When last item of a page has a option and page
     C*                  down is pressed, the item will now process.
     C*  12/11/97  C:69  When Enter/F23/F24 is pressed w/o selections,
     C*                  the list will now stay on same page.
     C*  12/10/97  C:69  When PAGE UP is pressed and beginning of file
     C*                  is reached, ensure that the correct SFLEND is
     C*                  displayed. Only happened when less than 1 page
     C*                  of records were displayed.
     C*  10/10/97  C:69  When doing single item, the program will not
     C*                  do SETLL & READP if PAGE UP is pressed.
     C*  09/24/97  C:69  1. Only allow F3 when it's a valid function.
     C*                  2. Check for positioning beyond the high-
     C*                  order key, show last record if necessary
     C*                  3. Check the option field (SCSEL) and make
     C*                  sure it contains numeric characters.
     C*  09/10/97  C:69  When repositioning subfile, the program will
     C*                  now check for deleted subfile records first
     C*  08/12/97  C:69  Clear and resend subset message when F5 is
     C*                  pressed
     C*  07/24/97  C:69  Set off *IN06 when first coming into program
     C*                  so cursor will be on position to field
     C*  07/21/97  C:69  Changed CHKERR routine to not allow options
     C*                  of 0 or less
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
     C*  11 - Used to indicate the list is subsetted
     C*  12 - Used to condition SFLDROP/SFLFOLD
     C*  13 - Used to condition SFLCLR (on confirm delete)
     C* N13 - Used to condition SFLDSPCTL (on confirm delete)
     C*  14 - Used to condition SFLDSP (on confirm delete)
     C*  15 - Used to condition SFLEND (on confirm delete)
     C*  21 - These indicators are used for the associated view #
     C* thru
     C*  29 - 21=View #1, 22=View #2, etc.
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
     C*  KRDO   - Process F17; subset selection
     C*  KXDO   - Process F23; more options
     C*  KYDO   - Process F24; more keys
     C*  LOADSF - Load item into subfile
     C*  OPT1   - Process option 1; select item
     C*  OPT2   - Process option 2; change item
     C*  OPT3   - Process option 3; copy item
     C*  OPT4   - Process option 4; delete item
     C*  OPT5   - Process option 5; display item
     C*  PGDNDO - Process page down; get another page in subfile
     C*  PGUPDO - Process page up; get page for subfile
     C*  READCK - Checks the number of reads; calls SRRDCCKR if necess
     C*  REFRSR - Refreshes the item in the subfile from data base
     C*  SF1CHK - Read subfile changes; checks for errors
     C*  SF1EXE - Process subfile changes
     C*  SNDSSM - Send subsetted list message if necessary
     C*  UPDOFA - Update values in the option/function array
     C*  UPDSF1 - Updates subfile record when item is deleted
     C*
     C****************************************************************
     C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
     C*
     C*  CHKSSM - Used to set indicator 11 for subset message
     C*  ERRCK1 - Used to determine errors on the subfile selection
     C*  ERRCK2 - Used to determine errors on the subset selection
     C*  INZSST - Used to initialize the subset selection fields
     C*  KEYINZ - Used to initialize key values when program starts
     C*  KEYSET - Used to set key values when using position to
     C*  KEYSFL - Used to set key values from subfile when paging
     C*  MVDBSC - Used to move data base fields to screen fields
     C*  OTHFKS - Used to process other function keys
     C*  OTHOPT - Used to process other options
     C*  PGMEND - Used for any end of program processing
     C*  PGMSTR - Used for any start of program processing
     C*  READ   - Used to read next record from file
     C*  READP  - Used to read previous record from file
     C*  SETGT  - Used to set greater than on file
     C*  SETLL  - Used to set lower limits on file
     C*  SF1UPD - Used to set subfile fields when item is deleted.
     C*  SLCTRD - Used to move fields for selected records
     C*  SUBSET - Used to determine if the record read should be placed
     C*           in the subfile.
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      *BLANKS       XXOPFN            1
     C                   MOVE      *BLANKS       XXNMBR            2
     C                   MOVE      *BLANKS       XXMIS             3
     C                   Z-ADD     1             INFO#             1 0
     C                   Z-ADD     1             VIEW#             1 0
     C                   Z-ADD     1000          @INC              7 0          BASE INCREMENT
     C                   Z-ADD     @INC          @INCC             7 0          CURR INCREMENT
     C                   MOVE      *OFF          AUTORF            1
     C                   MOVE      *OFF          SNGITM            1
     C                   MOVE      *OFF          SELMOD            1
     C                   MOVE      *OFF          *IN06
     C                   MOVEL     Q1USER        SCUSER           10
     C                   EXSR      GETCOF
     C                   EXSR      KEYINZ
     C                   EXSR      PGMSTR
     C                   EXSR      GETHD#
     C                   EXSR      BLDLNS
     C                   EXSR      GETPGT
     C                   EXSR      INZSST
     C                   EXSR      CLRGET
     C                   CALL      'SRRMVMSG'                           99
     C                   EXSR      SNDSSM
     C*
B01  C     RSTPGM        DOUEQ     *OFF
     C                   MOVE      *OFF          RSTPGM            1
     C*
B02  C     *INKC         DOUEQ     *ON
     C     VF(3)         ANDEQ     *ON
     C     OUTFLG        OREQ      *ON
     C                   MOVE      *OFF          OUTFLG            1
      /If not Defined (WEB)
     C  N90              WRITE     MSGSFLC
     C  N90              WRITE     SCR01
      /Endif
     C                   EXFMT     SCR01C
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
B03  C     *INKC         IFEQ      *OFF
     C     VF(3)         OREQ      *OFF
     C                   EXSR      CHKVFK
B04  C     *IN90         IFEQ      *OFF
     C                   EXSR      CHKSFM
B05  C     Q2IAID        IFEQ      '1'
     C*                    EXSR ERRCKC
B06  C     *IN90         IFEQ      *OFF
     C                   EXSR      ENTRDO
E06  C                   ENDIF
X05  C                   ELSE
     C     *IN01         CASEQ     *ON           PGDNDO
     C     *IN02         CASEQ     *ON           PGUPDO
     C     *INKE         CASEQ     *ON           KEDO
     C     *INKF         CASEQ     *ON           KFDO
     C     *INKI         CASEQ     *ON           KIDO
     C     *INKK         CASEQ     *ON           KKDO
     C     *INKL         CASEQ     *ON           KLDO
     C     *INKM         CASEQ     *ON           KMDO
     C     *INKR         CASEQ     *ON           KRDO
     C     *INKX         CASEQ     *ON           KXDO
     C     *INKY         CASEQ     *ON           KYDO
     C                   CAS                     OTHFKS
     C                   ENDCS
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
B03  C     *INKC         IFEQ      *ON
     C     VF(3)         ANDEQ     *ON
     C     SELMOD        ANDEQ     *ON
     C                   MOVE      *OFF          *INKC
E03  C                   ENDIF
E02  C                   ENDDO
     C                   EXSR      PGMEND
E01  C                   ENDDO
     C                   EXSR      KCDO
     C*

     *******************************************************************
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
B01  C     Q1PRMS        IFGT      0
     C                   MOVE      @@CALL        ##WWMO
E01  C                   ENDIF
     C                   CALL      OPTFNC                               99
     C                   PARM                    ##WWMO            1
     C                   PARM                    ##OF            100
     C                   PARM                    ##OFD
     C                   PARM                    OL
     C                   PARM                    FL
     C                   PARM                    VO
     C                   PARM                    VF
     C                   Parm      Q1PGM         ##PGM            10
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
     C                   TESTN                   TEST1L               99  99
B02  C     *IN99         IFEQ      *OFF
     C                   SETON                                        9093
E02  C                   ENDIF
     C  N90              MOVE      TEST1L        OF#
X01  C                   ELSE
     C                   TESTN                   SCSEL                999999
B02  C     *IN99         IFEQ      *OFF
     C                   SETON                                        9093
E02  C                   ENDIF
     C  N90              MOVE      SCSEL         OF#
E01  C                   ENDIF
B01  C     *IN90         IFEQ      *OFF
     C     OF#           ANDLT     1
     C                   SETON                                        9093
E01  C                   ENDIF
B01  C     *IN90         IFEQ      *OFF
     C     VO(OF#)       ANDEQ     *OFF
     C                   SETON                                        9093
E01  C                   ENDIF
B01  C     *IN90         IFEQ      *OFF
     C     SCDBRR        ANDEQ     *ZEROS
     C                   SETON                                        9093
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  CHKSFB - Check subfile records (backward) for non-deleted
     C*           record, then use for setting keys
     C*================================================================
     C     CHKSFB        BEGSR
B01  C     *IN04         IFEQ      *ON
     C     SFKEY         CHAIN     SCR01S                             99
B02  C     *IN99         DOWEQ     *OFF
     C     SCDBRR        ANDEQ     *ZEROS
     C                   SUB       1             SFKEY
     C     SFKEY         CHAIN     SCR01S                             99
E02  C                   ENDDO
E01  C                   ENDIF
B01  C     *IN99         IFEQ      *OFF
     C     *IN04         ANDEQ     *ON
     C                   SETON                                        08
      ***** BEG ADD ****** 09/27/01 **********************************
     C                   EXSR      KEYSFL
      ***** END ADD ****** 09/27/01 **********************************
     C                   UPDATE    SCR01S
      ***** BEG DEL ****** 09/27/01 **********************************
     C*********            EXSR KEYSFL
      ***** END DEL ****** 09/27/01 **********************************
X01  C                   ELSE
     C                   EXSR      KEYINZ
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  CHKSFF - Check subfile records (forward) for non-deleted
     C*           record, then use for setting keys
     C*================================================================
     C     CHKSFF        BEGSR
B01  C     *IN04         IFEQ      *ON
     C     SFKEY         CHAIN     SCR01S                             99
B02  C     *IN99         DOWEQ     *OFF
     C     SCDBRR        ANDEQ     *ZEROS
     C                   ADD       1             SFKEY
     C     SFKEY         CHAIN     SCR01S                             99
E02  C                   ENDDO
E01  C                   ENDIF
B01  C     *IN99         IFEQ      *OFF
     C     *IN04         ANDEQ     *ON
     C                   SETON                                        08
      ***** BEG ADD ****** 09/27/01 **********************************
     C                   EXSR      KEYSFL
      ***** END ADD ****** 09/27/01 **********************************
     C                   UPDATE    SCR01S
      ***** BEG DEL ****** 09/27/01 **********************************
     C*********            EXSR KEYSFL
      ***** END DEL ****** 09/27/01 **********************************
X01  C                   ELSE
     C                   EXSR      KEYINZ
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
     C     SCPST2        ORNE      *BLANKS
     C     SCPST3        ORNE      *BLANKS
     C     SCPST4        ORNE      *BLANKS
     C     SCPST5        ORNE      *BLANKS
     C     SCPST6        ORNE      *BLANKS
     C     SCPST7        ORNE      *BLANKS
     C     SCPST8        ORNE      *BLANKS
     C     SCPST9        ORNE      *BLANKS
     C                   EXSR      KEYSET
     C                   EXSR      CLRGET
     C                   SETON                                        06
X01  C                   ELSE
     C                   SUB       SCOUNT        SCOUNT            5 0
     C                   MOVE      *OFF          SL1FLG            1
B02  C     *IN04         IFEQ      *ON
B03  C     Q2LSFR        IFNE      *ZEROS
     C                   Z-ADD     Q2LSFR        SCRRN1
E03  C                   ENDIF
     C                   EXSR      SF1CHK
B03  C     *IN90         IFEQ      *OFF
     C                   SETON                                        06
     C                   EXSR      SF1EXE
B04  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
B05  C     *IN14         IFEQ      *ON
     C                   EXSR      DLTEXE
E05  C                   ENDIF
E04  C                   ENDIF
     C                   SUB       SFRRN2        SFRRN2               14
     C                   SETON                                        13
     C                   WRITE     SCR02C
     C                   SETOFF                                       13
E03  C                   ENDIF
E02  C                   ENDIF
B02  C     *IN90         IFEQ      *OFF
B03  C     SCOUNT        IFEQ      *ZERO
     C     VF(12)        ANDEQ     *ON
     C     SCOUNT        ORGT      *ZERO
     C     SL1FLG        ANDEQ     *ON
     C                   MOVE      *ON           OUTFLG
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
     C*  FNDRDB - Find a record (backwards)
     C*================================================================
     C     FNDRDB        BEGSR
     C                   SUB       @RCDS         @RCDS             3 0          RCDS GOTTEN
     C                   SUB       @READS        @READS            7 0          TOTAL READS
     C                   SUB       @READC        @READC            7 0          CURRENT READS
     C                   EXSR      READP
B01  C     *IN95         DOWEQ     *OFF
     C     @RCDS         ANDLT     1
     C                   ADD       1             @READS
     C                   ADD       1             @READC
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
B02  C     MEETSS        IFEQ      *OFF
     C                   EXSR      READCK
     C  N95              EXSR      READP
     C                   ELSE
     C                   ADD       1             @RCDS
E02  C                   ENDIF
E01  C                   ENDDO
     C                   ENDSR
     C*================================================================
     C*  GETHD# - Get the heading line number; move heading to screen
     C*================================================================
     C     GETHD#        BEGSR
     C     VIEWMX        MULT      INFO#         HD#               3 0
     C                   SUB       VIEWMX        HD#
     C                   ADD       VIEW#         HD#
     C                   MOVEL(P)  HD(HD#)       SCHDL1
     C     VIEW#         ADD       20            VI                3 0
     C                   MOVEA     '00000000'    *IN(21)
     C                   MOVEA     '0'           *IN(29)
     C                   MOVE      *ON           *IN(VI)
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
     C*  GETPGT - Get program titles for all screens
     C*================================================================
     C     GETPGT        BEGSR
B01  C     ##WWMO        IFEQ      '2'
     C     'Select'      CAT(P)    OPTNM:1       ##DATA
X01  C                   ELSE
     C     WWCONS        CAT(P)    OPTNM:1       ##DATA
E01  C                   ENDIF
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C     CDCONS        CAT(P)    OPTNM:1       ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRD2
     C     SSCONS        CAT(P)    OPTNM:1       ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRD3
     C                   ENDSR
     C*================================================================
     C*  GETRDB - Gets a page of records (backwards)
     C*================================================================
     C     GETRDB        BEGSR
     C                   SUB       @RCDS         @RCDS             3 0          RCDS GOTTEN
     C                   SUB       @READS        @READS            7 0          TOTAL READS
     C                   SUB       @READC        @READC            7 0          CURRENT READS
     C                   MOVE      *OFF          FIRSTI            1
     C***                  CALL 'SRSRCHMG'             99
B01  C     *IN95         DOWEQ     *OFF
     C     @RCDS         ANDLE     SF1PAG
     C                   ADD       1             @READS
     C                   ADD       1             @READC
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
B02  C     MEETSS        IFEQ      *ON
     C                   ADD       1             @RCDS
B03  C     @RCDS         IFLE      SF1PAG
     C     SF1PAG        SUB       @RCDS         SFRRN1               04
     C                   ADD       1             SFRRN1               04
B04  C     FIRSTI        IFEQ      *OFF
     C                   EXSR      CLRSF1
     C     SF1PAG        SUB       @RCDS         SFRRN1               04
     C                   ADD       1             SFRRN1               04
     C                   MOVE      *ON           FIRSTI
     C                   Z-ADD     SFRRN1        LASTRR
E04  C                   ENDIF
     C                   EXSR      LOADSF
E03  C                   ENDIF
E02  C                   ENDIF
B02  C     @RCDS         IFLE      SF1PAG
     C                   EXSR      READCK
     C  N95              EXSR      READP
E02  C                   ENDIF
E01  C                   ENDDO
     C                   Z-ADD     SFRRN1        SCRRN1
     C***  Begin Add  ***  11/07/03  **************************************
     C                   Z-ADD     1             FRSTRR
     C***  End   Add  ***  11/07/03  **************************************
     C                   ENDSR
     C*================================================================
     C*  GETRDS - Gets a page of records
     C*================================================================
     C     GETRDS        BEGSR
B01  C     SFRRN1        IFEQ      *ZEROS
     C                   EXSR      SETLL
B02  C     *IN96         IFEQ      *OFF
     C                   EXSR      READ
B03  C     *IN95         IFEQ      *ON                                          Read failed
     C                   EXSR      SETLL
     C                   EXSR      FNDRDB
E03  C                   ENDIF
X02  C                   ELSE
     C                   EXSR      FNDRDB
E02  C                   ENDIF
E01  C                   ENDIF
     C                   SUB       @RCDS         @RCDS             3 0          RCDS GOTTEN
     C                   SUB       @READS        @READS            7 0          TOTAL READS
     C                   SUB       @READC        @READC            7 0          CURRENT READS
     C                   MOVE      *OFF          FIRSTI            1
     C                   CALL      'SRSRCHMG'                           99
B01  C     *IN95         DOWEQ     *OFF
     C     @RCDS         ANDLE     SF1PAG
     C                   ADD       1             @READS
     C                   ADD       1             @READC
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
B02  C     MEETSS        IFEQ      *ON
     C                   ADD       1             @RCDS
B03  C     @RCDS         IFLE      SF1PAG
     C                   ADD       1             SFRRN1            4 004
B04  C     FIRSTI        IFEQ      *OFF
     C                   MOVE      *ON           FIRSTI
     C                   Z-ADD     SFRRN1        SCRRN1
     C***  Begin Add  ***  11/07/03  **************************************
     C     *LIKE         DEFINE    SFRRN1        FRSTRR
     C                   Z-ADD     SFRRN1        FRSTRR
     C***  End   Add  ***  11/07/03  **************************************
E04  C                   ENDIF
     C                   EXSR      LOADSF
E03  C                   ENDIF
E02  C                   ENDIF
B02  C     SNGITM        IFEQ      *OFF
B03  C     @RCDS         IFLE      SF1PAG
     C                   EXSR      READCK
     C  N95              EXSR      READ
E03  C                   ENDIF
X02  C                   ELSE
     C                   SETON                                        95
E02  C                   ENDIF
E01  C                   ENDDO
     C                   Z-ADD     SFRRN1        LASTRR            4 0
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
     C***  Begin Chg  ***  11/07/03  **************************************
     C**********           Z-ADD1         SFKEY   50
     C                   Z-ADD     FRSTRR        SFKEY             5 0
     C***  End   Chg  ***  11/07/03  **************************************
     C                   EXSR      CHKSFF
     C                   EXSR      CLRGET
     C                   CALL      'SRRMVMSG'                           99
     C                   EXSR      SNDSSM
     C                   ENDSR
     C*================================================================
     C*  KFDO   - Process F6; add records
     C*================================================================
     C     KFDO          BEGSR
     C                   MOVE      *ZEROS        SCDBRR
     C                   MOVE      'A'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C  NKC              EXSR      KEDO
     C                   ENDSR
     C*================================================================
     C*  KIDO   - Process F9; change info
     C*================================================================
     C     KIDO          BEGSR
     C                   ADD       1             INFO#
B01  C     INFO#         IFGT      INFOMX
     C     INFO#         OREQ      *ZERO
     C                   Z-ADD     1             INFO#
E01  C                   ENDIF
     C                   EXSR      GETHD#
     C                   Z-ADD     Q2LSFR        SFKEY
     C                   EXSR      CHKSFF
     C                   EXSR      CLRGET
     C                   ENDSR
     C*================================================================
     C*  KKDO   - Process F11; change view
     C*================================================================
     C     KKDO          BEGSR
     C                   ADD       1             VIEW#
B01  C     VIEW#         IFGT      VIEWMX
     C     VIEW#         OREQ      *ZERO
     C                   Z-ADD     1             VIEW#
E01  C                   ENDIF
     C                   EXSR      GETHD#
     C                   SETOFF                                       06
     C                   EXSR      KEYINZ
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
     C*  KRDO   - Process F17; subset selection
     C*================================================================
     C     KRDO          BEGSR
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR03
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
B02  C     *INKL         IFEQ      *OFF
     C                   EXSR      ERRCK2
B03  C     *IN90         IFEQ      *OFF
     C                   CALL      'SRRMVMSG'                           99
     C                   EXSR      SNDSSM
     C                   EXSR      KEYINZ
     C                   EXSR      CLRGET
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
     C                   ENDSR
     C*================================================================
     C*  KXDO   - Process F23; more options
     C*================================================================
     C     KXDO          BEGSR
     C     OL#2          ADD       1             OL#1
     C***  Begin Chg  ***  02/11/04  **************************************
B01  C********** OL#1      IFGT 12
B01  C     OL#1          IFGT      20
     C***  End   Chg  ***  02/11/04  **************************************
     C     OL(OL#1)      OREQ      *BLANKS
     C                   Z-ADD     1             OL#1
E01  C                   END
     C     OL#1          ADD       1             OL#2
     C                   MOVEL     OL(OL#1)      SCOPL1
     C                   MOVEL     OL(OL#2)      SCOPL2
B03  C     Q2LSFR        IFNE      *ZEROS
     C                   Z-ADD     Q2LSFR        SCRRN1
E03  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  KYDO   - Process F24; more keys
     C*================================================================
     C     KYDO          BEGSR
     C     FL#2          ADD       1             FL#1
B01  C     FL#1          IFGT      20
     C     FL(FL#1)      OREQ      *BLANKS
     C                   Z-ADD     1             FL#1
E01  C                   END
     C     FL#1          ADD       1             FL#2
     C                   MOVEL     FL(FL#1)      SCFKL1
     C                   MOVEL     FL(FL#2)      SCFKL2
B03  C     Q2LSFR        IFNE      *ZEROS
     C                   Z-ADD     Q2LSFR        SCRRN1
E03  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  LOADSF - Load item into subfile
     C*================================================================
     C     LOADSF        BEGSR
     C                   SETOFF                                       050708
B01  C                   SELECT
S01  C     VIEW#         WHENEQ    1
     C                   Z-ADD     DBRRN         SCDBRR
S02  C     VIEW#         WHENEQ    2
     C                   Z-ADD     DBRRN2        SCDBRR
S03  C     VIEW#         WHENEQ    3
     C                   Z-ADD     DBRRN3        SCDBRR
S04  C     VIEW#         WHENEQ    4
     C                   Z-ADD     DBRRN4        SCDBRR
S05  C     VIEW#         WHENEQ    5
     C                   Z-ADD     DBRRN5        SCDBRR
S06  C     VIEW#         WHENEQ    6
     C                   Z-ADD     DBRRN6        SCDBRR
S07  C     VIEW#         WHENEQ    7
     C                   Z-ADD     DBRRN7        SCDBRR
S08  C     VIEW#         WHENEQ    8
     C                   Z-ADD     DBRRN8        SCDBRR
S09  C     VIEW#         WHENEQ    9
     C                   Z-ADD     DBRRN9        SCDBRR
E01  C                   ENDSL
     C                   MOVE      *BLANKS       SCSEL
     C                   EXSR      MVDBSC
     C                   WRITE     SCR01S
     C                   ENDSR
     C*================================================================
     C*  OPT1   - Process option 1; select item
     C*================================================================
     C     OPT1          BEGSR
     C                   MOVE      *ON           SL1FLG
     C                   EXSR      SLCTRD
     C                   ENDSR
     C*================================================================
     C*  OPT2   - Process option 2; change item
     C*================================================================
     C     OPT2          BEGSR
     C                   MOVE      'C'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR
     C*================================================================
     C*  OPT3   - Process option 3; copy item
     C*================================================================
     C     OPT3          BEGSR
     C                   MOVE      'P'           ##MODE
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
B01  C     *IN95         IFEQ      *OFF
     C                   Z-ADD     LASTRR        SFKEY
     C                   EXSR      CHKSFB
     C                   Z-ADD     LASTRR        SFRRN1
     C                   EXSR      SETGT
B02  C     *IN96         IFEQ      *ON
     C                   SETON                                        9091
E02  C                   ENDIF
     C                   EXSR      READ
B02  C     *IN95         IFEQ      *ON
     C                   SETON                                        9091
     C                   SETOFF                                       95
X02  C                   ELSE
     C                   EXSR      GETRDS
E02  C                   ENDIF
X01  C                   ELSE
     C                   SETON                                        9091
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  PGUPDO - Process page up; get page for subfile
     C*================================================================
     C     PGUPDO        BEGSR
     C                   MOVE      *IN95         SV95              1
     C                   Z-ADD     1             SFKEY
     C                   EXSR      CHKSFF
B01  C     SNGITM        IFEQ      *OFF
     C                   EXSR      SETLL
     C                   EXSR      READP
E01  C                   ENDIF
B01  C     *IN95         IFEQ      *ON
     C                   SETON                                        9091
     C   04              READC     SCR01S                                 99
B02  C     SNGITM        IFEQ      *OFF
     C*                    SETOF                     95
     C                   MOVE      SV95          *IN95
E02  C                   ENDIF
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
     C*  READCK - Checks the number of reads; calls SRRDCCKR if necess
     C*================================================================
     C     READCK        BEGSR
     C     @READC        DIV       @INCC         @READR            5 0
     C                   MVR                     @READR
B01  C     @READR        IFEQ      0
     C                   CALL      'SRRDCKR'                            99
     C                   PARM                    @READS
     C                   PARM                    @RCDS
     C                   PARM                    @CONT             1
B02  C     @CONT         IFEQ      '1'
     C                   SETON                                        95
E02  C                   ENDIF
B02  C     @CONT         IFEQ      '2'
     C                   SUB       @READC        @READC
E02  C                   ENDIF
B02  C     @CONT         IFEQ      '3'
     C                   SUB       @READC        @READC
     C     @INC          MULT      3             @INCC
E02  C                   ENDIF
B02  C     @CONT         IFEQ      '4'
     C                   SUB       @READC        @READC
     C     @INC          MULT      5             @INCC
E02  C                   ENDIF
B02  C     @CONT         IFEQ      '5'
     C                   SUB       @READC        @READC
     C                   MOVEL     *HIVAL        @INCC
E02  C                   ENDIF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  REFRSR - Refreshes the item in the subfile from data base
     C*================================================================
     C     REFRSR        BEGSR
B01  C     *INKC         IFEQ      *OFF
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
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '0000'        *IN(86)
     C                   MOVEA     '00'          *IN(92)
     C                   READC     SCR01S                                 99
B01  C     *IN99         DOWEQ     *OFF
     C                   SETOFF                                       050708
B02  C     SCSEL         IFNE      *BLANKS
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      CHKERR
B04  C     *IN90         IFEQ      *OFF
      ***** BEG ADD ****** 06/17/03 **********************************
     C     SCDBRR        CHAIN     PFRCDF                             99
      ***** END ADD ****** 06/17/03 **********************************
     C                   EXSR      ERRCK1
B05  C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '1'
     C                   ADD       1             COUNT1
E05  C                   ENDIF
E04  C                   ENDIF
B04  C     COUNT1        IFGT      1
     C                   SETON                                        9092
E04  C                   ENDIF
B04  C     *IN90         IFEQ      *ON
     C                   SETON                                        0507
     C                   Z-ADD     SFRRN1        SCRRN1
E04  C                   ENDIF
E03  C                   ENDIF
     C                   SETON                                        08
E02  C                   ENDIF
     C                   UPDATE    SCR01S
     C                   SETOFF                                       050708
     C                   READC     SCR01S                                 99
E01  C                   ENDDO
     C                   ENDSR
     C*================================================================
     C*  SF1EXE - Process subfile changes
     C*================================================================
     C     SF1EXE        BEGSR
     C                   READC     SCR01S                                 99
B01  C     *IN99         DOWEQ     *OFF
     C                   SETOFF                                       0708
B02  C     SCSEL         IFNE      *BLANKS
      ***** BEG ADD ****** 05/11/04 **********************************
     C     SCDBRR        CHAIN     PFRCDF                             99
      ***** END ADD ****** 05/11/04 **********************************
     C                   ADD       1             SCOUNT
     C     SCSEL         CASEQ     '1'           OPT1
     C     SCSEL         CASEQ     '2'           OPT2
     C     SCSEL         CASEQ     '3'           OPT3
     C     SCSEL         CASEQ     '4'           OPT4
     C     SCSEL         CASEQ     '5'           OPT5
     C                   CAS                     OTHOPT
     C                   ENDCS
     C                   SETON                                        08
B03  C     SCSEL         IFNE      '4'
     C                   MOVE      *BLANKS       SCSEL
     C                   Z-ADD     SFRRN1        SCRRN1
     C                   EXSR      REFRSR
E03  C                   ENDIF
E02  C                   ENDIF
     C                   UPDATE    SCR01S
     C  NKC
     CANNKL              READC     SCR01S                                 99
     C  NKC
E01  CANNKL              ENDDO
     C                   ENDSR
     C*================================================================
     C*  SNDSSM - Send subsetted list message if necessary
     C*================================================================
     C     SNDSSM        BEGSR
     C                   MOVE      *OFF          *IN11
     C                   EXSR      CHKSSM
B01  C     *IN11         IFEQ      *ON
     C                   CALL      'SRPGMMSG'                           99
     C                   PARM      SLMSGI        ##MSGI            7
     C                   PARM      SLMSGF        ##MSGF           10
     C                   PARM      *BLANKS       ##MSGL           10
E01  C                   ENDIF
     C                   ENDSR
     C*===============================================================
     C* UPDOFA - Update values in the option/function array
     C*===============================================================
     C*  Before calling this subroutine make the following assignments:
     C*       XXOPFN = '1' if Option, '2' if Function
     C*       XXNMBR = Option/Function number to change
     C*            NOTE: Pass option/function numbers as 2 char fields.
     C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
     C*       XXMIS = 'XXX' where each 'X' is Y or N depending on change rqrd
     C*
     C*  This subroutine when called evaluates array OF for XXOPFN and checks
     C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
     C*  replaces 'XXX' with XXMIS.
     C*
     C     UPDOFA        BEGSR
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C*
B01  C     FOUND         DOWEQ     *OFF
     C     OF(I)         ANDNE     '3'
     C*
B02  C     OF(I)         IFEQ      XXOPFN
     C                   MOVEL     OFD(I)        DUMNUM            2
B03  C     XXNMBR        IFEQ      DUMNUM
     C                   MOVEL(P)  OFD(I)        DUMOFD           30
     C                   MOVE      XXMIS         DUMOFD
     C                   MOVEL     DUMOFD        OFD(I)
     C                   MOVE      *ON           FOUND
E03  C                   ENDIF                                                  ZZNUM=DUMNUM
E02  C                   ENDIF                                                  OF,I=XXOPFN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
E01  C                   ENDDO                                                  FOUND DOWEQ *OFF
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
