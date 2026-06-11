     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/24/06 C: 35 PROJECT: SUPPORT WA132/#53796                  *
     H*                         CLEAR SCDES1 FIELDS SO THAT F12       *
     H*                         CAUSES PROGRAM TO END WITHOUT CALLING *
     H*                         WRTPEN SUBROUTING AND GENERATING      *
     H*                         ENDORSEMENT.                          *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 08/14/00 C:90  PROJECT: WA081/23923                           *
     H*                PRINT MISCELLANEOUS ENDORSEMENT IF DESCRIPTION *
     H*                IS NOT BLANK INSTEAD OF BEFORE/AFTER CHANGES.  *
     H*                                                               *
     H* 07/30/99 C:113 PROJECT: SUPPORT WA093/#19054                  *
     H*                @@KL was being set on in the entry parms.      *
     H*                This was causing the endorsement to not be     *
     H*                generated.  Changed to not set this on.        *
     H*                                                               *
     H* 03/26/99 C:113 PROJECT: SUPPORT 17313                         *
     H*                Load federal ID number.                        *
     H*                                                               *
     H* 02/05/99  C:09   PROJECT: CM027                               *
     H*                  MISC ENDORSEMENT NEEDS TO ALLOW FOR UPPER AND*
     H*                  LOWER CASE ENTRY ON BEFORE/AFTER LINES.      *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H*****************************************************************
     FW0051F17  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMEMP     IF   E             DISK
     FWDELP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     FWMDXL     IF   E           K DISK
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMENP     IF   E             DISK
     F                                     RENAME(PEN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMENL3    IF A E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEN01:DBRCDF)
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
      /COPY SCOPYWW02D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW02E
     D*
     D***PY SCOPYWW02I
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Misc Employer Endors-
     D                                     ements')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
      *** BEGIN CHG *** 07/30/99 ***********************************************
     C**********           PARM '1'       @@KL    1
     C                   PARM                    @@KL              1
      *** BEGIN CHG *** 07/30/99 ***********************************************
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@EEDT            8 0          MMDDCCYY
     C*
     C*  ADD/CHG/DEL PLIST
     C*
     C     ACDPL         PLIST
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKEMP#
     C                   PARM                    KKDIV
     C                   PARM                    KKFYR
     C     SCAGT#        PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD
     C                   PARM                    ##PNAM           60
     C*
     C*  Compress the employer header information parameter list
     C*
     C     CMPRPL        PLIST
     C                   PARM      EMEMP#        KKEMP#
     C                   PARM      EMDIV         KKDIV
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    KKCO#             3 0
     C                   PARM                    KKFND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      **** END   ADD **** 10/14/98 *********************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    ENEMP#        KKEMP#
     C     *LIKE         DEFINE    ENDIV         KKDIV
     C     *LIKE         DEFINE    ENFYR         KKFYR
     C     *LIKE         DEFINE    ENREQ#        XXREQ#
     C     *LIKE         DEFINE    ENSEQ#        XXSEQ#
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C*
     C**/COPY SCOPYWW02C
?????C*
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
     C                   Z-ADD     1             SCRRN2
     C                   WRITE     SCR02
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
      **** BEGIN ADD **** 07/24/06 *********************************************
     C                   MOVE      *BLANKS       SCDES1
      **** END   ADD **** 07/24/06 *********************************************
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
     C                   SETON                                        08
     C                   UPDATE    SCR01S
     C                   EXSR      KEYSFL
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
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '0000'        *IN(86)
     C                   MOVEA     '00'          *IN(92)
     C                   READC     SCR01S                                 99
B01  C     *IN99         DOWEQ     *OFF
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
E02  C                   ENDIF
     C                   SETON                                        08
     C                   UPDATE    SCR01S
     C                   READC     SCR01S                                 99
E01  C                   ENDDO
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
?????C*
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCKA - Used to determine errors for the entire subfile
     C*================================================================
     C     ERRCKA        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCKC - Check fro Subfile Control Errors
     C*================================================================
     C     ERRCKC        BEGSR
     C*
     C*  ENDORSEMENT DESCRIPTION CANNOT BE BLANK
     C*
     C     SCDES1        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
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
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     *BLANKS       SCBFOR
     C                   MOVEL     *BLANKS       SCAFTR
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
     C*
     C                   Z-ADD     1             XXRRN             4 0
     C     XXRRN         CHAIN     SCR01S                             99
B01  C     *IN99         DOWEQ     *OFF
     C*
      **** BEGIN CHG **** 08/14/00 ************************************
     C********** SCBFOR    IFNE *BLANKS
     C********** SCAFTR    ORNE *BLANKS
     C     SCDES1        IFNE      *BLANKS
      **** END   CHG **** 08/14/00 ************************************
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C                   ADD       1             XXRRN
     C     XXRRN         CHAIN     SCR01S                             99
E01  C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL     *BLANKS       @@CALL            1
     C                   MOVEL     *BLANKS       @@MODE            1
     C                   MOVEL     *BLANKS       ##MODE            1
     C                   MOVEL     *BLANKS       SCUSER           10
     C                   MOVE      *ON           PEN1ST            1
     C                   MOVE      @@EEDT        SCEEDT
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN1        CHAIN     PEL01                              60
     C*
     C                   MOVE      ELCO#         KKCO#
     C                   MOVE      ELFND         KKFND
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELFYR         KKFYR
     C                   MOVE      ELEMP#        KKEMP#
     C                   MOVE      ELDIV         KKDIV
     C*
     C     FNKY          CHAIN     PFN01                              99
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              99
      **** END   ADD **** 10/14/98 *********************************************
     C                   MOVEL     FNNAME        ##FNAM
     C*
     C                   MOVEL     ##FNAM        SCNAME
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      CMPRES        CMPRPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C                   MOVE      *OFF          *IN58                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN58                          CANCELLED
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   MOVE      *ON           *IN95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   MOVE      *ON           *IN95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Used to update data base record
     C*================================================================
     C     UPDATR        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRITER - Used to write data base record
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRTPEN - Used to write data base records at once
     C*================================================================
     C     WRTPEN        BEGSR
     C*
     C     PEN1ST        IFEQ      *ON
     C                   MOVE      *OFF          PEN1ST
     C     KEY1          SETGT     DBRCDF
     C     KEY1          READPE    DBRCDF                                 60
     C   60              Z-ADD     *ZERO         XXREQ#
     C                   Z-ADD     *ZERO         XXSEQ#
     C                   ENDIF
     C*
     C                   MOVE      ELCO#         ENCO#
     C                   MOVE      ELFND         ENFND
     C                   MOVE      ELEMP#        ENEMP#
     C                   MOVE      ELDIV         ENDIV
     C                   MOVE      ELFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   Z-ADD     XXREQ#        ENREQ#
     C                   Z-ADD     XXSEQ#        ENSEQ#
     C*
     C                   MOVE      @@EEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C*** BEGIN ADD *** 03/26/99 **************************************
     C                   MOVE      EMFEIN        ENFEN1
     C*** END   ADD *** 03/26/99 **************************************
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C*
     C                   MOVEL     DXMISC        ENRMCD
     C                   MOVE      'Y'           ENMSFL
     C*
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C                   MOVEL     EMCNTY        ENCNT1
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C                   MOVEL     SCDES1        ENDSC1
     C                   MOVEL     SCDES2        ENDSC2
     C*
     C                   MOVEL     SCBFOR        ENBLIN
     C                   MOVEL     SCAFTR        ENALIN
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
      **** END   ADD **** 11/23/98 *********************************************
     C*
     C                   WRITE     DBRCDF
     C                   ADD       1             XXSEQ#
     C*
     C                   ENDSR
     C*================================================================
** Column headings
Before the Change:                      After the Change:
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
