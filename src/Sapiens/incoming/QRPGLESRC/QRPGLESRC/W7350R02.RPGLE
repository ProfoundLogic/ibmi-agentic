     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7350R02 - STATISTICAL SUBMISSION UPDATE/DISPLAY
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7350R                                          *
     H*                                                               *
     H*  CALLS TO: W7350C                                             *
     H*            W0010R (FUND SEARCH)                               *
     H*                                                               *
     H*  CREATED: 05/19/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/05/11 C:190 SY255 Priss# 79                               *
     H*                Change title and month end message when run   *
     H*                in financial mode                             *
     H*                                                              *
     H* 11/23/10 C:189 Project: 139WPQ                                *
     H*                Add new program to mode to call WCSTAT prompt  *
     H*                display effective month/year range instead of  *
     H*                reporting month/year range and to not display  *
     H*                the reporting level field.                     *
     H*                                                               *
     H* 04/07/09 C:65  Project: 139WKB                                *
     H*                REMOVE THE GROUP RANGE AND INSTEAD CALL THE    *
     H*                GROUP SELECTION PROMPT SCREEN.                 *
     H*                                                               *
     H* 01/08/08 C:15  Project: 139WIK                                *
     H*                Added group range and report month/year range. *
     H*                                                               *
     H* 11/26/07 C:190 Project: WC1590                                *
     H*                Write a record to WCSTAT Deleted Losses File   *
     H*                for every record that is deleted from WDSLP    *
     H*                                                               *
     H* 09/26/07 C:15  Project: 114WNI                                *
     H*                Added code to delete the ICR records when an   *
     H*                extract in deleted.                            *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7350F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMSSP     UF A E             DISK
     F                                     RENAME(PSS01:DBRCDF)
     F*
     F*  Logical file
     FWMSSL     IF   E           K DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDB)
     F*** BEGIN ADD ***  04/07/09  ************************************
     FWXPGL     IF   E           K DISK    USROPN
     F*** END ADD *****  04/07/09  ************************************
     F*
     F*  Link, etc.  file
     FWMSRL     UF   E           K DISK
     F*
     F*  Exposure file
     FWDSEL     UF   E           K DISK
     F*
     F*  Loss file
     FWDSLL     UF   E           K DISK
     F*
     F*  Unit total file
     FWMSQL     UF   E           K DISK
     F*
     F*** Begin add *** 09/26/07 **************************************
     F*  ICR file
     FWDSIL     UF   E           K DISK
     F*** Begin add *** 09/26/07 **************************************
     F*** Begin add *** 11/26/07 **************************************
     F*  Deleted Loss file
     FWDSLPX    O    E           K DISK
     F                                     RENAME(PSL01:PSL0X)
     F*** End   add *** 11/26/07 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D FNFDB           DS
     D  FNRRN                397    400B 0
     D*** Begin add *** 01/08/08 **************************************
     D                 DS
     D  XXRPMY                 1      5  0
     D  XXCYY                  1      3  0
     D  XXMM                   4      5  0
     D*** End add ***** 01/08/08 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** beg chg ***** 07/05/11 ******************************************
     D*******       'Statistical Submissi-C         OPTNM
     D*******       'on'
     D OPTNM1          C                   CONST('Statistical Submissi-
     D                                     on')
     D OPTNM2          C                   CONST('Financial Data Files')
     D*** end chg ***** 07/05/11 ******************************************
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D*** beg add ***** 11/23/10 ******************************************
     D XXPPGM          C                   CONST('W7350R02B')

     I*** end add ***** 11/23/10 ******************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
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
     C     FNKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKFND             3 0
     C*
     C     SSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY            5 0
     C*
     C*  Key list for general use (i.e. deleting all records assoc w/an extract)
     C     KEYX          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY            5 0
     C*** BEGIN ADD ***  04/07/09  ************************************
     C     PGKY          KLIST
     C                   KFLD                    KKCO#
     C*** END ADD *****  04/07/09  ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C                   Z-ADD     SSCO#         KKCO#
     C                   Z-ADD     SSFND         KKFND
     C                   Z-ADD     SSRPMY        KKRPMY
     C*
     C*  Delete Link,header,name,address record
     C     KEYX          CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PSR01
     C     KEYX          READE     PSR01                                  60
     C                   ENDDO
     C*
     C*  Delete Exposure records
     C     KEYX          CHAIN     PSE01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PSE01
     C     KEYX          READE     PSE01                                  60
     C                   ENDDO
     C*
     C*  Delete Loss records
     C     KEYX          CHAIN     PSL01                              60
     C     *IN60         DOWEQ     *OFF
     C*** Begin add *** 11/26/07 **************************************
     C                   WRITE     PSL0X
     C*** End   add *** 11/26/07 **************************************
     C                   DELETE    PSL01
     C     KEYX          READE     PSL01                                  60
     C                   ENDDO
     C*
     C*  Delete Unit total record
     C     KEYX          CHAIN     PSQ01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PSQ01
     C     KEYX          READE     PSQ01                                  60
     C                   ENDDO
     C*
     C*** Begin add *** 09/26/07 **************************************
     C*  Delete ICR records
     C     KEYX          CHAIN     PSI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PSI01
     C     KEYX          READE     PSI01                                  60
     C                   ENDDO
     C*** Begin add *** 09/26/07 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEGIN DEL ***  04/07/09  ************************************
     C********             Z-ADDSCFND     KKFND
     C********   FNKY      CHAINPFN01                9081
     C*
     C*** Begin add *** 01/08/08 **************************************
     C********   *IN90     IFEQ *OFF
     C********             Z-ADDSCTFND    KKFND
     C********   FNKY      CHAINPFN01                9085
     C********             ENDIF
     C*
     C********   *IN90     IFEQ *OFF
     C********   SCFND     ANDGTSCTFND
     C********             SETON                     9086
     C********             ENDIF
     C*** End add ***** 01/08/08 **************************************
     C*** END DEL *****  04/07/09  ************************************
     C*
B01  C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
     C     SRERR         IFEQ      *ON
     C                   SETON                                        82  90
     C                   ENDIF
     C*
     C*** Begin add *** 01/08/08 **************************************
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCTRMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCTRMY
     C     SRERR         IFEQ      *ON
     C                   SETON                                        87  90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPMY
     C*
     C                   Z-ADD     SCTRMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXTRMY            5 0
     C*
     C     *IN90         IFEQ      *OFF
     C     XXRPMY        ANDGT     XXTRMY
     C                   SETON                                        9089
     C                   ENDIF
     C*** End add ***** 01/08/08 **************************************
     c*** beg add ***** 11/23/10 ******************************************
     C                   MOVEL(P)  *BLANKS       SCFNAM           10
     C  N90##MODE        IFEQ      'F'
     C                   CALL      'SRCHKOB2'
     C                   PARM      SCLIB         ##LIB            10
     C                   PARM      SCFNAM        ##FIL            10
     C                   PARM      '*FILE'       ##TYP             7
     C                   PARM      *BLANK        ##ERR             1
      *
     C     ##ERR         IFEQ      '1'
     C                   SETON                                        68  90
     C                   ENDIF                                                  ##ERR
     C                   ENDIF
     c*** end add ***** 11/23/10 ******************************************
     C*
     c*** beg add ***** 11/23/10 ******************************************
     C  N90SCRPTL        IFLE      *ZERO
     C     ##MODE        ANDNE     'F'
     C**N90****  SCRPTL    IFLE *ZERO
     c*** END add ***** 11/23/10 ******************************************
     C                   SETON                                        84  90
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  04/07/09  ************************************
B02  C********   *IN90     IFEQ *OFF
     C********   @@MODE    ANDEQ'A'
     C*** Begin add *** 01/08/08 **************************************
     C********   SCFND     ANDEQSCTFND
     C*** End add ***** 01/08/08 **************************************
     C*
     C********             Z-ADDSCRPMY    SRDTI
     C********             CALL 'SRCVTMY' DTCYMD
     C********             MOVELSRDTO     KKRPMY
     C*
     C********   SSKY      CHAINPSS01                60
     C********   *IN60     IFEQ *OFF
     C********             SETON                     83  90
     C********             ENDIF
     C*
E02  C********             ENDIF                           *IN90=*OFF
     C*** END DEL *****  04/07/09  ************************************
     C*
E01  C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - Subroutine for F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*   FUND WORK WITH
     C*
     C     HFLD          IFEQ      'SCFND'
     C                   MOVEL     ##CO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C                   MOVE      @@FND         SCFND
     C*
     C     *INKC         IFEQ      *ON
     C                   RETURN
     C                   ENDIF
     C*
     C     SCFND         IFNE      *ZERO
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** Begin add *** 01/08/08 **************************************
     C     HFLD          IFEQ      'SCTFND'
     C                   MOVEL     ##CO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C                   MOVE      @@FND         SCTFND
     C*
     C     *INKC         IFEQ      *ON
     C                   RETURN
     C                   ENDIF
     C*
     C     SCTFND        IFNE      *ZERO
     C                   MOVE      *ON           *IN88
     C                   ELSE
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C                   ENDIF
     C*** End add ***** 01/08/08 **************************************
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* KFDO - Process F6=Add
     C*================================================================
     C     KFDO          BEGSR
     C*
     C                   MOVE      *ON           *INKL
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCRPMY
     C                   CLEAR                   SCRPTL
     C*
     C     @@MODE        IFEQ      'C'
     C                   MOVE      SSFND         SCFND
     C     190000        ADD       SSRPMY        SCRPMY
     C     10000.01      MULT      SCRPMY        SCRPMY
     C                   Z-ADD     1             SCRPTL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     ##CO#         SSCO#
     C*** BEGIN DEL ***  04/07/09  ************************************
     C*********            Z-ADDSCFND     SSFND
     C*** END DEL *****  04/07/09  ************************************
     C                   Z-ADD     SCRPTL        SSRPTL
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSSBDT
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         SSRPMY
     C*
     C*** Begin add *** 01/08/08 **************************************
     C*  Process all groups within the selected range.
     C*
     C*** BEGIN CHG ***  04/07/09  ************************************
     C*******              Z-ADDSCFND     KKFND
     C*******    KKFND     DOWLESCTFND
     C*
     C*******    FNKY      CHAINPFN01                99
     C*******    *IN99     IFEQ *ON
     C*******              GOTO NEXT1
     C*******              ENDIF
     C*
     c*** beg add ***** 11/23/10 ******************************************
     C                   MOVE      XXRPMY        QQRPMY            5
     C                   MOVE      XXTRMY        QQTRMY            5
     C                   MOVEL     SCLIB         QQLIB            10
     C     ##MODE        IFEQ      'F'
     C                   CALL      'W7353C5'
     C                   PARM                    QQRPMY
     C                   PARM                    QQTRMY
     C                   PARM      '1'           Q#RPTL            1
     C                   PARM                    QQLIB
     C                   PARM                    ##MODE
     C                   ELSE
     c*** end add ***** 11/23/10 ******************************************
     C                   OPEN      WXPGL
     C*
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PGFLAG        IFEQ      'I'
     C                   Z-ADD     PGGRP         KKFND
     C     FNKY          CHAIN     PFN01                              99
     C*** END CHG *****  04/07/09  ************************************
     C*
     C*  Process all selected reporting levels for group.
     C*
     C     XXRPMY        DOWLE     XXTRMY
     C*
     C*  If extract has already been processed, do not process again.
     C*
     C                   Z-ADD     XXRPMY        KKRPMY
     C     SSKY          CHAIN     PSS01                              99
     C     *IN99         IFEQ      *OFF
     C                   GOTO      NEXT2
     C                   ENDIF
     C*
     C                   Z-ADD     KKFND         SSFND
     C                   Z-ADD     XXRPMY        SSRPMY
     C*** End add ***** 01/08/08 **************************************
     C*
     C*
     C*  Call to W7350C to submit job to create files for transmission
     C                   MOVE      FNRRN         QQRRN             9
     C                   MOVE      SSRPMY        QQRPMY            5
     C                   MOVE      SSRPTL        QQRPTL            2
     C*
     C                   CALL      'W7350C'
     C                   PARM                    QQRRN
     C                   PARM                    QQRPMY
     C                   PARM                    QQRPTL
     C*
     C*** Begin add *** 01/08/08 **************************************
     C*  Write Extract Control record.
     C*
     C                   MOVEL     Q1USER        SSUSER
     C                   MOVEL     Q1USER        SSIUSR
     C                   MOVEL     Q1PGM         SSPGM
     C                   Z-ADD     DSTIME        SSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSDT
     C                   MOVEL     SRDTO         SSIDT
     C                   MOVE      'Y'           SSXTRC
     C*
     C                   WRITE     DBRCDF
     C*
     C*  Add one to process next reporting month/year.
     C*
     C     NEXT2         TAG
     C                   ADD       1             XXRPMY
     C     XXMM          IFEQ      13
     C                   ADD       1             XXCYY
     C                   Z-ADD     1             XXMM
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPMY
     C*
     C*** BEGIN CHG ***  04/07/09  ************************************
     C*******    KKFND     IFEQ 999
     C*******              LEAVE
     C*******              ENDIF
     C*
     C*  Add one to process next group.
     C*
     C*******    NEXT1     TAG
     C*******              ADD  1         KKFND
     C*******              ENDDO
     C                   ENDIF                                                  PGFLAG IFEQ
     C*
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   CLOSE     WXPGL
     C*** END CHG *****  04/07/09  ************************************
     C*
     C                   FEOD      WMSSP
     C*** End add ***** 01/08/08 **************************************
     c*** beg add ***** 11/23/10 ******************************************
     C                   ENDIF
     c*** end add ***** 11/23/10 ******************************************
     C*
     C*  Set outflg for return to main screen
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     c*** beg add ***** 07/05/11 ******************************************
     C                   MOVE      *BLANKS       OPTNM            40
     c*** end add ***** 07/05/11 ******************************************
     c*** beg add ***** 11/23/10 ******************************************
     C                   MOVE      @@MODE        ##MODE            1
     C     @@MODE        IFEQ      'F'
     C     @@RRN         ANDEQ     *ZEROS
     C                   MOVE      'A'           @@MODE
     C                   ENDIF
     C     @@MODE        IFEQ      'F'
     C     @@RRN         ANDNE     *ZEROS
     C                   MOVE      'C'           @@MODE
     C                   ENDIF
     C     ##MODE        IFEQ      'F'
     c*** beg add ***** 07/05/11 ******************************************
     C                   MOVEL     OPTNM2        OPTNM            40
     c*** end add ***** 07/05/11 ******************************************
     C                   SETON                                        69
     C                   ELSE
     c*** beg add ***** 07/05/11 ******************************************
     C                   MOVEL     OPTNM1        OPTNM            40
     c*** end add ***** 07/05/11 ******************************************
     C                   SETOFF                                       69
     C                   ENDIF
     c*** end add ***** 11/23/10 ******************************************
     c*** beg add ***** 07/05/11 ******************************************
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL     ##DATA        SCPRDS
     c*** end add ***** 07/05/11 ******************************************
     C*
     C*** BEGIN ADD ***  04/07/09  ************************************
     C     @@MODE        IFNE      'E'
     c*** beg add ***** 11/23/10 ******************************************
     C     ##MODE        ANDNE     'F'
     c*** end add ***** 11/23/10 ******************************************
      *
     C                   MOVE      ##CO#         @@CO#
     c*** beg chg ***** 11/23/10 ******************************************
     C********             CALL 'W7417R2'
     C********             PARM           @@CO#
     C********             PARM 'S'       #1CALL  1
     C********   *INKC     PARM '0'       ##KC    1
     C********   *INKL     PARM '0'       ##KL    1
     C                   CALL      'W7944C'
     C                   PARM      XXPPGM        @@PGM            10
     c*** End chg ***** 11/23/10 ******************************************
      *
     c*** beg chg ***** 11/23/10 ******************************************
     C********   *INKC     IFEQ *ON
     C*******    *INKL     OREQ *ON
     C     @@PGM         IFEQ      '*ON'
     c*** END chg ***** 11/23/10 ******************************************
     C                   RETURN
     C                   ENDIF
      *
     C                   ENDIF
     C*** END ADD *****  04/07/09  ************************************
     C*
     C                   MOVE      ##CO#         KKCO#
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SSUSER
     C                   MOVEL     Q1PGM         SSPGM
     C                   Z-ADD     DSTIME        SSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C*** Begin dlt *** 01/08/08 **************************************
     C*********            MOVELQ1USER    SSUSER
     C*********            MOVELQ1USER    SSIUSR
     C*********            MOVELQ1PGM     SSPGM
     C*********            Z-ADDDSTIME    SSTIME
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     SSDT
     C*********            MOVELSRDTO     SSIDT
     C*********            MOVE 'Y'       SSXTRC
     C*********
     C*********            WRITEDBRCDF
     C*********            FEOD WMSSP
     C*** End dlt ***** 01/08/08 **************************************
     C*
     C                   ENDSR
