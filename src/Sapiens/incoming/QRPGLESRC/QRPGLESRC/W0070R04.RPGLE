     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H/TITLE W0070R04 - CLAIM DELETION PROCESSING PROGRAM
     H*****************************************************************
     H* DESCRIPTION:  THIS PGM IS CALLED BY THE CLAIM DELETION        *
     H*    PROCESS IN W0070R.  THIS PGM PROMPTS THE USER TO ENTER AN  *
     H*    EXPLANATION FOR THE DELETION OF THE CLAIM, THEN WRITES     *
     H*    RECORDS TO THE CLAIM DELETION FILES, AND FINALLY, IT       *
     H*    DELETES THE RECORDS FROM THE APPROPRIATE CLAIM FILES.      *
     H*                                                               *
     H* CALLED FROM: W0070C01                                         *
     H* CALLS TO:    NONE                                             *
     H* CREATED BY:  C15       1/15/97       PROJECT: 60ANC           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/13/15 C:302 Project: WC2339                                *
     H*                When a claim is deleted we need to move any    *
     H*                images attached to the claim back to the       *
     H*                queue and ready to be re-index                 *
     H*                                                               *
     H* 12/01/14 C:279 Project: 114WLF                                *
     H*                Delete reserve worksheet history record on     *
     H*                claim deletion.                                *
     H*                                                               *
     H* 06/24/13 C:189 Redmine: 3190                                  *
     H*                Edit appearing even for a valid value. Company *
     H*                number not intialized properly.                *
     H*                                                               *
     H* 04/05/10 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                  PROJECT RE-JAD , RE-DESIGNED                 *
     H*                                                               *
     H* 08/22/07 C:15  PROJECT 114WMF                                 *
     H*                02/19/01 C:61  PROJECT 114WBC                  *
     H*                Delete Claim Trial Information Records (WMTIP) *
     H*                                                               *
     H* 12/10/03 C:15  Project: WC1172                                *
     H*                Added code to load the Tot Inc and Rsv chg amt *
     H*                into WACTP for deleted claims.                 *
     H*                                                               *
     H* 08/29/03 C:53 Support: WC072/38712                            *
     H*               Added code to delete rcds and create a          *
     H*               cancelation rcd if needed for 148 when a        *
     H*               claim is deleted.                               *
     H*                                                               *
     H* 01/04/01 C:15 Support: WC065/25696                            *
     H*               Added code to delete A49 log records when a     *
     H*               claim is deleted.                               *
     H*                                                               *
     H* 08/22/00 C:53  Support: WC065/23895                           *
     H*               -Corrected date conversion issue on report      *
     H*                 changed plist from DTMDY TO DTMDYY            *
     H*               -Corrected heading issue on report.             *
     H*                                                               *
     H* 07/19/00 C:15  Support: WC065/23557                           *
     H*               -Fixed code which deletes the pmt records.      *
     H*               -Removed pmt rqst override window routine.      *
     H*                                                               *
     H* 05/18/00 C:53  PROJECT: WC836                                 *
     H*                Add population of CTTRDT                       *
     H*                                                               *
     H* 01/29/99 C:112 PROJECT: WC827                               *
     H*            Allow for F12=Cancel if user changes decision to   *
     H*            to delete a record.  Return user to previous       *
     H*            information entry screen.                          *
     H*                                                               *
     H* 12/04/98 C:94 PROJECT: SUP WC087/15351                        *
     H*               CHANGED LENGTH OF PARM                          *
     H*                                                               *
     H* 10/12/98 C:94 PROJECT: SUPPORT                                *
     H*               CONDITION DELETION OF WCMC ON IF THEY HAVE IT   *
     H*               INSTALLED                                       *
     H*                                                               *
     H* 09/16/98 C:94 PROJECT: MC151                                  *
     H*               ADDED DELETION OF WCMC RECORDS IF LINKED        *
     H*                                                               *
     H* 07/06/98 C:65 PROJECT WC811                                   *
     H*               CALL W8024R TO DELETE A/R FOR DEDUCTIBLE BILLED.*
     H*                                                               *
     H* 05/15/97 C:62 PROJECT WC640                                   *
     H*               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;          *
     H*               "ADJUSTER" TO CXADJ.                            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* 71-89 These indicators are to used as error indicators for    *
     H*       the various screens. They will set off after            *
     H*       displaying each input capible screen.                   *
     H*  90 - Used as general purpose error indicator                 *
     H*  99 - General purpose indiactor (CHAIN, READ, LOKUP...)       *
     H*****************************************************************
     F*
     FW0070F04  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     FWMCML9    IF   E           K DISK
     F                                     RENAME(PCM01:PCM09)
     FSSYLF     IF   E           K DISK
     FWMCDL     UF   E           K DISK
     FWMLLL     IF   E           K DISK
     FWMRHL     UF   E           K DISK
     FWMCHL     UF   E           K DISK
     FWMTHL     UF   E           K DISK
     FWMWHL     UF   E           K DISK
     FWMDDL     UF   E           K DISK
     FWMBYL     UF   E           K DISK
     FWDPAL3    UF   E           K DISK
     FWARPL     UF   E           K DISK
     FWDDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSMARL     UF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWACML     O    E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWACTP     O    E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*** BEGIN ADD *** 05/15/97 **************************************
     FWMCXL     IF   E           K DISK
     F*** END   ADD *** 05/15/97 **************************************
     F*** BEGIN ADD *** 01/04/01 **************************************
     F*** BEGIN CHG *** 08/29/03 **************************************
     F*WMLGL***UF**E           K        DISK
     FWMLGL     UF A E           K DISK
     F*** END CHG *** 08/29/03 **************************************
     F*** END ADD ***** 01/04/01 **************************************
     F*** Begin add *** 08/22/07 **************************************
     FWMTIL     UF   E           K DISK
     F*** End add ***** 08/22/07 **************************************
     F*** BEG ADD ***** 04/05/10 **************************************
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
     FWMIXL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F*** END ADD ***** 04/05/10 **************************************
      ****** beg add ****** 12/01/14 **********************************
     fwdrwl     uf   e           k disk    rename(prw01:prw00)
      ****** beg add ****** 12/01/14 **********************************
      ****** beg add ****** 08/13/15 **********************************
     fimdolc    uf   e           k disk    usropn
     fimiel5    uf   e           k disk    usropn
      ****** beg add ****** 08/13/15 **********************************

     D*
     D REQ             S              1    DIM(12)
     D AMT             S              1    DIM(9) ASCEND
     D*** Begin add *** 12/10/03 **************************************
     D*** End add ***** 12/10/03 **************************************
     D*** BEGIN ADD *** 05/15/97 **************************************
     D*
     D* CLAIM EXAMINER HEADING AND CATEGORY 2 HEADING
     D*
     D XXCAT2          C                   CONST('CATEGORY 2')
     D XXADJ           C                   CONST('CLAIMS EXAMINER')
     D*
     D MSG1            C                   CONST('*** DELETION ***')
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3
     D  QCFND                  4      6
     D  QCFYR                  7      9
     D  QCCASE                10     16
     D                 DS
     D  XXAMTS                 1      9  2
     D  XXAMTC                 1      9
     D*** Begin add *** 12/10/03 **************************************
     D                 DS
     D  RRT                    1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** End add ***** 12/10/03 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D* --------------------------------------------------
     D* Prototype for procedure: ImgDetach
     D* --------------------------------------------------
     D ImgDetach       PR


     I*** END   ADD *** 05/15/97 **************************************
     I*
     IPCM03
     I              CMKEY                       C3KEY
     I              CMSEQ#                      C3SEQ#
     I              CMSTAT                      C3STAT
     I              CMPGM                       C3PGM
     I              CMTIME                      C3TIME
     I              CMDT                        C3DT
     I              CMDCDT                      C3DCDT
     I              CMDIFL                      C3DIFL

     C*
     C*  KEY LIST FOR THE DARTS ADDITIONAL REMARKS FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    C3SEQ#
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKY9         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XASS#
     C*
     C*** BEGIN ADD *** 05/15/97 **************************************
     C*   CLAIMS/DARTS CONTROL KEY LIST
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END   ADD *** 05/15/97 **************************************
     C*
     C*  KEY LIST FOR THE DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEG ADD ****** 04/05/10 **********************************************
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     IXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGTSET
     C*** END ADD ****** 04/05/10 **********************************************
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDT          XXDATE
     C***  Begin Chg  ***  06/09/20  *******************************
     C*    *LIKE         DEFINE    PAFEIN        XXSS#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C***  End Chg  ***  06/09/20  *******************************
     C     *LIKE         DEFINE    CMSS#         XASS#
     C     *LIKE         DEFINE    *INKC         ENDPGM
     C********** *LIKE     DEFN CMACDT    XXACDT           DEL 08/22/00
     C********** *LIKE     DEFN CMDTRP    XXDTRP           DEL 08/22/00
     C********** *LIKE     DEFN CMDTLW    XXDTLW           DEL 08/22/00
     C********** *LIKE     DEFN CMDTRE    XXDTRE           DEL 08/22/00
     C********** *LIKE     DEFN CMENTD    XXENTD           DEL 08/22/00
     C********** *LIKE     DEFN CMRCVD    XXRCVD           DEL 08/22/00
     C********** *LIKE     DEFN PATRDT    XXTRDT           DEL 08/22/00
     C********** *LIKE     DEFN PAFRDT    XXFRDT           DEL 08/22/00
     C********** *LIKE     DEFN PATODT    XXTODT           DEL 08/22/00
     C********** *LIKE     DEFN CMDTIC    XXDTIC           DEL 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXACDT                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXDTRP                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXDTLW                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXDTRE                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXENTD                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXRCVD                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXTRDT                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXFRDT                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXTODT                         ADD 08/22/00
     C     *LIKE         DEFINE    SRDTI         XXDTIC                         ADD 08/22/00
     C     *LIKE         DEFINE    DDAMTB        XXAMTB
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*** BEGIN ADD *** 05/15/97 **************************************
     C     *LIKE         DEFINE    CXADJ         X1ADJ
     C     *LIKE         DEFINE    CXCAT2        X1CAT2
     C*** END   ADD *** 05/15/97 **************************************
     C*** Begin add *** 12/10/03 **************************************
     C     *LIKE         DEFINE    CTTINC        XXTINC
     C     *LIKE         DEFINE    CTRSV         XXRSVC
     C     *LIKE         DEFINE    CTRSV         XXAMT
     C*** End add ***** 12/10/03 **************************************
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      * -----------------------------------------------------------------
      *  @@MODE WILL BE A '6' IF W0070R06 CALLS
      *  THIS PGM TO DELETE A CLAIM (AS OPPOSED TO BEING CALLED FROM
      *  THE W0070R).  DO NOT DISPLAY F3 = EXIT.
      *
     C     @@MODE        IFEQ      '6'
     C                   MOVE      *ON           *IN41                          HIDE F3=EXIT
     C                   END
     C*
     C*  DEFINE WORK FIELDS
     C*
     C                   MOVE      *BLANKS       XXNAME           30
     C                   MOVE      *BLANKS       XXDESC           30
     C                   MOVE      *BLANKS       XXPYDS           20
     C                   MOVE      *BLANKS       XXREQ            12
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Y                 3 0
     C                   MOVE      *OFF          ENDPGM
     C                   MOVE      *BLANKS       SCDEX1
     C                   MOVE      *BLANKS       SCDEX2
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** Begin add *** 12/10/03 **************************************
     C                   MOVEL     XXDATE        XXCYM             5 0
     C*** End add ***** 12/10/03 **************************************
     C*
     C*  GET THE SYSTEM CONTROL RECORD
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              99
     C*
     C*  GET THE CLAIM RECORD
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C                   MOVEL     CMSS#         XASS#
     C*
     C*** BEGIN ADD *** 05/15/97 **************************************
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
     C*
     C     CXKY          CHAIN     PCX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
     C                   MOVEL     CXADJ         X1ADJ
     C                   ELSE
     C                   MOVEL     XXADJ         X1ADJ
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     CXCAT2        ANDNE     *BLANKS
     C                   MOVEL     CXCAT2        X1CAT2
     C                   ELSE
     C                   MOVEL     XXCAT2        X1CAT2
     C                   ENDIF
     C*** END   ADD *** 05/15/97 **************************************
     C*
     C*  MAIN LOOP
     C*
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     DLTWDW
     C                   MOVEA     '0000'        *IN(87)                        87 - 90
     C*
     C     *INKC         CASEQ     *ON           TERM                            F3
     C     *INKJ         CASEQ     *ON           KJDO                     90     F10
     C     *INKL         CASEQ     *ON           TERM                            F12
     C                   ENDCS
     C*
     C                   EXSR      ERRCK1                                       EDIT WINDOW
     C                   ENDDO
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      WRITE
     C                   EXSR      DLTRCD
     C                   EXSR      DLTCD
     C                   END
     C*
     C                   EXSR      TERM
     C*=========================================================================
     C*  THIS SUBROUTINE EDITS THE SCR01 ENTRY FIELDS
     C*=========================================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 06/24/13 **************************************
     C     @@RRN         CHAIN     PCM01                              99
     C                   MOVEL     CMCO#         XXCO#
     C*** END   ADD *** 06/24/13 **************************************
     C*
     C                   DO
     C*
     C     SCDLTR        IFNE      *BLANK
     C                   MOVEL     'DLTR'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDLTR        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCDLTD
     C                   MOVE      *ON           *IN88                          DSPATR(PC)
     C                   ELSE
     C                   SETON                                        8790
     C                   LEAVE
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  SCDLTR NE BLANK
     C*
     C*  THE EXPLANATION FIELDS MUST NOT BE BLANK
     C*
     C     SCDEX1        IFEQ      *BLANKS
     C     SCDEX2        ANDEQ     *BLANKS
     C     SCDLTR        ANDEQ     *BLANKS
     C                   SETON                                        8990
     C                   LEAVE
     C                   END
     C*
     C                   ENDDO
     C*
     C                   ENDSR
      *
      *=========================================================================
      *  DLTCD - THIS WILL CHECK FOR ANY OTHER CLAIMS FOR THIS CLAIMANT.
      *          IF NO OTHER CLAIMS EXIST, DELETE THE CLAIMANT
      *          DEMOGRAPHIC RECORD.
      *=========================================================================
     C     DLTCD         BEGSR
     C*
     C     CMKY9         CHAIN     PCM09                              60
     C     *IN60         IFEQ      *ON
     C*
     C     XASS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PCD01
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*  DLTRCD - THIS SUBROUTINE DELETES THE CLAIM RECORDS FROM THE
     C*     APPROPRIATE FILES.
     C*=========================================================================
     C     DLTRCD        BEGSR
     C*
     C*  DELETE RECORDS FROM THE CLAIM HISTORY FILE
     C*
     C     *IN99         DOUEQ     '1'
     C     CMKY          DELETE    PCH01                              99
     C                   END
     C*
     C*  DELETE TRANSFER FILE
     C*
     C     *IN99         DOUEQ     '1'
     C     CMKY          DELETE    PTH01                              99
     C                   END
     C*
     C*  DELETE WORK HISTORY FILE
     C*
     C     *IN99         DOUEQ     '1'
     C     CMKY          DELETE    PWH01                              99
     C                   END
     C*
     C*  DELETE RECORDS FROM THE RESERVE HISTORY FILE
     C*
     C*** Begin dlt *** 12/10/03 **************************************
     C*********  *IN99     DOUEQ'1'
     C*********  CMKY      DELETPRH01                99
     C*********            END
     C*** End dlt ***** 12/10/03 **************************************
     C*** Begin add *** 12/10/03 **************************************
     C                   Z-ADD     0             XXTINC
     C                   Z-ADD     0             XXRSVC
     C*
     C     CMKY          CHAIN     PRH01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C*  Calculate the total inc amt deleted and the rsv change amt
     C*  within the current month.
     C*
     C                   XFOOT     RSU           XXAMT
     C                   ADD       XXAMT         XXTINC
     C*
     C                   MOVEL     RHEFDT        XXCYM1            5 0
     C     XXCYM1        IFEQ      XXCYM
     C                   ADD       XXAMT         XXRSVC
     C                   ENDIF
     C*
     C                   DELETE    PRH01
     C     CMKY          READE     PRH01                                  99
     C                   ENDDO
     C*** End add ***** 12/10/03 **************************************
     C*
     C*  DELETE RECORDS FROM DIARY FILE
     C*
     C     *IN99         DOUEQ     '1'
     C     CMKY          DELETE    PDD01                              99
     C                   END
     C*
     C*** Begin add *** 08/22/07 **************************************
      *  DELETE RECORDS FROM CLAIM TRIAL INFORMATION (WMTIP)
      *
     C     *IN99         DOUEQ     '1'
     C     CMKY          DELETE    PTI01                              99
     C                   END
     C*** End add ***** 08/22/07 **************************************
     C*
     C*** BEGIN DLT *** 07/19/00 **************************************
     C*  IF REQUESTS FOR PAYMENTS EXIST, DISPLAY A MESSAGE WINDOW AND
     C*  ALLOW THE USER TO OVERRIDE THE ERROR MESSAGE.
     C**********
     C**********           MOVE 'N'       SCOVR
     C********** CMKY      CHAINPRP01                99
     C********** *IN99     IFEQ *OFF
     C**********           EXFMTWDW01
     C**********           END
     C**********
     C********** SCOVR     IFEQ 'Y'
     C*** END DLT ***** 07/19/00 **************************************
     C*
     C*  MARK RECORDS FROM REQUEST FOR PAYMENT FILE WITH A 'D'
     C*
     C     CMKY          SETLL     PRP01
     C*
     C     *IN60         DOUEQ     '1'
     C     CMKY          READE     PRP01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'D'           RPSTAT
     C                   MOVEL     Q1PGM         RPPGM
     C                   MOVEL     Q1USER        RPUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     XXDATE        RPDT
     C                   UPDATE    PRP01
     C                   END
     C                   ENDDO
     C*
     C*** BEGIN DLT *** 07/19/00 **************************************
     C******************** END                             SCOVR = Y
     C*** END DLT ***** 07/19/00 **************************************
     C*
     C*  DELETE RECORDS FROM THE BODY PART FILE
     C*
     C     *IN99         DOUEQ     '1'
     C     CMKY          DELETE    PBY01                              99
     C                   END
     C*
      ********* beg add ****** 12/01/14 *******************************
      * Delete records from the reserve worksheet history file
     c     *in99         DoUEq     '1'
     c     cmKy          Delete    prw00                              99
     c                   End
      ********* end add ****** 12/01/14 *******************************
     C*** BEGIN ADD *** 01/04/01 **************************************
     C*  Delete EDI A49 Log records
     C*
     C     CMKY          CHAIN     PLG01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     LGTSET        IFEQ      'A49'
     C                   DELETE    PLG01
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/29/03 **************************************
     C*
     C*  Delete/Create EDI 148 Log records
     C*
     C     LGTSET        IFEQ      '148'
     C*
     C     LGMTC         IFEQ      '00'
     C*
     C     LGSTS         IFEQ      'EC'
     C     LGSTS         OREQ      'EI'
     C     LGSTS         OREQ      'EX'
     C                   DELETE    PLG01
     C*
     C                   ELSE
     C                   MOVEL     LGJST         XXJST             2
     C*
     C                   CLEAR                   PLG01
     C                   MOVE      XXCO#         LGCO#
     C                   MOVE      XXFND         LGFND
     C                   MOVE      XXFYR         LGFYR
     C                   MOVE      XXCASE        LGCASE
     C                   MOVEL     XXJST         LGJST
     C                   MOVEL     '148'         LGTSET
     C                   MOVE      '01'          LGMTC
     C                   MOVE      'EC'          LGSTS
     C*
     C                   Z-ADD     XXDATE        LGDUE
     C                   Z-ADD     XXDATE        LGMTCD
     C*
     C                   MOVEL     Q1USER        LGIUSR
     C                   Z-ADD     XXDATE        LGIDT
     C                   Z-ADD     DSTIME        LGITM
     C                   MOVEL     Q1PGM         LGIPGM
     C                   MOVE      Q1USER        LGUSER
     C                   Z-ADD     XXDATE        LGDT
     C                   Z-ADD     DSTIME        LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C*** BEG ADD ***** 04/05/10 **************************************
     C                   EXSR      DARTS
     C*** END ADD ***** 04/05/10 **************************************
     C                   WRITE     PLG01
     C                   ENDIF
     C*
     C                   ELSE
     C     LGMTC         IFEQ      '01'
     C     LGIDT         ANDEQ     XXDATE
     C                   ELSE
     C                   DELETE    PLG01
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD ***** 08/29/03 **************************************
     C     CMKY          READE     PLG01                                  99
     C                   ENDDO
     C*** END ADD ***** 01/04/01 **************************************
     C*  IF PMT RECORDS EXIST, PRINT THE PMT DETAIL ON THE DELETION
     C*  REPORT AND DELETE THE RECORDS.
     C*
     C     CMKY          CHAIN     PPA01                              99
B1   C     *IN99         IFEQ      '0'
     C                   OPEN      QSYSPRT
     C*
     C*  GET THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      '0'
     C     CDLSN         CAT(P)    ',':0         XXNAME
     C                   CAT       CDFSN:1       XXNAME
     C                   END
     C*
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL     CMACDS        XXDESC
     C*
     C*  GET THE ATTORNEYS' INFO
     C*
     C     CMKY          CHAIN     PLL01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZEROS        LLDEFA
     C                   MOVE      *ZEROS        LLDEFM
     C                   MOVE      *ZEROS        LLPLAA
     C                   MOVE      *ZEROS        LLPLAM
     C                   END
     C*
     C*   CONVERT DATES
     C                   Z-ADD     CMACDT        SRDTI                          ACCIDENT DT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   Z-ADD     CMDTRP        SRDTI                          DT REPORTED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXDTRP
     C*
     C                   Z-ADD     CMDTLW        SRDTI                          DT LAST WORKED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXDTLW
     C*
     C                   Z-ADD     CMDTIC        SRDTI                          DT OF INIT CONTACT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXDTIC
     C*
     C                   Z-ADD     CMDTRE        SRDTI                          DT REHAB ASSIGNED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXDTRE
     C*
     C                   Z-ADD     CMENTD        SRDTI                          DT ENTERED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXENTD
     C*
     C                   Z-ADD     CMRCVD        SRDTI                          DT RECEIVED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXRCVD
     C*
     C                   EXCEPT    CDET
     C                   EXCEPT    PHEAD
     C*
     C*** BEGIN ADD *** 07/19/00 **************************************
     C                   MOVE      '0'           *IN99
     C*** END ADD ***** 07/19/00 **************************************
B2   C     *IN99         DOWEQ     '0'
     C*
     C                   Z-ADD     PATRDT        SRDTI                          TRANS DT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   Z-ADD     PAFRDT        SRDTI                          FROM DT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     PATODT        SRDTI                          TO DT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CHG 08/22/00
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVEL     PADESC        XXPYDS
     C                   EXCEPT    PDET
     C                   DELETE    PPA01
     C                   ADD       1             XXCNT
     C*
     C     CMKY          READE     PPA01                                  99
E2   C                   ENDDO
     C*
     C                   CLOSE     QSYSPRT
E1   C                   END
     C*
     C*  INACTIVATE DARTS REMARKS AND THE CORRESPONDING ADDT'L RMKS
     C*  AND LOAD DIARY COMPLETION DATE TO DIARY ITEMS
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*
     C*  CHAIN TO DARTS REMARKS DETAIL FILE
     C*
     C     C3KY          CHAIN     PCM03                              99
B1   C     *IN99         DOWEQ     '0'
     C*
     C     C3DIFL        IFEQ      'Y'
     C                   Z-ADD     XXDATE        C3DCDT
     C                   ELSE
     C                   MOVE      'I'           C3STAT
     C                   END                                                    ENDIF 'Y'
     C*
     C*  CHAIN TO ADDITIONAL REMARKS FILE
     C*
     C     ARKY          CHAIN     PAR01                              99
     C     *IN99         DOWEQ     '0'
     C                   MOVE      'I'           ARSTAT
     C                   MOVEL     Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     XXDATE        ARDT
     C                   UPDATE    PAR01
     C     ARKY          READE     PAR01                                  99
     C                   ENDDO                                                  ENDDO ARKY
     C*
     C                   MOVEL     Q1PGM         C3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        C3TIME
     C                   Z-ADD     XXDATE        C3DT
     C                   UPDATE    PCM03
     C     C3KY          READE     PCM03                                  99
E1   C                   ENDDO                                                  DOWEQ '0'
     C*
     C*  ACCUMULATE THE DEDUCTIBLE DETAIL BILLING AMOUNT, THEN DELETE
     C*  THE RECORD.  THE BILLING AMOUNT WILL BE LOADED INTO THE
     C*  REQUESTER FIELD IN THE CLAIM TRANSFER RECORD.
     C*
     C                   Z-ADD     0             XXAMTB
     C     CMKY          CHAIN     PDD02                              99
     C     *IN99         DOWEQ     '0'
     C                   ADD       DDAMTB        XXAMTB
     C                   DELETE    PDD02
     C     CMKY          READE     PDD02                                  99
     C                   ENDDO
     C*
     C                   CLEAR                   REQ
     C                   CLEAR                   AMT
     C                   MOVE      *BLANKS       XXREQ
     C     XXAMTB        IFNE      *ZERO
     C     XXAMTB        IFGT      *ZERO
     C                   Z-ADD     XXAMTB        XXAMTS
     C                   ELSE
     C                   Z-SUB     XXAMTB        XXAMTS
     C                   ENDIF
     C                   MOVEA     XXAMTC        AMT
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y                 3 0
     C                   MOVE      '$'           REQ(X)
     C                   ADD       1             X
     C     XXAMTS        IFGT      9.99
     C     '0'           LOOKUP    AMT(Y)                             99
     C                   ELSE
     C                   Z-ADD     7             Y
     C                   END
     C*
     C     Y             DO        9             Y
     C                   MOVE      AMT(Y)        REQ(X)
     C                   ADD       1             X
     C*
     C     Y             IFEQ      7
     C                   MOVE      '.'           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C     Y             IFEQ      9
     C     XXAMTB        ANDLT     *ZERO
     C                   MOVE      '-'           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C                   ENDDO                                                  Y DO 9
     C*
     C                   MOVEA     REQ           XXREQ
     C                   ENDIF
     C*
     C*  WRITE A RECORD TO WACTP (CLAIM TRANSFER FILE)
     C*
     C                   Z-ADD     CMCO#         CTCO#
     C                   Z-ADD     CMFND         CTFND
     C                   Z-ADD     CMFYR         CTFYR
     C                   Z-ADD     CMEMP#        CTEMP#
     C                   Z-ADD     CMDIV         CTDIV
     C                   Z-ADD     CMCASE        CTCASE
     C                   MOVE      CMSS#         CTSS#
     C                   Z-ADD     CMCLAS        CTCLAS
     C                   Z-ADD     CMACDT        CTACDT
     C*
     C                   Z-ADD     CMFYR         CTXFYR
     C                   Z-ADD     CMEMP#        CTXEMP
     C                   Z-ADD     CMDIV         CTXDIV
     C                   Z-ADD     CMCASE        CTXCAS
     C                   MOVE      CMSS#         CTXSS#
     C                   Z-ADD     CMCLAS        CTXCLS
     C                   Z-ADD     CMACDT        CTXACD
     C*
     C*** Begin add *** 12/10/03 **************************************
     C                   Z-ADD     XXTINC        CTTINC
     C                   Z-ADD     XXRSVC        CTRSV
     C*** End add ***** 12/10/03 **************************************
     C*
     C                   MOVE      *BLANKS       CTREQ
     C                   MOVEL     MSG1          CTREQ
     C                   MOVE      XXREQ         CTREQ
     C****************     MOVE 'Y'       CTDLTF
     C                   MOVEL     SCDLTR        CTDLTR
     C                   Z-ADD     XXCNT         CTDET#
     C                   MOVE      *BLANK        CTPRT
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1PGM         CTPGM
     C                   Z-ADD     XXDATE        CTDT
     C                   Z-ADD     XXDATE        CTTRDT                         ADD 05/18/00
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CTTIME
     C                   WRITE     PCT01
     C*** BEGIN ADD ***  07/06/98  ************************************
     C*
     C*  DELETE A/R FOR THE DEDUCTIBLE BILLED.
     C*
     C*
     C                   MOVE      CMCO#         QZCO#
     C                   MOVE      CMFND         QZFND
     C                   MOVE      CMFYR         QZFYR
     C                   MOVE      CMCASE        QZCASE
     C                   MOVE      CMEMP#        QZFEMP
     C                   MOVE      CMDIV         QZFDIV
     C                   MOVE      *ZEROS        QZTEMP
     C                   MOVE      *ZEROS        QZTDIV
     C                   MOVE      XXAMTB        QZAMTB
     C*** END ADD *****  07/06/98  ************************************
     C*
     C*  DELETE RECORDS FROM THE CLAIM FILE
     C*
     C     @@RRN         DELETE    PCM01                              99
     C*** BEGIN ADD ***  07/06/98  ************************************
     C                   CALL      'W8024R'
     C                   PARM      *BLANK        QZCALL            1
     C                   PARM                    QZCO#             3
     C                   PARM                    QZFND             3
     C                   PARM                    QZFYR             3
     C                   PARM                    QZCASE            7
     C                   PARM                    QZFEMP            9
     C                   PARM                    QZFDIV            5
     C                   PARM                    QZTEMP            9
     C                   PARM                    QZTDIV            5
     C                   PARM                    QZAMTB            9
     C                   PARM      *BLANK        QZRTCD            1
     C*
     C*** END ADD *****  07/06/98  ************************************
     C*
     C*  CALL THE API PGM TO INDICATE THAT A DELETION OCCURRED,
     C*  IF EDI IS INSTALLED.
     C*
     C     WCFL09        IFEQ      'Y'
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
     C                   MOVE      'D'           QQACTF
     C                   CALL      'W0070R6'
     C                   PARM                    QCCO#
     C                   PARM                    QCFND
     C                   PARM                    QCFYR
     C                   PARM                    QCCASE
     C                   PARM                    QQACTF            1            ACTIVITY FLAG
     C                   PARM                    QQMGID            8            RTN MSG ID
     C                   PARM                    QQRTCD            1            RETURN FLAG
     C                   END
     C*
     C*** BEG ADD *** 10/12/98 ***************************************
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*** END ADD *** 10/12/98 ***************************************
     C*** BEG ADD *** 09/16/98 ***************************************
     C*
     C*   FIRST YOU NEED TO GET THE RRN OF THE CASE YOU ARE DELETEING
     C*
     C                   MOVE      XXCO#         XYCO#             3
     C                   MOVE      XXFND         XYFND             3
     C                   MOVE      XXFYR         XYFYR             3
     C                   MOVE      XXCASE        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN1            9 0
     C                   PARM      *BLANK        XYRTCD            1
     C*
     C                   MOVE      Q1USER        Q1USER
     C                   MOVE      '07'          Q1CODE           10 0
     C                   MOVE      SCDEX1        Q1COM1
     C                   MOVE      SCDEX2        Q1COM2
     C*
     C                   CALL      'F7095R'
     C                   PARM      XYRRN1        Q1RRN             9 0
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C                   PARM                    Q1FYR             3 0
     C                   PARM                    Q1CASE            7 0
     C                   PARM                    Q1EMP#            9 0
     C                   PARM                    Q1DIV             5 0
     C                   PARM                    Q1SS#             9
     C*** BEG CHG *** 12/04/98 ***************************************
     C                   PARM                    Q1ACDT            8 0
     C**********           PARM           Q1ACDT  70
     C*** END CHG *** 12/04/98 ***************************************
     C                   PARM                    Q1LSN            30
     C                   PARM                    Q1FSN            15
     C                   PARM                    Q1MID             1
     C                   PARM                    Q1ADR1           40
     C                   PARM                    Q1ADR2           40
     C                   PARM                    Q1CTY            25
     C                   PARM                    Q1ST              2
     C                   PARM                    Q1ZIP             9 0
     C                   PARM                    Q1DOB             7 0
     C                   PARM                    Q1SEX             1
     C                   PARM                    Q1USER           10
     C                   PARM                    Q1CODE           10 0
     C                   PARM                    Q1COM1           70
     C                   PARM                    Q1COM2           70
     C                   PARM                    Q1COM3           70
     C                   PARM      'X'           Q1FLAG            1
     C                   PARM      'D'           Q1TD              1
     C                   PARM                    Q1RRN3            9 0
     C                   PARM      'Y'           Q1WCMS            1
     C*
     C*** END ADD *** 09/16/98 ***************************************
     C*** BEG ADD *** 10/12/98 ***************************************
     C                   ENDIF
     C*** END ADD *** 10/12/98 ***************************************
      // *** Beg Add * 08/13/15 ****
      /Free

          Chain ('SY') SSY01;
          If SYimif = 'Y';
             ImgDetach();
          Endif;

      /End-free
      // *** End Add * 08/13/15 ****

     C                   ENDSR
     C*
     C*=========================================================================
     C*  END OF PROGRAM
     C*=========================================================================
     C     TERM          BEGSR
     C*
      **** BEGIN CHANGE **** 01/29/99 *******************************
     C     @@MODE        IFEQ      '6'
     C     *INKL         ANDEQ     *ON
     C                   MOVE      'A'           @@MODE
     C                   ENDIF
      **** END CHANGE ****** 01/29/99 *******************************
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*  KJDO - SEARCH SUBROUTINE
     C*=========================================================================
     C     KJDO          BEGSR
     C*
     C* F10 = SEARCH FOR DELETE REASON CODE
     C*
     C     *INKJ         IFEQ      *ON
     C                   MOVE      *OFF          *IN88
     C                   MOVE      *BLANK        XXTBID
     C                   MOVEL     'DLTR'        XXTBID
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C                   MOVEL     XXKEY         SCDLTR
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                   QQRTC='A'
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                   QQRTC='B'
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     'DLTR'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDLTR        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDNE     'I'
     C                   MOVEL(P)  GTDESC        SCDLTD
     C                   MOVE      *ON           *IN88
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  QQRTC='*'
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C* WRITE - THIS SUBROUTINE WRITES THE RECORDS TO THE CLAIM
     C*    DELETION FILES.
     C*=========================================================================
     C     WRITE         BEGSR
     C*
     C*  WRITE A RECORD TO THE CLAIM HISTORY FILE.
     C*
     C                   MOVEL     SCDEX1        CMDEX1
     C                   MOVEL     SCDEX2        CMDEX2
     C                   MOVEL     SCDLTR        CMDLTR
     C                   Z-ADD     XXDATE        CMDLDT
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVEL     Q1USER        CMUSER
     C                   Z-ADD     XXDATE        CMDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDLSN         CMLSN
     C                   MOVEL     CDFSN         CMFSN
     C                   MOVEL     CDMID         CMMID
     C                   MOVEL     CDADR1        CMADR1
     C                   MOVEL     CDADR2        CMADR2
     C                   MOVEL     CDCTY         CMCTY
     C                   MOVEL     CDST          CMST
     C                   Z-ADD     CDZIP         CMZIP
     C                   Z-ADD     CDDTOB        CMDTOB
     C                   MOVEL     CDSEX         CMSEX
     C                   Z-ADD     CDAREA        CMAREA
     C                   Z-ADD     CDPHN#        CMPHN#
     C                   MOVEL     CDMSTS        CMMSTS
     C                   MOVEL     CDCNTY        CMCNTY
     C                   Z-ADD     CDWARA        CMWARA
     C                   Z-ADD     CDWPH#        CMWPH#
     C                   END
     C*
     C                   WRITE     PCM02
     C*
     C                   ENDSR
     C*** BEG ADD ***** 04/05/10 **************************************
     C     DARTS         BEGSR
     C*
     C     LGTSET        IFEQ      '148'
     C     LGTSET        OREQ      'A49'
     C*
     C     IXKY1         CHAIN     PIX01                              60
     C*
     C                   SELECT
     C     LGSTS         WHENEQ    'RE'
     C     IXCRER        ANDNE     *BLANKS
     C                   MOVE      IXCRER        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EI'
     C     IXCEIR        ANDNE     *BLANKS
     C                   MOVE      IXCEIR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE      IXCECR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE      IXCTAR        Q#RMCD
     C*
     C     LGSTS         WHENEQ    'TR'
     C     IXCTRR        ANDNE     *BLANKS
     C                   MOVE      IXCTRR        Q#RMCD
     C*
     C                   OTHER
     C                   GOTO      END
     C                   ENDSL
     C*
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C*
     C     Q#RMCD        CHAIN     PCT03                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  CTDESC        Q#COMM
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C*
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   UPUSR         Q#USER           10
     C                   ENDIF
     C     Q#USER        IFEQ      *BLANKS
     C                   GOTO      END
     C                   ENDIF
     C     LGSTS         IFEQ      'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C     LGSTS         IFEQ      'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   ENDIF
     C*
     C     CTDPRG        IFGT      0
     C                   MOVEL(P)  'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   ENDIF
     C*
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL     *BLANKS       Q#COMM
     C     LGMTC         CAT       '-':0         Q#COMM
     C                   CAT       CTDESC:0      Q#COMM
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   ENDIF
     C     END           ENDSR
     C*** END ADD ***** 04/05/10 **************************************
     C*=========================================================================
     OQSYSPRT   E            CDET           2 03
     O                                           12 '***** CLAIM#'
     O                       CMCASE        Z     20
     O                                           45 'WITH PAYMENTS DELETED BY'
     O                       Q1USER              56
     O                                           62 '*****'
     O          E            CDET           1
     O                                           13 'CO#  FND  FYR'
     O                                           28 'CLAIMANT NAME'
     O                                           56 'SOC SEC #'
     O                                           65 'CLASS'
     O                                           82 'EMPLOYER#   DIV'
     O                                           96 'ACCIDENT DATE'
     O                                          119 'ACCIDENT DESCRIPTION'
     O          E            CDET           2
     O                       CMCO#         4      3
     O                       CMFND         4      8
     O                       CMFYR         4     13
     O                       XXNAME              45
     C***  Begin Chg  ***  06/09/20  *******************************
     O*                      XXSS#               58 '   -  -    '
     O                       XXSS#               58
     C***  End Chg  ***  06/09/20  *******************************
     O                       CMCLAS        4     65
     O                       CMEMP#        4     75
     O                       CMDIV         4     82
     O                       XXACDT              95 '  /  /    '
     O                       XXDESC             129
     O          E            CDET           1
     O                                           19 'AVERAGE WEEKLY WAGE'
     O                                           40 'DAYS  ACCIDENT TIME'
     O                                           61 'DATE REPORTED  LAST'
     O                                           85 'WORKED  LOST TIME  NCCI'
     O                                          106 'NATURE  PART  CAUSE'
     O          E            CDET           2
     O                       CMAWW         J     16
     O                       CMDAYS              23
     O                       CMACTM              37 '  :  :  '
     O                       XXDTRP              54 '  /  /    '
     O                       XXDTLW              68 '  /  /    '
     O                       CMLTFL              75
     O                       CMNCCI              84
     O                       CMNATR              91
     O                       CMPART              98
     O                       CMCAUS             104
     O          E            CDET           1
     O*** BEGIN DEL *** 05/15/97 **************************************
     O**********                         18 'SUPERVISOR  CLAIMS'
     O**********                         38 'EXAMINER  FIELD ADJ'
     O**********                         63 'METHOD CONTACT  INITIAL'
     O**********                         83 'CONTACT   LOCATION '
     O**********                        105 'CATEGORY 2   DEFENSE'
     O**********                        127 'ATTY   PLAINTIFF ATTY'
     O*** END   DEL *** 05/15/97 **************************************
     O*** BEGIN ADD *** 05/15/97 **************************************
     O                                           10 'SUPERVISOR'
     O                       X1ADJ               31
     O                                           50 'FIELD ADJ  METHOD'
     O                                           75 'CONTACT  INITIAL CONTACT'
     O                                           85 'LOCATION'
     O                       X1CAT2              98
     O                                          112 'DEFENSE ATTY'
     O                                          128 'PLAINTIFF ATTY'
     O*** END   ADD *** 05/15/97 **************************************
     O          E            CDET           2
     O                       CMSUPR               6
     O                       CMCLEX              20
     O                       CMFEAD              35
     O                       CMMCON              47
     O                       XXDTIC              70 '  /  /    '
     O                       CMCTG1              83
     O                       CMCTG2              95
     O                       LLDEFA        Z    107
     O                       LLDEFM        Z    111
     O                       LLPLAA        Z    122
     O                       LLPLAM        Z    126
     O          E            CDET           1
     O                                           24 'REHAB DATE  DATE ENTERED'
     O                                           39 'DATE RECEIVED'
     O                                           64 'RESERVES:  TYPE'
     O                                           77 'REMAINING'
     O                                           92 'PAYMENTS'
     O          E            CDET           1
     O                       XXDTRE              10 '  /  /    '
     O                       XXENTD              23 '  /  /    '
     O                       XXRCVD              37 '  /  /    '
     O                       CMRT01              63
     O                       CMRR01        K     79
     O                       CMAR01        K     94
     O          E            CDET           1
     O                       CMRT02              63
     O                       CMRR02        K     79
     O                       CMAR02        K     94
     O          E            CDET           1
     O                       CMRT03              63
     O                       CMRR03        K     79
     O                       CMAR03        K     94
     O          E            CDET           1
     O                       CMRT04              63
     O                       CMRR04        K     79
     O                       CMAR04        K     94
     O          E            CDET           1
     O                       CMRT05              63
     O                       CMRR05        K     79
     O                       CMAR05        K     94
     O          E            CDET           1
     O                       CMRT06              63
     O                       CMRR06        K     79
     O                       CMAR06        K     94
     O          E            CDET           1
     O                       CMRT07              63
     O                       CMRR07        K     79
     O                       CMAR07        K     94
     O          E            CDET           1
     O                       CMRT08              63
     O                       CMRR08        K     79
     O                       CMAR08        K     94
     O          E            CDET           1
     O                       CMRT09              63
     O                       CMRR09        K     79
     O                       CMAR09        K     94
     O          E            CDET           3
     O                       CMRT10              63
     O                       CMRR10        K     79
     O                       CMAR10        K     94
     O          E            PHEAD          2
     O                                           25 'LIST OF DELETED PAYMENTS'
     O          E            PHEAD          1
     O                                            5 'TRANS'
     O                                           14 'RESERVE'
     O                                           47 'DATES OF SERVICE'
     O                                           71 'PMT   PMT'
     O          E            PHEAD          2
     O                                            5 'CD TP'
     O                                           13 'CD TP'
     O*** BEG CHG *** 08/22/00 ***********************************
     O***************                    38 'TRANS DATE'
     O                                           26 'TRANS DATE'
     O*** BEG CHG *** 08/22/00 ***********************************
     O                                           35 'FROM'
     O                                           47 'TO'
     O                                           72 'WKS  DYS  VAL1  VAL2'
     O                                           87 'PAYEE     MOD'
     O                                          100 'DESCRIPTION'
     O                                          117 'CHECK#'
     O                                          130 'AMOUNT'
     O          E            PDET           1
     O                       PATRCD               2
     O                       PATYPE               5
     O                       PARECD              10
     O                       PARETP              13
     O                       XXTRDT              26 '  /  /    '
     O                       XXFRDT              38 '  /  /    '
     O                       XXTODT              50 '  /  /    '
     O                       PAWKS         4     55
     O                       PADAYS        4     59
     O                       PABR2               65
     O                       PABR7               71
     O                       PAFEIN        4     83
     O                       PAMOD               87
     O                       XXPYDS             110
     O                       PACHK#        4    118
     O                       PAAMT         J    132

      // *** Beg Add * 08/13/15 ****
     P* --------------------------------------------------
     P* Procedure name: ImgDetach
     P* Purpose: Move any images attach to original
     P*          queue ready for indexing.
     P* Returns:
     P* --------------------------------------------------
     P ImgDetach       B
     D ImgDetach       PI
     D
      * Purpose: Image Key
     D ImgKey          S             40A
      /Free

          If not %open(imdolc);
             Open imdolc;
          Endif;
          If not %open(imiel5);
             Open imiel5;
          Endif;

          ImgKey = %editc(CMco#:'X') + %editc(CMfnd:'X')
                   + %editc(CMfyr:'X') + %editc(CMcase:'X');
          Setll (ImgKey) imdolc;
          Reade (ImgKey) imdolc;
          Dow not %eof(imdolc);
             DOdkey = *Blanks;
             DOdsq# = 0;
             DOidfx = 'N';
             DOdsts = 'RL';
             Update pdo01;
             Chain (DOdcid) imiel5;
             If %Found(imiel5);
                IEstat = *Blanks;
                IEDsts = 'RL';
                IEstat = 'A';
                Update pie01;
             Endif;
             Reade (ImgKey) imdolc;
          Enddo;

      /End-Free
     P ImgDetach       E
      // *** End Add * 08/13/15 ****
