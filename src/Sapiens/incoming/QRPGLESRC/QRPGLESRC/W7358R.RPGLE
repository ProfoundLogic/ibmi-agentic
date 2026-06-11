     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7358R - WORK WITH LOSS RECORDS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: MENU                                            *
     H*                                                               *
     H*  CALLS TO: W7358R02                                           *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 08/12/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/06/08 C:15  Project: 114WNI                                *
     H*                Added code to resubmit ICR reports.            *
     H*                                                               *
     H* 01/25/07 C:185 PROJECT: C114WNI                               *
     H*                RPT ICR DATA (INDIVIDUAL CASE REPORT) NJ       *
     H*                Added Option 08 for ICR Records.               *
     H*                                                               *
     H* 08/12/05 C:15  Support: WCxxxx                                *
     H*                Add code for correction processing.            *
     H*                                                               *
     H*  07/30/00  C:87  PROJECT: WC591                               *
     H*                  Added code to process records based on the   *
     H*                  state code.                                  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7358F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDSLP     IF   E             DISK
     F                                     RENAME(PSL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDSLL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSL01:DBRCDF)
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     FWMSRP     IF   E             DISK
     F*** Begin add *** 08/12/05 **************************************
     FWMSRP2L   UF A E           K DISK
     F                                     RENAME(PSR01:PSR02)
     FWDSLP2L   IF   E           K DISK
     F                                     RENAME(PSL01:PSL02)
     FWDSLP2L1  IF   E           K DISK
     F                                     RENAME(PSL01:PSL03)
     FWDSLP2L2  IF A E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PSL01:PSL04)
     FWMSQL     IF   E           K DISK
     FWMSQP2    O    E             DISK
     F                                     RENAME(PSQ01:PSQ02)
     F*** End add ***** 08/12/05 **************************************
     F*** BEGIN ADD *** 01/25/07 **************************************
     FWDSIL     IF   E           K DISK
     F*** END   ADD *** 01/25/07 **************************************
     F*** Begin add *** 05/06/08 **************************************
     FWMSRP3L2  UF A E           K DISK
     F                                     RENAME(PSR01:PSR03)
     F*** End add ***** 05/06/08 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D*** Begin chg *** 08/12/05 **************************************
     D******************* OF      1  10  1   OFD    35   Options, Function
     D*** Begin chg *** 01/25/07 **************************************
     D******************* OF      1  13  1   OFD    35   Options, Function
     D*** Begin chg *** 05/06/08 **************************************
     D******************  OF      1  14  1   OFD    35   Options, Function
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*** End chg ***** 05/06/08 **************************************
     D*** End chg ***** 08/12/05 **************************************
     D*** End chg ***** 01/25/07 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Loss Records')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W7358R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structures for SCDATA
     D                 DS
     D  XXDATA                 1     75
     D  XXCLAS                 1      5
     D  XXLOS#                 8      9
     D  XX#CLM                12     16
     D  XXACDT                19     28
     D  XXCLM#                31     42
     D  XXINDM                45     55
     D  XXMEDI                58     68
     D*
     D*** Begin add *** 08/12/05 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** End add ***** 08/12/05 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
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
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     CHGPL2        PLIST
     C                   PARM                    XXNUM
     C                   PARM                    EDTMSK
     C                   PARM                    DECIML            3 0
     C                   PARM                    XXTHOU            1
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** End add ***** 08/12/05 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   KFLD                    KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C                   KFLD                    KKCLAS
     C***  BEGIN ADD  ***  08/25/00  **********************************
     C                   KFLD                    KKCLM#
     C***  END   ADD  ***  08/25/00  **********************************
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCORR
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKCLM#
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCORR
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCORR
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKLINK
     C                   KFLD                    KKUPDT
     C*** End add ***** 08/12/05 **************************************
     C*
     C     SLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   KFLD                    KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*** Begin add *** 01/25/07 **************************************
     C     SIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKSTCD
     C*** End   add *** 01/25/07 **************************************
     C*
     C*  Darts/admin control defn key
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Fund master file key
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Employer detail file key
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SLCO#         KKCO#
     C     *LIKE         DEFINE    SLFND         KKFND
     C     *LIKE         DEFINE    SLRPMY        KKRPMY
     C     *LIKE         DEFINE    SLEMP#        KKEMP#
     C     *LIKE         DEFINE    SLFYR         KKFYR
     C     *LIKE         DEFINE    SLCLAS        KKCLAS
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C     *LIKE         DEFINE    SLSTCD        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C***  BEGIN ADD  ***  08/25/00  **********************************
     C     *LIKE         DEFINE    SLCLM#        KKCLM#
     C***  END   ADD  ***  08/25/00  **********************************
     C*** Begin add *** 08/12/05 **************************************
     C     *LIKE         DEFINE    SLCORR        KKCORR
     C     *LIKE         DEFINE    SLLINK        KKLINK
     C     *LIKE         DEFINE    SLUPDT        KKUPDT
     C*** End add ***** 08/12/05 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN         CHAIN     PSR01                              60
     C*
     C                   MOVE      SRCO#         KKCO#
     C                   MOVE      SRFND         KKFND
     C                   MOVE      SRRPMY        KKRPMY
     C                   MOVE      SREMP#        KKEMP#
     C                   MOVE      SRFYR         KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   MOVE      SRSTCD        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C                   MOVE      *LOVAL        KKCLAS
     C***  BEGIN ADD  ***  08/25/00  **********************************
     C                   MOVE      *LOVAL        KKCLM#
     C***  END   ADD  ***  08/25/00  **********************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  EMEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C*
     C     XXNUM         CAT       EMNAME:1      SCEMPL
     C*
     C                   MOVE      SRFYR         SCFYR
     C*
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE(P)   SCPST1        KKCLAS
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCCLAS        KKCLAS
     C***  BEGIN ADD  ***  08/25/00  **********************************
     C                   MOVE      SCCLM#        KKCLM#
     C***  END   ADD  ***  08/25/00  **********************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      SLCLAS        SCCLAS
     C                   MOVE      SLCLAS        XXCLAS
     C***  BEGIN ADD  ***  08/25/00  **********************************
     C                   MOVE      SLCLM#        SCCLM#
     C***  END   ADD  ***  08/25/00  **********************************
     C*
     C*  Edit stuff for output
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  SLCLAS        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCLAS
     C*
     C                   MOVEL(P)  SLLOS#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXLOS#
     C*
     C                   MOVEL(P)  SL#CLM        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XX#CLM
     C*
     C                   MOVEL(P)  SLCLM#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCLM#
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C                   Z-ADD     SLACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXACDT
     C*
     C*       Use Edit Code 'N' to format the following numbers
     C                   MOVEL(P)  'N'           EDTMSK
     C                   Z-ADD     0             DECIML
     C*
     C                   MOVEL(P)  SLINDM        XXNUM
     C                   CALL      CHGNUM        CHGPL2
     C                   MOVEL     XXNUM         XXINDM
     C*
     C                   MOVEL(P)  SLMEDI        XXNUM
     C                   CALL      CHGNUM        CHGPL2
     C                   MOVEL     XXNUM         XXMEDI
     C*
     C                   MOVEL(P)  XXDATA        SCDATA
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*** Begin add *** 08/12/05 **************************************
     C*================================================================
     C* KGDO - Process F7 - correction addition
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   SETON                                            31
     C                   EXSR      WRTHDR
     C                   EXSR      WRTLSS
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7358R22'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C*
     C                   SETOFF                                           31
     C*
     C                   ENDSR                                                  KGDO
     C*================================================================
     C* OPT6 - PROCESS OPTION 6 (CORRECTION DELETION)
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   SETON                                            32
     C                   EXSR      WRTHDR
     C                   EXSR      WRTLSS
     C                   SETOFF                                           32
     C*
     C                   ENDSR                                                  OPT6
     C*================================================================
     C* OPT7 - PROCESS OPTION 7 (CORRECTIONS)
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   EXSR      WRTHDR
     C                   EXSR      WRTLSS
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7358R22'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C*
     C                   ENDSR                                                  OPT7
     C*** End add ***** 08/12/05 **************************************
     C*
     C*** Begin add *** 01/25/07 **************************************
     C*================================================================
     C     OPT8          BEGSR
     C                   MOVE      '0'           *IN40
     C*
     C                   MOVE      SCCLM#        KKCLM#
     C     SIKY          CHAIN     PSI01                              40
     C     *IN40         IFEQ      *OFF
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7358R08'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C                   PARM      KKCO#         ##CO#             3 0
     C                   PARM      KKFND         ##FND             3 0
     C                   PARM      KKRPMY        ##RPMY            5 0
     C                   PARM      KKEMP#        ##EMP#            9 0
     C                   PARM      KKFYR         ##FYR             3 0
     C                   PARM      SCCLM#        ##CLM#           12
     C                   PARM      KKSTCD        ##STCD            2 0
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT8
     C*** End add ***** 01/25/07 **************************************
     C*** Begin add *** 05/06/08 **************************************
     C*================================================================
     C* OPT11 - Process option 11 (resubmit ICR)
     C*================================================================
     C     OPT11         BEGSR
     C*
     C*  Determine if a "resubmission" record already exists.
     C*  If not, create it.
     C*
     C                   MOVE      SCCLM#        KKCLM#
     C     SIKY          CHAIN     PSR03                              99
     C     *IN99         IFEQ      *ON
     C*
     C*  Retrieve Header record info.
     C*
     C     @@RRN         CHAIN     PSR01                              60
     C*
     C                   MOVE      SCCLM#        SRCLM#
     C                   MOVE      'I'           SRSTAT
     C                   MOVE      Q1USER        SRUSER
     C                   MOVE      Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVE      SRUSER        SRIUSR
     C                   MOVE      SRDT          SRIDT
     C                   WRITE     PSR03
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR                                                  OPT11
     C*** End add ***** 05/06/08 **************************************
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     *INKG         CASEQ     *ON           KGDO
     C                   ENDCS
     C*** End add ***** 08/12/05 **************************************
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C*** Begin add *** 01/25/07 **************************************
     C     SCSEL         CASEQ     '8'           OPT8
     C*** End   add *** 01/25/07 **************************************
     C*** Begin add *** 05/06/08 **************************************
     C     SCSEL         CASEQ     '11'          OPT11
     C*** End add ***** 05/06/08 **************************************
     C                   ENDCS
     C*** End add ***** 08/12/05 **************************************
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C  N09
     CANN30              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   MOVE      '0'           *IN40
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21SLKY          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21SLKY          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
     C*** Begin add *** 08/12/05 **************************************
     C*================================================================
     C* WRTLSS - This subroutine creates the loss correction
     C*   record.
     C*================================================================
     C     WRTLSS        BEGSR
     C*
     C*  Determine if a correction record already exists for the
     C*  selected exposure record.
     C*
     C                   SETON                                            99
     C     *IN31         IFEQ      *OFF
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   MOVE      SLCLAS        KKCLAS
     C                   MOVE      SLCLM#        KKCLM#
     C                   MOVE      SVCORR        KKCORR
     C     KEY2          CHAIN     PSL02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      SLLINK        KKLINK
     C                   MOVE      'R'           KKUPDT
     C     KEY4          CHAIN     PSL04                              99
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  If the correction record does not exist, write a "previous"
     C*  record and a "revised" record.
     C*
     C     *IN99         IFEQ      *ON
     C*
     C*  Retrieve a unique link #, then retreive the original exposure
     C*  record.
     C*
     C     KEY3          SETGT     PSL03
     C     KEY3          READPE    PSL03                                  99
     C  N99SLLINK        ADD       1             SVLINK            3 0
     C   99              Z-ADD     1             SVLINK
     C*
     C  N31SCDBRR        CHAIN     PFRCDF                             99
     C*
     C                   MOVE      SRCO#         SLCO#
     C                   MOVE      SRFND         SLFND
     C                   MOVE      SRRPMY        SLRPMY
     C                   MOVE      SREMP#        SLEMP#
     C                   MOVE      SRFYR         SLFYR
     C                   MOVE      SRCARR        SLCARR
     C                   MOVE      SRSTCD        SLSTCD
     C                   MOVE      SREFDT        SLEFDT
     C                   MOVE      SRRPT#        SLRPT#
     C*
     C                   MOVE      'P'           SLUPDT
     C                   MOVE      SVCORR        SLCORR
     C                   MOVE      SVLINK        SLLINK
     C                   MOVE      Q1USER        SLUSER
     C                   MOVE      Q1PGM         SLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SLDT
     C                   MOVE      SLUSER        SLIUSR
     C                   MOVE      SLDT          SLIDT
     C  N31              WRITE     PSL04
     C*
     C                   MOVE      'R'           SLUPDT
     C  N32              WRITE     PSL04
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHDR - This subroutine creates the header and unit total
     C*   correction records.
     C*================================================================
     C     WRTHDR        BEGSR
     C*
     C*  Determine if a header correction record already exists.
     C*  If not, create it.
     C*
     C                   Z-ADD     0             SVCORR            1 0
     C     SLKY          CHAIN     PSR02                              99
     C     *IN99         DOWEQ     *OFF
     C     SRSTAT        ANDEQ     'P'
     C     SLKY          READE     PSR02                                  99
     C                   ENDDO
     C                   MOVE      SRCORR        SVCORR
     C*
     C*  Load the correction fields.
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     @@RRN         CHAIN     PSR01                              60
     C                   ADD       1             SVCORR
     C                   MOVE      SVCORR        SRCORR
     C                   MOVE      'L'           SRCTYP
     C                   MOVE      Q1USER        SRUSER
     C                   MOVE      Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVE      SRUSER        SRIUSR
     C                   MOVE      SRDT          SRIDT
     C                   WRITE     PSR02
     C*
     C     SLKY          CHAIN     PSQ01                              60
     C                   MOVE      SVCORR        SQCORR
     C                   MOVE      Q1USER        SQUSER
     C                   MOVE      Q1PGM         SQPGM
     C                   Z-ADD     SRTIME        SQTIME
     C                   Z-ADD     SRDT          SQDT
     C                   MOVE      SQUSER        SQIUSR
     C                   MOVE      SQDT          SQIDT
     C                   WRITE     PSQ02
     C*
     C                   ELSE
     C*
     C     SRCTYP        IFNE      'L'
     C                   MOVE      'M'           SRCTYP
     C                   ENDIF
     C                   UPDATE    PSR02
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C*** End add ***** 08/12/05 **************************************
** Column headings
Opt Cls  Code  #Clms  Acc Date         Clm #     Indemnity      Medical
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Correction deletion    YNN
1077=Correction             YNN
1088=ICR Record             YNN
11111=Resubmit ICR          YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Correction addition   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
