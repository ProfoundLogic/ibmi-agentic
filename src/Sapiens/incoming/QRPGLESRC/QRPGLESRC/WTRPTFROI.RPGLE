     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE WTRPRFROI  Scriptura FROI Print Process.
     H*****************************************************************
    *  Created    : 07/19/17                                        *
    *  Programmer : C:053                                           *
    *  Project    : WC2499                                          *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      *  06/09/20 C:298 Project: SY420
      *                Mask SSN
      *                                                               *
      * 04/30/20 C:62  Ticket: 88176(099)                             *
      *                -Load all fields regardless of state.          *
      *                -Load employment type.                         *
      *                                                               *
      * 01/08/20 C:65  Project: SY408 - Portal                        *
      *                Add state to the type of process field when a  *
      *                state based template is chosen.                *
      *                                                               *
      * 05/31/19 C:65  Support: 79861 (FMIC) (Replicate 64432)        *
      *                Save Output from OutputWindow in SPILETTER     *
      *                uses position 190 of LDA.  If set, it          *
      *                causes Scriptura to fail when processing a mass*
      *                run. Adjusted ldMassFile in LDA to use         *
      *                different positions.                           *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  20  -  EMPLOYER ADDRESS 2 = BLANKS                           *
     H*  21  -  EMP LOCATION ADDRESS 2 = BLANKS                       *
     H*  22  -  COMPANY ADDRESS 2 = BLANKS                            *
     H*  23  -  CLAIMANT ADDRESS 2 = BLANKS                           *
     H*  26  -  PRINTS 'X' IN ACC TIME (AM) BOX                       *
     H*  27  -  PRINTS 'X' IN ACC TIME (PM) BOX                       *
     H*  29  -  COMPANY AUXILIARY ADDRESS 2 = BLANKS
     H*  60  -  GENERAL USAGE (CHAIN, READ, etc...)                   *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMLGL1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWMCML1    IF   E           K DISK    INFDS(DBFDS)
     FWACML     IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      * Employer Contact
     FWMECL1    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMNIL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*
     FWTGTL     IF   E           K DISK
     FWSFFL     IF   E           K DISK    Rename(pff01:pff00)
     FWDF3L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FSMUPL     IF   E           K DISK    Rename(pup01:pup00)
     FSMUPL1    IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMTHL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK    RENAME(PPA01:PPA02)
     FSXFRP     uf a e             disk    Rename(pfr01:pfr0p)
     FXMLPREL1  uf   e           k disk

     D isMassReq       s              1n   inz
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     d order           s              5  0
     d xx10a1          s              3  0
     d xxage           s              7  0
     d outputFile      s                   like(XPFILE)
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D PR              S              1    DIM(40)
     D*
     D svlda           ds          1024    dtaara(*lda)
     d*** begin chg ***  05/31/19  *******************************************
     D****ldMassFile                    60    Overlay(svlda:131)
     D ldMassFile                    60    Overlay(svlda:129)
     d*** end chg *****  05/31/19  *******************************************
     D ldOutputFile                  60    Overlay(svlda:191)
     D*
     D NI80a6          ds            80
     D XI806a                        25    Overlay(NI80a6:1)
     D XI806b                         2    Overlay(NI80a6:26)
     D XI806c                        13    Overlay(NI80a6:28)
     D*
     D NI80a8          ds            80
     D XI808a                        25    Overlay(NI80a8:1)
     D XI808b                         2    Overlay(NI80a8:26)
     D XI808c                        13    Overlay(NI80a8:28)
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXACTM                 1      6  0
     D  XXTIME                 1      4  0
     D                 DS
     D  XXTIM2                 1      6  0
     D  X1TIM2                 1      4  0
     D                 DS
     D  XXPOL1                 1     15
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMFYR                 13     15  0
     D                 DS
     D  XXFNMD                 1     12
     D  XXFEIN                 1      9  0
     D  XXMOD                 10     12  0
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D isScriptUsed    s              1A   inz('N')
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D outCall         s             11
     D InXML           s             60
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     D string          s             10    inz(*blanks)
     D isTrCd          s               n
     D XIcnty          s             20
     D X2030a3         s             20
     D chrKey          s                   Like(PCkey)
     D procType        s            100
     d massFlag        s              1    Inz('Y')
     d ClaimRRN        s              9  0
     d length          s              2  0
     d pos1            s              2  0
     d pos2            s              2  0
     d phone           s             10
     d xxwfsn          s             35
     d xxwlsn          s             35
     d*
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     D*
     D* Get Generic Table Value
     D GetTblDesc      pr            40
     D   tableID                      4    Const
     D   tableKey                    10    Const
     D*
     D* Get Cause Code Description
     D GetCauseDs      pr            40
     D   causeCd                      2    Const
     D*
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    10
     D   InXML_                      60
     D   InReturn_                  250
     D   OutMode_                     1
     D   OutRrn_                      9
     D   OutPrefix_                   2
     D   OutString_                  10
     D   OutCo#_                      3
     D   OutFnd_                      3
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D/copy sPrModule
     D/copy sPrXML
     D***  Begin Add  ***  06/09/20  ****************************
     D/copy sPrMask
     D***  End   Add  ***  06/09/20  ****************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *

     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHCPY
     C                   KFLD                    XXJRST
     C                   KFLD                    XXDATE
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    LGIDT
     C                   KFLD                    LGITM
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGTSET
     C*
     C     F3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    LGJST
     C*
     C*  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE ADDITIONAL ADDRESS FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE ADDITIONAL LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
     C*
     C*  KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*  KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHAGT#
     C*
     C*  KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  KEY LIST FOR THE STATE NOI INFORMATION FILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X2ST                           ADD 05/18/00
     C*
     C*  KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE TREATING PHYSICIAN HISTORY FILE
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  OFFICE KEY
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXOFF#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROI            2
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDATE            8
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
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
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    LGJST
     C                   PARM                    LGTSET
     C                   PARM                    XXFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMOFF#        XXOFF#
     C     *LIKE         DEFINE    LGHCPY        XXHCPY
     C     *LIKE         DEFINE    LGJST         XXJRST
     C     *LIKE         DEFINE    CMDT          XXDATE
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    CDCTY         XXCITY
     C     *LIKE         DEFINE    CDST          XXST
     C     *LIKE         DEFINE    CDZIP         XXZIP
     C     *LIKE         DEFINE    FNSFIN        XXSFIN
     C     *LIKE         DEFINE    LGTSET        SVTSET
     C     *LIKE         DEFINE    Q#DCID        QQDCID
     C     *LIKE         DEFINE    Q#FOLD        QQFLDR
     C     *LIKE         DEFINE    TPPHYN        XXPHYN
     C     *LIKE         DEFINE    TPPHYA        XXPHYA
     C     *LIKE         DEFINE    NIST          X2ST                           ADD 05/18/00
     C     *LIKE         DEFINE    NIPRNM        X1PRNM                         ADD 05/18/00
     C*
     C                   MOVE      *BLANKS       XXAREA            3
     C                   MOVE      *BLANKS       XEAREA            5
     C                   MOVE      *BLANKS       XCAREA            5
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQFROI        XXHCPY
     C                   MOVE      QQJRST        XXJRST
     C                   MOVE      QQJRST        X2ST                           ADD 05/18/00
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C                   MOVE      QQDATE        XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXDTE             8 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         saveDate          7 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      /Free

       // Determine if this module get spool file and/or XML output
       procType = 'First Report of Injury';
       isUseSpoolOut = UseSpoolOut(procType);
       isUseXML = UseScriptura(procType) Or
                  Use3rdXml(procType);
       if isUseXML;
         isScriptUsed = 'Y';
       else;
         isScriptUsed = 'N';
       Endif;

      /End-free
     C*
     C*
     C*  LOAD THE COMPANY INFORMATION.
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XCOCSZ           40
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM
     C                   MOVEL     COCTY         XXCITY
     C                   MOVEL     COST          XXST
     C                   Z-ADD     COZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCOCSZ
     C     COADR2        COMP      *BLANKS                                22
     C                   END
     C     @@CASE        COMP      *BLANKS                            3131
     C*
     C*  READ ALL RECORDS IN THE LOG HISTORY FILE.
     C*
     C     *IN31         IFEQ      *OFF
     C     LGKY          CHAIN     PLG01                              60
     C                   ELSE
     C     CMKY          CHAIN     PCM01                              60
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     *IN31         IFEQ      *OFF
     C     LGPRSQ        IFNE      0
     C     LGTSET        ORNE      '148'
     C                   GOTO      SKIP
     C                   END
     C                   ENDIF
     C*
     C                   EXSR      SRINIT
     C*
     C*  RETRIEVE THE CLAIM MASTER INFORMATION.
     C*
     C     *IN31         IFEQ      *OFF
     C     CMKY          CHAIN     PCM01                              60
     C   60CMKY          CHAIN     PCM02                              60
     C   60              GOTO      SKIP
     C                   ENDIF
     C*
     C                   MOVE      CMSS#         XXSS#
     C                   MOVEL(P)  CMOCDS        XXOCDS           30
     C                   MOVE      CMOFF#        XXOFF#
     C*
     C*  RETRIEVE THE CLAIMANT NAME AND ADDRESS.
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXCNAM
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       CDFSN:1       XXCNAM
     C     XXCNAM        CAT       CDMID:1       XXCNAM
     C                   MOVEL     CDADR1        XXCAD1
     C                   MOVEL     CDADR2        XXCAD2
     C*
     C                   MOVE      CDCTY         XXCITY
     C                   MOVE      CDST          XXST
     C                   Z-ADD     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C*
     C     CDCTRY        IFNE      *BLANKS
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        XXCSZ
     C                   END
     C                   MOVEL     XXCSZ         XXCCSZ
     C                   MOVE      CDCTRY        XXCCSZ
     C*
     C     CDADR2        COMP      *BLANKS                                23
     C*
     C                   END
     C*
     C*
     C*  GET THE EMPLOYER MASTER AND DETAIL INFORMATION
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
     C                   MOVEL     EMCONT        XXCONT
     C                   MOVEL     EMNAME        XXENAM
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL     EMPAD1        XXEAD1
     C                   MOVEL     EMPAD2        XXEAD2
     C                   MOVEL     EMPCTY        XXECTY
     C                   MOVE      EMPST         XXEST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   ELSE
     C                   MOVEL     EMMAD1        XXEAD1
     C                   MOVEL     EMMAD2        XXEAD2
     C                   MOVEL     EMMCTY        XXECTY
     C                   MOVE      EMMST         XXEST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   END
     C*
     C                   MOVE      *BLANKS       XXEZIP
     C                   MOVEL     XXZIP         XXZP1
     C                   MOVE      XXZIP         XXZP2
     C                   MOVEL     XXZP1         XXEZIP
     C     XXZP2         IFEQ      '0000'
     C     XXEZIP        CAT       '-':0         XXEZIP
     C     XXEZIP        CAT       XXZP2:0       XXEZIP
     C                   END
     C*
     C                   MOVE      *BLANKS       XEAREA
     C                   MOVE      EMAREA        XXAREA
     C     EMAREA        IFNE      0
     C                   MOVEL     '('           XEAREA
     C     XEAREA        CAT       XXAREA:0      XEAREA
     C     XEAREA        CAT       ')':0         XEAREA
     C                   END
     C*
     C                   END
     C*
     C*  GET THE LOCATION INFORMATION, IF ANY.
     C*
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      '0'
     C     ALPAD1        IFNE      *BLANKS
     C                   MOVEL     ALPAD1        XXLAD1
     C                   MOVEL     ALPAD2        XXLAD2
     C                   MOVEL     ALPCTY        XXCITY
     C                   MOVEL     ALPST         XXST
     C                   MOVEL     ALPZIP        XXZIP
     C                   ELSE
     C                   MOVEL     ALMAD1        XXLAD1
     C                   MOVEL     ALMAD2        XXLAD2
     C                   MOVEL     ALMCTY        XXCITY
     C                   MOVEL     ALMST         XXST
     C                   Z-ADD     ALMZIP        XXZIP
     C                   END
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXLCSZ
     C     XXLAD2        COMP      *BLANKS                                21
     C                   END
     C*
     C* Get County for GA
     C*
     C     CMCTG2        IFNE      *BLANKS
     C                   MOVEL(P)  CMCTG2        XICNTY
     C                   ELSE
     C*
     C     ALCNTY        IFNE      *BLANKS
     C     ALPST         IFNE      *BLANKS
     C     ALPST         CAT(P)    ALCNTY:0      XXKEY
     C                   ELSE
     C     ALMST         CAT(P)    ALCNTY:0      XXKEY
     C                   ENDIF
     C                   ELSE
     C*
     C     EMPST         IFNE      *BLANKS
     C     EMPST         CAT(P)    EMCNTY:0      XXKEY
     C                   ELSE
     C     EMMST         CAT(P)    EMCNTY:0      XXKEY
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       XICNTY
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        XICNTY
     C                   ENDIF
     C*
     C*  Use the policy # from the Carrier Info file, if it exists.
     C*
     C     ELKY          CHAIN     PRP01                              60
     C   60              MOVE      *BLANKS       RPCAP#
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVEL(P)  RPCAP#        XXPOL#           18
     C                   ELSE
     C                   MOVEL(P)  XXPOL1        XXPOL#
     C                   ENDIF
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   ENDDO
     C   60              Z-ADD     0             AHAGT#
     C                   MOVE      *BLANKS       AGNAME
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      *ZEROS        AAFEIN
     C                   MOVE      *ZEROS        AAMOD
     C     AAKY          CHAIN     PAA01                              60
     C     AGKY          CHAIN     PAG01                              60
     C                   END
     C*
     C                   MOVE      *BLANKS       XCAREA
     C                   MOVE      CDAREA        XXAREA
     C     CDAREA        IFNE      0
     C                   MOVEL     '('           XCAREA
     C     XCAREA        CAT       XXAREA:0      XCAREA
     C     XCAREA        CAT       ')':0         XCAREA
     C                   ENDIF
     C*
     C*  GET THE EMPLOYMENT STATUS DESCRIPTION
     C*
     C                   MOVEL     'EMST'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMESTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXESTS
     C*
     C*  CALCULATE THE # OF DEPENDENTS
     C*
     C                   Z-ADD     CM#DEP        XXDPND
     C*
     C*  GET THE TYPE OF INJURY DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C  N60              MOVEL     NADESC        XXNADS
     C*
     C*  GET THE PART OF BODY DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C  N60              MOVEL     PADESC        XXPADS
     C*
     C*  GET THE STATE NOI INFORMATION
     C*
     C                   CLEAR                   PNI01
     C     NIKY          CHAIN     PNI01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       NI1A1
     C                   MOVE      *BLANKS       NI1A2
     C                   MOVE      *BLANKS       NI20A1
     C                   MOVE      *BLANKS       NI20A2
     C                   MOVE      *BLANKS       NI40A1
     C                   MOVE      *BLANKS       NI40A2
     C                   MOVE      *BLANKS       NI40A3
     C                   MOVE      *BLANKS       NI40A4
     C                   MOVE      *BLANKS       NI40A5
     C                   MOVE      *BLANKS       NI40A6
     C                   MOVE      *BLANKS       NI40A7
     C                   MOVE      *BLANKS       NI40A8
     C                   MOVE      *BLANKS       NI60A1
     C                   MOVE      *BLANKS       NI60A2
     C                   MOVE      *BLANKS       NI60A3
     C                   MOVE      *BLANKS       NI60A4
     C                   MOVE      *BLANKS       NI60A5
     C                   MOVE      *BLANKS       NI60A6
     C                   MOVE      *BLANKS       NI80A1
     C                   MOVE      *BLANKS       NI80A2
     C                   MOVE      *BLANKS       NI80A3
     C                   MOVEL     *BLANKS       NI40A9
     C                   MOVEL     *BLANKS       NI40A0
     C                   MOVEL     *BLANKS       NI30A1
     C                   MOVEL     *BLANKS       NI10A1
     C                   Z-ADD     0             NIDT5
     C                   Z-ADD     0             NI9N1
     C                   Z-ADD     0             NIPREP
     C                   MOVEL     *BLANKS       NIPRNM
     C                   MOVEL     *BLANKS       NIPRPH
     C                   MOVEL     *BLANKS       NICONT
     C                   MOVEL     *BLANKS       NI40A1
     C                   MOVEL     *BLANKS       NI40A2
     C                   MOVEL     *BLANKS       NI25A1
     C                   MOVEL     *BLANKS       NI2A1
     C                   Z-ADD     *ZEROS        NI9N2
     C*** State additions
     C                   MOVEL     *BLANKS       NI1a2
     C                   MOVEL     *BLANKS       NI1a3
     C                   MOVEL     *BLANKS       NI1a4
     C                   MOVEL     *BLANKS       NI20a1
     C                   MOVEL     *BLANKS       NI30a2
     C                   MOVEL     *BLANKS       NI30a4
     C                   MOVEL     *BLANKS       NI40a1
     C                   MOVEL     *BLANKS       NI40a2
     C                   MOVEL     *BLANKS       NI40a3
     C                   MOVEL     *BLANKS       NI40a4
     C                   MOVEL     *BLANKS       NI40a8
     C                   MOVEL     *BLANKS       NI80a4
     C                   MOVEL     *BLANKS       NI80a5
     C                   MOVEL     *BLANKS       NI80a6
     C                   MOVEL     *BLANKS       NI80a7
     C                   MOVEL     *BLANKS       NI1a5
     C                   MOVEL     *BLANKS       NI1a6
     C                   MOVEL     *BLANKS       NI40a6
     C                   MOVEL     *BLANKS       NIprep
     C                   MOVEL     *BLANKS       NI10a1
     C                   MOVEL     *BLANKS       NI20a3
     C                   Z-ADD     *ZEROS        NIdt1
     C                   Z-ADD     *ZEROS        NIdt2
     C                   Z-ADD     *ZEROS        NIdt4
     C                   Z-ADD     *ZEROS        NI3n1
     C                   Z-ADD     *ZEROS        NI5n1
     C                   Z-ADD     *ZEROS        NI5n2
     C                   Z-ADD     *ZEROS        NI5n3
     C                   Z-ADD     *ZEROS        NI9n1
     C                   Z-ADD     *ZEROS        NI9n2
     C                   Z-ADD     *ZEROS        NI9n3
     C                   END
     C*
     C     XXLAD1        IFEQ      *BLANKS
     C                   MOVEL     NI40A1        XXLAD1
     C                   MOVEL     NI40A2        XXLAD2
     C     XXLAD2        COMP      *BLANKS                                21
     C                   CAT       NI40A2:2      NI40A1
     C     NI25A1        CAT(P)    ',':0         XXLCSZ
     C                   CAT       NI2A1:1       XXLCSZ
     C                   MOVEL     NI9N2         XXLZP1            5
     C                   MOVE      NI9N2         XXLZP2            4
     C                   CAT       XXLZP1:1      XXLCSZ
     C                   CAT       '-':0         XXLCSZ
     C                   CAT       XXLZP2:0      XXLCSZ
     C                   ENDIF
     C*
     C                   MOVE      *OFF          BL                1
     C                   MOVEA     NI40A7        PR
     C     1             DO        40            X                 3 0
     C     PR(X)         IFEQ      ','
     C     BL            OREQ      *ON
     C                   MOVE      ' '           PR(X)
     C                   MOVE      *ON           BL
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     PR            NI40A7
     C*
     C                   SELECT
     C     NI20A9        WHENEQ    'Day'
     C                   MOVEL     'X'           XXWGP1
     C     NI20A9        WHENEQ    'Week'
     C                   MOVEL     'X'           XXWGP3
     C     NI20A9        WHENEQ    'Month'
     C                   MOVEL     'X'           XXWGP2
     C                   OTHER
     C                   MOVEL     'X'           XXWGP4
     C                   ENDSL
     C*
     C                   MOVEL     NICONT        XXCONT
     C*
     C                   Z-ADD     NIPREP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         XXDTE
     C                   ENDIF
     C*
     C                   MOVE      NIPRNM        X1PRNM
     C                   MOVEA     NIPRNM        PR
     C     1             DO        40            X                 3 0
     C     PR(X)         IFEQ      ','
     C                   MOVE      ' '           PR(X)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     PR            NIPRNM
     C                   MOVEL     NIPRNM        XXPRNM           30
     C*
     C                   MOVEL     NIPRPH        XXTEN            10
     C                   MOVEL     XXTEN         XXPRPH           10 0
     C*
     C                   MOVEL(P)  NI80A1        XX30A1
     C                   MOVE(P)   NI80A1        XX50A7
     C                   MOVEL(P)  NI80A2        XX30A2
     C                   MOVEL(P)  XX50A7        NI80A2
     C                   MOVE      XX30A2        NI80A2
     C                   MOVEL     NI60A1        XX50A1
     C                   MOVEL     NI60A2        XX50A2
     C                   MOVEL     NI60A3        XX50A3
     C                   MOVEL     NI60A4        XX50A4
     C                   MOVEL     NI60A5        XX50A5
     C                   MOVEL     NI60A6        XX50A6
     C                   MOVEL     NI10A2        XXSTOH            2
     C*
     C     XX50A3        IFEQ      *BLANK
     C     XX50A4        ANDEQ     *BLANK
     C     CMACDS        CAT       CMACD2:0      XXWK3           100
     C                   MOVEL     XXWK3         XX50A3
     C                   MOVE      XXWK3         XX50A4
     C                   ENDIF                                                  . 50A3,4 Blk
     C*
     C*
     C*
     C*  LOAD THE PHYSICIAN AND HOSPITAL NAME AND ADDRESS
     C*
     C*** chain to the TREATING PHYSICIAN HISTORY FILE (WMTPP)
     C*** to obtain Physician Name and Address information
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         IFEQ      *OFF
     C     TPPHYN        IFNE      *BLANKS
     C                   MOVEL     TPPHYC        XXCITY
     C                   MOVEL     TPPHYS        XXST
     C                   Z-ADD     TPPHYZ        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXTCSZ
     C                   MOVEL     TPPHYN        XXPHYN
     C                   MOVEL     TPPHYA        XXPHYA
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C*** populate PDKY key fields with Hospital FEIN and MOD codes
     C*** from the Claim Master file WMCMP
     C                   MOVE      CMHOSF        XXFEIN
     C                   MOVE      CMHOSM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C     PDNAME        CAT       ',':0         XXPDNM           40
     C     XXPDNM        CAT       PDFSN:1       XXPDNM
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR2        XXPDAD           40
     C                   ELSE
     C                   MOVEL     PDADR1        XXPDAD
     C                   ENDIF
     C                   MOVEL     PDCTY         XXCITY
     C                   MOVEL     PDST          XXST
     C                   Z-ADD     PDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXPCSZ           40
     C                   ELSE
     C     CMHOSN        IFNE      *BLANK
     C                   MOVEL     CMHOSN        XXPDNM
     C                   MOVEL     CMHOSA        XXPDAD
     C                   MOVEL     CMHOSC        XXCITY
     C                   MOVEL     CMHOSS        XXST
     C                   Z-ADD     CMHOSZ        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXPCSZ           40
     C                   ENDIF
     C                   ENDIF
     C*
     C** California form begin
     C     QQFROI        IFEQ      'CA'
     C     CMHOSF        IFNE      *ZEROS
     C*
     C*** PPD01 CHAIN
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PDAREA        XXAREA            3
     C                   MOVEL     PDPHN#        XXPHNA            3
     C                   MOVE      PDPHN#        XXPHNB            4
     C*
     C     PDPHN#        IFNE      *ZEROS
     C     PDAREA        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXHPHN           14
     C                   CAT       ')':0         XXHPHN
     C                   CAT       XXPHNA:1      XXHPHN
     C                   CAT       '-':0         XXHPHN
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXHPHN
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXHPHN
     C                   ELSE
     C                   CLEAR                   XXHPHN
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CMHOSR        XXAREA            3
     C                   MOVEL     CMHOSP        XXPHNA            3
     C                   MOVE      CMHOSP        XXPHNB            4
     C*
     C     CMHOSP        IFNE      *ZEROS
     C     CMHOSR        IFNE      *ZEROS
     C     '('           CAT(P)    XXAREA:0      XXHPHN           14
     C                   CAT       ')':0         XXHPHN
     C                   CAT       XXPHNA:1      XXHPHN
     C                   CAT       '-':0         XXHPHN
     C                   ELSE
     C     XXPHNA        CAT(P)    '-':0         XXHPHN
     C                   ENDIF
     C                   CAT       XXPHNB:0      XXHPHN
     C                   ELSE
     C                   CLEAR                   XXHPHN
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C** California form end
     C*
     C*  LOAD THE PLACE OF ACCIDENT
     C                   MOVEL     NI30A1        XXCITY
     C                   MOVEL     NI10A1        XXST
     C                   Z-ADD     NI9N1         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXACSZ
     C*
     C*  LOAD AN 'X' TO THE FIELDS WHICH CORRESPOND TO THE APPROPRIATE
     C*  BOXES ON THE REPORT.
     C*
     C     CDSEX         IFNE      *BLANKS
     C     CDSEX         IFEQ      'M'                                          CLAIMANT'S SEX
     C                   MOVE      'X'           XXSEX1
     C                   ELSE
     C     CDSEX         IFEQ      'F'
     C                   MOVE      'X'           XXSEX2
     C                   ELSE
     C                   MOVE      'X'           XXSEX3
     C                   END
     C                   END
     C                   END
     C*
     C     CDMSTS        IFNE      *BLANKS
     C     CDMSTS        IFEQ      'U'                                          MARITAL STATUS
     C                   MOVE      'X'           XXMST1
     C                   ELSE
     C     CDMSTS        IFEQ      'M'
     C                   MOVE      'X'           XXMST2
     C                   ELSE
     C     CDMSTS        IFEQ      'S'
     C                   MOVE      'X'           XXMST3
     C                   ELSE
     C                   MOVE      'X'           XXMST4
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     CMWGPD        IFNE      *BLANKS
     C     CMWGPD        IFEQ      '6'                                          WAGE PERIOD
     C                   MOVE      'X'           XXWGP1
     C                   ELSE
     C     CMWGPD        IFEQ      '4'
     C                   MOVE      'X'           XXWGP2
     C                   ELSE
     C     CMWGPD        IFEQ      '1'
     C                   MOVE      'X'           XXWGP3
     C                   ELSE
     C                   MOVE      'X'           XXWGP4
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     CMPOI         IFEQ      'Y'                                          FULL SALARY
     C                   MOVE      'X'           XXPOI1
     C                   ELSE
     C                   MOVE      'X'           XXPOI2
     C                   END
     C*
     C     CMFSAL        IFEQ      'Y'                                          FULL SALARY
     C                   MOVE      'X'           XXFSL1
     C                   ELSE
     C                   MOVE      'X'           XXFSL2
     C                   END
     C*
     C     CMEADR        IFEQ      'Y'                                          EMP PREMISES
     C                   MOVE      'X'           XXEPR1
     C                   ELSE
     C                   MOVE      'X'           XXEPR2
     C                   END
     C*
     C     CMITRT        IFNE      *BLANKS
     C     CMITRT        IFEQ      '0'                                          INITIAL TREATMENT
     C                   MOVE      'X'           XXITR0
     C                   ELSE
     C     CMITRT        IFEQ      '1'
     C                   MOVE      'X'           XXITR1
     C                   ELSE
     C     CMITRT        IFEQ      '2'
     C                   MOVE      'X'           XXITR2
     C                   ELSE
     C     CMITRT        IFEQ      '3'
     C                   MOVE      'X'           XXITR3
     C                   ELSE
     C     CMITRT        IFEQ      '4'
     C                   MOVE      'X'           XXITR4
     C                   ELSE
     C                   MOVE      'X'           XXITR5
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     NI1A1         IFNE      *BLANKS                                      SAFETY EQUIP PROVIDE
     C     NI1A1         IFEQ      'Y'
     C                   MOVE      'X'           XXSFY1
     C                   ELSE
     C                   MOVE      'X'           XXSFY2
     C                   END
     C                   END
     C*
     C     NI1A2         IFNE      *BLANKS                                      SAFETY EQUIP USED
     C     NI1A2         IFEQ      'Y'
     C                   MOVE      'X'           XXSFY3
     C                   ELSE
     C                   MOVE      'X'           XXSFY4
     C                   END
     C                   END
     C*
     C*  CONVERT ALL DATES WHICH ARE PRINTED.
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTOB
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOH
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTLW
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTRP
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDBDT
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTW
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOD
     C                   Z-ADD     NIDT5         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCVD
     C                   Z-ADD     NIdt4         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFPMT            8 0
     C     XXRCVD        IFEQ      *ZEROS
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCVD
     C                   ENDIF
     C*
     C*  CONVERT THE BEGIN TIME
     C*
     C                   CLEAR                   XXTIM2
     C     NITIM2        IFGE      130000
     C     NITIM2        SUB       120000        XXTIM2
     C                   ELSE
     C     NITIM2        IFLT      10000
     C                   MOVEL     NITIM2        XXWK4A            4
     C                   MOVEL     '12'          XXWK4A
     C                   MOVEL     XXWK4A        XXTIM2
     C                   ELSE
     C                   Z-ADD     NITIM2        XXTIM2
     C                   END
     C                   END
     C     NITIM2        IFGE      120000
     C     NITIM2        ANDNE     240000
     C                   MOVE      '1'           *IN28
     C                   ELSE
     C                   MOVE      '0'           *IN28
     C                   END
     C*
     C*  CONVERT THE ACCIDENT TIME
     C*
     C     CMACTM        IFGE      130000
     C     CMACTM        SUB       120000        XXACTM
     C                   ELSE
     C     CMACTM        IFLT      10000
     C                   MOVEL     CMACTM        XXWK4A            4
     C                   MOVEL     '12'          XXWK4A
     C                   MOVEL     XXWK4A        XXACTM
     C                   ELSE
     C                   Z-ADD     CMACTM        XXACTM
     C                   END
     C                   END
     C     CMACTM        IFGE      120000
     C     CMACTM        ANDNE     240000
     C                   MOVE      '1'           *IN27
     C                   ELSE
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C*** Get additional files for states.
     C*
     C*  OPEN THE SPOOLFILE AND PRINT THE REPORT.  THEN CLOSE THE
     C*  SPOOLFILE, CREATE THE DARTS REMARK AND RESOLVE THE OUTPUT.
     C*
     C                   If        isUseSpoolOut
     C                   EXSR      SRDART
     C                   EndIf
     C*
     C*** Write to Scriptura FROI
     C                   If        isScriptUsed = 'Y'
     C                   ExSr      ScriptXML
     C                   Exsr      wrtSXFRP
     C                   Exsr      ScriptPRT
     C                   Endif
     C*
     C     *IN31         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C*  RETRIEVE THE NEXT RECORD
     C*
     C     SKIP          TAG
     C     LGKY          READE     PLG01                                  60
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
     C*****************************************************************
     C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
     C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
     C*****************************************************************
     C     SRINIT        BEGSR
     C*
     C                   MOVEA     '0000000'     *IN(21)
     C                   MOVE      *BLANKS       XXCNAM           35
     C                   MOVE      *BLANKS       XXCAD1           35
     C                   MOVE      *BLANKS       XXCAD2           35
     C                   MOVE      *BLANKS       XXCCSZ           35
     C                   MOVE      *BLANKS       XXCONT           33
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXEAD1           40
     C                   MOVE      *BLANKS       XXEAD2           40
     C                   MOVE      *BLANKS       XXECTY           25
     C                   MOVE      *BLANKS       XXEST             2
     C                   MOVE      *BLANKS       XXEZIP            9
     C                   MOVE      *BLANKS       XXLAD1           40
     C                   MOVE      *BLANKS       XXLAD2           40
     C                   MOVE      *BLANKS       XXLCSZ           40
     C                   MOVE      *BLANKS       XXTCSZ           40
     C                   MOVE      *BLANKS       XXPCSZ           40
     C                   MOVE      *BLANKS       XXACSZ           40
     C                   MOVE      *BLANKS       XXESTS           30
     C                   MOVE      *BLANKS       XXNADS           25
     C                   MOVE      *BLANKS       XXPADS           25
     C                   MOVE      *BLANKS       XX30A1           30
     C                   MOVE      *BLANKS       XX30A2           30
     C                   MOVE      *BLANKS       XX50A1           50
     C                   MOVE      *BLANKS       XX50A2           50
     C                   MOVE      *BLANKS       XX50A3           50
     C                   MOVE      *BLANKS       XX50A4           50
     C                   MOVE      *BLANKS       XX50A5           50
     C                   MOVE      *BLANKS       XX50A6           50
     C                   MOVE      *BLANKS       XX50A7           50
     C                   Z-ADD     0             XXSS#             9 0
     C                   Z-ADD     0             XXDPND            5 0
     C                   Z-ADD     0             XXPFDT            8 0
     C                   Z-ADD     0             XXPTDT            8 0
     C                   Z-ADD     0             XXDTOB            8 0
     C                   Z-ADD     0             XXDOH             8 0
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     0             XXACTM            6 0
     C                   Z-ADD     0             XXDTLW            8 0
     C                   Z-ADD     0             XXDTRP            8 0
     C                   Z-ADD     0             XXDBDT            8 0
     C                   Z-ADD     0             XXRTW             8 0
     C                   Z-ADD     0             XXDOD             8 0
     C                   Z-ADD     0             XXRCVD            8 0
     C                   MOVE      *BLANK        XXSEX1            1
     C                   MOVE      *BLANK        XXSEX2            1
     C                   MOVE      *BLANK        XXSEX3            1
     C                   MOVE      *BLANK        XXMST1            1
     C                   MOVE      *BLANK        XXMST2            1
     C                   MOVE      *BLANK        XXMST3            1
     C                   MOVE      *BLANK        XXMST4            1
     C                   MOVE      *BLANK        XXWGP1            1
     C                   MOVE      *BLANK        XXWGP2            1
     C                   MOVE      *BLANK        XXWGP3            1
     C                   MOVE      *BLANK        XXWGP4            1
     C                   MOVE      *BLANK        XXFSL1            1
     C                   MOVE      *BLANK        XXFSL2            1
     C                   MOVE      *BLANK        XXEPR1            1
     C                   MOVE      *BLANK        XXEPR2            1
     C                   MOVE      *BLANK        XXITR0            1
     C                   MOVE      *BLANK        XXITR1            1
     C                   MOVE      *BLANK        XXITR2            1
     C                   MOVE      *BLANK        XXITR3            1
     C                   MOVE      *BLANK        XXITR4            1
     C                   MOVE      *BLANK        XXITR5            1
     C                   MOVE      *BLANK        XXSFY1            1
     C                   MOVE      *BLANK        XXSFY2            1
     C                   MOVE      *BLANK        XXSFY3            1
     C                   MOVE      *BLANK        XXSFY4            1
     C                   MOVE      *BLANK        XXPOI1            1
     C                   MOVE      *BLANK        XXPOI2            1
     C                   MOVE      *BLANK        XXPHYN
     C                   MOVE      *BLANK        XXPHYA
     C                   MOVE      *BLANK        XXPDNM
     C                   MOVE      *BLANK        XXPDAD
     C*
     C     *IN31         IFEQ      *OFF
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C                   MOVE      LGJST         X2ST                           ADD 05/18/00
     C                   ELSE
     C                   MOVE      CMJRST        X2ST
     C                   ENDIF
     C*
     C*  RETRIEVE THE NEW FUND INFORMATION, IF THE FUND CHANGED.
     C*
     C     XXFND         IFNE      SVFND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C   60              MOVEL     *BLANKS       FNSFIN
     C                   MOVEL     *BLANKS       XXSFIN
     C     FNSFIN        IFEQ      'Y'
     C                   MOVE      'X'           XXSFIN
     C                   END
     C*
     C     FNKY          CHAIN     PF201                              60
     C     OFKY          CHAIN     POF01                              60
     C*
     C                   Z-ADD     XXFND         SVFND
     C                   END
     C*
     C*
     C*  RETRIEVE THE EDI CLAIMS CONTROL INFO, IF THE TRANS ID CHANGED.
     C*    ALSO, RETRIEVE THE DARTS REMARK CODE INFO.
     C*
     C     LGTSET        IFNE      SVTSET
     C*
     C***        IXKY      CHAINPIX01                60
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C   60              MOVE      *BLANKS       IXHCPR
     C     CTKY          CHAIN     PCT01                              60
     C   60              MOVE      *BLANKS       CTTYPE
     C   60              MOVE      *BLANKS       CTDESC
     C*
     C                   MOVEL     LGTSET        SVTSET
     C                   END
     C*
     C     F3KY          CHAIN     PF301                              60
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
     C* THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
     C* CONTAINING THE CITY, STATE AND ZIP                            *
     C*****************************************************************
     C     SRADDR        BEGSR
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVE      *BLANKS       XXSZ             13
     C*
     C                   MOVEA     XXST          SZ
     C                   MOVEL     XXZIP         XXZP1
     C                   MOVE      XXZIP         XXZP2
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZP2         IFNE      '0000'
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST           56
     C                   PARM                    XXFRST           15
     C                   PARM                    XXTERM            1
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
     C* IS PROCESSED.                                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C*  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
     C*  BLANK OR INVALID.
     C*
     C     IXHCPR        IFEQ      *BLANKS
     C     CTTYPE        OREQ      *BLANKS
     C                   GOTO      SKIP1
     C                   END
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
     C                   MOVEL(P)  IXHCPR        Q#RMCD            5
     C     Q#RMCD        CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  LGIUSR        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVE      *BLANKS       QQFND             8
     C                   MOVE      *BLANKS       QQSER             8
     C                   MOVE      *BLANKS       QQSUB             8
     C                   MOVE      *BLANKS       QQFOLD            8
     C                   MOVE      *BLANKS       QQFLDX            7
     C                   MOVE      *BLANKS       QQFPTH           40
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVEL     XXFND         QQFND                          for Fund
     C                   MOVEL     'CS'          QQSER                          for Area of Service
     C                   MOVEL     CTTYPE        QQSUB                          for Sub-Area
     C                   MOVEL     'C'           QQFOLD                         means "Member"
     C                   MOVEL     CMCASE        QQFLDX                         CASE for Folder name
     C                   MOVE      QQFLDX        QQFOLD
     C                   CALL      'SRFBLD'
     C                   PARM                    QQFND
     C                   PARM                    QQSER
     C                   PARM                    QQSUB
     C                   PARM                    QQFOLD
     C                   PARM                    QQFPTH                         Built, returned
     C                   PARM                    QQTERM
     C                   MOVEL(P)  QQFPTH        Q#FOLD           80
     C                   MOVE      *BLANKS       QQRT              2
     C                   MOVE      *BLANKS       QQDOC            12
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVEL     CTTYPE        QQRT                           Remark Type
     C                   CALL      'SRDBLD'
     C                   PARM                    QQFPTH                         From SRFBLD routine
     C                   PARM                    QQRT                           Rmrk Type for Doc ID
     C                   PARM                    QQDOC                          Built, returned
     C                   PARM                    QQTERM
     C                   MOVEL(P)  QQDOC         Q#DCID           12
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
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
     C*
     C*  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
     C*
     C     LGKY1         CHAIN     PLG02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      Q#SEQ#        LGPRSQ
     C                   UPDATE    PLG02
     C                   END
     C*
     C*  CALL W8070C1 TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
     C*
     C                   MOVE      Q#DCID        QQDCID
     C                   MOVE      Q#FOLD        QQFLDR
     C                   CALL      'W8070C1'
     C                   PARM                    Q1JOB
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C*
     C     SKIP1         TAG
     C                   ENDSR
     C*
     C     SRDE          BEGSR
     C*
     C     CDDTOB        IFEQ      *ZEROS
     C                   MOVEL     NI10A1        XX10A1
     C                   ELSE
     C     CMACDT        SUB       CDDTOB        XXAGE             7 0
     C                   MOVEL     XXAGE         XX10A1
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C     SRMA          BEGSR
     C                   ENDSR
     C*
     C     SRNY          BEGSR
     C                   MOVEL(P)  NI20A2        ZZ20A2            8
     C                   MOVE      *ZEROS        XXNYUI
     C*
     C                   TESTN                   ZZ20A2               69
     C     *IN69         IFEQ      *ON
     C                   MOVE      ZZ20A2        XXNYUI            8 0
     C                   ENDIF
     C                   ENDSR
     C*
     C     SRNC          BEGSR
      * Date of Injury
     C                   CLEAR                   X1DOI
     C                   CLEAR                   X1DAY
     C     CMACDT        IFNE      *ZERO
      *
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DOI             6 0
      *
     C                   MOVE      X1DOI         #1DATE
      *
     C                   CALL      'SRDAY'
     C                   PARM                    #1DATE            6            M/D/Y
     C                   PARM                    #1SDAY            4            Abbreviation
     C                   PARM                    #1LDAY            9            Full Day Name
     C                   PARM                    #1ERR             1            Error Flag
      *
     C                   MOVEL(P)  #1LDAY        X1DAY             9
      *
     C                   ENDIF
      *
     C                   MOVE(P)   *BLANKS       X1IAM             2
     C                   MOVE(P)   *BLANKS       X1IPM             2
     C                   MOVEL     CMTIME        X1ITM             4 0
      *
     C     X1ITM         IFGE      1300
     C                   SUB       1200          X1ITM
     C                   ENDIF
      *
     C     CMTIME        IFLT      120000
     C                   MOVE      'A'           X1ampm            1
     C                   ELSE
     C                   MOVE      'P'           X1ampm
     C                   ENDIF
      *
      *
     C     NI1A5         IFNE      ' '
     C                   MOVE      'A.M.'        X2AMPM            4
     C                   ENDIF
      *
     C     NI1A6         IFNE      ' '
     C                   MOVE      'P.M.'        X2AMPM            4
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       X1USNM           25
     C                   MOVE      *BLANKS       X1USER           25
      *  Name of Supervisor
     C     CMCLEX        CHAIN     SMUPL1                             60
     C  N60              MOVEL     UPUSNM        X1USNM
      *  Name of User
     C     CMUSER        CHAIN     SMUPL                              60
     C  N60              MOVEL     UPUSNM        X1USER
      *
      *
      *  Time employed by you
     C                   MOVEL     NI20A3        X1YRS             2 0
     C                   MOVEL     NI20A3        XXTMP4            4
     C                   MOVE      XXTMP4        X1MTHS            2 0
     C                   MOVEL     NI20A3        XXTMP6            6
     C                   MOVE      XXTMP6        X1DAYS            2 0
      *
     C                   MOVEL     X1YRS         XAYRS             2
     C                   MOVE      X1MTHS        XAMTHS            2
     C                   MOVE      X1DAYS        XADAYS            2
      *
      *  Wages per hour.
     C                   MOVE      NI10A1        X110A1            9 2
      *  Wages per day
     C                   MOVE      NI10A2        X110A2            9 2
      *  Board, lodging, fuel
     C                   MOVE      NI10A3        X110A3            9 2
     C                   MOVE      NI10A4        X110A4           10
      *
     C                   MOVE      NI10A5        X110A5            4 0
     C                   MOVEL     NI20A2        X120A2            9 2
      *
     C                   ENDSR
     C*
      /Free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       InXML   = ' ';
       string = ' ';
       OutString = QQFROI+'Froi    ';
       outCall = 'XMLREQUEST';
       OutPrefix = 'CL';
       OutMODE   = 'C';
       InReturn  = %trim(procType);
       massFlag = 'N';
       outco#   = %char(CMco#);
       outfnd   = %char(CMfnd);
       ClaimRRN  = dbRRN;
       OutRRN   = %EditC(ClaimRRN:'X');

       isMassReq = *off;
       If isMassReq;

          In *Lock *dtaara;
          Out *dtaara;

          If ldMassFile <> ' ';
             InXML   = %trim(ldMassFile);
          Else;
              StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
               OutPrefix:OutString:OutCo#:OutFnd:massFlag:xmlLink);
             In *Lock *dtaara;
             ldMassFile = %trim(InXML);
             Out *dtaara;
       //                                                                   *****
             Chain (InXML) xp100;
             XPCat = 'CS';
             Update xp100;
       //                                                                   *****
          Endif;

          isMassReq = *off;
          xmlFlowName = InXML;
          xmlLink = InXML;

          InXML   = ' ';
          string = ' ';
          OutString = QQFROI+'Froi    ';
          outCall = 'XMLREQUEST';
          OutPrefix = '  ';
          OutMODE   = 'C';
          InReturn  = %trim(procType);
          massFlag = 'N';
          outco#   = %char(CMco#);
          outfnd   = %char(CMfnd);
          ClaimRRN  = dbRRN;
          OutRRN   = %EditC(ClaimRRN:'X');
       Endif;

              StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
               OutPrefix:OutString:OutCo#:OutFnd:massFlag:xmlLink);

       If xmlFlowName = ' ';
          xmlFlowName = InXML;
       Endif;

       If xmlLink = ' ';
          Chain (InXML) xp100;
          InXML = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:InXML) xp100;
       Endif;

       xmlfile = xpxmlt;
       outputFile = XPFile;

       //* Set the output file name into the local area for additional documents to pick up.
       In *Lock *dtaara;
       ldOutputFile = outputFile;
       Out *dtaara;

       Setgt (000:xxHcpy:xxDate) pff00;
       Readpe (000:xxHcpy) pff00;
       If not %eof;
          //* Amend type of process to include state selected/found.
          XPType = %trim(XPType) + ' ' + %trim(xxHcpy);
          If FFDocC <> ' ';
             XPDcid = %trim(FFDocC);
             XPPath = %trim(FFPathC);
          Else;
             If FFDoc <> ' ';
                XPDcid = %trim(FFDoc);
                XPPath = %trim(FFPath);
             Endif;
          Endif;
       Else;
          Setgt (000:'  ':xxDate) pff00;
          Readpe (000:'  ') pff00;
          If not %eof;
             If FFDocC <> ' ';
                XPDcid = %trim(FFDocC);
                XPPath = %trim(FFPathC);
             Else;
                If FFDoc <> ' ';
                   XPDcid = %trim(FFDoc);
                   XPPath = %trim(FFPath);
                Endif;
             Endif;
          Endif;
       Endif;
       //*** end chg *****  12/19/16  *********************************************

       order = 1;

       XPData = 'Y';

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = saveDate;

       Update xp100;

       Endsr;

       // *=========================================================================
       // *  ScriptPRT - Print Scriptura output.
       // *=========================================================================
           Begsr ScriptPRT;
              If xmlFlowName <> ' ';
                 S4401C(xmlFlowName + '$');
              EndIf;
           Endsr;

       //-------------------------------------------------------------
       // WrtSXFRP - Write to Scriptura First Report if Injury File
       //--------------------------------------------------------------
           Begsr WrtSXFRP;

              Clear pfr0p;

              FRxml    = xmlfile;
              FReNam   = EMname;
              FRco#    = CMco#;
              FRfnd    = CMfnd;
              FRfName  = FNname;
              FRfYr    = CMfyr;
              FRcase   = CMcase;
              FReMad1  = EMmAd1;
              FReMad2  = EMmAd2;
              FReMcty  = EMmCty;
              FReMst   = EMmSt;
              FReMzip  = %EditC(EMmZip:'X');
              FRePad1  = EMpAd1;
              FRePad2  = EMpAd2;
              FRePcty  = EMpCty;
              FRePst   = EMpSt;
              FRePzip  = %EditC(EMpZip:'X');
              FRjrSt   = CMjrSt;
              FRacl#   = CMacl#;
              FRlPad1  = ALpAd1;
              FRlPad2  = ALpAd2;
              FRlPcty  = ALpCty;
              FRlPst   = ALpSt;
              FRlPzip  = ALpZip;
              FRlMad1  = ALmAd1;
              FRlMad2  = ALmAd2;
              FRlMcty  = ALmCty;
              FRlMst   = ALmSt;
              FRlMzip  = ALmZip;
              FRsic    = CMsic;
              FReitq   = CMeitq;

              chrKey   = CMsic;
              FRsicDs  = GetTblDesc('SIC ':chrKey);

              FReFei   = EMfein;
              FReArea  = EMarea;
              FRePhn#  = EMphn#;
              FRcoNm   = COname;

              Monitor;
                 FRpfDt = %dec(%date(ELpfDt:*cymd):*usa);
              On-Error;
                 FRpfDt = 0;
              EndMon;

              Monitor;
                 FRptDt = %dec(%date(ELptDt:*cymd):*usa);
              On-Error;
                 FRptDt = 0;
              EndMon;

              FRcoAd1  = COadr1;
              FRcoAd2  = COadr2;
              FRcoCty  = COcty;
              FRcoSt   = COst;
              FRcoZip  = COzip;

              If FNsFin = 'Y';
                 FRsFin   = 'X';
              EndIf;

              FRcofei  = COfein;

              If RPcap# <> ' ';
                 FRPol# = RPcap#;
              Else;
                 FRPol# = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
                          %EditC(CMfYr:'X');
              EndIf;

              FRagNm   = AGname;
              FRcFsn   = CDfsn;
              FRcLsn   = CDlsn;
              FRcMid   = CDmid;

              Monitor;
                 FRdtob = %dec(%date(CDdtob:*cymd):*usa);
              On-Error;
                 FRdtob = 0;
              EndMon;

//*********  Begin Chg  ***  06/09/20  **********************
//*******     FRss#    = CDss#;
              FRss#    = MaskData('SSN':'CDss#':CDss#);
//*********  End Chg  ***  06/09/20  **********************
              FRstoh   = NI10A2;
              FRsex    = CDsex;
              FRcAd1   = CDadr1;
              FRcAd2   = CDadr2;
              FRcCty   = CDcty;
              FRcSt    = CDst;
              FRcZip   = CDzip;
              FRcArea  = CDarea;
              FRcPhn#  = CDphn#;
              FRmSt    = CDmSts;
              FRocDs   = CMocDs;

              chrKey   = CMeSts;
              FRestD   = %Subst(GetTblDesc('EMST':chrKey):1:30);

              FRdPnd   = CM#dep;
              FRclas   = CMclas;
              FRwgp    = NI20A9;
              FRpoi    = CMpoi;
              FRaww    = CMaww;
              FRdays   = CMdays;
              FRfSal   = CMfSal;
              FRbgTm   = NItim2;

              Monitor;
                 FRacDt = %dec(%date(CMacdt:*cymd):*usa);
              On-Error;
                 FRacDt = 0;
              EndMon;

              FRacTm   = CMactm;

              Monitor;
                 FRdtLw = %dec(%date(CMdtLw:*cymd):*usa);
              On-Error;
                 FRdtLw = 0;
              EndMon;

              Monitor;
                 FRdtRp = %dec(%date(CMdtRp:*cymd):*usa);
              On-Error;
                 FRdtRp = 0;
              EndMon;

              Monitor;
                 FRdbDt = %dec(%date(CMdbDt:*cymd):*usa);
              On-Error;
                 FRdbDt = 0;
              EndMon;

              FRcont   = EMcont;
              FRnads   = xxNaDs;
              FRpads   = xxPaDs;
              FRePrm   = CMeAdr;
              FRnatr   = CMnatr;
              FRnatrDs = xxNaDs;
              FRpart   = CMpart;
              FRpartDs = xxPaDs;
              FRaccL1  = NI40A7;
              FRaccL2  = NI40A0;
              FRaCty   = NI30A1;
              FRaSt    = NI10A1;
              FRaZip   = NI9N1;
              FReUse1  = NI60A1;
              FReUse2  = NI60A2;
              FRempA1  = NI60A3;
              FRempA2  = NI60A4;
              If FRempA1 = *blanks And FRempa2 = *blanks;
                 FRempA1 = CMacds;
                 FRempA2 = CMacd2;
              EndIf;
              FRwPrc1  = NI60A5;
              FRwPrc2  = NI60A6;
              FRhowi1  = NI80A1;
              FRhowi2  = NI80A2;
              FRcaus   = CMcaus;
              FRcausDs = GetCauseDs(CMcaus);
              FRsafeP  = NI1A1;
              FRsafeU  = NI1A2;

              Monitor;
                 FRrtw  = %dec(%date(CMrtw:*cymd):*usa);
              On-Error;
                 FRrtw  = 0;
              EndMon;

              Monitor;
                 FRdod  = %dec(%date(CMdod:*cymd):*usa);
              On-Error;
                 FRdod  = 0;
              EndMon;

              FRinit   = CMiTrt;

              chrKey   = CMiTrt;
              FRinitDs = GetTblDesc('INIT':chrKey);

              FRphyN   = TPphyN;
              FRphyA   = TPphyA;
              FRtCty   = TPphyC;
              FRtSt    = TPphyS;
              FRtZip   = TPphyZ;
              FRhosN   = CMhosN;
              FRhosA   = CMhosA;
              FRhosC   = CMhosC;
              FRhosS   = CMhosS;
              FRhosZ   = CMhosZ;
              FRphyE   = NI40A4;
              FRhosE   = NI40A8;
              FRprPhA  = NI80A3;

              Monitor;
                 FRdTan = %dec(%date(NIdt5:*cymd):*usa);
              On-Error;
                 FRdtan = %dec(%date(CMrcvd:*cymd):*usa);
              EndMon;

              FRdte    = %dec(%date():*usa);
              FRprNm   = NIprNm;

              If NIprPh <> ' ' And NIprPh <> 'nullnull';
                 FRprPh   = %Dec(%Trim(NIprPh):10:0);
              EndIf;

              FRiDt =  %dec(%date():*cymd);
              Monitor;
              FRhidt = %dec(%date(CMhidt:*cymd):*usa);
              On-Error;
              FRhidt = 0;
              EndMon;
              FRests   = CMESTS;
              FRacds   = CMacds;
              FRffei   = F2fein;
              //*** Begin Add *** 04/30/20 *********************************
              // Common State Fields
              FRctg1   = CMctg1;
              FRnaob   = EMnaob;
              FRgt1    = CMgt1;
              FRacnm   = CMacnm;
              FRtparea = TPphyr;
              FRtpphn# = TPphyp;
              FRhrsd   = CMhrsd;
              FRhrsw   = CMhrsd * CMdays;
              FRacd2   = CMacd2;
              FRfarea  = FNph1a;
              FRfphn#  = FNph1;
              FRfpad1  = FNpad1;
              FRfpad2  = FNpad2;
              FRfpcty  = FNpcty;
              FRfpst   = FNpst ;
              FRfpzip  = FNpzip;
              FRfmad1  = FNmad1;
              FRfmad2  = FNmad2;
              FRfmcty  = FNmcty;
              FRfmst   = FNmst ;
              FRfmzip  = FNmzip;
              FRalnam  = FNanam;
              FRalad1  = FNaad1;
              FRalad2  = FNaad2;
              FRalcty  = FNacty;
              FRalst   = FNast ;
              FRalzip  = FNazip;
              FRcara   = COarea;
              FRcph#   = COphn#;
              FRrcvd   = CMrcvd;
              FRcads   = GetCauseDs(CMcaus);
              FRfdnm   = EMfdnm;
              FRfsdt   = CMfsdt;
              FReagr   = CMeagr;
              FRhosr   = CMhosr;
              FRhosp   = CMhosp;
              FRcrt    = CMcrt ;
              FRtpaara = F2area;
              FRtpaphn = F2sph#;
              FRiaud   = F2iaud;
              FRnaic   = EMnaic;
              FRsctc   = F2sctc;
              FRuser   = CMiusr;
              FReemad  = ECemal;
              FRoadr   = OFadr1;
              FRocty   = OFcty ;
              FRost    = OFst  ;
              FRozip   = OFzip ;
              FRonam   = OFname;
              FRoarea  = OFarea;
              FRophn#  = OFphn#;
              FRprww   = CMprww;
              FRpoww   = CMpoww;
              FRsemp   = EMsemp;
              FRcrcd   = F2crcd;
              FRwara   = CDwara;
              FRwph#   = CDwph#;
              FRcfar   = COfara;
              FRcfax   = COfax#;
              FRnab1   = EMnaob;
              FRpst    = CMeadr;
              FRsunm   = CMespv;
              FRsicd   = EMsic ;
              FRaccL1  = CMacdr;
              FRaCty   = CMacty;
              FRaSt    = CMacst;
              FRaZip   = CMacz1 + Cmacz2;
              //*** End   Add *** 04/30/20 *********************************

              Select;
              When QQfroi = 'CA';
                 Exsr LoadCA;
              When QQfroi = 'DE';
                 Exsr LoadDE;
              When QQfroi = 'FL';
                 Exsr LoadFL;
              When QQfroi = 'GA';
                 Exsr LoadGA;
              When QQfroi = 'MA';
                 Exsr LoadMA;
              When QQfroi = 'NC';
                 Exsr LoadNC;
              When QQfroi = 'NY';
                 Exsr LoadNY;
              When QQfroi = 'PA';
                 Exsr LoadPA;
              When QQfroi = 'LA';
                 Exsr LoadLA;
              EndSl;

              Exsr CltOvr;

              Write pfr0p;
           Endsr;

       //-------------------------------------------------------------
       // LoadCA - Load California fields
       //--------------------------------------------------------------
           Begsr LoadCA;
              FRsuin   = NI30a2;
              FRetyp   = NI20a1;
              FRstillw = NI1a3;
              Monitor;
                 FRepdt = %dec(%date(NIdt1:*cymd):*usa);
              On-Error;
                 FRepdt = 0;
              EndMon;
              FRcnty   = NI30a3;
              FRdepo   = NI40a4;
              FRowin   = NI1a4;
              FRemc1   = NI80a4;
              FRemc2   = NI80a5;
              FRsact1  = NI80a6;
              FRsact2  = NI80a7;
              FRhphn   = XXHPHN;
              FRtemr   = NI1a6;
              FRwhrs   = NI3n1;
              FRperd   = NI40a6;
              If CMotwk = *zeros And NI1a7 <> 'Y';
              FRopmt   = 'X';
              EndIf;
              FRprdt   = NIprep;
              FReUse1  = NI80A4;
              FReUse2  = NI80A5;
              FRratep  = NI9n3 / 100;
              Exsr OvrAcLoc;
           Endsr;

       //-------------------------------------------------------------
       // LoadDE - Load Delaware fields
       //--------------------------------------------------------------
           Begsr LoadDE;
              Exsr SRDE;
              FRdepo   = NI40a4;
              FRprdt   = NIprep;
              FRage = xx10a1;
              FRempp   = NI20a3;
              FRecnt   = XIcnty;
              FRswage  = NI1a2 ;
              FRlfdt   = NIdt2 ;
              FRadpt   = NI40a8;
           Endsr;

       //-------------------------------------------------------------
       // LoadFL - Load Florida fields
       //--------------------------------------------------------------
           Begsr LoadFL;

              FRratep  = NI9n3 / 100;
              If CMdny = 'Y' And NI1a6 <> 'Y';
                 FRdny    = 'X';
              EndIf;
              FRmolt   = NI1a3 ;
              If CMdny = 'Y' And NI1a6 = 'Y';
                 FRidny   = 'X';
              EndIf;
              FRdd8    = NIdt1 ;
              FRadd8   = NIdt2 ;
              FRfpmt   = xxfpmt;
              FRcompt  = NI10a1;
              FRpena   = NI9n1 / 100;
              FRpeni   = NI9n2 / 100;
              FRrmrk   = NI40a1 + NI40a2;
              FRcopy   = 'Employer Copy ';
              if NI20a4 = '3';
                 FRmedo   = 'X';
              Endif;
              Exsr OvrAcLoc;
           Endsr;

       //-------------------------------------------------------------
       // LoadGA - Load Georgia fields
       //--------------------------------------------------------------
           Begsr LoadGA;
              FRcnty   = XIcnty;
              FRdepo   = NI40a4;
              FRprdt   = %dec(%date():*usa);

              FRcemad  = NI40a3;
              FRoemal  = NI40a4;
              FRsbwc   = NI5n3 ;
              FRdyof   = NI20a1;
              If X1usnm <> *blanks;
                 FRprep   = X1usnm;
              Else;
                 FRprep   = X1user;
              Endif;
              If CMltfl = 'I';
                 Chain cmky pth01;
                 DoU THfrom ='M' or %eof(WMTHL);
                    Reade cmky pth01;
                 EndDo;
                 If not %Eof(WMTHL);
                    FRpmed   = 'Y';
                 Else;
                    FRpmed   = 'N';
                 EndIf;
              Endif;
              FRwkbn   = NI5n2 ;
              Chain cmky ppa02;
              DoU %eof(WDPAL5);
                 If PAtype = 'C';
                    FRpadt   = PAtrdt;
                    FRaadt   = PAamt ;
                    Leave;
                 EndIf;
                 Reade cmky ppa02;
              EndDo;

              Chain cmky ppa02;
              DoU %eof(WDPAL5);
              xxkey = PABR2;
              Chain tbky pb201;
              If not %eof(WTABL);
                 If B2PLCD = 'A';
                    FRnlty = FRnlty + PAamt;
                 EndIf;
              EndIf;
                 Reade cmky ppa02;
              EndDo;

              FRpayf   = NIdt1 ;
              FRbenf   = NI10a2;
              FRperc   = NI3n1 ;
              FRdpart  = NI10a3;
              FRdwks   = NI3n2 ;
              FRpayt   = NIdt2 ;
              FRnbep   = NI60a1;
           Endsr;

       //-------------------------------------------------------------
       // LoadMA - Load Massachusetts fields
       //--------------------------------------------------------------
           Begsr LoadMA;
              Exsr SRMA;
              If NI1a2 = 'E';
                 FRawwe   = 'X';
              EndIf;
              If NI1a2 = 'A';
                 FRawwe   = 'X';
              EndIf;
              FRindi   = NI2a1 ;
              FRbust   = NI20a7;
              FRsin#   = NI40a3;
              FRboso   = NI20a7;
              FR5thd   = NIdt1 ;
              FRsrci1  = NI60a1;
              FRsrci2  = NI60a2;
              FRdtrw   = NIdt2 ;
              FRincd1  = NI10a1;
              FRincd2  = NI10a2;
              FRincd3  = NI10a3;
              FRptcd1  = NI10a4;
              FRptcd2  = NI10a5;
              FRptcd3  = NI20a6;
              FRwitn1  = NI80a3;
              evalR FRwitn2  = NI80a3;
              FRrtwq   = NI1a3 ;
              FRrtro   = NI1a4 ;
              FRprti   = NIprnm;
              FRaccL1  = NI40a1;
              FRaCty   = NI25a1;
              FRaSt    = NI2a1;
              FRaZip   = NI9n2;
              Exsr OvrAcLoc;
           Endsr;

       //-------------------------------------------------------------
       // LoadNC - Load North Carolina fields
       //--------------------------------------------------------------
           Begsr LoadNC;
              Exsr SRNC;
              FRnab2   = *blanks;
              FRploc   = NI40a1;
              FRpcnt   = NI30a1;
              FRpdpt   = NI30a2;
              FRaday   = X1day ;
              FRdmap   = X1ampm;
              FRtemp   = XAyrs + 'y' + XAmths + 'm' + XAdays + 'd';
              FRwphr   = X110a1;
              FRwpdy   = X110a2;
              FRblf    = X110a3;
              FRblfp   = X110a4;
              FRlinj1  = NI60a3;
              FRlinj2  = NI60a4;
              FRhrrw   = X110a5;
              FRrtap   = X2AMPM;
              FRrtww   = X120a2;
              FRrtwwp  = NI20a4;
              FRtphy   = NI1a8 ;
              If CMdod <> *zeros;
                 FRdth    = 'X';
              EndIf;
           Endsr;

       //-------------------------------------------------------------
       // LoadNY - Load New York fields
       //--------------------------------------------------------------
           Begsr LoadNY;
              Exsr SRNY;
              FRdepo   = NI40a4;
              FRrtwq   = NI1a3 ;
              FRrpdt   = %dec(%date():*usa);
              FRosha   = NI20a1;
              FRnyui   = XXnyui;
              FRacnt   = NI20a3;
              FR52wp   = NI5n1 ;
              FRwmon   = ' ';
              FRwtue   = ' ';
              FRwwed   = ' ';
              FRwthu   = ' ';
              FRwfri   = ' ';
              FRwsat   = ' ';
              FRwsun   = ' ';
              Select;
              When NI10a1 = '1';
                 FRwmon   = 'X';
              When NI10a1 = '2';
                 FRwtue   = 'X';
              When NI10a1 = '3';
                 FRwwed   = 'X';
              When NI10a1 = '4';
                 FRwthu   = 'X';
              When NI10a1 = '5';
                 FRwfri   = 'X';
              When NI10a1 = '6';
                 FRwsat   = 'X';
              When NI10a1 = '7';
                 FRwsun   = 'X';
              Endsl;
              FRmedc   = NI1a2 ;
              FRmddt   = NIdt1 ;
              FReter   = NI1a2 ;
              FRhoso   = NI1a3 ;
              FRhowa1  = NI60a3;
              FRhowa2  = NI60a4;
              FRobji   = NI60a1 + NI60a2;
              FRreln   = NI80a5;
              evalR FRrela = NI80a5;
              FRrelc   = XI806a;
              FRrels   = XI806b;
              FRrelz   = XI806c;
              FRrltn   = NI20a3;
              FRprpe   = NI10a2;
              FR3rdn   = NI80a7;
              evalR FR3rda = NI80a7;
              FR3rdc   = XI808a;
              FR3rds   = XI808b;
              FR3rdz   = XI808c;
              FR3rdt   = NI40a5;
              FR3rdp   = NI30a5;
                 clear FRprnm;
                 clear FRprti;
              if x1prnm <> *blanks;
                 x1prnm = %trim(x1prnm);
                 pos1 = 1;
                 pos2 = %scan(',':x1prnm:pos1);
                 length = pos2 - pos1;
                 if length > 0;
                    FRprnm = %subst(x1prnm:pos1:length);
                 endif;
                 pos1 = pos2 + 1;
                 FRprti = %subst(x1prnm:pos1:27);
              endif;
              Exsr OvrAcLoc;
           Endsr;

       //-------------------------------------------------------------
       // LoadPA - Load Pennsylvania fields
       //--------------------------------------------------------------
           Begsr LoadPA;
              FRcnty   = NI30a4;

                 clear FRprnm;
                 clear FRprti;
              if x1prnm <> *blanks;
                 x1prnm = %trim(x1prnm);
                 pos1 = 1;
                 pos2 = %scan(',':x1prnm:pos1);
                 length = pos2 - pos1;
                 if length > 0;
                    FRprnm = %subst(x1prnm:pos1:length);
                 endif;
                 pos1 = pos2 + 1;
                 FRprti = %subst(x1prnm:pos1:27);
              endif;

              if nicont <> *blanks;
                 nicont = %trim(nicont);
                 pos1 = 1;
                 pos2 = %scan(',':nicont:pos1);
                 length = pos2 - pos1;
                 if length > 0;
                    FRcntf = %subst(nicont:pos1:length);
                 endif;
                 pos1 = pos2 + 1;
                 FRcntl = %subst(nicont:pos1:27);
              else;
                 clear                   FRcntf;
                 clear                   FRcntl;
              endif;
    
              if ni80a3 <> *blanks;
                 ni80a3 = %trim(ni80a3);
                 pos1 = 1;
                 pos2 = %scan(',':ni80a3:pos1);
                 eval      length = pos2 - pos1;
                 if        length > 0;
                    xxwfsn = %subst(ni80a3:pos1:length);
                    FRwfsn = xxwfsn;
                 endif;
                 pos1 = pos2 + 1;
                 pos2 = %scan(',':ni80a3:pos1);
                 length = pos2 - pos1;
                 if length > 0;
                    xxwlsn = %subst(ni80a3:pos1:length);
                    FRwlsn = xxwlsn;
                 endif;
                 pos1 = pos2 + 1;
                 phone = %subst(ni80a3:pos1:10);
                 if phone <> *blanks;
                    FRwphn = phone;
                 else;
                    clear FRwphn;
                 endif;

              else;
                 clear FRwfsn;
                 clear FRwlsn;
                 clear FRwphn;
              endif;

              FRcnta   = NIcar ;
              FRcntp   = NIcphn;
              FRbucd   = NI20a4;
              FRecnt   = XIcnty;
              FRbgTm   = NItim1;
           Endsr;

       //-------------------------------------------------------------
       // LoadLA - Load Louisiana fields
       //--------------------------------------------------------------
           Begsr LoadLA;
              FRdepo   = NI40a4;
              FRswage  = NI1a2 ;
              FRempui  = NI20a2;
              Select;
              When NI20a4 = '1';
                 FRm7d    = 'X';
              When NI20a4 = '2';
                 FRpossd  = 'X';
              When NI20a4 = '3';
                 FRmedo   = 'X';
              When NI20a4 = '4';
                 FRinjd   = 'X';
              When NI20a4 = '5';
                 FRlump   = 'X';
              When NI20a4 = '6';
                 FRamp    = 'X';
              When NI20a4 = '7';
                 FRother  = 'X';
              Endsl;
              FRrpdt   = %dec(%date():*usa);
              FRnstm   = NItim2;
              FRpari   = NI20a3;
              FRdiadt  = NIdt3 ;
              if NI20a4 = '3';
                 FRmedo   = 'X';
              Endif;
              Select;
              When CMwgpd = '6';
                 FRemppdd = 'X';
              When CMwgpd = '1';
                 FRemppdw = 'X';
              When CMwgpd = '4';
                 FRemppdm = 'X';
              When CMwgpd <> '6' and CMwgpd <> '1' and CMwgpd <> '4';
                 FRemppdo = 'X';
              Other;
                 FRemppdd = ' ';
                 FRemppdw = ' ';
                 FRemppdm = ' ';
                 FRemppdo = ' ';
              Endsl;
              Exsr OvrAcLoc;
           Endsr;
       //-------------------------------------------------------------
       // OvrAcLoc - Override accident location.
       //--------------------------------------------------------------
           Begsr OvrAcLoc;
              if QQfroi = 'CA' or QQfroi = 'LA' or QQfroi = 'NY';
                 FRaccL2  = NI40A0;
              Else;
                 FRaccL2  = *blanks;
              Endif;
           Endsr;
       //-------------------------------------------------------------
       // CltOvr - Client specific overrides
       //--------------------------------------------------------------
           Begsr CltOvr;

           If Cltid = '025';
              FRcoFe = F3Ifei;
           EndIf;

           Endsr;

      /End-free
     C* ************************************************************************
       //===================================================================
       // GetTblDesc - Get the Generic Table Description
       //===================================================================
     P GetTblDesc      B
     D GetTblDesc      PI            40
     D  tableID                       4    Const
     D  tableKey                     10    Const

     D  description    s             40
      /Free

         Chain (CMco#:tableID:tableKey) pgt01;
         If %Found(WTGTL);
            Return GTdesc;
         EndIf;

         Return  ' ';

      /End-Free
     P GetTblDesc      E
       //===================================================================
       // GetCauseDs - Get Cause Code Description
       //===================================================================
     P GetCauseDs      B
     D GetCauseDs      PI            40
     D  causeCd                       2    Const

      /Free

         chrKey = causeCd;
         Chain (CMco#:chrKey) pca01;
         If %Found(WTABL);
            Return CAdesc;
         EndIf;

         Return  ' ';

      /End-Free
     P GetCauseDs      E
