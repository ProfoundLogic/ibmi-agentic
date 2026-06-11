     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE SR89600B - LOAD ENDORSE FORM WORKFILE FORM WC890600.B
      *===============================================================*
      * Called from W7051C1 - Produces WC890600B form                 *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 07/31/25 C:300 Support: 117577 (AmWins)                       *
      *                Waiver of Subrogation MTE not attaching WC890600B
      *                                                               *
      * 01/21/25 C:342 Support: 118196 Prescient                      *
      *                1.Populated Wxmlenp1 file for endorsment_      *
      *                 description as tag was missing in XML.        *
      *                2.This support need template changes for       *
      *                  WC890600B form by Chakrapani.                *
      *                                                               *
      * 03/11/24 C:340 Project: 148WEQ                                *
      *                Change Program Description                     *
      *                Always generate Form WC890600B.                *
      *                                                               *
      * 09/07/23 C:65  Support: Phw#102091 (Amwins)                   *
      *                Print the WC890600B form for officer mid-term  *
      *                endorsements.  The checkbox checked and text   *
      *                will be determined later after which officer   *
      *                form is determined to attach to the MTE.       *
      *                                                               *
      * 07/19/22 C:65  Support: Phw#99485  (Amwins)                   *
      *                Print proper form # of the schedule.  Check    *
      *                setup to determine if there is a client        *
      *                template set up as well to override base form. *
      *                                                               *
      * 05/20/22 C:333 Support: 72454                                 *
      *                Mid Term Endorsement / Additional Named Insured*
      *                - The print verbiage for the form should read  *
      *                  "See Attached Schedule".                     *
      *                                                               *
      * 04/22/22 C:079 Support: 140/97336                             *
      *                Remove dash from FEIN's on change.             *
      *                                                               *
      * 02/10/21 C:136 Support: 80110                                 *
      *                Fix 'is changed to read' verbiage.             *
      *                                                               *
      * 03/27/20 C:318 Support: 84705                                 *
      *                When multiple adress is changed simultaneously *
      *                process only once                              *
      *                                                               *
      * 03/05/18 C:136 Project: 150WCT                                *
      *                Add change for additional named insured.       *
      *                - XMLPREP is created here instead of W7051R1   *
      *                - LDA is populated here instead of W7051R1     *
      *                Old comments removed                           *
      *                - Priss 70493 - Blank out employer name        *
      *                  for add named ins endorsement                *
      *                                                               *
      * 06/28/17 C:136 Support: 47334                                 *
      *                Use 'WC 00 00 01 B' form for policies          *
      *                effective 01/01/15 and later. Reference        *
      *                project WC2318.                                *
      *                                                               *
      * 12/01/16 C:136 Project: SY371                                 *
      *                Implement Scriptura Phase 2 logic              *
      *                                                               *
      * 11/10/16 C:136 Project: SY371                                 *
      *               -Add field for extended description             *
      *               -Renamed fields in WTENP because ENdsc2 is      *
      *                used in WMENP and WTENP                        *
      *                                                               *
      * 05/06/16 C:298 Project WC2423                                 *
      *               -Replace 'WC04000001A' with 'WC040001A' for 'CA'*
      *                state.                                         *
      *               -Attach for CA Misc Endorsement                 *
      *                                                               *
      * 07/12/10 C:61  PROJECT WC1866
      *                Populate new "Insured Name" and "Insured Federal
      *                Name" fields.
      *                                                               *
      * 12/21/07 C:61  SUPPORT WA099 61379                            *
      *                Change text to print on endorsement.  Clt139
      *                uses form wc990655 instead of wc000001a for
      *                FL policy.
      *
      * 11/15/06 C:61  project 139WCI
      *                Print PEO Insured info if PEO group.
      *                                                               *
      * 09/11/06 C:61  SUPPORT: WA132 54290 132WEW
      *                - Call SRPAMT to get premium amount.
      *
      * 07/19/06 C:61  SUPPORT WA132 51985
      *                Change text to print on endorsement.  Clt132
      *                uses form wc040005 instead of wc000001a.
      *
      * 07/14/06 C:61  SUPPORT WA132 53599
      *                Include Terrorism and Domestic Terrorism
      *                charges in premium amount.
     H*                                                               *
     H* 04/10/06 C:33  Seq#: WA139/50831                              *
     H*                Reference WC 00 00 01 B for NJ policies.       *
     H*                                                               *
     H* 01/27/06 C:61  PROJECT CM027                                  *
     H*                Copy from SR89600 and revise for WC890600.B.   *
     H*                                                               *
     H* 08/17/05 C:61  PROJECT WC1266                                 *
     H*                Check for "Rescission Division Deletion" flag. *
     H*                                                               *
     H* 06/15/04 C:61  PROJECT WC1224  WA091 40129                    *
     H*                Only print WC890605 for mailing address change *
     H*                of state reporting division.  If not state     *
     H*                reporting division, print WC890608.            *
     H*                                                               *
     H* 01/22/04 C:09  PROJECT: CM027                                 *
     H*                Correct issue whereby multiple WC890600A forms *
     H*                with same exact data were printing on entity/  *
     H*                location changes.                              *
     H*                                                               *
     H* 01/20/04 C:09  PROJECT: CM027                                 *
     H*                We will no longer generate a separate NCCI     *
     H*                midterm endorsement form (WC890600A) if        *
     H*                payroll changes are made.  One form will       *
     H*                be created with Item 4.*Class, Rate, Other     *
     H*                (WC890615) marked or Interim Adjustment of     *
     H*                Premium (WC890616) marked or both, as appr.    *
     H*                (see W0060R, W0090R  for other changes).       *
     H*                                                               *
     H* 06/04/01 C:90  PROJECT: CM027                                 *
     H*                Load 3 line mid-term endorsement description   *
     H*                from employer billing maintenance.             *
     H*                                                               *
     H* 03/27/01 C:90  PROJECT: CM027                                 *
     H*                ADD CODE TO RECOGNIZE MIDTERM PRODUCER CHANGE. *
     H*                                                               *
     H* 06/29/99 C:113 PROJECT: CM027                                 *
     H*                Add/Delete Division & Add/Delete/Change        *
     H*                Entity/Location now uses constant line under   *
     H*                'is changed to read:'.                         *
     H*                                                               *
     H* 05/27/99 C:113 PROJECT CM027                                  *
     H*                For Deletion of Division and Deletion of       *
     H*                Ent/Loc print constant line under "is changed  *
     H*                to read:"                                      *
     H*                                                               *
     H* 05/20/99 C:113 PROJECT CM027                                  *
     H*                Changed to load mailing address if physical    *
     H*                address is not found on Addition or Deletion   *
     H*                of Division.                                   *
     H*                                                               *
     H* 05/17/99 C:09 PROJECT CM044                                   *
     H*               ADD CODE TO RECOGNIZE MIDTERM ADDITION/DELETION *
     H*               OF DIVISION.                                    *
     H*                                                               *
     H* 05/06/99 C:09 PROJECT CM044                                   *
     H*               ADD CODE TO RECOGNIZE MIDTERM ENTITY/LOCATION   *
     H*               ADDITION, DELETION AND CHANGES.                 *
     H*                                                               *
     H* 10/09/98 C:09 PROJECT CM027                                   *
     H*               ONLY ACCUMULATE PREMIUM FOR A GIVEN FUND YEAR.  *
     H*                                                               *
     H* 08/18/98 C:78 PROJECT CM027                                   *
     H*               LOADING PREMIUM FOR ALL DIVISIONS.              *
     H*                                                               *
     H* 07/07/98 C:57 PROJECT SUPPORT 12250                           *
     H*               REMOVE SECTION FOR BOTH ADDRESS CHG PROCESSING  *
     H*               AS IT WAS CAUSEING TWO SETS OF OUTPUT.          *
     H*                                                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK    infds(dbfds)
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTENL     IF   E           K DISK
     F                                     RENAME(PEN01:PTEN01)
     FWMENL3    IF   E           K DISK
     F                                     RENAME(PEN01:PMEN01)
     FWTGTL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMMUL4    IF A E           K DISK
     FWXICP     O    E             DISK
     FWXFMP     O    E             DISK
     FWMRSL     IF   E           K DISK
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     fWXWXFMP   o    e             disk    UsrOpn
     f*** begin add ***  07/19/22  ****************************************
     FWSPPL     IF   E           K DISK    rename(ppp01:ppp00)
     f*** end add *****  07/19/22  ****************************************
     f*** begin add ***  01/21/25  ****************************************
     fwxmlenp1  o    e             disk    usropn
     F                                     prefix(e1)
     f*** end add *****  01/21/25  ****************************************

     D formName        s             15A
     D isScriptUsed    s              1A   inz('N')
     D PrintingPolicy  S               N
     D RunningSRText   S               N   Inz(*Off)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D outCall         s             11
     D InXML           s             60
     D InXML@          s             61
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     D pgmInStack      s              1
     D processType     s            250
     D UseScript       s              1n
     D UsePrintOut     s              1n
     D UseXML          S              1n
     D
     D* Returns '1' in parm 2 if pgm in stack
     D ChkPgmInStack   pr                  ExtPgm('SRCHKPGM')
     D                               10    const
     D                                1
     D
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    11
     D   InXML_                      60
     D   InReturn_                  250    options(*nopass)
     D   OutMode_                     1    const options(*nopass)
     D   OutRrn_                      9    const options(*nopass)
     D   OutPrefix_                   2    const options(*nopass)
     D   OutString_                  10    const options(*nopass)
     D   OutCo#_                      3    const options(*nopass)
     D   OutFnd_                      3    const options(*nopass)

     D/copy sPrModule
     D/copy sPrXML

     d SendToScriptura...
     d                 pr                  ExtPgm('S4401C')
     d  outXMLString                 61
     D*
     D XXFORM          C                   CONST('WC890600B ')
     d prMex           s             15
     D PRMEXA          C                   CONST('WC 00 00 01 A')
     D PRMEXB          C                   CONST('WC 00 00 01 B')
     D PRM132          C                   CONST('WC 04 00 05')
     D P139FL          C                   CONST('WC 99 06 55')
     D SEEREV          C                   CONST('See Revised')
     D XXEXT1          C                   CONST('See Extension of Inf-
     D                                     ormation Page for')
     D*
     D XXEXT2          C                   CONST('Other Workplaces Inf-
     D                                     ormation')
     D*** begin add ***  09/07/23  ***************************************
     D OFFTEXT         C                   CONST('See Attached Owner/O-
     D                                     fficer Endorsement')
     D*** end add *****  09/07/23  ***************************************
     D*** begin chg ***  07/19/22  ***************************************
     D***** PRMEXC          C                   CONST('WC 04 00 01 A')
     D PRMEXC          C                   CONST('WC 00 00 01 C')
     D*** end chg *****  07/19/22  ***************************************
     D*
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9
     D*
     D                 DS
     D  DSDAT7                 1      7  0
     D  DSCENT                 1      1
     D  DSYY                   2      3
     D  DSMM                   4      5
     D  DSDD                   6      7
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXNAME                 1    100
     D  FMEMNM                 1     40
     D  FMC215                41     60
     D  FMC441                61    100
      *
     D                 DS
     D  XXFNAM                 1    100
     D  FMFENM                 1     40
     D  FMC216                41     60
     D  FMC442                61    100
      *
     D*

     d dbfds           ds
     d  binElrrn             397    400b 0

     D svLDA           ds                  Dtaara(*LDA)
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     d  ldOWCg                28     28
     d  ldAddNamEnd           30     30
     d  ldFlowName           131    190
     d  LDAXML               191    250
     D*
     D                 DS
     D  XXFLGS                 1     25
     D*  WC890601
     D  FMX01                  1      1
     D*  WC890602
     D  FMX02                  2      2
     D*  WC890603
     D  FMX03                  3      3
     D*  WC890604
     D  FMX04                  4      4
     D*  WC890605
     D  FMX05                  5      5
     D*  WC890406
     D  FMX06                  6      6
     D*  WC890607
     D  FMX07                  7      7
     D*  WC890608
     D  FMX08                  8      8
     D*  WC890610
     D  FMX09                  9      9
     D*  WC890611
     D  FMX10                 10     10
     D*  WC890612
     D  FMX11                 11     11
     D*  WC890613
     D  FMX12                 12     12
     D*  WC890614
     D  FMX13                 13     13
     D*  WC890615
     D  FMX14                 14     14
     D*  WC890616
     D  FMX15                 15     15
     D*  WC890617
     D  FMX16                 16     16
     D*  WC890618
     D  FMX17                 17     17
     D*  WC890619
     D  FMX18                 18     18
     D  FMFECG                19     19
     D  FMINCG                20     20
     D  FMLSCG                21     21
     D  FMMACG                22     22
     D  FMPACG                23     23
     D  FMPBCG                24     24
     D  FMPRCG                25     25
     D*
     D                 DS
     D  XXDSCS                 1    200
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     Ipmen01
     I              ENdsc2                      ENdsc2A
     C****************************************************************
     C*
     C*  FUND MASTER KLIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER MASTER KLIST
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL KLIST
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  ENDORSEMENT FORM TABLE FILE
     C*
     C     TENKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
     C*
     C*  FUND AUXILIARY KLIST
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CARRIER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER ATTACHED ENDORSEMENTS FILE  (CO#/FND/EMP#/DIV/FYR/END#/SEQ#)
     C*
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEND#
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     MENKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXJOB
     C                   KFLD                    LDEND#
     C*
     C***  Begin Add  ***  04/10/06  *********************************
     C*  RATE SET CODE MASTER FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C***  End   Add  ***  04/10/06  *********************************
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    ENFORM        XKFORM
     C     *LIKE         DEFINE    FMEMST        XXEMST
     C     *LIKE         DEFINE    FMEAD1        XXEAD1
     C     *LIKE         DEFINE    FMEAD2        XXEAD2
     C     *LIKE         DEFINE    FMEAD3        XXEAD3
     C     *LIKE         DEFINE    FNPST         XXFNST
     C     *LIKE         DEFINE    FMCARR        XXCARR
     C     *LIKE         DEFINE    FMCPL#        XXCPL#
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C     *LIKE         DEFINE    MUSEQ#        XXSEQ#
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    ENJOB         XXJOB
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C*
     C*  DATE CHECK/CONVERSION PARM LISTS
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
     C*  ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
     C                   PARM                    QQJOB             6
     C                   PARM                    QWRT89            1
      *
      * Retrieve Client ID
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    ##CLT#            3
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQJOB         XXJOB
     C*
     C                   MOVEL     XXFORM        XKFORM
     C                   MOVE      *OFF          XWRT89            1
     C                   Z-ADD     *ZEROS        XXCNT             3 0
     C                   Z-ADD     *ZEROS        XXCNT1            3 0
     C*
      /Free
       In svLda;
      /End-Free
     C*
     C********************************************************************
     C*  MAINLINE
     C********************************************************************
     C*
      /Free
          formName = %subst(xxForm :1 :2) + ' ' + %subst(xxForm :3 :2) + ' ' +
           %subst(xxForm :5 :2) + ' ' + %subst(xxForm :7 :2) + ' ' +
           %subst(xxForm :9 :2);
          Exsr Scriptura;
      /End-Free

     C                   SELECT
     C*
     C*  GATHER INFO TO MERGE TO THE FORM
     C*
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
     C     XWRT89        IFEQ      *ON
     C                   EXSR      INCREC
     C                   ENDIF
     C                   ENDSL
     C*
     C                   MOVE      XWRT89        QWRT89
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C********************************************************************
     C*  GATHER INFO TO MERGE TO THE FORM
     C********************************************************************
     C     BULDIT        BEGSR
     C*
     C*  GET THE BASIC INFO
     C*
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
     C*
     C*  CLEAR OUT THE RECORD
     C*
     C                   CLEAR                   PFM01
     C*
     C*  LOAD BASIC WXFMP FIELDS
     C*
     C                   EXSR      BASINF
     C*
     C*  LOAD FORM SPECIFIC INFO
     C*
     C                   EXSR      OTHINF
     C*
     C*  WRITE A RECORD TO THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
     C*
     C     XWRT89        IFEQ      *ON
     C                   EXSR      WRTPMU
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFND - GET FUND INFORMATION
     C********************************************************************
     C     GETFND        BEGSR
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF
     C*
     C*  GET THE FUND AUXILIARY RECORD
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVE      F2CAR#        XXCARR
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETEMP - GET EMPLOYER INFORMATION
     C********************************************************************
     C     GETEMP        BEGSR
     C*
     C                   MOVE      *ZERO         XXPFDT
     C*
     C*  GET THE EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  BUILD UP ADDRESS LINES USING MAILING ADDRESS DATA
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        XXEAD1
     C                   MOVEL     EMMST         XXEMST
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXCSZ            40
     C                   CAT       EMMST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
     C*
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
     C*
     C     EMMAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMMAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMMAD2 <> ' '
     C*
     C                   ELSE
     C*
     C*  BUILD UP ADDRESS LINES USING PHYSICAL ADDRESS DATA
     C*
     C                   MOVEL(P)  EMPAD1        XXEAD1
     C                   MOVEL     EMPST         XXEMST
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXCSZ
     C                   CAT       EMPST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
     C*
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
     C*
     C     EMPAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMPAD2 <> ' '
     C*
     C                   ENDIF                                                  EMMPFL = 'M'
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  GET THE EMPLOYER DETAIL RECORD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   CLEAR                   XXST
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   MOVE      ELRTCD        XXRTCD
     C                   CLEAR                   RSJRST
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   ENDIF
     C*

      /Free
       Select;
       //*** begin chg ***  07/19/22  ***************************************
       //*****When ##clt# = '132';
       //*****   prMex =  prm132;
       //*****When xxSt = 'NJ';
       //*****   prMex =  prMexB;
       //*****When xxSt = 'CA';
       //*****   prMex =  prMexC;
       When ##clt# = '132' or xxSt = 'CA';
          prMex =  prm132;
       When xxSt = 'NJ' and ELpfdt >= 1150101;
          prMex =  prMexA;
       When xxSt = 'NJ';
          prMex =  prMexB;
       When ELpfdt >= 1170501;
          prMex =  prMexC;
       //*** end chg *****  07/19/22  ***************************************
       When ELpfdt >= 1150101;
          prMex =  prMexB;
       Other;
          prMex =  prMexA;
       Endsl;
       //*** begin add ***  07/19/22  ***************************************
       //* Determine if a client template is in use for premium schedule.
          Setgt ('S':'P':ELFnd:xxSt:ELPfdt) ppp00;
          Readpe(n) ('S':'P':ELFnd:xxSt) ppp00;
          If not %eof;
             If PPCltFrm# <> ' ';
                prMex = %trim(PPCltFrm#);
             Endif;
          Else;
             Setgt ('S':'P':ELFnd:'  ':ELPfdt) ppp00;
             Readpe(n) ('S':'P':ELFnd:'  ') ppp00;
             If not %eof;
                If PPCltFrm# <> ' ';
                   prMex = %trim(PPCltFrm#);
                Endif;
             Else;
                Setgt ('S':'P':000:xxSt:ELPfdt) ppp00;
                Readpe(n) ('S':'P':000:xxSt) ppp00;
                If not %eof;
                   If PPCltFrm# <> ' ';
                      prMex = %trim(PPCltFrm#);
                   Endif;
                Else;
                   Setgt ('S':'P':000:'  ':ELPfdt) ppp00;
                   Readpe(n) ('S':'P':000:'  ') ppp00;
                   If not %eof;
                      If PPCltFrm# <> ' ';
                         prMex = %trim(PPCltFrm#);
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;
       //*** end add *****  07/19/22  ***************************************
      /End-Free
     C*
     C                   ENDSR
      *
      *
     C********************************************************************
     C*  GETAGT - GET AGENT INFORMATION
     C********************************************************************
     C     GETAGT        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXAGT#
     C*
     C*  GET THE CURRENT AGENT HISTORY RECORD
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AHAGT#        XXAGT#
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C  N60AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFRM - GET ENDORSMENT FORM TABLE RECORD
     C********************************************************************
     C     GETFRM        BEGSR
     C*
     C*  GET THE ENDORSMENT FORM TABLE RECORD
     C*
     C     TENKY         CHAIN     PTEN01                             60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     TENKY         READE     PTEN01                                 60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETCAR - GET CARRIER INFORMATION
     C********************************************************************
     C     GETCAR        BEGSR
     C*
     C*  GET THE CARRIER INFO RECORD
     C*
     C     RPKY          CHAIN     PRP01                              60
     C*
     C                   MOVEL     RPCAP#        XXCPL#
     C*
     C                   ENDSR
     C********************************************************************
     C*  BASINF - LOAD BASIC WXFMP FIELDS
     C********************************************************************
     C     BASINF        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      XXCO#         FMCO#
     C                   MOVE      XXFND         FMFND
     C                   MOVE      XXFYR         FMFYR
     C                   MOVE      XXEMP#        FMEMP#
     C                   MOVE      XXDIV         FMDIV
     C                   MOVEL     XXFORM        FMFORM
      /Free
       If xxST = 'CA';
          FMc101 = 'CALIFORNIA';
       Endif;
      /End-Free
     C*
     C                   MOVE      ENEDDT        DSDAT7
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT
     C*
     C                   MOVE      LDEEDT        FMEEDT
     C*
     C                   Z-ADD     LDEND#        FMEND#
     C*
     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
     C*
     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
     C                   eval      fminnm = emname + emnam2
     C                   eval      fmifnm = emfdnm + emfdn2
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
      *
     C                   CALL      'SRPAMT'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXEMP#
     C                   PARM                    XXENMP
     C                   PARM      *BLANK        XXCLR             1
      *
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     XXENMP        FMPREM
     C                   MOVE      XXAGT#        FMAGT#
     C                   MOVEL     AGNAME        FMAGNM
     C                   MOVEL     AGPST         FMAGST
     C                   MOVEL     XXCPL#        FMCPL#
     C                   MOVE      XXCARR        FMCARR
     C                   MOVEL     XXEAD1        FMEAD1
     C                   MOVEL     XXEAD2        FMEAD2
     C                   MOVEL     XXEAD3        FMEAD3
     C                   MOVEL     XXEMST        FMEMST
     C*
     C                   MOVE      ELANDT        DSDAT7
     C                   MOVE      DSDD          FMANDA
     C                   MOVE      DSMM          FMANMO
     C*
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT
     C*
     C                   MOVEL     DSTIME        FMTIME
     C                   MOVEL     'A'           FMSTAT
     C                   MOVEL     Q1USER        FMIUSR
     C                   MOVEL     Q1PGM         FMIPGM
     C                   MOVE      FMDT          FMIDT
     C*
     C                   ENDSR
     C********************************************************************
     C*  OTHINF - LOAD FORM SPECIFIC INFORMATION
     C********************************************************************
     C     OTHINF        BEGSR
      *
      * Get PEO Information if PEO Group
      *
     C     FNPEOF        IFNE      *BLANK
     C                   MOVE      QQCO#         #5CO#
     C                   MOVE      QQFND         #5FND
     C                   MOVE      QQEMP#        #5EMP#
     C                   MOVE      QQDIV         #5DIV
     C                   MOVE      QQFYR         #5FYR
      *
     C                   CALL      'SRADD1'
     C                   PARM                    #5CO#             3 0
     C                   PARM                    #5FND             3 0
     C                   PARM                    #5EMP#            9 0
     C                   PARM                    #5DIV             5 0
     C                   PARM                    #5FYR             3 0
     C     XXNAME        PARM                    #5NAME          100
     C     XXFNAM        PARM                    #5FNAM          100
     C                   PARM                    #5EAD1           40
     C                   PARM                    #5EAD2           40
     C                   PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C                   PARM                    #5FEIN            9 0
     C                   PARM                    #5EADR           90
      *
     C                   ENDIF
      *
     C*
     C*  GET THE MID-TERM ENDORSEMENT WORKFILE RECORD
     C*
     C     MENKY         SETLL     PMEN01
     C     MENKY         READE     PMEN01                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ENSEQ#        IFNE      *ZERO
     C                   GOTO      NXTMEN
     C                   ENDIF
     C*
     C*  CLEAR ANY FIELDS MANIPULATED IN THIS SUB-ROUTINE TO AVOID CARRY
     C*  OVER DATA
     C*
     C                   MOVEL     *BLANKS       XXFLGS
     C                   MOVEL     *BLANKS       XXDSCS
     C                   MOVE      *ON           XXPRNT            1
     C*
     C*  SET THE APPROPRIATE MID-TERM ENDORSEMENT PRINT FLAG
     C*
     C                   SELECT
      * Don't provide employer name - see 70493
     c                   When      ldAddNamEnd = '1'
     c                   Eval      FMx01 = 'X'
     c                   Eval      FMincg = 'Y'
     C*-------------------------------------------------------------------------
     C     ENNCFL        WHENEQ    'Y'                                          NAME CHANGE
     C                   MOVE      'X'           FMX01                            WC890601
     C                   MOVEL     EMNAME        FMC401                           DESCRIPTION
     C                   MOVE      'Y'           FMINCG                           INS NAME CHG
     C*-------------------------------------------------------------------------
     C     ENFIFL        WHENEQ    'Y'                                          FED ID # CHANGE
     C                   MOVEL     ENFEN2        XXA2              2
     C                   MOVE      ENFEN2        XXA7              7
     C     'Federal '    CAT       'ID Num':1    FMC401                           DESCRIPTION
     C                   CAT       'ber Ch':0    FMC401
     C                   CAT       'ange':0      FMC401
     C                   CAT       XXA2:1        FMC401
     C*** Beg Del *** 04/22/22 *********************************************
     C***************    CAT       '-':0         FMC401
     C*** End Del *** 04/22/22 *********************************************
     C                   CAT       XXA7:0        FMC401
     C                   MOVE      'Y'           FMFECG                           FED ID CHG
     C*** Beg Add *** 04/22/22 *********************************************
       FMx01 = 'X';
     C*** End Add *** 04/22/22 *********************************************
     C*-------------------------------------------------------------------------
     C     ENTBFL        WHENEQ    'Y'                                          TYP OF BUSINESS CHG
     C                   MOVE      'X'           FMX09                            WC890610
     C                   MOVEL     'BUSI'        XXTBID
     C                   MOVEL(P)  ENTOB2        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        FMC401
     C                   ENDIF
     C                   MOVE      'Y'           FMLSCG                           LEGAL STAT CHG
     C*-------------------------------------------------------------------------
     C     ENMAFL        WHENEQ    'Y'                                          MAILING ADDR CHG
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      'X'           FMX05                            WC890605
     C                   ELSE
     C                   MOVE      'X'           FMX08                            WC890608
     C                   ENDIF                                                    EMSTRP
      *
     C                   MOVEL     ENMA12        FMC401                           DESCRIPTION 1
     C                   MOVEL     ENMA22        FMC402                           DESCRIPTION 2
     C*
     C                   MOVEL     ENMZP2        XXZIP5            5
     C                   MOVE      ENMZP2        XXZIP4            4
     C     ENMCT2        CAT(P)    ',':0         FMC403                           DESCRIPTION 3
     C                   CAT       ENMST2:1      FMC403
     C                   CAT       XXZIP5:1      FMC403
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP4:0      FMC403
     C                   ENDIF
     C*  FLOAT CSZ UP IF NECESSARY
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL     FMC403        FMC402
     C                   MOVEL     *BLANKS       FMC403
     C                   ENDIF
     C                   MOVE      'Y'           FMMACG                           MAIL ADDR CHG
     C*-------------------------------------------------------------------------
     C     ENPAFL        WHENEQ    'Y'                                          PHYSICAL ADDR CHG
     C                   MOVE      'X'           FMX08                            WC890608
     C                   MOVEL     ENPA12        FMC401                           DESCRIPTION 1
     C                   MOVEL     ENPA22        FMC402                           DESCRIPTION 2
     C*
     C                   MOVEL     ENPZP2        XXZIP5            5
     C                   MOVE      ENPZP2        XXZIP4            4
     C     ENPCT2        CAT(P)    ',':0         FMC403                           DESCRIPTION 3
     C                   CAT       ENPST2:1      FMC403
     C                   CAT       XXZIP5:1      FMC403
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP4:0      FMC403
     C                   ENDIF
     C*  FLOAT CSZ UP IF NECESSARY
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL     FMC403        FMC402
     C                   MOVEL     *BLANKS       FMC403
     C                   ENDIF
     C                   MOVE      'Y'           FMPACG                           PHYS ADDR CHG
     C*-------------------------------------------------------------------------
     C     ENILFL        WHENEQ    'Y'                                          INCR LMT CHANGE
     C                   MOVE      'X'           FMX11                            WC890612
     C                   MOVEL     ENID12        FMC401                           DESCRIPTION 1
     C                   MOVEL     ENID22        FMC402                           DESCRIPTION 2
     C                   MOVE      'Y'           FMPBCG                           PREM BEARING CHG
     C*-------------------------------------------------------------------------
     C     ENERFL        WHENEQ    'Y'                                          EXP MOD CHG
     C                   MOVE      'X'           FMX06                            WC890406
     C     SEEREV        CAT       PRMEX:1       FMC401
     C                   MOVE      'Y'           FMPBCG                           PREM BEARING CHG
     C*-------------------------------------------------------------------------
     C     ENIAPF        WHENEQ    'Y'                                          INT ADJ PREM CHG
     C     ENCRFL        IFEQ      'Y'
     C                   MOVE      'X'           FMX14                            WC890615
     C                   ELSE
     C                   MOVE      *BLANK        FMX14
     C                   ENDIF
     C                   MOVE      'X'           FMX15                            WC890616
     C                   MOVEL     ENDSC1        FMC401
     C                   MOVEL     ENDSC2A       FMC402
     C                   MOVEL     ENDSC3        FMC403
      *** beg add ***** 02/10/21 **********************
       FM2001 = ENdsc1 + ENdsc2a + ENdsc3;
      *** end add ***** 02/10/21 **********************
     C     SEEREV        CAT       PRMEX:1       FMC405
     C                   MOVE      'Y'           FMPBCG                           PREM BEARING CHG
     C*-------------------------------------------------------------------------
     C     ENDELF        WHENEQ    'Y'                                          DEL OF ENT/LOC
     C     ENAELF        OREQ      'Y'                                          ADD OF ENT/LOC
     C     ENCELF        OREQ      'Y'                                          CHG OF ENT/LOC
     C                   ADD       1             XXCNT
     C                   MOVE      'X'           FMX08                            WC890608
     C                   MOVEL     XXEXT1        FMC401
     C                   MOVEL     XXEXT2        FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC404
     C                   MOVE      'Y'           FMPACG                         PHYS ADDR CHG
     C     XXCNT         IFGT      1
     C                   MOVE      *OFF          XXPRNT
     C                   ENDIF
     C*-------------------------------------------------------------------------
     C     ENDLDF        WHENEQ    'Y'                                          DEL OF DIV
     C     ENADDF        OREQ      'Y'                                          ADD OF DIV
     C     ENRDDF        OREQ      'Y'                                          RESCISSION
     C                   MOVE      'X'           FMX08                            WC890608
     C                   MOVEL     XXEXT1        FMC401
     C                   MOVEL     XXEXT2        FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC404
     C                   MOVE      'Y'           FMPACG                           PHYS ADDR CHG
     C*-------------------------------------------------------------------------
     C     ENCIFL        WHENEQ    'Y'                                          CERT OF INS CHG
     C     ENAGT1        ANDNE     ENAGT2
     C                   MOVE      'X'           FMX07                            WC890607
     C                   MOVEL     AGNAME        FMC401
     C                   MOVE      'Y'           FMPRCG                         PRODUCER CHG
     C*-------------------------------------------------------------------------
      *** Beg Chg *** 03/11/24 *** 148WEQ **************************************
     c***                When      ENmsfl = 'Y' and
     c***                          xxSt = 'CA'
                         When      ENmsfl = 'Y' and
                                   (xxSt = 'CA' Or ##clt# = '148') ;
      *** End Chg *** 03/11/24 *** 148WEQ **************************************
     c                   Eval      FMc401 = ENdsc1
     c                   Eval      FMc402 = ENdsc2a
      *** beg add ***** 02/10/21 **********************
       FM2001 = ENdsc1 + ENdsc2a + ENdsc3;
      *** end add ***** 02/10/21 **********************
     c                   Eval      xxCnt1 += 1
     c                   If        xxCnt1 > 1
     c                   Eval      xxPrnt = *off
     c                   Endif
      *** Begin Add *** 07/31/25 *******************************
     c                   When      ENwvfl = 'Y' and
     c                             (xxSt = 'CA' Or ##clt# = '148')
     c                   Eval      FMc401 = ENdsc1
     c                   Eval      FMc402 = ENdsc2a
     c                   Eval      FM2001 = ENdsc1 + ENdsc2a + ENdsc3
      * FMX13 flag will tick "Item_3.D._Endorsement_Numbers_WC890614" checkbox on Policy Page
     c                   Eval      FMx13 = 'X'
     c                   Eval      xxCnt1 += 1
     c                   If        xxCnt1 > 1
     c                   Eval      xxPrnt = *off
     c                   Endif
      *** End   Add *** 07/31/25 *******************************
     C*** begin add ***  09/07/23  ********************************************
     C     ENNOFL        WHENEQ    'A'                                          Add of Officer
     C     ENNOFL        OREQ      'D'                                          Del of Officer
     C     ENNOFL        OREQ      'C'                                          Chg of Officer
     c                   Eval      FMc401 = offText
     C*** end add *****  09/07/23  ********************************************
     C                   OTHER
     C                   MOVE      *OFF          XXPRNT
     C                   ENDSL
     C*
     C*  WRITE THE OUTPUT RECORD
     C*

      /Free
       If xxPrnt = *On;
           xWrt89 = *On;
           FMtime += 1;
       Endif;
       Select;
       When ENblin = 'Additional Named Insured';
          outCall = 'XMLREQUEST';
          OutPrefix = 'WC';
          OutString = xxForm;
          InReturn = processType;
          outMode = 'E';
          outRrn = %Editc(binElRrn:'X');
          StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
          OutPrefix:OutString:'000':'000');
          Chain inXML XMLPREL1;
          If %found(XMLPREL1);
             XPdata = 'Y';
             XPdcid = %Trim(xxForm) + '.SCD';
             XPtype = %SubSt(xxForm:1:8);
             Update XP100;
             Open WXWXFMP;
             FMxml = XPxmlT;
             FMxmlT = XPxmlT;
             FMFORMF = formName;
      ***** Beg ADD *** 05/20/22 *******************************
             //*** Beg Chg *** 01/21/25 *******************************
             //*** FMC401 = 'See Attached Schedule';
             FMC401 = 'See Schedule of Named Insureds';
             //*** End Chg *** 01/21/25 *******************************
      ***** End ADD *** 05/20/22 *******************************
             Write FMRecord;
             Close WXWXFMP;
             In svLda;
             ldOwCg = '1';
             ldAddNamEnd = '1';
             ldFlowName = XPxml;
             ldaXML = XPfile;
             Out svLda;
             //*** Beg Add *** 01/21/25 *******************************
             Exsr Wrtwxmlenp1;
             //*** End Add *** 01/21/25 *******************************
          Endif;
       When xxPrnt = *On;
          Exsr WriteWorkFile;
       Endsl;
      /End-Free
     C*
     C     NXTMEN        TAG
     C     MENKY         READE     PMEN01                                 60
     C                   ENDDO                                                    *IN60  DOWEQ *OFF
     C*
     C                   ENDSR
     C****************************************************************
     C*  WRITE A RECORD TO THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
     C****************************************************************
     C     WRTPMU        BEGSR
     C*
     C*  DETERMINE THE CURRENT MID-YEAR ENDORSEMENT NUMBER
     C                   Z-ADD     LDEND#        XXEND#
     C     MUKY          SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             XXSEQ#
     C                   ELSE
     C     MUSEQ#        ADD       1             XXSEQ#
     C                   ENDIF
     C*
     C*  CLEAR OUT THE RECORD
     C                   CLEAR                   PMU01
     C*  LOAD THE RECORD
     C                   TIME                    DSDTTM
     C                   MOVE      XXCO#         MUCO#
     C                   MOVE      XXFND         MUFND
     C                   MOVE      XXEMP#        MUEMP#
     C                   MOVE      XXDIV         MUDIV
     C                   MOVE      XXFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      XXSEQ#        MUSEQ#
     C                   MOVEL     XXFORM        MUFORM
     C                   MOVEL     ENST          MUENST
     C                   MOVEL     ENDESC        MUDESC
     C                   MOVEL     ENdsc2        MUdsc2
     C                   MOVE      ELPFDT        MUEEDT
     C                   MOVEL     'E'           MUEXFL
     C                   MOVEL     ENMNAU        MUMNAU
     C                   MOVEL     ENPRNT        MUPRNT
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     DSTIME        MUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C*
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      MUDT          MUIDT
     C*
     C*  WRITE A RECORD TO THE EMPLOYER ATTECHED ENDORSEMENT FORMS FILE
     C*
     C                   WRITE     PMU01
     C*
     C                   ENDSR
     C********************************************************************
     C*  INCREC - WRITE A MERGED FORMS DATA RECORD
     C********************************************************************
     C     INCREC        BEGSR
     C*
     C     XXINCF        IFNE      *ON
     C                   MOVE      *ON           XXINCF            1
     C*
     C     '.inc(WC8'    CAT(P)    '90600':0     ICSTMT
     C                   CAT       '.B,':0       ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT
     C*
     C                   WRITE     PIC01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      ***** Beg Add *** 01/21/25 *******************************
       Begsr Wrtwxmlenp1;
        Open Wxmlenp1;
        Clear penxml;
        e1ENxmlkey =  FMxml;
        e1ENco#    =  FMco#;
        e1ENfnd    =  FMfnd;
        e1ENemp#   =  FMemp#;
        e1ENdiv    =  FMdiv;
        e1ENfyr    =  FMfyr;
        e1ENjob    =  xxJob;
        e1ENend#   =  FMend#;
        e1ENreq#   =  FMseq#;
        e1ENseq#   =  FMseq#;
        e1ENeed8   =  FMeedt;
        Monitor;
         e1ENeedt  =  %Dec(%Char(%Date(FMeedt:*Usa):*Cymd0):7:0);
         e1ENpdt8  =  %Dec(%Char(%Date(FMdt:*CYMD):*usa0):8:0);
        On-Error;
         e1ENeedt  =  0;
         e1ENpdt8  =  0;
        EndMon;
        e1ENfnnm   =  FNname;
        e1ENagt#   =  FMagt#;
        e1ENagnm   =  FMagnm;
        e1ENrusr   =  q1user;
        e1ENuser   =  q1user;
        e1ENpgm    =  q1pgm;
        e1ENtime   =  FMtime;
        e1ENdt     =  FMdt;
        e1ENdsc1   =  'ADDITIONAL NAMED INSURED';
        //*** Begin Add *** 07/31/25 *******************************
        e1ENwvfl = ENwvfl;
        //*** End   Add *** 07/31/25 *******************************
        write penxml;
        Close Wxmlenp1;
       Endsr;
      ***** End Add *** 01/21/25 *******************************
     C****************************************************************
      /Copy SCOPYMT02C
