     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE W0051R30
     H*==============================================================
     H*  DESCRIPTION:  Create/Print Addition or Deletion of Division/
     H*                Location midterm endorsement
     H*
     H*  CALLED FROM:  W8700R - Quote Transfer
     H*                W8393R - PEO Policy Import
     H*==============================================================
     H* REVISIONS:
     H*                                                               *
     H* 02/12/24 C:300 Support: 114122 (Sunz)                         *
     H*                PEO master renewal transfer creating Addition  *
     H*                of Division records in Darts                   *
     H*                Fix - Don't create new 'Quote - Addition of Division'
     H*                Endorsement record if it already exists for a Division
     H*
     H* 04/28/23 C:333 Support: 109150 (SUNZ) - retrofitting 105470 changes
     H* 01/18/23 C:340 Support: 105470 - AmWins only
     H*                Allow Term Upload with Cancel Date = Expiry Date.
     H*
     H* 11/18/20 C:180 Project WC2757
     H*                Recompiled only.
     H*
     H* 09/14/20 C:136 Project: WC2703
     H*                Suppress the printing of the location schedule
     H*                when called from Quote Transfer or PEO Policy
     H*                Import.
     H*
     H* 09/28/18 C:267 Support: 50562
     H*                Added new file WXDA1P/WXDA1L to store
     H*                newly added division# for use by STLETTER
     H*                to print darts remarks accordingly.
     H*
     H* 08/17/18 C:136 Support: 71746
     H*                Ensure deletion of division can write DARTS
     H*
     H* 08/08/17 C:136 Project: 150WAM
     H*                Add midterm processing for deletion of
     H*                division or location
     H*
     H* 06/14/17 C:136 Support: 47681
     H*               -Ensure location midterm add writes to division's
     H*                DARTS with effective date of the location
     H*               -Ensure that this program knows if it called for
     H*                a PEO import or not
     h*               -Temp disable call to Alternate Employer endorsement API
     h*                WA071R
     H*
     H* 03/08/17 C:136 Project: 148WAC
     H*                Add locations
     H*
     H* 11/05/08 C:190 Project: WC1700
     H*                Add call of WA071R in 'IssEnd' subroutine.
     H*
     H* 10/01/07 C:61  Project: WC1330  Seq# DR139/59595
     H*                When processing "ADD of Division" endorsement,
     H*                assign requesting user to be initial user that
     H*                entered employer division.
     H*
     H* 03/14/07  C:61 SUPPORT WA135 57087
     H*                Change endorsement date to be Policy Effective Date
     H*                of new Employer Division; instead of current date.
     H*
     H* 02/26/07  C:183 PROJECT 135WHS
     H*                 Check to see if this process is being called
     H*                 from the PEO Import porcess and if it is called
     H*                 from PEO Import then use the new description.
     H*
     H* 01/18/07  C:61 SUPPORT WA139 56630
     H*                Do not issue mid-term "ADD of Division"
     H*                endorsement if policy has not yet been issued.
     H*
     H* 09/15/06  C:61 SUPPORT WA135  54839
     H*                Use Policy FROM date as endorsement date if
     H*                only policy is in the future.
     H*
     H* 07/12/05  C:61 PROJECT WC1330
     H*                Create.
     H*==============================================================
      * Locations - co/fnd/emp/div/elid
     FWMALL     IF   E           K DISK    RENAME(PAL01:PAL00)
      * Endorse/DARTS Control Definition
     FWMDXL     IF   E           K DISK
      *
      * Group
     FWMFNL     IF   E           K DISK
      *
      * Group Auxiliary
     FWDF2L     IF   E           K DISK
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      *
      * Employer Endorsement Forms
     FWMMUL6    IF A E           K DISK
      *
      * Employer
     FWMEML     IF   E           K DISK    infds(emFDS)
      *
      * Endorsement
     FWMENL3    IF A E           K DISK
      *
      * Employer Policy Year
     FWDELL     IF   E           K DISK    infds(elFDS)
      *
     F*** Beg Add ****** 09/28/18 ************************************
      * New Division# - workfile
     FWXDA1L    IF   E           K DISK
     f                                     infds(da1fds)
     FWXDA1P    UF A E             DISK    RENAME(PDA01:PDA0P)
     F*** End Add ****** 09/28/18 ************************************
      *
      * Submission History
     FWMSHL1    IF   E           K DISK
      *
      * User Parameter
     FWMUPL     IF   E           K DISK
     fwtmyl     if   e           k disk    rename(pmy01:pmy00)
     f                                     infds(myFDS)
      *==============================================================
      * Prototypes
      *==============================================================
     d/copy sPrLetter
     d/copy sPrXML
     d/copy wprTable
     d DeleteDivision  pr
     d WriteDltDivEnd  pr                  extpgm('WTPOLDLTDV')
     d  inEMrrn                       9  0
     d  inELrrn                       9  0
     d  inMYrrn                       9  0
     d  inEEDT                        8  0
     d  inDelr                       50
      *=================================================
      * Stand Alone Fields
      *=================================================
     d flgEndType      s              1
     d intEndDiv       s              5  0
     d intEmRrn        s              9  0
     d intElRrn        s              9  0
     d intMyRrn        s              9  0
     d isUseScript     s              1n   inz
     d isDeletingLoc   s               n
     d processType     c                   const('Mid-Term Endorsement')
     d xxElid          s             10
     d xxMass          s              1
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxTbid          s              4    inz
     d xxKey           s             10    inz
     d x7eedt          s              7  0
     d x8eedt          s              8  0
      *** beg add ***** 02/12/24 **************************
     d intcount        s              8  0
      *** end add ***** 02/12/24 **************************

      *==============================================
      * Data Structure
      *==============================================
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26  0
     d ldPeoImpFlg                    1    overlay(svlda:29)
     d xxDart                         1    overlay(svlda:251)
     d xxRmCd                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     d emFDS           ds
     d  emRRN                397    400b 0
     d elFDS           ds
     d  elRRN                397    400b 0
     d myFDS           ds
     d  myRRN                397    400b 0
      *** beg add ***** 09/28/18 **********************************
     d da1fds          ds
     d  binda1rrn            397    400b 0
      *** end add ***** 09/28/18 **********************************
      *================================================================
      * constants
      *================================================================
     D DESC1           C                   CONST('QUOTE - Add a Divisi-
     D                                     on')
     D DESC2           C                   CONST('PEO Import - Add a D-
     D                                     ivision')
     D ADDDIV          C                   CONST('ADDITION OF DIVISION-
     D                                      ')
     D ADDLOC          C                   CONST('ADDITION OF ENTITY/-
     D                                     LOCATION(S)')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * Submission History
      *
     C     SHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
     C                   KFLD                    XXTYPE            1
      *
      * Group
      *
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      *  Group Auxiliary
      *
     C     F2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
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
      *  ENDORS/DARTS CONTROL DEFINITION
      *
     C     DXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      * Location
      *
     C     ALKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    XXELID
      *
      *  Employer
      *
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
     C     EMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
      *  Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
     C     ELKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    SVDIV
      *
      *  EMPLOYER ENDORSEMENT FORMS
      *
     C     MUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    ELFYR
     C                   KFLD                    KKEMP#
      *
     C     MUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    ELFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKEND#
      *
     C***** beg add ***** 09/28/18 ***********************
      * WXDA1L workfile
     C     DAKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    ENEND#
     C                   KFLD                    ENAGT2
     C***** end add ***** 09/28/18 ***********************
      *
      *  EMPLOYER ENDORSEMENT REQUESTS
      *
     C     ENKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    XXEND#
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMDIV         SVDIV
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD2        XXADR2
     C     *LIKE         DEFINE    EMPAD2        XXADR3
     C     *LIKE         DEFINE    EMCO#         ##CO#
     C     *LIKE         DEFINE    EMFND         ##FND
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    ELFYR         KKFYR
     C     *LIKE         DEFINE    MUEND#        KKEND#
     C     *LIKE         DEFINE    MUEND#        XXEND#
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@ENDT            8
     C                   PARM                    @@ELID           10
     C                   Parm                    @@EndType         1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM      KKCO#         ##CO#
     C                   PARM      KKFND         ##FND
     C                   PARM      KKEMP#        ##EMP#            9 0
     C                   PARM      KKDIV         ##DIV             5 0
     C                   PARM      KKFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *================================================================
      *  MAIN
      *================================================================
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@EMP#        KKEMP#
     C                   MOVE      @@DIV         KKDIV
     C                   MOVE      @@ENDT        X8ENDT            8 0

      /Free
       //*** beg add ***** 02/12/24 **************************
       Exec Sql Set Option Commit = *None;
       //*** end add ***** 02/12/24 **************************
       If Q1prms > 5;
          xxElid = @@Elid;
       Else;
          xxElid = *Blanks;
       Endif;

       If Q1prms > 6;
          flgEndType = @@EndType;
       Else;
          flgEndType = *Blanks;
       Endif;
      /End-Free
     C                   CALL      COMAPI        COMPL
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE            7 0
      *
     C                   Z-ADD     X8ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7ENDT            7 0
      *
     C                   MOVE      'N'           XXPROC            1
      *
     C     ELKY          SETLL     PEL01
     C     ELKY          READE     PEL01                                  60
      *
B5   C     *IN60         DOWEQ     *OFF
      *
      *  ISSUE ENDORSEMENT FOR POLICY YEAR IN WHICH THE ENDORSEMENT
      *  EFFECTIVE DATE FALLS
      *
      *** Chg Beg - 01/18/23 *************************************************************
      *
     C     X7ENDT        IFGE      ELPFDT
     C     X7ENDT        ANDLE     ELPTDT
     C**   X7ENDT        ANDLT     ELPTDT
      *
      *** Chg End - 01/18/23 *************************************************************
      *** beg add ***** 02/12/24 **************************
    C* Write 'Addition of Division' records, If not already existing
      /free
       intcount = 0;
       Exec SQL Select Count(*) Into :intcount From Wmmup
        Where MUco# = :kkco# and MUfnd = :kkfnd and MUemp# = :kkemp#
        and MUdiv = :kkdiv and MUform = 'MID-YEAR'
        and MUdesc = 'Quote - Addition of Division';
       if intcount = 0;
      /end-free
      *** end add ***** 02/12/24 **************************
      *
      *  ONLY ISSUE MID-TERM ENDORSEMENT IF POLICY PAGE HAS BEEN ISSUED
      *
     C                   MOVE      'P'           XXTYPE
     C     SHKY          CHAIN     PSH01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SHSTAT        ANDGT     '2'
     C                   EXSR      ISSEND
     C                   MOVE      'Y'           XXPROC            1
     C                   LEAVE
     C                   ENDIF
      *** beg add ***** 02/12/24 **************************
      /free
       Endif;
      /end-free
      *** end add ***** 02/12/24 **************************
      *
     C                   ENDIF
      *
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO
      *
      *** Beg Del ***** 09/14/20 ***************************
      * Don't look for prior years. It doesn't make sense.
     C***     XXPROC        IFEQ      'N'
     C***     ELKY          SETGT     PEL01
     C***     ELKY          READPE    PEL01                                  60
     C***     *IN60         IFEQ      *OFF
      ****
      ****  ONLY ISSUE MID-TERM ENDORSEMENT IF POLICY PAGE HAS BEEN ISSUED
      ****
     C***                   MOVE      'P'           XXTYPE
     C***     SHKY          CHAIN     PSH01                              60
     C***     *IN60         IFEQ      *OFF
     C***     SHSTAT        ANDGT     '2'
     C***                   EXSR      ISSEND
     C***                   ENDIF
     C***                   ENDIF
     C***                   ENDIF                                                  XXPROC
      *** End Del ***** 09/14/20 ***************************
      *
     C                   SETON                                        LR
      *=================================================================
      * ISSEND - Issue mid-term "Add of Division" endorsement
      *=================================================================
     C     ISSEND        BEGSR

      /Free
       Select;
       When flgEndType = 'D';
          x7eedt = x7endt;
          If xxElid <> *Blanks;
             Chain alky pal00;
          Endif;
       When xxElid <> *blanks;
          Chain alky pal00;
          x7eedt = ALefdt;
       Other;
          x7eedt = ELpfdt;
       Endsl;

       Monitor;
          x8eedt = %dec(%char(%date(x7eedt:*cymd):*usa0):8:0);
       On-Error;
       EndMon;
      /End-Free
      *
      *  GET CURRENT ENDORSEMENT # FOR FYR BEING PROCESSED
      *
     C                   Z-ADD     ELFYR         KKFYR
     C                   MOVE      *HIVAL        KKEND#
     C     MUKY1         SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60
      *
B7   C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         MUEND#
E7   C                   ENDIF
      *
      *  LOAD ENDORSEMENT NUMBER TO LOCAL DATA AREA POSITION 1 THRU 5
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C     MUEND#        ADD       1             XXEND#
     C                   MOVE      XXEND#        LDEND#
     C                   MOVE      X8EEDT        LDEEDT
     C                   MOVE      ELFYR         LDFYR
     C                   OUT       *DTAARA
      *
      *  CREATE AN EMPLOYER ENDORSEMENT RECORD
      *
     C                   MOVE      ELCO#         MUCO#
     C                   MOVE      ELFND         MUFND
     C                   MOVE      ELEMP#        MUEMP#
     C                   MOVE      ELDIV         MUDIV
     C                   MOVE      ELFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      *ZERO         MUSEQ#
     C                   MOVE      *BLANKS       MUFORM
     C                   MOVEL     'MID-YEAR'    MUFORM
     C                   MOVEL     *BLANKS       MUENST

      /Free
       Select;
       When flgEndType = 'D' and xxElid <> *Blanks;
          MUdesc = 'PEO Import - Deletion of Location';
          MUdsc2 = 'PEO Import - Deletion of Location';
          MUppgm = 'W0051R65';
       When flgEndType = 'D' and xxElid =  *Blanks;
          MUdesc = 'PEO Import - Deletion of Division';
          MUdsc2 = 'PEO Import - Deletion of Division';
          MUppgm = 'W0051R20';
       When xxelid <> *blanks;
          MUdesc = 'PEO Import - Addition of Location';
          MUdsc2 = 'PEO Import - Addition of Location';
          MUppgm = 'W0051R15';
       When ldPeoImpFlg = 'Y';
          MUdesc = 'PEO Import - Addition of Division';
          MUdsc2 = 'PEO Import - Addition of Division';
          MUppgm = 'W0051R19';
       Other;
          MUdesc = 'Quote - Addition of Division';
          MUdsc2 = 'Quote - Addition of Division';
          MUppgm = 'W0051R19';
       Endsl;
      /End-Free
     C                   MOVE      X7EEDT        MUEEDT
     C                   MOVEL     'I'           MUEXFL
     C                   MOVEL     'M'           MUMNAU
     C                   MOVEL     'N'           MUPRNT
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C     DSTIME        ADD       1             MUTIME
     C                   MOVE      X7DATE        MUDT
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      X7DATE        MUIDT
     C     DSTIME        ADD       1             MUTIME
     C                   WRITE     PMU01
      *
      *  Write Endorsement records
      *
      /Free
       intEndDiv = kkdiv;
       Select;
       When flgEndType = 'D' and xxElid =  *Blanks;
          DeleteDivision();
       //*** beg add ***** 08/17/18 ***********************
          Setgt  enKy pen01;
          Readpe enKy pen01;
       //*** end add ***** 08/17/18 ***********************
       When flgEndType = 'D' and xxElid <> *Blanks;
          isDeletingLoc = *On;
          Exsr CkChgs;
       Other;
          Exsr CkChgs;
       Endsl;

       isUseScript = UseScriptura(processType);
       If isUseScript;
          xxProcType = processType;
          In *Lock *Dtaara;
          If ENrmcd <> *Blanks;
             xxDart = 'Y';
             xxRmCd = ENrmcd;
          Else;
             xxDart = 'N';
             xxRmCd = *blanks;
          Endif;
          Out *dtaara;
          xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');
       Endif;
      /End-free
      *
     C                   MOVE      ELCO#         QQCO#             3
     C                   MOVE      ELFND         QQFND             3
     C                   MOVE      ELEMP#        QQEMP#            9
     C                   MOVE      intEndDiv     QQDIV             5
     C                   MOVE      ELFYR         QQFYR             3
     C                   MOVE      Q1JOBN        QQJOBN            6
     C                   MOVE      XXEND#        QQEND#            5
     C                   MOVEL     ##OUTQ        QQOUTQ           10
      *
     C                   CALL      'W7050C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQJOBN            6
     C                   PARM                    QQEND#            5
     C                   PARM                    QQOUTQ           10
      *** beg add ***** 09/14/20 ************************************
     C                   PARM      'Y'           QQSUPPSCH         1
      *** end add ***** 09/14/20 ************************************

      *
     C                   ENDSR                                                  ISSEND
      *==============================================================
      * CKCHGS
      *==============================================================
     C     CKCHGS        BEGSR
      *
      *  DETERMINE THE STATE REPORTING DIVISION
      *
     C     FNKY          CHAIN     PFN01                              60
     C     DXKY          CHAIN     PDX01                              60
     C                   Z-ADD     *ZEROS        SVDIV
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         SVDIV
     C                   LEAVE
     C                   ENDIF
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
      *  SETUP FOR NEXT REQUEST NUMBER
      *
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      *
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      SVDIV         ENDIV
      *
      *  SAVE VALUE OF DIVISION THAT WAS ADDED FOR W7418R PROCESSING
      *
     C                   MOVE      EMDIV         ENAGT2
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
      *
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMNAM2        ENENM2
     C                   MOVE      EMFDN2        ENEFN2
      *
      * Get Employer Contact Names
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENCON1
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENATT1
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENCN31
      *
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
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
      *
     C     F2KY          CHAIN     PF201                              60
     C                   MOVE      F2CAR#        ENCAR1
     C                   MOVE      F2CAR#        ENCAR2

      * Get policy from date from state reporting division

     C     ELKY2         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
     C     ELKY2         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   Eval      intEndDiv = ELdiv
     C                   ENDIF                                                  *IN60
     C
     C                   MOVEL     FNNAME        ENFNNM
     C                   CALL      AGTAPI        AGTPL
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL(P)  *BLANKS       ENRUSR
     C     EMIUSR        CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        ENRUSR
     C                   MOVEL(P)  ADDDIV        ENDSC1
     C                   MOVEL     EMNAME        ENNAM2
     C                   MOVEL     EMFDNM        ENFDN2
     C                   MOVEL     EMNAME        ENANM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENANM1
     C                   MOVEL     EMFDNM        ENAFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENAFN1
     C                   MOVE      EMNAM2        ENANM2
     C                   MOVE      EMFDN2        ENAFN2
     C                   MOVEL     ENCON1        ENCON2
     C                   MOVEL     ENATT1        ENATT2
     C                   MOVEL     ENCN31        ENCN32
     C                   MOVE      EMFEIN        ENFEN2
     C                   MOVEL     EMTPOB        ENTOB2
     C                   MOVEL     EMMAD1        ENMA12
     C                   MOVEL     EMMAD2        ENMA22
     C                   MOVEL     EMMCTY        ENMCT2
     C                   MOVEL     EMMST         ENMST2
     C                   MOVEL     EMMZIP        ENMZP2
     C                   MOVEL     EMPAD1        ENPA12
     C                   MOVEL     EMPAD2        ENPA22
     C                   MOVEL     EMPCTY        ENPCT2
     C                   MOVEL     EMPST         ENPST2
     C                   MOVEL     EMPZIP        ENPZP2
     C                   MOVEL     EMCNTY        ENCNT2
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
      *
      *  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      *
     C     EMPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4:0      XXADR3
     C                   ENDIF
      *
     C                   ELSE                                                   LOAD MAIL ADDR
      *
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4:0      XXADR3
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       ENBLIN
      * Write division
     C                   IF        XXELID = *BLANKS
      *
     C                   MOVE      'Y'           ENADDF
     C                   MOVEL(P)  DXDVAD        ENRMCD
      *
      *  EMPLOYER NAME
     C                   EVAL      ENALIN = EMNAME
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  EMPLOYER FEDERAL NAME
     C                   EVAL      ENALIN = EMFDNM
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  EMPLOYER ADDRESS 1
     C                   EVAL      ENALIN = XXADR1
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  EMPLOYER ADDRESS 2
     C     XXADR2        IFNE      *BLANKS
     C                   EVAL      ENALIN = XXADR2
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *
      *  EMPLOYER CITY/ST/ZIP
     C                   EVAL      ENALIN = XXADR3
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      * Else write location
     C                   ELSE

      /Free
       ENdiv = ALdiv;
       intEndDiv = ALdiv;
       Select;
       When isDeletingLoc and ALeltp = 'L';
          ENdsc1 = 'Deletion of Location';
          ENrmcd = DXlcdl;
          ENdelf = 'Y';
          ENeldd = ALdldt;
          ENeldr = ALdlrs;
          ENeltp = 'L';
       When isDeletingLoc and ALeltp = 'E';
          ENdsc1 = 'Deletion of Entity';
          ENrmcd = DXendl;
          ENdelf = 'Y';
          ENeldd = ALdldt;
          ENeldr = ALdlrs;
          ENeltp = 'E';
       When ALeltp = 'L';
          ENdsc1 = addLoc;
          ENrmcd = DXlcad;
          ENaelf = 'Y';
       When ALeltp = 'E';
          ENdsc1 = addLoc;
          ENrmcd = DXenad;
          ENaelf = 'Y';
       Endsl;
      /End-Free

      *  EMPLOYER NAME
     C                   MOVEL     ALNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C                   MOVEL     ALFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C     ALPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C                   MOVEL     ALPAD1        XXADR1
     C                   MOVEL     ALPAD2        XXADR2
     C                   MOVE      ALPZIP        XXZIP
     C     ALPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       ALPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4:0      XXADR3
     C                   ENDIF
      *
     C                   ELSE                                                   LOAD MAIL ADDR
      *
     C                   MOVEL     ALMAD1        XXADR1
     C                   MOVEL     ALMAD2        XXADR2
     C                   MOVE      ALMZIP        XXZIP
     C     ALMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       ALMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4:0      XXADR3
     C                   ENDIF
      *
     C                   ENDIF
      *  EMPLOYER ADDRESS 1
     C                   MOVEL     XXADR1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *
      *  EMPLOYER CITY/ST/ZIP
     C                   MOVEL     XXADR3        ENALIN
     C                   WRITE     PEN01

      /Free
       If flgEndType = 'D';
          ENalin = 'Federal ID #: ' + %Editc(ALfein:'X');
          Write pen01;
          xxTbId = 'BUSI';
          xxKey  = ALtpob;
          ENalin = GetGenCodeDs(ALco#:xxTbId:xxKey);
          Write pen01;
          ENalin = 'Ent/Loc ID #: ' + ALelid;
          Write pen01;
          ENalin = ALdlrs;
          Write pen01;
       Endif;
      /End-Free

     C                   ENDIF
      *
       //*** beg add ***** 09/28/18 ***********************
       // For a division add in mid-term endorsement,
       //   add rcd in workfile for future use
      /Free
       If ENADDF = 'Y';
          Chain DAKY pda01;
          If NOT %found;
             Clear pda0p;
             PDCO#  = ENCO# ;
             PDFND  = ENFND ;
             PDEMP# = ENEMP#;
             PDDIV  = ENDIV ;   //State reporting division
             PDFYR  = ENFYR ;
             PDNWDV = ENAGT2;   //new division#
             PDEND# = ENEND#;
             PDNAME = ENNAM1;   //employer name
             PDEFDT = ENEEDT;
             PDJOBN = Q1JOBN;
             PDUSER = Q1USER;
             PDPGM  = Q1PGM ;
             PDTIME = ENTIME;
             PDDT   = ENDT;
             PDNJBN = *ZERO;

             Write  pda0p;
             unlock wxda1p;
          EndIf;
       EndIf;
      /end-free
       //*** end add ***** 09/28/18 ***********************

     C                   ENDSR                                                  CKCHGS
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
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
     C                   ENDIF
      *
     C                   ENDSR                                                  SRCONT

      *===============================================================
      * Delete Division
      *===============================================================
     p DeleteDivision  b
     d DeleteDivision  pi

     d  strDltReason   s             50

      /Free
       Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
       Chain (ELco#:ELfnd:200) pmy00;
       intEmRrn = emRRN;
       intElRrn = elRRN;
       intMyRrn = myRRN;
       strDltReason = 'PEO Import - Deletion of Division';
       WriteDltDivEnd(intEMrrn:intELrrn:intMYrrn:x8EnDt:strDltReason);
      /End-Free

     p DeleteDivision  e

