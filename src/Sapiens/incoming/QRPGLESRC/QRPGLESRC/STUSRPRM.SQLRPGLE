     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
      //
     H/TITLE STUSRPRM - User Parameters
      //**********************************************************************
      // Revisions:                                                          *
      *                                                                      *
      * 05/30/24  C:344 Support: 115628                                      *
      *                 - Update Group# for Worker's Compensation & Managed  *
      *                 Care based on the contents of the flags set in the   *
      *                 Application System file.                             *
      *                 - Validate Group# when a new group# is entered.      *
      *                                                                      *
      * 04/10/24  C:079 Ticket: 99/115388                                    *
      *                 If remark code = 0 then set to blanks                *
      *                                                                      *
      * 04/08/24  C:344 Support: 115361                                      *
      *                 Add a deletion confirmation pop up message to        *
      *                 warn user before deleting a user parameter.          *
      *                                                                      *
      * 12/06/23  C:333 Support: 114714                                      *
      *                 AIC Assistance locating Service By address.          *
      *                 DSPF Changes:                                        *
      *                  Commented onFocus, Bind choice selection Criteria   *
      *                                                                      *
      * 11/06/23  C:333 Project: WC2926                                      *
      *                 Policy copy and search enhancements                  *
      *                 - DSPF : Added default search Tab.                   *
      *                                                                      *
      * 06/01/23  C:300 Support: 110456                                      *
      *                 Filter applied to the "Users" table is  being cleared*
      *                 upon Edit/Add any record in the grid.                *
      *                 In DSPF: Changed the Persist State property to TRUE  *
      *                                                                      *
      * 04/11/22 C:333 Project: SY434                                        *
      *                - Update Grids for Sorting Filtering Upgrade          *
      *                                                                      *
      * 01/25/22  C:079 Project: 133WSP                                      *
      *                 UW Allow Update of Staff Codes in Inquiry Mode       *
      *                                                                      *
      * 06/03/19  C:295 Support: 79864                                       *
      *                 Changes in display file detail box of users Grid.    *
      *                                                                      *
      * 02/14/19  C:300 Support: 76693 (CoveRisk)                            *
      *                 User Set-up                                          *
      *                 Update WCMS Group Restriction Flag edits             *
      *                                                                      *
      * 12/14/18  C:079 Support: 99/75374                                    *
      *                 The fix for history (74178) caused WMUPP not to      *
      *                 get updated each time.                               *
      *                                                                      *
      * 10/29/18  C:079 Support: 99/74178                                    *
      *                 Fix the logic that writes to history file SMUHP      *
      *                 Update SMUPP before reading WMUPP                    *
      *                                                                      *
      * 10/23/18  C:180 Project 129WVH                                       *
      *                 Add check box for Contacts security.                 *
      *                                                                      *
      * 05/16/18  C:33  Support 70713                                        *
      *                 Only allow upper case security types                 *
      *                                                                      *
      * 02/28/18  C:33  Support 64683                                        *
      *                 Check for open files before using                    *
      *                                                                      *
      * 10/16/17  C:33  Support 50271                                        *
      *                 Delete team records when deleting records            *
      *                                                                      *
      * 10/05/17  C:62  Support 49999(133)                                   *
      *                 Remove darts security level edit.                    *
      *                                                                      *
      * 08/08/17  C:33  Support 48597                                        *
      *                -Use appropriate comparions for security filters      *
      *                                                                      *
      * 05/30/17  C:33  Support 47421                                        *
      *                -Display/allow access to iDARTS security level        *
      *                -Remove obsolete DARTS flags
      *                -Add 'iDARTS Administration' link
      *                -Process only DARTS when called as DARTS security
      *                  maintenance
      *                                                                      *
      * 04/25/17  C:33  Support 46857                                        *
      *                 Update deletion routine.                             *
      *                                                                      *
      * 03/17/17  C:062 Support 46223                                        *
      *                 Don't set company restriction if not single.         *
      *                                                                      *
      * 02/14/17  C:062 Support 45667                                        *
      *                 Make sure and read all change records                *
      *                                                                      *
      * 10/28/16  C:309 Support:43941 Added screen header and fix grid header*
      *                        (Screen changes only)                         *
      //                                                                     *
      //**********************************************************************
      //                                                                    *
     fSTUSRPRM  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflUsrPrm:scSflRRN)
     f                                     sfile(sflRstWCMS:scSflRRNWC)
     f                                     sfile(sflRstWCMC:scSflRRNMC)
     fStActpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      //  Physical file definition (for maintaining record)
     FSMUPP     UF A E             disk    usropn
     F                                     rename(PUP01:sup0p)
     F                                     infds(upFdsP)
      //
     FSSYLF     IF   E           K disk
      //
     FMMOPP     IF   E             disk    usropn
     F                                     rename(pop01:mop0p)
     FMMOPL     IF   E           K disk    usropn
     F                                     rename(pop01:mop00)
     FSMUPL     IF   E           K disk    usropn
     F                                     rename(pup01:sup00)
     F                                     infds(upFds)
     FSMUPL1    IF   E           K disk    usropn
     F
     FSMCOL     IF   E           K disk    usropn
     F                                     rename(pco01:sco00)
     FSMUHP     O    E             disk    usropn
     F                                     rename(puh01:suh0p)
     FSMAZP     UF A E             disk    usropn
     F                                     rename(paz01:saz0p)
     FSMAZL     UF   E           K disk    usropn
     F                                     rename(paz01:waz00)
     F                                     infds(wAzFds)
     FSMAZL1    UF   E           K disk    usropn
     F                                     rename(paz01:maz01)
     F                                     infds(mAzFds)
     FWMEMP     IF   E             disk    usropn
     F                                     rename(pem01:wem0p)
     FWMEML     IF   E           K disk    usropn
     F                                     rename(pem01:wem00)
     FWMFNL     IF   E           K disk    usropn
     F                                     rename(pfn01:wfn00)
     FWMAAP     IF   E             disk    usropn
     F                                     rename(paa01:waa0p)
     FWMAGP     IF   E             disk    usropn
     F                                     rename(pag01:wag0p)
     FWMOFL     IF   E           K disk    usropn
     F                                     rename(pof01:wof00)
     FWMUPL     UF A E           K disk    usropn
     F                                     rename(pup01:wup00)
     FFMSML     IF   E           K disk    usropn
     F                                     rename(psm01:fsm00)
     FFMFNL     IF   E           K disk    usropn
     F                                     rename(pfn01:ffn00)
     FFMEML     IF   E           K disk    usropn
     F                                     rename(pem01:fem00)
     FFMSMP     IF   E             disk    usropn
     F                                     rename(psm01:fsm0p)
     FFMFNP     IF   E             disk    usropn
     F                                     rename(pfn01:ffn0p)
     FFMEMP     IF   E             disk    usropn
     F                                     rename(pem01:fem0p)
     FSMSLP     UF A E             disk    usropn
     F                                     rename(psl01:ssl0p)
     FSMSLL2    UF   E           K disk    usropn
     F                                     rename(psl01:ssl02)
     FSMCGL     IF   E           K disk    usropn
     F                                     rename(pcg01:scg00)
     FSTCTL1    IF   E           K disk    usropn

     dpsds           esds                  extname(szq1p)
     ddsSMUPPprv       ds                  likerec(sup0P:*input) inz
     ddsSMUPPnew       ds                  likerec(sup0P:*input) inz

      *entry
     d StUsrPrms       pr                  Extpgm('STUSRPRM')
     d  inCall                        1a
     d  inAvlMods                    50a   options(*nopass)                     Comma separated list
     d StUsrPrms       pi
     d  inCall                        1a
     d  inAvlMods                    50a   options(*nopass)                     Comma separated list

     ddspMode          s              1    inz('0')
     dedtMode          s              1    inz('0')

      // Prototypes
     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrGenErr
     d/copy sPrModule
     d/copy sPrString
     d/copy sPrUsrPrms
     d/copy wPrOffice

     DChkDTM           pr             1n

     DChkRestriction   pr             1n
     d inType                         4    const

     DClearErrCfg      pr
     DClearErrRst      pr

     dDltRestrictions  pr
     D inCo#                          3  0 const
     d inUser                        10    const

     DErrChkCfg        pr             1n
     D inRRN                          9  0 options(*nopass)
     DErrChkDARTS      pr             1n
     D inRRN                          9  0 options(*nopass)
     DErrChkRst        pr             1n
     D inType                         4    const
     D inRRN                          9  0 const options(*nopass)
     DErrChkSYSM       pr             1n
     D inRRN                          9  0 options(*nopass)
     DErrChkWCMC       pr             1n
     D inRRN                          9  0 options(*nopass)
     DErrChkWCMS       pr             1n
     D inRRN                          9  0 options(*nopass)

     dFilesClose       pr
     dFilesOpen        pr

     dMain             pr

     dProgramStart     pr

     dUserFilter       pr
     d inMode                        10    const options(*nopass)
     d
     dRstUpdate        pr
     d inMode                        10    const
     d inType                         4    const
     d inRRN                          9  0 const options(*nopass)

     dUserUpdate       pr
     d inRRN                          9  0 const
     d inMode                        10    const

     d ValidRstAdd     pr             1n
     d  inType                        4    const options(*nopass)

     D CheckFolder     pr                  extpgm('SVALFLR')
     D  inFolder                     80    const
     D  outRtcd                       1

     D CheckObj        pr                  extpgm('SRCHKOBJ')
     D  inObj                        10    const
     D  inType                        7    const
     D  outErr                        1n

     D GetSysVals      pr                  extpgm('SA001R')
     D                                1                                         WC installed
     D                                1                                         MC installed
     D                                1                                         WC interface
     D                                1                                         MC AR installed
     D                                1                                         Network installed
     D                                1                                         Pre-cert installed
     D                                1                                         Med Mgmt installed
     D                                1                                         EDI installed
     D                                1                                         WC DARTS key
     D                                1                                         Rehab installed
     D                                1                                         MS Support
     D                                1                                         Fee Schedule
     D                                1                                         Provider Bills
     D                                1                                         Non-PDS WC
     D                                1                                         Outome/Savings
     D                                1                                         Auto-Release
     D                                1                                         Corect Coding
     D                                1                                         Code Review
     D                                1                                         ProMatch
     D                                1                                         Medicare CR
     D                                1                                         Frequency
     D                                1                                         CR Auto Complete
     D                                1
     D                                1                                         Par/Non-Par/Both
     D                                1                                         Multi-Group Restrict

     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  cltId                         3

     D RtvISeries      pr                  extPgm('SRRTVUSR')
     D  outUsr                       10
     D  inUsrNm                      50
     D  inOutQ                       10
     D  inError                       1n

     D UpdateRecent    pr                  extpgm('STUPDRCNT')
     d  inRRN                         9    const
     d  inType                        1    const
     d  inQte                         1    const

     DdsFileInfo       ds
     D rrn                   397    400b 0
     DupFdsP           ds                  likeds(dsFileInfo)
     DupFds            ds                  likeds(dsFileInfo)
     DmAzFds           ds                  likeds(dsFileInfo)
     DwAzFds           ds                  likeds(dsFileInfo)

     d##AR             s              1
     d##ARLS           s              1
     d##CCOD           s              1
     d##CRVW           s              1
     d##CRAC           s              1
     d##EDI            s              1
     d##err            s              1n
     d##FREQ           s              1
     d##FSCH           s              1
     d##MCIF           s              1
     d##MCRV           s              1
     d##MED            s              1
     d##MGRI           s              1
     d##MSS            s              1
     d##NPWC           s              1
     d##NTRK           s              1
     d##OD             s              1
     d##PRVB           s              1
     d##PROM           s              1
     d##PARN           s              1
     d##PREO           s              1
     d##RHAB           s              1
     d##UR             s              1
     d##WCDK           s              1
     d##WCIF           s              1
     d##WCIN           s              1
     dcltID            s              3
     ddecSflMC#        s              5  0
     ddecSflWC#        s              5  0
     disAddMode        s              1n
     disCpyMode        s              1n
     disDspMode        s              1n
     disEdtMode        s              1n
     disOV             s              1n
     disProfileExists  s              1n
     doutMessage       s            256
     doutTitle         s             50
     doutBtnText       s             10
     dstrAvlMods       s             50a
     dstrHoldStat      s                   like(scStat)
      //*** Beg Add *** 04/08/24 *** 115361 ********************************
       dcl-s  strMsg char(200) inz;
      //*** End Add *** 04/08/24 *** 115361 ********************************

      /Free
       Exec SQL
        set option commit=*none;

       strAvlMods = 'WCMC,WCMS,DARTS';
       If %parms >= 2 and inAvlMods <> *blanks;
          strAvlMods = Upper(inAvlMods);
       Endif;

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       FilesClose();

       *InLr = *on;
      /end-free
       //===================================================================
       // Check for DTM install flag
       //===================================================================
     pChkDTM           b
     DChkDTM           pi             1n
     d useDTM          s              1
     C                   CALL      'DTMCHK'
     C                   PARM      *BLANKS       useDTM
      /Free
       Return useDTM = 'Y';
      /End-free
     pChkDTM           e
       //===================================================================
       // Check for active restriction recordsp
       //===================================================================
     pChkRestriction   b
     DChkRestriction   pi             1n
     d inType                         4    const

     dstrAppl          s                   like(AZappl)

      /Free
       Select;
       When inType = 'WCMC';
          strAppl = 'M';
       When inType = 'WCMS';
          strAppl = 'W';
       Endsl;
      /End-free
     C                   CALL      'SA008R'                             99
     C                   PARM                    scCo#
     C                   PARM      SCUSR         #@USR            10
     C                   PARM      strAppl       ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
      /Free
       Return ##rcds = *on;
      /End-free
     pChkRestriction   e

       //===================================================================
       // Clear errors for configuration screen
       //===================================================================
     pClearErrCfg      b
     DClearErrCfg      pi
      /Free
       // SYSM
       erUsr = *off;
       erUsNm = *off;
       erStaf = *off;
       erTitl = *off;
       erIPgm = *off;
       erLvl = *off;
       erType = *off;
       erMail = *off;
       erOutQ = *off;
       erPrDt = *off;
       erqst = *off;
       // WCMS
       erFndWCMS = *off;
       erEmp#WCMS = *off;
       erXFnd = *off;
       erXEmp = *off;
       erRAL  = *off;
       erMRAL  = *off;
       erCRAL  = *off;
       erERAL  = *off;
       erLRAL  = *off;
       erRsv1  = *off;
       // WCMC
       erSub# = *off;
       erFndWCMC = *off;
       erEmp#WCMC = *off;
       erMrSu = *off;
       erMrGr = *off;
       erMrEm = *off;
       // DARTS
       erSecL = *off;
       erVDtD = *off;
       erRmCd = *off;
       erCsId = *off;
       erFold = *off;
      /End-free
     pClearErrCfg      e

       //===================================================================
       // Clear errors for restriction screens
       //===================================================================
     pClearErrRst      b
     DClearErrRst      pi
      /Free
       erSub# = *off;
       erFnd = *off;
       erEmpFr = *off;
       erEmpTo = *off;
      /End-free
     pClearErrRst      e

       //===================================================================
       // Delete restriction records for user
       //===================================================================
     pDltRestrictions  b
     DDltRestrictions  pi
     D inCo#                          3  0 const
     D inUser                        10    const
      /Free
       If not %open(smazl1);
          Open smazl1;
       Endif;
       Dou not %found;
          Delete (inCo#:inUser) smazl1;
       Enddo;
       If not %open(smazl);
          Open smazl;
       Endif;
       Dou not %found;
          Delete (inCo#:inUser) smazl;
       Enddo;
      /End-free
     pDltRestrictions  e

       //===================================================================
       // ErrChkCfg - Error check configuration screen
       //===================================================================
     pErrChkCfg        b
     DErrChkCfg        pi             1n
     D inRRN                          9  0 options(*nopass)

     DdecRRN           s              9  0

      /Free
       If %parms >= 1;
          decRRN = inRRN;
       Endif;
       ClearErrCfg();
       If ErrChkSYSM(decRRN);
          Return *on;
       Endif;
       If isWCMS = *on and ErrChkWCMS();
          Return *on;
       Endif;
       If isWCMC = *on and ErrChkWCMC();
          Return *on;
       Endif;
       If isDARTS = *on and ErrChkDARTS();
          Return *on;
       Endif;

       Return *off;
      /End-Free
     p ErrChkCfg       e

       //===================================================================
       // ErrChkDARTS - Error check configuration screen (DARTS)
       //===================================================================
     pErrChkDARTS      b
     DErrChkDARTS      pi             1n
     D inRRN                          9  0 options(*nopass)

     DdecRRN           s              9  0
     DinRtCd           s              1

      /Free
       If %parms >= 1;
          decRRN = inRRN;
       Endif;

       // DARTS security level can not be less than system level

       // Visual DARTS access and default fields
       If scVDtD = 'Y' and scVDta <> 'Y';
          msVDtD = 'SCM0650';
          mfVDtD = 'SCMESG';
          erVDtD = *on;
          Return *on;
       Endif;

       //*** Beg Add *** 04/10/24 **************************************
       // If = 0 then set to blank
       If scRmCd = '0';
         scRmCd = ' ';
       Endif;
       //*** End Add *** 04/10/24 **************************************
       // If present, Remarks Type Code must be valid.
       If scRmCd <> *blanks;
          Chain scRmCd PCT01;
          If not %found;
             msRmCd = 'SCM0501';
             mfRmCd = 'SCMESG';
             erRmCd = *on;
             Return *on;
          Endif;                                                              //*IN60 = *ON
       Endif;                                                                 //SCRMCD != *BLANKS

       // Validate the Custom Type
       If scRAcc <> 'Y';
          Chain scCsId scg00;
          If not %found;
             msCsId = 'SCM0505';
             mfCsId = 'SCMESG';
             erCsId = *on;
             Return *on;
          Endif;
       Endif;

       // If entered, validate folder. Only perform this function if OV/400  is installed.
       If isOV and scFold <> *blanks;
          CheckFolder(scFold:inRtCd);
          Select;
          When inRtCd = '1';
             msFold = 'SCM0521';
             mfFold = 'SCMESG';
             erFold = *on;
             Return *on;
          When inRtCd = '2';
             msFold = 'SCM0517';
             mfFold = 'SCMESG';
             erFold = *on;
             Return *on;
          Endsl;                                                              //QQRTCD
       Endif;                                                                //SCFOLD != *BLANKS

       Return *off;
      /End-Free
     p ErrChkDARTS     e

       //===================================================================
       // ErrChkRst - Error check restrictions
       //===================================================================
     pErrChkRst        b
     DErrChkRst        pi             1n
     D inType                         4    const
     D inRRN                          9  0 const options(*nopass)

     DdecRRN           s              9  0
     DdecAZRRN         s              9  0
     DdecHoldTo        s                   like(AZtEmp)
     DdecCurrTo        s                   like(AZtEmp)

      /Free
       If %parms >= 2;
          decRRN = inRRN;
       Endif;
       ClearErrRst();

       Select;
       When inType = 'WCMS';
          If not %open(WMFNL);
             Open WMFNL;
          Endif;
          Chain (scCo#:scFnd) wfn00;
          If not %found;
             msFnd = 'WCR0508';
             mfFnd = 'WMESGG';
             erFnd = *on;
             Return *on;
          Endif;
       When inType = 'WCMC';
          If not %open(FMSML);
             Open FMSML;
          Endif;
          Chain (scCo#:scSub#) fsm00;
          If not %found;
             msSub# = 'MCR0601';
             mfSub# = 'MMESGG';
             erSub# = *on;
             Return *on;
          Endif;
          If scFnd <> *zeros;
             If not %open(FMFNL);
                Open FMFNL;
             Endif;
             Chain (scCo#:scSub#:scFnd) ffn00;
             If not %found;
                msFnd = 'MCR0508';
                mfFnd = 'MMESGG';
                erFnd = *on;
                Return *on;
             Endif;
          Endif;
       Endsl;

       If scEmpFr > scEmpTo;
          msEmpFr = 'MCR0504';
          mfEmpFr = 'MMESGG';
          erEmpFr = *on;
          msEmpTo = 'MCR0504';
          mfEmpTo = 'MMESGG';
          erEmpTo = *on;
          Return *on;
       Endif;

       // Employer range overlap
       Select;
       When scEmpTo = *zero;
          decHoldTo = *hival;
       Other;
          decHoldTo = scEmpTo;
       Endsl;
       Select;
       When inType = 'WCMS';
          Setll *loval waz00;
       When inType = 'WCMC';
          Setll *loval maz01;
       Endsl;
       Dou %eof;
          Select;
          When inType = 'WCMS';
             Clear wAzFds;
             Reade (scCo#:scUsr:scFnd) waz00;
             decAZRRN = wAzFds.rrn;
          When inType = 'WCMC';
             Clear mAzFds;
             Reade (scCo#:scUsr:scSub#:scFnd) maz01;
             decAZRRN = mAzFds.rrn;
          Endsl;
          If %eof;
             Iter;
          Endif;
          Select;
          When AZtEmp = *zero;
             decCurrTo = *hival;
          Other;
             decCurrTo = AZtEmp;
          Endsl;
          // Ignore record
          If decHoldTo < AZfEmp or scEmpFr > decCurrTo or AZstat = 'I'
           or decAZRRN = decRRN;
             Iter;
          Endif;
          msEmpFr = 'MC30001';
          mfEmpFr = 'MMESGG';
          erEmpFr = *on;
          msEmpTo = 'MC30001';
          mfEmpTo = 'MMESGG';
          erEmpTo = *on;
          Return *on;
       Enddo;

       Return *off;
      /End-Free
     p ErrChkRst       e

       //===================================================================
       // ErrChkSYSM - Error check configuration screen (SYSM)
       //===================================================================
     pErrChkSYSM       b
     DErrChkSYSM       pi             1n
     D inRRN                          9  0 options(*nopass)

     D ChkISeries      pr                  extPgm('SA041C')
     D  outUsr                       10
     D  inError                       1n

     DdecRRN           s              9  0
     DisChkUser        s              1n   inz(*on)
     DisError          s              1n

      /Free
       If %parms >= 1;
          decRRN = inRRN;
       Endif;

       If scUsr = *blanks;
          msUsr = 'MCR0500';
          mfUsr = 'MMESGG';
          erUsr = *on;
          Return *on;
       Endif;

       scUsr = Upper(scUsr);
       Chain scUsr SMUPL;
       If %found and isAddMode;
          msUsr = 'MCR0592';
          mfUsr = 'MMESGG';
          erUsr = *on;
          Return *on;
       Endif;

       disCommit = *off;

       Select;
       When cltID = '129' and %subst(scUsr:1:1) = '#';
          isChkUser = *off;
       When isEdtMode and scStat = 'A' and strHoldStat = 'I';
          isChkUser = *on;
       When isEdtMode;
          isChkUser = *off;
       When isAddMode;
          isChkUser = *on;
       Endsl;

       Callp ChkISeries(scUsr:isError);
       If isChkUser and isError;
          msUsr = 'MCR0506';
          mfUsr = 'MMESGG';
          erUsr = *on;
          Return *on;
       Endif;
       isProfileExists = not isError;

       If isEdtMode;
          Chain(n) decRRN smupp;
          If (dsSMUPPprv.UPdt < UPdt) or (dsSMUPPprv.UPtime < UPtime);
             msUsr = 'MC30000';
             mfUsr = 'MMESGG';
             erUsr = *on;
             Return *on;
          Endif;
       Endif;

       If scUsNm = *blanks;
          msUsNm = 'MCR0500';
          mfUsNm = 'MMESGG';
          erUsNm = *on;
          Return *on;
       Endif;

       If scStaf = *blanks;
          msStaf = 'MCR0500';
          mfStaf = 'MMESGG';
          erStaf = *on;
          Return *on;
       Endif;

       scStaf = Upper(scStaf);
       Chain scStaf SMUPL1;
       If %found and scUsr <> UPusr;
          msStaf = 'MCR0569';
          mfStaf = 'MMESGG';
          erStaf = *on;
          Return *on;
       Endif;

       If scTitl = *blanks;
          msTitl = 'MCR0500';
          mfTitl = 'MMESGG';
          erTitl = *on;
          Return *on;
       Endif;

       If scIPgm <> *blanks;
          Chain scIPgm MMOPL;
          If not %found;
             msIPgm = 'MCR0594';
             mfIPgm = 'MMESGG';
             erIPgm = *on;
             Return *on;
          Endif;
       Endif;

       If scLvl = *zeros;
          msLvl = 'MCR0513';
          mfLvl = 'MMESGG';
          erLvl = *on;
          Return *on;
       Endif;

       scType = Upper(scType);
       If scType = *blanks;
          msType = 'MCR0500';
          mfType = 'MMESGG';
          erType = *on;
          Return *on;
       Endif;

       If scPhn# <> 0 and %len(%char(scPhn#)) < 10;
          msPhn# = 'Invalid phone number entered.';
          erPhn# = *on;
          Return *on;
       Endif;

       If %len(%trim(scMail)) > 0;
          erMail = Check_Email(scMail:msMail);
          If erMail = *on;
             Return *on;
          Endif;
       Endif;


       scOutQ = Upper(scOutQ);
       CheckObj(scOutQ:'*OUTQ':##err);
       If ##err = *on;
          msOutQ = 'MCR0509';
          mfOutQ = 'MMESGG';
          erOutQ = *on;
          Return *on;
       Endif;

       If DtCheck(scPrDt) = *zero;
          msPrDt = 'MCR0512';
          mfPrDt = 'MMESGG';
          erPrDt = *on;
          Return *on;
       Endif;

       If scDsTab = 'Quick Search' and scQst = *blanks;
          erqst = *on;
          Return *on;
       Endif;
       Return *off;
      /End-Free
     p ErrChkSYSM      e
       //===================================================================
       // ErrChkWCMC - Error check configuration screen (WCMC)
       //===================================================================
     pErrChkWCMC       b
     DErrChkWCMC       pi             1n
     D inRRN                          9  0 options(*nopass)

     DdecRRN           s              9  0

      /Free
       If %parms >= 1;
          decRRN = inRRN;
       Endif;

       // Subscriber must be valid
       If scSub# <> *zero or scMrSu = 'Y';
          Chain (scCo#:scSub#) fsm00;
          If not %found or SMstat = 'I';
             msSub# = 'MCR0301';
             mfSub# = 'FSMESG';
             erSub# = *on;
             Return *on;
          Endif;
       Endif;

       // Group must be valid
       //*** Beg Chg *** 05/30/24 *** PHW115628 ************************
       //***If scFndWCMC <> *zero or scMrGr = 'Y';
       If scFndWCMCC = *on and scFndWCMC <> *zero or
        scMrGr = 'Y';
       //*** End Chg *** 05/30/24 *** PHW115628 ************************
       // Check Subscriber number should not be zero
          If scSub# <> 0;
          Chain (scCo#:scSub#:scFndWCMC) ffn00;
          If not %found or FNstat = 'I';
             msFndWCMC = 'MCR0508';
             mfFndWCMC = 'FSMESG';
             erFndWCMC = *on;
             Return *on;
          Endif;
          Endif;
       Endif;

       // Employer(division) must be valid
       If scEmp#WCMC <> *zero or scMrEm = 'Y';
          If scDivWCMC <> *blanks;
             //  Be sure the field is right justified
             scDivWCMC = RightJstfy(scDivWCMC:5);
             Chain
              (scCo#:scSub#:scFndWCMC:scEmp#WCMC:%dec(scDivWCMC:5:0)) fem00 ;
          Else;
             Chain (scCo#:scSub#:scFndWCMC:scEmp#WCMC) fem00;
          Endif;
          If not %found or EMstat = 'I';
             msEmp#WCMC = 'MCR0508';
             mfEmp#WCMC = 'FSMESG';
             erEmp#WCMC = *on;
             Return *on;
          Endif;
          If scEmp#WCMC = *zeros and scDivWCMC <> *blanks;
             msEmp#WCMC = 'MCR0508';
             mfEmp#WCMC = 'FSMESG';
             erEmp#WCMC = *on;
             Return *on;
          Endif;
       Endif;
       // Check for active restriction records
       isRstWCMC = ChkRestriction('WCMC');
       If (scMrSu = 'N' and scMrGr = 'N' and scMrEm = 'N') and isRstWCMC =  *on;
          msMrSu = 'MC40176';
          mfMrSu = 'FSMESG';
          erMrSu = *on;
          msMrGr = 'MC40176';
          mfMrGr = 'FSMESG';
          erMrGr = *on;
          msMrEm = 'MC40176';
          mfMrEm = 'FSMESG';
          erMrEm = *on;
          Return *on;
       Endif;

       Return *off;
      /End-Free
     p ErrChkWCMC      e
       //===================================================================
       // ErrChkWCMS - Error check configuration screen (WCMS)
       //===================================================================
     pErrChkWCMS       b
     DErrChkWCMS       pi             1n
     D inRRN                          9  0 options(*nopass)

     DdecRRN           s              9  0

      /Free
       If %parms >= 1;
          decRRN = inRRN;
       Endif;

       //*** Beg Chg *** 05/30/24 *** PHW115628 ************************
       //***If scFndWCMS <> *zeros;
       If scFndWCMSC = *on and scFndWCMS <> *zeros;
       //*** End Chg *** 05/30/24 *** PHW115628 ************************
          Chain (scCo#:scFndWCMS) wfn00;
          If not %found or FNstat = 'I';
             msFndWCMS = 'MCR0508';
             mfFndWCMS = 'MMESGG';
             erFndWCMS = *on;
             Return *on;
          Endif;
       Endif;

       // Update the editing as follows
       // If the restriction flag is set and there additional restrictions set up, no edit message
       // If the restriction flag is set and there is a group entered, no edit message
       //(1) If the restriction flag is set and no add'l restrictions and no group, edit message
       // If the restriction flag is not set, no edit message
       //(2) The restriction flag must be set if there are additional restrictions

       isRstWCMS = ChkRestriction('WCMS');

       If scXFnd = 'Y' and isRstWCMS = *off and scFndWCMS = *zeros;
          msFndWCMS = 'MCR0508';
          mfFndWCMS = 'MMESGG';
          erFndWCMS = *on;
          Return *on;
       Endif;

       If isRstWCMS = *on and scXFnd <> 'Y';
          msXFnd = 'WC30398';
          mfXFnd = 'WMESGG';
          erXFnd = *on;
          Return *on;
       Endif;

       //  These edits are only preformed of the user profile exists on the  iSeries
       If isProfileExists;

          If scXEmp = 'Y' and scEmp#WCMS <> *zeros;
            If scDivWCMS <> *blanks;
                //  Be sure the field is right justified
                scDivWCMS = RightJstfy(scDivWCMS:5);
                Chain (scCo#:scFndWCMS:scEmp#WCMS:%dec(scDivWCMS:5:0)) wem00;
             Else;
                Chain (scCo#:scFndWCMS:scEmp#WCMS) wem00;
             Endif;
             If not %found;
                msEmp#WCMS = 'WCR0538';
                mfEmp#WCMS = 'WMESGG';
                erEmp#WCMS = *on;
                Return *on;
             Endif;
             If scEmp#WCMS = *zeros and scDivWCMS <> *blanks;
                msEmp#WCMS = 'WCR0538';
                mfEmp#WCMS = 'WMESGG';
                erEmp#WCMS = *on;
                Return *on;
             Endif;                                                          //SCEMP#=*ZEROS
          Endif;
          // Check for active restriction records
          isRstWCMS = ChkRestriction('WCMS');
          If (scXFnd = 'N' and scXEmp = 'N') and isRstWCMS = *on;
             msXFnd = 'WC30179';
             mfXFnd = 'WMESGG';
             erXFnd = *on;
             msXEmp = 'WC30179';
             mfXEmp = 'WMESGG';
             erXEmp = *on;
             Return *on;
          Endif;

       Endif;

       // Both cumulative and non-cum are mutually exclusive
       If scRAL > *zero and
        (scMRAL > *zero or scCRAL > *zero or scERAL > *zero or scLRAL > *zero);
          msRAL  = 'WC40028';
          mfRAL  = 'WMESGG';
          erRAL  = *on;
          Select;
          When scMRAL > *zero;
             msMRAL  = 'WC40028';
             mfMRAL  = 'WMESGG';
             erMRAL  = *on;
          When scCRAL > *zero;
             msCRAL  = 'WC40028';
             mfCRAL  = 'WMESGG';
             erCRAL  = *on;
          When scERAL > *zero;
             msERAL  = 'WC40028';
             mfERAL  = 'WMESGG';
             erERAL  = *on;
          When scLRAL > *zero;
             msLRAL  = 'WC40028';
             mfLRAL  = 'WMESGG';
             erLRAL  = *on;
          Endsl;
          Return *on;
       Endif;

       // If cumulative entry made, must have reserve type
       If scRal > *zero and
        scRsv1 = *blank and scRsv2 = *blank and
        scRsv3 = *blank and scRsv4 = *blank;
          msRsv1  = 'WC40029';
          mfRsv1  = 'WMESGG';
          erRsv1  = *on;
          Return *on;
       Endif;

       Return *off;
      /End-Free
     p ErrChkWCMS      e

       //===================================================================
       // Close files after use
       //===================================================================
     p FilesClose      b
     d FilesClose      pi
      /Free
       If %open(SMUPP);
          Close SMUPP;
       Endif;
       If %open(MMOPP);
          Close MMOPP;
       Endif;
       If %open(MMOPL);
          Close MMOPL;
       Endif;
       If %open(SMUPL);
          Close SMUPL;
       Endif;
       If %open(SMUPL1);
          Close SMUPL1;
       Endif;
       If %open(SMCOL);
          Close SMCOL;
       Endif;
       If %open(SMUHP);
          Close SMUHP;
       Endif;
       If %open(WMEMP);
          Close WMEMP;
       Endif;
       If %open(WMEML);
          Close WMEML;
       Endif;
       If %open(WMFNL);
          Close WMFNL;
       Endif;
       If %open(WMAAP);
          Close WMAAP;
       Endif;
       If %open(WMAGP);
          Close WMAGP;
       Endif;
       If %open(WMOFL);
          Close WMOFL;
       Endif;
       If %open(WMUPL);
          Close WMUPL;
       Endif;
       If %open(FMSML);
          Close FMSML;
       Endif;
       If %open(FMFNL);
          Close FMFNL;
       Endif;
       If %open(FMEML);
          Close FMEML;
       Endif;
       If %open(FMSMP);
          Close FMSMP;
       Endif;
       If %open(FMFNP);
          Close FMFNP;
       Endif;
       If %open(FMEMP);
          Close FMEMP;
       Endif;
       If %open(SMSLP);
          Close SMSLP;
       Endif;
       If %open(SMSLL2);
          Close SMSLL2;
       Endif;
       If %open(SMCGL);
          Close SMCGL;
       Endif;
       If %open(STCTL1);
          Close STCTL1;
       Endif;
      /End-free
     p FilesClose      e

       //===================================================================
       // Open files for use
       //===================================================================
     p FilesOpen       b
     d FilesOpen       pi
      /Free
       If not %open(SMUPP);
          Open SMUPP;
       Endif;
       If not %open(MMOPP);
          Open MMOPP;
       Endif;
       If not %open(MMOPL);
          Open MMOPL;
       Endif;
       If not %open(SMUPL);
          Open SMUPL;
       Endif;
       If not %open(SMUPL1);
          Open SMUPL1;
       Endif;
       If not %open(SMCOL);
          Open SMCOL;
       Endif;
       If not %open(SMUHP);
          Open SMUHP;
       Endif;
       If not %open(WMEMP) and isWCMS = *on;
          Open WMEMP;
       Endif;
       If not %open(WMEML) and isWCMS = *on;
          Open WMEML;
       Endif;
       If not %open(WMFNL) and isWCMS = *on;
          Open WMFNL;
       Endif;
       If not %open(WMAAP) and isWCMS = *on;
          Open WMAAP;
       Endif;
       If not %open(WMAGP) and isWCMS = *on;
          Open WMAGP;
       Endif;
       If not %open(WMOFL) and isWCMS = *on;
          Open WMOFL;
       Endif;
       If not %open(WMUPL) and isWCMS = *on;
          Open WMUPL;
       Endif;
       If not %open(FMSML) and isWCMC = *on;
          Open FMSML;
       Endif;
       If not %open(FMFNL) and isWCMC = *on;
          Open FMFNL;
       Endif;
       If not %open(FMEML) and isWCMC = *on;
          Open FMEML;
       Endif;
       If not %open(FMSMP) and isWCMC = *on;
          Open FMSMP;
       Endif;
       If not %open(FMFNP) and isWCMC = *on;
          Open FMFNP;
       Endif;
       If not %open(FMEMP) and isWCMC = *on;
          Open FMEMP;
       Endif;
       If not %open(SMSLP) and isDARTS = *on;
          Open SMSLP;
       Endif;
       If not %open(SMSLL2) and isDARTS = *on;
          Open SMSLL2;
       Endif;
       If not %open(SMCGL) and isDARTS = *on;
          Open SMCGL;
       Endif;
       If not %open(STCTL1) and isDARTS = *on;
          Open STCTL1;
       Endif;
      /End-free
     p FilesOpen       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       UserFilter('Set');

       Dou scBtnCncl = *on;
          // Load user subfile grid
          Exsr LoadUserGrid;
          // Display grid info screen
          Exsr ExfmtUsers;

          // Check for a edit/delete click
          ReadC sflUsrPrm;
          DoW Not %Eof;

             Select;
             When scUsrCpy = *on;
                scUsrCpy = *off;
                UserUpdate(scRRN:'Copy');
                Leave;
             When scUsrEdt = *on;
                scUsrEdt = *off;
                UserUpdate(scRRN:'Edit');
                Leave;
             When scUsrDlt = *on;
                scUsrDlt = *off;
                //***  Begin Add  ***  04/08/24  *** 115361 ****************
                strMsg = 'Do you want to continue with the deletion?';
                If PopTwoBtn(strmsg:'Confirm':'Continue':'Cancel');
                //***  End   Add  ***  04/08/24  *** 115361 ****************
                   UserUpdate(scRRN:'Delete');
                   Leave;
                //***  Begin Add  ***  04/08/24  *** 115361 ****************
                Endif;
                //***  End   Add  ***  04/08/24  *** 115361 ****************
             Endsl;
             ReadC sflUsrPrm;
          EndDo;
          Select;
          When scUsrAdd = *on;
             scUsrAdd = *off;
             UserUpdate(0:'Add');
          When scUsrFlt = *on;
             scUsrFlt = *off;
             UserFilter();
          When scUsrFltCl = *on;
             scUsrFltCl = *off;
             UserFilter('Clear');
          When scBtnTab = *on;
             scBtnTab = *off;
          When scBtnRfrsh = *on;
             scBtnRfrsh = *off;
          When scBtnSave = *on;
             scBtnSave = *off;
          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       Begsr ExfmtUsers;

         Select;
         When TabIndex = 0;
            UserFilter('Check');
            // Tab height and action panel height are calculated in javascrip (on-load)
            Write RcdSysTbl;
            Write RcdPanel;
            Exfmt RcdUsrParm;
            Read RcdSysTbl;
            Read RcdPanel;
         EndSl;

       Endsr;
       //===================================================================
       // Load user grid
       //===================================================================
       Begsr LoadUserGrid;

         // Clear the subfile
         scSflDsp = *off;
         scSflClr = *on;
         Write rcdUsrParm;
         scSflClr = *off;
         scSflDsp = *on;
         scSflRRN = 0;

         // Populate the subfile
         Setll *loval smupl;
         Dou %eof;
            Read smupl;
            Select;
            When %eof;
               Iter;
            When ssName <> *blanks and not StringFound(UPusNm:ssName:'1');
               Iter;
            When ssLvl <> *zero and ssLvlEval = '1' and UPlvl >= ssLvl;
               Iter;
            When ssLvl <> *zero and ssLvlEval = '2' and UPlvl <> ssLvl;
               Iter;
            When ssLvl <> *zero and ssLvlEval = '3' and UPlvl <= ssLvl;
               Iter;
            When ssType <> *blank and ssTypeEval = '1' and UPtype >= ssType;
               Iter;
            When ssType <> *blank and ssTypeEval = '2' and UPtype <> ssType;
               Iter;
            When ssType <> *blank and ssTypeEval = '3' and UPtype <= ssType;
               Iter;
            When ssStatus <> *blank and UPstat <> ssStatus;
               Iter;
            Endsl;
            scSflRRN += 1;
            scSflSel = *off;
            If scSflRRN = 1;
               scSflSel = *on;
            Endif;
            scUsr  = UPusr;
            scUsNm = UPusNm;
            scStaf = UPstaf;
            Select;
            When UPstat = 'A';
               scStatus = 'Active';
            Other;
               scStatus = 'Inactive';
            Endsl;
            scRRN  = upFds.rrn;
            Write sflUsrPrm;
         Enddo;

         scSflRRN = 1;

       Endsr;
      /End-Free
     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       Select;
       When inCall = 'A';
          edtMode = '1';
       When inCall = 'C';
          edtMode = '1';
       When inCall = 'D';
          dspMode = '1';
       When inCall = '1';
          dspMode = '1';
       Other;
          edtMode = '1';
       Endsl;

       // CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
       GetSysVals(##wcIf:##mcIf:##wcIn:##ar:##ntrk:##ur:##med:##edi:##wcDk:
        ##rhab:##mss:##fsch:##prvB:##npWC:##od:##aRls:##cCod:##cRvw:##prom:
        ##mcRv:##freq:##crAC:##PReo:##pArN:##mGRi);

       Chain 'SY' ssy01;
       isWCMS = *off;
       If SYwcIf = 'Y';
          isWCMS = *on;
       Endif;
       // Non-PDS WCMS
       If SYfl05 = 'Y';
          isWCMS = *off;
       Endif;
       isWCMC = *off;
       If SYfbIf = 'Y';
          isWCMC = *on;
       Endif;
       isDARTS = *off;
       If SYscIf = 'Y';
          isDARTS = *on;
       Endif;
       isExtRst = *off;
       If SYfl06 = 'Y';
          isExtRst = *on;
          isRstWCMS = ChkRestriction('WCMS');
          isRstWCMC = ChkRestriction('WCMC');
       Endif;
       isOV = *off;
       If SYofIf = 'Y';
          isOV = *on;
       Endif;
       If %scan('WCMS':strAvlMods) = 0;
          isWCMS = *off;
       Endif;
       If %scan('WCMC':strAvlMods) = 0;
          isWCMC = *off;
       Endif;
       If %scan('DARTS':strAvlMods) = 0;
          isDARTS = *off;
       Endif;

       FilesOpen();

       isDTM = ChkDTM;
       isScript = isModEnabled('Scriptura-Documents');
       RtvCltId(cltID);

       // Load Action panel with the Company Name.
       scCompName = GetCompNam(GetUserInfo(q1user));

       scBtnTab = *Off;
       VisContUpd = *on;
       Return;
      /End-Free
     p ProgramStart    e

       //===================================================================
       // RstUpdate - Update restriction record
       //===================================================================
     p RstUpdate       b
     d RstUpdate       pi
     d  inMode                       10    const
     d  inType                        4    const
     d  inRRN                         9  0 const options(*nopass)
     d
     dconMxRi          c                   const(8)
     d
     ddecRRN           s              9  0 inz
     dindex            s              5  0
     disError          s              1n
     dmaxSfl#          s              5  0 inz
     dstrAppl          s                   like(AZappl)

      /Free
       If not %open(SMAZP);
          Open SMAZP;
       Endif;

       Select;
       When inType = 'WCMC';
          strAppl = 'M';
       When inType = 'WCMS';
          strAppl = 'W';
       Endsl;

       If inMode = 'Commit';
          Exsr CommitRecords;
          disCommit = *off;
          Return;
       Endif;

       If %parms >= 3;
          decRRN = inRRN;
       Endif;
       Clear saz0p;
       Chain(e) decRRN SMAZP;
       If %status = 1218;
          PopOneBtn('Record locked by another user.  Please try again later.':
           'Record Lock');
          Return;
       Else;
          Unlock SMAZP;
       Endif;
       Clear saz0p;
       Chain(n) decRRN SMAZP;
       Exsr LoadRstScr;
       Select;
       When inMode = 'Add';
          posCancel = 440;
          dspAddNext = *on;
       When inMode = 'Edit';
          posCancel = 560;
          dspAddNext = *off;
       When inMode = 'Delete';
          Exsr DeleteRecords;
          Return;
       Endsl;

       ClearErrRst();
       btnAddNext = *off;
       Dou not isError;
          isError = *off;
          Select;
          When inType = 'WCMC';
             Exfmt rcdRstWCMC;
          When inType = 'WCMS';
             Exfmt rcdRstWCMS;
          Endsl;
          Select;
          When btnCancel = *on;
             btnCancel = *off;
             Return;
          Endsl;
          isError = ErrChkRst(inType:decRRN);
       Enddo;

       Select;
       When inMode = 'Add';
          Clear saz0p;
          Exsr LoadRstDB;
          Exsr WriteRcds;
          If btnAddNext = *on;
             btnAddNext = *off;
             RstUpdate('Add':inType);
          Endif;
       When inMode = 'Edit';
          Exsr UpdateRcds;
       Endsl;

       Return;

       //======================================================================
       Begsr CommitRecords;
         If not ValidRstAdd(inType);
            Leavesr;
         Endif;
         Select;
         When inType = 'WCMC';
            maxSfl# = decSflMC#;
         When inType = 'WCMS';
            maxSfl# = decSflWC#;
         Endsl;
         For index = 1 to maxSfl#;
            Exsr ReadRstSfl;
            Exsr LoadRstScr;
            Clear saz0p;
            Exsr LoadRstDB;
            Exsr WriteRcds;
            Exsr ReadRstSfl;
         Endfor;
         Select;
         When inType = 'WCMC';
            commitWCMC = *off;
            dspCmtWCMC = *off;
         When inType = 'WCMS';
            commitWCMS = *off;
            dspCmtWCMS = *off;
         Endsl;
       Endsr;
       //======================================================================
       Begsr #CommitRecords;
         If not ValidRstAdd(inType);
            Leavesr;
         Endif;
         Exsr ReadRstSfl;
         Dow not %eof;
            Exsr LoadRstScr;
            Clear saz0p;
            Exsr LoadRstDB;
            Exsr WriteRcds;
            Exsr ReadRstSfl;
         Enddo;
         Select;
         When inType = 'WCMC';
            commitWCMC = *off;
            dspCmtWCMC = *off;
         When inType = 'WCMS';
            commitWCMS = *off;
            dspCmtWCMS = *off;
         Endsl;
       Endsr;

       //======================================================================
       Begsr DeleteRecords;
         Delete decRRN SMAZP;
       Endsr;

       //======================================================================
       Begsr LoadRstDB;

         AZco# = scCo#;
         AZusr = scUsr;
         AZsub# = scSub#;
         AZfnd = scFnd;
         AZfEmp = scEmpFr;
         AZtEmp = scEmpTo;
         AZauth = scAuthRst;
         AZappl = strAppl;

         AZstat = scStat;
         AZuser = q1User;
         AZpgm  = q1Pgm;
         AZdt   = DateTo7(SystemDate);
         AZtime = TimeTo6(SystemTime);

       Endsr;

       //======================================================================
       Begsr LoadRstScr;

         Clear rcdRstWCMS;
         Clear rcdRstWCMS;
         scSub# = AZsub#;
         scFnd = AZfnd;
         scEmpFr = AZfEmp;
         scEmpTo = AZtEmp;
         scAuthRst = AZauth;
         If scAuthRst = *blank;
            scAuthRst = 'A';
         Endif;
         scStat = AZstat;
         if scStat = *blank;
            scStat = 'A';
         Endif;
         strHoldStat = scStat;

       Endsr;

       //======================================================================
       Begsr ReadRstSfl;
         Select;
         When inType = 'WCMC';
            Chain index sflRstWCMC;
            Chain(n) scRRNWCMC SMAZP;
         When inType = 'WCMS';
            Chain index sflRstWCMS;
            Chain(n) scRRNWCMS SMAZP;
         Endsl;
       Endsr;
       //======================================================================
       Begsr #ReadRstSfl;
         Select;
         When inType = 'WCMC';
            Readc sflRstWCMC;
            Chain(n) scRRNWCMC SMAZP;
         When inType = 'WCMS';
            Readc sflRstWCMS;
            Chain(n) scRRNWCMS SMAZP;
         Endsl;
       Endsr;

       //======================================================================
       Begsr UpdateRcds;
         Chain decRRN SMAZP;
         Exsr LoadRstDB;
         Update saz0p;
       Endsr;

       //======================================================================
       Begsr WriteRcds;
         AZiUsr = AZuser;
         AZiPgm = AZpgm;
         AZiDt  = AZdt;
         AZiTim = AZtime;
         Write saz0p;
         Feod SMAZP;
       Endsr;
      /End-Free
       //======================================================================
     p RstUpdate       e

       //===================================================================
       // User filters
       //===================================================================
     pUserFilter       b
     dUserFilter       pi
     d inMode                        10    const options(*nopass)

     disError          s              1n
     dsvLvl            s                   like(ssLvl)
     dsvLvlEval        s                   like(ssLvlEval)
     dsvName           s                   like(ssName)
     dsvStatus         s                   like(ssStatus)
     dsvType           s                   like(ssType)
     dsvTypeEval       s                   like(ssTypeEval)
      /Free
       Select;
       When %parms >= 1 and inMode = 'Clear';
          Exsr ClearFilters;
       When %parms >= 1 and inMode = 'Check';
       When %parms >= 1 and inMode = 'Set';
          Exsr SetFilters;
       Other;
          Exsr DefineFilters;
       Endsl;
       Exsr CheckFilters;
       Return;
       //==================================================================
       Begsr CheckFilters;
        isUsrFltr = *off;
        If ssName <> *blank
         or ssLvl <> *zero
         or ssType <> *blank
         or ssStatus <> *blank;
           isUsrFltr = *on;
        Endif;
       Endsr;
       //==================================================================
       Begsr ClearFilters;
        Clear ssName;
        Clear ssLvl;
        Clear ssLvlEval;
        Clear ssType;
        Clear ssTypeEval;
        Clear ssStatus;
       Endsr;
       //==================================================================
       Begsr DefineFilters;
        svName     = ssName;
        svLvl      = ssLvl;
        svLvlEval  = ssLvlEval;
        svType     = ssType;
        svTypeEval = ssTypeEval;
        svStatus   = ssStatus;
        Dou not isError;
           isError = *off;
           Exfmt rcdFilter;
           Select;
           When filtCancel = '1';
              Iter;
           Other;
              Exsr EditFilters;
           Endsl;
        Enddo;
        // Reset filters to entry values
        If filtCancel = '1';
           filtCancel = '0';
           ssName     = svName;
           ssLvl      = svLvl;
           ssLvlEval  = svLvlEval;
           ssType     = svType;
           ssTypeEval = svTypeEval;
           ssStatus   = svStatus;
        Endif;
       Endsr;
       //==================================================================
       Begsr EditFilters;
        // Security Level
        If (ssLvl = *zero and ssLvlEval <> *blank)
         or (ssLvl <> *zero and ssLvlEval = *blank);
           msLvl = 'MCR0593';
           mfLvl = 'MMESGG';
           erLvl = *on;
           msLvlEval = 'MCR0593';
           mfLvlEval = 'MMESGG';
           erLvlEval = *on;
           isError = *on;
           Leavesr;
        Endif;
        If (ssType = *blank and ssTypeEval <> *blank)
         or (ssType <> *blank and ssTypeEval = *blank);
           msType = 'MCR0593';
           mfType = 'MMESGG';
           erType = *on;
           msTypeEval = 'MCR0593';
           mfTypeEval = 'MMESGG';
           erTypeEval = *on;
           isError = *on;
           Leavesr;
        Endif;
       Endsr;
       //==================================================================
       Begsr SetFilters;
        UserFilter('Clear');
        ssStatus = 'A';
       Endsr;
      /End-free
     pUserFilter       e

       //===================================================================
       // UserUpdate - Update user record
       //===================================================================
     p UserUpdate      b
     d UserUpdate      pi
     d  inRRN                         9  0 const
     d  inMode                       10    const

     dconMxRi          c                   const(8)

     disAction         s              1n
     disError          s              1n
     dstrPhn#          s             10
     dstrUsr           s             10
     dstrUsrRst        s             10

      /Free
       Clear sup0p;
       Select;
       When inMode = 'Copy';
          Chain(n) inRRN SMUPP;
       Other;
          Chain(e) inRRN SMUPP dsSMUPPprv;
          If %status = 1218;
             PopOneBtn('Record locked by another user.  Please try again later.'
              :'Record Lock');
             Return;
          Else;
             Unlock SMUPP;
          Endif;
          Chain(n) inRRN SMUPP;
       Endsl;
       isAddMode = *off;
       isCpyMode = *off;
       isDspMode = *off;
       isEdtMode = *off;
       dspCmtWCMC = *off;
       dspCmtWCMS = *off;
       //*** Beg Add *** 05/30/24 *** PHW115628 ************************
       // Reset 'Changed' indicators
       scFndWCMSC = *off;
       scFndWCMCC = *off;
       //*** Beg Add *** 05/30/24 *** PHW115628 ************************
       Select;
       When inMode = 'Add';
          isAddMode = *on;
          scMode = 'Add';
       When inMode = 'Copy';
          isCpyMode = *on;
          isAddMode = *on;
          scMode = 'Copy';
          If isWCMC = *On;
             dspCmtWCMC = *on;
          EndIf;
          If isWCMS = *On;
             dspCmtWCMS = *on;
          EndIf;
       When inMode = 'Edit';
          isEdtMode = *on;
          scMode = 'Edit';
       When inMode = 'Delete';
          Exsr DeleteRecords;
          Return;
       Endsl;
       isNotAdd = not isAddMode;
       disCommit = *on;
       Exsr LoadConfigScr;

       ClearErrCfg();
       Dou not isError and not isAction;
          isError = *off;
          isAction = *on;
          Exsr ClrRstWCMC;
          Exsr ClrRstWCMS;
          If isWCMC = *on;
             Exsr LoadRstWCMC;
          Endif;
          If isWCMS = *on;
             Exsr LoadRstWCMS;
          Endif;
          Exfmt rcdUserCfg;
          ClearErrCfg();
          // Check for a edit/delete click
          ReadC sflRstWCMS;
          ReadC sflRstWCMC;
          Select;
          When btnUserCan = *on;
             btnUserCan = *off;
             If isCpyMode = *on;
                DltRestrictions(scCo#:scUsr);
             Endif;
             Return;
          When addRstWCMC = *on;
             If not ValidRstAdd('WCMC');
                isError = *on;
                Iter;
             Endif;
             RstUpdate('Add':'WCMC');
             addRstWCMC = *off;
          When addRstWCMS = *on;
             If not ValidRstAdd('WCMS');
                isError = *on;
                Iter;
             Endif;
             RstUpdate('Add':'WCMS');
             addRstWCMS = *off;
          When commitWCMC = *on;
             RstUpdate('Commit':'WCMC');
          When commitWCMS = *on;
             RstUpdate('Commit':'WCMS');
          When scEdtWCMC = *on;
             RstUpdate('Edit':'WCMC':scRRNWCMC);
             scEdtWCMC = *off;
          When scEdtWCMS = *on;
             RstUpdate('Edit':'WCMS':scRRNWCMS);
             scEdtWCMS = *off;
          When scDltWCMC = *on;
             RstUpdate('Delete':'WCMC':scRRNWCMC);
             scDltWCMC = *off;
          When scDltWCMS = *on;
             RstUpdate('Delete':'WCMS':scRRNWCMS);
             scDltWCMS = *off;
          Other;
             isError = ErrChkCfg();
             isAction = *off;
          Endsl;
       Enddo;

       Select;
       When isAddMode;
          Clear sup0p;
          If isWCMS = *on;
             Clear sup00;
          Endif;
          If isDARTS = *on;
             Clear ssl0p;
          Endif;
          Exsr LoadConfigDB;
          Exsr WriteRcds;
       When isEdtMode;
          Exsr UpdateRcds;
       Endsl;
       Exsr CheckUpdate;

       Return;

       //======================================================================
       Begsr CheckCo#;
       Endsr;

       //======================================================================
       Begsr CheckUpdate;
         // We do not care if the only change is the maintenance fields
         dsSMUPPprv.UPuser = dsSMUPPnew.UPuser;
         dsSMUPPprv.UPpgm  = dsSMUPPnew.UPpgm;
         dsSMUPPprv.UPdt   = dsSMUPPnew.UPdt;
         dsSMUPPprv.UPtime = dsSMUPPnew.UPtime;

         If dsSMUPPnew <> dsSMUPPprv;
            UHco#  =dsSMUPPprv.UPco#;
            UHusr  =dsSMUPPprv.UPusr;
            UHuser =dsSMUPPnew.UPuser;
            UHpgm  =dsSMUPPnew.UPpgm;
            UHdate =dsSMUPPnew.UPdt ;
            UHtime =dsSMUPPnew.UPtime;

            UHpLvl = dsSMUPPprv.UPlvl;
            UHnLvl = dsSMUPPnew.UPlvl;
            UHpTyp = dsSMUPPprv.UPtype;
            UHnTyp = dsSMUPPnew.UPtype;
            UHpUNm = dsSMUPPprv.UPusNm;
            UHnUNm = dsSMUPPnew.UPusNm;
            UHpTtl = dsSMUPPprv.UPtitl;
            UHnTtl = dsSMUPPnew.UPtitl;
            UHpStf = dsSMUPPprv.UPstaf;
            UHnStf = dsSMUPPnew.UPstaf;
            UHpOtQ = dsSMUPPprv.UPoutQ;
            UHnOtq = dsSMUPPnew.UPoutQ;
            UHpPDt = dsSMUPPprv.UPprDt;
            UHnPDt = dsSMUPPnew.UPprDt;
            UHpSts = dsSMUPPprv.UPstat;
            UHnSts = dsSMUPPnew.UPstat;
            UHpFnd = dsSMUPPprv.UPfnd;
            UHnFnd = dsSMUPPnew.UPfnd ;
            UHpFyr = dsSMUPPprv.UPfyr;
            UHnFyr = dsSMUPPnew.UPfyr;
            UHpEmp = dsSMUPPprv.UPemp#;
            UHnEmp = dsSMUPPnew.UPemp#;
            UHpDiv = dsSMUPPprv.UPdiv;
            UHnDiv = dsSMUPPnew.UPdiv;
            UHpALc = dsSMUPPprv.UPaLic;
            UHnALc = dsSMUPPnew.UPaLic;
            UHpSb# = dsSMUPPprv.UPsub#;
            UHnSb# = dsSMUPPnew.UPsub#;
            UHpRgn = dsSMUPPprv.UPrgn;
            UHnRgn = dsSMUPPnew.UPrgn;
            UHpOfc = dsSMUPPprv.UPofc#;
            UHnOfc = dsSMUPPnew.UPofc#;
            UHpExt = dsSMUPPprv.UPext#;
            UHnExt = dsSMUPPnew.UPext#;
            UHpLob = dsSMUPPprv.UPlob;
            UHnLob = dsSMUPPnew.UPlob;
            UHpRal = dsSMUPPprv.UPral;
            UHnRal = dsSMUPPnew.UPral;
            UHpPal = dsSMUPPprv.UPpal;
            UHnPal = dsSMUPPnew.UPpal;
            UHpDt1 = dsSMUPPprv.UPdt1;
            UHnDt1 = dsSMUPPnew.UPdt1;
            UHpDt2 = dsSMUPPprv.UPdt2;
            UHnDt2 = dsSMUPPnew.UPdt2;
            UHpDt3 = dsSMUPPprv.UPdt3;
            UHnDt3 = dsSMUPPnew.UPdt3;
            UHpDt4 = dsSMUPPprv.UPdt4;
            UHnDt4 = dsSMUPPnew.UPdt4;
            UHpDt5 = dsSMUPPprv.UPdt5;
            UHnDt5 = dsSMUPPnew.UPdt5;
            UHpFl1 = dsSMUPPprv.UPfl1;
            UHnFl1 = dsSMUPPnew.UPfl1;
            UHpFl2 = dsSMUPPprv.UPfl2;
            UHnFl2 = dsSMUPPnew.UPfl2;
            UHpFl3 = dsSMUPPprv.UPfl3;
            UHnFl3 = dsSMUPPnew.UPfl3;
            UHpFl4 = dsSMUPPprv.UPfl4;
            UHnFl4 = dsSMUPPnew.UPfl4;
            UHpFl5 = dsSMUPPprv.UPfl5;
            UHnFl5 = dsSMUPPnew.UPfl5;
            UHpNu1 = dsSMUPPprv.UPnu1;
            UHnNu1 = dsSMUPPnew.UPnu1;
            UHpNu2 = dsSMUPPprv.UPnu2;
            UHnNu2 = dsSMUPPnew.UPnu2;
            UHpNu3 = dsSMUPPprv.UPnu3;
            UHnNu3 = dsSMUPPnew.UPnu3;
            UHpNu4 = dsSMUPPprv.UPnu4;
            UHnNu4 = dsSMUPPnew.UPnu4;
            UHpNu5 = dsSMUPPprv.UPnu5;
            UHnNu5 = dsSMUPPnew.UPnu5;
            UHpAl1 = dsSMUPPprv.UPal1;
            UHnAl1 = dsSMUPPnew.UPal1;
            UHpAl2 = dsSMUPPprv.UPal2;
            UHnAl2 = dsSMUPPnew.UPal2;
            UHpAl3 = dsSMUPPprv.UPal3;
            UHnAl3 = dsSMUPPnew.UPal3;
            UHpAl4 = dsSMUPPprv.UPal4;
            UHnAl4 = dsSMUPPnew.UPal4;
            UHpAl5 = dsSMUPPprv.UPal5;
            UHnAl5 = dsSMUPPnew.UPal5;
            UHpCo# = dsSMUPPprv.UPxCo#;
            UHnCo# = dsSMUPPnew.UPxCo#;
            UHpXEm = dsSMUPPprv.UPxEmp;
            UHnXEm = dsSMUPPnew.UPxEmp;
            UHpMrS = dsSMUPPprv.UPmrSu;
            UHnMrS = dsSMUPPnew.UPmrSu;
            UHpMrG = dsSMUPPprv.UPmrGr;
            UHnMrG = dsSMUPPnew.UPmrGr;
            UHpMrE = dsSMUPPprv.UPmrEm;
            UHnMrE = dsSMUPPnew.UPmrEm;
            UHpFei = dsSMUPPprv.UPfein;
            UHnFei = dsSMUPPnew.UPfein;
            UHpMal = dsSMUPPprv.UPmail;
            UHnMal = dsSMUPPnew.UPmail;
            UHpAd1 = dsSMUPPprv.UPadr1;
            UHnAd1 = dsSMUPPnew.UPadr1;
            UHpAd2 = dsSMUPPprv.UPadr2;
            UHnAd2 = dsSMUPPnew.UPadr2;
            UHpCty = dsSMUPPprv.UPcty;
            UHnCty = dsSMUPPnew.UPcty;
            UHpSta    = dsSMUPPprv.UPst;
            UHnSta    = dsSMUPPnew.UPst;
            UHpZip    = dsSMUPPprv.UPzip;
            UHnZip    = dsSMUPPnew.UPzip;
            UHpPhA    = dsSMUPPprv.UPphnA;
            UHnPhA    = dsSMUPPnew.UPphnA;
            UHpPhN    = dsSMUPPprv.UPphn#;
            UHnPhN    = dsSMUPPnew.UPphn#;
            UHpPhnTxt = dsSMUPPprv.UPphnTxt;
            UHnPhnTxt = dsSMUPPnew.UPphnTxt;
            UHpAlt#   = dsSMUPPprv.UPalt#;
            UHnAlt#   = dsSMUPPnew.UPalt#;
            UHpAltTxt = dsSMUPPprv.UPaltTxt;
            UHnAltTxt = dsSMUPPnew.UPaltTxt;
            UHpFxA    = dsSMUPPprv.UPfaxA;
            UHnFxA    = dsSMUPPnew.UPfaxA;
            UHpFx#    = dsSMUPPprv.UPfax#;
            UHnFx#    = dsSMUPPnew.UPfax#;
            UHpOfL    = dsSMUPPprv.UPoff#;
            UHnOfL    = dsSMUPPnew.UPoff#;
            UHpContUpd = dsSMUPPprv.UPcontUpd;
            UHnContUpd = dsSMUPPnew.UPcontUpd;

            Write suh0p;

         Endif;

       Endsr;

       //======================================================================
       Begsr ClrRstWCMC;
         scSflDspMC = *off;
         scSflClrMC = *on;
         Write rcdUserCfg;
         scSflClrMC = *off;
         scSflDspMC = *on;
         scSflRRNMC = 0;
       Endsr;

       //======================================================================
       Begsr ClrRstWCMS;
         scSflDspWC = *off;
         scSflClrWC = *on;
         Write rcdUserCfg;
         scSflClrWC = *off;
         scSflDspWC = *on;
         scSflRRNWC = 0;
       Endsr;

       //======================================================================
       Begsr DeleteRecords;
         Chain(n) inRRN SMUPP;
         If not %open(SMSLL2);
            Open SMSLL2;
         Endif;
         Chain UPusr ssl02;
         If %found;
            Delete ssl02;
         Endif;
         If isWCMS = *on;
            If not %open(WMUPL);
               Open WMUPL;
            Endif;
            Delete UPusr wup00;
         Endif;
         Exec SQL
          delete from SDTSP
           where TSTYPE = ' ' and TSUSR = :UPusr;
         DltRestrictions(UPco#:UPusr);
         Delete inRRN SMUPP;
       Endsr;

       //======================================================================
       Begsr LoadConfigDB;

         // SYSM
         UPusr = scUsr;
         UPusNm = scUsNm;
         UPtitl = scTitl;
         UPstaf = scStaf;
         Select;
         When scPhn# = 0;
            strPhn# = '0000000000';
         Other;
            strPhn# = %char(scPhn#);
         Endsl;
         UPphnA = %dec(%subst(strPhn#:1:3):3:0);
         UPphn# = %dec(%subst(strPhn#:4:7):7:0);
         UPphnTxt = scPhnTxt;
         UPalt# = scAlt#;
         UPaltTxt = scAltTxt;
         UPcontUpd =  scContUpd;
         UPstafUpd = scStafUpd;
         UPext# = scExt#;
         UPlvl = scLvl;
         UPtype = scType;
         UPiPgm = scIPgm;
         UPmail = scMail;
         UPal4 = scWrdP;
         UPpKey = scPrtKey;
         UPoutQ = scOutQ;
         UPmxRi = scMxRi;
         If scDsTab = *blanks;
            UPdstab = ' ';
            UPqdflt = ' ';
         Endif;
         If scDsTab = 'Policy Search';
            UPdstab = 'P';
            UPqdflt = ' ';
         Endif;
         If scDsTab = 'Claim Search';
            UPdstab = 'C';
            UPqdflt = ' ';
         Endif;
         If scDsTab = 'Quick Search';
            UPdstab = 'Q';
            UPqdflt = scQST;
         Endif;

         If UPmxRi > 0 and UPmxRi > dsSMUPPprv.UPmxRi;
            outTitle   = 'Information';
            outBtnText = 'Ok';
            outMessage = 'When the ''' + 'Maximum # of Recent Items''' +
             ' is increased, the system will begin adding new records to the +
             grid from that point forward. It will not automatically retrieve +
             older items.';
            PopOneBtn(outMessage:outTitle:outBtnText);
         Endif;
         UPdept = scDept;
         UPprDt = DtCYMD(scPrDt);
         UPco# = scCo#;
         UPxCo# = scXCo#;

         UPstat = scStat;
         UPuser = q1User;
         UPpgm  = q1Pgm;
         UPdt   = DateTo7(SystemDate);
         UPtime = TimeTo6(SystemTime);

         // WCMC
         If isWCMC = *on;
            UPfyr = scFyrWCMC;
            UPsub# = scSub#;
            UPmrSu = scMrSu;
            //*** Beg Chg *** 05/30/24 *** PHW115628 *******************
            //***UPfnd = scFndWCMC;
            If scFndWCMCC = *on; // Managed Care Grp# Changed
               UPfnd = scFndWCMC;
            Endif;
            //*** End Chg *** 05/30/24 *** PHW115628 *******************
            UPmrGr = scMrGr;
            UPemp# = scEmp#WCMC;
            Select;
            When scDivWCMC = *blanks;
               UPdiv = 0;
            Other;
               UPdiv = %int(scDivWCMC);
            Endsl;
            UPmrEm = scMrEm;
            Clear UPfl5;
            If scDivWCMC <> *blank and scMrEm = 'Y';
               UPfl5 = 'Y';
            Endif;
         Endif;

         // WCMS
         If isWCMS = *on;
            UPusr = scUsr;
            UPco# = scCo#;
            UPiPgm = scIPgm;
            UPlvl = scLvl;
            UPtype = scType;
            UPusNm = scUsNm;
            UPtitl = scTitl;
            UPstaf = scStaf;
            UPoutQ = scOutQ;
            UPfyr = scFyrWCMS;
            //*** Beg Chg *** 05/30/24 *** PHW115628 *******************
            //***UPfnd = scFndWCMS;
            If scFndWCMSC = *on; // Workman's Comp Grp# Entered?
               UPfnd = scFndWCMS;
            Endif;
            //*** End Chg *** 05/30/24 *** PHW115628 *******************
            UPxFnd = scXFnd;
            UPemp# = scEmp#WCMS;
            Select;
            When scDivWCMS = *blanks;
               UPdiv = 0;
            Other;
               UPdiv = %int(scDivWCMS);
            Endsl;
            UPxEmp = scXEmp;
            UPrAl = scRAl;
            UPpAl = scPAl;
            UPfl2 = scFl2;
            UPfein = scFein;
            UPoff# = scOff#;
            UPmRal = scMRal;
            UPcRal = scCRal;
            UPeRal = scERal;
            UPlRal = scLRal;
            UPrsv1 = scRsv1;
            UPrsv2 = scRsv2;
            UPrsv3 = scRsv3;
            UPrsv4 = scRsv4;
            UPtInc = scTInc;
            UPfl5 = *blank;
            If scDivWCMS <> *blank and scXEmp = 'Y';
               UPfl5 = 'Y';
            Endif;
            UPext# = scExt#;
            UPmail = scMail;
            UPal4 = scWrdP;
            UPmail = scMail;
            UPphnA = %dec(%subst(strPhn#:1:3):3:0);
            UPphn# = %dec(%subst(strPhn#:4:7):7:0);
            UPprDt = DtCYMD(scPrDt);
            UPstat = scStat;
            UPuser = UPuser;
            UPpgm  = UPpgm;
            UPdt   = UPdt;
            UPtime = UPtime;
         Endif;

         // DARTS
         If isDARTS = *on;
            SLusrP = scUsr;
            SLuDsc = scUsNm;
            SLsecL = scSecL;
            SLrAcc = scRAcc;
            SLf01 = scFl01;
            SLf02 = scFl02;
            SLf03 = scFl03;
            SLf04 = scFl04;
            SLf05 = scFl05;
            SLf06 = scFl06;
            SLf07 = scFl07;
            SLf08 = scFl08;
            SLf09 = scFl09;
            SLf10 = scFl10;
            SLf11 = scFl11;
            SLf12 = scFl12;
            SLf13 = scFl13;
            SLf14 = scFl14;
            SLf15 = scFl15;
            SLf16 = scFl16;
            SLf17 = scFl17;
            SLf18 = scFl18;
            SLf19 = scFl19;
            SLf20 = scFl20;
            SLf21 = scFl21;
            SLf22 = scFl22;
            SLf23 = scFl23;
            SLf24 = scFl24;
            // Always default to iDARTS;
            SLvDaD = 'D';
            SLalwR = scAlwR;
            SLeMal = scMail;
            SLrmCd = Upper(scRmCd);
            SLcode = Upper(scCode);
            SLcsId = Upper(scCsId);
            SLcCTf = scCCTf;
            SLcCIf = scCCIf;
            SLfold = scFold;
            SLeDc = scEDc;
            SLdDc = scDDc;
            SLaDc = scADc;
            SLdelC = scDelC;
            SLstat = scStat;
            SLuser = UPuser;
            SLpgm  = UPpgm;
            SLdt   = UPdt;
            SLtime = UPtime;
         Endif;
       Endsr;

       //======================================================================
       Begsr LoadConfigScr;
         strUsr = UPusr;
         // Clear unique fields from copied record
         isCopyMode = *off;
         If isCpyMode;
            isCopyMode = *on;
            scCopyFrom = %trim(UPusr) + ' - ' + %trim(UPusNm);
            Clear UPusr;
            Clear UPusNm;
            Clear UPtitl;
            Clear UPstaf;
            Clear UPphnA;
            Clear UPphn#;
            Clear UPphnTxt;
            Clear UPalt#;
            Clear UPaltTxt;
            Clear UPext#;
            Clear UPprDt;
            Clear UPoutQ;
            Clear UPmail;
            Clear UPstat;
         Endif;
         // SYSM
         scUsr = UPusr;
         scUsNm = UPusNm;
         scTitl = UPtitl;
         scStaf = UPstaf;
         scPhn# = %int(%editc(UPphnA:'X') + %editc(UPphn#:'X'));
         scPhnTxt = UPphnTxt;
         scAlt# = UPalt#;
         scAltTxt = UPaltTxt;
         scContUpd = UPcontUpd;
         scStafUpd = UPstafUpd;
         scExt# = UPext#;
         scLvl = UPlvl;
         scType = UPtype;
         scIPgm = UPiPgm;
         scPrDt = DtMDYY(UPprDt);
         If scPrDt = *zero;
            scPrDt = DateTo8(SystemDate);
         Endif;
         scOutQ = UPoutQ;
         scMail = UPmail;
         scWrdP = UPal4;
         scPrtKey= UPpKey;
         scMxRi = UPmxRi;
         scDept = UPdept;
         scDsTab = UPdstab;
         scQST = UPqdflt;
         If UPdstab = 'P';
            scDsTab = 'Policy Search';
         Endif;
         If UPdstab = 'C';
            scDsTab = 'Claim Search';
         Endif;
         If UPdstab = 'Q';
            scDsTab = 'Quick Search';
         Endif;

         isSnglCo# = *on;
         scCo# = UPco#;
         If scCo# = *zero;
            Setll *loval smcol;
            Read smcol;
            DoW Not %Eof(smcol);
               If scCo# <> *zero;
                  isSnglCo# = *off;
                  Clear scCo#;
                  Leave;
               Endif;
               Read smcol;
               scCo# = COco#;
            Enddo;
         Endif;
         scXCo# = UPxCo#;
         Select;
         When scXCo# = *blanks and isSnglCo# = *on;
            scXCo# = 'Y';
         Endsl;
         If isDspMode = *on;
            isSnglCo# = *on;
         Endif;

         scStat = UPstat;
         If scStat = *blanks;
            scStat = 'A';
         Endif;

         // WCMC
         If isWCMC = *on;
            scFyrWCMC = UPfyr;
            scSub# = UPsub#;
            scFndWCMC = UPfnd;
            scEmp#WCMC = UPemp#;
            scDivWCMC = *blanks;
            If UPfl5 = 'Y';
               scDivWCMC = %editc(UPdiv:'Z');
               If UPdiv = *zero;
                  scDivWCMC = RightJstfy('0':%size(scDivWCMC));
               Endif;
            Endif;

            scMrSu = UPmrSu;
            If scMrSu = *blanks;
               scMrSu = 'N';
            Endif;

            scMrGr = UPmrGr;
            If scMrGr = *blanks;
               scMrGr = 'N';
            Endif;

            scMrEm = UPmrEM;
            If scMrEm = *blanks;
               scMrEm = 'N';
            Endif;
            Exsr LoadRstWCMC;
         Endif;

         // WCMS
         If isWCMS = *on;
            Clear wup00;
            // Defaults
            //*
            UPxFnd = 'N';
            UPxEmp = 'N';
            UPfl2 = 'N';
            Chain(n) strUsr WMUPL;
            scFyrWCMS = UPfyr;
            scFndWCMS = UPfnd;
            scXFnd = UPxFnd;
            scEmp#WCMS = UPemp#;
            scXEmp = UPxEmp;
            scFein = UPfein;
            scDivWCMS = *blanks;
            If UPfl5 = 'Y';
               scDivWCMS = %editc(UPdiv:'Z');
               If UPdiv = *zero;
                  scDivWCMS = RightJstfy('0':%size(scDivWCMS));
               Endif;
            Endif;
            scFl2 = UPfl2;
            scRal = UPral;
            scPal = UPpal;
            scMRal = UPmRal;
            scCRal = UPcRal;
            scERal = UPeRal;
            scLRal = UPlRal;
            scRsv1 = UPrsv1;
            scRsv2 = UPrsv2;
            scRsv3 = UPrsv3;
            scRsv4 = UPrsv4;
            scTInc = UPtInc;
            scOff# = UPoff#;
            scSelCri = 'OFSTAT <> ''I'' and OFCO# = ' +
                       %Editc(SCCO#:'X');
            Exsr LoadRstWCMS;
         Endif;

         // DARTS
         If isDARTS = *on;
            Clear ssl02;
            // Defaults
            SLf01 = 'Y';
            SLf02 = 'Y';
            SLf03 = 'Y';
            SLf04 = 'Y';
            SLf05 = 'Y';
            SLf06 = 'Y';
            SLf07 = 'Y';
            SLf08 = 'Y';
            SLf09 = 'Y';
            SLf10 = 'Y';
            SLf11 = 'Y';
            SLf12 = 'Y';
            SLf13 = 'Y';
            SLf14 = 'Y';
            SLf15 = 'Y';
            SLf16 = 'Y';
            SLf17 = 'Y';
            SLf18 = 'Y';
            SLf19 = 'Y';
            SLf20 = 'Y';
            SLf21 = 'Y';
            SLf22 = 'Y';
            SLf23 = 'Y';
            SLf24 = 'Y';
            SLcCTf = 'Y';
            SLcCIf = 'Y';
            SLeDc = 'Y';
            SLdDc = 'Y';
            SLaDc = 'Y';
            SLdelC = 'Y';
            Chain(n) strUsr SMSLL2;
            If isCpyMode;
               Clear SLfold;
            Endif;
            scUDsc = UPusNm;
            scSecL = SLsecL;
            scAlWr = SLalWr;
            scEMal = UPmail;
            scCode = SLcode;
            scRmcd = SLrmCd;
            scCsId = SLcsId;
            scCCTf = SLcCTf;
            scCCIf = SLcCIf;
            scDelC = SLdelC;
            scFold = SLfold;
            scEDc = SLeDc;
            scDDc = SLdDc;
            scADc = SLaDc;
            scRAcc = SLrAcc;
            scFl01 = SLf01;
            scFl02 = SLf02;
            scFl03 = SLf03;
            scFl04 = SLf04;
            scFl05 = SLf05;
            scFl06 = SLf06;
            scFl07 = SLf07;
            scFl08 = SLf08;
            scFl09 = SLf09;
            scFl10 = SLf10;
            scFl11 = SLf11;
            scFl12 = SLf12;
            scFl13 = SLf13;
            scFl14 = SLf14;
            scFl15 = SLf15;
            scFl16 = SLf16;
            scFl17 = SLf17;
            scFl18 = SLf18;
            scFl19 = SLf19;
            scFl20 = SLf20;
            scFl21 = SLf21;
            scFl22 = SLf22;
            scFl23 = SLf23;
            scFl24 = SLf24;

            // Access and default to Visual DARTS
            Select;
            When SLvDad = 'D';
               scVDta = 'Y';
               scVDtD = 'Y';
            When SLvDad = 'Y';
               scVDta = 'Y';
               scVDtD = *blank;
            When SLVDad = *blank;
               scVDta = *blank;
               scVDtD = *blank;
            Endsl;
         Endif;

       Endsr;

       //======================================================================
       Begsr LoadRstWCMC;
         If not %open(FMSML);
            Open FMSML;
         Endif;
         If not %open(FMFNL);
            Open FMFNL;
         Endif;
         // Clear the subfile
         Clear decSflMC#;
         Exsr ClrRstWCMC;
         Select;
         When dspCmtWCMC = *off;
            strUsrRst = scUsr;
         Other;
            strUsrRst = strUsr;
         Endsl;
         dspEdtWCMC = *on;
         dspDltWCMC = *on;
         // Populate the subfile
         If not %open(smazl1);
            Open smazl1;
         Endif;
         Setll (scCo#:strUsrRst) smazl1;
         Dou %eof;
            Reade (scCo#:strUsrRst) smazl1;
            If %eof and strUsrRst <> scUsr;
               decSflMC# = scSflRRNMC;
               strUsrRst = scUsr;
               Setll (scCo#:strUsrRst) smazl1;
               Reade (scCo#:strUsrRst) smazl1;
            Endif;
            Select;
            When %eof;
               Iter;
            When strUsrRst <> scUsr and AZstat <> 'A';
               Iter;
            Endsl;

            scSflRRNMC += 1;
            scSub# = AZsub#;
            Clear SMname;
            Chain(n) (AZco#:AZsub#) fsm00;
            scSubName = SMname;
            scGrp = AZfnd;
            Clear FNname;
            Chain(n) (AZco#:AZsub#:AZfnd) ffn00;
            scGrpName = FNname;
            scEmpFr = AZfEmp;
            scEmpTo = AZtEmp;
            Select;
            When AZauth = 'A';
               scAuth = 'All';
            Other;
               scAuth = 'Inquiry';
            Endsl;
            scSflNxtMC = *off;
            dspEdtWCMC = *on;
            dspDltWCMC = *on;
            Select;
            When strUsrRst <> scUsr;
               scStatus = 'Imported';
               dspEdtWCMC = *off;
               dspDltWCMC = *off;
            When AZstat = 'A';
               scStatus = 'Active';
            Other;
               scStatus = 'Inactive';
            Endsl;
            scRRNWCMC  = mAzFds.rrn;
            Write sflRstWCMC;
         Enddo;
         If scSFLRRNMC < 1;
            dspCmtWCMC = *off;
         Endif;

         scSflRRNMC = 1;
       Endsr;

       //======================================================================
       Begsr LoadRstWCMS;
         If not %open(WMFNL);
            Open WMFNL;
         Endif;
         // Clear the subfile
         Clear decSflWC#;
         Exsr ClrRstWCMS;
         Select;
         When dspCmtWCMS = *off;
            strUsrRst = scUsr;
         Other;
            strUsrRst = strUsr;
         Endsl;
         dspEdtWCMS = *on;
         dspDltWCMS = *on;
         // Populate the subfile
         If not %open(smazl);
            Open smazl;
         Endif;
         Setll (scCo#:strUsrRst) smazl;
         Dou %eof;
            Reade (scCo#:strUsrRst) smazl;
            If %eof and strUsrRst <> scUsr;
               decSflWC# = scSflRRNWC;
               strUsrRst = scUsr;
               Setll (scCo#:strUsrRst) smazl;
               Reade (scCo#:strUsrRst) smazl;
            Endif;
            Select;
            When %eof;
               Iter;
            When strUsrRst <> scUsr and AZstat <> 'A';
               Iter;
            Endsl;

            scSflRRNWC += 1;
            scGrp = AZfnd;
            Clear FNname;
            Chain(n) (AZco#:AZfnd) wfn00;
            scGrpName = FNname;
            scEmpFr = AZfEmp;
            scEmpTo = AZtEmp;
            Select;
            When AZauth = 'A';
               scAuth = 'All';
            Other;
               scAuth = 'Inquiry';
            Endsl;
            scSflNxtWC = *off;
            dspEdtWCMS = *on;
            dspDltWCMS = *on;
            Select;
            When strUsrRst <> scUsr;
               scStatus = 'Imported';
               scSflNxtWC = *on;
               dspEdtWCMS = *off;
               dspDltWCMS = *off;
            When AZstat = 'A';
               scStatus = 'Active';
            Other;
               scStatus = 'Inactive';
            Endsl;
            scRRNWCMS  = wAzFds.rrn;
            Write sflRstWCMS;
         Enddo;
         If scSFLRRNWC < 1;
            dspCmtWCMS = *off;
         Endif;

         scSflRRNWC = 1;
       Endsr;

       //======================================================================
       //  Delete old recent items
       Begsr PurgeRecentItems;
         If (dsSMUPPnew.UPmxRi < dsSMUPPprv.UPmxRi) or
          (dsSMUPPprv.UPmxRi = 0 and dsSMUPPnew.UPmxRi < conMxRi);
            UpdateRecent(%editc(upFdsP.rrn:'X'):'P':' ');
         Endif;
       Endsr;

       //======================================================================
       Begsr UpdateRcds;
         Chain inRRN SMUPP;
         If isWCMS = *on;
            Chain UPusr wup00;
         Endif;
         If isDARTS = *on;
            Chain UPusr ssl02;
         Endif;
         Exsr LoadConfigDB;
         Update sup0p;
         Chain(n) upFdsP.RRN sup0p dsSMUPPnew;
         If isWCMS = *on;
            If %found(WMUPL);
               Update wup00;
            Else;
               Write wup00;
            Endif;
         Endif;
         If isDARTS = *on;
            If %found(SMSLL2);
               Update ssl02;
            Else;
               Write ssl0p;
            Endif;
         Endif;
       Endsr;
       //======================================================================
       Begsr WriteRcds;
         UPiUsr = UPuser;
         UPiDt  = UPdt;
         Write sup0p;
         Feod SMUPP;
         If isWCMS = *on;
            Write wup00;
            Feod WMUPL;
         Endif;
         If isDARTS = *on;
            SLiUsr = SLuser;
            Write ssl0p;
         Endif;
         Chain(n) upFdsp.RRN sup0p dsSMUPPnew;
       Endsr;
      /End-Free
       //======================================================================
     p UserUpdate      e

       //===================================================================
       // ValidRstAdd - Validate restriciton add
       //===================================================================
     p ValidRstAdd     b
     d ValidRstAdd     pi             1n
     d  inType                        4    const options(*nopass)

     dstrType          s              4    inz
      /Free
       If %parms >= 1;
          strType = inType;
       Endif;

       // Only process errors in Add Mode
       If isAddMode = *off;
          Return *on;
       Endif;

       scUsr = Upper(scUsr);
       Chain scUsr SMUPL;
       If %found;
          msUsr = 'MCR0592';
          mfUsr = 'MMESGG';
          erUsr = *on;
          Return *off;
       Endif;

       If scUsr = *blanks;
          msUsr = 'MCR0500';
          mfUsr = 'MMESGG';
          erUsr = *on;
          Return *off;
       Endif;

       Chain scCo# SMCOL;
       If not %found;
          msCo# = 'MCR0507';
          mfCo# = 'MMESGG';
          erCo# = *on;
          Return *off;
       Endif;
       Return *on;
      /End-free
     p ValidRstAdd     e
