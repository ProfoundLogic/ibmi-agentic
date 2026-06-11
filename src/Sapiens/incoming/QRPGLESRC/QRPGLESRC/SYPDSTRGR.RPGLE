     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt) bnddir('SYPDSTRGR')
     H dftactgrp(*no) actgrp(*NEW)
      *====================================================================
      * Instructions:
      *  - Keep EVERYTHING in alphabetical order
      *  - If a new file is needed
      *    - Add it to the compile time array
      *    - Add a procedure by the same name to process triggers (copy *SHELL)
      *      - Include prototype
      *    - Add the procedure address to dsProcPtr array
      *  - If a new trigger is needed for an existing file
      *    - Add the processing to the current procedure of the same
      *====================================================================
      * Revisions:
      *  01/19/21 C:180 Support 93617 - Changes done with date of 02/03/20
      *                 as this is part of ongoing develop for  WC2679.
      *
      *  02/03/20 C:180 Project: WC2679
      *                 Add trigger to WMALP, WMA3P, WMECP, and WMOOP.
      *
      *  05/22/19 C:33  Support: 79577
      *                 Use more global routines
      *
      *  07/17/18 C:33  Project: 129WUL  71158
      *                -Move address change outside of conditional
      *
      *  02/08/18 C:62  Project: 114WJD
      *                -Add trigger to WDELP
      *
      *  09/06/17 C:33  Project: WC2405
      *                -Account for Inactive triggers
      *
      * 08/15/17  C:62  Project: WC2501
      *                 Add trigger to SDCMP
      *
      * 08/09/17  C:33  Project: WC2405
      *                 Only update generic if SMTRPFRQ exists (no comments)
      *
      * 04/05/17 C:136 Project: 129WUL
      *                Add trigger to WMEMP
      *
      * 04/04/17  C:33  Project: WC2405
      *                 Include generic database rules (no comments)
      *====================================================================
     Fsmtrpfrq  if   e           k disk    rename(ptr01:ptr0pfrq) usropn

     D#ofFiles         c                   38
     DaryFiles         s             10    dim(#ofFiles) ctdata perrcd(1)
     DaryFilesEnv      s              4    dim(#ofFiles) alt(aryFiles)
     DdsProcPtr        ds
     D ptrFAEDP                        *   procPtr inz(%paddr(FAEDP))
     D ptrFAEHP                        *   procPtr inz(%paddr(FAEHP))
     D ptrFAEOP                        *   procPtr inz(%paddr(FAEOP))
     D ptrFAEPP                        *   procPtr inz(%paddr(FAEPP))
     D ptrFAEUP                        *   procPtr inz(%paddr(FAEUP))
     D ptrFDCAP                        *   procPtr inz(%paddr(FDCAP))
     D ptrFDDDP                        *   procPtr inz(%paddr(FDDDP))
     D ptrFDDEP                        *   procPtr inz(%paddr(FDDEP))
     D ptrFDDHP                        *   procPtr inz(%paddr(FDDHP))
     D ptrFDDPP                        *   procPtr inz(%paddr(FDDPP))
     D ptrFDDUP                        *   procPtr inz(%paddr(FDDUP))
     D ptrFMCDP                        *   procPtr inz(%paddr(FMCDP))
     D ptrFMPDP                        *   procPtr inz(%paddr(FMPDP))
     D ptrSADAP                        *   procPtr inz(%paddr(SADAP))
     D ptrSDCMP                        *   procPtr inz(%paddr(SDCMP))
     D ptrSMUPP                        *   procPtr inz(%paddr(SMUPP))
     D ptrWAMCP                        *   procPtr inz(%paddr(WAMCP))
     D ptrWDF2P                        *   procPtr inz(%paddr(WDF2P))
     D ptrWDF3P                        *   procPtr inz(%paddr(WDF3P))
     D ptrWDEHP                        *   procPtr inz(%paddr(WDEHP))
     D ptrWDELP                        *   procPtr inz(%paddr(WDELP))
     D ptrWDPAP                        *   procPtr inz(%paddr(WDPAP))
      *** Begin Add 02/03/20 *********************************************
     D ptrWMALP                        *   procPtr inz(%paddr(WMALP))
     D ptrWMA3P                        *   procPtr inz(%paddr(WMA3P))
      *** End   Add 02/03/20 *********************************************
     D ptrWMCDP                        *   procPtr inz(%paddr(WMCDP))
     D ptrWMCMP                        *   procPtr inz(%paddr(WMCMP))
      *** Begin Add 02/03/20 *********************************************
     D ptrWMECP                        *   procPtr inz(%paddr(WMECP))
      *** End   Add 02/03/20 *********************************************
     D ptrWMEMP                        *   procPtr inz(%paddr(WMEMP))
     D ptrWMFNP                        *   procPtr inz(%paddr(WMFNP))
     D ptrWMLLP                        *   procPtr inz(%paddr(WMLLP))
     D ptrWMOFP                        *   procPtr inz(%paddr(WMOFP))
      *** Begin Add 02/03/20 *********************************************
     D ptrWMOOP                        *   procPtr inz(%paddr(WMOOP))
      *** End   Add 02/03/20 *********************************************
     D ptrWMPDP                        *   procPtr inz(%paddr(WMPDP))
     D ptrWMRPP                        *   procPtr inz(%paddr(WMRPP))
     D ptrWMSHP                        *   procPtr inz(%paddr(WMSHP))
     D ptrWMWHP                        *   procPtr inz(%paddr(WMWHP))
     D ptrWTCNP                        *   procPtr inz(%paddr(WTCNP))
     D ptrWXELP                        *   procPtr inz(%paddr(WXELP))
     D  aryProcPtr                     *   overlay(dsProcPtr)
     D                                     procPtr dim(#ofFiles)

     DAddTrigger       pr
     D inFile                        10    const
     D inTrgTime                      8    const
     D inTrgEvent                     8    const
     D inPgm                         10    const
     D inTrgName                    128    const
     D inEnv                          4    const options(*nopass)

     DMessages         pr
     D inErrInd                       1n   const
     D inFile                        10    const
     D inLib                         10    const

     DQp0zLprintf      pr            10I 0 extProc('Qp0zLprintf')
     D szOutputStg                     *   value options(*string)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)

     DProcessGenFiles  pr

     DProcess          pr
     D inFile                        10    const
     D inEnv                          4    const options(*nopass)

     DProcessFile      pr                  extproc(ptrProcess)
     D FAEDP           pr
     D FAEHP           pr
     D FAEOP           pr
     D FAEPP           pr
     D FAEUP           pr
     D FDCAP           pr
     D FDDDP           pr
     D FDDEP           pr
     D FDDHP           pr
     D FDDPP           pr
     D FDDUP           pr
     D FMCDP           pr
     D FMPDP           pr
     D SADAP           pr
     D SDCMP           pr
     D SMUPP           pr
     D WAMCP           pr
     D WDF2P           pr
     D WDF3P           pr
     D WDEHP           pr
     D WDELP           pr
     D WDPAP           pr
      *** Begin Add 02/03/20 *********************************************
     D WMALP           pr
     D WMA3P           pr
      *** End   Add 02/03/20 *********************************************
     D WMCDP           pr
     D WMCMP           pr
      *** Begin Add 02/03/20 *********************************************
     D WMECP           pr
      *** End   Add 02/03/20 *********************************************
     D WMEMP           pr
     D WMFNP           pr
     D WMLLP           pr
     D WMOFP           pr
      *** Begin Add 02/03/20 *********************************************
     D WMOOP           pr
      *** End   Add 02/03/20 *********************************************
     D WMPDP           pr
     D WMRPP           pr
     D WMSHP           pr
     D WMWHP           pr
     D WTCNP           pr
     D WXELP           pr

     D/copy sPrModule
     D/copy sPrRunCmd
     D***  Begin Chg  ***  05/22/19  ***************************
     D*copy sPrTrigger
     D/copy sPrTrig
     D***  End   Chg  ***  05/22/19  ***************************

     D*entry
     DSYPDSTRGR        pr
     D inAction                       4    const
     D inFile                        10    const
     D inLib                         10    const
     D inQuoteLib                    10    const
     DSYPDSTRGR        pi
     D inAction                       4    const
     D inFile                        10    const
     D inLib                         10    const
     D inQuoteLib                    10    const

     Di1               s              5  0 inz
     DisProcComplete   s              1n   inz('0')
     DisAutoDiaryTrg   s              1n
     DisISOWorkComp    s              1n
     DptrProcess       s               *   procptr inz

      /Free
       isAutoDiaryTrg = IsModEnabled('Auto-Diary Triggers ');
       isISOWorkComp  = IsModEnabled('ISOWorkComp');

       Select;
       When inFile = '*ALL';
          For i1 = 1 to #ofFiles;
             Process(aryFiles(i1):aryFilesEnv(i1));
          Endfor;
       Other;
          i1 = %lookup(inFile:aryFiles:1);
          If i1 > 0 and i1 <= #ofFiles;
             Process(inFile:aryFilesEnv(i1));
          Else;
             Callp Qp0zLprintf(
              'No non-database rule(s) configured for %s. ' + X'25' :
              %trim(inFile));
          Endif;
       Endsl;
       ProcessGenFiles();
       *inlr = *on;
      /End-free

     P*===============================================================
     P*SHELL           b
     D*SHELL           pi
      /Free
      /End-free
     P*SHELL           e

     P*===============================================================
     PMessages         b
     DMessages         pi
     D inErrInd                       1n   const
     D inFile                        10    const
     D inLib                         10    const

     D abend           s              3    inz
     D strAction       s             10    inz
     D strLib          s             10    inz
      /free
       If inErrInd;
        abend = 'un';
       Endif;
       Select;
       When inAction = '*RMV';
          strAction = 'removed';
       When inAction = '*ADD';
          strAction = 'added';
       Endsl;
       callp Qp0zLprintf('Trigger(s) %ssuccessfully %s ' +
         'to %s in library %s.  ' + X'25' :
         %trim(abend):%trim(strAction):%trim(inFile):%trim(inLib));
      /end-free
     PMessages         e

     P*===============================================================
     PProcess          b
     DProcess          pi
     D inFile                        10    const
     D inEnv                          4    const options(*nopass)

     DdecProcess       s              3i 0 inz
     DstrCmd           s           3000    inz
     DstrLib           s             10

      /Free
       strLib = inLib;
       If %parms >= 2 and inEnv = '*QTE';
          strLib = inQuoteLib;
       Endif;
       Monitor;
          Select;
          When inAction = '*ADD';
             Clear ptrProcess;
             decProcess = %lookup(inFile:aryFiles:1);
             If decProcess > 0;
                ptrProcess = aryProcPtr(decProcess);
             Endif;
             If ptrProcess <> *null;
                ProcessFile();
             Else;
                Callp Qp0zLprintf(
                 'No non-database rule(s) configured for %s. ' + X'25' :
                 %trim(inFile));
             Endif;
          When inAction = '*RMV';
             strCmd = 'RMVPFTRG FILE(' + %trim(strLib) + '/' + %trim(inFile) +
              ') TRGLIB(*FILE)';
             RunCmd(strCmd:%len(%trim(strCmd)));
             Messages('0':inFile:strLib);
          Endsl;
       On-error;
          Messages('1':inFile:strLib);
       Endmon;
      /End-free
     PProcess          e

     P*===============================================================
     PProcessGenFiles  b
     DProcessGenFiles  pi

     DstrCmd           s           3000    inz
     D strSql          s            550    inz
      /Free
       Monitor;
          If not %open(smtrpfrq);
             Open smtrpfrq;
          Endif;
       On-error;
          Return;
       Endmon;
       Setll *loval smtrpfrq;
       Dou %eof;
          Read smtrpfrq;
          Select;
          When %eof;
             Iter;
           //***  Begin Add  ***  09/06/17  *******************************
           When TRstat = 'I';
              Iter;
           //***  End   Add  ***  09/06/17  *******************************
          When inFile = '*ALL';
             Exsr Process;
          When inFile = TRfile and inLib = TRlibrary;
             Exsr Process;
          Endsl;
       Enddo;
       //===============
       Begsr Process;
        Monitor;
           Select;
           When inAction = '*ADD';
              CreateFileTrg(TRtrgName);
           When inAction = '*RMV';
              strSql = 'DROP TRIGGER ' + %trim(TRtrgName);
              strCmd = 'SYRUNSQL STMT(''' + %trim(strSql) + ''')';
            //strCmd = 'RMVPFTRG FILE(' + %trim(TRlibrary) + '/' +
            // %trim(TRfile) + ') TRGLIB(*FILE) ' +
            // 'TRG(' + %trim(TRtrgName) + ')';
              RunCmd(strCmd:%len(%trim(strCmd)));
           Endsl;
           Messages('0':TRfile:TRlibrary);
        On-Error;
           Messages('1':TRfile:TRlibrary);
        Endmon;
       Endsr;
      /End-free
     PProcessGenFiles  e

     P*===============================================================
     PAddTrigger       b
     DAddTrigger       pi
     D inFile                        10    const
     D inTrgTime                      8    const
     D inTrgEvent                     8    const
     D inPgm                         10    const
     D inTrgName                    128    const
     D inEnv                          4    const options(*nopass)

     DstrCmd           s           3000    inz
     DstrLib           s             10
      /Free
       strLib = inLib;
       If %parms >= 6 and inEnv = '*QTE';
          strLib = inQuoteLib;
       Endif;
       strCmd = 'ADDPFTRG FILE(' + %trim(strLib) + '/' + %trim(inFile) + ')' +
        ' TRGTIME(' + %trim(inTrgTime) + ')' +
        ' TRGEVENT(' + %trim(inTrgEvent) + ')' +
        ' PGM(' + %trim(inPgm) + ')' +
        ' TRG(' + %trim(inTrgName) + ')' +
        ' RPLTRG(*YES) ALWREPCHG(*YES)';
       RunCmd(strCmd:%len(%trim(strCmd)));
       Messages('0':inFile:strLib);
      /End-free
     PAddTrigger       e

     P*===============================================================
     PFAEDP            b
     DFAEDP            pi
      /Free
       If isISOWorkComp;
          // FAEDP INSERT
          AddTrigger('FAEDP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FAEDP_AFTER_INSERT');
          // FAEDP UPDATE
          AddTrigger('FAEDP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FAEDP_AFTER_UPDATE');
          // FAEDP DELETE
          AddTrigger('FAEDP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FAEDP_BEFORE_DELETE');
       Endif;
      /End-free
     PFAEDP            e

     P*===============================================================
     PFAEHP            b
     DFAEHP            pi
      /Free
       If isISOWorkComp;
          // FAEHP INSERT
          AddTrigger('FAEHP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FAEHP_AFTER_INSERT');
          // FAEHP UPDATE
          AddTrigger('FAEHP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FAEHP_AFTER_UPDATE');
          // FAEHP DELETE
          AddTrigger('FAEHP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FAEHP_BEFORE_DELETE');
       Endif;
      /End-free
     PFAEHP            e

     P*===============================================================
     PFAEOP            b
     DFAEOP            pi
      /Free
       If isISOWorkComp;
          // FAEOP INSERT                                               */
          AddTrigger('FAEOP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FAEOP_AFTER_INSERT');
          // FAEOP UPDATE                                               */
          AddTrigger('FAEOP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FAEOP_AFTER_UPDATE');
          // FAEOP DELETE                                               */
          AddTrigger('FAEOP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FAEOP_BEFORE_DELETE');
       Endif;
      /End-free
     PFAEOP            e

     P*===============================================================
     PFAEPP            b
     DFAEPP            pi
      /Free
       If isISOWorkComp;
          // FAEPP INSERT                                               */
          AddTrigger('FAEPP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FAEPP_AFTER_INSERT');
          // FAEPP UPDATE                                               */
          AddTrigger('FAEPP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FAEPP_AFTER_UPDATE');
          // FAEPP DELETE                                               */
          AddTrigger('FAEPP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FAEPP_BEFORE_DELETE');
       Endif;
      /End-free
     PFAEPP            e

     P*===============================================================
     PFAEUP            b
     DFAEUP            pi
      /Free
       If isISOWorkComp;
          // FAEUP INSERT                                               */
          AddTrigger('FAEUP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FAEUP_AFTER_INSERT');
          // FAEUP UPDATE                                               */
          AddTrigger('FAEUP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FAEUP_AFTER_UPDATE');
          // FAEUP DELETE                                               */
          AddTrigger('FAEUP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FAEUP_BEFORE_DELETE');
       Endif;
      /End-free
     PFAEUP            e

     P*===============================================================
     PFDCAP            b
     DFDCAP            pi
      /Free
       If isISOWorkComp;
          // FDCAP INSERT                                               */
          AddTrigger('FDCAP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FDCAP_AFTER_INSERT');
          // FDCAP UPDATE                                               */
          AddTrigger('FDCAP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FDCAP_AFTER_UPDATE');
          // FDCAP DELETE                                               */
          AddTrigger('FDCAP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FDCAP_BEFORE_DELETE');
       Endif;
      /End-free
     PFDCAP            e

     P*===============================================================
     PFDDDP            b
     DFDDDP            pi
      /Free
       If isISOWorkComp;
          // FDDDP INSERT                                               */
          AddTrigger('FDDDP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FDDDP_AFTER_INSERT');
          // FDDDP UPDATE                                               */
          AddTrigger('FDDDP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FDDDP_AFTER_UPDATE');
          // FDDDP DELETE                                               */
          AddTrigger('FDDDP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FDDDP_BEFORE_DELETE');
       Endif;
      /End-free
     PFDDDP            e

     P*===============================================================
     PFDDEP            b
     DFDDEP            pi
      /Free
       If isISOWorkComp;
          // FDDEP INSERT                                               */
          AddTrigger('FDDEP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FDDEP_AFTER_INSERT');
          // FDDEP UPDATE                                               */
          AddTrigger('FDDEP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FDDEP_AFTER_UPDATE');
          // FDDEP DELETE                                               */
          AddTrigger('FDDEP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FDDEP_BEFORE_DELETE');
       Endif;
      /End-free
     PFDDEP            e

     P*===============================================================
     PFDDHP            b
     DFDDHP            pi
      /Free
       If isISOWorkComp;
          // FDDHP INSERT                                               */
          AddTrigger('FDDHP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FDDHP_AFTER_INSERT');
          // FDDHP UPDATE                                               */
          AddTrigger('FDDHP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FDDHP_AFTER_UPDATE');
          // FDDHP DELETE                                               */
          AddTrigger('FDDHP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FDDHP_BEFORE_DELETE');
       Endif;
      /End-free
     PFDDHP            e

     P*===============================================================
     PFDDPP            b
     DFDDPP            pi
      /Free
       If isISOWorkComp;
          // FDDPP INSERT                                               */
          AddTrigger('FDDPP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FDDPP_AFTER_INSERT');
          // FDDPP UPDATE                                               */
          AddTrigger('FDDPP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FDDPP_AFTER_UPDATE');
          // FDDPP DELETE                                               */
          AddTrigger('FDDPP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FDDPP_BEFORE_DELETE');
       Endif;
      /End-free
     PFDDPP            e

     P*===============================================================
     PFDDUP            b
     DFDDUP            pi
      /Free
       If isISOWorkComp;
          // FDDUP INSERT                                               */
          AddTrigger('FDDUP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FDDUP_AFTER_INSERT');
          // FDDUP UPDATE                                               */
          AddTrigger('FDDUP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FDDUP_AFTER_UPDATE');
          // FDDUP DELETE                                               */
          AddTrigger('FDDUP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FDDUP_BEFORE_DELETE');
       Endif;
      /End-free
     PFDDUP            e

     P*===============================================================
     PFMCDP            b
     DFMCDP            pi
      /Free
       If isISOWorkComp;
          // FMCDP INSERT                                               */
          AddTrigger('FMCDP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FMCDP_AFTER_INSERT');
          // FMCDP UPDATE                                               */
          AddTrigger('FMCDP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FMCDP_AFTER_UPDATE');
          // FMCDP DELETE                                               */
          AddTrigger('FMCDP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FMCDP_BEFORE_DELETE');
       Endif;
      /End-free
     PFMCDP            e

     P*===============================================================
     PFMPDP            b
     DFMPDP            pi
      /Free
       If isISOWorkComp;
          // FMPDP INSERT                                               */
          AddTrigger('FMCDP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_FMPDP_AFTER_INSERT');
          // FMPDP UPDATE                                               */
          AddTrigger('FMPDP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_FMPDP_AFTER_UPDATE');
          // FMPDP DELETE                                               */
          AddTrigger('FMPDP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_FMPDP_BEFORE_DELETE');
       Endif;
      /End-free
     PFMPDP            e

     P*===============================================================
     PSADAP            b
     DSADAP            pi
      /Free
       If isISOWorkComp;
          // SADAP INSERT                                               */
          AddTrigger('SADAP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_SADAP_AFTER_INSERT');
          // SADAP UPDATE                                               */
          AddTrigger('SADAP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_SADAP_AFTER_UPDATE');
          // SADAP DELETE                                               */
          AddTrigger('SADAP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_SADAP_BEFORE_DELETE');
       Endif;
      /End-free
     PSADAP            e

     P*===============================================================
     PSDCMP            b
     DSDCMP            pi
      /Free
       If isAutoDiaryTrg;
          // SDCMP INSERT                                               */
          AddTrigger('SDCMP':'*AFTER':'*INSERT':'STDRTTRG'
           :'STDRTTRG_SDCMP_AFTER_INSERT');
          // SDCMP UPDATE                                               */
          AddTrigger('SDCMP':'*AFTER':'*UPDATE':'STDRTTRG'
           :'STDRTTRG_SDCMP_AFTER_UPDATE');
          // SDCMP DELETE                                               */
          AddTrigger('SDCMP':'*BEFORE':'*DELETE':'STDRTTRG'
           :'STDRTTRG_SDCMP_BEFORE_DELETE');
       Endif;
      /End-free
     PSDCMP            e

     P*===============================================================
     PSMUPP            b
     DSMUPP            pi
      /Free
       If isISOWorkComp;
          // SMUPP INSERT                                               */
          AddTrigger('SMUPP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_SMUPP_AFTER_INSERT');
          // SMUPP UPDATE                                               */
          AddTrigger('SMUPP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_SMUPP_AFTER_UPDATE');
          // SMUPP DELETE                                               */
          AddTrigger('SMUPP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_SMUPP_BEFORE_DELETE');
       Endif;
      /End-free
     PSMUPP            e

     P*===============================================================
     PWAMCP            b
     DWAMCP            pi
      /Free
       If isAutoDiaryTrg;
          // WAMCP Insert                                               */
          AddTrigger('WAMCP':'*AFTER':'*INSERT':'STAUTODRY'
           :'WAMCP_Insert');
       Endif;
      /End-free
     PWAMCP            e

     P*===============================================================
     PWDF2P            b
     DWDF2P            pi
      /Free
       If isISOWorkComp;
          // WDF2P INSERT                                               */
          AddTrigger('WDF2P':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WDF2P_AFTER_INSERT');
          // WDF2P UPDATE                                               */
          AddTrigger('WDF2P':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WDF2P_AFTER_UPDATE');
          // WDF2P DELETE                                               */
          AddTrigger('WDF2P':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WDF2P_BEFORE_DELETE');
       Endif;
      /End-free
     PWDF2P            e

     P*===============================================================
     PWDF3P            b
     DWDF3P            pi
      /Free
       If isISOWorkComp;
          // WDF3P INSERT                                               */
          AddTrigger('WDF3P':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WDF3P_AFTER_INSERT');
          // WDF3P UPDATE                                               */
          AddTrigger('WDF3P':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WDF3P_AFTER_UPDATE');
          // WDF3P DELETE                                               */
          AddTrigger('WDF3P':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WDF3P_BEFORE_DELETE');
       Endif;
      /End-free
     PWDF3P            e

     P*===============================================================
     PWDEHP            b
     DWDEHP            pi
      /Free
       If isAutoDiaryTrg;
          // WDEHP Insert                                               */
          AddTrigger('WDEHP':'*AFTER':'*INSERT':'STAUTODRY'
           :'WDEHP_Insert');
       Endif;
      /End-free
     PWDEHP            e

     P*===============================================================
     PWDELP            b
     DWDELP            pi
      /Free
       If isISOWorkComp;
          // WDELP INSERT                                               */
          AddTrigger('WDELP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WDELP_AFTER_INSERT');
          // WDELP UPDATE                                               */
          AddTrigger('WDELP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WDELP_AFTER_UPDATE');
          // WDELP DELETE                                               */
          AddTrigger('WDELP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WDELP_BEFORE_DELETE');
       Endif;
      /End-free
     PWDELP            e

     P*===============================================================
     PWDPAP            b
     DWDPAP            pi
      /Free
       If isAutoDiaryTrg;
          // WDPAP Insert                                               */
          AddTrigger('WDPAP':'*AFTER':'*INSERT':'STAUTODRY'
           :'WDPAP_Insert');
       Endif;
      /End-free
     PWDPAP            e

      *** Begin Add 02/03/20 ********************************************
     P*===============================================================
     PWMALP            b
     DWMALP            pi
      /Free
       // WMALP INSERT                                               */
       AddTrigger('WMALP':'*AFTER':'*INSERT':'STTRIGGER'
        :'STTRIGGER_WMALP_AFTER_INSERT');

       // WMALP UPDATE                                               */
       AddTrigger('WMALP':'*AFTER':'*UPDATE':'STTRIGGER'
        :'STTRIGGER_WMALP_AFTER_UPDATE');

       // WMALP DELETE                                               */
       AddTrigger('WMALP':'*BEFORE':'*DELETE':'STTRIGGER'
        :'STTRIGGER_WMALP_BEFORE_DELETE');
      /End-free
     PWMALP            e

     P*===============================================================
     PWMA3P            b
     DWMA3P            pi
      /Free
       // WMA3P ADD                                                  */
       AddTrigger('WMA3P':'*AFTER':'*INSERT':'STTRIGGER'
        :'STTRIGGER_WMA3P_AFTER_INSERT');

       // WMA3P UPDATE                                               */
       AddTrigger('WMA3P':'*AFTER':'*UPDATE':'STTRIGGER'
        :'STTRIGGER_WMA3P_AFTER_UPDATE');

       // WMA3P DELETE                                               */
       AddTrigger('WMA3P':'*BEFORE':'*DELETE':'STTRIGGER'
        :'STTRIGGER_WMA3P_BEFORE_DELETE');
      /End-free
     PWMA3P            e
      *** End   Add 02/03/20 *********************************************

     P*===============================================================
     PWMCDP            b
     DWMCDP            pi
      /Free
       If isISOWorkComp;
          // WMCDP INSERT                                               */
          AddTrigger('WMCDP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMCDP_AFTER_INSERT');
          // WMCDP UPDATE                                               */
          AddTrigger('WMCDP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMCDP_AFTER_UPDATE');
          // WMCDP DELETE                                               */
          AddTrigger('WMCDP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMCDP_BEFORE_DELETE');
       Endif;
       If isAutoDiaryTrg;
          // WMCDP Update                                               */
          AddTrigger('WMCDP':'*AFTER':'*UPDATE':'STAUTODRY'
           :'WMCDP_Update');
       Endif;
      /End-free
     PWMCDP            e

     P*===============================================================
     PWMCMP            b
     DWMCMP            pi
      /Free
       If isISOWorkComp;
          // WMCMP INSERT                                               */
          AddTrigger('WMCMP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMCMP_AFTER_INSERT');
          // WMCMP UPDATE                                               */
          AddTrigger('WMCMP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMCMP_AFTER_UPDATE');
          // WMCMP DELETE                                               */
          AddTrigger('WMCMP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMCMP_BEFORE_DELETE');
       Endif;
       If isAutoDiaryTrg;
          // WMCMP Update                                               */
          AddTrigger('WMCMP':'*AFTER':'*UPDATE':'STAUTODRY'
           :'WMCMP_Update');
       Endif;
      /End-free
     PWMCMP            e
      *** Begin Add 02/03/20 ********************************************
     P*===============================================================
     PWMECP            b
     DWMECP            pi
      /Free
       // WMECP ADD                                                  */
       AddTrigger('WMECP':'*AFTER':'*INSERT':'STTRIGGER'
        :'STTRIGGER_WMECP_AFTER_INSERT');

       // WMECP UPDATE                                               */
       AddTrigger('WMECP':'*AFTER':'*UPDATE':'STTRIGGER'
        :'STTRIGGER_WMECP_AFTER_UPDATE');

       // WMECP DELETE                                               */
       AddTrigger('WMECP':'*BEFORE':'*DELETE':'STTRIGGER'
        :'STTRIGGER_WMECP_BEFORE_DELETE');
      /End-free
     PWMECP            e
      *** End   Add 02/03/20 ********************************************

     P*===============================================================
     PWMEMP            b
     DWMEMP            pi
      /Free
       If isISOWorkComp;
          // WMEMP INSERT                                               */
          AddTrigger('WMEMP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMEMP_AFTER_INSERT');

          //*** Begin Delete 02/03/20 ***********************************
          // WMEMP UPDATE                                               */
          //AddTrigger('WMEMP':'*AFTER':'*UPDATE':'STTRIGGER'
          // :'STTRIGGER_WMEMP_AFTER_UPDATE');
          //*** End   Delete 02/03/20 ***********************************

          // WMEMP DELETE                                               */
          AddTrigger('WMEMP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMEMP_BEFORE_DELETE');
        //***  Begin Del  ***  07/17/18  ********************************
          // WMEMP UPDATE for 129WUL
        //AddTrigger('WMEMP':'*AFTER':'*UPDATE':' STAUTODRY'
        // :' STAUTODRY_WMEMP_AFTER_UPDATE');
        //***  End   Del  ***  07/17/18  ********************************
       Endif;


       //***  Begin Add  ***  07/17/18  ********************************
       // WMEMP UPDATE for 129WUL
       AddTrigger('WMEMP':'*AFTER':'*UPDATE':' STAUTODRY'
        :' STAUTODRY_WMEMP_AFTER_UPDATE');
       //***  End   Add  ***  07/17/18  ********************************

       //*** Begin Add   02/03/20 **************************************
       // WMEMP UPDATE
       AddTrigger('WMEMP':'*AFTER':'*UPDATE':'STTRIGGER'
        :'STTRIGGER_WMEMP_AFTER_UPDATE');
       //*** End   Add 02/03/20 ****************************************

      /End-free
     PWMEMP            e

     P*===============================================================
     PWMFNP            b
     DWMFNP            pi
      /Free
       If isISOWorkComp;
          // WMFNP INSERT                                               */
          AddTrigger('WMFNP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMFNP_AFTER_INSERT');
          // WMFNP UPDATE                                               */
          AddTrigger('WMFNP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMFNP_AFTER_UPDATE');
          // WMFNP DELETE                                               */
          AddTrigger('WMFNP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMFNP_BEFORE_DELETE');
       Endif;
      /End-free
     PWMFNP            e

     P*===============================================================
     PWMLLP            b
     DWMLLP            pi
      /Free
       If isISOWorkComp;
          // WMLLP INSERT                                               */
          AddTrigger('WMLLP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMLLP_AFTER_INSERT');
          // WMLLP UPDATE                                               */
          AddTrigger('WMLLP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMLLP_AFTER_UPDATE');
          // WMLLP DELETE                                               */
          AddTrigger('WMLLP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMLLP_BEFORE_DELETE');
       Endif;
      /End-free
     PWMLLP            e

     P*===============================================================
     PWMOFP            b
     DWMOFP            pi
      /Free
       If isISOWorkComp;
          // WMOFP INSERT                                               */
          AddTrigger('WMOFP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMFOP_AFTER_INSERT');
          // WMOFP UPDATE                                               */
          AddTrigger('WMOFP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMOFP_AFTER_UPDATE');
          // WMOFP DELETE                                               */
          AddTrigger('WMOFP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMOFP_BEFORE_DELETE');
       Endif;
      /End-free
     PWMOFP            e
      *** Begin Add 02/03/20 ********************************************
     P*===============================================================
     PWMOOP            b
     DWMOOP            pi
      /Free
       // WMOOP INSERT                                               */
       AddTrigger('WMOOP':'*AFTER':'*INSERT':'STTRIGGER'
        :'STTRIGGER_WMOOP_AFTER_INSERT');

       // WMOOP UPDATE                                               */
       AddTrigger('WMOOP':'*AFTER':'*UPDATE':'STTRIGGER'
        :'STTRIGGER_WMOOP_AFTER_UPDATE');

       // WMOOP DELETE                                               */
       AddTrigger('WMOOP':'*BEFORE':'*DELETE':'STTRIGGER'
        :'STTRIGGER_WMOOP_BEFORE_DELETE');
      /End-free
     PWMOOP            e
      *** End   Add 02/03/20 ********************************************

     P*===============================================================
     PWMPDP            b
     DWMPDP            pi
      /Free
       If isISOWorkComp;
          // WMPDP INSERT                                               */
          AddTrigger('WMPDP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMPDP_AFTER_INSERT');
          // WMPDP UPDATE                                               */
          AddTrigger('WMPDP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMPDP_AFTER_UPDATE');
          // WMPDP DELETE                                               */
          AddTrigger('WMPDP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMPDP_BEFORE_DELETE');
       Endif;
      /End-free
     PWMPDP            e

     P*===============================================================
     PWMRPP            b
     DWMRPP            pi
      /Free
       If isISOWorkComp;
          // WMRPP INSERT                                               */
          AddTrigger('WMRPP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMRPP_AFTER_INSERT');
          // WMRPP UPDATE                                               */
          AddTrigger('WMRPP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMRPP_AFTER_UPDATE');
          // WMRPP DELETE                                               */
          AddTrigger('WMRPP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMRPP_BEFORE_DELETE');
       Endif;
      /End-free
     PWMRPP            e

     P*===============================================================
     PWMSHP            b
     DWMSHP            pi
      /Free
       If isAutoDiaryTrg;
          // WMSHP Insert                                               */
          AddTrigger('WMSHP':'*AFTER':'*INSERT':'STAUTODRY'
           :'WMSHP_Insert');
          // WMSHP Update                                               */
          AddTrigger('WMSHP':'*AFTER':'*UPDATE':'STAUTODRY'
           :'WMSHP_Update');
       Endif;
      /End-free
     PWMSHP            e

     P*===============================================================
     PWMWHP            b
     DWMWHP            pi
      /Free
       If isISOWorkComp;
          // WMWHP INSERT                                               */
          AddTrigger('WMWHP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WMWHP_AFTER_INSERT');
          // WMWHP UPDATE                                               */
          AddTrigger('WMWHP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WMWHP_AFTER_UPDATE');
          // WMWHP DELETE                                               */
          AddTrigger('WMWHP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WMWHP_BEFORE_DELETE');
       Endif;
      /End-free
     PWMWHP            e

     P*===============================================================
     PWTCNP            b
     DWTCNP            pi
      /Free
       If isISOWorkComp;
          // WTCNP INSERT                                               */
          AddTrigger('WTCNP':'*AFTER':'*INSERT':'STTRIGGER'
           :'STTRIGGER_WTCNP_AFTER_INSERT');
          // WTCNP UPDATE                                               */
          AddTrigger('WTCNP':'*AFTER':'*UPDATE':'STTRIGGER'
           :'STTRIGGER_WTCNP_AFTER_UPDATE');
          // WTCNP DELETE                                               */
          AddTrigger('WTCNP':'*BEFORE':'*DELETE':'STTRIGGER'
           :'STTRIGGER_WTCNP_BEFORE_DELETE');
       Endif;
      /End-free
     PWTCNP            e

     P*===============================================================
     PWXELP            b
     DWXELP            pi
      /Free
       If isAutoDiaryTrg;
          // WXELP Insert                                               */
          AddTrigger('WXELP':'*AFTER':'*INSERT':'STAUTODRY'
           :'WXELP_Insert':'*QTE');
       Endif;
      /End-free
     PWXELP            e
**
FAEDP
FAEHP
FAEOP
FAEPP
FAEUP
FDCAP
FDDDP
FDDEP
FDDHP
FDDPP
FDDUP
FMCDP
FMPDP
SADAP
SDCMP
SMUPP
WAMCP
WDF2P
WDF3P
WDEHP
WDELP
WDPAP
WMALP
WMA3P
WMCDP
WMCMP
WMECP
WMEMP
WMFNP
WMLLP
WMOFP
WMOOP
WMPDP
WMRPP
WMSHP
WMWHP
WTCNP
WXELP     *QTE
