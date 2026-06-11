     *========================================================================*
     H*Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description:  This copybook will be used to declare the protype of the
     h*  procedure which will load Claim action panel in Tranform
     h*
     h*  Called from: Many prograns
     h*  Calls to:    None
     h*
     h*
     h*  Created on 3/7/2012 by C:189 for Project WC2115
     h**********************************************************************
     h* Revisions:
    h*
     h*  03/02/20 C:300 Support: 82990 (LCTA)
     h*                 WTCLMSUM program changed from RPGLE to SQLRPGLE
      *
      * 01/07/16 C:65  Project: WC2401
      *                Add prototype for window api.
    h*
    h*  04/29/15 C:65  Project: SY251
    h*                 Added hidden field for Scriptura Module flag.
     h*
     h*  09/04/14 C:279 Project: 114WOE
     h*                 Added prototype for isInstExist.
     h*
     h*  07/28/14 C:295 Project: 114WMD
     h*                 Modified the action panel to use to load
     h*                 Claim Action Plan Tab.
     h*
     h*
     h**********************************************************************
     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d* Loads Action Panel
     d LoadActPan      pr
     d****** Beg Add ********* 07/28/14 ******************************
     d inClmPln                       1n   options(*nopass)
     d****** End Add ********* 07/28/14 ******************************

     d* Move Database to Screen Fields
     d MoveDbSc        pr

     d* Move Screen to Database Fields
     d MoveScDb        pr

     d* Update Claim Information
     d UpdateRecord    pr

     d* Claimr Search
     d ClmSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     D*** begin add ***  04/29/15  ************************************************
     d* System Flag API for Scriptura
     d GetScriptura    pr                  extpgm('SA047R')
     d   outSysFlag                   1
     D*** end add *****  04/29/15  ************************************************

     d**** begin add ***  01/07/16  ****************************************
     d PopupWindow     pr                  extpgm('WTPOPUP')
     d outProgram_                   40    const
     d outRRN_                        9  0 const
     d outMode_                       2    const
     d outData_                     500    const
     d**** end add *****  01/07/16  ****************************************

      *** beg chg ***** 03/02/20 ****************************************
     d***** /copy sPrModule
     d/include sPrModule
     d***** /copy WTSearchDs
     d/include WTSearchDs
      *** end chg ***** 03/02/20 ****************************************
     D noEMail         c                   Const('No E-Mail Available')
     D kkPcnt          s              1
     D date            s               d   datfmt(*usa)
     D xxCxDate        s              7  0
     D xxRRN           s              9  0
     D xxEmRRN1        s              9  0
     D xxName          s             40
     D xxPad1          s             40
     D xxPzip          s              9  0
     D xxPcty          s             25
     D xxMad1          s             40
     D xxMzip          s              9  0
     D xxMcty          s             25
     D xxPst           s              2
     D xxMst           s              2
     D xxPhn#          s              7  0
     D xxArea          s              3  0
     D xxMode          s              1
     D strRRN          s              9
     D intRRN          s              9  0
     D strIncLt1       s              2
     D strIncLt2       s              2
     D intInEffDt1     s              7  0
     D intInEffDt2     s              7  0
     D strLiblmtD      s             30
     D sysDate7        s              7  0
     D intPolEfdt      s              7  0
     D programIn       s             10
     D tabNameIn       s             50
     D modeIn          s             50    inz('C')
     D callIn          s             50    inz(' ')
     D rrnOut          s             50    inz('000000000')
     D selectIn        s           1024
     D OutincLt1D      s             30
     D OutincLt2D      s             30
     d ##kc#           s              1
     d ##kl#           s              1
     d ##Qte           s              1
     d ##Mode1         s              1
     d ##Call          s              1
     d ##View          s              1
     d strEmpName      s             40

      *** beg chg ***** 03/02/20 ****************************************
      ***** /copy sCpyClmHdI
      /include sCpyClmHdI
      *** end chg ***** 03/02/20 ****************************************
