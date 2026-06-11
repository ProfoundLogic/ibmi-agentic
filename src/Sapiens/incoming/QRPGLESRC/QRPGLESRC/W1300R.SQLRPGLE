    H*****************************************************************
    H* (c) 2024 Sapiens. All Rights Reserved.                        *
     H Bnddir('WTBNDDIR')
    H*****************************************************************
    H/TITLE W1300R - UPLOAD PAYMENT TRANSACTION INFO PROMPT
    H*****************************************************************
    H* CALLED BY: MENU                                               *
    H* CALLS TO:  W1300C - SUBMIT                                    *
    H*            W1301R - SEARCH                                    *
    H*****************************************************************
    H* Revisions:                                                    *
    H*                                                               *
    H* 02/16/24 C:340 Project: WC2954                                *
    H*                Lockbox file load from IFS.                    *
    H*                                                               *
    H* 10/02/18 C:079 Support: 99/70482                              *
    H*                Do not duplicate triggers on crtdupobj         *
    H*                                                               *
    H* 05/22/14 C:136 Project: 129WSB                                *
    H*                Retrofit CoveRisk LockBox into Base            *
    H*                                                               *
     H* 07/18/11  C:190  Project 71WLB                                *
     H*                  Change prompt screen to check for auto-cash  *
     H*                  dissolve set-up on any group and if set up,  *
     H*                  display prompt for "Dissolve lockbox cash?".
     H*                                                               *
     H* 01/28/10  C:XXX  Project: SY215                               *
     H*           C:189  Retrieve brand Name
    H* 08/20/08 C:136 Project: 114WQB                                *
    H*                Add Wachovia Upload functionality              *
    H*                                                               *
    H* 02/17/06  C:33  Project: 71WGB   WA071/51420                  *
    H*                -Implement F23=Clear                           *
    H*                -Implement F11=Invalid records                 *
    H*                 Determine if bank has sent valid data.        *
    H*                                                               *
    H* 08/02/05  C:33  Project: 71WGB                                *
    H*                -Moved to CLT071                               *
    H*                -Convert to RPGIV                              *
    H*****************************************************************
    F*
     H*DFTACTGRP(*NO)
     Fw1300f    cf   e             workstn ExtFile('W1300F')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     infds(fids01)
     Fsmcol     if   e           k disk
     Fwmupl     if   e           k disk
     F***  Begin Chg  ***  02/17/06  **************************************
     F**********wxlup     if   e             disk    usropn
     F***  Begin Chg  ***  07/18/11  **************************************
     F**********wxlup     if a e             disk    usropn
     Fwxlup     uf a e             disk    usropn
     F***  End   Chg  ***  07/18/11  **************************************
     F***  End   Chg  ***  02/17/06  **************************************
     F***  Begin Chg  ***  02/17/06  **************************************
     F**********wxlupinp  if   e             disk    usropn rename(wxlupinp:plu01inp)
     F*** beg chg ***** 08/20/08 ******************************************
     F******* wxlupinp  uf   e             disk    usropn rename(wxlupinp:plu01inp)
     Fwxlupinp  uf a e             disk    usropn rename(wxlupinp:plu01inp)
     F*** end chg ***** 08/20/08 ******************************************
     F***  End   Chg  ***  02/17/06  **************************************
     F                                            prefix(inp)
     F***  Begin Add  ***  02/17/06  **************************************
     Fwxlul3    if   e           k disk    usropn rename(plu01:plu03)
     F***  End   Add  ***  02/17/06  **************************************
     F*** beg add ***** 08/20/08 ******************************************
     fwxlwp     if   e             disk    usropn
     F*** beg add ***** 08/20/08 ******************************************
     D*
     D/copy sPrRunCmd
Np01 d/copy sPrMessage
Np01 d/copy sPrSQL
Np01 d/copy sPrGenPop
     d*/copy sCopyPr01d
     d*/copy sPrCompany
     d*/copy wPrUsrprms
     d*/copy sPrDate
     d*/copy sPrLetter
     d*/copy sPrXML
     d*/copy wPrEndorse
     d*/copy wPrAgent
     d*/copy wPrGroup
    D*
      *** beg add ***** 08/20/08 ******************************************
     *=====================================================================
      * Wachovia Upload (114WQB) Field Definitions - TOP
     *=====================================================================
     d em              s             50    dim(8) ctdata perrcd(1)

     d wachUpld        ds
     d wRcdTyp                        1    overlay(wachUpld)
     d wPostDt                        6    overlay(wachUpld : 15)
     d wCheck#                       10    overlay(wachUpld : 15)
     d wPolicy#                      20    overlay(wachUpld : 12)
     d wFnd                           3    overlay(wPolicy#)
     d wEmp#                          9    overlay(wPolicy# : 4)
     d wFyr                           3    overlay(wPolicy# : 13)
     d wDiv                           5    overlay(wPolicy# : 16)
     d wAmt                          10    overlay(wachUpld : 32)

     d svCheck#        s                   like(wCheck#)
     d svPostDt        s                   like(LUCKDT)
     d passed          s               n   inz('1')

     d getPostDt       pr
     d checkWXLUP      pr              n
     d checkWXLWP      pr              n
     d loadWXLWP       pr
     d loadWXLUPINP    pr
     *=====================================================================
      * Wachovia Upload (114WQB) Field Definitions - BOTTOM
     *=====================================================================
      *** end add ***** 08/20/08 ******************************************
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
    D*
     D lo              c                   'abcdefghijklmnopqrstuvwxyz'
     D up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D title           c                   'Upload Payment Transaction-
     D                                      Information'
     D noRcds          c                   'There are currently no-
     D                                      transactions pending upload.'
     D rcds            c                   '(F10=Search can be used to view-
     D                                      transactions pending for upload)'
     D***  Begin Add  ***  02/17/06  **************************************
     D invRcds1        c                   'There are records with invalid-
     D                                      formatting within this upload.'
     D invRcds2        c                   'Please review to determine if-
     D                                      this upload should continue.'
     D invRcds3        c                   'This upload can not continue.'
     D f23Msg          c                   'There are no records in the-
     D                                      file to be cleared.'
     D***  End   Add  ***  02/17/06  **************************************
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** fiservName   c                   'Fiserv, Inc.'
     D pdsName         S             40    Inz
     D*** END CHG ************* 01/28/10 ************************************
     D
     D
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##kc            s              1
     D ##kl            s              1
     D ##leng          s              3  0
     D @@call          s              1
     D @@co#           s              3  0
     D @@rrn           s              9  0
     D cmd             s           3000
     D***  Begin Add  ***  02/17/06  **************************************
     D dltFlag         s              1n
     D***  End   Add  ***  02/17/06  **************************************
     d***  Begin Add  ***  07/18/11  **************************************
     d autoCash        s              1n
     d noAutoCash      s              1n
     d***  End   Add  ***  07/18/11  **************************************
     D filnam          s             10
     D***  Begin Add  ***  02/17/06  **************************************
     D invalidCo#      s              1n
     D invalidRcd      s              1n
     D invalidRcds     s              1n
     D***  End   Add  ***  02/17/06  **************************************
     D libnam          s             10
     D mbrnam          s             10
     D qqcall          s              1
     D qqco#           s              3
     D qqprds          s             40
     D srdfld          s             80
     D srdlen          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D submit          s               n   inz(*on)
     D xxco#           s              3  0
     D xxrecf          s             10
     D xxcall          s                   like(qqcall)
     D xxprds          s                   like(qqprds)
     D***  Begin Add  ***  02/17/06  **************************************
     D xxType          s                   like(LUTYPE)
     D***  End   Add  ***  02/17/06  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

      ***  Begin Add  ***  02/17/06  **************************************
     DinpWXLUP         ds
     D inpCo#                         3    overlay(inpWXLUP)
     D inpChk#                        7    overlay(inpWXLUP:4)
     D inpCkAm                        9    overlay(inpWXLUP:11)
     D inpCkDt                        8    overlay(inpWXLUP:20)
     D inpScnL                       80    overlay(inpWXLUP:28)
     D inpType                        1    overlay(inpWXLUP:108)
     D  inpTypeScnL                   1    overlay(inpScnL)
     D  inpFnd                        3    overlay(inpScnL:2)
     D  inpEmp#                       9    overlay(inpScnL:5)
     D  inpDiv                        5    overlay(inpScnL:14)
     D  inpFyr                        3    overlay(inpScnL:19)
     D  inpAmt                       11    overlay(inpScnL:22)
     D  inpFyr2                       3    overlay(inpScnL:33)
     D  inpAmt2                      11    overlay(inpScnL:36)
      *** beg add ***** 08/20/08 *******************************************************
     D  wachType                      1    overlay(inpScnL:80)
      *** end add ***** 08/20/08 *******************************************************

     DInvalid3         pr             1n
     D alpha3                         3

     DInvalid5         pr             1n
     D alpha5                         5

     DInvalid7         pr             1n
     D alpha7                         7

     DInvalid8         pr             1n
     D alpha8                         8

     DInvalid9         pr             1n
     D alpha9                         9

     DInvalid11        pr             1n
     D alpha11                       11
      ***  End   Add  ***  02/17/06  **************************************
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     D***  Beg Add  ***  02/16/24  *** WC2954 ***************************
     D pathNameDta     ds                  DtaAra('W1300R')
     D  lastPathName           1    256
     D
     D RtvSuffix       Pr                  ExtPgm('SA030R')
     D   suffix                       3
     D
     d ChkDlo          pr                  extPgm('CHKIFSOBJ')
     d  docName                     256    const
     d  error                          n
     D
     d dsMsgInfo       ds                  Likeds(dsRtnMsgInfo)
     d dsInMsgInfo     ds                  Likeds(dsRtvMsgInfo)
     D
     d msg2ndLevel     s           4096a
     d charNum         s              1a
     d pattern         s             25a
     d charRecNotCopy  s              7a
     d fullPathName    s            256    Varying
     d isValidFormat   s              1n
     d length          s              3s 0
     d PopMsg          s            256
     d result          s              3s 0
     d strRunCmd       s            500
     d suffix          s              3
     d PopTitle        s             50
     D ExitNow         s              1N   Inz(*Off)
     D
     D***  Beg Add  ***  02/16/24  *** WC2954 ***************************

     D/copy sprString

    C*
    C*   KEY LIST FOR THE COMPANY FILE
     C     cokey         klist
     C                   kfld                    upco#
    C*
    C*  USER PARAMETER FILE
     C     upkey         klist
     C                   kfld                    q1user

      ***  Begin Add  ***  02/17/06  **************************************
     C     keyWXLUL3     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxType
      ***  End   Add  ***  02/17/06  **************************************
    C*
     C     *entry        plist
     C                   parm                    qqprds
     C                   parm                    qqcall
    
      *** beg add ***** 08/20/08 ***********************************************
     *
     *  Condition display screen for Wachovia Upload function (F7) for CSI
     *
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3
     c                   if         clientID = '114'
     c                   eval       *in44 = *on
     c                   endif
      *** end add ***** 08/20/08 ***********************************************
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
    C*
     C                   if        *in60 = *on
     C                   clear                   upco#
     C                   endif
    C*
     C                   time                    dsdttm
     C*
     C                   eval      scInfo = CenterFld(rcds:%size(scInfo))
     C*
     C**** Beg Add ***** 02/16/24 *** WC2954 *******************
        // Retrieve library suffix and build archive library name
        RtvSuffix(suffix);

        // Create Data Area to store folder path
        strRunCmd = 'CRTDTAARA DTAARA(PDSOBJ';
        strRunCmd = %Trim(strRunCmd) + %Trim(Suffix) + '/';
        strRunCmd = %Trim(strRunCmd) + 'W1300R)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TYPE(*CHAR) LEN(256)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TEXT(''W1300R Data Area'')';
        Monitor;
           RunCmd(strRunCmd:98);
        On-Error;
        Endmon;

        // Process until exit button is pressed.
        Dou BtnCancel = *On ;
           ExfmtScreens();
           Select;
           When BtnSubmit = *On ;
              BtnSubmit = *Off;
              ClearError();
              // Validate the screen (folder path and document name exists)
              If Validate();
                 // Update the data area with the folder path selected
                 UpdDataArea();
                 // Import the IFS file to the newly created table
                 ExitNow = *Off;
                 ImpFiletoTable();
                 If ExitNow = *On;
                    Leave;
                 EndIf;
              EndIf;
           When BtnCancel = *On;
              *InLR = *On;
              Return;
           EndSL;

        EndDo;
     C**** End Add ***** 02/16/24 *** WC2954 *******************
     C                   open      wxlup
     C                   read      wxlup
     C                   close     wxlup
     C                   if        %eof
     C                   open      wxlupinp
     C                   read      wxlupinp
     C                   close     wxlupinp
     C                   if        %eof
     C                   eval      submit = *off
     C                   eval      scInfo = CenterFld(noRcds:%size(scInfo))
     C                   else
     C                   exsr      Upload
     C                   endif
     c                   endif
    C*
    C*  GET THE COMPANY RECORD
     C     strt01        tag
     C                   move      upco#         xxco#
     C     cokey         chain     pco01
     C                   if        not %found
     C*** BEGIN CHG *********** 01/28/10 ************************************
     C***                eval      CONAME = fiservName
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval      Coname    = brandname
     C*** END CHG ************* 01/28/10 ************************************
     C                   endif
     C                   eval      scCoNm = CenterFld(CONAME:%size(scCoNm))
    C*
     C                   eval      xxprds = title
     C                   if        %parms >= 1 and qqprds <> *blanks
     C                   eval      xxprds = qqprds
     C                   endif
     C                   eval      scprds = CenterFld(xxprds:%size(scprds))
    C*
    C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
    C*
     C                   movel(p)  'SCR01'       q2recf
     C*
      ***  Begin Add  ***  02/17/06  **************************************
     C                   eval      xxType = 'X'
     C                   open      wxlul3
     C     keyWXLUL3     chain     plu03
     C                   eval      invalidRcds = %found
     C                   if        invalidRcds
     C                   eval      scInfo2 = CenterFld(invRcds1:%size(scInfo2))
     C                   eval      scInfo3 = CenterFld(invRcds2:%size(scInfo3))
     C                   endif
     C                   close     wxlul3
      ***  End   Add  ***  02/17/06  **************************************
    C*
     C***  Begin Add  ***  02/17/06  **************************************
     C                   eval      dltFlag = *off
     C***  End   Add  ***  02/17/06  **************************************
     C***  Begin Add  ***  07/18/11  **************************************
     C                   eval      autoCash= *off
     C                   eval      noAutoCash = *off

     C                   open      wxlup
     C*
     C                   dou       %eof
     C                   read      wxlup
     c                   move      *blanks       xxcnl             3
      *** beg chg ***** 05/22/14 *****************************************
     C***********        eval      xxcnl = %subst(luscnl:2:4)
      /free
       // If processing CoveRisk, move scan info right one character
       // and insert a '1' to process as a premium payment. If re-processing
       // file, don't insert another '1'
       If clientID = '129' and %subst(LUscnl:79:1) <> 'P';
          LUscnl = '1' + LUscnl;
          %subst(LUscnl:79:1) = 'P';
       Endif;
       xxCnl = %subst(LUscnl:2:3);
      /end-free
      *** end chg ***** 05/22/14 *****************************************

     C                   if        not %eof and xxcnl <> '+++'
     c                   move      xxco#         ##Co#
     c                   move      xxcnl         ##Fnd
     c                   move      'L'           ##Pgm

     c                   call      'WA083R'
     c                   parm                    ##Co#             3
     c                   parm                    ##Fnd             3
     c                   parm                    ##Pgm             1
     c                   parm                    ##RtCd            1

      *** beg chg ***** 05/22/14 *****************************************
     c***********        if        ##RtCd = '0'
     c***********        eval      noAutoCash = *on
     c***********        else
     c***********        eval      autoCash = *on
     c***********        endif                                                  ##RtCd = '0'
     c                   if        ##RtCd = '1'
     c                   eval      autoCash = *on
     c                   else
     c                   eval      noAutoCash = *on
     c                   endif                                                  ##RtCd = '0'
      *** end chg ***** 05/22/14 *****************************************

     c                   move      *blanks       xxcnl1            1
     C                   eval      xxcnl1 = %subst(luscnl:80:1)
     c                   if        ##RtCd = '1' and xxcnl1 <> 'W'
     c                   move      'L'           luscnl
     c                   update    plu01
     c                   endif

     c                   endif

     c                   enddo
     c                   close     wxlup

      *** beg chg ***** 05/22/14 *****************************************
     c***********        if        autoCash = *on
     c***********        eval      *in45   = *on
     c***********        if        Noautocash = *on
     c***********        eval      *in47   = *on
     c***********        endif                                                  NoautoCash = *on
     c***********        endif                                                  autoCash = *on
     c                   if        autoCash = *on
     c                   eval      *in45   = *on
     c                   endif                                                  NoautoCash = *on
     c                   if        noAutoCash = *on
     c                   eval      *in47   = *on
     c                   endif                                                  autoCash = *on
      *** end chg ***** 05/22/14 *****************************************
     C***  end   Add  ***  07/18/11  **************************************
    C*
    C*   DISPLAY ALL FORMATS
    C*
     C     strt          tag
    C*
     C***  Begin Add  ***  02/17/06  **************************************
     C                   if        dltFlag and not *inkx
     C                   eval      dltFlag = *off
     C                   endif
     C***  End   Add  ***  02/17/06  **************************************
    C*
     C                   if        q2recf = 'SCR01'
     C                   exfmt     scr01
     C                   endif
    C*
    C*   PROCESS ALL COMMAND KEYS
    C*
     C                   if        *in11 = *on
    C*
    C*   COMMAND 03 = END OF JOB
    C*
     C                   if        *inkc = *on
     C                   move      *on           *inlr
     C                   goto      term
     C                   endif

      *** beg add ***** 08/20/08 *******************************************************
      *
      * F7=Wachovia Upload
      *
     c                   if        *inkg = *on
     c                   clear                   errwdw
     c                   eval      *in70 = checkWXLWP
     c                   if        *in70
     c                   eval      wwmsg1 = em(1)
     c                   eval      wwmsg2 = em(2)
     c                   eval      wwinst = em(3)
     c                   exfmt     errwdw
     C                   eval      q2recf = 'SCR01'
     c                   goto      strt
     c                   endif
     c                   eval      *in71 = checkWXLUP
     c                   dow       not *in71
     c                   eval      wwmsg1 = em(4)
     c                   eval      wwmsg2 = em(5)
     c                   eval      wwmsg3 = em(6)
     c                   eval      wwmsg4 = em(7)
     c                   eval      wwinst = em(8)
     c                   exfmt     errwdw
     c                   select
     c                   when      *inkh
     c                   eval      *in71 = *on
     C                   eval      cmd = 'CLRPFM FILE(WXLUP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C                   eval      cmd = 'CLRPFM FILE(WXLUPINP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     c                   when      *inkl
     c                   eval      q2recf = 'SCR01'
     c                   goto      strt
     c                   endsl
     c                   enddo
     c                   callp     loadWXLWP
     c                   exsr      Upload
     c                   eval      submit = not(invalidRcd)
     c                   endif
      *** end add ***** 08/20/08 *******************************************************
    C*
    C*   F10 = SEARCH
    C*
     C                   if        *inkj = *on
    C*
     C                   call      'W1301R'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      ' '           @@call
     C                   parm      0             @@rrn
     C                   parm      xxco#         @@co#
    C*
     C                   if        *inkc = *on
     C                   goto      term
     C                   endif
    C*
     C                   goto      strt
    C*
     C                   endif
    C*
     C***  Begin Add  ***  02/17/06  **************************************
     C                   if        *inkk
     C                   exsr      kkDo
     C                   goto      strt
     C                   endif
    C*
     C                   if        *inkx
     C                   exsr      kxDo
     C                   goto      strt
     C                   endif
     C***  End   Add  ***  02/17/06  **************************************
    C*
    C*   PROCESS HELP REQUESTS
    C*
     C                   if        *in10 = *on
     C                   move      q2recf        xxrecf
     C                   movel     'QHLP    '    filnam
     C                   movel     '*LIBL   '    libnam
     C                   move      q1pgm         mbrnam
     C                   move      q2recf        mbrnam
     C                   call      'M0006C'
     C                   parm                    filnam
     C                   parm                    libnam
     C                   parm                    mbrnam
     C                   move      xxrecf        q2recf
     C                   goto      strt
     C                   endif
    C*
    C*   PROCESS CHANGE OF USER PARAMETERS
    C*
     C                   if        *inkm = *on
     C                   call      'W0006R'
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   clear                   upco#
     C                   endif
     C                   move      upco#         xxco#
     C                   goto      strt01
     C                   endif
    C*
     C                   endif
    C*
    C*   PROCESS REQUEST SCREEN
    C*
     C     scr01t        tag
     C***  beg   Add  ***  07/18/11  **************************************
     c                   if        sclbca <> 'Y' and sclbca <> ' '
     C                   seton                                        46  90
     c                   goto      strt
     c                   endif
     C***  end   Add  ***  07/18/11  **************************************
    C*
    C*   CALL THE CLP TO SUBMIT THE LIST
    C*
     C                   move      upco#         qqco#
    C*
     C                   if        submit
    C*
     C                   call      'W1300C'
     C                   parm                    qqco#
     C***  Begin Add  ***  07/18/11  **************************************
     C                   parm      sclbca        qqlbca            1
     C***  End   Add  ***  07/18/11  **************************************
    C*
     C                   endif
    C*
    C*  END THE PROGRAM
    C*
     C     term          tag
     C                   move      *on           *inlr
    C*
     C***  Begin Add  ***  02/17/06  **************************************
     C*====================================================================
     C* Process F11=Invalid records
     C*====================================================================
     C     kkDo          begsr
    C*
     C                   call      'W1301R'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      'I'           @@call
     C                   parm      0             @@rrn
     C                   parm      xxco#         @@co#
    C*
     C                   if        *inkc = *on
     C                   goto      term
     C                   endif
    C*
     C                   endsr
     C*
     C*====================================================================
     C* Process F23=Clear file
     C*====================================================================
     C     kxDo          begsr
     C*
     C                   select
     C                   when      not submit
     C                   eval      scInfo2 = CenterFld(f23Msg:%size(scInfo2))
     C                   when      not dltFlag
     C                   eval      *in23   = *on
     C                   eval      dltFlag = *on
     C                   when      dltFlag
     C                   eval      cmd = 'CLRPFM FILE(WXLUP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C                   eval      cmd = 'CLRPFM FILE(WXLUPINP)'
     C                   callp     RunCmd(cmd:%len(cmd))
      *** beg add ***** 08/20/08 ***********************************************
     C                   eval      cmd = 'CLRPFM FILE(WXLWP)'
     C                   callp     RunCmd(cmd:%len(cmd))
      *** end add ***** 08/20/08 ***********************************************
     C                   eval      submit = *off
     C                   eval      scInfo = CenterFld(noRcds:%size(scInfo))
     C                   eval      scInfo2 = *blanks
     C                   eval      scInfo3 = *blanks
     C                   endsl
     C*
     C                   endsr
     C***  End   Add  ***  02/17/06  **************************************
     C*
     C*====================================================================
     C* Copy records from the Input file
     C*====================================================================
     C     Upload        begsr
     C*
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********         eval      cmd = 'CPYF FROMFILE(WXLUPINP) +
     C**********                    TOFILE(WXLUP) MBROPT(*REPLACE) +
     C**********                    FMTOPT(*NOCHK)'
     C**********         callp     RunCmd(cmd:%len(cmd))
     C                   open      wxlupinp
     C                   open      wxlup
     C*
     C                   dou       %eof
     C*
     C                   read      wxlupinp
     C*
     C                   if        not %eof
     C*
     C                   eval      invalidRcd = *off
     C*
     C                   if        Invalid3(inpCo#)
     C                   eval      invalidCo# = *on
     C                   leave
     C                   endif
     C*
     C                   if        Invalid7(inpChk#)
     C                   eval      invalidRcd = *on
     C                   eval      inpChk# = *zeros
     C                   endif
     C*
     C                   if        Invalid9(inpCkAm)
     C                   eval      invalidRcd = *on
     C                   eval      inpCkAm = *zeros
     C                   endif
     C*
     C                   if        Invalid8(inpCkDt)
     C                   eval      invalidRcd = *on
     C                   eval      inpCkDt = *zeros
     C                   endif
     C*
     C                   if        Invalid3(inpFnd)
     C                   eval      invalidRcd = *on
     C                   eval      inpFnd = *all'+'
     C                   endif
     C*
     C                   if        Invalid9(inpEmp#)
     C                   eval      invalidRcd = *on
     C                   eval      inpEmp# = *all'+'
     C                   endif
     C*
     C                   if        Invalid5(inpDiv)
     C                   eval      invalidRcd = *on
     C                   eval      inpDiv = *all'+'
     C                   endif
     C*
     C                   if        Invalid3(inpFyr)
     C                   eval      invalidRcd = *on
     C                   eval      inpFyr = *all'+'
     C                   endif
     C*
     C                   if        Invalid11(inpAmt)
     C                   eval      invalidRcd = *on
     C                   eval      inpAmt = *all'+'
     C                   endif
     C*
     C                   if        Invalid3(inpFyr2)
     C                   eval      invalidRcd = *on
     C                   eval      inpFyr2 = *all'+'
     C                   endif
     C*
     C                   if        Invalid11(inpAmt2)
     C                   eval      invalidRcd = *on
     C                   eval      inpAmt2 = *all'+'
     C                   endif
     C*
     C                   if        invalidRcd
     C                   eval      invalidRcds = *on
     C                   eval      inpType = 'X'
     C                   endif
     C*
     C                   move      inpCo#        LUCO#
     C                   move      inpCo#        LUCO#
     C                   move      inpChk#       LUCHK#
     C                   move      inpCkAm       LUCkAm
     C                   move      inpCkDt       LUCKDT
     C                   eval      LUSCNL = inpScnL
     C                   eval      LUTYPE = inpType
     C*
     C                   write     plu01
     C*
     C                   endif
     C*
     C                   enddo
     C*
     C                   close     wxlupinp
     C                   close     wxlup
     C*
     C                   select
     C                   when      invalidCo#
     C                   eval      submit = *off
     C                   eval      scInfo  = *blanks
     C                   eval      scInfo2 = CenterFld(invRcds1:%size(scInfo2))
     C                   eval      scInfo3 = CenterFld(invRcds3:%size(scInfo3))
     C                   eval      cmd = 'CLRPFM FILE(WXLUP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C*** beg add ***** 08/20/08 ******************************************
     C                   eval      cmd = 'CLRPFM FILE(WXLWP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C*** end add ***** 08/20/08 ******************************************
     C*
     C                   when      invalidRcds
     C*
     C                   eval      cmd = 'DLTF FILE(WXLUPINV)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C*
/*** C*** Beg Chg *** 10/02/18 ********************************
     C***************    eval      cmd = 'CRTDUPOBJ OBJ(WXLUPINP) +
     C***************               FROMLIB(*LIBL) OBJTYPE(*FILE) +
     C***************               NEWOBJ(WXLUPINV) DATA(*YES)'
     C                   eval      cmd = 'CRTDUPOBJ OBJ(WXLUPINP) +
     C                              FROMLIB(*LIBL) OBJTYPE(*FILE) +
     C                              NEWOBJ(WXLUPINV) DATA(*YES) TRG(*NO)'
/*** C*** End Chg *** 10/02/18 ********************************
     C                   callp     RunCmd(cmd:%len(cmd))
     C***  End   Chg  ***  02/17/06  **************************************
     C*
     C                   eval      cmd = 'CLRPFM FILE(WXLUPINP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C*
     C***  Begin Add  ***  02/17/06  **************************************
     C*** beg add ***** 08/20/08 ******************************************
     C                   eval      cmd = 'CLRPFM FILE(WXLWP)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C*** end add ***** 08/20/08 ******************************************
     C                   endsl
     C***  End   Add  ***  02/17/06  **************************************
     C*
     C                   endsr





      ***  Begin Add  ***  02/17/06  **************************************
     PInvalid3         b
     DInvalid3         pi             1n
     D alpha3                         3
     C                   testn                   alpha3               60
     C                   return    not *in60
     PInvalid3         e



     PInvalid5         b
     DInvalid5         pi             1n
     D alpha5                         5
     C                   testn                   alpha5               60
     C                   return    not *in60
     PInvalid5         e



     PInvalid7         b
     DInvalid7         pi             1n
     D alpha7                         7
     C                   testn                   alpha7               60
     C                   return    not *in60
     PInvalid7         e



     PInvalid8         b
     DInvalid8         pi             1n
     D alpha8                         8
     C                   testn                   alpha8               60
     C                   return    not *in60
     PInvalid8         e



     PInvalid9         b
     DInvalid9         pi             1n
     D alpha9                         9
     C                   testn                   alpha9               60
     C                   return    not *in60
     PInvalid9         e



     PInvalid11        b
     DInvalid11        pi             1n
     D alpha11                       11
     C                   testn                   alpha11              60
     C                   return    not *in60
     PInvalid11        e
      ***  End   Add  ***  02/17/06  **************************************
      *** beg add ***** 08/20/08 ***********************************************
      *=========================================================================
      * checkWXLUP - return *off if WXLUP is populated, *on if not
      *=========================================================================
     p checkWXLUP      b
     d checkWXLUP      pi              n
     c                   open      WXLUP
     C     1             setll     WXLUP
     c                   read      WXLUP
     c                   close     WXLUP
     c                   return    %eof
     p checkWXLUP      e
      *=========================================================================
      * checkWXLWP - return *off if WXLWP is populated, *on if not
      *=========================================================================
     p checkWXLWP      b
     d checkWXLWP      pi              n
     c                   open      WXLWP
     c                   read      WXLWP
     c                   close     WXLWP
     c                   return    %eof
     p checkWXLWP      e
      *=========================================================================
      * loadWXLWP - load WXLWP to WXLUP
      *=========================================================================
     p loadWXLWP       b
      *
      * Open up files and clear the Lockbox Alpha Flat file. WXLWP is loaded to
      * this file as an intermediate step.
      *
     c                   open      WXLUPINP
     c     1             setll     WXLUPINP
     c                   read      WXLUPINP
     c                   dow       not(%eof)
     c                   delete    WXLUPINP
     c                   read      WXLUPINP
     c                   enddo
      *
      * Load WXLWP to WXLUPINP
      *
     c                   open      WXLWP
     c                   read      WXLWP
     c                   dow       not(%eof)
     c                   clear                   wachUpld
     c                   movel     data          wachUpld
     c                   select
     c                   when      wRcdTyp = '4'
     c                   callp     loadWXLUPINP
     c                   when      wRcdTyp = '5'
     c                   callp     getPostDt
     c                   when      wRcdTyp = '6'
     c                   move      wCheck#       svCheck#
     c                   when      wRcdTyp = '7'
     c                   leave
     c                   endsl
     c                   read      WXLWP
     c                   enddo
     c                   close     WXLWP
     c                   close     WXLUPINP

     p loadWXLWP       e
      *=========================================================================
      * loadWXLUPINP - write Wachovia Upload Records to WXLUPINP
      *=========================================================================
     p loadWXLUPINP    b
     c                   move      *all'0'       inpWXLUP
     c                   movel     UPCO#         inpCo#
     c                   move      svCheck#      inpChk#
     c                   move      wAmt          inpCkAm
     c                   move      '1'           inpTypeScnL
     c                   movel     svPostDt      inpCkDt
     c                   move      wFnd          inpFnd
     c                   move      wEmp#         inpEmp#
     c                   move      wDiv          inpDiv
     c                   move      wFyr          inpFyr
     c                   move      *all'0'       inpAmt
     c                   move      wAmt          inpAmt
     c                   move      'W'           wachType
     c                   write     PLU01INP
     p loadWXLUPINP    e
      *=========================================================================
      * getPostDt - edit and convert wPostDt to CYMD date, return 0 if error
      *=========================================================================
     p getPostDt       b
     d date            s               d
     c                   monitor
     c                   eval      date = %date(wPostDt : *YMD0)
     c                   on-error
     c                   eval      svPostDt = 0
     c                   return
     c                   endmon
     c     *iso          move      date          svPostDt
     p getPostDt       e
      *** end add ***** 08/20/08 ***********************************************
      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
     d docExists       s               n
     d Lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     d root            s             10
     d rootPlus        s             20
     d sFldPos         s              2  0
     d suffix          s              3
     d Up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d x               s              3  0 inz(1)
     d y               s              3  0
     d intCharPos      s              3  0

       // Must start with a '/'
       If %subst(scPathName:1:1) <> '/';
          errPathNam = *On;
          msgPathNam = 'The import file path name must start with a ''/''.';
          Return *Off;
       Endif;

       fullPathName = %Xlate(lo:up:scPathName);
       // Must be a .TXT file
       length = %Len(%Trim(fullPathName));
       If %Subst(fullPathName:length-3:4) <> '.TXT';
          errPathNam = *On;
          msgPathNam = 'The file extension must be .TXT.';
          Return *Off;
       Endif;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          errPathNam = *On;
          msgPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       Return *On;
      /End-Free

     P Validate        e
      //===================================================================
      // ClearError - Clear Error/Message fields
      //===================================================================
     p ClearError      b
     d ClearError      pi

      /Free
        errPathNam = *off;
      /End-Free

     p ClearError      e
      //===================================================================
      //  UpdDataArea - Update Data Area with Path Name selected
      //===================================================================
     p UpdDataArea     b
     d UpdDataArea     pi

      /Free
        strRunCmd = 'CHGDTAARA DTAARA(PDSOBJ';
        strRunCmd = %Trim(strRunCmd) + %Trim(Suffix) + '/';
        strRunCmd = %Trim(strRunCmd) + 'W1300R)';
        strRunCmd = %Trim(strRunCmd) + ' ' + '(1 256))';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'VALUE(''';
        strRunCmd = %Trim(strRunCmd) + %Trim(scPathName) + '''' + ')';
        Monitor;
           RunCmd(strRunCmd:500);
        On-Error;
        Endmon;
       In *Lock pathNameDta;
       lastPathName = scPathName;
       Out pathNameDta;

     p UpdDataArea     e
      //===================================================================
     *// ImpFiletoTable - Import the Rating Factors File to table FACTORS
      //===================================================================
     p ImpFileToTable  b
     d ImpFileToTable  pi

      /Free
        isValidFormat = *on;
        strRunCmd = 'CPYFRMIMPF FROMSTMF(';
        strRunCmd = %Trim(strRunCmd) + '''' + %Trim(scPathName) + '''' + ')';
        strRunCmd = %Trim(strRunCmd) + ' TOFILE(*LIBL/WXLUPINP)';
        strRunCmd = %Trim(strRunCmd) + ' MBROPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RCDDLM(*CRLF)';
        strRunCmd = %Trim(strRunCmd) + ' FLDDLM(*TAB)';
        strRunCmd = %Trim(strRunCmd) + ' ERRRCDOPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RPLNULLVAL(*FLDDFT)';
        Monitor;
           RunCmd(strRunCmd:500);
           ExitNow = *On;
        On-Error;
           // Get the message indicating how many records were not uploaded
           // If more than 2 inform the user the data was not formatted properly
           dsInMsgInfo = dsRtvMsgInfo;
           dsInMsgInfo.type = '*LAST';
           dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           dsInMsgInfo.type = '*PRV';
           Dow dsMsgInfo.MsgID <> *blanks;
              If dsMsgInfo.MsgID = 'CPF2817';
                 Clear msg2ndLevel;
                 PopTitle = 'File System Error';
                 PopMsg = 'Please check the text file and' +
                  ' Submit again.';
                 PopOneBtn(PopMsg:PopTitle);
                 isValidformat = *off;
                 Leave;
              Endif;
              If dsMsgInfo.MsgID = 'CPF2845';
                 Clear msg2ndLevel;
                 Exec Sql
                 select MESSAGE_SECOND_LEVEL_TEXT into :msg2ndLevel
                     from table(qsys2.joblog_info('*')) A
                     where ORDINAL_POSITION = :dsmsginfo.pos;
                 pattern = 'reason code 6';
                 result = %scan(%trim(pattern):%trim(dsMsgInfo.MsgText));
                 If result > 0;
                    PopTitle = 'File Lock';
                    PopMsg = 'Please close File WXLUPINP - Lockbox Upload' +
                     ' and Submit again.';
                    PopOneBtn(PopMsg:PopTitle);
                    isValidformat = *off;
                    Leave;
                 Endif;
              Endif;
              If dsMsgInfo.MsgID = 'CPFA0D4';
                 Clear msg2ndLevel;
                 Exec Sql
                 select MESSAGE_SECOND_LEVEL_TEXT into :msg2ndLevel
                     from table(qsys2.joblog_info('*')) A
                     where ORDINAL_POSITION = :dsmsginfo.pos;
                 pattern = 'Error number 3029';
                 result = %scan(%trim(pattern):%trim(dsMsgInfo.MsgText));
                 If result > 0;
                    PopTitle = 'File Lock';
                    PopMsg = 'Please close the text file and' +
                     ' Submit again.';
                    PopOneBtn(PopMsg:PopTitle);
                    isValidformat = *off;
                    Leave;
                 Endif;
              Endif;
              dsInMsgInfo.pos  = dsMsgInfo.pos;
              dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           Enddo;
        Endmon;
      /End-Free

     p ImpFileToTable  e
      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     p ExfmtScreens    b
     d ExfmtScreens    pi

        Write RcdPathNm;
        If scPathName = *blanks;
           In pathNameDta;
           scPathName = lastPathName;
        Endif;
        Exfmt RcdPathNm;

     p ExfmtScreens    e
      //===================================================================
** Error Messages (EM)
The Wachovia Upload file is empty. Populate it
before attempting to load.
Press Enter to continue.
The Lockbox Upload file is currently populated.
Press F8 to overwrite the current Lockbox file
and continue loading the Wachovia file or
F12 to cancel
F8=Continue  F12=Cancel
