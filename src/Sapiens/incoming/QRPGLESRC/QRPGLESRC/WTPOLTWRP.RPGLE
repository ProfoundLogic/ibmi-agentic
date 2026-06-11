     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLTWRP - Policy Toolbar Wrapper Program
     H DFTNAME(WTPOLTWRP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('WTBNDDIR':'YAJL')
     h**********************************************************************
    *                                                               *
    *  This program is called from toolbar actions and determines   *
    *  which program to call, passing along needed parameters.      *
    *                                                               *
    *===============================================================*
    *  Created   : 10/09/12                                         *
    *  Programmer: C:065                                            *
    *  Project   : WC2116                                           *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
     h*  07/01/21 C:33  Support: 96462                                *
     h*                 Implement standard employer deletion routine. *
    *                                                               *
     h*  08/23/18 C:291 Project: 133WQP                               *
     h*                 Pass an additional parameter to W1054R to be  *
     h*                 called from Quote Toolbar/Waiver Endorsemens. *
     h*                                                               *
     h*  03/05/18 C:33  Support: 64765                                *
     h*                 Check first position of @@mode                *
    *                                                               *
     h*  10/11/16 C:310 project: WC2401                               *
     h*                Add the new call for generating premium notice *
     h*                report.                                        *
     h*
     h*  01/12/15 C:283 RedMine: 4885                                 *
     h*                Add the handling for Deletion of All Divisions *
     h*                if requested.                                  *
     h*                                                               *
   h*  10/07/13 C:190 Project: 25WSS                                *
     h*                Add 'Campus Code' to toolbar selections
   h*                                                               *
   h*  08/05/13 C:136 Project: WC2182                               *
   h*                 Add option for Single Employer Renewal        *
   h*                                                               *
     h*  04/01/13 C:282 Project: 147WAK                               *
     h*                 Replaced W0050R17 by WTPOLQTFR.               *
   h*                                                               *
     h*  01/31/13 C:279 Project: 146WFJ                               *
     h*                 Added processing for 'Third Party Billing'    *
     h*                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     F                                     infds(dbfds)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     f                                     infds(dbfds1)

     D dbfds           ds
     D  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0

     D elRRN           s              9  0
     D xxRRN           s              9  0
     D xxEmRRN         s              9  0
     D intCo#          s                   like(ELCO#)
     D intFnd          s                   like(ELFND)
     D intEmp#         s                   like(ELEMP#)
     D intDiv          s                   like(ELDIV)
     D intFyr          s                   like(ELFYR)
     d***  Begin Add  ***  07/01/21  ********************************
     D ioJson          s               a   len(1000) varying
     d strErrorMsg     s            500a   varying
     d***  End   Add  ***  07/01/21  ********************************

     D xxDiv           s                   like(ELDIV)
     D xxMode          s              1    inz
     D xxNext          s             10    inz
     D xxTab           s              5    inz
     D xxQte           s              1    inz
     D xxErr           s              1    inz
     D ##Mode          s              1    inz
     D ##Qte           s              1    inz
     D ##Kc            s              1    inz
     D ##Kl            s              1    inz
     D ##Rrn           s              9  0 inz
     D ##Call          s              1    inz
      *** Beg Add *** 04/01/13 *********************************************
     d inRrn           s              9
     d inMode          s              1
     d inNext          s             10
     d inTab           s              5
     d inQte           s              1
      *** End Add *** 04/01/13 *********************************************
     D PRENOT          c                   const('            PREMIUM -
     D                                      NOTICES           ')

     d***  Begin Add  ***  07/01/21  ********************************
     d node0           s                   Like(yajl_val)
     d i0              s             10i 0 inz
     d key             s             50a   varying
     d val             s                   Like(yajl_val)

      /include yajl_h
     d***  End   Add  ***  07/01/21  ********************************

     d/copy sPrGenPop
     d***  Begin Add  ***  07/01/21  ********************************
      /copy sPrJson
     d***  End   Add  ***  07/01/21  ********************************
     d/copy wPrEmpLr

     DDriverPgm        pr                  extpgm('STDRVPGM')
     D inRrn                          9
     D inMode                         1
     D inNext                        10
     D inTab                          5
     D inQte                          1

     DW0050R17         pr                  extpgm('W0050C17')
     D outMode                        1    const
     D outCo#                         3  0 const
     D outFnd                         3  0 const
     D outEmp#                        9  0 const
     D outDiv                         5  0 const
     D outErr                         1

      *** beg add ***** 08/05/13 *************************************
     dRenewEmp         pr                  extpgm('W7810R')
     d pgmTitle                      40    const
     d pgmCall                        1    const
     d wdelpRRN                       9  0 const
      *** end add ***** 08/05/13 *************************************

      *** beg add ***** 10/07/13 *************************************
     dCampus           pr                  extpgm('W2267R')
     d inKC                           1    const
     d inKL                           1    const
     d inCALL                         1    const
     d wmempRRN                       9  0 const
      *** end add ***** 10/07/13 *************************************
      *
      * Employer Master File Key List
      *
     C     keyWMEML      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C*
     C     key1WMEML     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    xxDIV
     C                   Kfld                    ELFYR
     C*
     c* Entry of Program.
     c*
     C     *ENTRY        PLIST
     C                   PARM                    @@rrn             9            wdelp
     C                   PARM                    @@action         20            action
     C                   PARM                    @@mode          256            mode
     C                   PARM                    @@qte             1            quote
     C*
      /free

       elRRN = %int(@@rrn);

       Chain(n) elRRN PEL0P;
       If %found;
          intCo#  = ELCO#;
          intFnd  = ELFND;
          intEmp# = ELEMP#;
          intDiv  = ELDIV;
          intFyr  = ELFYR;

          Select;

          When @@action = 'Policy Page';
             Exsr PolicyPage;

          When @@action = 'Department Letters';
             Exsr DepLetters;

          When @@action = 'Premium Notice';
             Exsr PrmNotice;

          When @@action = 'Cert Holders';
             Exsr CertHolders;

          When @@action = 'Estimated Bill';
             Exsr Estimated;

          When @@action = 'Audited Bill';
             Exsr Audited;

          When @@action = 'Audit Request';
             Exsr AuditReq;

          When @@action = 'Endorsements';
             Exsr Endorsements;

          When @@action = 'Quote Transfer';
             Exsr QuoteTransfer;

          When @@action = 'Delete Employer';
             //*** Beg Chg *** 01/12/15 ******************************************
             //*********** Exsr DeleteEmp;
          //***  Begin Del  ***  07/01/21  ******************************
          //*##Mode=%Subst(@@Mode:1:1);
          //*Select;**
          //*When*##Mode = '1';
          //****##AllDiv = 'Y';
          //*Other;***
          //****##AllDiv = ' ';
          //*Endsl;***
          //***  End   Del  ***  07/01/21  ******************************

             Exsr DeleteEmp;

             //*** End Chg *** 01/12/15 ******************************************

          When @@action = 'Copy';
             Exsr EmplrCopy;

          //*** Beg Add *** 01/31/13 *********************************************
          When @@action = 'Third Party Billing';
             Exsr TPBilling;
          //*** End Add *** 01/31/13 *********************************************

          //*** beg add ***** 08/05/13 ******************************
          When @@action = 'Renew';
             Exsr Renewal;
          //*** beg add ***** 08/05/13 ******************************
          //*** beg add ***** 10/07/13 ******************************
          When @@action = 'Campus';
             Exsr CampusCode;
          //*** end add ***** 10/07/13 ******************************

          Endsl;

       Endif;

       *InLr = *On;
      /end-free
     C*
     C*===============================================================
     C* State Reporting Div - Subroutine to find state reporting division.
     C*===============================================================
     C*
     C     StateRpt      BEGSR
     C*
     C                   Z-ADD     ELDIV         xxDiv             5 0
     C*
     C     keyWMEML      Chain     PEM00                              60
     C     *in60         Doweq     *OFF
     C*
     C     EMSTRP        Ifeq      'Y'
     C                   Z-add     EMDiv         xxDiv
     C                   Leave
     C                   Endif
     C*
     C     keyWMEML      Reade     PEM00                                  60
     C                   Enddo
     C*
     C                   Z-ADD     0             xxRRN
     C*
     C                   If        *in60 = *off
     C*
     C     keyWDELL      Chain     PEL00
     C*
     C                   If        %found
     C                   eval      xxRRN = dbRRN
     C                   Endif
     C*
     C                   Endif
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* EstimatedBill - Estimated Billing
     C*===============================================================
     C     Estimated     BEGSR
     C*
     C                   eval      ##Mode=%Subst(@@Mode:1:1)
     C*
     C                   MOVEL     *BLANKS       ##QTE
     C                   If        %parms>3
     C                   IF        @@qte <> *blanks
     C                   MOVEL     'Q'           ##QTE
     C                   ENDIF
     c                   endif
     C*
     C                   CALL      'W8001R'                             99
     C                   PARM                    ##MODE
     C                   PARM                    ##QTE
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      elRRN         ##RRN
     C*
     C*
     C                   ENDSR                                                  PolicyPage
     C*===============================================================
     C* AuditedBill   - Audited Billing
     C*===============================================================
     C     Audited       BEGSR
     C*
     C                   eval      ##Mode=%Subst(@@Mode:1:1)
     C*
     C                   CALL      'W7710R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM                    ##MODE
     C                   PARM      elRRN         ##RRN
     C*
     C                   ENDSR                                                  PolicyPage
     C*===============================================================
     C* AuditReq - Audit Request
     C*===============================================================
     C     AuditReq      BEGSR
     C*
     C                   CALL      'W7230R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      ' '           ##MODE
     C                   PARM      elRRN         ##RRN
     C*
     C                   ENDSR                                                  PolicyPage
     C*
     C*===============================================================
     C* EmplrCopy - Employer Copy
     C*===============================================================
     C     EmplrCopy     BEGSR
     C*
     C                   MOVEL     *BLANKS       ##MODE
     C*
     C                   MOVEL     *BLANKS       ##QTE
     C                   If        %parms>3
     C                   IF        @@qte <> *blanks
     C                   MOVEL     'Q'           ##QTE
     C                   ENDIF
     c                   endif
     c                   eval      xxEmRRN = GetEmpRRN(intEmp# :intDiv :intFnd :
     c                             intCo#)
     C*
     C                   CALL      'W0050R03'                           99
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      xxEmRRN       ##RRN
     C                   PARM                    ##QTE
     C*
     C*
     C                   ENDSR                                                  PolicyPage
     C*===============================================================
     C* Endorsements  - Endorsements
     C*===============================================================
     C     Endorsements  BEGSR
     C*
     C***  Begin Chg  ***  03/05/18  **************************************
     C**********         If        @@Mode='D'
     C                   If        %subst(@@Mode:1:1)='D'
     C***  End   Chg  ***  03/05/18  **************************************
     C                   eval      ##Call='1'
     C                   Else
     C                   eval      ##Call=' '
     C                   EndIf

      //*** Beg Add ***** 08/23/18 *******************
      /free
         ##Qte = ' ';
         If %Parms > 3;
            If @@qte <> *blanks;
               ##Qte = 'Q';
            Endif;
         Endif;
      /End-free
      //*** End Add ***** 08/23/18 *******************
     C*
     C                   Exsr      StateRpt
     C*
     C                   If        xxRRN <> 0
     C*
     C                   CALL      'W1054R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM                    ##CALL
     C                   PARM      xxRRN         ##RRN
      **** Beg Add ***** 08/23/18 *******************
     C                   PARM                    ##QTE
      **** End Add ***** 08/23/18 *******************
     C*
     c                   Endif
     C*
     C                   ENDSR                                                  PolicyPage
     C*===============================================================
     C* PolicyPage - Subroutine to call Policy Page Print program.
     C*===============================================================
     C     PolicyPage    BEGSR
     C*
     C                   MOVE      intEmp#       qqEmp#
     C                   MOVE      *blank        qqRtc
     C*
     C                   Exsr      StateRpt
     C*
     C                   If        xxRRN <> 0
     C*
     C                   CALL      'W7412R'
     C                   PARM      ' '           QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQRTC             1
     C                   PARM      intCo#        QQCO#             3 0
     C                   PARM      intFnd        QQFND             3 0
     C                   PARM      intFyr        QQFYR             3 0
     C*
     c                   Endif
     C*
     C                   ENDSR                                                  PolicyPage
     C*===============================================================
     C*
      /Free
       //=============================================================
       // Call the Quote Transfer program
       //=============================================================
       Begsr QuoteTransfer;

       xxErr = *on;

       //*** Beg Chg *** 04/01/13 *********************************************
       //     Callp W0050R17('*':intCo#:intFnd:intEmp#:intDiv:xxErr);
       inRrn  = @@rrn;
       inMode = @@mode;
       inNext = 'WTPOLQTFR';
       inTab  = '00000';
       inQte  = 'Q';
       DriverPgm(inRrn:inMode:inNext:inTab:inQte);

       // Select;
       //   When xxErr = 'L';
             //* do nothing here, cancel was hit.
       //   When xxErr = *on;
             //* do nothing here, successful transfer.
       //   When xxErr = *off;
       //      PopOneBtn('This quote has already been flagged f+
       //      or transfer.':'Quote Transfer':'Enter');
       //   When xxErr = '2';
       //      PopOneBtn('Quote cannot be transferred at this t+
       //      ime.':'Quote Transfer':'Enter');
       //   Other;
       //      PopOneBtn('Quote transfer has returned a conditi+
       //      on of ' + xxErr + '.':'Quote Transfer':'Enter');
       // Endsl;

       //*** End Chg *** 04/01/13 *********************************************
       Endsr;
      /End-free

     C*===============================================================
     C* DepLetters  -- Subroutine for Department Letters
     C*===============================================================
     C*
     C* DEPARTMENT LETTERS
     C*
     C     DepLetters    BEGSR
     C*
     C                   MOVE      intEmp#       QQEMP#
     C                   MOVE      intDiv        QQDIV
     C*
     C                   MOVEL     *BLANKS       QQCALL
     C                   If        %parms>3
     C                   IF        @@qte <> *blanks
     C                   MOVEL     'Q'           QQCALL
     C                   ENDIF
     c                   endif
     C*
     C                   CALL      'W4115R'                             99
     C                   PARM      *BLANKS       QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV             5
     C                   PARM      'PS'          QQDEPT            2
     C                   PARM      intCo#        QQCO#
     C                   PARM      intFnd        QQFND
     C                   PARM      intFyr        QQFYR
     C*
     C                   ENDSR
     C*===============================================================
     C* premium notice  - This subroutine is to generate Prmium Notice
     C*===============================================================
     C*
     C*  Print premium notice
     C*
     C     PrmNotice     BEGSR
      *
      *** Beg-Chg *** 10/11/16 ********************************************
     C                   CALL      'W7320R'                             99
     C***                CALL      'WTRPTPRNTC'
      *** Beg-Chg *** 10/11/16 ********************************************
     C                   PARM      PRENOT        ##OPNM           40
     C                   PARM      *BLANK        ##CALL            1
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      elRRN         ##RRN             9 0
      *
     C*
     C                   ENDSR                                                  OPT32
     C*===============================================================
     C* CERTFICATE HOLDERS - Subroutine to call program for certificate Holders
     C*===============================================================
     C*
     C* CERTFICATE HOLDERS
     C*
     C     CertHolders   BEGSR
     C*
     C                   CALL      'W0810R'                             99
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##CALL
     C                   PARM                    ##RRN             9 0
     C                   PARM      elRRN         ##RRN2            9 0
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* DeleteEmp -  Edits for Employer Deletion
     C*===============================================================
     C*
     C     DeleteEmp     BEGSR

     C     key1WMEML     Chain     PEM00
     C                   If        %found(WMEML)
     C*
      //***  Begin Chg  ***  07/01/21  ******************
     C**********         Clear                   ##Qte             1
     C**********         If        @@QTE = 'Q'
     C**********         Eval      ##Qte = 'Q'
     C**********         Else
     C**********         Eval      ##Qte = 'W'
     C**********         Endif
     C**********         Call      'WTPOLDEL'
     C**********         PARM      dbrrn1        ##Rrn             9 0
     C**********         PARM                    ##Qte
     C****Beg*Add *** 01/12/15 ******************************************
     C**********         PARM                    ##AllDiv          1
     C****End*Add *** 01/12/15 ******************************************
      /Free
       ioJson = '{';
       ioJson += BuildJsonNode('rrn':%char(dbrrn1)) + ',';
       ioJson += BuildJsonNode('display':'*YES') + ',';
       If @@QTE = 'Q';
          ioJson += BuildJsonNode('quote':'Q') + ',';
       Endif;
       If %Subst(@@Mode:1:1) = '1';
          ioJson += BuildJsonNode('div':'*ALL') + ',';
       Endif;
       ioJson = %scanrpl(',':'}':ioJson:%len(ioJson)-1);
       DeleteEmployer(ioJson);
       node0 = yajl_string_load_tree(ioJson:strErrorMsg);
       Dow YAJL_object_loop(node0:i0:key:val) and strErrorMsg = *blank;
          Select;
          When key = 'ind';
          When key = 'msgId';
          When key = 'msg';
           //message = YAJL_get_string(val);
          Endsl;
       Enddo;
       yajl_tree_free(node0);
      /End-free
      //***  End   Chg  ***  07/01/21  ******************
     C*
     C                   Endif
     C*
     C                   ENDSR                                                  DeleteEmp Subr

      ****** Beg Add *** 01/31/13 *********************************************
     C*===============================================================
     C* TPBilling -  Process for Third Party Billing
     C*===============================================================
     C*
     C     TPBilling     BEGSR

     C                   Clear                   ##Qte             1
     C                   If        %parms>3
     C                   If        @@qte <> *blanks
     C                   Eval      ##Qte = 'Q'
     C                   Endif
     c                   Endif

     C                   Call      'W0093C01'                           99
     C                   Parm      '0'           ##KC
     C                   Parm      '0'           ##KL
     C                   Parm      'C'           ##MODE
     C                   Parm      elRrn         ##RRN
     C                   Parm                    ##QTE
     C*
     C                   ENDSR                                                  TPBilling Subr

     C*===============================================================
      ****** End Add *** 01/31/13 *********************************************
      *** beg add ***** 08/05/13 *************************************
      *===============================================================
      * RenewEmp - Single Employer Renewal
      *===============================================================
      /free
        begsr Renewal;
           RenewEmp(*blanks:*blank:elRRN);
        endsr;
       /end-free
      *===============================================================
      *** end add ***** 08/05/13 *************************************
      *** beg add ***** 10/07/13 *************************************
      *===============================================================
      * Campus Code
      *===============================================================
      /free
        begsr CampusCode;
           xxEmRRN = GetEmpRRN(intEmp# :intDiv :intFnd : intCo#);
           Campus(*blanks:*blanks:*blanks:xxEmRRN);
        endsr;
       /end-free
      *** end add ***** 10/07/13 *************************************
      *===============================================================
