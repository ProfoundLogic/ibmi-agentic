     H Copyright('(c) 2021 Sapiens. All Rights Reserved.')
     H Option(*Nodebugio:*Srcstmt)
     H Dftactgrp(*no) Actgrp(*CALLER) Bnddir('WTBNDDIR')
     H/TITLE SR42304BI - Additional Entry - Texas Endorsements WC420304B
      *----------------------------------------------------------------- *
      *                                                                   *
      *  Called From: SR42304B                                            *
      *                                                                   *
      *----------------------------------------------------------------- *
      *                                                                   *
      * Created   : 04/24/14                                              *
      * Programmer: C: 282                                                *
      * Project   : WC2270                                                *
      *                                                                   *
      *----------------------------------------------------------------- *
      *Revisions:                                                        *
      *                                                                   *
      * 02/02/21 C:136 Project: WC2757                                    *
      *                Default blanket waiver verbiage.                   *
      *                Maintain only one WXFMP1 record per policy year.   *
      *                                                                   *
      * 07/22/19 C:136 Support: 81117                                     *
      *                Remove condition that only calls W/W Waivers       *
      *                for a specific type waiver                         *
      *                                                                   *
      *----------------------------------------------------------------- *
     FSR42304BF cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      **Additional Information File
      *** beg chg ***** 02/02/21 **************************
     F***WXFMP1    uf a e           k disk
     FWXFML12   uf a e           k disk
      *** end chg ***** 02/02/21 **************************
      **Group Master File
     FWMFNL     if   e           k disk
      **Employer Master File
     FWMEML     if   e           k disk
      **Employer Detail File
     FWDELL1    if   e           k disk
     F                                     infds(elfdb)
      *----------------------------------------------------------------- *
      */COPY 'D' specs
      *----------------------------------------------------------------- *
     D/COPY SCOPYPR01D
      *** beg add ***** 02/02/21 **************************
      /copy wPrEndorse
      *** end add ***** 02/02/21 RNF7030*******************
      *----------------------------------------------------------------- *
      *Stand Alone Fields
      *----------------------------------------------------------------- *
     D optnm           c                   const('Additional information for WC+
     D                                     420304B')
     D comapi          c                   const('WA001R')

     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3

     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0

      *** beg add ***** 02/02/21 **************************
     d @@WvrType       s              1
     d chrAddPgm       s             10
     d chrPrtPgm       s             10
     d chrState        s              2    Inz('TX')
     d chrWvBlkPrjDs   s            500
     d chrWvrForm      s             15
     d chrWvrMode      s              1
     d chrWvrType      s              1
     d intRtnDiv       s              5  0
     d isMidTerm       s               n
     d qqKC            s              1
     d qqKL            s              1
     d qqCall          s              1
     d qqRRN           s              9  0
     d qqRRN2          s              9  0
     d qqWvrMode       s              1

     d W0815R          pr                  ExtPgm('W0815R')
     d  ##kc                          1
     d  ##kl                          1
     d  ##call                        1
     d  ##rrn                         9  0
     d  ##rrn2                        9  0
     d  ##CPgm                       10
     d  ##WTyp                        1    Options(*Nopass)

     d svlda           ds          1024    dtaara(*lda)
     d  ldEnd#                 1      5  0
     d  ldEnda                 1      5
     d  ldMyrr                 6     14  0
     d  ldEeDt                15     22  0
     d  ldFyr                 23     25  0
     d  ldPyrl                26     26
     d  ldWvrMode             32     32
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55
      *** end add ***** 02/02/21 **************************

     D xxfrm1          s             15
     D xxcall          s                   like(@@call)
     D xxForm          s             10    inz('WC420304B ')
     D xxco#           s                   like(emco#)
     D xxfnd           s                   like(emfnd)
     D xxemp#          s                   like(ememp#)
     D xxdiv           s                   like(emdiv)
     D xxfyr           s              3  0
     D xxedn           s             80
     D elfdb           ds
     D elrrn                 397    400b 0
    
     C/COPY SCOPYDTPL
    
      *----------------------------------------------------------------- *
      * PLIST definitions
      *----------------------------------------------------------------- *
      **Entry parameters
     C     *entry        Plist
     C                   Parm                    @@prds
     C                   Parm                    @@call
     C                   Parm                    @@co#
     C                   Parm                    @@fnd
     C                   Parm                    @@emp#
     C                   Parm                    @@div
     C                   Parm                    @@fyr

      **Company file/fund (group) retrieve program parameter list
     C     compl         Plist
     C                   Parm                    ##co#
     C                   Parm                    ##fnd
     C                   Parm                    ##conm
     C                   Parm                    ##fnam

      *-------------------------------------------------------------- *
      * KLIST definitions
      *-------------------------------------------------------------- *

      **Employer Master file
     C     emky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv

      **Additional Information file
     C     fmky1         Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
      *** beg chg ***** 02/02/21 **************************
     C***                   Kfld                    xxfyr
     C***                   Kfld                    xxemp#
     C***                   Kfld                    xxdiv
     C                   Kfld                    xxemp#
     C                   Kfld                    xxfyr
      *** end chg ***** 02/02/21 **************************
     C                   Kfld                    xxfrm1
    
      **Group Master file
     C     fnky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd

      **Employer Detail file
     C     elky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv

      *-------------------------------------------------------------- *
      *Main
      *-------------------------------------------------------------- *
     C/COPY SCOPYPR01C
      *-------------------------------------------------------------- *
      *DSCAPI -Call API for getting descriptions (use 91 on error)
      *-------------------------------------------------------------- *
     C     DSCAPI        Begsr
     C                   Call      comapi        compl                  91
     C                   Endsr
      *----------------------------------------------------------------- *
      *ERRCK1 -Error checking for srceen 1 (SCR01)
      *----------------------------------------------------------------- *

     C     ERRCK1        Begsr
    
      *  Specific/Blanket Waiver
      //*** beg chg ***** 02/02/21 *************************************
     C***                if        (scSBWV <> 'S' And  scSBWV <> 'B' And
     C***                           scSBWV <> ' ')
     C***                   seton                                        7190
     C***                   leavesr
     C***                   endif
       If (scSBWV <> 'S' And  scSBWV <> 'B');
          *In71 = *On;
          *In90 = *On;
       Endif;
      //*** end chg ***** 02/02/21 *************************************

     C                   Endsr
      *----------------------------------------------------------------- *
      *KJDO -Processing for F10
      *----------------------------------------------------------------- *
     C     KJDO          Begsr

     C                   Endsr
      *----------------------------------------------------------------- *
      *OTHFKS -Processing for other functions keys
      *----------------------------------------------------------------- *
     C     OTHFKS        Begsr
    
     C     *inkj         Caseq     *on           kjdo                     90
     C                   Endcs
    
     C                   Endsr
      *----------------------------------------------------------------- *
      *PGMEND -End of program processing
      *----------------------------------------------------------------- *
     C     PGMEND        Begsr

     C                   If        *inkc = *on OR
     C                             *inkl = *on
     C                   Eval      @@call = 'F'
     C                   Endif

     C                   Endsr
      *----------------------------------------------------------------- *
      *PGMSTR -Used for any start of program processing (one-time)
      *----------------------------------------------------------------- *

     C     PGMSTR        Begsr
    
     C                   If        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   Movel(p)  @@prds        ##data
     C                   Else
     C                   Movel(p)  optnm         ##data
     C                   Endif
    
     C                   Call      'SRCENTER'
     C                   Parm                    ##data
     C                   Parm      40            ##leng
     C                   MOVEL     ##DATA        SCPRDS
    
     C                   If        q1prms >= 2
     C                   Movel     @@call        xxcall
     C                   Else
     C                   Clear                   xxcall
     C                   Endif
    
     C                   Eval      xxfyr = 0
     C                   Move      @@co#         xxco#
     C                   Move      @@fnd         xxfnd
     C                   Move      @@emp#        xxemp#
     C                   Move      @@div         xxdiv
     C                   Move      @@fyr         xxfyr

      *** beg add ***** 02/02/21 **************************
       // Look at LDA for ldWvrMode and ldWvrForm.
       //  Populate chrWvrType with @@CALL and clear @@Call
       //   for midterm endorsement.
       In svLDA;
       Out svLDA;
       Select;
       When ldWvrMode <> *Blank;
          isMidTerm = *Off;
          @@Call = *Blank;
          chrWvrMode = ldWvrMode;
          If ldWvrMode = '1';
             *In09 = *On;
          Endif;
       Other;
          isMidterm = *On;
          chrWvrMode = @@Call;
          @@call = *Blank;
       Endsl;
       Chain elky pel01;
       scSBWv = ELwvtp;
       // Default operations vbg from GetWaiverInfo for
       //  blanket waivers
       chrWvrForm = xxForm;
       If GetWaiverInfo(ELco#:ELfnd:chrState:ELpfdt:chrWvrForm:
        chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:ELfyr:ELemp#) = *On;
          chrWvrType = %Subst(chrWvBlkPrjDs:500:1);
          If chrWvrType = 'B';
             scWvrDesc = %Subst(chrWvBlkPrjDs:1:499);
          Endif;
       Endif;
       Select;
       When isMidTerm And chrWvrMode = 'D' And chrWvrType = 'B' ;
      // Midterm Delete
          qqRRN2 = elRRN;
          W0815R(qqKC:qqKL:qqCall:qqRRN:qqRRN2:Q1pgm:chrWvrMode);
          Return;
       When Not(isMidTerm);
          Select;
          When ldWvrMode = 'A';
      // Called from W/W Employer Endorsements (W1054R02)
      //  Need to get a TX division to process waiver for.
             If GetDivForWaiverType(ELco#:ELfnd:ELfyr:ELemp#:chrWvrForm:
              chrState:chrWvrType:intRtnDiv) = *On;
                If intRtnDiv <> xxDiv;
                   xxDiv = intRtnDiv;
                   Chain elky pel01;
                   scSBWv = ELwvtp;
                Endif;
             Endif;
          Other;

          Endsl;
       Endsl;
      *** end add ***** 02/02/21 **************************

     C                   Eval      scfnd = xxfnd
     C                   Eval      scfyr = xxfyr
     C                   Eval      xxfrm1 = xxform

     C     fnky          Chain     pfn01                              60
     C                   Eval      scfnnm = fnname

     C     emky          Chain     pem01                              60

     C                   Call      'WA005R'
     C                   Parm                    ememp#
     C                   Parm                    emdiv
     C                   Parm                    emname
     C                   Parm                    xxedn

     C                   Eval      scedn = xxedn

     C                   Endsr
      *----------------------------------------------------------------- *
      *PREDSP -Pre-processing before displaying (every time)
      *----------------------------------------------------------------- *

     C     PREDSP        Begsr

      **Get endorsement additional information
      *** beg chg ***** 02/02/21 ****************
     C     fmky1         Chain     pfm01                              60
     C     fmky1         Chain(n)  pfm01                              60
      *** end chg ***** 02/02/21 ****************

     C                   If        *in60 = *off

      **Move the custom fields to the screen
     C                   Eval      scsbwv = f3fl03
     C                   Eval      scopn1 = f3c401
     C                   Eval      scopn2 = f3c402
     C                   Eval      scprem = f3a921
     C                   Eval      scppre = f3a521
     C                   Eval      scapre = f3a923
      *** beg add ***** 02/02/21 **************************
      // Migrate from old operation fields to scWvrDesc
       If F3500a1 <> *Blanks;
          scWvrDesc = F3500a1;
       Else;
          scWvrDesc = F3c401 + F3c402;
       Endif;
      *** end add ***** 02/02/21 **************************

     C                   Else
      *** beg chg ***** 02/02/21 **************************
     C***                   Eval      scsbwv = *blanks
       scSBWv = ELwvtp;
      *** end chg ***** 02/02/21 **************************
     C                   Eval      scopn1 = *blanks
     C                   Eval      scopn2 = *blanks
     C                   Eval      scprem = *zeros
     C                   Eval      scppre = *zeros
     C                   Eval      scapre = *zeros

     C                   Endif

     C                   Endsr
      *----------------------------------------------------------------- *
      *PRSCTL -Process control after no errors found (Call Submitter)
      *----------------------------------------------------------------- *
     C     PRSCTL        Begsr

     C     fmky1         Chain     pfm01                              60

     C                   If        *in60 = *on
     C                   Clear                   pfm01
     C                   Endif

      **Move the additional information fields to the file
     C                   Eval      f3co# = xxco#
     C                   Eval      f3fnd = xxfnd
     C                   Eval      f3fyr = xxfyr
     C                   Eval      f3emp# = xxemp#
     C                   Eval      f3div = xxdiv
     C                   Eval      f3form = xxform
     C                   Eval      f3fl03 = scsbwv
      *** beg chg**** 02/02/21 ********************
      // Migrate from old operation fields
     C***                   Eval      f3c401 = scopn1
     C***                   Eval      f3c402 = scopn2
       F3c401 = *Blanks;
       F3C402 = *Blanks;
       F3500A1 = scWvrDesc;
      *** end chg ***** 02/02/21 ********************
     C                   Eval      f3a921 = scprem
     C                   Eval      f3a521 = scppre
     C                   Eval      f3a923 = scapre

     C   60              Write     pfm01
     C  N60              Update    pfm01

      *** beg chg ***** 02/02/21 **************************************
     C***     elky          Chain     pel01                              60
     C***                   Call      'W0815R'
     C***                   Parm      *off          qqkc              1
     C***                   Parm      *off          qqkl              1
     C***                   Parm      ' '           qqcall            1
     C***                   Parm      0             qqrrn             9 0
     C***                   Parm      elrrn         qqrrn2            9 0
     C***                   Parm                    q1pgm

       Select;
       // Don't call W/W Waivers to display blanket waiver info
       When Not(isMidTerm) And @@Call = 'D' And chrWvrType = 'B';
       Other;
          If isMidTerm = *On;
             qqCall = *Blank;
             qqWvrMode = chrWvrMode;
          Else;
             qqCall = *Blank;
             qqWvrMode = chrWvrMode;
          Endif;
          qqRRN2 = elRRN;
          W0815R(qqKC:qqKL:qqCall:qqRRN:qqRRN2:Q1pgm:qqWvrMode);
          If qqKC = *On Or qqKL = *On;
             qqCall = 'F';
          Endif;
       Endsl;
      *** end chg ***** 02/02/21 **************************************

     C                   Endsr
