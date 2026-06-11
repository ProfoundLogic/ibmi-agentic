     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *=========================================================================
     H/TITLE - Work with Update Division/Policy Errors                         *
      *=========================================================================
      *                                                                        *
      *  DESCRIPTION: This program Shows the W/W Error Screen for error records*
      *  and updates all WDELP records if no error record found for other      *
      *  Policy /Divisions.                                                    *
      *                                                                        *
      *   CALLED BY: W0051R02                                                  *
      *                                                                        *
      *   CALLS TO : W0051C2                                                    *
      *                                                                        *
      *  CREATED: 07/27/2007  C:189 PROJECT WC1454                             *
      *=========================================================================
      * REVISIONS:
      *
    * 05/28/25 C:333 Support: PHW#118908 (Vital)
      *                BO - Noted Issues / Data Set Request
      *                - When policy effective date changed, effective
      *                date in WAQTP should match with changed date.
      *
    * 11/29/23 C:344 Project: WC2857
      *                Revise the processing that updates all divisions in the
      *                SRUPD routine to only update experience modifier
      *                witten within the same reporting rating bureau.
      *
    * 06/23/22 C:65  Support: Phw#91924   (Sunz)
    *                Do not update the division that is passed in as it was
      *                updated manually by user.
      *                Do not adjust the 1st modifier eff date as it can be
      *                prior to policy effective date, but if multiple modifiers
      *                are effective prior then roll them up to eliminate the
      *                modifiers that won't be used in calculation.
      *
    * 04/24/14 C:298 Support: WA136 85424
    *                Added logic to enable Payment Plan (Billing
      *                Schedule Code) saved across all divisions.
    *
    * 11/14/13 C:279 Project: WC1454  Priss#:45
    *                Modified to update all divisions
    *
      * 08/25/10  C:33  Project: WC1851  Priss#69
      *                -Add Claim Fee info
      *                -Add Collateral info
      *=========================================================================
    H*
     Fw0051f2   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxuap     if   e             disk
     F                                     rename(pua01:pfrcdf)
     Fwdell     uf   e           k disk
    F*
     Fwdell1    if   e           k disk    rename(pel01:pel02)  prefix(A_)
     F***  Begin Add  ***  08/25/10  **************************************
     Fwdefl     if   e           k disk    rename(pef01:pef00)  prefix(A_)
     Fwdefl1    uf   e           k disk
     F***  End   Add  ***  08/25/10  **************************************
    F*
     Fwmeml     if   e           k disk
    F*
    F*  Primary logical file definition (for displaying items)
     Fwxual     uf   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pua01:dbrcdf)
     Fwxual1    if   e           k disk    rename(pua01:dbrcdf1)
     F                                     infds(dbfds2)
     Fwxual2    if   e           k disk    rename(pua01:dbrcdf2) Prefix(B_)
     F                                     infds(dbfds3)
    F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    F*
     F***  Begin Chg  ***  11/29/23  **************************************
     F***wmrsl     if   e           k disk
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     F***  End   Chg  ***  11/29/23  **************************************
     F***  Begin Add  ***  11/29/23  **************************************
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     F***  End   Add  ***  11/29/23  **************************************
      //*** Beg Add ***** 05/28/25 ************************
     fwaqtl12   uf a e           k disk    rename(PQT01:PQT12)
     fwmeml3    uf   e           k Disk    rename(pem01:pem03)
     f                                     prefix(l3_)
      //*** End Add ***** 05/28/25 ************************
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
     D xxco#           s                   like(uaco#)
     D xxcanflg        s                   like(*in21)
     D qqemp#          s                   like(EMEMP#)
     D qqdiv           s                   like(EMDIV)
     D qqdiv1          s                   like(EMDIV)
     D qqdivM          s                   like(EMDIV)
     D qqfnd           s                   like(EMFND)
     D QQname          s                   like(SCNAME)
     D QQedn           s                   like(SCEDN)
     D xxerr           s              1
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Update Division/Policy Errors'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0051R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   2
     D viewmx          c                   2
     D sf1pag          c                   12
      *
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkuser          s                   like(uauser)
     D kkdt            s                   like(uadt)
     D kktime          s                   like(uatime)
     D kkco#           s                   like(uaco#)
     D kkfnd           s                   like(uafnd)
     D kkfyr           s                   like(uafyr)
     D kkemp#          s                   like(uaemp#)
     D kkst            s                   like(uast)
     D kkdiv           s                   like(uadiv)
     D kkdata          s                   like(uadata)
      ***  Begin Add  ***  11/29/23  *** WC2857 ***************************
     D @@RateBureau1   s              2    inz
     D @@RateBureau2   s              2    inz
      ***  End   Add  ***  11/29/23  *** WC2857 ***************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    QQuser           10
     C                   parm                    QQdate            7 0
     C                   parm                    QQtime            6 0
     C                   parm                    QQmode            1
     C                   parm                    QQcxfl            1
     C                   parm                    QQemp#
     C                   parm                    QQfnd
     C                   parm                    QQdiv                          It wil give Div
     C                   parm                    QQname
     C                   parm                    QQedn
     C                   parm                    QQMPO             1
     C                   parm                    QQdiv1                         It wil give # of DIV
     C                   parm                    QQdivM                         give # of DIV for 'M
     C                   parm                    #ntupd            5 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     Key1          klist
     C                   kfld                    kkuser
     C                   kfld                    kkdt
     C                   kfld                    kktime
    C*
     C     Key2          klist
     C                   kfld                    kkuser
     C                   kfld                    kkdt
     C                   kfld                    kktime
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    KKst
     C                   kfld                    kkdiv
     C                   kfld                    KKdata
    C*
    C*
     C     Key3          klist
     C                   kfld                    kkuser
     C                   kfld                    kkdt
     C                   kfld                    kktime
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkdiv
     C                   kfld                    kkemp#
     C                   kfld                    KKst
     C                   kfld                    KKdata
    C*
     C     Key4          klist
     C                   kfld                    uauser
     C                   kfld                    uadt
     C                   kfld                    uatime
     C                   kfld                    uaco#
     C                   kfld                    uafnd
     C                   kfld                    uafyr
     C                   kfld                    uaemp#
     C                   kfld                    uadiv
     C                   kfld                    uadata
    C*
     C     K_del1        klist
     C                   kfld                    uaco#
     C                   kfld                    uafnd
     C                   kfld                    uaemp#
     C                   kfld                    uadiv
    C*
     C     K_del3        klist
     C                   kfld                    uaco#
     C                   kfld                    uafnd
     C                   kfld                    uafyr
     C                   kfld                    qqemp#
     C                   kfld                    qqdiv
    C*
     C     K_del31       klist
     C                   kfld                    uaco#
     C                   kfld                    uafnd
     C                   kfld                    uaemp#
     C                   kfld                    uadiv
     C                   kfld                    uafyr
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkemp# = *loval
     C                   eval      kkdiv  = *loval
     C                   eval      kkst  = *loval
     C                   eval      kkdata = *loval
     C                   eval      kkco#  = *loval
     C                   eval      kkfnd  = *loval
     C                   eval      kkfyr  = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   If        *in21 = *on
     C                   Move      scpst1        xxemp#            9 0
     C                   Move      scpst2        xxdiv#            5 0
     C                   eval      kkemp#    =   xxemp#
     C                   eval      KKdiv     =   xxdiv#
     C                   eval      KKst      =   scrst
     C                   eval      kkdata    =   scfeld
     C                   eval      kkco#     =   scco#
     C                   eval      kkfnd     =   scfnd
     C                   eval      kkfyr     =   scfyr
     C                   eval      kkdata    =   *loval
     C                   endif
     C*
     C                   If        *in22 = *on
     C                   Move      scpst2        xxdiv#            5 0
     C                   eval      kkemp#    =   *loval
     C                   eval      KKdiv     =   xxdiv#
     C                   eval      KKst      =   *loval
     C                   eval      kkdata    =   scfeld
     C                   eval      kkco#     =   scco#
     C                   eval      kkfnd     =   scfnd
     C                   eval      kkfyr     =   scfyr
     C                   eval      kkdata    =   *loval
     C                   endif
     C*
     C                   If        *in67     = *on
     C                   Move      *blanks       scpst1
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkemp#    =   scemp#
     C                   eval      KKdiv     =   scdiv#
     C                   eval      KKst      =    scrst
     C                   eval      kkdata    =   scfeld
     C                   eval      kkco#     =   scco#
     C                   eval      kkfnd     =   scfnd
     C                   eval      kkfyr     =   scfyr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      Scemp# = uaemp#
     C                   eval      Scdiv# =  uadiv
     c                   eval      Scco#  =  uaco#
     c                   eval      Scfnd  =  uafnd
     c                   eval      Scfyr  =  uafyr
     c                   eval      Scrst  =  uast
     C*
     C**** uaco#         Chain     WMRSL
     C*
     C                   Movel     uadata        SCFELD
     C
     C                   eval      scdata = ' ' + %editc(scemp#:'4') + '   '
     C                                    + %editc(scdiv#:'2') + '            '
     C                                    + uast + '         '
     C                                    + scfeld
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C                   If        *Inkg = *on
     C                   Exsr      SRUPD
     C                   Exsr      PGMEND
     C                   Endif
     C*
     C                   If        *Inkv = *on
     C                   If        *In67 = *on
     C                   Eval      QQ#div = SC#DIV#
     C                   else
     C                   Eval      QQ#div = SC#DIV
     C                   Endif
     C*
     C                   Call      'W0051C2'
     C                   Parm                    kkuser
     C                   Parm                    kkdt
     C                   Parm                    kktime
     C                   parm                    QQname
     C                   parm                    QQedn
     C                   parm                    QQ#div            5 0
     C                   parm                    QQcxfl            1
     C                   parm                    QQmpo             1
     C                   parm                    #ntupd
     C                   Endif
     C*
     C                   Exsr      PGMEND
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   return
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
       //*** Beg Add *** 11/29/23 *** WC2857 *******************************
       // Determine the Rate Set Code of division being passed
       Chain (##Co#:qqFnd:qqEmp#:qqDiv) pel01;

       // Determine the rating state code
       Chain (ELco#:ELrtcd) prs00;

       // Determine rating bureau by extracting from 'NCCI' generic table.
       // If rec exists, set state field to GTKEY. If no rec exists then
       // populate the state field with the rating state of the division.
       Chain (RSco#:'NCCI':RSjrst) pgt00;
       If %found(WTGTL);
          @@RateBureau1 = 'XX';    // NCCI state
       Else;
          @@RateBureau1 = RSjrst;  // non-NCCI state
       Endif;
       //*** End Add *** 11/29/23 *** WC2857 *******************************
     C*
     C                   eval      kkuser   = QQuser
     C                   eval      kkdt     = QQdate
     C                   eval      kktime   = QQtime
     C*
     C                   Move      *off          *in67
     C                   Move      *off          *in69
     C*
     C                   If        QQmpo =  'M'
     C                   Move      *on           *in67
     C                   Else
     C                   Move      *off          *in67
     C                   Endif
     C*
     C*If Program is called in U mode than execute SRUPD & exit
     C*
     C                   If        QQmode = 'U'
     C                   Exsr      SRUPD
     C                   Exsr      PGMEND
     C                   Endif
     C*
     C                   Z-ADD     QQfnd         SCFND
     C                   If        *IN67 = '1'
     C                   Z-ADD     QQdivM        SC#DIV#
     C                   Else
     C                   Z-ADD     QQdiv1        SC#DIV
     C                   endif
     C                   Move      QQname        SCNAME
     C                   Move      QQedn         SCEDN
     C*
     C                   If        QQcxfl = 'Y'
     C                   If        *IN67 = '1'
     C                   Move      'Y'           SCCAN
     C                   Else
     C                   Move      'Y'           SCCAN1
     C                   endif
     C                   Else
     C                   If        *IN67 = '1'
     C                   Move      'N'           SCCAN
     C                   Else
     C                   Move      'N'           SCCAN1
     C                   endif
     C                   endif
     C*
     C*Check If Its PEO Employer
     C                   If        QQmpo <> 'M'
     C                   Eval      scpst1 =  %editc(qqemp#:'Z')
     C                   Move      *on           *in69                          Protect indicator
     C                   Move      *off          *in67
     C                   exsr      kkdo
     C                   Else
     C                   If        QQmpo  = 'M'
     C                   Move      *on           *in67
     C                   endif
     C                   endif
     C*
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      xxco# = upco#
     C                   endif                                                  %found
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key1          reade     dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key1          reade     dbrcdf1                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key1          readpe    dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C     key1          readpe    dbrcdf1                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key2          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key3          setgt     dbrcdf1                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key2          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key3          setll     dbrcdf1                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
      *
     C                   if        uast   = *Blanks
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
    C*================================================================
    C* SRUPD  - Update data across All Divisions / Policies
    C*================================================================
     C     srupd         begsr
     C
     C* Read All WXUAP records
     C
     C     key1          Setll     dbrcdf
     C     key1          Reade     dbrcdf
     C                   Dow       not %Eof(Wxual)
     C     K_del3        Chain     PEL02
     C***  Begin Add  ***  08/25/10  **************************************
     C                   clear                   a_EFEXCF
     C                   clear                   a_EFEXCO
     C                   clear                   a_EFSCBD
     C     K_del3        chain     pef00
     C***  End   Add  ***  08/25/10  **************************************
     C
     C* Only process the recods having Rating state Blank
     C
     C                   If        uast  = ' '
     C******             If        uamode = 'T'
     C     K_del31       Chain     Pel01
     C                   if        %found(Wdell)
     C*
     C* Check the Flag for cancelled Division if cancelled divisions to
     C* be included or not. if QQcxfl <> 'Y' then find out that Policy is cancelled or not
     C*
     C                   Eval      xxcanflg = *off
     C*
     C                   If        QQcxfl <> 'Y'
     c     k_del1        chain     wmeml
     c                   if        %Found(wmeml)
     C*
     c* determine if policy is active
     C*
     c                   if        (emcad1 = 0 and emcad2 = 0) and
     c                             (emcad3 = 0 and emcad4 = 0 and emcad5 = 0)
     c                   Eval      xxcanflg  = *off
     c                   else
     c                   Eval      xxcanflg  = *on
     c                   endif
     c                   endif
     C                   Endif                                                  QQcxfl <> 'Y'
     C
     c                   If        xxcanflg  = *on  and QQcxfl <> 'Y'
     C                   Goto      Skip1
     C                   Endif
     C*** begin add ***  06/23/22  *********************************************
     C                   If        ElEmp# = qqEmp# and ELDiv = qqDiv
     C                   Goto      Skip1
     C                   Endif
     C*** end add *****  06/23/22  *********************************************
     C
     C                   Movel     Q1pgm         Elpgm
     C                   Movel     Q1user        Eluser
     C                   Movel     QQdate        Eldt
     C                   Movel     QQtime        Eltime
     C
     C* Update the field mentioned in UADATA of WXUAP
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'POLICY FROM DATE ERRORS'
      /Free
       If UAdata = 'Policy Effective Date Errors';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-ADD     A_ELPFDT      ELPFDT
      *** Beg Add ***** 05/28/25 **************************************
       Chain(e) (uaCO#:uaFND:uaEMP#:uaDiv) pqt12;
       If %Found(WAQTL12);
          QTefdt = A_ELPFDT;
          QTuser = Q1USER;
          QTpgm  = Q1PGM;
          QTtime = QQtime;
          QTdt   = QQdate;
          Update pqt12;
       Endif;

       // Inception Date
       Chain(e) (uaCO#:uaEMP#:uaDiv:uaFND) pem03;
       If %Found(WMEML3);
          L3_EMefD1 = A_ELPFDT;
          L3_EMuser = Q1USER;
          L3_EMpgm  = Q1PGM;
          L3_EMtime = QQtime;
          L3_EMdt   = QQdate;
          Update pem03;
       Endif;
      *** End Add ***** 05/28/25 **************************************
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'POLICY TO DATE ERRORS'
      /Free
       If UAdata = 'Policy Expiration Date Errors';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-ADD     A_ELPTDT      ELPTDT
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'SUMMARIZE A/R TO ONE DIV'
      /Free
       If UAdata = 'Summarize A/R To One Div';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELARSM      ELARSM
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata='OVERRIDE ON CHARGING INSTALLMENT FEE'
      /Free
       If UAdata = 'Override On Charging Installment Fee';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELIFFL      ELIFFL
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'POLICY TYPE  '
      /Free
       If UAdata = 'Policy Type';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELQTYP      ELQTYP
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'ADVERSE ACCOUNT FLAG'
      /Free
       If UAdata = 'Adverse Account';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELADVR      ELADVR
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'Do not renew to new year (Y/ )'
      /Free
       If UAdata = 'Do Not Renew To New Year';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELRLFL      ELRLFL
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'BILLING START DATE ERRORS'
      /Free
       If UAdata = 'Billing Start Date Errors';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELBSDT      ELBSDT
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = '"A" RATING FLAG'
      /Free
       If UAdata = '"A" Rating';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELSTAT      ELSTAT
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'INCREASED LIMITS CODE ' or
     C******                       uadata = 'INCREASED LIM. EFF. DATE' or
     C******                       uadata = 'INCREASED LIM. EXP. DATE' or
     C******                       uadata = 'INCREASED LIMITS 2 CODE'  or
     C******                       uadata = 'INC LIMITS 2 EFF DT'      or
     C******                       uadata = 'INC LIMITS 2 EXP DT '
      /Free
       If UAdata ='Increased Limits Code' or UAdata =
        'Increased Limits Effective Date' or UAdata =
        'Increased Limits Expiration Date' or UAdata =
        'Increased Limits 2 Code' or UAdata = 'Increased Limits 2 Eff Date'
        or UAdata = 'Increased Limits 2 Exp Date';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C*
     C                   Movel     A_ELILCD      ELILCD
     C*
     C                   If        A_ELILEF  <   ELPFDT
     C
     C                   If        A_ELILEF  >   0
     C                   Z-Add     ELPFDT        ELILEF
     C                   Else
     C                   Z-Add     0             ELILEF
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELILEF      ELILEF
     C                   Endif
     C*
     C                   If        A_ELILEX  >   ELPTDT
     C                   If        A_ELILEX  >   0
     C                   Z-Add     ELPTDT        ELILEX
     C                   Else
     C                   Z-Add     0             ELILEX
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELILEX      ELILEX
     C                   Endif
     C*
     C                   Movel     A_ELINL2      ELINL2
     C*
     C                   If        A_ELI2EF  <   ELPFDT
     C                   If        A_ELI2EF  >   0
     C                   Z-Add     ELPFDT        ELI2EF
     C                   Else
     C                   Z-Add     0             ELI2EF
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELI2EF      ELI2EF
     C                   Endif
     C*
     C                   IF        A_ELI2EX  >   ELPTDT
     C                   If        A_ELI2EX  >   0
     C                   Z-Add     ELPTDT        ELI2EX
     C                   Else
     C                   Z-Add     0             ELI2EX
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELI2EX      ELI2EX
     C                   Endif
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*****              If        uadata = 'DEDUCTIBLE CODE'             or
     C*****                        uadata = 'DEDUCTIBLE EFF. DATE'        or
     C*****                        uadata = 'DEDUCTIBLE EXP. DATE'        or
     C*****                        uadata = '2ND DEDUCTIBLE DISCOUNT CODE' or
     C*****                        uadata = '2ND DEDUCTIBLE EFF. DATE'     or
     C*****                        uadata = '2ND DEDUCTIBLE EXP. DATE '    or
     C*****                        uadata = 'DEDUCTIBLE BILL IF CANCELLED'
      /Free
       If UAdata = 'Deductible Code' or UAdata = 'Deductible Effective Date'
        or UAdata ='Deductible Expiration Date' or UAdata =
        '2nd Deductible Discount Code' or UAdata =
        '2nd Deductible Effective Date' or UAdata =
        '2nd Deductible Expiration Date' or UAdata =
        'Deductible Bill If Cancelled';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C*
     C                   Movel     A_ELDCDE      ELDCDE
     C*
     C                   If        A_ELDCEF  <   ELPFDT
     C                   If        A_ELDCEF  >   0
     C                   Z-Add     ELPFDT        ELDCEF
     C                   Else
     C                   Z-Add     0             ELDCEF
     C                   endif
     C                   else
     C                   Z-Add     A_ELDCEF      ELDCEF
     C                   endif
     C*
     C                   If        A_ELDCEX  >   ELPTDT
     C                   If        A_ELDCEX  >   0
     C                   Z-Add     ELPTDT        ELDCEX
     C                   Else
     C                   Z-Add     0             ELDCEX
     C                   endif
     C                   else
     C                   Z-Add     A_ELDCEX      ELDCEX
     C                   Endif
     C*
     C                   Movel     A_ELDCD2      ELDCD2
     C*
     C                   If        A_EL2DEF  <   ELPFDT
     C                   If        A_EL2DEF  >   0
     C                   Z-Add     ELPFDT        EL2DEF
     C                   Else
     C                   Z-Add     0             EL2DEF
     C                   Endif
     C                   else
     C                   Z-Add     A_EL2DEF      EL2DEF
     C                   Endif
     C*
     C                   If        A_EL2DEX  >   ELPTDT
     C                   If        A_EL2DEX  >   0
     C                   Z-Add     ELPTDT        EL2DEX
     C                   Else
     C                   Z-Add     0             EL2DEX
     C                   Endif
     C                   else
     C                   Z-Add     A_EL2DEX      EL2DEX
     C                   Endif
     C*
     C                   Movel     A_ELCXBL      ELCXBL
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'WAIVER TYPE'                  or
     C******                       uadata = 'WAIVER SURCHRG PREM LVL'      or
     C******                       uadata = 'WAIVER % (SPECIFIC=NET%)'     or
     C******                       uadata = 'MINIMUM WAIVER SURCHARGE'     or
     C******                       uadata = 'WAIVER FLAT CHARGE'           or
     C******                       uadata = 'WAIVER MAX CHRG'
      /Free
       If UAdata = 'Waiver Type' or UAdata = 'Waiver Surcharge Prem Lvl'
        or UAdata = 'Waiver % (Specific=Net%)' or UAdata =
        'Minimum Waiver Surcharge' or UAdata = 'Waiver Flat Charge'
        or UAdata = 'Waiver Maxium Charge';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C*
     C                   Movel     A_ELWVTP      ELWVTP
     C                   Movel     A_ELWVFL      ELWVFL
     C                   Movel     A_ELWVPC      ELWVPC
     C                   Z-Add     A_ELWMIN      ELWMIN
     C                   Z-Add     A_ELFCHG      ELFCHG
     C                   Z-Add     A_ELWMAX      ELWMAX
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'DEVIATION '           or
     C******                       uadata = 'DEVIATION EFF. DATE'  or
     C******                       uadata = 'DEVIATION EXP. DATE'
      /Free
       If UAdata = 'Deviation' or UAdata = 'Deviation Effective Date'
        or UAdata = 'Deviation Expiration Date';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C*
     C                   Z-Add     A_ELASP2      ELASP2
     C                   If        A_ELDVEF  <   ELPFDT
     C                   If        A_ELDVEF  >   0
     C                   Z-Add     ELPFDT        ELDVEF
     C                   Else
     C                   Z-Add     0             ELDVEF
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELDVEF      ELDVEF
     C                   Endif
     C*
     C                   IF        A_ELDVEX  >   ELPTDT
     C                   If        A_ELDVEX  >   0
     C                   Z-Add     ELPTDT        ELDVEX
     C                   Else
     C                   Z-Add     0             ELDVEX
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELDVEX      ELDVEX
     C                   Endif
     C*
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'CONTRACTORS ADJ CREDIT' or
     C******                       uadata = 'CONTRACTORS EFF. DATE'  or
     C******                       uadata = 'CONTRACTORS EXP. DATE'
      /Free
       If UAdata = 'Contractors Adj Credit' or UAdata =
        'Contractors Effective Date' or UAdata =
        'Contractors Expiration Date';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELASP1      ELASP1
     C*
     C                   If        A_ELCREF  <   ELPFDT
     C                   If        A_ELCREF  >   0
     C                   Z-Add     ELPFDT        ELCREF
     C                   Else
     C                   Z-Add     0             ELCREF
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELCREF      ELCREF
     C                   Endif
     C*
     C                   If        A_ELCREX   >  ELPTDT
     C                   If        A_ELCREX  >   0
     C                   Z-Add     ELPTDT        ELCREX
     C                   Else
     C                   Z-Add     0             ELCREX
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELCREX      ELCREX
     C                   Endif
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'SUBJ PREMIUM CREDIT 1A PERCENT' or
     C******                       uadata = 'SUBJ PREM 1A EFF. DATE '        or
     C******                       uadata = 'SUBJ PREM 1A EXP. DATE'
      /Free
       If UAdata = 'Subj Premium Credit 1A Percent' or UAdata =
        'Subj Premium 1A Effective Date' or UAdata =
        'Subj Premium 1A Expiration Date';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELASPM      ELASPM
     C*
     C                   If        A_ELMCEF  <   ELPFDT
     C                   If        A_ELMCEF  >   0
     C                   Z-Add     ELPFDT        ELMCEF
     C                   Else
     C                   Z-Add     0             ELMCEF
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELMCEF      ELMCEF
     C                   Endif
     C*
     C                   IF        A_ELMCEX  >   ELPTDT
     C                   If        A_ELMCEX  >   0
     C                   Z-Add     ELPTDT        ELMCEX
     C                   Else
     C                   Z-Add     0             ELMCEX
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELMCEX      ELMCEX
     C                   Endif
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'MCO TYPE CODE'  or
     C******                       uadata = 'MCO GROUP CODE'
      /Free
       If UAdata = 'MCO Type Code' or UAdata = 'MCO Group Code' ;
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELMCCD      ELMCCD
     C                   Movel     A_ELMCGP      ELMCGP
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'DEVIATION 2 '        or
     C******                       uadata = 'DEVIATION 2 EFF DT'  or
     C******                       uadata = 'DEVIATION 2 EXP DT'
      /Free
       If UAdata = 'Deviation 2' or UAdata = 'Deviation 2 Effective Date'
           or UAdata = 'Deviation 2 Expiration Date' ;
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C*
     C                   Z-Add     A_ELDEV2      ELDEV2
     C*
     C                   If        A_ELD2EF  <   ELPFDT
     C                   If        A_ELD2EF  >   0
     C                   Z-Add     ELPFDT        ELD2EF
     C                   Else
     C                   Z-Add     0             ELD2EF
     C                   Endif
     C                   else
     C                   Z-Add     A_ELD2EF      ELD2EF
     C                   Endif
     C*
     C                   If        A_ELD2EX   >  ELPTDT
     C                   If        A_ELD2EX  >   0
     C                   Z-Add     ELPTDT        ELD2EX
     C                   Else
     C                   Z-Add     0             ELD2EX
     C                   Endif
     C                   else
     C                   Z-Add     A_ELD2EX      ELD2EX
     C                   Endif
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'EXPERIENCE MODIFIER 1'  or
     C******                       uadata = 'EXPERIENCE MODIFIER 2'  or
     C******                       uadata = 'EXPERIENCE MODIFIER 3'  or
     C******                       uadata = 'EXP MOD EFFECT DATE 1'  or
     C******                       uadata = 'EXP MOD EFFECT DATE 2'  or
     C******                       uadata = 'EXP MOD EFFECT DATE 3'
      /Free
       If UAdata = 'Experience Modifier 1' or UAdata =
        'Experience Modifier 2' or UAdata = 'Experience Modifier 3'
        or UAdata = 'Experience Modifier Eff Date 1' or UAdata =
        'Experience Modifier Eff Date 2' or UAdata =
        'Experience Modifier Eff Date 3';
      /End-free
      //*** Beg Add *** 11/29/23 *** WC2857 ********************************
       // Determine the rating state code
       Chain (ELco#:ELrtcd) prs00;

       // Retrieve rating bureau
       Chain (RSco#:'NCCI':RSjrst) pgt00;
       If %found(WTGTL);
          @@RateBureau2 = 'XX';    // NCCI state
       Else;
          @@RateBureau2 = RSjrst;  // non-NCCI state
       Endif;

       // Update Experience modifier info only if the
       // rating bureau of the division being processed is the same
       // as the rating bureau determined at the beginning of the program.
       If @@RateBureau1 = @@RateBureau2;
      //*** End Add *** 11/29/23 *** WC2857 ********************************
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELEXM1      ELEXM1
     C                   Z-Add     A_ELEXM2      ELEXM2
     C                   Z-Add     A_ELEXM3      ELEXM3
     C*
     C*** begin chg ***  06/23/22  **************************************
     C******             If        A_ELEMD1  <   ELPFDT
     C******             If        A_ELEMD1  >   0
     C******             Z-Add     ELPFDT        ELEMD1
     C******             Else
     C******             Z-Add     0             ELEMD1
     C******             Endif
     C******             Else
     C******             Z-Add     A_ELEMD1      ELEMD1
     C******             Endif
     C*
     C******             If        A_ELEMD2  <   ELPFDT
     C******             If        A_ELEMD2  >   0
     C******             Z-Add     ELPFDT        ELEMD2
     C******             Else
     C******             Z-Add     0             ELEMD2
     C******             Endif
     C******             Else
     C******             Z-Add     A_ELEMD2      ELEMD2
     C******             Endif
     C*
     C******             If        A_ELEMD3  <   ELPFDT
     C******             If        A_ELEMD3  >   0
     C******             Z-Add     ELPFDT        ELEMD3
     C******             Else
     C******             Z-Add     0             ELEMD3
     C******             Endif
     C******             Else
     C******             Z-Add     A_ELEMD3      ELEMD3
     C******             Endif
     C*
     C* Default all modifier information in and then roll out old mods.
     C*
     C                   Z-Add     A_ELEMD1      ELEMD1
     C                   Z-Add     A_ELEMD2      ELEMD2
     C                   Z-Add     A_ELEMD3      ELEMD3
     C*
     C                   Move      A_ELExmd1nf   ELExmd1nf
     C                   Move      A_ELExmd2nf   ELExmd2nf
     C                   Move      A_ELExmd3nf   ELExmd3nf
     C*
     C* Move 3rd mod to 1st and clear other two.
     C*
     C                   If        ELEmd3 < ELPfdt and ELEmd3 <> 0
     C                   Z-add     ELEmd3        ELEmd1
     C                   Z-add     ELExm3        ELExm1
     C                   Z-add     ELArp3        ELArp1
     C                   Move      ELExmd3nf     ELExmd1nf
     C                   Clear                   ELEmd2
     C                   Clear                   ELExm2
     C                   Clear                   ELEmd3
     C                   Clear                   ELExm3
     C                   Clear                   ELExmd2nf
     C                   Clear                   ELExmd3nf
     C                   Clear                   ELArp2
     C                   Clear                   ELArp3
     C                   Else
     C*
     C* Move 2nd and 3rd mod to 1st and 2nd and clear 3rd.
     C*
     C                   If        ELEmd2 < ELPfdt and ELEmd2 <> 0
     C                   Z-add     ELEmd2        ELEmd1
     C                   Z-add     ELExm2        ELExm1
     C                   Z-add     ELEmd3        ELEmd2
     C                   Z-add     ELExm3        ELExm2
     C                   Z-add     ELArp2        ELArp1
     C                   Z-add     ELArp3        ELArp2
     C                   Move      ELExmd2nf     ELExmd1nf
     C                   Move      ELExmd3nf     ELExmd2nf
     C                   Clear                   ELEmd3
     C                   Clear                   ELExm3
     C                   Clear                   ELExmd3nf
     C                   Clear                   ELArp3
     C                   Endif
     C*
     C                   Endif
     C*** end chg *****  06/23/22  **************************************
      //*** Beg Add *** 11/29/23 *** WC2857 ********************************
       Endif;
      //*** End Add *** 11/29/23 *** WC2857 ********************************
     C*
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'SUBJ PREMIUM CREDIT 1B PERCENT'  or
     C******                       uadata = 'SUBJ PREM 1B EFF. DATE'          or
     C******                       uadata = 'SUBJ PREM 1B EXP. DATE'
      /Free
       If UAdata = 'Subj Premium Credit 1B Percent' or UAdata =
        'Subj Premium 1B Effective Date' or UAdata =
        'Subj Premium 1B Expiration Date';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELASPS      ELASPS
     C*
     C                   If        A_ELSCEF  <   ELPFDT
     C                   If        A_ELSCEF  >   0
     C                   Z-Add     ELPFDT        ELSCEF
     C                   Else
     C                   Z-Add     0             ELSCEF
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELSCEF      ELSCEF
     C                   Endif
     C*
     C                   If        A_ELSCEX  >   ELPTDT
     C                   If        A_ELSCEX  >   0
     C                   Z-Add     ELPTDT        ELSCEX
     C                   Else
     C                   Z-Add     0             ELSCEX
     C                   Endif
     C                   Else
     C                   Z-Add     A_ELSCEX      ELSCEX
     C                   Endif
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'SHORT RATE/ PRORATA FLAG '
      /Free
       If UAdata = 'Short Rate/ Prorata';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELSRPR      ELSRPR
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'DISCOUNT'      or
     C******                       uadata = 'DISC TYPE S,G'
      /Free
       If UAdata = 'Discount' or UAdata = 'Disc Type S,G';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELDISC      ELDISC
     C                   Z-Add     A_ELASP3      ELASP3
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'INTERNAL PROMULGATION'
      /Free
       If UAdata = 'Internal Promulgation';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELPROM      ELPROM
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'SCHEDULE DB/CR FACTOR'  or
     C******                       uadata = 'SCHEDULE DB/CR EFF DT'  or
     C******                       uadata = 'SCHEDULE DB/CR EXP DT'
      /Free
       If UAdata = 'Schedule Db/Cr Factor' or UAdata =
        'Schedule Db/Cr Effective Date' or UAdata =
        'Schedule Db/Cr Expiration Date';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELDBCR      ELDBCR
     C                   If        A_ELSDEF  <   ELPFDT
     C                   If        A_ELSDEF  >   0
     C                   Z-Add     ELPFDT        ELSDEF
     C                   Else
     C                   Z-Add     0             ELSDEF
     C                   Endif
     C                   else
     C                   Z-Add     A_ELSDEF      ELSDEF
     C                   Endif
     C*
     C                   If        A_ELSDEX  >   ELPTDT
     C                   If        A_ELSDEX  >   0
     C                   Z-Add     ELPTDT        ELSDEX
     C                   Else
     C                   Z-Add     0             ELSDEX
     C                   Endif
     C                   else
     C                   Z-Add     A_ELSDEX      ELSDEX
     C                   Endif
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'CX NOTICE DAYS '
      /Free
       If UAdata = 'CX Notice Days';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELCXDY      ELCXDY
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'POLICY PAGE ITEM 3C '     or
     C******                       uadata = 'POLICY PG ITEM 3C LINE 2'
      /Free
       If UAdata = 'Policy Page Item 3C' or UAdata =
        'Policy Page Item 3C Line 2';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELPP3C      ELPP3C
     C                   Movel     A_ELPPC3      ELPPC3
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata = 'MEMBER FEE AMOUNT '
      /Free
       If UAdata = 'Member Fee Amount';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELMFEE      ELMFEE
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'SAFETY FEE PERCENT'  or
     C*******                      uadata = 'SAFETY FEE AMOUNT'   or
     C*******                      uadata = 'SAFETY FEE MIN AMT'
      /Free
       If UAdata = 'Safety Fee Percent' or UAdata = 'Safety Fee Min Amount';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELSFPC      ELSFPC
     C                   Z-Add     A_ELSFAM      ELSFAM
     C                   Z-Add     A_ELSFMN      ELSFMN
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'ADDITIONAL MAILING DAYS'
      /Free
       If UAdata = 'Additional Mailing Days';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELADDM      ELADDM
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'ANNIVERSARY RATING DATE ERRORS'
      /Free
       If UAdata = 'Anniversary Rating Date Errors';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Z-Add     A_ELANDT      ELANDT
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'EXCLUDE MEDICAL'
      /Free
       If UAdata = 'Exclude Medical';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELEXMF      ELEXMF
     C                   Endif
     C*
     C* Check that if its MUltiple PEO Employer and the Diviisons are more than 1
     C*
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata = 'ACTIVITY STATUS'
      /Free
       If UAdata = 'Activity Status';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELASCD      ELASCD
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C*******            If        uadata='SHORT TERM TO ESTA BLISH CONCURRENCY'
      /Free
       If UAdata = 'Short Term To Establish Concurrency';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELCOFL      ELCOFL
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata='TYPE OF BUILDING CODE'
      /Free
       If UAdata = ' Type Of Building Code';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELTYPB      ELTYPB
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata='PREMIUM FINANCE CODE '
      /Free
       If UAdata = 'Premium Finance Code';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELPFIN      ELPFIN
     C                   Endif
     C*
      ******************* Beg Chg ******************** 11/14/13 *********************
     C******             If        uadata='CONTESTED AUDIT'
      /Free
       If UAdata = 'Contested Audit';
      /End-free
      ******************* End Chg ******************** 11/14/13 *********************
     C                   Movel     A_ELCTST      ELCTST
     C                   Endif

      /Free
       //***** Beg Add ***** 04/24/14 *************************************************
       If UAdata = 'Billing Schedule Code';
          ELbscd = A_ELbscd;
       Endif;
       //***** End Add ***** 04/24/14 *************************************************
      /End-free
     C*
     C*
     C* Here we need to check before update that the record we are going to
     C* update is not having any error record in the same file
     C*
     C                   eval      xxerr = ' '
     C     Key4          Setll     dbrcdf2
     C     Key4          Reade     dbrcdf2
     C                   Dow       Not %eof(WXUAL2)
     C                   If        B_UAST <> *blanks
     C                   eval      xxerr = 'Y'
     C                   leave
     C                   Endif
     C     Key4          Reade     dbrcdf2
     C                   Enddo
     C*
     C                   If        xxerr <> 'Y'
     C                   Update    Pel01
     C***  Begin Add  ***  08/25/10  **************************************
     C                   exsr      UpdateWDEFP
     C***  End   Add  ***  08/25/10  **************************************
     C                   Endif
     C
     C     Skip1         Tag
     C                   endif                                                  Chain
     C********           endif                                                  uamode = 'T'
     C
     C                   endif                                                  uast = ' '
     C     key1          Reade     dbrcdf
     C                   Enddo
     C
     C                   endsr
     C*
     C***  Begin Add  ***  08/25/10  **************************************
    C*====================================================================
    C* Update collateral fee information
    C*====================================================================
     C     UpdateWDEFP   begsr
     C*
     C                   clear                   EFEXCF
     C                   clear                   EFEXCO
     C                   clear                   EFSCBD
     C     K_del31       chain     pef01
     C                   if        UADATA='Exclude employer from claim fee'
     C                              or UADATA='Exclude employer from collateral'
     C                              or UADATA='Stop collateral billing date'
     C                   eval      EFEXCF = a_EFEXCF
     C                   eval      EFEXCO = a_EFEXCO
     C                   eval      EFSCBD = a_EFSCBD
     C                   eval      EFPGM  = q1Pgm
     C                   eval      EFUSER = q1User
     C                   eval      EFDT   = qqDate
     C                   eval      EFTIME = qqTime
     C                   endif
     C                   if        %found(wdefl1)
     C                   update    pef01
     C                   endif
     C*
     C                   endsr
     C***  End   Add  ***  08/25/10  **************************************
    C*================================================================
** Column headings
Opt     Employer#   Division    Rating state   Field
Opt     Employer#   Division    Rating state   Field
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
207F7=Update                YYY
212F12=Cancel               NNN
217F17=Subset               NNN
221F21=Print                YYY
