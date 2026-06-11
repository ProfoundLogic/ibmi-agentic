     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W0028R02 - EFT Account Information Maintenance           *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: EFT Account Information Maintenance      *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: W0028R02                                            *
     H*                                                               *
     H* CREATED: 08/09/07  C:191 PROJECT WC1453                       *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 05/20/21 C:180 Project: WC1986
     H*                Add starting policy year.
     H*
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 05/06/15 C:62 Project 114MRH                                  *
     H*               Add EOB Format fields.                          *
     H*                                                               *
     H* 12/27/11 C:282 Project: WC2005                                *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H* 11/30/11 C:180 SUPPORT  78504                                 *
     H*                Change date validation - Do not allow dates    *
     H*                older than 1970.                               *
     H*                                                               *
     H* 10/24/11 C:180 SUPPORT  78290                                 *
     H*                Strenthen edit for dates (Effective,Expiration,*
     H*                approved, and Pre-note) which are used in      *
     H*                date conversions with the *mdy special word.   *
     H*                                                               *
     H* 07/20/11 C:136 Support: WA099 77471                           *
     H*                Strengthen edit for overlapping date ranges.   *
     H*                                                               *
     H* 04/26/11 C:136 Support: WA025 76707                           *
     H*                Correct EFKEY for additional addresses         *
     H*                                                               *
     H* 04/21/11 C:190 Project: 114WYI                                *
     H*                Add Account Type field.                        *
     H*                                                               *
     H* 07/12/10 C:136 Support: WA025 73052                           *
     H*                Prevent the seq# from being changed when       *
     H*                adding an EFT account when called from W/W     *
     H*                Aditional Addresses (*in51=*on)                *
     H*                                                               *
     H* 07/01/10 C:136 Support: WA133 73210                           *
     H*                Allow users to backdate the eff date for       *
     H*                'E'mployer type EFT accounts to the inception  *
     H*                date of the policy.                            *
     H*                                                               *
     H* 06/29/09 C:191 Project: WC1806                                *
     H*               -Update params to match W0028R                  *
     H*               -Add employer mode when @@type = 'E'            *
     H*                                                               *
     H* 05/14/08 C:33  Project WC997  Priss#10                        *
     H*               -Do not default pre-note or approval date.      *
     H*               -Pre-note date should be dispaly only and not   *
     H*                 shown in -Add- mode.                          *
     H*               -Approval date should not be shown in -Add- mode*
     H*               -Ensure approval date can be updated outside of *
     H*                 -Add- mode.                                   *
     H*                                                               *
     H* 11/15/07 C:191 Project WC1453 PRISS                           *
     H*                Correct company number field                   *
     H*                                                               *
     H* 10/05/07 C:136 Project WC1453 PRISS                           *
     H*                Correct edits for dates                        *
     H*                                                               *
     H* 09/20/07 C:136 Project: WC997                                 *
     H*                EFT - Agent Commossions                        *
     H*****************************************************************
     Fw0028f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmefp     uf a e             disk
     F                                     rename(pef01:dbrcdf)
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    F*
     F*                          (for getting dependent name)
     Fwdacl     if   e           k disk
     Fwdacl1    if   e           k disk    rename(pac01 : pacnm)
     F*                          (for getting provider name)
     Fwmpdl     if   e           k disk
    F*
    F*  Claims file definition (for validating claim#)
     Fwmcml1    if   e           k disk
    F*  Claims physical file definition (for search)
     Fwmcmp     if   e             disk    rename(pcm01:pcm01p)
    F*                                  (for search)
     Fwdacp     if   e             disk    rename(pac01:pac01p)
     fwmaal1    if   e           k disk    rename(paa01 : paa0l1)
     fwmfnl     if   e           k disk    infds(dbfdsf)
     Fwmefl7    if   e           k disk    rename(pef01 : pef0l7)
     f                                     infds(ef7fds) prefix(l7)
     fwmeml     if   e           k disk
     Fwmaap     if   e             disk    rename(paa01:paa01p)
     Fwmemp     if   e             disk    rename(pem01:pem01p)
     Fwmpdp     if   e             disk    rename(ppd01:ppd01p)
      *** beg add ***** 05/20/21 ***************************
     fwdell2    if   e           k disk    rename(pel01:pel02)
      *** end add ***** 05/20/21 ***************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EFT Account Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d dbfdsf          ds
     d   dbrrnf              397    400B 0
     d ef7fds          ds
     d   ef7rrn              397    400B 0
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkco#           s                   like(efco#)
     D kkfnd           s                   like(effnd)
     D kkfein          s                   like(effein)
     D kkfyr           s                   like(effyr)
     D kkmod           s                   like(efmod)
     D kkcase          s                   like(efcase)
     D kkseq#          s                   like(efseq#)
     D kkefdt          s                   like(efefdt)
     D kkname          s                   like(efpnam)
     D kkemp#          s                   like(efemp#)
     D kkdiv           s                   like(efdiv)
     D xxrrn1          s              9  0 Inz(0)
     D xxrrn           s              9  0 Inz(0)
     D xx1940          s              8  0 Inz(19700101)
      //*** beg add ***** 05/20/21 *************************************
       Dcl-S intSysDt Packed(7:0) inz;
      //*** end add ***** 05/20/21 *************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
    
     D@@fein           DS
     D#1fein                         12
     Dxxfein                          9  0 overlay(#1fein)
     Dxxmod                           3  0 overlay(#1fein : 10)
     D/copy sprWrkWth
     D/copy sprRunCmd
     D/copy sprMessage
     D/copy sprString
     D/copy sprDate
     D/copy sprCompany
      *** beg add ***** 05/20/21 **********************************************
      /Copy wprEmplr
      *** end add ***** 05/20/21 **********************************************
     C/COPY SCOPYMT41C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@type            1
     C     xxrrn         parm                    @@rrn
     C     xxrrn1        parm                    @@rrn1            9 0
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*  to retrieve date in DTMDYY format
    C*
     C     dtmdyp        plist
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C     dtmdyp2       plist
     C                   PARM                    SRDTI             8 0
     C                   PARM      *blanks       SRFFMT            8
     C                   PARM      *blanks       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C     dtymdp        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *blanks       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1

      *** beg add ***** 07/01/10 ***********************************************
     C     dtymdp2       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
      *** beg add ***** 07/01/10 ***********************************************
     C     dtcmdp        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *blanks       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C     dtcmdp2       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
    C*================================================================
    C*  KLIST definitions
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C     acky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C     ackynm        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkname
    C* key list for claims file
     C     cmky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C     aaky          klist
     C                   kfld                    ##co#
     C                   kfld                    s1fnd
     C                   kfld                    s1agt#


     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    s1fnd
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C     efky7         klist
     C                   kfld                    kktype            1
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkkey            30
     C                   kfld                    kkefdt
     C     efky          klist
     C                   kfld                    kktype            1
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkkey            30
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C                   if        *in40 = *on                                  add mode
     c                             and not *in51
     C                   select
     c                   when      *in41                                        type = 'C'
     C* Validate group
     C                   if        scfnd = 0
     C                   eval      *in91 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C**** validate policy year
     C                   if        scfyr = 0
     C                   eval      *in92 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C* Validate Claim number
     C                   if        sccase = 0
     C                   eval      *in93 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C* validate SSN#
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scss#'       qqname           25
     C     scss#         parm      scss#         qqval           100
     C                   if        scss# = *blanks
     C                   eval      *in96 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
     C*
     C* Dependent name Mandatory for Dependent
     C                   if        *in43
     C*
     C                   if        scdnam= *blanks
     C                   eval      *in80 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  scdnam = *blanks
     C*
     C*** validate Dependent
     C                   eval      kkname = scdnam
      *
     C     ackynm        chain     pacnm
     C                   if        not %found(wdacl1)
     C                   eval      *in80 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   else
     C                   if        acpnam <> scdnam
     C                   eval      *in80 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  acpnam<>scdnam
     C                   eval      scseq# = acseq#
     C                   endif                                                  not %found(wdacl1)
     C                   endif                                                  *IN43
     C*
     C                   eval      kkseq# = scseq#
     C*
     C  N43cmky1         chain     pcm01
     C   43acky          chain     pac01
     C                   if        not %found
     C                   eval      *in78 = *on
     C                   move      *blanks       scclmn
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   if        cmss# <> scss#
     C                   eval      *in79 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   when      *in44                                        type = 'E'
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = s1fnd
     C                   eval      kkemp# = scemp#
     C                   eval      kkdiv = scdiv
     C*
     c     emky          chain     pem01
     c                   if        not(%found)
     c                   seton                                        65  90
     C                   leavesr
     c                   else
     c                   eval      scempn = emname
     c                   eval      s1fein = emfein
     c                   eval      s1mod  = *zero
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     c                   endif                                                  not(%found)

      *** beg add **** 05/20/21 ***************************
       // Default starting policy year if adding and
       //  starting policy year not populated.
       If scSFyr = 0;
          intSysDt = %Dec(%Date():*Cymd);
          Setgt (kkCo#:kkFnd:kkEmp#:kkDiv:intSysDt) pel02;
          Readpe (kkCo#:kkFnd:kkEmp#:kkDiv) pel02;
          Select;
          When Not %Eof;
             scSFyr = ELfyr;
          When %Eof;
             Setll (kkCo#:kkFnd:kkEmp#:kkDiv:intSysDt) pel02;
             Reade (kkCo#:kkFnd:kkEmp#:kkDiv) pel02;
             If Not %Eof;
                scSFyr = ELfyr;
             Endif;
          Endsl;
       Endif;
      *** end add **** 05/20/21 ***************************

     C                   when      *in45                                        type = 'P'
     C* Validate group
     C                   if        s1fnd = 0
     C                   eval      *in64 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
      *
     c     fnky          chain     pfn01
     C                   if        not %found
     C                   eval      *in64 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
      *
     c                   eval      s1fndm = fnname
      *
     C** validate payee and its fed ID#
     C                   if        scfein = 0
     C                   eval      *in94 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   eval      kkfein = scfein
     C                   eval      kkmod  = scmod
     C     pdky          chain     wmpdl
     C                   if        not %found
     C                   eval      *in81 = *on
     C                   eval      *in82 = *on
     C                   move      *blanks       scpnam
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     c                   when      *in42                                        type = 'A'

     c     aaky          chain     paa0l1
     c                   if        not(%found)
     c                   seton                                        74  90
     C                   leavesr
     c                   else
     c                   eval      s1agtn = aaname
     c                   eval      s1fein = aafein
     c                   eval      s1mod  = aamod
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     c                   endif

     c                   endsl
     C                   endif                                                  end of add mode

       // Start edits for updated records
     C                   select
     C                   when      scefdt = 0
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   when      scefdt <> 0 and
     C                             scefdt <> dtMDYY(EFEFDT)
     C                   eval      scefdt = DtCheck(scefdt : '*mdy')
     C                   if        scefDt = *zero
     C                   eval      *in71 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     c                   endif
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxeftd            8 0
     C                   if        srerr <> '0'
     c                             or xxeftd < xx1940
     C                   eval      *in71 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   else
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxeftd            8 0
     c                   select
     c                   when      @@type = 'E'                                 Employer Type
      *** beg del ***** 05/20/21 **************************
       // These edits removed.  EFT eff date can precede inception date
       //  and the 114WYI implementing Agent type Employer EFT records implemented
       //  has been removed.
      ****
      **** Get the Policy Inception Date
      ****
     c***                   eval      kkco# = ##co#
     c***                   eval      kkfnd = s1fnd
     c***                   eval      kkemp# = scemp#
     c***                   eval      kkdiv = scdiv
     c***     emky          chain     pem01
     c***                   z-add     EMEFD1        srdti
     c***                   call      'SRCVTDAT'    dtymdp2
     c***                   movel     srdto         xxdate            8 0
      ****
      **** Flag error if Eff Date less than Policy Inception Date
      ****
     c***                   if        xxeftd < xxdate
     c***                   eval      *in70 = *on
     c***                   eval      *in90 = *on
     c***                   leavesr
     c***                   endif
     c***                   if        sceatp <> 'AG' and sceatp <> '  '
     c***                   seton                                        46  90
     C***                   leavesr
     c***                   endif                                                  sceatp <> 'AG'
      *** end del ***** 05/20/21 **************************
     c                   other
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtymdp
     c                   movel     srdto         xxdate            8 0
     c                   if        xxeftd < xxdate
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   endsl
     C                   endif
     C                   endsl
     C                   select
     C                   when      scexdt <> 0 and
     C                             scexdt <> dtMDYY(EFEXDT)
     C                   eval      scexdt = DtCheck(scexdt : '*mdy')
     C                   if        scexdt = *zero
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     c                   endif
     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxexpd            8 0
     C                   if        srerr <> '0'
     c                             or xxexpd < xx1940
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   else
     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxexpd            8 0
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxdate            8 0
     C                   if        (xxexpd < xxeftd) or  (xxexpd < xxdate)
     C                   eval      *in85 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   endif
     C                   endsl
     C*
     C* Pre note date
     C*
     C                   if        scpdt <> 0 and
     C                             scpdt <> dtMDYY(EFPDT)
     C                   eval      scpdt = DtCheck(scpdt : '*mdy')
     C                   if        scpdt = *zero
     C                   eval      *in66 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     c                   endif
     C                   z-add     scpdt         srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxpndd            8 0
     C                   if        (srerr <> '0' )
     c                             or xxpndd < xx1940
     C                   eval      *in66 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   MOVEL     SRDTO         xxdate            8 0
     C                   if        (xxpndd < xxdate)
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  xxpndd < xxdate
     C                   endif                                                  scpdt <> 0
     C*
     C* approval date
     C*
     C                   if        scadt <> 0 and
     C                             scadt <> dtMDYY(EFADT)
     C                   eval      scadt = DtCheck(scadt : '*mdy')
     C                   if        scadt = *zero
     C                   eval      *in67 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     c                   endif
     C                   z-add     scadt         srdti
     C                   call      'SRCVTDAT'    dtymdp
     C                   movel     srdto         xxappd            8 0
     C                   if        (srerr <> '0')
     c                             or xxappd < xx1940
     C                   eval      *in67 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   if        (xxappd < xxdate)
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  xxappd < xxdate
     C* Pre-note date should be non-zero for approval date to be entered
     C                   if        scpdt = 0
     C                   eval      *in87 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  scpdt=0
     C                   endif                                                  scadt <> 0
     C*
      *** additional edits
      *** first edit for duplicate record
      *
      * Check for overlapping date ranges
     c                   eval      kkco# = ##co#
     c                   z-add     scefdt        srdti
     c                   call      'SRCVTDAT'    dtcmdp
     c                   movel     srdto         kkefdt
     c                   z-add     scexdt        srdti
     c                   call      'SRCVTDAT'    dtcmdp
     c                   movel     srdto         x7exdt            7 0
     c                   exsr      srBldKey
     c     efky          setll     pef0l7
     c     efky          reade     pef0l7
     c                   dow       not(%eof)
     c                   select
     c                   when      *in40                                        Edit added records
     c                   select
      *** beg add ***** 05/20/21 **************************
       // Allow overlapping date ranges if starting policy years
       //  are different. SCSFYR is populated only for employer
       //  type EFT.
     c                   when      l7EFsfyr <> scSFyr
      *** end add ***** 05/20/21 **************************
     c                   when      l7EFEFDT = kkefdt
     c                             and l7EFEXDT <> l7EFEFDT
     c                   seton                                        7590
     c                   leavesr
     c                   when      l7EFEFDT < kkefdt
     c                             and (l7EFEXDT =  0 or l7EFEXDT > kkefdt)
     c                   seton                                        7590
     c                   leavesr
     c                   when      l7EFEFDT > kkefdt
     c                             and l7EFEFDT <> l7EFEXDT
     c                   seton                                        7590
     c                   leavesr
     c                   endsl
     c                   other                                                  Edit updated records
     c                   select
     c                   when      ef7rrn   = @@rrn
     c                   when      kkefdt   = x7exdt
     c                   when      l7EFEFDT = l7EFEXDT
      *** beg add ***** 05/20/21 **************************
       // Allow overlapping date ranges if starting policy years
       //  are different. SCSFYR is populated only for employer
       //  type EFT.
     c                   when      l7EFsfyr <> scSFyr
      *** end add ***** 05/20/21 **************************
     c                   when      kkefdt  <= l7EFEFDT
     c                             and (x7exdt > l7EFEFDT or x7exdt = 0)
     c                   seton                                        7590
     c                   leavesr
     c                   when      kkefdt > l7EFEFDT
     c                             and kkefdt < l7EFEXDT
     c                   seton                                        7590
     c                   leavesr
     c                   endsl
     c                   endsl
     c     efky          reade     pef0l7
     c                   enddo
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   setoff                                       414243
     C                   setoff                                       4445
     C                   if        *in40 = *off
     C                   eval      ##co# = efco#
     C                   select
     C                   when      eftype = 'P'
     C                   eval      scfein =  effein
     C                   eval      scmod =  efmod
     C                   eval      kkfein = effein
     C                   eval      kkmod = efmod
     C                   eval      scpnam =  efpnam
     C                   eval      *in45 = *on
     c                   eval      s1fnd  = effnd
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     C                   when      eftype = 'C' or eftype = 'D'
     C                   eval      *in41 = *on
     C                   eval      scfnd = effnd
     C                   eval      scfyr = effyr
     C                   eval      sccase = efcase
     C                   eval      scss#  = %char(effein)
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scss#'       qqname           25
     C     scss#         parm      scss#         qqval           100
     C                   eval      kkfyr = effyr
     C                   eval      kkcase = efcase
     C                   eval      kkco# = efco#
     C                   eval      scclmn =  efpnam
     C                   eval      kkfnd = effnd
     C                   if        eftype = 'D'
     C                   eval      *in43 = *on
     C                   eval      scdnam =  efpnam
     C     cmky1         chain     pcm01
     C                   if        %found
     C                   eval      scclmn = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C                                    + ' ' + %trim(cmmid)
     C                   endif                                                  %found
     C                   endif                                                  eftype = 'D'
     c                   when      eftype = 'A'
     C                   eval      *in42  = *on
     c                   eval      s1fnd  = effnd
     c                   eval      s1agt# = efagt#
     c                   eval      s1fein = effein
     c                   eval      s1mod  = efmod
     c                   eval      s1agtn = efpnam
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     c                   when      eftype = 'E'
     C                   eval      *in44  = *on
     c                   eval      s1fnd  = effnd
     c                   eval      scemp# = efemp#
     c                   eval      scdiv  = efdiv
     c                   eval      s1fein = effein
     c                   eval      s1mod  = efmod
     c                   eval      scempn = efpnam
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     C                   endsl
     C                   z-add     efefdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scefdt
     C                   move      *blanks       SRDTO
     C                   z-add     efexdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scexdt
     C                   move      *blanks       SRDTO
     C                   z-add     efpdt         srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scpdt
     C                   move      *blanks       SRDTO
     C                   z-add     efadt         srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scadt
     C                   move      *blanks       SRDTO
     C                   z-add     efftdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scftdt
     C                   move      *blanks       SRDTO
     C                   z-add     efltdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scltdt
     C                   move      *blanks       SRDTO
     c                   eval      sceatp = efeatp
     C                   Eval      scEobType = EFeobT
     C                   else
     C                   select
     C                   when      sctype = 'P'
     C                   eval      *in45 = *on
     C                   when      sctype = 'E'
     C                   eval      *in44 = *on
     C                   when      sctype = 'D'
     C                   eval      *in41 = *on
     C                   eval      *in43 = *on
     C                   when      sctype = 'C'
     C                   eval      *in41 = *on
     C                   when      sctype = 'A'
     C                   eval      *in42 = *on
     C                   endsl
     C                   Eval      scEobType = ' '
     C                   endif                                                  *in40 = *off
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      efco# =  ##co#
     C                   move      *blanks       srdto
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel     srdto         efefdt
     C                   move      *blanks       srdto
     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel     srdto         efexdt
     C                   z-add     scpdt         srdti
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel     srdto         efpdt
     C                   z-add     scadt         srdti
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel     srdto         efadt
     C                   if        *in40 = *on
     C                   select
     C                   when      *in41                                        type = 'C'/'D'
     C                   eval      effnd =  scfnd
     C                   eval      effyr =  scfyr
     C                   eval      efcase =  sccase
     C                   movel     scss#         effein
     C                   eval      efpnam = scclmn
     C                   eval      efmod = 0
     C                   eval      eftype = 'C'
     C                   movel(p)  scfyr         xxwk10           10
     C                   move      sccase        xxwk10
     C                   movel(p)  xxwk10        efkey
     C                   if        *in43                                        type = 'D'
     C                   eval      efpnam = scdnam
     C                   eval      eftype = 'D'
     C                   eval      efseq# = scseq#
     c                   movel     xxwk10        xxwk15           15
     c                   move      scseq#        xxwk15
     c                   movel(p)  xxwk15        efkey
     C                   endif                                                  *in43
     C                   when      *in45                                           type = 'P'
     C                   movel(p)  scfein        xxwk12           12
     C                   move      scmod         xxwk12
     C                   movel(p)  xxwk12        efkey
     c                   eval      effnd  = s1fnd
     C                   eval      effein = scfein
     C                   eval      efmod  = scmod
     C                   eval      efpnam = scpnam
     C                   eval      eftype = 'P'
     C                   eval      efseq# = 0
     C                   when      *in42                                        type = 'A'
     c                   eval      effnd  = s1fnd
     c                   eval      efagt# = s1agt#
     c                   eval      effein = s1fein
     c                   eval      efmod  = s1mod
     c                   eval      eftype = sctype
     c                   eval      efpnam = s1agtn
     C                   movel(p)  s1agt#        efkey
     C                   when      *in44                                        type = 'E'
     c                   eval      effnd  = s1fnd
     c                   eval      efemp# = scemp#
     c                   eval      efdiv  = scdiv
     c                   eval      effein = s1fein
     c                   eval      efmod  = s1mod
     c                   eval      eftype = 'E'
     c                   if        sceatp = 'AG'
     c                   clear                   #2ANAM
      *  GET Agent/Producer Information
     C                   CALL      'WA002R'
     C                   PARM      efco#         #2CO#             3 0
     C                   PARM      effnd         #2FND             3 0
     C                   PARM      efemp#        #2EMP#            9 0
     C                   PARM      efdiv         #2DIV             5 0
     C                   PARM      0             #2FYR             3 0
     C                   PARM                    #2AGT#            5 0
     C                   PARM                    #2ANAM           60
     C                   PARM                    #2PROD            5 0
     C                   PARM                    #2PNAM           60
     C                   eval      efpnam = #2ANAM
     C                   eval      efagt# = #2AGT#
     c                   else
     c                   eval      efpnam = scempn
     c                   endif
     C                   movel(p)  scemp#        xxwk14           14
     C                   move      scdiv         xxwk14
     C                   movel(p)  xxwk14        efkey
     c     efkey         cat       sceatp:0      efkey
     c                   endsl
     C                   move      *blanks       eftrcd
     C                   move      *blanks       efpre
     C                   move      *blanks       efid#
     C                   move      *blanks       efdgit
     C                   move      *blanks       efacct
     C                   move      *blanks       efstat
     C                   eval      efipgm = q1pgm
     C                   eval      efiusr = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         efidt
     C                   eval      efitm = dstime
     C                   endif                                                  *in40 = *on
     c                   eval      efeatp = sceatp
     C                   Eval      EFeobT = scEobType
     C                   eval      efpgm = q1pgm
     C                   eval      efuser = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         efdt
     C                   eval      eftime = dstime
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C***
     C* Process search key
     C                   select
     C                   when      *inkj = *on
     C                   exsr      kjdo
     C                   endsl
     C***
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   setoff                                       414243
     C                   setoff                                       444551
     C*** END   ADD ****** 06/29/09 ************************************
     C                   select
     C*
      *** beg chg ***** 09/20/07 ***********************************************
     C*****************  when      @@mode = 'A'
     C                   when          @@mode = 'A'
     C                             and @@type = *blank
      *** end chg ***** 09/20/07 ***********************************************
     C                   eval      *in40 = *on
     C                   eval      sctype = *blanks
     C                   eval      *in74 = *off
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     D*** BEG CHG ****** 11/15/07 ************************************
     C*****************  eval      xxco# = upco#
     C                   eval      ##co# = upco#
     D*** END CHG ****** 11/15/07 ************************************
     C                   endif                                                  %found
     C**** show up a window asking for claimant/provider/agent type
     C                   dow       *inkl = *off
     C                   exfmt     scr04
     C                   select
     C                   when      *inkl = *on
     C                   eval      *inkc = *off
     C                   return
     C                   when      (sctype <> 'C')  and (sctype <> 'P')
      *** beg add ***** 09/20/07 ***********************************************
     c                             and sctype <> 'A'
      *** end add ***** 09/20/07 ***********************************************
     c*** BEGIN ADD ****** 06/29/09 ************************************
     c                             and sctype <> 'D' and sctype <> 'E'
     c*** END   ADD ****** 06/29/09 ************************************
     C                   eval      *in74 = *on
     C                   when      (sctype = 'C')  or (sctype = 'P')
      *** beg add ***** 09/20/07 ***********************************************
     c                             or sctype =  'A'
      *** end add ***** 09/20/07 ***********************************************
     c*** BEGIN ADD ****** 06/29/09 ************************************
     c                             or sctype = 'D' or sctype = 'E'
     c*** END   ADD ****** 06/29/09 ************************************
     C                   eval      *in74 = *off
     C                   leave
     C                   endsl
     C                   enddo
     C*
      *** beg add ***** 09/20/07 ***********************************************
     c                   when          @@mode = 'A'
     c                             and @@type <> *blank
     c                   eval      sctype = @@type
     c                   eval      *in40 = *on

     c                   select
     c                   when      sctype = 'A'
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c*************      if        @@fnd  <> *zeros and
     c*************                @@agt# <> *zeros
     c*************      eval      *in51 = *on
     c*************      eval      s1fnd  = @@fnd
     c*************      eval      s1agt# = @@agt#
     c*****aaky          chain     paa0l1
     c*
     c                   if        xxrrn1 <> *zeros
     c     xxrrn1        chain     paa01p
     c*
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif                                                  not(%found)
     c*
     c                   eval      *in42=*on
     c                   eval      *in51 = *on
     c                   eval      ##co#  = aaco#
     c                   eval      s1fnd  = aafnd
     c                   eval      s1agt# = aaagt#
     c*
     c*** END   CHG ****** 06/29/09 ************************************
     c                   eval      s1agtn = aaname
     c                   eval      s1fein = aafein
     c                   eval      s1mod  = aamod
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     c                   endif

     c                   when      sctype = 'C'
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c**************     if            @@clm  <> *zeros
     c**************               and @@clm  <> *blanks
     c**************     eval      *in51 = *on
     C**************     eval      scfnd = xxfnd
     C**************     eval      scfyr = xxfyr
     C**************     eval      sccase = xxcase
     C**************     eval      kkfyr = xxfyr
     C**************     eval      kkcase = xxcase
     C**************     eval      kkco# = ##co#
     C**************     eval      kkfnd = xxfnd
     C*****cmky1         chain     pcm01
     c*
     c                   if        xxrrn1 <> *zeros
     c     xxrrn1        chain     pcm01p
     c*
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif                                                  not(%found)
     c*
     c                   eval      *in41 = *on
     c                   eval      *in51 = *on
     C                   eval      scfnd = cmfnd
     C                   eval      scfyr = cmfyr
     C                   eval      sccase = cmcase
     C*
     c                   eval      ##co#  = cmco#
     C                   eval      kkco# = cmco#
     C                   eval      kkfnd = cmfnd
     C                   eval      kkfyr = cmfyr
     C                   eval      kkcase = cmcase
     c*** END   CHG ****** 06/29/09 ************************************
     C                   eval      scclmn = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C                                    + ' ' + %trim(cmmid)
     C                   eval      scss#  = cmss#
     C******  Begin Add  ***  06/09/20  **************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scss#'       qqname           25
     C     scss#         parm      scss#         qqval           100
     C******  End Add  ***  06/09/20  ****************
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c                   endif                                                  xxrrn1 <> *zeros
     c*
     c                   when      sctype = 'D'
     c                   if        xxrrn1 <> *zeros
     c     xxrrn1        chain     pac01p
     c*
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif                                                  not(%found)
     c*
     c                   eval      *in41 = *on
     c                   eval      *in43 = *on
     c                   eval      *in51 = *on
     c*
     C                   eval      scfnd = acfnd
     C                   eval      scfyr = acfyr
     C                   eval      sccase = accase
      *** beg add ***** 07/12/10 *************************************
     C                   eval      scseq# = acseq#
      *** end add ***** 07/12/10 *************************************
     C                   eval      scdnam = acpnam
     C                   eval      scclmn = *blanks
     C*
     c                   eval      ##co#  = acco#
     C                   eval      kkco# = acco#
     C                   eval      kkfnd = acfnd
     C                   eval      kkfyr = acfyr
     C                   eval      kkcase = accase
     C                   eval      kkseq# = acseq#
     C*
     C     cmky1         chain     pcm01
     C                   if        %found
     C                   eval      scclmn = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C                                    + ' ' + %trim(cmmid)
     C                   eval      scss#  = cmss#
     C******  Begin Add  ***  06/09/20  **************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scss#'       qqname           25
     C     scss#         parm      scss#         qqval           100
     C******  End Add  ***  06/09/20  ****************
     C                   endif                                                  %found
     C
     C*** END   CHG ****** 06/29/09 ************************************
     c                   endif

     c*** BEGIN CHG ****** 06/29/09 ************************************
     c                   when      sctype = 'E'
     c                   if        xxrrn1 <> *zeros
     c     xxrrn1        chain     pem01p
     c*
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif                                                  not(%found)
     c*
     c                   eval      *in44 = *on
     c                   eval      *in51 = *on
     c*
     C                   eval      s1fnd = emfnd
     C                   eval      scemp# = ememp#
     C                   eval      scdiv = emdiv
     C*
     C                   eval      scempn = emname
     C                   eval      s1fein = emfein
     C                   eval      s1mod = *zeros
     C*
     c                   eval      ##co#  = emco#
     C                   eval      kkco# = emco#
     C                   eval      kkfnd = emfnd
     C                   eval      kkemp# = ememp#
     C                   eval      kkdiv = emdiv
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     c*
     c                   endif                                                  xxrrn1 <> *zeros
     c*** END   CHG ****** 06/29/09 ************************************

     c                   when      sctype = 'P'
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c****************** if            @@fein  <> *zeros
     c**************     eval      *in51 = *on
     C**************     movel(p)  xxfein        kkfein
     C**************     movel(p)  xxmod         kkmod
     C*****pdky          chain     wmpdl
     c*
     c                   if        xxrrn1 <> *zeros
     c     xxrrn1        chain     ppd01p
     c*
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif                                                  not(%found)
     c*
     c                   eval      *in45 = *on
     c                   eval      *in51 = *on
     C                   movel(p)  pdfein        kkfein
     C                   movel(p)  pdmod         kkmod
     c*** END   CHG ****** 06/29/09 ************************************
     C                   movel(p)  pdname        scpnam
     C                   movel(p)  pdfein        scfein
     C                   movel(p)  pdmod         scmod
     c                   endif

     c                   endsl

     c                   other
     c                   eval      *in51 = *on
      *** end add ***** 09/20/07 ***********************************************
     C*
     C                   endsl
     C*** Begin Add *** 05/06/15 ***********************************************
     C*
     C* Check to see if provider type
     C*
     C                   Eval      isPrvType = *Off
     C                   If        @@type = 'P'
     C                   Eval      isPrvType = *On
     C                   EndIf
     C*** End   Add *** 05/06/15 ***********************************************
     C*
     C                   endsr                                                  pgmstr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C**** processing for 10 option, it will have @@mode = 'S'
     C**** clear pre note in the DB if either no approval date or date of first transfer exist
     C**** and then update the file by calling updatr.
     C                   select
     C                   when      @@mode = 'S'
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   eval      efpdt = 0
     C                   eval      efpgm = q1pgm
     C                   eval      efuser = q1user
     C                   exsr      getdtm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         efdt
     C                   eval      eftime = dstime
     C                   update    dbrcdf
     C                   endif
     C                   return
     C                   endsl
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmefp
      *** beg del ***** 09/20/07 **************************************
     C*****************  select
     C*****************  when      sctype = 'P'
     C*****************  z-add     0             scfein
     C*****************  z-add     0             scmod
     C*****************  move      *blanks       scpnam
     C*****************  when      sctype = 'C'
     C*****************  z-add     0             scfnd
     C*****************  z-add     0             scfyr
     C*****************  z-add     0             sccase
     C*****************  move      *blanks       scclmn
     C*****************  move      *blanks       scss#
     C*****************  move      *blanks       scdnam
     C*****************  endsl
     C*****************  z-add     0             scefdt
     C*****************  z-add     0             scexdt
      *** end del ***** 09/20/07 **************************************
      *** beg add ***** 09/20/07 **************************************
     c                   eval      outflg = *on
      *** end add ***** 09/20/07 **************************************
     C                   endsr
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr
     C***
     C                   select
     C*
     C                   when      hfld = 'SCFND' or
     C                             hfld = 'SCFYR' or
     C                             hfld = 'SCSS#' or
     C                             hfld = 'SCCASE'
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                             or hfld = 'SCDNAM'
     C*** END   ADD ****** 06/29/09 ************************************
     C                   call      'W0070R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm      *blanks       ##kc              1
     C     *inkl         parm      *blanks       ##kl              1
     C                   parm      ' '           ##mode            1
     C                   parm      *zeros        ##rrn             9 0
     C                   parm      'CN'          ##sort            2
     C                   parm      *zeros        ##rrn2            9 0
     C                   parm      *zeros        ##fyr             3 0
     C*
     C                   if        *inkl <> *on and ##rrn <> *zeros
     C     ##rrn         chain     pcm01p
     C                   if        %found
     D*** BEG CHG ****** 11/15/07 ************************************
     C****************** eval      xxco# = cmco#
     C                   eval      ##co# = cmco#
     D*** END CHG ****** 11/15/07 ************************************
     C                   eval      scfnd = cmfnd
     C                   eval      scfyr = cmfyr
     C                   eval      sccase = cmcase
     C                   eval      scss#  = cmss#
     C******  Begin Add  ***  06/09/20  **************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scss#'       qqname           25
     C     scss#         parm      scss#         qqval           100
     C******  End Add  ***  06/09/20  ****************
     C                   eval      scclmn = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C                                    + ' ' + %trim(cmmid)
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C*************      seton                                        25
     C*
     C* if *in43, search dependent
     C                   if        *in43 = *on
     C                   call      'W0573R'
     C                   parm      '2'           ##call            1
     C                   parm      'Y'           ##pmtf            1
     C     *inkc         parm      *blanks       ##kc              1
     C     *inkl         parm      *blanks       ##kl              1
     C                   parm      ##rrn         ##rrn             9 0
     C*
     C                   if        *inkl <> *on and ##rrn <> *zeros
     C     ##rrn         chain     pac01p
     C                   if        %found
     C                   movel     acpnam        scdnam
     C                   eval      scseq# = acseq#
     C                   seton                                        26
     C                   else
     C                   seton                                        25
     C                   endif                                                  %found
     C*
     C                   else
     C                   seton                                        24
     C                   endif                                                  *inkl <> *on
     C                   endif                                                  *in43
     C*** END   CHG ****** 06/29/09 ************************************
     C                   endif                                                  %found
     C                   else
     C                   seton                                        21
     C                   endif                                                  *inkl <> *on
     C*
     c*** BEGIN DEL ****** 06/29/09 ************************************
     C**** for searching dependent
     C***************    when      hfld = 'SCDNAM'
     C***************    If        sccase <> 0
     C***************    call      'W0573R'
     C***************    parm      '2'           ##call            1
     C***************    parm      'Y'           ##pmtf            1
     C**** *inkc         parm      *blanks       ##kc              1
     C**** *inkl         parm      *blanks       ##kl              1
     C***************    parm      ##rrn         ##rrn             9 0
     C***************
     C***************    if        *inkl <> *on and ##rrn <> *zeros
     C**** ##rrn         chain     pac01p
     C***************    if        %found
     C***************    movel     acpnam        scdnam
     C***************    seton                                        26
     C***************    endif                                                  %found
     C***************    else
     C***************    seton                                        24
     C***************    endif                                                  *inkl <> *on
     C***************    endif
     c*** END   DEL ****** 06/29/09 ************************************
     C*
     C**** payee and its fed id
     C                   when      hfld = 'SCFEIN' or
     C                             hfld = 'SCMOD'
     C                   call      'W0030R'
     C                   parm      '2'           ##call            1
     C                   parm      *blanks       ##fein            9
     C                   parm      *blanks       ##mod             3
     C                   parm      *blanks       ##rtcd            1
     C                   movel     ##fein        kkfein
     C                   movel     ##mod         kkmod
     C*
     C     pdky          chain     wmpdl
     C                   if        %found
     C                   movel     pdname        scpnam
     C                   movel     pdfein        scfein
     C                   movel     pdmod         scmod
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C*************      seton                                            26
     C                   seton                                            29
     C*** END   CHG ****** 06/29/09 ************************************
     C                   else
     C                   seton                                            28
     C                   endif                                                  %found
      *** beg add ***** 09/20/07 ******************************************************
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C                   when          (hfld = 'S1FND' and *in45)
     C*
     C                   move      ##co#         #1co#
     C                   call      'W0010R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c                   parm                    #1co#             3
     c                   parm      *blanks       #1fnd             3
     c*
     c                   if        #1fnd <> *blanks
     c                   move      #1fnd         s1fnd
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     C                   seton                                            26
     C                   else
     C                   seton                                            29
     C                   endif                                                  #1fnd <> *blanks
     C*
     C***************    when          (hfld = 'S1FND' or  hfld = 'S1AGT#')
     C                   when          (hfld = 'S1FND' and *in42)
     C                                 or hfld = 'S1AGT#'
     C*
     C                   if        s1fnd = *zeros
     C*
     C                   move      ##co#         #1co#
     C                   call      'W0010R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c                   parm                    #1co#             3
     c                   parm      *blanks       #1fnd             3
     c*
     c                   if        #1fnd <> *blanks
     c                   move      #1fnd         s1fnd
     C                   endif                                                  #1fnd <> *blanks
     c                   endif                                                  s1fnd = *zeros
     c*
     c                   if        s1fnd <> *zeros
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     C*** END   CHG ****** 06/29/09 ************************************

     c                   call      'W0040R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c*****************  parm      *zeros        ##rrn
     c                   parm      dbrrnf        ##rrn
     c*** END   CHG ****** 06/29/09 ************************************

     c                   if        ##rrn <> 0
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c**** ##rrn         chain     WMAGP
     c***************    eval      s1fein = agfein
     c***************    eval      s1mod  = agmod
     c*****aaky1         chain     paa0l
     c***************    eval      s1fnd  = aafnd
     c*
     c     ##rrn         chain     paa01p
     c                   eval      s1fein = aafein
     c                   eval      s1mod  = aamod
     c*** END   CHG ****** 06/29/09 ************************************
     c                   eval      s1agtn = aaname
     c*** BEGIN DEL ****** 06/29/09 ************************************
     c*****fnky          chain     pfn01
     c*************      eval      s1fndm = fnname
     c*** END   DEL ****** 06/29/09 ************************************
     c                   eval      s1agt# = aaagt#
     c                   eval      *in88 = *on
     c                   endif
      *** end add ***** 09/20/07 ******************************************************
     c*** BEGIN ADD ****** 06/29/09 ************************************
     c                   endif                                                  s1fnd <> *zeros
     c*
     C                   when          (hfld = 'S1FND' and *in44)
     C                                 or hfld = 'SCEMP#' or hfld = 'SCDIV'
     c                   if        s1fnd = *zero                                search if fund = 0
     c                   call      'W0050R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c                   parm      0             ##rrn
     c                   parm      0             ##rrn2            9 0
     c                   parm      *blanks       ##qte             1
     c                   else                                                   search if fund <> 0
     c                   eval      scfnd = s1fnd
     c     fnky          chain     pfn01
     c                   call      'W0050R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c                   parm      0             ##rrn
     c                   parm      dbrrnf        ##rrn2
     c                   parm      *blank        ##qte
     c                   endif                                                  s1fnd = 0
      *
     c                   if        ##rrn <> 0                                   if rcrd is selected
     c     ##rrn         chain     pem01p
     c                   if        %found
     c                   eval      s1fnd = emfnd                                load screen fields
     c                   eval      scemp# = ememp#
     c                   eval      scdiv = emdiv
     c                   eval      scempn = emname
     c                   eval      s1fein = emfein
     c     fnky          chain     pfn01
     c                   eval      s1fndm = fnname
     c                   seton                                        26        position to
     c                   endif                                                  not 60
     c                   else
     c                   if        *inkl = *on                                  position to scemp
     c                   seton                                        66        if F12 is selected
     c                   endif
     c                   endif                                                  qqrrn <> 0
     c*** END   ADD ****** 06/29/09 ************************************
     C                   endsl
     C                   seton                                        90
     C
     C                   endsr
    C*====================================================================
    C* srBldKey - Build record key
    C*====================================================================
     C     srBldKey      begsr
     C*
     C                   select
     C                   when      *in41 or *in43                               C or D
     C                   movel(p)  scfyr         xxwk10           10
     C                   move      sccase        xxwk10
     C                   movel(p)  xxwk10        kkkey
     C                   eval      kktype = 'C'
     C                   eval      kkfnd = scfnd
     C                   if        *in43                                        D
     C                   move      scseq#        kkkey
     C                   eval      kktype = 'D'
     C                   endif                                                  *in43
     C*
     C                   when      *in42                                        A
     C                   movel(p)  s1agt#        kkkey
     C                   eval      kktype = 'A'
     C                   eval      kkfnd = s1fnd
     C*
     C                   when      *in44                                        E
     C                   movel(p)  scemp#        xxwk14           14
     C                   move      scdiv         xxwk14
     C                   movel(p)  xxwk14        kkkey
     c     kkkey         cat       sceatp:0      kkkey
     C                   eval      kktype = 'E'
     C                   eval      kkfnd = s1fnd
     C*
     C                   when      *in45                                        P
     C                   movel(p)  scfein        xxwk12           12
     C                   move      scmod         xxwk12
     C                   movel(p)  xxwk12        kkkey
     C                   eval      kktype = 'P'
     C                   eval      kkfnd = s1fnd
     C                   endsl
     C*
     C                   endsr
