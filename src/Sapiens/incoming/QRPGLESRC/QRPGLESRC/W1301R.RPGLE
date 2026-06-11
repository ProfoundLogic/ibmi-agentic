    Hcopyright ('(C) Copyright Fiserv, Inc., 2003 - 2006')
    H*****************************************************************
    H/TITLE W1301R - LOCKBOX PAYMENT UPLOAD INFO.
    H*****************************************************************
    H* Description:  This program will display a list of payment     *
    H*   transaction records.                                        *
    H*                                                               *
    H* Called from:  W1300R                                          *
    H* Calls to:     none                                            *
    H*                                                               *
    H* Created by:  C:65     Date: 06/05/03   Project: 129WAD        *
    H*****************************************************************
    H* Revisions:                                                    *
    H*                                                               *
    H* 02/17/06  C:33  Project: 71WGB   WA071/51420                  *
    H*                 Display invalid data.                         *
    H*                                                               *
    H* 08/02/05  C:33  Project: 71WGB                                *
    H*                -Moved to CLT071                               *
    H*                -Convert to RPGIV                              *
    H*****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw1301f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxlup     if   e             disk    rename(plu01:pfrcdf)
    F*
     Fwxlul2    if   e           k disk    rename(plu01:dbrcdf)
     F                                     infds(dbfds)
     F***  Begin Add  ***  02/17/06  **************************************
     FwxlupInv  if   e             disk    rename(wxlupinp:plu0pinv)
     F                                     usropn prefix(inv)
     F***  End   Add  ***  02/17/06  **************************************
    F*
     D hd              s             27    dim(2) ctdata perrcd(1)
    D*
     D #ofd            c                   10
     D of              s              1    dim(#ofd) ctdata perrcd(1)
     D ofd             s             35    dim(#ofd) alt(of)
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('Lockbox Payment Upl-
     D                                     oad Info    ')
     D slmsgi          c                   const('WC39998')
     D slmsgf          c                   const('WMESGG')
     D acdpgm          c                   const('NONE')
     D comapi          c                   const('WA001R')
     D chgnum          c                   const('WA003R')
     D uspapi          c                   const('W0006R')
     D infomx          c                   const(2)
     D viewmx          c                   const(1)
     D sf1pag          c                   const(14)
    D*
     D LUSCNL          ds            80
     D  xxtype                        1    overlay(LUSCNL)
     D***  Begin Chg  ***  02/17/06  **************************************
     D**xxfnd                         3  0 overlay(LUSCNL:2)
     D**xxemp#                        9  0 overlay(LUSCNL:5)
     D**xxdiv                         5  0 overlay(LUSCNL:14)
     D**xxfyr                         3  0 overlay(LUSCNL:19)
     D**xxamt                        11  2 overlay(LUSCNL:22)
     D**xxfyr2                        3  0 overlay(LUSCNL:33)
     D**xxamt2                       11  2 overlay(LUSCNL:36)
     D  xxfnd                         3    overlay(LUSCNL:2)
     D  xxemp#                        9    overlay(LUSCNL:5)
     D  xxdiv                         5    overlay(LUSCNL:14)
     D  xxfyr                         3    overlay(LUSCNL:19)
     D  xxamt                        11    overlay(LUSCNL:22)
     D  xxfyr2                        3    overlay(LUSCNL:33)
     D  xxamt2                       11    overlay(LUSCNL:36)
     D***  End   Chg  ***  02/17/06  **************************************

      ***  Begin Add  ***  02/17/06  **************************************
     DinvWXLUP         ds
     D invCo#                         3    overlay(invWXLUP)
     D invChk#                        7    overlay(invWXLUP:4)
     D invCkAm                        9    overlay(invWXLUP:11)
     D invCkDt                        8    overlay(invWXLUP:20)
     D invScnL                       80    overlay(invWXLUP:28)
     D invType                        1    overlay(invWXLUP:108)
     D  invTypeScnL                   1    overlay(invScnL)
     D  invFnd                        3    overlay(invScnL:2)
     D  invEmp#                       9    overlay(invScnL:5)
     D  invDiv                        5    overlay(invScnL:14)
     D  invFyr                        3    overlay(invScnL:19)
     D  invAmt                       11    overlay(invScnL:22)
     D  invFyr2                       3    overlay(invScnL:33)
     D  invAmt2                      11    overlay(invScnL:36)
      ***  End   Add  ***  02/17/06  **************************************

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
     D @@co#           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkchk#          s                   like(LUCHK#)
     D kkckam          s                   like(LUCKAM)
     D kkckdt          s                   like(LUCKDT)
     D kkco#           s                   like(LUCO#)
     D kkscnl          s                   like(LUSCNL)
     D kktype          s                   like(LUTYPE)
      ***  Begin Add  ***  02/17/06  **************************************
     D size            s              3  0
      ***  End   Add  ***  02/17/06  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
      ***  Begin Add  ***  02/17/06  **************************************
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

     DCvtNum           pr                  extPgm('WA003R')
     D number                        80
     D editMask                      10

     DEditCode         pr            25    opdesc
     D alpha                         15    options(*varsize) const
     D code                           1                      const
      ***  End   Add  ***  02/17/06  **************************************

     D/copy sprWrkWth
     D/copy sprRunCmd
     D/copy sprMessage
     D/copy sprString
     D/copy sprDate
     D/copy sprCompany
    C*
     C/COPY SCOPYWW41C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@co#
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
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkchk#
     C                   kfld                    kkckam
     C                   kfld                    kkckdt
     C                   kfld                    kkscnl
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
     C                   move      *loval        kktype
     C                   move      *loval        kkchk#
     C                   move      *loval        kkckam
     C                   move      *loval        kkckdt
     C                   clear                   kkscnl
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   select
    C*
     C                   when      *in21 = *on
     C                   eval      kktype = scpst1
     C                   eval      kkchk# = scpst1a
     C                   eval      kkckam = scpst1b
     C                   move      *loval        kkckdt
     C                   clear                   kkscnl
    C*
     C                   endsl
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kktype = sctype
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********         eval      kkchk# = scchk#
     C**********         eval      kkckam = scckam
     C**********         eval      kkckdt = scckdt
     C**********         move      scscnl        kkscnl
     C                   eval      kkchk# = sfchk#
     C                   eval      kkckam = sfckam
     C                   eval      kkckdt = sfckdt
     C                   eval      kkscnl = sfscnl
     C***  End   Chg  ***  02/17/06  **************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   eval      scType = LUTYPE
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********         eval      scChk# = LUCHK#
     C**********         eval      scCkAm = LUCKAM
     C**********         eval      scCkDt = LUCKDT
     C                   eval      sfChk# = LUCHK#
     C                   eval      sfCkAm = LUCKAM
     C                   eval      sfCkDt = LUCKDT
     C***  End   Chg  ***  02/17/06  **************************************
     C                   eval      scScNl = LUSCNL
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********         eval      scfnd  = xxfnd
     C**********         eval      scemp# = xxemp#
     C**********         eval      scdiv  = xxdiv
     C                   eval      scFnd  = EditCode(xxFnd:'3')
     C                   eval      scEmp# = EditCode(xxEmp#:'3')
     C                   eval      scDiv  = EditCode(xxDiv:'3')
     C***  End   Chg  ***  02/17/06  **************************************
     C*
     C***  Begin Add  ***  02/17/06  **************************************
     C                   eval      sfScnL = LUSCNL
     C*
     C                   if        LUTYPE = 'X'
     C                   open      WXLUPINV
     C     dbRRN         chain     WXLUPINV
     C                   eval      scScnL = invScnL
     C                   close     WXLUPINV
     C                   endif
     C*
     C                   select
     C                   when      LUCHK# = *zeros
     C                   eval      scChk# = *all'+'
     C                   other
     C                   eval      scChk# = %editc(LUCHK#:'3')
     C                   endsl
     C*
     C                   select
     C                   when      LUCKAM = *zeros
     C                   eval      size = %size(scCkAm)
     C                   eval      %subst(scCkAm:1:size-1) = *all'+'
     C                   other
     C                   eval      scCkAm = %editc(LUCKAM:'J')
     C                   endsl
     C*
     C                   select
     C                   when      LUCKDT = *zeros
     C                   eval      scCkDt = *all'+'
     C                   other
     C                   eval      scCkDt = %editw(LUCKDT:'  /  /    ')
     C                   endsl
     C***  End   Add  ***  02/17/06  **************************************
    C*
     C                   select
    C*
     C                   when      hd# = 1
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********         eval      scfyr = xxfyr
     C**********         eval      scamt = xxamt
     C                   eval      scFyr = EditCode(xxFyr:'3')
     C                   eval      scAmt = EditCode(xxAmt:'J')
     C***  End   Chg  ***  02/17/06  **************************************
     C                   eval      schd  = 'Current Year'
    C*
     C                   when      hd# = 2
     C***  Begin Chg  ***  02/17/06  **************************************
     C***********        eval      scfyr = xxfyr2
     C***********        eval      scamt = xxamt2
     C                   eval      scFyr = EditCode(xxFyr2:'3')
     C                   eval      scAmt = EditCode(xxAmt2:'J')
     C***  End   Chg  ***  02/17/06  **************************************
     C                   eval      schd  = ' Prior Year '
    C*
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
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
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   eval      kkco# = @@co#
     C*
     C***  Begin Add  ***  02/17/06  **************************************
     C                   eval      *in12 = *on
     C*
     C                   if        @@call = 'I'
     C                   eval      ssType = 'X'
     C                   eval      @@call = ' '
     C                   eval      *in43 = *on
     C                   endif
     C***  End   Add  ***  02/17/06  **************************************
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     ##co#         reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     ##co#         readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
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
     C*
     C                   select
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********         when      ssfnd <> *zero and ssfnd <> xxfnd
     C                   when      ssFnd <> *zero and
     C                             %editc(ssFnd:'X') <> xxfnd
     C***  End   Chg  ***  02/17/06  **************************************
     C                   eval      meetss = *off
     C                   when      ssfyr <> *zero and
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********                   hd# = 1 and ssfyr <> xxfyr
     C                             hd# = 1 and %editc(ssFyr:'X') <> xxfyr
     C***  End   Chg  ***  02/17/06  **************************************
     C                   eval      meetss = *off
     C                   when      ssfyr <> *zero and
     C***  Begin Chg  ***  02/17/06  **************************************
     C**********                   hd# = 2 and ssfyr <> xxfyr2
     C                             hd# = 2 and %editc(ssFyr:'X') <> xxfyr2
     C***  End   Chg  ***  02/17/06  **************************************
     C                   eval      meetss = *off
     C                   when      sstype <> *blank and sstype <> LUTYPE
     C                   eval      meetss = *off
     C                   endsl
     C*
     C                   endsr
    C*================================================================





      ***  Begin Add  ***  02/17/06  **************************************
     PEditCode         b                   export
     DEditCode         pi            25    opdesc
     D inAlpha                       15    options(*varsize) const
     D code                           1                      const

     D alpha           s                   inz like(inAlpha)
     D converted       s             25
     D cvtAlpha        s             80    inz
     D editMask        s             10
     D extra           s              3  0
     D num             s             15  0
     D sizeAlpha       s              3  0
     D sizeNum         s              3  0

     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   eval      sizeAlpha = inLen
     C                   eval      sizeNum   = %len(Num)

     C                   if        %subst(inAlpha:1:sizeAlpha) = *all'+'
     C                   select
     C                   when      code = '3'
     C                   return    %subst(inAlpha:1:sizeAlpha)
     C                   when      code = 'J'
     C                   eval      extra = sizeAlpha / 3
     C                   eval      %subst(alpha:1:sizeAlpha+extra) = *all'+'
     C                   return    alpha
     C                   when      code = 'X'
     C                   return    %subst(inAlpha:1:sizeAlpha)
     C                   endsl
     C                   endif

     C                   eval      alpha = %subst(inAlpha:1:sizeAlpha)
     C                   move      alpha         num
     C                   eval      cvtAlpha = alpha
     C*
     C                   select
     C                   when      code = '3'
     C                   eval      converted = %editc(num:'3')
     C                   when      code = 'J'
     C                   eval      editMask = 'X,XXX.XX'
     C                   callp     CvtNum(cvtAlpha:editMask)
     C                   movel     cvtAlpha      converted
     C                   when      code = 'X'
     C                   eval      converted = %editc(num:'X')
     C                   endsl
     C
     C                   return    converted
     PEditCode         e
      ***  End   Add  ***  02/17/06  **************************************





** Column headings - HD
Current Year     Amount Due
 Prior Year      Amount Due
** Description--------------MISN
1011=Select                 NNN
203F3=Exit                  YNN
209F9=Change Info           YNN
212F12=Cancel               YNN
215F15=Fold/Truncate        YYY                            02/17/06
217F17=Subset               YYY
3



