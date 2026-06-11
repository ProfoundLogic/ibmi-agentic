     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*/TITLE W1265R    -
    H*********************************************************************
    H*  Description: EFT File creation - Prompt program                  *
    H*                                                                   *
    H*  Called from: W7945R                                              *
    H*                                                                   *
    H*  Calls to: W1265C                                                 *
    H*                                                                   *
    H*  Created: C:191  Project: WC1806  Dated: 06/29/09                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  09/01/10  C:136  Project: WC1806 PRISS 139                                        *
    H*                   Remove Bank Account field from prompt. It is not needed.         *
    H*                                                                                    *
/* 07H*  07/29/10  C:180  SUPPORT 73695
    H*                                                                                    *
    H**************************************************************************************
     fw1265f    cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     fwmupl     if   e           k disk
      *** beg del ***** 09/01/10 *****************************************
     f*********** wsckl     if   e           k disk
      *** end del ***** 09/01/10 *****************************************

     d psds          esds                  extname(szq1p)
     d rstpgm          s              1

    c*================================================================
     c     *entry        plist
     c                   parm                    @@co#             3
     c                   parm                    @@kc              1
     c                   parm                    @@kl              1
     C***** BEGIN ADD  ******** 07/29/10 **********************************
     C                   parm                    @@call            1
     C***** END   ADD  ******** 07/29/10 **********************************
    c*================================================================
     c     upky          klist
     c                   kfld                    q1user
    c*
      *** beg del ***** 09/01/10 *************************************
    c*****ckky          klist
    c*********          kfld                    ##co#             3 0
    c*********          kfld                    scacct
      *** end del ***** 09/01/10 *************************************
    c*
    c*================================================================
     c                   movel     @@co#         ##co#
    c*
     c                   movel     q1user        scuser
     c     upky          chain     pup01
     c* Get company name
     c                   call      'WA001R'
     c                   parm                    ##co#             3 0
     c                   parm      *zeros        ##fnd             3 0
     c                   parm      *blanks       ##conm           40
     c                   parm      *blanks       ##fnam           40
     c*
     c                   movel     ##conm        scconm
     c*
     c* MAIN LOOP
     c                   dou       not *in90

     c                   exfmt     scr01

     c                   select
     c                   when      *inkj
     c                   exsr      kjdo
     c                   when      *inkc = *on or *inkl = *on
      *** beg add ***** 09/01/10 *************************************
     c                   eval      @@kc = *inkc
     c                   eval      @@kl = *inkl
      *** end add ***** 09/01/10 *************************************
     c                   leave
     c                   other
     c* check for errors
     c                   exsr      errck1
     c                   if        not *in90
     c* no errors found, call program
     c                   call      'W1265C'
     c                   parm                    @@co#
     c                   parm                    upoutq
      *** beg chg ***** 09/01/10 ******************************************
     c***********        parm                    scacct
     c                   parm                    ##acct           20
      *** end chg ***** 09/01/10 ******************************************
     c                   parm                    sctype
     c                   parm      'M'           ##mode            1
     C***** BEGIN ADD  ******** 07/29/10 **********************************
     c                   if        ##mode = 'S'
     C                   move      '1'           @@call
     c                   else
     c                   move      ' '           @@call
     c                   endif
     C***** END   ADD  ******** 07/29/10 **********************************
     c                   leave
     c                   endif                                                  not xxerror

     c                   endsl

     c                   enddo

     c                   move      *on           *inlr
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     kjdo          begsr
      *
      *** beg del ***** 09/01/10 *************************************
     C*********          select
     C*********          when      hfld = 'SCACCT'
     C*********          call      'W2422R'
     C*********          parm      *blank        ##kc              1
     C*********          parm      *blank        ##kl              1
     C*********          parm      '2'           ##call            1
     C*********          parm      *blanks       @@co#             3
     C*********          parm      *blanks       ##acct           20
     C*********          if        ##acct <> *blank
     C*********          eval      scacct = ##acct
     C*********          seton                                        72  90
     C*********          else
     C*********          seton                                        71  90
     C*********          endif
     C*********          endsl
      *** end del ***** 09/01/10 *************************************
      *
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     errck1        begsr
      *
     c                   setoff                                       515253
     c                   setoff                                       90
      *** beg del ***** 09/01/10 *************************************
     c* bank account should be valid, if entered
     c***********        if        scacct <> *blanks
     c*****ckky**        chain     pck01                              60
     c***********        if        *in60
     c***********        seton                                        9051
     c***********        leavesr
     c***********        endif                                                  *in60
     c**bank*account should be valid EFT type
     c***********        if        ckeftf = *blanks or ckidst = *zeros
     c***********                  or ckiorg = *zeros or ckordi = *zeros
     c***********                  or cksvcl = *zeros
     c***********        seton                                        9052
     c***********        leavesr
     c***********        endif                                                  EFT setup
     c***********        endif                                                  scacct <> *blanks
      *** end del ***** 09/01/10 *************************************
      *
     c* EFT Type must be A, C, E or P
     c                   if        sctype <> *blank and
     c                             sctype <> 'A' and
     c                             sctype <> 'C' and
     c                             sctype <> 'E'
     c                   seton                                        9053
     c                   leavesr
     c                   endif
      *
     C                   endsr
