     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
      ****************************************************************
      ** Program Description:                                       **
      **  This program reads WABLP records and lists employers that **
      **  were "skipped" in mass billing because the ACH info or    **
      **  Vendor number was missing.                                **
      **                                                            **
      ** Called from: W8002C                                        **
      **                                                            **
      ** Calls to:                                                  **
      **                                                            **
      ** Programmer: C: 132                                         **
      ** Created:    06/17/03  PROJECT: 129WBW                      **
      ****************************************************************
      **  Revisions:                                                **
      **                                                            **
      **  06/05/14 C:65  Project: 129WSB (Redmine #4246)            **
      **                 Change system flag for EFT check.          **
      **                                                            **
      **  11/22/13 C:279 Project: 129WSB                            **
      **                 Modified to list employers which do not    **
      **                 have EFT setup and billing schedule code   **
      **                 required EFT only.                         **
      **                                                            **
      **  11/17/04 C:136 Support: WA129 44654                       **
      **                 Correct program so that it can check       **
      **                 policies in future.                        **
      **                                                            **
      **  01/13/04 C:136 Project: 129WEJ                            **
      **                 Update program to use date ranges in       **
      **                 the Direct Debit file and Vendor History   **
      **                 file to ascertain policy status in regards **
      **                 to ACH or Payroll activity.                **
      ****************************************************************

     fwabll     uf   e           k disk
     fwmrpl     if   e           k disk
     fwmfnl     if   e           k disk
     fwmeml     if   e           k disk
     fwdell     if   e           k disk
     fwmdbl3    if   e           k disk
     fwdvhl1    if   e           k disk
     fsmcol     if   e           k disk
     fssylf     if   e           k disk    ignore(ssy01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
     f                                     ignore(sdr01)
     f                                     ignore(sfb01)
     f                                     ignore(sgl01)
     f                                     ignore(spc01)
     f                                     ignore(sp201)
     f                                     ignore(src01)
     fwtabl     if   e           k disk    ignore(pat01)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcn01)
     f                                     ignore(pcs01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f                                     ignore(pna01)
     f                                     ignore(pnc01)
     f                                     ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
     fqsysprt   o    f  132        printer oflind(*INOA) usropn

     d psds          esds                  extname(szq1p)
     d fids01        e ds                  extname(szq2p)

     d                 DS
     d  DSdttm                 1     14  0
     d  DStime                 1      6  0
     d  DSdate                 7     14  0

     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d xxefdt          s              8  0
     d xxco#           s              3  0
     d xxfnd           s              3  0
     d xxKEY           s             10
     d ##data          s             80
     d ##leng          s              3  0
     d emcnt           s              7  0
     d sysDate7        s              7  0
     d kkDate          s              7  0
      ****** beg add ******* 11/22/13 ***********************************
     d outCo#          s              3
     d outFnd#         s              3
     d outEftType      s              1    inz('E')
     d outEftKey       s             30    inz
     d outDate         s              7
     d outPreNote      s              1
     d isEftFlg        s              1
     d isEftSetUp      s              1
     d Title           c                   CONST('ACH/EFT Only/Payroll Billing -
     d                                     Plan Exception Report')
      ****** end add ******* 11/22/13 ***********************************

      ****** beg del ******* 11/22/13 ***********************************************
     d***** Title           c                   CONST('ACH/Payroll Billing Plan Exce-
     d*****                                     ption Report')
      ****** end del ******* 11/22/13 ***********************************************

      ***** beg add ***** 11/22/13 *************************
      /copy wprBank
      /copy wprEft
      ***** end add ***** 11/22/13 *************************
      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next Request Payment Transaction record

     c     blky          setll     pbl01
     c     blky          reade     pbl01                                  LR

     c                   dow       *inLR=*off

      * If dont have Group Vendor Flag set or ACH flag set, bypass this group, and exit pgm.
      ********* beg chg ********* 11/22/13 *************************************************
     c*********          if        vndrfl = *off and achflg = *off

      /free
       // If dont have EFT installed, bypass this group, and exit pgm.
       If vndrFl = *Off and achFlg = *Off and isEFTFlg = *Off;
      /end-free
      ********* end chg ********* 11/22/13 *************************************************
     c                   goto      Term
     c                   endif

      * Check the employer detail billing schedule code for either a T (twice Monthy) or
      * a "W" (Weekly). If is not either, bypass record and go read another one.
      *
     c                   move      *off          vdrpol            1
     c     elky          chain     pel01                              60
     c                   if        NOT *in60
     c                   movel(p)  elbscd        xxkey
     c     tbky          chain     pbs01                              60
     c                   if        not *in60 AND bsfreq <> 'T' AND
     c                             bsfreq <> 'W'
     c                   goto      nxtrcd

     c                   else

     c                   eval      biltype = *Blanks
     c                   select
     c                   when      bsfreq = 'T'
     c                   eval      biltype = 'Twice Weekly '
     c                   when      bsfreq = 'W'
     c                   movel(P)  'Weekly'      biltype          14
     c                   other
     c                   movel(p)  'Error Freq'  biltype
     c                   endsl
     c                   endif                                                  not 60, not T or W
      *
      * Else check for Payroll Vendor
     c                   if        vndrfl = *on
     c     vhky          setgt     pvh01
     c     elky          readpe    pvh01
     c                   if        not %eof and vhexdt = 0 or
     c                             not %eof and sysDate7 < vhexdt
     c                   eval      vdrpol = *ON
     c                   endif                                                  vndrfl = *on
     c                   endif                                                  Not %eof
     c                   endif                                                  NOT *IN60

     c     emky          chain     pem01                              60
     c                   if        not *in60
     c                   movel     emname        xxname           25

      * Check for ACH Employer
     c                   move      *off          achemp            1
     c                   if        achflg = *on
     c     dbky          setgt     pdb01
     c     emky          readpe    pdb01
     c                   if        not %eof and dbexdt = 0 or
     c                             not %eof and dbexdt > sysDate7
     c                   eval      achemp = *ON
     c                   endif                                                  not %eof
     c                   endif                                                  ACHFLG = *ON
     c                   endif                                                  not 60  elky

      /free
       //****** beg chg ********* 11/22/13 *************************************************
       // Check if Employer did not have EFT setup and billing schedule code required EFT only

       isEftSetUp = *Off;
       If isEFTFlg = *On;
          isEftSetUp = *On;
          outEftKey = %Editc(BLemp#:'X') + %Editc(BLdiv:'X') + '  ';
          outDate = %Editc(sysDate7:'X');
          outCo#  = %editc(BLco#:'X');
          outFnd# = %editc(BLfnd:'X');
          If not GetEftStatus(outCo#:OutEftType:outFnd#:outEftKey:outDate:
           outPreNote);

             If ELbscd <> *zero;
                xxKey = %char(ELbscd);
                Chain (BLco#:xxKey) pbs01;
                If %found and BSeftf = 'Y';
                   isEftSetUp = *Off;
                Endif;
             Endif;
          Endif;
       Endif;

       // If don't have ACH or EFT Only or Payroll vendor for this record, print message
       If achEmp = *Off And vdrPol = *Off and isEftSetUp = *Off;

       //* If don't have ACH or Payroll vendor for this record, print message
       //c                   if        achemp = *off AND vdrpol = *off
      /end-free
      ********* end chg ********* 11/22/13 *************************************************
      * If first print, open the printer file and set on flag
     c                   if        not *in68
     c                   open      qsysprt
     c                   except    head
     c                   eval      *IN68 = *on
     c                   endif                                                  not *IN68
      *
     c   OA              except    head
     c                   eval      emcnt = emcnt + 1
     c                   except    det
     c                   delete    pbl01
     c                   endif                                                  not achemp or vdrpol

     c     nxtrcd        tag
      *
     c     blky          reade     pbl01                                  LR
     c                   enddo
      *
      ** ==================================================================== **
      ** Termination & exit, Check to see if need to print total line
      ** ==================================================================== **
      *
     c     TERM          tag
      *
     c                   move      *ON           *INLR
      *
     c                   if        emcnt > *zeros OR *IN68 = *ON
     c                   except    totl
     c                   close     qsysprt
     c                   endif
      *
      ** ==================================================================== **
      ** *INZSR - Initalize subroutine
      ** ==================================================================== **
      *
     c     *inzsr        begsr
      *
      ** =============== **
      *  Parameter Lists  *
      ** =============== **
      *
     c     *entry        plist
     c                   parm                    qqco#             3            Company Number
     c                   parm                    qqfnd             3            Group Number

     c                   move      qqco#         xxco#
     c                   move      qqfnd         xxfnd

     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
      ** =============== **
      *  Key Field Lists  *
      ** =============== **

     c     blky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

     c     coky          klist
     c                   kfld                    xxco#

     c     dbky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv
     c                   kfld                    kkdate
      *
     c     vhky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv
     c                   kfld                    blfyr
     c                   kfld                    kkdate

     c     elky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv
     c                   kfld                    blfyr

     c     emky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv

     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

     c     rpky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv
     c                   kfld                    blfyr

     c     tbky          klist
     c                   kfld                    blco#
     c                   kfld                    xxkey

     c     syky          klist
     c                   kfld                    xxsyk             2
      *
      ** ========================= **
      *  Other First Time processes *
      ** ========================= **
      *
     c                   time                    dsdttm
     c                   eval      kkdate = *hival
      *
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysDate7
      *
      *  Center Company name in heading field
     c     coky          chain     pco01                              60
     c*
     c                   movel(p)  coname        ##data
     c                   call      'SRCENTER'                           99
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel(p)  ##data        coname
      *
      *  Get Company Flags for ACH/Payroll Vendor
      *
     c                   move      *off          achflg            1
     c                   move      *off          vndrfl            1
      *
     c                   eval      xxsyk  = 'WC'
     c     syky          chain     swc01                              60
      *
     c                   if        wcfl23 = 'Y'
     c                   move      *on           vndrfl
     c                   endif
      *
     c                   if        wcfl24 = 'Y'
     c                   move      *on           achflg
     c                   endif
      /free
       //***** beg add ******** 11/22/13 **************************************
       isEftFlg = *off;
       //*** begin chg ***  06/05/14  *****************************************
       //******If WCfl35 = 'Y';
       If WCfl24 = 'Y' Or WCfl38 = 'Y';
       //*** end chg *****  06/05/14  *****************************************
          isEftFlg = *on;
       Endif;
       //***** end add ******** 11/22/13 **************************************
      /end-free
      *
      *  Get Fund name
     c                   move      *blanks       xxfnam
     c     fnky          chain     pfn01                              60
     c  n60              movel(P)  fnname        xxfnam           60
     c                   endsr
      *
      ** ==================================================================== **
      *
     oqsysprt   e            head           1 06
     o                       dstime               8 '  :  :  '
     o                       title               73
     o                       dsdate             130 '  /  /    '
      *
     o          e            head           2
     o                       q1pgm               10
     o                       coname              73
     o                                          125 'PAGE'
     o                       PAGE               130
     o          e            head           2
     o                                            6 'Group:'
     o                       blfnd               10
     o                       xxfnam              72
     o          e            head           2
     o                                            9 'Employer'
     o                                           19 'Division'
     o                                           26 'Name'
     o                                           68 'Year'
     o                                           82 'Billing Plan'

     o          e            det            1
     o                       blemp#        3      9
     o                       bldiv         3     18
     o                       emname              62
     o                       blfyr               67
     o                       biltype             84

     o          e            totl        1  1
     o                                           24 'Total Employers Listed:'
     o                       emcnt         3     32

