     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H**************************************************************************************
     H* H/TITLE - SNF calculation display program                                          *
     H*                                                                                    *
     H* DESCRIPTION: This pgm shows the detailed calculation derived from FMSKP,FMRGP etc  *
     H*                                                                                    *
     H* CALLED BY: F0363R                                                                  *
     H*                                                                                    *
     H* CALLS TO : None                                                                    *
     H*                                                                                    *
     H**************************************************************************************
     H*  Created: 10/06/11   Programmer C:251   Project:114MZO                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                               *
     H*  03/14/14  C:274 Project: FS427                               *
     H*                  Strip off leading "999" coming from          *
     H*                  FMSKP/KKCBSA                                 *
     H*                                                                                    *
     H**************************************************************************************
     Ff0368f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Ffaeul     if   e           k disk
     Ffdcal26   if   e           k disk
     Ffdcal24   if   e           k disk    rename (pca01 : pca02)
     Ffaeolg    if   e           k disk
     Fftrtl2    if   e           k disk
     Ffmskl1    if   e           k disk
     Ffmrgl1    if   e           k disk
     Ffddul     if   e           k disk
     Ffmwgl2    if   e           k disk
     Fststl1    if   e           k disk
     Ffmhpl1    if   e           k disk
      *
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     D OPTNM           C                   CONST('Skilled Nursing Facility Rate+
     D                                          s')
     D kkrvcd          s                   like(durvcd)
     D kksvcd          s                   like(casvcd)
     D kkdseq          s                   like(dudseq)
     D kkffdt          s                   like(skffdt)
     D kkprov          s                   like(skprov)
     D kkcbsa          s                   like(skacbs)
     D xxpaid          s                   like(capaid)
     D xxlen           s              3  0
     D xxcbs           s              2
     D xxcnt           s              1  0
     D kkco#           s              3  0
     D kksub#          s              5  0
     D kkfnd           s              3  0
     D kkfyr           s              3  0
     D kkcase          s              7  0
     D kkeob#          s              9  0
     D kkconm          s             40
     D kkst            s              2
     D kktype          s              1
     D kkefdt          s              7  0
     D*
     D*** Beg Add 03/14/14 *******************************************
     D                 ds
     D  xxCBSA                 1      5
     D  xxCBSA3                1      3
     D  xxCBSA2                4      5
     D*** End Add 03/14/14 *******************************************
     D*
     Dpsds           esds                  extname(SZQ1P)
     D*
     C     euky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     caky#         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kksvcd
     C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     C*
     C     stky          klist
     C                   kfld                    kkast
     C*
     C     rtky          KLIST
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C*
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C*
     C     skky          klist
     C                   kfld                    kkprov
     C                   kfld                    kkffdt
     C     skky1         klist
     C                   kfld                    kkprov
     C*
     C     wgky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    kkefdt
     C*
     C     wgky1         klist
     C                   kfld                    kkcbsa
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *Blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd1       PLIST
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      *Blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C*
     C     *entry        plist
     C                   parm                    kkco#
     C                   parm                    kksub#
     C                   parm                    kkfnd
     C                   parm                    kkfyr
     C                   parm                    kkcase
     C                   parm                    kkeob#
     C                   parm                    kkconm
     C*
     C*----------------------------------------------------------------------
     C*  MAINLINE CODE
     C*----------------------------------------------------------------------
     C*
     C                   if        *in99 = *off
     C*
     C                   exsr      pgmstr
     C                   exsr      mvdbsc
     C                   exfmt     Scr01
     C                   endif
     C*
     C                   move      *on           *inlr
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        Begsr
     C*
     C*          Get FAEOP
     C*          ------------------------------------------------------
     C*
     C     euky          chain     peo01
     C                   if        %found
     C                   eval      scast = eoast
     C                   movel     eoast         kkast            10
     C     stky          chain     pst01
     C                   if        %found
     C                   movel(p)  stname        scstds
     C                   endif
     C                   endif
     C*
     C*          Get from FAEUP   - Provider, From Date, Through date
     C*          ------------------------------------------------------
     C*
     C     euky          chain     peu01                              99
     C                   eval      scprov = eumcar
     C                   eval      kkprov = eumcar
     C*
     C                   move      euscpt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         kkffdt
     C*
     C                   move      euscpt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         scthdt
     C*          ------------------------------------------------------
     C*
     C*          Get FMSKP - CBSA code
     C*          ------------------------------------------------------
     C*
     C*** Beg Add 03/14/14 ********************************************
     C                   Eval      xxCBSA = *blanks
     C*** End Add 03/14/14 ********************************************
     C     skky          setgt     psk01                              99
     C     skky1         readpe    psk01                                  99
     C*
     C                   if        *in99 = *off
     C                   move      skefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   movel     srdto         scefdt
     C*
     C*** Beg Del 03/14/14 ********************************************
     C******             eval      sccbsa =  skacbs
     C*** End Del 03/14/14 ********************************************
     C*
     C*** Beg Add 03/14/14 ********************************************
     C                   Eval      xxCBSA = skacbs
     C                   if        xxCBSA3 = '999'
     C                   eval      xxCBSA3 = *blanks
     C                   eval      skacbs  = xxCBSA
     C                   endif
     C                   eval      sccbsa =  skacbs
     C*** End Add 03/14/14 ********************************************
     C                   eval      xxlen = %len(%trim(skacbs))
     C*
     C                   if        skacbs = '00000' or skacbs  = *blanks
     C                   eval      sccbsa = *blanks
     C                   eval      xxlen = 0
     C                   else
     C*
     C*          Get Wage Index from CBSA code file FMWGP
     C*          ------------------------------------------------------
     C*
B002 C                   if        xxlen > 2
     C                   eval      kkcbsa =  skacbs
     C                   else
     C                   move      skacbs        xxcbs
     C                   movel     xxcbs         kkcbsa
     C                   endif
     C*
     C                   Movel     euscpt        kkefdt
     C     wgky          setgt     pwg01
     C     wgky1         readpe    pwg01
B002 C                   if        not %eof
     C                   eval      scwind = wgwind
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C*          Get FTRTP
     C*          ------------------------------------------------------
     C*
     C                   Movel     'MC'          kkst
     C                   Movel     'S'           kktype
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C                   eval      scmult  = rtmul1 * 100
     C                   eval      sclbp   = rtsnfl
     C                   endif
     C*
     C* Get from state FTRTp, value of state multiplier
     C*
     C                   Movel     eoast         kkst
     C                   Movel     'S'           kktype
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C                   eval      scmult  = rtmul1 * 100
     C                   endif
     C*
     C*          Get FDCAP - CMS and total payments
     C*          ------------------------------------------------------
     C*
     C                   eval      kksvcd = 22
     C     caky#         setll     pca01
     C     caky#         reade     pca01
     C                   dow       not %eof
     C                   eval      xxpaid = capaid + xxpaid
     C     caky#         reade     pca01
     C                   enddo
     C*
     C                   eval      sctot =  xxpaid
     C                   if        rtmul1 <> *zeros
     C                   eval      sccms = xxpaid/rtmul1
     C                   else
     C                   eval      sccms = xxpaid
     C                   endif
     C*
     C*          Get FDDUP
     C*          ------------------------------------------------------
     C                   eval      xxcnt = 0
     C*
     C     caky          setll     pca02
     C     caky          reade     pca02
     C*
     C                   dow       not %eof and xxcnt < 4
     C*
     C                   eval      kkdseq = cadseq
     C*
     C** check if its a HIPPS code
     C*
     C     cacpt4        chain     php01
     C                   if        %found
     C                   eval      xxcnt = xxcnt + 1
     C*
     C                   select
     C*
     C                   when      xxcnt = 1
     C*
     C                   z-add     capaid        scpay1
     C                   eval      kkdseq = cadseq
     C     duky          chain     pdu01
     C                   if        %found
     C                   eval      scrug1 = durate
     C                   movel     duunts        scunt1
     C                   endif
     C*
     C                   when      xxcnt = 2
     C*
     C                   z-add     capaid        scpay2
     C                   eval      kkdseq = cadseq
     C     duky          chain     pdu01
     C                   if        %found
     C                   eval      scrug2 = durate
     C                   movel     duunts        scunt2
     C                   endif
     C*
     C                   when      xxcnt = 3
     C                   z-add     capaid        scpay3
     C                   eval      kkdseq = cadseq
     C     duky          chain     pdu01
     C                   if        %found
     C                   eval      scrug3 = durate
     C                   movel     duunts        scunt3
     C                   endif
     C*
     C                   when      xxcnt = 4
     C                   z-add     capaid        scpay4
     C     duky          chain     pdu01
     C                   if        %found
     C                   eval      scrug4 = durate
     C                   movel     duunts        scunt4
     C                   endif
     C*
     C                   endsl
     C*
     C                   endif
     C     caky          reade     pca02
     C*
     C                   enddo
     C*
     C                   endsr
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     Pgmstr        Begsr
     C*
     C** Clear work variables
     C*
     C                   clear                   xxpaid
     C                   clear                   kkrvcd
     C                   clear                   kkprov
     C                   clear                   kkffdt
     C                   clear                   srdti
     C                   clear                   srdto
     C*
     C                   movel(P)  kkconm        scconm
     C                   movel(P)  optnm         ##data
     C*
     C                   call      'SRCENTER'
     C                   parm                    ##data           80
     C                   parm      40            ##leng            3 0
     C*
     C                   movel     ##data        scprds
     C*
     C                   endsr                                                  pgmstr
