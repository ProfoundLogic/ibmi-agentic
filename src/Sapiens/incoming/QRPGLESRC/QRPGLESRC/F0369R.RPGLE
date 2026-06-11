     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H**************************************************************************************
     H* H/TITLE - IRF calculation display program                                          *
     H*                                                                                    *
     H* DESCRIPTION: This pgm shows the detailed calculation derived from FMSKP,FMRGP etc  *
     H*                                                                                    *
     H* CALLED BY: F0363R                                                                  *
     H*                                                                                    *
     H* CALLS TO : None                                                                    *
     H*                                                                                    *
     H**************************************************************************************
     H*  Created: 10/17/11   Programmer C:251   Project:114MAR                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     Ff0369f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Ffaeul     if   e           k disk
     Ffdcal24   if   e           k disk
     Ffdcal26   if   e           k disk    rename(pca01:pca02)
     Ffaeolg    if   e           k disk
     Fftrtl2    if   e           k disk
     Ffmmgl1    if   e           k disk
     Ffddul     if   e           k disk
     Ffmirl1    if   e           k disk
     Fststl1    if   e           k disk
      *
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     D OPTNM           C                   CONST('IRF Payment Information')
     D kkffdt          s                   like(irffdt)
     D kkprov          s                   like(irprov)
     D kkcbsa          s                   like(iracbs)
     D kktier          s                   like(mgtier)
     D kkcode          s                   like(mgcode)
     D xxcode          s                   like(mgcode)
     D xxchar          s              1
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
     D srdti           s              8  0
     D srdto           s             10
     D srffmt          s              8
     D srtfmt          s              8
     D srerr           s              1
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
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    #1dseq
     C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    #1dseq
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
     C     irky          klist
     C                   kfld                    kkprov
     C                   kfld                    kkffdt
     C*
     C     irky1         klist
     C                   kfld                    kkprov
     C*
     C     iwky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    kkefdt
     C*
     C     iwky1         klist
     C                   kfld                    kkcbsa
     C*
     C     mgky          klist
     C                   kfld                    kkcode
     C                   kfld                    kktier
     C                   kfld                    kkefdt
     C*
     C     mgky1         klist
     C                   kfld                    kkcode
     C                   kfld                    kktier
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *Blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
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
     C                   parm                    #1dseq            5 0
     C*
     C*----------------------------------------------------------------------
     C*  MAINLINE CODE
     C*----------------------------------------------------------------------
     C*
     C                   exsr      pgmstr
     C                   exsr      mvdbsc
     C                   exfmt     Scr01
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
     C                   eval      scwind = eowind
     C                   eval      scast = eoast
     C                   movel     eoast         kkast            10
     C     stky          chain     pst01
     C                   if        %found
     C                   movel(p)  stname        scstds
     C                   endif
     C                   endif
     C*
     C*          Get from FAEUP
     C*          ------------------------------------------------------
     C*
     C     euky          chain     peu01
     C                   if        %found
     C                   eval      scprov = eumcar
     C                   eval      kkprov = eumcar
     C                   move      euaddt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         scaddt
     C*
     C                   move      eudcdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         scdcdt
     C*
     C                   endif
     C*
     C*          Get FDCAP - CMS and total payments
     C*          ------------------------------------------------------
     C*
     C     caky          chain     pca01
     C                   if        %found
     C                   eval      sctch  = catch
     C                   eval      scirfb = cairfb
     C                   eval      scoutl = caoutl
     C                   eval      sclip  = calip
     C                   eval      scpaid = capaid
     C                   move      cafdat        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         scefdt
     C                   endif
     C*
     C                   eval      scfaca = 0
     C                   eval      sctota = sctch + scirfb + scoutl + scfaca
     C                                      + sclip
     C*
     C*
     C*          Get from FMIRP
     C*          ------------------------------------------------------
     C                   move      cafdat        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         kkffdt
     C*
     C     irky          setgt     pir01
     C     irky1         readpe    pir01
     C                   if        not %eof
     C*
     C                   eval      scoccr = iroccr
     C                   eval      scssi  = irssi
     C                   eval      scmrat = irmrat
     C                   eval      sccbsa = iracbs
     C*
     C                   move      irefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         kkefdt
     C                   endif
     C*
     C* Get code, relative weight, length iof stay
     C*          Get FDDUP
     C*          ------------------------------------------------------
     C*
     C                   eval      xxcode = *blanks
     C*
     C     duky          chain     pdu01
     C                   if        %found
     C*
     C                   exsr      GetTier
     C*
     C                   movel     cafdat        kkefdt
     C                   eval      kkcode = durate
     C*
     C     mgky          setgt     Pmg01
     C     mgky1         readpe    Pmg01
     C*
     C                   exsr      GetCode
     C*
     C                   endif
     C*
     C*          Get FTRTP
     C*          ------------------------------------------------------
     C*
     C                   Movel     'MC'          kkst
     C                   Movel     'R'           kktype
     C*
     C     rtky          setgt     prt01
     C     rtky1         readpe    prt01
     C*
     C                   dow       not %eof
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        not %eof
     C                   eval      scbncv = rtbncv
     C                   eval      sclbp  = rtilpc
     C                   eval      scnlbp = rtinpc
     C                   endif
     C*
     C* Get from state FTRTP, value of state multiplier
     C*
     C                   Movel     eoast         kkst
     C                   Movel     'R'           kktype
     C*
     C     rtky          setgt     prt01
     C     rtky1         readpe    prt01
     C*
     C                   dow       not %eof
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        not %eof
     C                   eval      scmult  = rtmul1 * 100
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* GetTier- Get the tier of the durate code
     C*================================================================
     C     GetTier       Begsr
     C*
     C*** Get Tier (first char of CMG code from file FDDUP)
     C*
     C                   eval      xxchar = *blanks
     C                   eval      xxchar = %subst(durate:1:1)
     C                   select
     C                   when      xxchar = 'A'
     C                   eval      kktier = '0'
     C*
     C                   when      xxchar = 'B'
     C                   eval      kktier = '1'
     C*
     C                   when      xxchar = 'C'
     C                   eval      kktier = '2'
     C*
     C                   when      xxchar = 'D'
     C                   eval      kktier = '3'
     C*
     C                   endsl
     C                   endsr
     C*================================================================
     C* GetCode - Get the code of the durate code
     C*================================================================
     C     GetCode       Begsr
     C*
     C* Calculate for logic as to whether short stay is applicable or not
     C*
     C                   If        (duunts < mgalos and (eupats = '02' or
     C                             eupats = '03' or eupats = '61' or
     C                             eupats = '62' or eupats = '63' or
     C                             eupats = '64' ))
     C                             or
     C                             ((eupats <> '20' and eupats <> '40' and
     C                             eupats <> '41' and eupats <> '42') and
     C                             (durate <> 'A0901' and durate <> 'A0902'
     C                             and durate <> 'A0903' and durate <> 'A0904')
     C                             and  duunts > 3 )
     C*
     C                   movel     durate        scrate
     C                   eval      screlw = mgrelw
     C                   eval      scalos = mgalos
     C                   eval      scdesc = mgdesc
     C*
     C                   else
     C*
     C** Its a short stay code
     C*
     C                   select
     C* For days <= 3 (unconditional short stay case)
     C*
     C                   when      duunts <= 3
     C*
     C                   if        eupats = '20' or eupats = '40' or
     C                             eupats = '41' or eupats = '42'
     C                   eval      xxcode = 'A5101'
     C                   else
     C                   eval      xxcode = 'A5001'
     C                   endif
     C*
     C* Code for expired patient status and orthopedic case
     C*
     C                   when      (eupats = '20' or eupats = '40' or
     C                             eupats = '41' or eupats = '42') and
     C                             (durate = 'A0901' or durate = 'A0902' or
     C                              durate = 'A0903' or durate = 'A0904')
     C*
     C                   if        duunts <= 13
     C                   eval      xxcode = 'A5101'
     C                   elseif    duunts >= 14
     C                   eval      xxcode = 'A5102'
     C                   endif
     C*
     C* Code for expired patient status and non orthopedic case
     C*
     C                   when      (eupats = '20' or eupats = '40' or
     C                             eupats = '41' or eupats = '42') and
     C                             (durate <> 'A0901' and durate <> 'A0902'
     C                              and durate <> 'A0903' and durate <> 'A0904')
     C*
     C                   if        duunts <= 15
     C                   eval      xxcode = 'A5103'
     C                   elseif    duunts >= 16
     C                   eval      xxcode = 'A5104'
     C                   endif
     C*
     C                   endsl                                                  for short stay
     C*
     C*
     C*
     C* Get relative weight from FMMGP for the overriding code
     C*
     C                   eval      kkcode = xxcode
     C                   eval      kktier = '0'
     C                   movel     xxcode        scrate
     C
     C*          Get from FMMGP
     C*          ------------------------------------------------------
     C*
     C     mgky          setgt     pmg01
     C     mgky1         readpe    pmg01
     C                   if        not %eof
     C                   eval      screlw = mgrelw
     C                   eval      scalos = mgalos
     C                   eval      scdesc = mgdesc
     C                   endif
     C*
     C                   endif
     C                   endsr
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     Pgmstr        Begsr
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
