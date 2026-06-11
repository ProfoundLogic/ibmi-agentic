     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title WA085R - API to update EFT payee name/FEIN on Master information change
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
      * CALLED FROM:                                                  *
      *  W0050R02 - Employer master information maintenance           *
      *  W0051R11 - Employer endorsements - employer name changes     *
      *  W0051R18 - Employer endorsements - employer federal id change*
      *  W7265R   - Claims transfer                                   *
      *  W0030R1  - Payee Master Maintenance                          *
      *  W4020R   - Agent federal id change                           *
      *  W0040R02 - Agent Master Information update/display           *
      *  W0070R07 - Claim Master - Claimant Info                      *
      *  W0573R02 - Additional Address Master Information             *
      *                                                               *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 12/27/11  C:282  Project: WC2005                      *
      *****************************************************************
      *  Revisions:
      *
      *****************************************************************
      * Agent Master File
     Fwmagp     if   e             disk    rename(pag01:dbrcdf)
      * Claim file
     Fwmcmp     if   e             disk    rename(pcm01:dbrcdf1)
      * Additional Address File
     Fwdacp     if   e             disk    rename(pac01:dbrcdf2)
      * Employer Master File
     Fwmemp     if   e             disk    rename(pem01:dbrcdf3)
      * Payee demographics master
     Fwmpdp     if   e             disk    rename(ppd01:dbrcdf4)
      * Agent Assignment File (CO#/FEIN/MOD/FND)
     Fwmaal     if   e           k disk
      * EFT Account File (TYPE/CO#/FND/AGT#)
     Fwmefl8    uf   e           k disk
      * EFT Account File (TYPE/CO#/FND/KEY/EFDT)
     Fwmefl7    uf   e           k disk    rename(pef01:pef02)
      * EFT Account File (CO#/FEIN/MOD/EFDT)
     Fwmefl2    uf   e           k disk    rename(pef01:pef03)
      *
     dxxagt            s              5  0
     dxxco#            s              3  0
     dxxpco#           s              3  0
     dxxfein           s              9  0
     dxxmod            s              3  0
     dxxfnd            s              3  0
     dsys_date         s               D   datfmt(*USA)
     dxxtime           s              6  0
     dxxdate           s              7  0
     dxxtype           s              1
     d
     dpsds           esds                  extname(szq1p)
     d*---------------------------------------------------------------------
     d* Data Structures
     d*---------------------------------------------------------------------
     d** Date & Time structure
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      * Agent assignment file wmaal
     c     aaky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfein
     c                   kfld                    xxmod
      *
      * EFT Account file wmefl8
     c     efky1         klist
     c                   kfld                    xxtype
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxagt
      *
      * EFT Account file wmefl7
     c     efky2         klist
     c                   kfld                    xxtype
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxkey
      *
      * EFT Account file wmefl2
     c     efky3         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfein
     c                   kfld                    xxmod
      *
     c     *entry        plist
     c                   parm                    qqtype            1            type of payee
     c                   parm                    qqrrn             9 0          relative record no.
     c                   parm                    qqrtn             1            return code
     c                   parm                    qqco#             3 0
     c****************************************************************
      * Main Logic
     c****************************************************************
     c                   eval      qqrtn  = *blanks
     c                   eval      xxpco# = 0
      *
     c                   if        %parms > 3
     c                   eval      xxpco# = qqco#
     c                   endif
      *
     c                   time                    dsdttm
     c                   move      *date         sys_date
     c     *cymd         move      sys_date      xxdate
     c                   eval      xxtime = dstime
      *
      * Access master information based on type of payee:
     c                   select
      * Agent,
     c                   when      qqtype = 'A'
     c                   exsr      srPrcAgt
      * Claimant,
     c                   when      qqtype = 'C'
     c                   exsr      srPrcClm
      * Dependent,
     c                   when      qqtype = 'D'
     c                   exsr      srPrcDep
      * Employer,
     c                   when      qqtype = 'E'
     c                   exsr      srPrcEmp
      * Provider,
     c                   when      qqtype = 'P'
     c                   exsr      srPrcPrv
      *
     c                   endsl
      *
     c                   eval      *inlr = *on
      ***********************************************************
      * srldmtfld - Subroutine to update maitenance fields
      *             of file WMEFP.
      ***********************************************************
     c     srldmtfld     begsr
     c                   eval      efuser = q1cusr
     c                   eval      efpgm  = q1pgm
     c                   eval      eftime = xxtime
     c                   eval      efdt   = xxdate
     c                   endsr
      ***********************************************************
      * srPrcAgt - Subroutine to process Agent
      ***********************************************************
     c     srPrcAgt      begsr
      *
     c                   eval      xxco#  = *zero
     c                   eval      xxfein = *zero
     c                   eval      xxmod  = *zero
     c                   eval      xxagt  = *zero
     c                   eval      xxfnd  = *zero
     c                   eval      xxtype = *blank
      *
     c                   eval      xxtype = qqtype
      * Access record using rrn from agent master file WMAGP.
     c     qqrrn         chain     dbrcdf                             60
      *
     c                   if        *in60 = *off
      * record found
     c                   eval      xxco#  = agco#
     c                   eval      xxfein = agfein
     c                   eval      xxmod  = agmod
      *
      * Read each record of agent assignment from WMAAL.
     c     aaky1         setll     paa01
     c     aaky1         reade     paa01                                  60
      *
     c                   dow       *in60 = *off
      * record found
      *
     c                   eval      xxtype = 'A'
     c                   eval      xxfnd = aafnd
     c                   eval      xxagt = aaagt#
      *
      * Read record to update the file WMEFL8 when type = 'A'
      *
     c     efky1         setll     pef01
     c     efky1         reade     pef01                                  60
      *
     c                   dow       *in60 = *off
     c                   eval      effein = xxfein
     c                   eval      efmod  = xxmod
     c                   eval      efpnam = agname
     c                   exsr      srldmtfld
     c                   update    pef01
     c     efky1         reade     pef01                                  60
     c                   enddo                                                  *in60 = *off
      *
      * Read record to update the file WMEFL8 when type = 'E'
     c                   eval      xxtype = 'E'
      *
     c     efky1         setll     pef01
     c     efky1         reade     pef01                                  60
      *
     c                   dow       *in60 = *off
     c                   if        efeatp = 'AG'
     c                   eval      efpnam = agname
     c                   exsr      srldmtfld
     c                   update    pef01
     c                   endif                                                  efeatp = 'AG'
     c     efky1         reade     pef01                                  60
     c                   enddo                                                  *in60 = *off
      *
      * Read the next record from file WMAAL.
     c     aaky1         reade     paa01                                  60
     c                   enddo                                                  dow  *in60 = *off
      *
     c                   else
     c                   eval      qqrtn = 'X'
      *
     c                   endif                                                  *in60 = *off
      *
     c                   endsr
      ***********************************************************
      * srPrcClm - Subroutine to process Claimaint
      ***********************************************************
     c     srPrcClm      begsr
      *
     c                   eval      xxco#  = *zero
     c                   eval      xxfnd  = *zero
     c                   eval      xxwk10 = *blanks
     c                   eval      xxkey  = *blanks
     c                   eval      xxtype = *blank
      *
     c                   eval      xxtype = qqtype
      * Access record using rrn from agent master file WMCMP
     c     qqrrn         chain     dbrcdf1                            60
      *
     c                   if        *in60 = *off
      * record found
     c                   eval      xxco#  = cmco#
     c                   eval      xxfnd  = cmfnd
     c                   movel(p)  cmfyr         xxwk10           10
     c                   move      cmcase        xxwk10
     c                   movel(p)  xxwk10        xxkey            30
      *
      * Read record to update the file WMEFL7 when type = 'C'
      *
     c     efky2         setll     pef02
     c     efky2         reade     pef02                                  60
      *
     c                   dow       *in60 = *off
     c                   eval      efpnam = %trim(cmlsn) + ', ' + %trim(cmfsn)
     c                                      + ' ' + %trim(cmmid)
     c                   movel     cmss#         effein
     c                   exsr      srldmtfld
     c                   update    pef02
     c     efky2         reade     pef02                                  60
     c                   enddo                                                  *in60 = *off
      *
     c                   else
     c                   eval      qqrtn = 'X'
      *
     c                   endif                                                  *in60 = *off
      *
     c                   endsr
      ***********************************************************
      * srPrcDep - Subroutine to process Dependent
      ***********************************************************
     c     srPrcDep      begsr
      *
     c                   eval      xxco#  = *zero
     c                   eval      xxfnd  = *zero
     c                   eval      xxwk10 = *blanks
     c                   eval      xxkey  = *blanks
     c                   eval      xxtype = *blank
      *
     c                   eval      xxtype = qqtype
      * Access record using rrn from agent master file WDACP
     c     qqrrn         chain     dbrcdf2                            60
      *
     c                   if        *in60 = *off
      * record found
     c                   eval      xxco#  = acco#
     c                   eval      xxfnd  = acfnd
     c                   movel(p)  acfyr         xxwk10           10
     c                   move      accase        xxwk10
     c                   move      acseq#        xxwk05            5
     c                   eval      xxkey = xxwk10 + xxwk05
      *
      * Read record to update the file WMEFL7 when type = 'D'
      *
     c     efky2         setll     pef02
     c     efky2         reade     pef02                                  60
      *
     c                   dow       *in60 = *off
     c                   eval      efpnam = acpnam
     c                   if        acdss# <> *blanks
     c                   movel     acdss#        effein
     c                   endif
     c                   exsr      srldmtfld
     c                   update    pef02
     c     efky2         reade     pef02                                  60
     c                   enddo                                                  *in60 = *off
      *
     c                   else
     c                   eval      qqrtn = 'X'
      *
     c                   endif                                                  *in60 = *off
      *
     c                   endsr
      ***********************************************************
      * srPrcEmp - Subroutine to process Employer
      ***********************************************************
     c     srPrcEmp      begsr
      *
     c                   eval      xxco#  = *zero
     c                   eval      xxfnd  = *zero
     c                   eval      xxwk14 = *blanks
     c                   eval      xxkey  = *blanks
     c                   eval      xxtype = *blank
      *
     c                   eval      xxtype = qqtype
      * Access record using rrn from agent master file WMEMP
     c     qqrrn         chain     dbrcdf3                            60
      *
     c                   if        *in60 = *off
      * record found
     c                   eval      xxco#  = emco#
     c                   eval      xxfnd  = emfnd
     c                   movel(p)  ememp#        xxwk14           14
     c                   move      emdiv         xxwk14
     c                   movel(p)  xxwk14        xxkey            30
      *
      * Read record to update the file WMEFL7 when type = 'E'
      *
     c     efky2         setll     pef02
     c     efky2         reade     pef02                                  60
      *
     c                   dow       *in60 = *off
     c                   eval      effein = emfein
     c                   eval      efpnam = emname
     c                   exsr      srldmtfld
     c                   update    pef02
     c     efky2         reade     pef02                                  60
     c                   enddo                                                  *in60 = *off
      *
      * Read record to update the file WMEFL7 when type = 'E'
      *
     C                   eval      xxkey = *blanks
     c                   movel     'AG'          xxwk02            2
     c                   eval      xxkey = xxwk14 + xxwk02
      *
     c     efky2         setll     pef02
     c     efky2         reade     pef02                                  60
      *
     c                   dow       *in60 = *off
     c                   eval      effein = emfein
     c                   exsr      srldmtfld
     c                   update    pef02
     c     efky2         reade     pef02                                  60
     c                   enddo                                                  *in60 = *off
      *
     c                   else
     c                   eval      qqrtn = 'X'
      *
     c                   endif                                                  *in60 = *off
      *
     c                   endsr
      ***********************************************************
      * srPrcPrv - Subroutine to process Provider
      ***********************************************************
     c     srPrcPrv      begsr
      *
     c                   eval      xxco#  = *zero
     c                   eval      xxfein = *zero
     c                   eval      xxmod  = *zero
     c                   eval      xxtype = *blank
      *
     c                   eval      xxtype = qqtype
      * Access record using rrn from agent master file WMPDP
     c     qqrrn         chain     dbrcdf4                            60
      *
     c                   if        *in60 = *off
      * record found
     c                   eval      xxco#  = xxpco#
     c                   eval      xxfein = pdfein
     c                   eval      xxmod  = pdmod
      *
      * Read record to update the file WMEFL2 when type = 'P'
      *
     c     efky3         setll     pef03
     c     efky3         reade     pef03                                  60
      *
     c                   dow       *in60 = *off
     c                   if        eftype = 'P'
     c                   eval      efpnam = pdname
     c                   exsr      srldmtfld
     c                   update    pef03
     c                   endif                                                  eftype = 'P'
     c     efky3         reade     pef03                                  60
     c                   enddo                                                  *in60 = *off
      *
     c                   else
     c                   eval      qqrtn = 'X'
      *
     c                   endif                                                  *in60 = *off
      *
     c                   endsr
