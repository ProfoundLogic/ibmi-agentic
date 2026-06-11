    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
    H*
     H**************************************************************************
    H*  F8287R01 - First Health EOR Financial Bills with Errors Update/Display
    H*
    H*  Called from: F8287R - First Health EOR Financial Work with Bills with E
    H*  Calls to   : F8282R - First Health EOR Financial Process - Transfer pro
    H*
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8267R01 from Client 135
     H**************************************************************************
    H*  Revisions:
    H*
     H*  12/04/06 C:182 Support: 55903                                         *
    H*                 Added logic to display the Provider Payments,          *
    H*                 Patriot Payment and From & To Date of Service for      *
    H*                 reconsideration errors.                                *
    H*                                                                        *
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff8287f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffax4p     uf a e             disk
     F                                     rename(px401:dbrcdf)
     Ffax3l1    if   e           k disk
     Ffaxal1    uf   e           k disk
     Ffmc1l     uf   e           k disk
     Ffmcml1    if   e           k disk
     Ffmpdl     if   e           k disk
    D*
     D clm             s              1    dim(20)
    D*
     D***********************/COPY SCOPYMT01I
    D*
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D value1                        10
     D  sign1                         1    overlay(value1)
     D  amt1                          9  2 overlay(value1:2)
     D                 ds
     D value2                        10
     D  sign2                         1    overlay(value2)
     D  amt2                          9  2 overlay(value2:2)
     D                 ds
     D claim#                        20
     D  xxfnd                         3    overlay(claim#)
     D  xxfyr                         3    overlay(claim#:4)
     D  xxcase                        7    overlay(claim#:7)
     D                 ds
     D x3femd
     D  xxfein                        9  0        overlay(x3femd:*next)
     D  xxmod                         3  0        overlay(x3femd:*next)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('First Health Bill -
     D                                     Transfer')
     D optnmt          c                   const('First Health Bill -
     D                                     Transfer')
     D optnmd          c                   const('First Health Bill -
     D                                     Deletion')
     D comapi          c                   const('FA007R')
     D zeros           c                   const('000000000')
    D*================================================================
    D*  User defined data structures
    D*================================================================
    D*
     D                 ds
     D xachrg
     D  xachrg#                                   like(amouns) overlay(xachrg:2)
     D xafs
     D  xafs#                                     like(amouns) overlay(xafs:2)
     D xappo
     D  xappo#                                    like(amouns) overlay(xappo:2)
     D xapaid
     D  xapaid#                                   like(amouns) overlay(xapaid:2)
     D xasurc
     D  xasurc#                                   like(amouns) overlay(xasurc:2)
     D xaplfe
     D  xaplfe#                                   like(amouns) overlay(xaplfe:2)
     D xapbfe
     D  xapbfe#                                   like(amouns) overlay(xapbfe:2)
     D xa2ts1
     D  xa2ts1#                                   like(amouns) overlay(xa2ts1:2)
     D xa2ts2
     D  xa2ts2#                                   like(amouns) overlay(xa2ts2:2)
     D xa2ts3
     D  xa2ts3#                                   like(amouns) overlay(xa2ts3:2)
     D xa2ts4
     D  xa2ts4#                                   like(amouns) overlay(xa2ts4:2)
     D xa2ts5
     D  xa2ts5#                                   like(amouns) overlay(xa2ts5:2)
     D xa2ts6
     D  xa2ts6#                                   like(amouns) overlay(xa2ts6:2)
     D xa2ts7
     D  xa2ts7#                                   like(amouns) overlay(xa2ts7:2)
     D xa2ts8
     D  xa2ts8#                                   like(amouns) overlay(xa2ts8:2)
     D xa2ts9
     D  xa2ts9#                                   like(amouns) overlay(xa2ts9:2)
     D xa2t10
     D  xa2t10#                                   like(amouns) overlay(xa2t10:2)
     D xa2lnf
     D  xa2lnf#                                   like(amouns) overlay(xa2lnf:2)
     D*---------------------------------------------------------------------
     D* Stand Alone Fields - TOP
     D*---------------------------------------------------------------------
     D $#amt           s              9  2
     D $#case          s              7  0
     D $#co#           s              3  0
     D $#fein          s              9  0
     D $#fnd           s              3  0
     D $#from          s              7  0
     D $#fyr           s              3  0
     D $#recd          s              2
     D $#retp          s              1
     D $#rtcd          s              2
     D $#to            s              7  0
     D ##acdt          s              8  0
     D ##call          s              1
     D ##case          s                   like(cmcase)
     D ##co#           s                   like(cmco#)
     D ##conm          s             40
     D ##dcid          s             12
     D ##fein          s              9
     D ##fnam          s             40
     D ##fnd           s                   like(cmfnd)
     D ##frst          s             15
     D ##fyr           s                   like(cmfyr)
     D ##last          s             56
     D ##mod           s              3
     D ##prvc          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##rtcd          s              1
     D ##ss#           s                   like(cmss#)
     D ##sub#          s                   like(cmsub#)
     D ##term          s              1
     D ##type          s              1
     D #ofscr          s              1  0
     D #1co#           s              3
     D #1err           s              1
     D #1fein          s              9  0
     D #1fnd           s              3
     D #1mod           s              3  0
     D #1name          s             40
     D #1sub#          s              5
     D #1wcf#          s              3
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rtcd          s              1
     D amount          s             12  2
     D amouns          s             12s 2
     D hdfein          s                   like(pdfein)
     D hdmod           s                   like(pdmod)
     D kkacdt          s                   like(cmacdt)
     D kkcase          s                   like(cmcase)
     D kkcntl          s                   like(x4cntl)
     D kkfein          s                   like(pdfein)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkmod           s                   like(pdmod)
     D kkss#           s                   like(cmss#)
     D kksub#          s                   like(cmsub#)
     D sign            s              1
     D xxamt1          s              9  2
     D xxamt2          s              9  2
     D xxchg           s              1
     D xxdate          s              8  0
     D xxdateiso       s               d   datfmt(*ISO)
     D xxfrdt          s                   like(cmdt)
     D xxpall          s              9  2
     D xxprpo          s              9  2
     D xxtodt          s                   like(cmdt)
     D*---------------------------------------------------------------------
     D* Stand Alone Fields - BOTTOM
     D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@call
     C                   parm                    @@rtcd
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
    C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcym2        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     c1ky          klist
     C                   kfld                    ##co#
    C*
     C     cmky1         klist
     C                   kfld                    ##co#
     C                   kfld                    ##sub#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##case
    C*
     C     cmky2         klist
     C                   kfld                    ##co#
     C                   kfld                    ##sub#
     C                   kfld                    scfnd
     C                   kfld                    scfyr
     C                   kfld                    sccase
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     x3ky          klist
     C                   kfld                    kkcntl
    C*
     C     xaky1         klist
     C                   kfld                    kkcntl
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
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
    C* ERRCK - Error checking for why this bill wasn't processed.
    C*================================================================
     C     errck         begsr
    C*
     C                   eval      *in21 = *off
     C                   eval      *in22 = *off
     C                   eval      *in23 = *off
     C                   eval      *in24 = *off
     C                   eval      *in25 = *off
    C*
    C* LETS SEE IF THIS IS A RECONSIDERATION
    C*
     C                   movel     x4clm#        claim#
     C                   move      xxfnd         scfnd
     C                   move      xxfyr         scfyr
     C                   move      xxcase        sccase
     C                   if        x4prfl = 'P'
     C                             and x4case <> *zeros
     C                   move      x4fnd         scfnd
     C                   move      x4fyr         scfyr
     C                   move      x4case        sccase
     C                   endif
    C*
    C* Check WCMC Claim Master to see if SS#/ACDT exists.
    C*
     C     cmky2         chain     pcm01                              99
     C                   if        *in99 = *on
     C                   eval      *in21 = *on
     C                   endif
    C*
    C* FEIN/MOD does not exist.
    C*
     C                   eval      kkfein = xxfein
     C                   eval      kkmod  = xxmod
    C*
     C                   if        x4prfl = 'P'
     C                             and x4fein <> *zeros
     C                   eval      hdfein = kkfein
     C                   eval      hdmod  = kkmod
     C                   eval      kkfein = x4fein
     C                   eval      kkmod  = x4mod
     C                   endif
    C*
     C     pdky          chain     ppd01                              99
     C                   if        *in99 = *on
     C                             or *in99 = *off
     C                             and pdstat = 'I'
     C                   eval      *in22 = *on
     C                   movel     'Y'           xxchg
     C                   endif
    C*
     C                   if        x4prfl = 'R'
    C*
    C* What kind of recon error
    C*
     C                   eval      *in24 = *off
     C                   eval      *in25 = *off
    C*
     C                   select
     C                   when      x4rcne = '1'
    C*** BEG DEL ***** 12/04/06 *****************************************************
     C****************   eval      *in24 = *on
    C*** END DEL ***** 12/04/06 *****************************************************
   C*** BEG ADD ***** 12/04/06 *****************************************************
     C                   eval      *in17 = '1'
     C                   eval      *in25 = *on
    C*** END ADD ***** 12/04/06 *****************************************************
     C                   when      x4rcne = '2'
    C*** BEG DEL ***** 12/04/06 *****************************************************
     C****************   eval      *in25 = *on
    C*** END DEL ***** 12/04/06 *****************************************************
   C*** BEG ADD ***** 12/04/06 *****************************************************
     C                   eval      *in17 = '1'
     C                   eval      *in24 = *on
    C*** END ADD ***** 12/04/06 *****************************************************
     C                   endsl
    C*
     C                   endif
    C*
    C* Payment error
    C*
     C                   eval      *in26 = *off
     C                   if        x4prfl = 'P'
   C*** BEG ADD **** 12/04/06 *****************************************************
     C                             Or x4prfl = 'R'
    C*** END ADD **** 12/04/06 *****************************************************
     C                   eval      *in26 = *on
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
    C*
     C                   if        *in16  = *off
    C*
    C* Check WCMC Claim Master to see if the claim number exists
    C*
     C                   if        *in90  = *off
    C*
     C                   eval      kksub# = ##sub#
     C                   eval      kkfnd  = scfnd
     C                   eval      kkfyr  = scfyr
     C                   eval      kkcase = sccase
     C     cmky2         chain     pcm01                              99
     C                   if        *in99  = *on
     C                   eval      *in71  = *on
     C                   eval      *in90  = *on
     C                   endif
     C                   endif
    C*
    C* FEIN/MOD does not exist.
    C*
     C                   if        *in90  = *off
     C                   eval      kkfein = scfein
     C                   eval      kkmod  = scmod
    C*
     C     pdky          chain     ppd01                              99
     C                   if        *in99  = *on
     C                             or  *in99  = *off
     C                             and pdstat = 'I'
     C                   eval      *in72  = *on
     C                   eval      *in90  = *on
     C                   endif
     C                   endif
    C*
     C                   else
    C*
    C* DELETION REASON CAN'T BE BLANKS
    C*
     C                   if        scdlre = *blanks
     C                             and *in90 = *off
     C                   eval      *in73  = *on
     C                   eval      *in90  = *on
     C                   endif
     C                   endif
    C*
    C* HAS THE PROVIDER CHANGED
    C*
     C                   if        *in90  = *off
     C                             and xxchg <> 'Y'
     C                   if        scfein <> hdfein
     C                             or scmod  <> hdmod
     C                   movel     'Y'           xxchg
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
    C*
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* KJDO - F10=Search
    C*================================================================
     C     kjdo          begsr
    C*
     C                   select
     C                   when      hfld = 'SCFND'
     C                             or hfld = 'SCFYR'
     C                             or hfld = 'SCCASE'
    C*
     C                   call      'F0269R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##sub#
     C                   parm      *zeros        ##fnd
     C                   parm      *zeros        ##fyr
     C                   parm      *zeros        ##case
     C                   parm      *blanks       ##ss#
     C                   parm      *zeros        ##acdt
     C                   parm      ' '           ##rtcd
    C*
     C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fyr = *zeros
     C                             or ##case = *zeros
     C                   eval      *in31 = *on
     C                   eval      *in90 = *on
     C                   else
     C     cmky1         chain     pcm01                              99
    C*
     C                   movel(p)  cmlsn         ##last
     C                   movel(p)  cmfsn         ##frst
     C                   eval      scfnd = cmfnd
     C                   eval      scfyr = cmfyr
     C                   eval      sccase = cmcase
     C                   move      '1'           ##term
    C*
     C                   call      'SRNAME'
     C                   parm                    ##last
     C                   parm                    ##frst
     C                   parm      '1'           ##term
    C*
     C                   movel(p)  ##last        sccnam
     C                   movel(p)  ##ss#         scssn
     C                   eval      scdoi = ##acdt
     C                   eval      *in32 = *on
    C*
     C                   endif
    C*
    C* PROVIDER SEARCH
    C*
     C                   when      hfld = 'SCFEIN'
     C                             or hfld = 'SCMOD'
    C*
     C                   call      'F0032R'
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##fein
     C                   parm      *blanks       ##mod
     C                   parm      *blanks       ##rtcd
     C                   parm      *blanks       ##type
    C*
     C                   if        ##rtcd = 'A'
     C                             or ##rtcd = 'B'
     C                             or ##fein = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   else
     C                   move      ##fein        scfein
     C                   move      ##mod         scmod
    C*
     C                   call      'FA725R'
     C                   parm      scfein        #1fein
     C                   parm      scmod         #1mod
     C                   parm      *blanks       #1name
     C                   parm      *blanks       #1err
     C                   if        #1err <> '0'
     C                   movel(p)  #1name        scpnam
     C                   endif
     C                   endif
    C*
     C                   endsl
    C*
     C                   eval      *in90 = *on
    C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   eval      kkcntl = x4cntl
     C     x3ky          chain     px301
     C     xaky1         chain(n)  pxa01
     C                   if        %found(faxal1)
     C                   exsr      xasign
     C                   endif
    C*
     C                   if        *in16 = *off
     C                   exsr      errck
     C                   endif
    C*
     C                   movel(p)  x4cntl        scblid
    C*
     C                   call      'SRNAME'
     C                   parm      cmlsn         ##last
     C                   parm      cmfsn         ##frst
     C                   parm      '1'           ##term
    C*
     C                   movel(p)  ##last        sccnam
     C                   if        x4prfl = 'P'
     C                             and x4case <> *zeros
    C*
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdoi
     C                   movel     cmss#         scssn
    C*
     C                   else
     C                   movel(p)  x4ss#         scssn
     C                   if        x4acdt <> 0
     C     *cymd         movel     x4acdt        xxdateiso
     C     *usa          move      xxdateiso     scdoi
     C                   endif
     C                   endif
    C*
     C                   if        x4prfl = 'P'
     C                             and x3femd <> *zeros
     C                             and xxfein <> hdfein
     C                             and xxfein <> *zeros
     C                             or x4prfl = 'P'
     C                             and xxfein <> *zeros
     C                             and xxmod <> hdmod
     C                             and hdfein <> *zeros
    C*
     C                   eval      scfein = kkfein
     C                   eval      scmod = kkmod
     C                   if        pddba1 <> 'Y'
     C                   if        pdfsn = *blanks
     C                   move      pdname        scpnam
     C                   else
     C     pdfsn         cat(p)    pdname:1      scpnam
     C                   endif
     C                   move      pdadr1        scpad1
     C                   else
     C                   move      pdadr1        scpnam
     C                   move      pdadr2        scpad1
     C                   endif
     C                   move      pdcty         scpcty
     C                   move      pdst          scpst
     C                   move      pdzip         scpzip
    C*
     C                   else
    C*
     C                   move      kkfein        scfein
    C*
     C                   if        x3bfsn = *blanks
     C                   movel(p)  x3blsn        scpnam
     C                   else
     C     x3bfsn        cat(p)    x3blsn:1      scpnam
     C                   endif
    C*
     C                   movel(p)  x3bad1        scpad1
     C                   movel(p)  x3bcty        scpcty
     C                   movel(p)  x3bst         scpst
     C                   move      x3bzip        scpzip
    C*
     C                   eval      hdfein = scfein
     C                   eval      hdmod = scmod
     C                   endif
    C*
     C                   clear                   scdlre
    C*
    C*  PAID
    C*
     C                   eval      scpaid = xapaid#
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *usa          move      xxdateiso     scfrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *usa          move      xxdateiso     sctodt
     C                   endif
    C*
    C* FEE
    C*
     C                   eval      scnamt  = xa2ts1# + xa2ts2# + xa2ts3# +
     C                                       xa2ts4# + xa2ts5# + xa2ts6# +
     C                                       xa2ts7# + xa2ts8# + xa2ts9# +
     C                                       xa2t10#
    C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
    C*
     C                   if        *in16 = *on
    C*
     C                   movel(p)  q1user        x4dlus
     C                   movel(p)  q1pgm         x4dlpg
    C*
     C                   time                    dsdttm
    C*
     C                   eval      x4dltm = dstime
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x4dldt
     C                   movel     srdto         x4prdt
    C*
     C                   movel(p)  scdlre        x4dlre
     C                   movel(p)  'D'           x4prfl
    C*
     C                   eval      x4co# = *zeros
     C                   eval      x4sub# = *zeros
     C                   eval      x4fnd = *zeros
     C                   eval      x4fyr = *zeros
     C                   eval      x4case = *zeros
     C                   eval      x4eob# = *zeros
     C                   clear                   x4ss#
     C                   eval      xxfein = *zeros
     C                   eval      xxmod = *zeros
     C                   eval      x4acdt = *zeros
    C*
     C                   else
    C*
    C* Now that the the errors have been corrected let's check for
    C* duplicate payments
    C*
     C                   eval      *in91 = *off
     C                   if        *in90 = *off
     C                             and x4prfl = 'E'
    C*
    C*  PAID
    C*
     C                   eval      scpaid = xapaid#
    C*
     C                   if        scpaid <> *zeros
    C*
     C                   if        x4fdos <> *blanks
     C                   movel     x4fdos        xxdateiso
     C     *cymd         move      xxdateiso     xxfrdt
     C                   endif
    C*
     C                   if        x4tdos <> *blanks
     C                   movel     x4tdos        xxdateiso
     C     *cymd         move      xxdateiso     xxtodt
     C                   endif
    C*
     C                   move      cmco#         #1co#
     C                   move      cmsub#        #1sub#
     C                   move      cmfnd         #1fnd
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       #1wcf#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
    C*
     C                   if        ##rtcd = *blanks
     C                   move      #1wcf#        $#fnd
     C                   else
     C                   eval      $#fnd = *zeros
     C                   endif
    C*
    C* Check for duplicate request/payment
    C*
     C                   call      'WA052R'
     C                   parm      cmco#         $#co#
     C                   parm                    $#fnd
     C                   parm      cmfyr         $#fyr
     C                   parm      cmcase        $#case
     C                   parm      xxfrdt        $#from
     C                   parm      xxtodt        $#to
     C                   parm      scpaid        $#amt
     C                   parm      '11'          $#recd
     C                   parm      pdfein        $#fein
     C                   parm      'M'           $#retp
     C                   parm      *blanks       $#rtcd
    C*
     C                   if        $#rtcd <> *blanks
     C                   exfmt     wdw01
     C                   if        *inkl = *on
     C                   eval      *in91 = *on
     C                   eval      *inkl = *off
     C                   movel     'P'           x4prfl
     C                   movel     'P'           @@rtcd
     C                   endif
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      x4co#  = cmco#
     C                   eval      x4sub# = cmsub#
     C                   eval      x4fnd  = cmfnd
     C                   eval      x4fyr  = cmfyr
     C                   eval      x4case = cmcase
     C                   movel     cmss#         x4ss#
     C                   eval      x4acdt = cmacdt
     C                   eval      x4fein = pdfein
     C                   eval      x4mod  = pdmod
    C*
     C                   if        *in91  = *off
     C                   if        x4eob# = *zeros
     C                             and x4prfl = 'E'
     C                             or x4eob#  = *zeros
     C                             and x4prfl = 'P'
     C     c1ky          chain     pc101                              99
     C                   if        *in99 = *off
     C                   eval      c1eob# = c1eob# + 1
     C                   update    pc101
     C                   endif
     C                   eval      x4eob# = c1eob#
     C                   endif
     C                   endif
    C*
     C                   eval      x4rcn = *zeros
     C                   clear                   x4dlus
     C                   clear                   x4dlpg
     C                   eval      x4dltm = *zeros
     C                   eval      x4dldt = *zeros
     C                   clear                   x4dlre
     C                   endif
    C*
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
    C*
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off
    C*
     C     *inkj         caseq     *on           kjdo
     C                   endcs
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
     C                   eval      *in09 = *off
     C                   eval      *in16 = *off
     C                   eval      *in21 = *off
     C                   eval      *in22 = *off
     C                   eval      *in23 = *off
     C                   eval      *in24 = *off
     C                   eval      *in25 = *off
    C*
     C                   eval      *in09 = @@mode = 'D'
     C                   eval      *in16 = @@call = 'D'
    C*
     C                   eval      #ofscr = 1
     C                   movel     'N'           xxchg
    C*
     C                   if        *in16 = *off
     C                   movel(p)  optnmt        ##data
     C                   else
     C                   movel(p)  optnmd        ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprdx
    C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
    C*
     C                   eval      kkcntl = x4cntl
     C     x3ky          chain     px301                              93
     C     xaky1         chain(n)  pxa01                              98
     C                   if        %found(faxal1)
     C                   exsr      xasign
     C                   endif
    C*
     C                   exsr      mvscdb
     C                   update    dbrcdf
    C*
     C                   if        *in16 = *off
     C                   if        *in17 = *off
     C                   if        *in91 = *off
     C                   call      'F8282R'
     C                   parm      *blanks       ##dcid
     C                   parm      *zeros        ##rrn
     C                   parm      @@rrn         ##rrn2
     C                   parm      xxchg         ##prvc
     C                   endif
     C                   else
     C                   call      'F8288R'                             99
     C                   parm      *blanks       ##dcid
     C                   parm      *zeros        ##rrn
     C                   parm      @@rrn         ##rrn2
     C                   parm      xxchg         ##prvc
     C                   endif
     C                   endif
E01  C                   endif
    C*
     C                   endsr
    C*================================================================
    C* XASIGN - XA amounts sign
    C*================================================================
     C     xasign        begsr
    C*
     C                   if        %subst(xachrg:1:1)= '-'
     C                   eval      xachrg#= 0 - xachrg#
     C                   endif
     C                   if        %subst(xafs:1:1)  = '-'
     C                   eval      xafs#  = 0 - xafs#
     C                   endif
     C                   if        %subst(xappo:1:1) = '-'
     C                   eval      xappo# = 0 - xappo#
     C                   endif
     C                   if        %subst(xapaid:1:1)= '-'
     C                   eval      xapaid#= 0 - xapaid#
     C                   endif
     C                   if        %subst(xasurc:1:1)= '-'
     C                   eval      xasurc#= 0 - xasurc#
     C                   endif
     C                   if        %subst(xaplfe:1:1)= '-'
     C                   eval      xaplfe#= 0 - xaplfe#
     C                   endif
     C                   if        %subst(xapbfe:1:1)= '-'
     C                   eval      xapbfe#= 0 - xapbfe#
     C                   endif

     C                   if        %subst(xa2ts1:1:1)= '-'
     C                   eval      xa2ts1#= 0 - xa2ts1#
     C                   endif
     C                   if        %subst(xa2ts2:1:1)= '-'
     C                   eval      xa2ts2#= 0 - xa2ts2#
     C                   endif
     C                   if        %subst(xa2ts3:1:1)= '-'
     C                   eval      xa2ts3#= 0 - xa2ts3#
     C                   endif
     C                   if        %subst(xa2ts4:1:1)= '-'
     C                   eval      xa2ts4#= 0 - xa2ts4#
     C                   endif
     C                   if        %subst(xa2ts5:1:1)= '-'
     C                   eval      xa2ts5#= 0 - xa2ts5#
     C                   endif
     C                   if        %subst(xa2ts6:1:1)= '-'
     C                   eval      xa2ts6#= 0 - xa2ts6#
     C                   endif
     C                   if        %subst(xa2ts7:1:1)= '-'
     C                   eval      xa2ts7#= 0 - xa2ts7#
     C                   endif
     C                   if        %subst(xa2ts8:1:1)= '-'
     C                   eval      xa2ts8#= 0 - xa2ts8#
     C                   endif
     C                   if        %subst(xa2ts9:1:1)= '-'
     C                   eval      xa2ts9#= 0 - xa2ts9#
     C                   endif
     C                   if        %subst(xa2t10:1:1)= '-'
     C                   eval      xa2t10#= 0 - xa2t10#
     C                   endif

     C                   if        %subst(xa2lnf:1:1)= '-'
     C                   eval      xa2lnf#= 0 - xa2lnf#
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      fax4p
    C*
     C                   endsr
