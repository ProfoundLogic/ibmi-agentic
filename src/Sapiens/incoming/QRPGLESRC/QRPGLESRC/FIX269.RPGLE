     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

      *****************************************************************
      * FIX269 - CLT132 record count of files containing FEIN/SS#
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 02/08/14 C:180 SUPPORT PRISS 027 - WC2234
      *                Re-compiled only.
      *                                                               *
      * 12/19/10 C:061 Project: SY269                                 *
      *                Created.                                       *
      *                                                               *
      *****************************************************************

     ffactp     if   e             disk
     ffaeap     if   e             disk
     ffaedp     if   e             disk
     ffaehp     if   e             disk
     ffaeop     if   e             disk
     ffaepp     if   e             disk
     ffaesp     if   e             disk
     fFAEUP     if   e             disk
     fFAPPP     if   e             disk
     fFAU1P     if   e             disk
     fFDCAP     if   e             disk
     fFDDAP     if   e             disk
     fFDDDP     if   e             disk
     fFDDEP     if   e             disk
     fFDDHP     if   e             disk
     fFDDPP     if   e             disk
     fFDDUP     if   e             disk
     fFMC1P     if   e             disk
     fFMCDP     if   e             disk
     fFMCMP     if   e             disk
     fFMEMP     if   e             disk
     fFMF1P     if   e             disk
     fFMP1P     if   e             disk
     fFMPDP     if   e             disk
     fNMPHP     if   e             disk
     fQFE2P     if   e             disk
     fQFE2P1    if   e             disk
     f                                     rename(pe201:pe202) prefix(p1_)
     fQFEMP     if   e             disk
     f                                     rename(pem01:pem02) prefix(QM_)
     fSMCOP     if   e             disk
     fSMUHP     if   e             disk
     fSMUPP     if   e             disk
     fWACEP2    if   e             disk
     fWACMP     if   e             disk
     f                                     rename(pcm01:pcm02) prefix(AC_)
     fWACTP     if   e             disk
     f                                     rename(pct01:pct02)
     fWAGCP     if   e             disk
     fWAR4P     if   e             disk
     fWARPP     if   e             disk
     fWARPPM    if   e             disk
     f                                     rename(prp01:prp02)
     fWATNPB    if   e             disk
     fWDABP     if   e             disk
     fWDACP     if   e             disk
     fWDCLP     if   e             disk
     fWDF2P     if   e             disk
     fWDPAP     if   e             disk
     fWDPHP     if   e             disk
     f                                     rename(pph01:pph02)
     fWDSLP     if   e             disk
     fWDSLPX    if   e             disk
     f                                     rename(psl01:psl02)
     fWFA2P     if   e             disk
     fWFD1P     if   e             disk
     fWHAGP     if   e             disk
     fWHAPP     if   e             disk
     f                                     rename(ppa01:ppa02)  prefix(AP_)
     fWHCLP     if   e             disk
     f                                     rename(pcl01:pcl02)  prefix(CL_)
     fWHFRP     if   e             disk
     fWHPAP     if   e             disk
     f                                     rename(ppa01:ppa03)  prefix(PA_)
     fWHPOP     if   e             disk
     fWHPRP     if   e             disk
     fWHUPP     if   e             disk
     f                                     rename(pup01:pup02)
     fWMA5P     if   e             disk
     f                                     rename(pac01:pac02)
     fWMAAP     if   e             disk
     fWMACP     if   e             disk
     f                                     rename(pac01:pac03)
     fWMAGP     if   e             disk
     f                                     rename(pag01:pag02) prefix(AG_)
     fWMALP     if   e             disk
     fWMANP     if   e             disk
     fWMAPP     if   e             disk
     fWMCDP     if   e             disk
     f                                     rename(pcd01:pcd02)
     fWMCIP     if   e             disk
     fWMCMP     if   e             disk
     f                                     rename(pcm01:pcm03) prefix(MC_)
     fWMEFP     if   e             disk
     fWMEMP     if   e             disk
     f                                     rename(pem01:pem03) prefix(EM_)
     fWMENP     if   e             disk
     fWMIXP     if   e             disk
     fWMLLP     if   e             disk
     fWMNRP     if   e             disk
     fWMOOP     if   e             disk
     fWMPDP     if   e             disk
     f                                     rename(ppd01:ppd02)
     fWMRRP     if   e             disk
     fWMSRP     if   e             disk
     fWMSRP2    if   e             disk
     f                                     rename(psr01:psr02)
     fWMSRP3    if   e             disk
     f                                     rename(psr01:psr03)
     fWMTPP     if   e             disk
     fWMUPP     if   e             disk
     f                                     rename(pup01:pup03)
     fWXACP     if   e             disk
     f                                     rename(pac01:pac04)
     fWXC#P     if   e             disk
     fWXEFP     if   e             disk
     f                                     rename(pef01:pef02)
     fWXFMP1    if   e             disk
     fWXFMP2    if   e             disk
     fWXFRP     if   e             disk
     f                                     rename(pfr01:pfr02) prefix(FR_)
     fWXHAP     if   e             disk
     fWXMCP     if   e             disk
     fWXN1P     if   e             disk
     fWXNAP     if   e             disk
     fWXNEP     if   e             disk
     fWXSUP     if   e             disk
     fWXT5P     if   e             disk
     fWXT6P     if   e             disk

      *** Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)



     d x1              s              7  0
     d ##Fnam          s             10
     d ##text          s             50
     d ##fein          s             11  0
     d ##ss#           s             11  0


      *==============================================================
      *  MAIN LOOP
      *==============================================================

      *========

      * FACTP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FACTP'

     c                   dou       %eof(FACTP)
     c                   read      FACTP

     c                   if        %eof
     c                   leave
     c                   endif
      ****   ss#
     c                   if        (CTFSS# <> *BLANK)
     c                             or (CTTSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif
      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FAEAP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FAEAP'

     c                   dou       %eof(FAEAP)
     c                   read      FAEAP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (EASS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (EAFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FAEDP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FAEDP'

     c                   dou       %eof(FAEDP)
     c                   read      FAEDP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (EDSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FAEHP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FAEHP'

     c                   dou       %eof(FAEHP)
     c                   read      FAEHP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (EHSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * faeop

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'faeop'

     c                   dou       %eof(faeop)
     c                   read      faeop

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (eoSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (eoFEIN <> *ZERO)
     c                             or (eosfei <> *ZERO)
     c                             or (eoffei <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * faepp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'faepp'

     c                   dou       %eof(faepp)
     c                   read      faepp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (epSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * faesp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'faesp'

     c                   dou       %eof(faesp)
     c                   read      faesp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (esSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * faeup

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'faeup'

     c                   dou       %eof(faeup)
     c                   read      faeup

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (euSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (eutfei <> *ZERO)
     c                             or (euofe1 <> *ZERO)
     c                             or (euofe2 <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *  FAPPP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     C                   eval      ##fnam = 'FAPPP'

     c                   dou       %eof(FAPPP)
     c                   read      FAPPP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (ppfein <> *zero)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * fappp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'fappp'

     c                   dou       %eof(fappp)
     c                   read      fappp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (PPFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FAU1P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FAU1P'

     c                   dou       %eof(FAU1P)
     c                   read      FAU1P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (U1SS# <> *BLANK)
     c                             OR (U1PSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (U1FEIN <> *ZERO)
     c                             OR (U1PFEI <> *ZERO)
     c                             OR (U1FFEI <> *ZERO)
     c                             OR (U1RFEI <> *ZERO)
     c                             OR (U1PROV <> *ZERO)
     c                             OR (U1PXFE <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDCAP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDCAP'

     c                   dou       %eof(FDCAP)
     c                   read      FDCAP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CASS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (CAFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDDAP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDDAP'

     c                   dou       %eof(FDDAP)
     c                   read      FDDAP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (DASS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDDDP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDDDP'

     c                   dou       %eof(FDDDP)
     c                   read      FDDDP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (DDSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDDEP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDDEP'

     c                   dou       %eof(FDDEP)
     c                   read      FDDEP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (DESS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDDHP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDDHP'

     c                   dou       %eof(FDDHP)
     c                   read      FDDHP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (DHSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDDPP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDDPP'

     c                   dou       %eof(FDDPP)
     c                   read      FDDPP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (DPSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FDDUP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FDDUP'

     c                   dou       %eof(FDDUP)
     c                   read      FDDUP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (DUSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMC1P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMC1P'

     c                   dou       %eof(FMC1P)
     c                   read      FMC1P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (C1FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMCDP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMCDP'

     c                   dou       %eof(FMCDP)
     c                   read      FMCDP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CDSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMCMP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMCMP'

     c                   dou       %eof(FMCMP)
     c                   read      FMCMP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CMSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMEMP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMEMP'

     c                   dou       %eof(FMEMP)
     c                   read      FMEMP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (EMFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMF1P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMF1P'

     c                   dou       %eof(FMF1P)
     c                   read      FMF1P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (F1NI01 <> *ZERO)
     c                             OR (F1NI02 <> *ZERO)
     c                             OR (F1NI03 <> *ZERO)
     c                             OR (F1NI04 <> *ZERO)
     c                             OR (F1NI05 <> *ZERO)
     c                             OR (F1NI06 <> *ZERO)
     c                             OR (F1NI07 <> *ZERO)
     c                             OR (F1NI08 <> *ZERO)
     c                             OR (F1NI09 <> *ZERO)
     c                             OR (F1NI10 <> *ZERO)
     c                             OR (F1AFEI <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMP1P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMP1P'

     c                   dou       %eof(FMP1P)
     c                   read      FMP1P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (P1FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * FMPDP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'FMPDP'

     c                   dou       %eof(FMPDP)
     c                   read      FMPDP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (PDFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * NMPHP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'NMPHP'

     c                   dou       %eof(NMPHP)
     c                   read      NMPHP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (PHSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * QFE2P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'QFE2P'

     c                   dou       %eof(QFE2P)
     c                   read      QFE2P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (E2SS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * QFE2P1

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'QFE2P1'

     c                   dou       %eof
     c                   read      pe202

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (P1_E2SS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * QFEMP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'QFEMP'

     c                   dou       %eof
     c                   read      pem02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (QM_EMSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (QM_EMFEIN <> *ZERO)
     c                             OR (QM_EMTFEI <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * SMCOP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'SMCOP'

     c                   dou       %eof(SMCOP)
     c                   read      SMCOP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (COFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * SMUHP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'SMUHP'

     c                   dou       %eof(SMUHP)
     c                   read      SMUHP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (UHPFEI <> *ZERO)
     c                              OR (UHNFEI <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * SMUPP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'SMUPP'

     c                   dou       %eof(SMUPP)
     c                   read      SMUPP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (UPFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WACEP2

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WACEP2'

     c                   dou       %eof(WACEP2)
     c                   read      WACEP2

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CESS# <> 0)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WACMP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WACMP'

     c                   dou       %eof
     c                   read      pcm02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (AC_CMSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (AC_CMHOSF <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WACTP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WACTP'

     c                   dou       %eof
     c                   read      pct02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CTSS# <> *BLANK)
     c                             OR (CTXSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WAGCP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WAGCP'

     c                   dou       %eof(WAGCP)
     c                   read      WAGCP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (GCFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WAR4P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WAR4P'

     c                   dou       %eof(WAR4P)
     c                   read      WAR4P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (R4FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WARPP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WARPP'

     c                   dou       %eof(WARPP)
     c                   read      WARPP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (RPFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WARPPM

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WARPPM'

     c                   dou       %eof
     c                   read      prp02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (RPFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WATNPB

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WATNPB'

     c                   dou       %eof(WATNPB)
     c                   read      WATNPB

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (TNFEIN <> *BLANK)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDABP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDABP'

     c                   dou       %eof(WDABP)
     c                   read      WDABP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (ABFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDACP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDACP'

     c                   dou       %eof(WDACP)
     c                   read      WDACP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (ACDSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (ACCRTI <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDCLP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDCLP'

     c                   dou       %eof(WDCLP)
     c                   read      WDCLP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CLSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (CLCFID <> *ZERO)
     c                             OR (CLEFID <> *ZERO)
     c                             OR (CLAFID <> *ZERO)
     c                             OR (CLPHYF <> *ZERO)
     c                             OR (CLHOSF <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDF2P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDF2P'

     c                   dou       %eof(WDF2P)
     c                   read      WDF2P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (F2FEIN <> *ZERO)
     c                             OR (F2RRE# <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDPAP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDPAP'

     c                   dou       %eof(WDPAP)
     c                   read      WDPAP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (PAFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDPHP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDPHP'

     c                   dou       %eof
     c                   read      pph02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (PHFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDSLP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDSLP'

     c                   dou       %eof(WDSLP)
     c                   read      WDSLP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (SLSS# <> 0)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WDSLPX

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WDSLPX'

     c                   dou       %eof
     c                   read      psl02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (SLSS# <> 0 )
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WFA2P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WFA2P'

     c                   dou       %eof(WFA2P)
     c                   read      WFA2P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (A2AFE1 <> *ZERO)
     c                             OR (A2AFE2 <> *ZERO)
     c                             OR (A2AFEC <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WFD1P

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WFD1P'

     c                   dou       %eof(WFD1P)
     c                   read      WFD1P

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (D1SS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WHAGP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WHAGP'

     c                   dou       %eof(WHAGP)
     c                   read      WHAGP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (AGFEIN <> *BLANKS)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WHAPP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WHAPP'

     c                   dou       %eof
     c                   read      ppa02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (AP_APSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WHCLP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WHCLP'

     c                   dou       %eof
     c                   read      pcl02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (CL_CLSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (CL_CLEXFE <> *BLANKS)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WHFRP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WHFRP'

     c                   dou       %eof(WHFRP)
     c                   read      WHFRP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (FRSS <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (FRFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WHPAP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WHPAP'

     c                   dou       %eof
     c                   read      ppa03

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (PA_PAFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * whpop

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'whpop'

     c                   dou       %eof(whpop)
     c                   read      whpop

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (poFEIN <> *blanks)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * whprp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'whprp'

     c                   dou       %eof(whprp)
     c                   read      whprp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (prFEIN <> *blank)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * whupp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'whupp'

     c                   dou       %eof
     c                   read      pup02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (upFEIN <> *zero)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wma5p

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wma5p'

     c                   dou       %eof
     c                   read      pac02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (a5FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WMAAP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WMAAP'

     c                   dou       %eof(WMAAP)
     c                   read      WMAAP

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (AAFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WMACP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WMACP'

     c                   dou       %eof
     c                   read      pac03

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (ACFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * WMAGP

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WMAGP'

     c                   dou       %eof
     c                   read      pag02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
      ****   fein
     c                   if        (AG_AGFEIN <> *ZERO)
     c                   if        (AG_AGSS# = 'Y')
     c                   eval      ##ss# += 1
     C                   else
     c                   eval      ##fein += 1
     c                   endif
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmalp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmalp'

     c                   dou       %eof(wmalp)
     c                   read      wmalp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (alFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmanp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmanp'

     c                   dou       %eof(wmanp)
     c                   read      wmanp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (anFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmapp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmapp'

     c                   dou       %eof(wmapp)
     c                   read      wmapp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (apSS# <> *zero)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmcdp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmcdp'

     c                   dou       %eof
     c                   read      pcd02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (cdSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmcip

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmcip'

     c                   dou       %eof(wmcip)
     c                   read      wmcip

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (ciFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmcmp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmcmp'

     c                   dou       %eof
     c                   read      pcm03

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (MC_cmSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (MC_cmhosf <> *ZERO)
     c                             or (MC_cmmcof <> *blanks)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmefp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmefp'

     c                   dou       %eof(wmefp)
     c                   read      wmefp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (efFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmemp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmemp'

     c                   dou       %eof
     c                   read      pem03

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (EM_emFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmenp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmenp'

     c                   dou       %eof(wmenp)
     c                   read      wmenp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (enfen1 <> *ZERO)
     c                             or (enofi1 <> *ZERO)
     c                             or (enfen2 <> *ZERO)
     c                             or (enofi2 <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmixp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmixp'

     c                   dou       %eof(wmixp)
     c                   read      wmixp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (ixsein <> *blanks)
     c                             or (ixrein <> *blanks)
     c                             or (ixifei <> *zero)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmllp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmllp'

     c                   dou       %eof(wmllp)
     c                   read      wmllp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (lldefa <> *ZERO)
     c                             or (llplaa <> *ZERO)
     c                             or (lltpaa <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmnrp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmnrp'

     c                   dou       %eof(wmnrp)
     c                   read      wmnrp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (NRFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmoop

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmoop'

     c                   dou       %eof(wmoop)
     c                   read      wmoop

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (oooSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmpdp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmpdp'

     c                   dou       %eof
     c                   read      ppd02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (pdFEIN <> *ZERO)
     c                   if        (pdss# = 'Y')
     c                   eval      ##ss# += 1
     c                   else
     c                   eval      ##fein += 1
     c                   endif
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmrrp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmrrp'

     c                   dou       %eof(wmrrp)
     c                   read      wmrrp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (rrFEIN <> *ZERO)
     c                             or (rratty <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmsrp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmsrp'

     c                   dou       %eof(wmsrp)
     c                   read      wmsrp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (srFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmsrp2

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmsrp2'

     c                   dou       %eof
     c                   read      psr02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (srFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmsrp3

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmsrp3'

     c                   dou       %eof
     c                   read      psr03

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (srFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmtpp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmtpp'

     c                   dou       %eof(wmtpp)
     c                   read      wmtpp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (tpphyf <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wmupp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wmupp'

     c                   dou       %eof
     c                   read      pup03

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (upFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxacp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxacp'

     c                   dou       %eof
     c                   read      pac04

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (acFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxc#p

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxc#p'

     c                   dou       %eof(wxc#p)
     c                   read      wxc#p

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (crFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxefp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxefp'

     c                   dou       %eof
     c                   read      pef02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (efSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (efdefa <> *ZERO)
     c                             or (efplaa <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxfmp1

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxfmp1'

     c                   dou       %eof(wxfmp1)
     c                   read      wxfmp1

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (f3FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxfmp2

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxfmp2'

     c                   dou       %eof(wxfmp2)
     c                   read      wxfmp2

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (f4FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxfrp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxfrp'

     c                   dou       %eof
     c                   read      pfr02

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (FR_frSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (FR_frclcd <> *blanks)
     c                             or (FR_fricsn <> *blanks)
     c                             or (FR_frmcof <> *blanks)
     c                             or (FR_frcafe <> *blanks)
     c                             or (FR_fridfe <> *blanks)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxhap

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxhap'

     c                   dou       %eof(wxhap)
     c                   read      wxhap

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (hasEIN <> *blanks)
     c                             or (harein <> *blanks)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxmcp

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxmcp'

     c                   dou       %eof(wxmcp)
     c                   read      wxmcp

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (mcFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxn1p

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxn1p'

     c                   dou       %eof(wxn1p)
     c                   read      wxn1p

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (n1FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxnap

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxnap'

     c                   dou       %eof(wxnap)
     c                   read      wxnap

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (naFEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxnep

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxnep'

     c                   dou       %eof(wxnep)
     c                   read      wxnep

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (nefeid <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxsup

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'WXSUP'

     c                   dou       %eof(wxsup)
     c                   read      wxsup

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#
     c                   if        (suSS# <> *BLANK)
     c                   eval      ##ss# += 1
     c                   endif

      ****   fein
     c                   if        (suclcd <> *blanks)
     c                             or (sufein <> *blanks)
     c                             or (sucafe <> *blanks)
     c                             or (suiif  <> *blanks)
     c                             or (suemp# <> *blanks)
     c                             or (suidfe <> *blanks)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxt5p

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxt5p'

     c                   dou       %eof(wxt5p)
     c                   read      wxt5p

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (t5FEIN <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========
      *========

      * wxt6p

     c                   clear                   ##fein
     c                   clear                   ##ss#
     c                   eval      ##fnam = 'wxt6p'

     c                   dou       %eof(wxt6p)
     c                   read      wxt6p

     c                   if        %eof
     c                   leave
     c                   endif

      ****   ss#

      ****   fein
     c                   if        (t6FEIN <> *ZERO)
     c                             or (t6rfen <> *ZERO)
     c                             or (t6pfen <> *ZERO)
     c                   eval      ##fein += 1
     c                   endif

     c                   enddo

     c                   call      'FIXSY269C'
     c                   parm                    ##Fnam
     c                   parm      *blanks       ##text
     c                   parm      *zero         ##Nrec           10 0
     c                   parm      *blank        ##Err             1

     c                   if        ##err = '0'
     c                             and ((##SS# <> 0) or (##fein <> 0))
     c                   except    det
     c                   endif

      *========

     C                   SETON                                        LR


     OQSYSPRT   E            det            1
      *
     O                       ##Fnam
     O                       ##text            +010
     O                       ##SS#         3   +005
     O                       ##FEIN        3   +005
      *

