     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================*
      /title seq73405 - FMIC Deposit Report                                     *
     H option(*nodebugio: *srcstmt)                                             *
     H debug(*yes)                                                              *
      *=========================================================================*
     fwmeml     if   e           k disk
     fwdell     if   e           k disk
     fwdf1l     if   e           k disk
     fwmeal     if   e           k disk
     fwtabl     if   e           k disk
     fseq73405  uf a e           k disk
     d/copy wpremplr
     d/copy wprar
     dgetDepTotals     pr
     dcxdate           s                   like(emcad1)

     c     elky          klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c                   kfld                    EMEMP#
     c                   kfld                    EMDIV

     c     f1ky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR

     c     tbkey         klist
     c                   kfld                    ELCO#
     c                   kfld                    kkkey            10

     c                   read      seq01
     c                   dow       not(%eof)
     c                   delete    seq01
     c                   read      seq01
     c                   enddo

     c                   read      pem01
     c                   dow       not(%eof)
     c     elky          setgt     pel01
     c     elky          readpe    pel01
     c                   movel(p)  ELBSCD        kkkey
     c     tbkey         chain     pbs01
     c                   if        BSSEFL = 'S'
     c                   z-add     BSAB01        depPerc           7 6
     c     BSAB01        ifeq      *zero
     c                   z-add     BSAB02        depPerc
     c                   endif
     c                   eval      cxDate = getCxDate(EMCO#:EMFND:EMEMP#:EMDIV)
     c                   if        cxDate = 0 or
     c                             cxDate <> 0 and ELFAF <> 'A'
     c     f1ky          chain     pf101
     c                   if        F1DPS = 'S'
     c                   eval      COMPANY    = ELCO#
     c                   eval      GROUP      = ELFND
     c                   eval      EMPLOYER   = ELEMP#
     c                   eval      DIVISION   = ELDIV
     c                   eval      LASTYEAR   = ELFYR
     c                   eval(h)   STDDEPCALC = ELSTPR * depPerc
     c                   eval(h)   MODDEPCALC = ELEMPM * depPerc
     c                   callp     getDepTotals
     c                   write     seq01
     c                   endif
     c                   endif
     c                   endif
     c                   read      pem01
     c                   enddo
     c                   seton                                        lr
      *=========================================================================
     pgetDepTotals     b
     dgetDepTotals     pr
     darCat            s              1

     c     eaky          klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c                   kfld                    EMEMP#
     c                   kfld                    EMDIV

     c                   clear                   DEPBILLED
     c                   clear                   DEPPAID
     c     eaky          setll     pea01
     c     eaky          reade     pea01
     c                   dow       not(%eof)
     c                   eval      arCat = GetARCat(EAARCD)
     c                   if        arCat = 'D'
     c                   select
     c                   when      EAARTY = 'H'
     c                   eval      DEPBILLED = DEPBILLED + EAAMT
     c                   when      EAARTY = 'I'
     c                   eval      DEPPAID   = DEPPAID   + EAAMT
     c                   when      EAARTY = 'T'
     c                   eval      DEPPAID   = DEPPAID   + EAAMT
     c                   when      EAARTY = 'R'
     c                   eval      DEPPAID   = DEPPAID   - EAAMT
     c                   endsl
     c                   endif
     c     eaky          reade     pea01
     c                   enddo

     pgetDepTotals     e
