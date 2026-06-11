    Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H option(*nodebugio:*srcstmt)
    H dftactgrp(*no) actgrp(*caller)
    H/define CONVERT
    H/TITLE FIXPXP -  Populate PXEXDT from PXEFDT                        *
    H****************************************************************    *
    H*                                                                   *
    H* CREATED:  02/09/18     C:UXN    PROJECT: WC2580                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     FWMPXL1    if   e           k disk    Rename(PPX01:PPX01L1)
     F                                     Prefix(C)
     FWMPXL2    uf   e           k disk    Prefix(W)
     F
     FWMPRL     if   e           k disk
     D
     d                 ds
     d PayClsExt               1     36
     d  PXCO#X                 1      3  0 Inz
     d  PXCLASX                4      8  0 Inz
     d  PXEFDTX                9     15  0 Inz
     d  PXEXDTX               16     22  0 Inz
     d  PXSTX                 23     24
     d  PXEXTX                25     29
     d  PXEFDTXP              30     36  0 Inz

     dfirst_rec        s              1    inz('Y')

     c     Clsky         klist
     c                   kfld                    PXCO#X
     c                   kfld                    PXCLASX
     c                   kfld                    PXEFDTX
     c                   kfld                    PXSTX
     c                   kfld                    PXEXTX

     c     EffDtKy       klist
     c                   kfld                    PXCO#X
     c                   kfld                    PXCLASX

     c** Update Payroll Extension Effective Date
     c                   ExSr      PayClsEff

     c** Update Payroll Extension Expiration Date
     c                   ExSr      PayClsExp

     C                   eval      *inlr = *on

     c     PayClsEff     BegSr
     c**   This routine updates the Eff Date if not coded.

     C     *loval        setll     PPX01
     C                   read      PPX01
     C                   dow       not %eof
     c
     C                   if        WPXEFDT  = 0

     c                   eval      PXCO#X   = WPXCO#
     c                   eval      PXCLASX  = WPXCLAS

     c** If effective date is blank, get ealiest Class Code Eff Date
     c** from master table WMPRP and udpate WMPXP
     C     EffDtKy       setll     WMPRL
     C     EffDtKy       reade     WMPRL

     c                   if        %found(WMPRL)
     c                   eval      WPXEFDT = PREFDT
     c                   update    PPX01
     c                   endif
     c
     c                   endif

     C                   read      PPX01
     C                   enddo

     C                   endsr

     c     PayClsExp     BegSr

     C** Set expiration date to the effective date of subsequent/next
     c** record when multiple class code extensions exist.
     c**
     C     *hival        setgt     PPX01L1
     C                   readp     PPX01L1
     C                   dow       not %eof
     C
     c                   if        first_rec <> 'Y'

     c                   if        (PXCO#X = CPXCO# and PXCLASX = CPXCLAS and
     c                             PXSTX  = CPXST  and PXEXTX = CPXEXT)

     c                   eval      PXEFDTX  = CPXEFDT
     C     Clsky         chain     PPX01
     c                   if        %found(WMPXL2)
     c                   if        WPXEXDT = 0
     c                   eval      WPXEXDT = PXEFDTXP
     c                   update    PPX01
     c                   clear                   PayClsExt

     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      PXCO#X   = CPXCO#
     c                   eval      PXCLASX  = CPXCLAS
     c                   eval      PXEFDTXP = CPXEFDT
     c                   eval      PXSTX    = CPXST
     c                   eval      PXEXTX   = CPXEXT
     c                   eval      first_rec =  ' '


     C                   readp     PPX01L1
     C                   enddo
     C                   endsr
     C*
