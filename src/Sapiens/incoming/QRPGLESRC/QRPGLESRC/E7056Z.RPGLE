     H***************************************************************
     H* (C) Copyright Progressive Data Solutions, 2004
     H***************************************************************
     H* E7056S    Raise value to fractional Power for Mass.
     H***************************************************************
     H*
     H* Given paraameters:   Return code 1A
     H*                      Value:    5.4
     H*                      Exponent: 5.4
     H*                      Result:   5.4
     H*
     H* Return:
     H*   Value = received value, raised to power of exponent,
     H*           or zero if failed
     H*
     H*   Return codes:   'Z' zero result
     H*                   'X' overflow
     H*                   ' ' successful
     H*
     H***************************************************************
     H* CREATED            8.28.04    C:58
     H* REVISIONS:
     H***************************************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)


     D@@rtcd           s              1
     D@@amt            s              5  4
     D@@expt           s              5  4
     D@@rslt           s              5  4

     D@@anxr           s             15  4

      *================================================================
      * PLIST
      *================================================================

     C     *entry        plist
     C                   parm                    @@rtcd
     C                   parm                    @@amt
     C                   parm                    @@expt
     C                   parm                    @@rslt


     c                   eval      @@anxr = @@amt ** @@expt


     c     @@anxr        ifeq      *zero
     c                   eval      @@rtcd = 'Z'
     c                   endif

     c     @@anxr        ifgt      9
     c                   eval      @@rtcd = 'X'
     c                   endif
     c                   except    oute


     c                   z-add     @@anxr        @@rslt

     C                   seton                                        lr

      *=================================================================================
     OQSYSPRT   E            OUTE           1 06
     O                                           19 'Input value:'
     O                       @@amt         j     28
     O                                           39 'Exponent:'
     O                       @@expt        j     48
     O                                           57 'Result:'
     O                       @@anxr        j     66
     O                                           75 'Code:'
     O                       @@rtcd              77
