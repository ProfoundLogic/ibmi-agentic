      *****************************************************************
      * (C) Copyright Fiserv Insurance Solutions, Inc., 2006
      *****************************************************************
      * Fix program for QFEUP/EUNWRW                                  *
      *                                                               *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *****************************************************************
      *
     * Quest Calendar Earned/Unearned
     FQFEUP     UF   E             Disk
      *
     * Employer Detail
     FWDELL     IF   E           K Disk
     *
     * Employer Cross Reference
     FWZEXL     IF   E           K Disk
      *
     D renewalFlag     s              1
     C     elky          klist
     C                   kfld                    EUCO#
     C                   kfld                    EUFND
     C                   kfld                    EUEMP#
     C                   kfld                    EUDIV
     C                   kfld                    EUFYR
      *
     C     exky          klist
     C                   kfld                    EUCO#
     C                   kfld                    EUFND
     C                   kfld                    EUEMP#
     C                   kfld                    EUDIV
      *
      **********************************************************************
      *   M A I N L I N E   R O U T I N E                                  *
      **********************************************************************
     C                   read      peu01
     C                   dow       not %eof
      *
     c     elky          setll     pel01
     c     exky          readpe    pel01
     c                   if        not(%eof)
     c                   eval      renewalFlag = 'R'
     c                   else
     c     exky          chain     pex01
     c                   if        %found
     c                   eval      renewalFlag = 'R'
     c                   else
     c                   eval      renewalFlag = 'N'
     c                   endif
     c                   endif

     c                   if        EUNWRW <> renewalFlag
     c                   eval      EUNWRW = renewalFlag
     c                   update    peu01
     c                   endif

     C                   read      peu01
     C                   enddo
      *
     C                   Eval      *Inlr = *On
