     H* (C) Copyright Fiserv Insurance Solutions, 2006
     H***************************************************************
     *** Retrieve iDarts Control Values from SDRPF file
     H*  Created:
     H*             10/25/06  C:02     Pjct: DR214
     H*                  Add iDarts Receive Dataq  (DRRDTQ of SDRPF)
     H*                  Add iDarts Send Dataq     (DRSDTQ of SDRPF)
     H*                  Add iDarts Dataq Library  (DRDTQL of SDRPF)
     H*                  Add iDarts Response wait time (DRDT05 of SDRPF)
     H*  Revisions:
     H*
     H*             07/06/07  C:02     Pjct: DR214A
     H*                  Add Citrix/Go Flag (DRFL14)
     H*
     H***************************************************************
     Fsdrpf     if   e             disk
     F
      *
     C     *entry        plist
     C                   parm                    DRRDTQ
     C                   parm                    DRSDTQ
     C                   parm                    DRDTQL
     C                   parm                    DRDT05
     **** add 07/06/07 ************************************************************************
     c                   parm                    XXFL14            1             Citrix/GO Flag
     **** END add 07/06/07 ********************************************************************
     **
     C                   read      sdrpf                                  99
     **
     **** add 07/06/07 ************************************************************************
     C                   if        %parms > 4
     c                   eval      XXFL14 = DRFL14
     C                   endif
     **** END add 07/06/07 ********************************************************************
     C
     C                   seton                                        lr
