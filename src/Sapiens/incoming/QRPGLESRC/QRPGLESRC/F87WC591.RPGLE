     H* (C) Copyright Progressive Data Solutions, 2000
     H
     H**************************************************************************************
     H*  Description:  Looks at every exposure record,  if the state code is not filled    *
     H*                then the appropriate state code is retrieved from WMSRP and WDSEP   *
     H*                is updated.                                                         *
     H*                                                                                    *
     H*   created: 08/09/00    by: C87    project: supp 23905                              *
     H**************************************************************************************

     Fwdsep     uf   e             disk
     Fwmsrl     if   e           k disk

     C     srky          klist
     C                   kfld                    seco#
     C                   kfld                    sefnd
     C                   kfld                    serpmy
     C                   kfld                    seemp#
     C                   kfld                    sefyr

     C                   read      pse01                                  60
     C                   dow       *in60 = *off
     C
     C                   if        sestcd = 0
     C
     C     srky          chain     psr01                              61
     C                   if        *in61 = *off
     C                   eval       sestcd = srstcd
     C                   update    pse01
     C                   endif
     C
     C                   endif                                                  sestcd=0
     C
     C                   read      pse01                                  60
     C                   enddo

     C                   eval      *inlr=*on
