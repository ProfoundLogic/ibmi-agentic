     H* (C) Copyright Progressive Data Solutions, 1999
     H/TITLE CVTWC705                                                                       *
     H**************************************************************************************
     H*  Description:  Program looks at each record in WXCCP for existing cancellation     *
     H*                batches.  If CCPTDT = 0, then the value in CCBLDT is assigned to    *
     H*                CCPTDT (CCPTDT = CCBLDT)                                            *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 10/07/99   Programmer C:87    Project: WC705                             *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H**************************************************************************************
     FWXCCP     UF   E             DISK

     C                   read      PCC01                                  60
     C*
     C                   dow       *in60 = *off
     C*
     C                   if         CCPTDT = 0
     C                   eval      CCPTDT = CCBLDT
     C                   update    PCC01
     C                   endif
     C*
     C                   read      PCC01                                  60
     C                   enddo                                                  *in60=*off

     C                   eval      *inlr = *on
