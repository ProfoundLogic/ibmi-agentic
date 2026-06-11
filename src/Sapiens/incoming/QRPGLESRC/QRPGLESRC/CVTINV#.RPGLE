     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('CVTINV#')
    H*********************************************************************
    H/title CVTINV# - A/R Statement # Conversion                         *
    H*********************************************************************
    H*  Created: 11/19/09   C:33   Project: 25WQI                        *
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*********************************************************************
     Fwmeal6    uf   e           k disk    rename(pea01:pea06) infds(eaFDS6)

     DeaFDS6           ds
     D eaRRN6                397    400b 0

     D                 ds
     D sysDtTime                       z
     D  sysDate                        d   overlay(sysDtTime)
     D  sysTime                        t   overlay(sysDtTime:12)

     DsaveEAEMP#       s                   like(EAEMP#)
     DsaveEAPODT       s                   like(EAPODT)

     Dpsds           esds                  extname(szq1p)

     D/copy sPrRunCmd
     D/copy sPrDate
     D/copy wPrAR
     D/copy srInv#pr
     D*entry

     C                   dou       %eof
     C*
     C                   read      wmeal6
     C*
     C                   if        EAEMP# <> saveEAEMP#
     C                              or EAPODT <> saveEAPODT
     C                   eval      saveEAEMP# = EAEMP#
     C                   eval      saveEAPODT = EAPODT
     C                   eval      sysDtTime = %timestamp
     C                   endif
     C*
     C                   if        not %eof
     C                              and EANINV = *zeros
     C                              and BillType(EAARTY)
     C                              and EAPODT <> *zero
     C*
     C                   eval      EANINV = RetrieveInv#(EACO#
     C                                                  :EAFND
     C                                                  :EAEMP#
     C                                                  :EADIV
     C                                                  :EAFYR
     C                                                  :CvtToDate(EAPODT)
     C                                                  )
     C*
     C                   if        EANINV <> *zero
     C* * * * *          exsr      MaintFieldsEA
     C                   update    pea06
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo

     C                   eval      *inlr = *on
     C*
     C*====================================================================
     C     MaintFieldsEA begsr
     C                   eval      EAUSER = q1user
     C                   eval      EAPGM  = q1pgm
     C                   eval      EADT   = DateTo7(sysDate)
     C                   eval      EATIME = TimeTo6(sysTime)
     C                   adddur    1:*s          sysDtTime
     C                   endsr
