     H*      Copyright Progressive Data Solutions, Inc., 2000
    H*
     H*/TITLE W8114R1 -
    H*********************************************************************
    H*  Description: Populates WXFNP workfile for Madison download.      *
    H*                                                                   *
    H*  Called from: W8114C1                                             *
    H*                                                                   *
    H*  Calls to: WA001R                                                 *
    H*                                                                   *
    H*  Created: 05/01/00      C:87      Project: WC964                  *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  xx/xx/xx  C:xx  Project: xxxxx                                                    *
     H*                  Base project is installed as a mod for client.                    *
    H*                                                                                    *
    H**************************************************************************************
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxfnp     uf a e             disk
    F*
    F*  Fund master file
     Fwmfnl     if   e           k disk
     F                                     rename(pfn01:pf201)
     F                                     prefix(f2:2)
     C*
     C                   call      'WA001R'
     C                   parm                    ##co#             3 0
     C                   parm                    ##fnd             3 0
     C                   parm                    ##conm           40
     C                   parm                    ##fnam           40
     C*
     C     ##co#         chain     pf201                              60
     C                   dow       *in60=*off
     C
     C                   eval      fnco# = f2co#
     C                   eval      fnfnd = f2fnd
     C                   eval      fnname = f2name
     C                   eval      fnflag = 'E'
     C
     C                   write     pfn01
     C*
     C     ##co#         reade     pf201                                  60
     C                   enddo
     C*
     C                   eval      *inlr=*on
