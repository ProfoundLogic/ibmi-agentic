    Hcopyright ('(c) Copyright StoneRiver, Inc., 2009')
    H*****************************************************************
    H/TITLE WA723R - Check for Indemnity Payments
    H*****************************************************************
    H*                                                               *
    H*  Description:                                                 *
    H*    Reads thru the payment file and determine if there are any *
    H*    compensation payments.  Return Y if one is found else N.   *
    H*    Parameters:                                                *
    H*      @@CO#  - Company Number - Input                          *
    H*      @@FND  - Group Number   - Input                          *
    H*      @@FYR  - Year           - Input                          *
    H*      @@CASE - Claim Number   - Input                          *
    H*      @@RTCD - Return Code    - Output                         *
    H*                                                               *
    H*  Called from:                                                 *
    H*    W0070R12 - Claim Reserve/Paytments                         *
    H*    W8608R   - Claim Auto Closure Report                       *
    H*                                                               *
    H*  Calls to:                                                    *
    H*    None                                                       *
    H*                                                               *
    H*****************************************************************
    H*  Created on 09/11/2009 by c:62 for Project 133WGP             *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*****************************************************************
    F*  Payment File
     Fwdpala    if   e           k disk
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s                   like(paco#)
     D @@fnd           s                   like(pafnd)
     D @@fyr           s                   like(pafyr)
     D @@case          s                   like(pacase)
     D @@rtcd          s              1a
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@rtcd
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     paky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    @@fyr
     C                   kfld                    @@case
    C*================================================================
    C*  Normal Programming Area
    C*================================================================
    C*
     C                   eval      @@rtcd='N'
     C     paky          chain     ppa01
     C                   if        %found
     C                   dou       %eof
     C*
     C                   if        patype='P' and paadst=' ' and paretp='C'
     C                   eval      @@rtcd='Y'
     C                   leave
     C                   endif
     C*
     C     paky          reade     ppa01
     C                   enddo
     C                   endif
    C*
     C                   return
