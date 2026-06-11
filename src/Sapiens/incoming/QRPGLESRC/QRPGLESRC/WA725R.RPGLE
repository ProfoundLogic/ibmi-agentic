    Hcopyright ('(c) Copyright StoneRiver, Inc., 2009')
    H*****************************************************************
    H/TITLE WA725R - WC990306 mailing/physical states
    H*****************************************************************
    H*                                                               *
    H*  Description:                                                 *
    H*    Accepts state abreviation code.
    H*    Return Y if client ID is 132 and mid-term endorsement
    H*    WC990306 should be used.
    H*                                                               *
    H*  Called from:                                                 *
    H*    SR99306  - WC990306 print/attach program
    H*    SR00313  - WC000313 print/attach program
    H*                                                               *
    H*  Calls to:                                                    *
    H*    None                                                       *
    H*                                                               *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  12/02/09  C:61  132WMP                                       *
    H*                  Created.                                     *
    H*                                                               *
    H*****************************************************************

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@state         s              2
     D @@rtcd          s              1a


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm                    @@state
     C                   parm                    @@rtcd


    C*================================================================
    C*  Normal Programming Area
    C*================================================================
    
     C                   eval      @@rtcd='N'
    
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3

     c                   if        cltid = '132'

     c                   if        (@@state = 'AZ')
     c                             or (@@state = 'CA')
     c                             or (@@state = 'CO')
     c                             or (@@state = 'ID')
     c                             or (@@state = 'NV')
     c                             or (@@state = 'OR')
     C                   eval      @@rtcd='Y'
     c                   endif                                                   <-----+ : :

     c                   endif                                                   <-----+ : :
    
     C                   return
    
