     H copyright('(C) Copyright Progressive Data Solutions, Inc., 2003')
    H*********************************************************************
    H/title Delete A/R Records
    H*********************************************************************
    H*  Description:                                                     *
    H*   This program will remove all A/R records (WMEAP/WMXAP) for a    *
    H*    given CO#/FND/FYR.  If the EMP# is entered without a division, *
    H*    all A/R for that employer only (ALL division) will be deleted. *
    H*    If the EMP# and DIV are entered, only the A/R records for that *
    H*    employer/division will be deleted.                             *
    H*                                                                   *
    H*  Created: 11/11/03      C:33      Project: WA071/39899            *
    H*********************************************************************

     Fwmeal10   uf   e           k disk    rename(pea01:pea10)
     Fwmxal     uf   e           k disk    rename(pxa01:pxa00)

     D/copy sprstring

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@Co#           s              3
     D @@Div           s              5
     D @@Emp#          s              9
     D @@Fnd           s              3
     D @@Fyr           s              3
     D xxCo#           s                   like(EACO#)
     D xxDiv           s                   like(EADIV)
     D xxEmp#          s                   like(EAEMP#)
     D xxFnd           s                   like(EAFND)
     D xxFyr           s                   like(EAFYR)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     fyrDltKy      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
    C*
     C     empDltKy      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    xxEmp#
    C*
     C     divDltKy      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
    C*
     C     *entry        plist
     C                   parm                    @@Co#
     C                   parm                    @@Fnd
     C                   parm                    @@Fyr
     C                   parm                    @@Emp#
     C                   parm                    @@Div
     *
     C                   eval      @@Co# = rightJstfy(@@Co#)
     C                   eval      @@Fnd = rightJstfy(@@Fnd)
     C                   eval      @@Fyr = rightJstfy(@@Fyr)
     *
     C                   move      @@Co#         xxCo#
     C                   move      @@Fnd         xxFnd
     C                   move      @@Fyr         xxFyr
     *
     C                   if        %parms >= 4 and @@Emp# <> *blank
     C                   eval      @@Emp# = rightJstfy(@@Emp#)
     C                   move      @@Emp#        xxEmp#
     C                   endif
     *
     C                   if        %parms >= 5 and @@Div <> *blank
     C                   eval      @@Div = rightJstfy(@@Div)
     C                   move      @@Div         xxDiv
     C                   endif
     *
     *  Delete records from the A/R Detail Files
     *
     C                   dou       not %found
     C                   select
     C                   when      %parms >= 5 and @@Div <> *blank
     C     divDltKy      delete    pea10
     C                   when      %parms >= 4 and @@Emp# <> *blank
     C     empDltKy      delete    pea10
     C                   when      %parms >= 3
     C     fyrDltKy      delete    pea10
     C                   endsl
     C                   enddo
     *
     C                   dou       not %found
     C                   select
     C                   when      %parms >= 5 and @@Div <> *blank
     C     divDltKy      delete    pxa00
     C                   when      %parms >= 4 and @@Emp# <> *blank
     C     empDltKy      delete    pxa00
     C                   when      %parms >= 3
     C     fyrDltKy      delete    pxa00
     C                   endsl
     C                   enddo
     *
     C                   move      *on           *inlr
