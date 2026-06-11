     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WA084R - API to determine earliest policy effective date
     H**********************************************************************
     H*  Description:  This program will determine the earliest effective  *
     H*                on the policy to be used by the various             *
     H*                cancellation/notice programs.                       *
     H*                                                                    *
     H**********************************************************************
     H*  Created on 08/17/2011 by c:190 for Project WC2019                 *
     H**********************************************************************
     H* Revisions:                                                         *
     H*                                                                    *
     H*                                                                    *
     H**********************************************************************
     f*
     fwmeml     if   e           k disk
     fwzexl     if   e           k disk

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s                   like(emco#)
     D @@fnd           s                   like(emfnd)
     D @@emp#          s                   like(ememp#)
     D @@div           s                   like(emdiv)
     D xxco#           s                   like(emco#)
     D xxfnd           s                   like(emfnd)
     D xxemp#          s                   like(ememp#)
     D xxdiv           s                   like(emdiv)
     D xxodiv          s                   like(emdiv)
     D xxoemp#         s                   like(ememp#)
     D xxofnd          s                   like(emfnd)
     D @@date          s                   like(emefd1)
     D xxdate          s                   like(emefd1)
     D ##date          s                   like(emefd1)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     d prGetEffDate    pr             7  0
     d inco#                               like(emco#)
     d infnd                               like(emfnd)
     d inemp#                              like(ememp#)
     d indiv                               like(emdiv)
     d*
     d prGetXrefEMP    pr             1a
     d inco#                               like(emco#)
     d infnd                               like(emfnd)
     d inemp#                              like(ememp#)
     d indiv                               like(emdiv)
     d outofnd                             like(emfnd)
     d outoemp#                            like(ememp#)
     d outodiv                             like(emdiv)
     c*
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@fnd
     c                   parm                    @@emp#
     c                   parm                    @@div
     c                   parm                    @@date
    c*

     c                   eval      xxco# = @@co#
     c                   eval      xxfnd = @@fnd
     c                   eval      xxemp#= @@emp#
     c                   eval      xxdiv = @@div

     c                   eval      ##date = prGetEffDate(xxco#:xxfnd:xxemp#:+
     c                             xxdiv)

     c                   dow       prGetXrefEMP(xxco#:xxfnd:xxemp#:xxdiv: +
     c                             xxofnd:xxoemp#:xxodiv) = '0'

     c                   eval      ##date = prGetEffDate(xxco#:xxofnd:xxoemp#:+
     c                             xxodiv)
     c                   eval      xxfnd  = xxofnd
     c                   eval      xxemp# = xxoemp#
     c                   eval      xxdiv  = xxodiv

     c                   enddo

     c                   eval      @@date = ##date

     c                   seton                                        lr
      *=============================================================
      * prGetEffDate
      *=============================================================
     p prGetEffDate    b
     d prGetEffDate    pi             7  0
     d inco#                          3  0
     d infnd                          3  0
     d inemp#                         9  0
     d indiv                          5  0
      *
     d outdate         s              7  0 inz
     c*
     c     emky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkemp#            9 0
     c                   kfld                    kkdiv             5 0
     c*
     c                   eval      kkco# = inco#
     c                   eval      kkfnd = infnd
     c                   eval      kkemp#= inemp#
     c                   eval      kkdiv = indiv

     c                   eval      outdate = 0
     c     emky          chain     pem01                              60
     c                   if        *in60 = *off
     c                   eval      outdate = emefd1
     c                   endif
     c                   return    outdate

     p prGetEffDate    e
      *=============================================================
      * prGetXrefEMP
      *=============================================================
     p prGetXrefEMP    b
     d prGetXrefEMP    pi             1a
     d inco#                          3  0
     d infnd                          3  0
     d inemp#                         9  0
     d indiv                          5  0
     d outofnd                        3  0
     d outoemp#                       9  0
     d outodiv                        5  0
     d*
     c*
     c     exky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkemp#            9 0
     c                   kfld                    kkdiv             5 0
     c*
     c                   eval      kkco# = inco#
     c                   eval      kkfnd = infnd
     c                   eval      kkemp#= inemp#
     c                   eval      kkdiv = indiv

     c     exky          chain     pex01                              60
     c                   if        *in60 = *off
     c                   eval      outofnd = exofnd
     c                   eval      outoemp#= exofnd
     c                   eval      outodiv = exdiv
     c                   return    '0'
     c                   else
     c                   eval      outofnd = 0
     c                   eval      outoemp#= 0
     c                   eval      outodiv = 0
     c                   return    '1'
     c                   endif
     p prGetXrefEMP    e
