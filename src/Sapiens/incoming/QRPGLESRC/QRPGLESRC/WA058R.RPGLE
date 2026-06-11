    H* (C) FISERV INSURANSE SOLUTIONS, 2005
    H*****************************************************************
     H dftactgrp(*no) actgrp(*caller)
    H/TITLE WA058R - API TO AUTO CALCULATE THE COMP PAYMENT AMOUNT
    H*****************************************************************
    H*  Description:
    H*
    H*    BASED ON THE STATE COMP BENEFIT FILES DETERMINE IF A
    H*    PAYMENT SHOULD BE CALCUATED AND THE AMOUNT OF THE PAYMENT
    H*
    H*  Called From:
    H*    Program
    H*
    H*  Calls To:
    H*
    H*    None
    H*
    H*  Created:  04/20/05  C:53  Project:  WC1279
    H*
    H*****************************************************************
    H*  Revisions:
    H*
     H* 09/07/16 C:307 Support: 42765                                 *
     H*                -Code changes to correct the auto calculation  *
     H*                 of comp check.                                *
    H*
    H*****************************************************************
    H*  Indicators:
    H*
    H*    99 - General File Access
    H*****************************************************************
    H*
     Fwdr1l     if   e           k disk
     Fwdrbl     if   e           k disk
     Fwmcml1    if   e           k disk
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkbr2           s                   like(r1br2)
     D kkcase          s                   like(cmcase)
     D kkco#           s                   like(cmco#)
     D kkefdt          s                   like(r1efdt)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkjrst          s                   like(r1st)
     D qqamt           s              9  2
     D qqbr2           s              2
     D qqcase          s              7
     D qqco#           s              3
     D qqdays          s              1
     D qqfnd           s              3
     D qqfyr           s              3
     D qqwks           s              3
     D xxamt           s              9  2
     D xxdays          s              1  0
     D xxpday          s              9  5
     D xxrate          s                   like(r1rate)
     D xxwkrt          s              7  2
     D xxwks           s              3  0
     D x1rate          s                   like(r1rate)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     cmkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
     C     r1kset        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkjrst
     C                   kfld                    kkbr2
     C                   kfld                    kkefdt
    C*
     C     r1key         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkjrst
     C                   kfld                    kkbr2
    C*
     C     rrkset        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkjrst
     C                   kfld                    kkefdt
    C*
     C     rrkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkjrst
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqwks
     C                   parm                    qqdays
     C                   parm                    qqbr2
     C                   parm                    qqamt
    C*
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqfyr         kkfyr
     C                   move      qqcase        kkcase
     C                   move      qqbr2         kkbr2
     C                   move      qqwks         xxwks
     C                   move      qqdays        xxdays
    C*
     C     cmkey         chain     pcm01                              99
     C     *in99         cabeq     *on           end
    C*
     C                   move      cmacdt        kkefdt
     C                   move      cmjrst        kkjrst
    C*
     C     r1kset        setgt     pr101
     C     r1key         readpe    pr101                                  99
    C*
     C                   if        *in99 = *off
     C                   eval      xxrate = r1rate
    C*
     C                   else
     C     rrkset        setgt     prb01
     C     rrkey         readpe    prb01                                  99
     C                   eval      xxrate = rbcmpr
     C                   endif
    C*
     C     *in99         cabeq     *on           end
    C*
    C*** THE WEEKLY RATE = WEEKLY COMP RATE * AVG WEEKLY WAGE
    C*
     C                   eval(h)   x1rate = xxrate / 100
     C                   eval      xxwkrt = cmaww * x1rate
    C*
    C*** THE PAYMENT DAYS = WEEKLY COMP RATE * AVG WEEKLY WAGE
    C*
?LK  C                   if        cmdays <= *zeros
     C                   eval      cmdays = 7
     C                   endif
    C*
     C                   if        xxdays > *zeros
     C                   eval      xxpday = xxdays / cmdays
     C                   endif
    C*
    C*** ADD IN THE WEEKS TO THE PAYMENT DAYS AND CALCULATE
    C*
     C                   eval      xxpday = xxpday + xxwks
     C                   eval(h)   xxamt = xxpday * xxwkrt
     C                   eval      qqamt = xxamt
     ***** Beg Add ***** 09/07/16 ************************************
      /Free
       //*Check to see if minimum amount applies
       If qqAmt<RBmnrt;
       qqAmt=RBmnrt;
       EndIf;

       //*Check to see if maximum amount applies
       If qqAmt>RBpacp;
       qqAmt=RBpacp;
       EndIf;
      /End-free
     ***** End Add ***** 09/07/16 ************************************
    C*
     C     end           tag
    C*
     C                   move      *on           *inlr
     C                   return
