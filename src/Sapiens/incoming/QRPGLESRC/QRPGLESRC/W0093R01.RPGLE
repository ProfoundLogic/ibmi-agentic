     Hcopyright('(c) Copyright StoneRiver, Inc., 2009')
     H*** SY266_OVR dftactgrp(*no) option(*nodebugio:*srcstmt)
     H Dftactgrp(*no) ActGrp(*Caller) option(*nodebugio:*srcstmt)
     H*********************************************************************
     H/TITLE W0093R01 - Third Party Billing
     H*********************************************************************
     H* CREATED: 02/03/09  C:33  PROJECT 146WBW                           *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 01/03/18  C:62  Ticket: 75935(146)                                *
     H*                 Add all divison flag to audit.                    *
     H*                                                                   *
     H* 03/21/14  C:65  Project:  140WAU (Redmine #4063)                  *
     H*                 Add 2nd a/r code parm to call to W8001C.          *
     H*                                                                   *
     H* 01/23/13  C:287 Seq#: SY266_OVR                                   *
     H*                 Added DFTACTGRP and ACTGRP compile options        *
     H*                                                                   *
     H* 06/08/09 C:189 Support: WA146 68753
     H*                Getting parameter mismatch error on W7710C call
     H*
     H*********************************************************************
     Fwdelp     if   e             disk
     F*
     D @@kc            s              1
     D @@kl            s              1
     D @@Mode          s              1
     D finalQuote      s              1
     D @@RRN           s              9  0
     D @@Qte           s              1
     d @@co#           s              3
     d @@fnd           s              3
     d @@emp#          s              9
     d @@div           s              5
     d @@fyr           s              3
     d @@rev           s              3
     d @@postDate      s              8
     D*
     D step            s              2  0 inz
     D*
     DEndProc          pr
     D*
     DProcessing       pr             1n
     D inStep                         2  0 const
     D*
     DStartProc        pr
     D*
     DW0093R           pr                  extPgm('W0093R')
     D inKC                                like(*inkc)
     D inKL                                like(*inkl)
     D Mode                           1                 const
     D rrn                                 like(@@rrn)  const
     D*
     DW0093R1          pr                  extPgm('W0093R1')
     D Co#                                 like(ELCO#)  const
     D Fnd                                 like(ELFND)  const
     D Emp#                                like(ELEMP#) const
     D Div                                 like(ELDIV)  const
     D Fyr                                 like(ELFYR)  const
     D Mode                           1                 const
     D*
     DW0093R2          pr                  extPgm('W0093R2')
     D inKC                                like(*inkc)
     D inKL                                like(*inkl)
     D Mode                           1                 const
     D rrn                                 like(@@rrn)  const
     D qte                                 like(@@qte)  const
     D*
     DW0094R           pr                  extPgm('W0094R')
     D Mode                           1                 const
     D rrn                                 like(@@rrn)  const
     D inKC                                like(*inkc)
     D inKL                                like(*inkl)
     D*
     DW7710C           pr                  extPgm('W7710C')
     D Co#                            3    const
     D Fnd                            3    const
     D Fyr                            3    const
     D Emp#                           9    const
     D Div                            5    const
     D date                           8    const
     D code                           2    const
     D com1                           2    const
     D com2                           2    const
     D com3                           2    const
     D rev                            1    const
     D que                           10    const
     D rmcd                           5    const
     D free1                         30    const
     D free2                         30    const
     D free3                         30    const
     D cont                           1    const
     D fadc                           5    const
     D fasc                           3    const
      ***** BEG ADD ******* 06/08/09 **********************************
     D idbd                           1    const
      ***** END ADD ******* 06/08/09 **********************************
      ***** BEG ADD ******* 01/03/19 **********************************
     D allDivision                    1    const
      ***** END ADD ******* 01/03/19 **********************************
     D*
     DW8001C           pr                  extPgm('W8001C')
     D Co#                            3    const
     D Fnd                            3    const
     D Fyr                            3    const
     D Emp#                           9    const
     D Div                            5    const
     D date                           8    const
     D code                           2    const
     D com1                           2    const
     D com2                           2    const
     D com3                           2    const
     D interimDays                    8    const
     D prtBal                         1    const
     D stl                            1    const
     D prt                            1    const
     D ovr                            1    const
     D srcd                           2    const
     D ajcd                           2    const
     D qte                            1    const
     D que                           10    const
     D rnb                            1    const
     D dart                           1    const
     D hard                           1    const
     D agto                           1    const
     D fqt                            1    const
     D rmcd                           5    const
     D ar                             2    const
     D effDate                        8    const
     D free1                         30    const
     D free2                         30    const
     D free3                         30    const
     C*** begin add ***  03/21/14  **************************************************
     D ar2                            2    const
     C*** end add *****  03/21/14  **************************************************
     D*
     Dparms            ds          2000
     D postDt                         8
     D printBal                       1
     D interimDays                    8
     D commentCode1                   2
     D commentCode2                   2
     D commentCode3                   2
     D freeComment1                  25
     D freeComment2                  25
     D freeComment3                  25
     D resolve                        1
     D remarkCode                     5
     D hardCopy                       1
     D userOutQ                      10
     D*
     C     *dtaara       Define    billParms     parms
     C*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@Mode
     C                   parm                    @@RRN
     C                   parm                    @@Qte
     C*
     C                   eval      *inkc = *off
     C                   eval      *inkl = *off
     C*
     C                   callp     StartProc
     C*
     C                   callp     Processing(1)
     C*
     C                   eval      *inlr = *on
     C*====================================================================
     C* End Processing
     C*====================================================================
     PEndProc          b
     DEndProc          pi
     C*
     C                   callp     W0093R1(ELCO#
     C                                    :ELFND
     C                                    :ELEMP#
     C                                    :ELDIV
     C                                    :ELFYR
     C                                    :'2'
     C                                    )
     C*
     C                   exsr      CallSubmit
     C*
     C*====================================================================
     C* Call audit bills
     C*====================================================================
     C     AuditBills    begsr
     c                   move(p)   postDt        @@PostDate
     c                   if        ELRAFL = 'Y'
     c                   eval      @@rev = '4'
     c                   else
     c                   eval      @@rev = '1'
     c                   endif
     C                   callp     W7710C(@@co#
     C                                   :@@fnd
     C                                   :@@fyr
     C                                   :@@emp#
     C                                   :@@div
     C                                   :@@PostDate
     c                                   :' '                                   arCode
     C                                   :commentCode1
     C                                   :commentCode2
     C                                   :commentCode3
     C                                   :@@rev                                 revision
     C                                   :userOutQ
     C                                   :remarkCode
     C                                   :freeComment1
     C                                   :freeComment2
     C                                   :freeComment3
     C                                   :'3'                                   contribution
     C                                   :' '
     C                                   :' '
      ***** BEG ADD ******* 06/08/09 **********************************
     C                                   :' '
      ***** END ADD ******* 06/08/09 **********************************
      *** Begin Add *** 01/03/19 **************************************
     C                                   :' '
      *** End   Add *** 01/03/19 **************************************
     C                                   )
     C
     C                   endsr
     C*====================================================================
     C* Call sumbitter programs
     C*====================================================================
     C     CallSubmit    begsr
     C*
     C     *lock         in        parms
     C                   out       parms
     C*
     C                   select
     C                   when      ELFAF = 'A'
     C                   exsr      AuditBills
     C                   other
     C                   exsr      PrintBills
     C                   endsl
     C*
     C                   endsr
     C*====================================================================
     C* Call print bills
     C*====================================================================
     C     PrintBills    begsr
     c                   move(p)   postDt        @@PostDate
     c                   if        @@Qte = 'A'
     c                   eval      finalQuote = 'Y'
     c                   else
     c                   eval      finalQuote  = ' '
     c                   endif
     C                   callp     W8001C(@@co#
     C                                   :@@fnd
     C                                   :@@fyr
     C                                   :@@emp#
     C                                   :@@div
     C                                   :@@PostDate
     C                                   :' '                                      ESCD
     C                                   :commentCode1
     C                                   :commentCode2
     C                                   :commentCode3
     C                                   :interimDays
     C                                   :printBal
     c                                   :'3'                                      stl
     C                                   :' '                                      prt
     C                                   :' '                                      ovr
     C                                   :' '                                      srcd
     C                                   :' '                                      ajcd
     C                                   :@@qte                                    qte
     C                                   :userOutQ                                 que
     C                                   :'B'                                      rnb
     C                                   :resolve
     C                                   :hardCopy
     C                                   :'N'                                      agtto
     C                                   :finalQuote                               fqt
     C                                   :remarkCode
     C                                   :' '                                      ar
     C                                   :'00000000'                               efdt
     C                                   :freeComment1
     C                                   :freeComment2
     C                                   :freeComment3
     C*** begin add ***  03/21/14  **************************************************
     C                                   :' '                                      ar2
     C*** end add *****  03/21/14  **************************************************
     C                                   )
     C                   endsr
     C
     PEndProc          e
     C*====================================================================
     C* Processing (Uses recurrsion)
     C*====================================================================
     PProcessing       b
     DProcessing       pi             1n
     D inStep                         2  0 const
     D*
     D getOut          s              1n   inz
     D F12             s              1n   inz
     C*
     C                   dou       *inkc or F12 or getOut
     C*
     C     inStep        caseq     1             Step1
     C     inStep        caseq     2             Step2
     C     inStep        caseq     3             Step3
     C                   cas                     LastStep
     C                   endcs
     C*
     C                   if        not *inkc and not *inkl and not getOut
     C                   eval      getOut = Processing(inStep+1)
     C                   endif
     C*
     C                   eval      F12    = *inkl
     C                   eval      *inkl  = *off
     C*
     C                   enddo
     C*
     C                   return    getOut
     C*====================================================================
     C* Last Step
     C*====================================================================
     C     LastStep      begsr
     C                   callp     EndProc
     C                   eval      getOut = *on
     C                   endsr
     C*====================================================================
     C* Process 1st Step
     C*====================================================================
     C     Step1         begsr
     C                   callp     W0093R(*inkc
     C                                   :*inkl
     C                                   :'C'
     C                                   :@@rrn
     C                                   )
     C                   endsr
     C*====================================================================
     C* Process 2nd Step
     C*====================================================================
     C     Step2         begsr
     C                   callp     W0094R(' '
     C                                   :@@rrn
     C                                   :*inkc
     C                                   :*inkl
     C                                   )
     C                   endsr
     C*====================================================================
     C* Process 3rd Step
     C*====================================================================
     C     Step3         begsr
     C                   callp     W0093R2(*inkc
     C                                    :*inkl
     C                                    :' '
     C                                    :@@rrn
     C                                    :@@qte
     C                                    )
     C                   endsr
     C*====================================================================
     C* Process 4th Step
     C*====================================================================
     C     Step4         begsr
     C                   endsr

     PProcessing       e
     C*====================================================================
     C* Start Processing
     C*====================================================================
     PStartProc        b
     DStartProc        pi
     C*
     C     @@RRN         chain     WDELP
     c                   move(p)   ELCO#         @@co#
     c                   move(p)   ELFND         @@fnd
     c                   move(p)   ELEMP#        @@emp#
     c                   move(p)   ELDIV         @@div
     c                   move(p)   ELFYR         @@fyr
     C*
     C                   callp     W0093R1(ELCO#
     C                                    :ELFND
     C                                    :ELEMP#
     C                                    :ELDIV
     C                                    :ELFYR
     C                                    :'1'
     C                                    )
     C*
     PStartProc        e
