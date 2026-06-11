     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H optimize(*none)
     H*****************************************************************
     H/TITLE WA080R - RDP API
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program distributes the Retro amount and write the A/R   *
     H* records depending on the mode from which its being called.    *
     H* It has the logic to split the RDP refund into installments    *
     H* to recognize and appropriately handle the RDP refund          *
     H* installments. API will write RDP payments with transaction    *
     H* and post dates in the future                                  *
     H*                                                               *
     H* Its called in 3 modes                                         *
     H*                                                               *
     H* MODE = 'D' Called from Retroprocessing and writes A/R records *
     H*            to WDEAP                                           *
     H* MODE = 'E' Called from Estimated Billing & updates A/R records*
     H*            to WMEAP                                           *
     H* MODE = 'R' Called from Estimated Billing & updates A/R records*
     H*            to WMEAP                                           *
     H*                                                               *
     H* CALLS FROM: W8010R , W8011R , W7996R1                         *
     H*                                                               *
     H* CALLS TO:   NONE                                              *
     H*                                                               *
     H* CREATED:                                                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71 - 80  Used for error messaging                            *
     H*  85 - 88  Used for positioning cursor                         *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     H*
     D*
     D*================================================================
     D*  Define all the variables
     D*================================================================
     Fwdell     if   e           k disk
     Fwdf1p     if   e           k disk
     Fwmeap     if a e           k disk
     Fwdeap     if a e           k disk    rename(pea01:pea02)
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     d exit            s              1    inz('N')
     d x               s              2  0
     d qqmode          s              1
     d qqco#           s                   like(elco#)
     d qqfnd           s                   like(elfnd)
     d qqemp#          s                   like(elemp#)
     d qqdiv           s                   like(eldiv)
     d qqfyr           s                   like(elfyr)
     d qqbat#          s                   like(eabat#)
     d curBSCD         s                   like(elbscd)
     d prvBSCD         s                   like(elbscd)
     d policyFromDate  s                   like(elpfdt)
     d policyEXPDate   s                   like(elptdt)
     d billStartDate   s                   like(elbsdt)
     d processDate     s                   like(elbsdt)
     d retroCreditCod  s                   like(F1RCCD)
     d retroAmt        s                   like(EAAMT)
     d installDats     s              7    dim(14)
     d installAmts     s             11    dim(14)
     d nxtBat#         s                   like(eabat#)
     d systemDate      s                   like(eadt)
     d systemTime      s                   like(eatime)
     d retroType       s                   Like(eaarty)
     d retroCategory   s                   Like(eactyp)
     d retroDesc       s                   Like(eadesc)
     d   YesORno       s              1
     d systemCode      s             10
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     d getinfo         pr
     d exit                                like(qqmode)
     d getRetroAmt     pr
     d exit                                like(qqmode)
     d getProcessDate  pr             7  0
     d getInstDates    pr
     d writeDummyRcd   pr
     d writeWDEAP      pr
     d writeWMEAP      pr
     d adjustWMEAP     pr
     d deleteDummyRcd  pr
     d writeInstments  pr
     d updateBatchTot  pr
     d BillPlanChgd    pr             1
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    qqmode
     C                   PARM                    qqco#
     C                   PARM                    qqfnd
     C                   PARM                    qqemp#
     C                   PARM                    qqdiv
     C                   PARM                    qqfyr
     C                   PARM                    qqbat#
     C*
     C                   time                    dsdttm
     c
     c                   Movel     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   Movel     srdto         systemdate
     c                   Movel     dstime        systemtime
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     c     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C
     C*================================================================
     C*  Mainline Code starts here
     C*================================================================
      *
     c                   Callp     getInfo(Exit)
     c                   if        Exit = 'Y'
     c                   return
     c                   endif
     C
     c                   Callp     getRetroAmt(EXIT)
     c                   if        Exit = 'Y'
     c                   return
     C                   endif
     c
     c                   callp     getInstDates
     c
     c                   select
     c
     c* Called from Retro Processing (W7996R1)
     c                   when      qqmode = 'D'
     c
     c                   callp     writeWDEAP
     c                   callp     updateBatchTot
     c
     c* Called from Estimated Billing (W8010R)
     c                   when      qqmode = 'E'
     c
     c                   if        YesOrNo = 'Y'
     c                   callp     adjustWMEAP
     c                   endif
     c
     c* Called from Revise Billing (W8011R)
     c                   when      qqmode = 'R'
     c
     c                   if        YesOrNo = 'Y'
     c                   callp     adjustWMEAP
     c                   endif
     c
     c                   endsl
     c
     c                   exsr      pgmend
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     pgmend        begsr
     c
     C                   seton                                        LR
     C                   return
     c
     C                   endsr
     c*=====================================================================---*
     c* getinfo    - It gathers various pieces of information about the policy
     c*=====================================================================---*
     p getinfo         B
     d
     d getinfo         PI
     d exit                           1


     c*
     c                   eval      exit = 'N'
     c                   select
     c                   when      qqmode = 'E' or qqmode = 'D'
     c
     c/exec SQL
     c+       select elbscd, elpfdt, elptdt, elbsdt
     c+         into :curBSCD, :policyFromDate, :policyExpDate, :billStartDate
     c+                       from WDELP where
     c+                            elco#  = :qqco#  and
     c+                             elfnd  = :qqfnd  and
     c+                               elemp# = :qqemp# and
     c+                                 eldiv  = :qqdiv  and
     c+                                   elfyr  = :qqfyr
     c/end-exec
     c
     c                   when      qqmode = 'R'
     c
     c/exec SQL
     c+
     c+   select ELBSCD, ELPFDT, ELPTDT, ELBSDT
     c+        into :curBSCD, :policyFromDate, :policyExpDate, :billStartDate
     c+                    from WAELP  where
     c+                        elco#  = :qqco#  and
     c+                          elfnd  = :qqfnd  and
     c+                             elemp# = :qqemp# and
     c+                               eldiv  = :qqdiv  and
     c+                                 elfyr  = :qqfyr
     c/end-exec
     c
     c                   endsl
     c
     c*IF no record found
     c                   if        curBSCD = 0
     c                   eval      exit = 'Y'
     c                   return
     c                   endif
     c
     c*If called from billing, check to see if the bill plan changed.  If not, there is no need for
     c*any more processing.
     c
     c                   if        qqmode = 'E' or qqmode = 'R'
     c                   Eval      YesOrNo=  billPlanChgd
     c
     c                   if        YesOrNo = 'N'
     c                   eval      exit = 'Y'
     c                   return
     c
     c                   endif
     c                   endif
     c*
     c*  Use the following SQL statement to retrieve the retro A/R code from WDF1P (Group Detail).
     c*  . If this statement fails to populate the variables return to the calling program
     c*
     c/exec SQL
     c+     select F1RCCD into :retroCreditCod
     c+      from WDF1P
     c+         where f1co#  = :QQCO#  and
     c+            f1fnd  = :QQFND  and
     c+            f1fyr  = :QQFYR
     c/end-exec
     c
     c                   if        retroCreditCod  = *blank
     c                   eval      exit = 'Y'
     c                   return
     c                   endif
     c
     c
     c*Store the value of the process date that will be passed to SRINST (Installment Date API).
     c*
     c                   select
     c                   when      QQMODE = 'D' or  QQMODE = 'E'
     c                   eval      processDate = billStartDate
     c
     c                   when      QQMODE = 'R'
     c                   eval      processDate = getProcessDate
     c
     c                   endsl
     c
     c*Get the A/R description, type, and category from the A/R Code Transaction Table (WTTTP)
     c
     c/exec SQL
     c+     select tttype, ttctyp, ttdesc
     c+          into :retroType, :retroCategory, :retroDesc
     c+             from WTTTP
     c+                 where ttkey  =  :retroCreditCod
     c/end-exec
     c
     c*Get the system code for writing A/R Batch if called from RDP
     c
     c                   if        QQMODE = 'D'
     c
     c/exec SQL
     c+   select sykey into :systemCode
     c+      from WTSYP
     c+        where sytype  =  'Y' and
     c+            systat <>  'I'
     c/end-exec
     c
     c                   endif
     p getinfo         E
     c*=====================================================================---*
     c* getRetroAmt- getRetroAmt  retrieves the amount of Retro Credit that needs to be processed
     c*=====================================================================---*
     p getRetroAmt     B
     d
     d getRetroAmt     PI
     d exit                           1
     c
     c
     c                   eval      exit = 'N'
     c                   select
     c                   when         QQMODE = 'D'
     c
     c
     c*Use the following SQL statement to retrieve the retro credit from the RDP file when called
     c*from W7996R1(RDP). It pulls the Retro Credit amount from the Retro Credit workfile.
     c
     c/exec SQL
     c+   select sum(CDCRDT) into :retroAmt
     c+      from WFCDP where
     c+            CDCO#  = :QQCO#  and
     c+            CDFND  = :QQFND  and
     c+            CDEMP# = :QQEMP# and
     c+            CDDIV  = :QQDIV  and
     c+            CDCRYR = :QQFYR
     c/end-exec
     c
     c                   other
     c
     c*Use the following SQL statement to retrieve the retro credit amounts posted
     c*in the future from the A/R file.
     c
     c/exec SQL
     c+
     c+                  select sum(EAAMT) into :retroAmt
     c+                     from WMEAP where
     c+                           EACO#  =  :QQCO#  and
     c+                           EAFND  =  :QQFND  and
     c+                           EAEMP# =  :QQEMP# and
     c+                           EADIV  =  :QQDIV  and
     c+                           EAFYR  =  :QQFYR  and
     c+                           EAARCD =  :retroCreditCod and
     c+                           EATRDT >= :processDate
     c/end-exec
     c
     c                   if        retroAmt = 0
     c                   eval      exit = 'Y'
     c                   return
     c                   endif
     c
     c                   endsl
     p getRetroAmt     E
     c*=====================================================================---*
     c* getProcessDate- This subprocedure calculates the cutoff date that will be used to accumulate
     c* Retro Credit Installments for revised bill processing (QQMODE = 'R').
     c*=====================================================================---*
     p getProcessDate  B
     d
     d getProcessDate  PI             7  0
     d  workdate       s               d   datfmt(*usa)
     d  returnDate     s              7  0
     d  #ofAccCurrDay  s              2  0
     d  #ofDirBillDay  s              2  0
     d  #OfDays        s              2  0
     c
     c                   CALL      'WA002R'
     c                   parm      qqco#         @@co#             3 0
     c                   parm      qqfnd         @@fnd             3 0
     c                   parm      qqemp#        @@emp#            9 0
     c                   parm      qqdiv         @@div             5 0
     C                   parm      qqfyr         @@fyr             3 0
     c                   parm                    @@agt#            5 0
     c                   parm                    @@anam           60
     c                   parm                    @@prod            5 0
     c                   parm                    @@pnam           60
     c                   parm                    @@area            3 0
     c                   parm                    @@phon            7 0
     c                   parm      '*'           @@plan            1
     c                   parm      *ZERO         @@date            8 0
     c
     c*Then, pull in the number of revised interim days for account current and direct bill that is
     c*set up in billing options off of W/W Groups.
     c
     c
     c/exec SQL
     c+
     c+
     c+           select EXRVID, EXAOID
     c+              into :#ofDirBillDay , :#ofAccCurrDay
     c+              from WMEXP where
     c+              EXCO# = :qqco#  and
     c+              EXFND = :qqfnd
     c+
     c/end-exec
     c
     c* Then, calculate and return the process date based upon the bill plan's interim day value
     c* added to the system date..
     c                   if        @@plan = 'A'
     c                   eval      #OfDays = #ofAccCurrDay
     c                   else
     c                   eval      #OfDays = #ofDirBillDay
     c                   endif
     c
     c     *usa          move      *date         workDate
     c                   adddur    #ofdays:*days workDate
     c
     c     *cymd         move      workDate      returnDate
     c
     c                   return    returnDate
     p getProcessDate  E
     c*=====================================================================---*
     c* getInstDates
     c*
     c*=====================================================================---*
     p getInstDates    B
     d
     d getInstDates    PI
     c
     c                   Move      qqco#         ##co#
     c                   Move      qqfnd         ##fnd
     c                   eval      ##fyr  = %char(qqfyr)
     c                   eval      ##bscd = %char(curBSCD)
     c                   eval      ##bsdt = %char(billStartDate)
     c                   eval      ##pfdt = %char(policyFromDate)
     c                   eval      ##ptdt = %char(policyExpDate)
     c                   eval      ##date = %char(processDate)
     c                   move      *zeros        ##prem
     c                   move      retroAmt      ##prem
     c
     c                   CALL      'SRINST'
     c                   parm                    ##co#             3
     c                   parm                    ##fnd             3
     c                   parm                    ##fyr             3
     c                   parm                    ##bscd            3
     C                   parm                    ##bsdt            7
     c                   parm                    ##pfdt            7
     c                   parm                    ##ptdt            7
     c                   parm                    ##date            7
     c                   parm                    ##prem           11
     c                   parm      qqmode        ##type            1
     c                   parm                    ##da1           126
     c                   parm                    ##da2           126
     c                   parm                    ##da3           126
     c                   parm                    ##am1           198
     c                   parm                    ##am2           198
     c                   parm                    ##am3           198
     c
     c                   movea     ##da1         installDats
     c                   movea     ##am1         installAmts
     c
     p getInstDates    E
     c*=====================================================================---*
     c* writeWDEAP  - Write the records in WDEAP
     c*=====================================================================---*
     p writeWDEAP      B
     d
     d writeWDEAP      PI
     c
     c*First, get an A/R and Batch number write a dummy A/R Batch record as a placeholder.
     c
     c                   callp     writeDummyRcd
     c
     c* Then, iterate through the installDate array and write an A/R Batch record
     c                   callp     writeInstments
     c
     c*Then, delete the dummy record.
     c
     c                   callp     deleteDummyRcd
     p writeWDEAP      E
     c*=====================================================================---*
     c* writeDummyRcd
     c*
     c*=====================================================================---*
     p writeDummyRcd   B
     d
     d writeDummyRcd   PI
     c
     c/exec SQL
     c+
     c+ select EABAT# into :nxtbat# from wdeap
     c+    where EAACHF = 'R' and
     c+          EAIDT  = :systemDate
     c/end-exec
     c
     c*If nxtBat# = 0, get the next batch number from WDEAP
     c
     c                   if        nxtBat# = 0
     c
     c/exec SQL
     c+
     c+ select max(EABAT#) into :nxtBat# from wdeap
     c+
     c/end-exec
     c
     c                   eval      nxtBat# = nxtBat# + 1
     c
     c                   if        nxtBat# = *hival
     c                   eval      nxtBat# = 1
     c                   endif
     c                   endif
     c
     c*Use the following SQL to write a dummy record to A/R Batch.
     c
     c/exec SQL
     c+
     c+
     c+  insert into WDEAP
     c+        ( EACO#,
     c+         EAFND,
     c+         EAEMP#,
     c+         EADIV,
     c+         EAFYR,
     c+         EATFAP,
     c+         EATFFL,
     c+         EAIUSR,
     c+         EAIPGM,
     c+         EAIDT,
     c+         EAUSER,
     c+         EAPGM,
     c+         EADT,
     c+         EATIME,
     c+         EABAT#,
     c+         EAACHF )
     c+   Values   ( :QQCO#,
     c+         999,
     c+         999999999,
     c+         99999,
     c+          999,
     c+          'N',
     c+          'N',
     c+          :Q1USER,
     c+          :Q1PGM,
     c+          :systemDate,
     c+          :Q1USER,
     c+          :Q1PGM,
     c+          :systemDate,
     c+          :systemTime,
     c+          :nxtBat#,
     c+          'R')
     c/end-exec
     p writeDummyRcd   E
     c*=====================================================================---*
     c* deleteDummyRcd
     c*
     c*=====================================================================---*
     p deleteDummyRcd  B
     d
     d deleteDummyRcd  PI
     c
     c/exec SQL
     c+
     c+   delete from wdeap
     c+   where eaco# = :qqco#   and
     c+     eaachf = 'R'         and
     c+     eaidt = :systemDate  and
     c+     eafnd = 999          and
     c+     eaemp# = 999999999   and
     c+     eadiv = 99999        and
     c+     eafyr = 999
     c/end-exec
     c
     p deleteDummyRcd  E
     c*=====================================================================---*
     c* writeInstments
     c*
     c*=====================================================================---*
     p writeInstments  B
     d
     d writeInstments  PI
     c
     c*Iterate through the installDats and installAmts arrays and write an A/R Batch record for
     c*each non-zero entry in installAmts.
     c                   do        14            x
     c                   if        installAmts(x) <> '00000000000'
     c
     c*
     c                   eval      eaco# = qqco#
     c                   eval      eabat# = nxtBat#
     c                   eval      eafnd = qqfnd
     c                   eval      eafyr = qqfyr
     c                   eval      eaemp#= qqemp#
     c                   eval      eadiv = qqdiv
     c                   eval      eaarcd = retroCreditCod
     c                   eval      eadesc = retroDesc
     c                   Movel     installDats(x)eapodt
     c                   eval      eachk# = 0
     c                   eval      eackam = 0
     c                   move      installAmts(x)EAAMT
     c                   eval      easycd = systemCode
     c                   eval      eatfap = 'N'
     c                   eval      eatffl = 'N'
     c                   eval      eatfdt = 0
     c                   eval      eatfam = 0
     c                   eval      eastat = *blanks
     c                   eval      eauser = q1user
     c                   eval      eapgm  = q1pgm
     c                   eval      eatime = systemTime
     c                   eval      eadt   = systemDate
     c                   eval      eainv# = 0
     c
     c                   Movel     installDats(x)xxinstalldats     7 0
     c
     c                   if        xxinstalldats   > systemDate
     c                   eval      eatrdt = xxinstalldats
     c                   else
     c                   eval      eatrdt = systemDate
     c                   endif
     c
     c                   eval      ealate = *blanks
     c                   eval      eabb#  = *blanks
     c                   eval      eaachf = 'R'
     c                   eval      eaachd = 0
     c                   eval      eaacht = 0
     c                   eval      eaachu = *blanks
     c                   eval      eabbdt = 0
     c                   eval      eaiusr = q1user
     c                   eval      eaipgm = q1pgm
     c                   eval      eaidt  = systemDate
     c                   eval      eactyp = 'R'
     c                   eval      eacash = *blanks
     c                   eval      eardiv = 0
     c                   eval      eaautr = *blanks
     c                   eval      eaonli = *blanks
     c
     c                   Write     pea02
     c
     c                   endif
     c                   enddo
     p writeInstments  E
     c*=====================================================================---*
     c* updateBatchTotal
     c*
     c*=====================================================================---*
     p updateBatchTot  B
     d
     d updateBatchTot  PI
     c* Update the batch total.
     c*
     c/exec SQL
     c+  update WDEAP
     c+    set EABATA = (select sum(EAAMT) from WDEAP
     c+    where EABAT# = :nxtBat#) where EABAT# = :nxtBat#
     c+
     c/end-exec
     c
     p updateBatchTot  E
     c*=====================================================================---*
     c* writeWMEAP
     c*
     c*=====================================================================---*
     p writeWMEAP      B
     d writeWMEAP      PI

      *Iterate through the installDats and installAmts arrays and write an A/R Batch record for
      *each non-zero entry in installAmts.

     c                   do        14            x
     c                   if        installAmts(x) <> '00000000000'
     c                   eval      eaco# = qqco#
     c                   eval      eafnd = qqfnd
     c                   eval      eafyr = qqfyr
     c                   eval      eaemp#= qqemp#
     c                   eval      eadiv = qqdiv
     c                   eval      eaarcd = retroCreditCod
     c                   eval      eaarty = retroType
     c                   eval      eadesc = retroDesc
     c                   move      installAmts(x)EAAMT
     c                   movel     installDats(x)xxInstallDats     7 0
     c                   eval      eapodt = xxinstalldats
     c                   eval      eaedit = *blanks
     c                   eval      eastat = *blanks
     c                   eval      eauser = q1user
     c                   eval      eapgm  = q1pgm
     c                   eval      eatime = systemTime
     c                   eval      eadt   = systemDate
     c                   eval      eabul1 = QQBAT#
     c                   eval      eachk# = 0
     c                   eval      eaninv = 0
     c                   eval      ealate = *blanks
     c                   eval      eafl01 = *blanks
     c                   eval      eafl02 = *blanks
     c                   eval      eadt01 = 0
     c                   eval      eadt02 = 0
     c                   eval      eacctp = *blanks
     c                   eval      eacnum = *blanks
     c                   eval      eacnam = *Blanks
     c                   eval      eactyp = 'R'
     c                   eval      eardiv = 0
     c                   eval      eastat = 'A'
     c                   if        xxInstallDats   > systemDate
     c                   eval      eatrdt = xxInstallDats
     c                   else
     c                   eval      eatrdt = systemDate
     c                   endif
     c                   write     pea01
     c                   endif
     c                   enddo
     p writeWMEAP      E
     c*=====================================================================---*
     c* adjustWMEAP
     c*
     c*=====================================================================---*
     p adjustWMEAP     B
     d
     d adjustWMEAP     PI
     c*
     c*First, delete the Retro Credits that are written with transaction dates in future months.
     c*
     c/exec SQL
     c+  delete from WMEAP
     c+ where EACO#  =  :QQCO#  and
     c+       EAFND  =  :QQFND  and
     c+       EAEMP# =  :QQEMP# and
     c+       EADIV  =  :QQDIV  and
     c+       EAFYR  =  :QQFYR  and
     c+       EAARCD =  :retroCreditCod and
     c+       EATRDT >= :processDate
     c+
     c/end-exec
     c
     c* Write the installments by using the subprocedure writeWMEAP
     c                   Callp     writeWMEAP
     c
     p adjustWMEAP     E
     c*=====================================================================---*
     c* billPlanChgd (YesOrNo)
     c*
     c*=====================================================================---*
     p billPlanChgd    B
     d
     d billPlanChgd    PI             1
     d prvAB           s              9  8 dim(14)
     d prvAB01         s              9  8
     d prvAB02         s              9  8
     d prvAB03         s              9  8
     d prvAB04         s              9  8
     d prvAB05         s              9  8
     d prvAB06         s              9  8
     d prvAB07         s              9  8
     d prvAB08         s              9  8
     d prvAB09         s              9  8
     d prvAB10         s              9  8
     d prvAB11         s              9  8
     d prvAB12         s              9  8
     d prvAB13         s              9  8
     d prvAB14         s              9  8
     c*
     d curAB01         s              9  8
     d curAB02         s              9  8
     d curAB03         s              9  8
     d curAB04         s              9  8
     d curAB05         s              9  8
     d curAB06         s              9  8
     d curAB07         s              9  8
     d curAB08         s              9  8
     d curAB09         s              9  8
     d curAB10         s              9  8
     d curAB11         s              9  8
     d curAB12         s              9  8
     d curAB13         s              9  8
     d curAB14         s              9  8
     c*
     c/exec SQL
     c+
     c+  declare chkPrvBSCD  scroll  cursor
     c+      for select EHBSCD,EHTRDT from WDEHP
     c+      where ehco#  = :qqco#  and
     c+      ehfnd  = :qqfnd  and
     c+      ehemp# = :qqemp# and
     c+      ehdiv  = :qqdiv  and
     c+      ehfyr  = :qqfyr  and
     c+      EHTRDT  > 0
     c+      order by EHTRDT
     c+      for read only
     c/end-exec
     c
     c/exec SQL
     c+    open  chkPrvBSCD
     c/end-exec
     c*
     c
     c/exec SQL
     c+    fetch last from chkPrvBSCD into :prvBSCD
     c/end-exec
     c*
     c
     c                   if        SQLSTT = '02000'
     c                   return    'N'
     c                   endif
     c/exec SQL
     c
     c+    close chkPrvBSCD
     c/end-exec
     c
     c
     c                   if        curBSCD <> prvBSCD
     c
     c/exec SQL
     c+ select BSAB01,BSAB02,BSAB03,BSAB04,BSAB05,BSAB06,
     c+ BSAB07,BSAB08,BSAB09,BSAB10,BSAB11,BSAB12, BSAB13,BSAB14
     c+ into : PRVAB01,:PRVAB02,:PRVAB03,:PRVAB04,:PRVAB05,:PRVAB06,:PRVAB07,
     c+  :PRVAB08,:PRVAB09,:PRVAB10,:PRVAB11,:PRVAB12,:PRVAB13,:PRVAB14
     c+   from WTBSP where  BSCO# = :qqco# and
     c+   BSKEY = char(:prvBSCD)
     c/end-exec
     c
     c/exec SQL
     c+ select BSAB01,BSAB02,BSAB03,BSAB04,BSAB05,BSAB06,
     c+ BSAB07,BSAB08,BSAB09,BSAB10,BSAB11,BSAB12, BSAB13,BSAB14
     c+ into : curAB01,:curAB02,:curAB03,:curAB04,:curAB05,:curAB06,:curAB07,
     c+  :curAB08,:curAB09,:curAB10,:curAB11,:curAB12,:curAB13,:curAB14
     c+   from WTBSP where  BSCO# = :qqco# and
     c+   BSKEY = char(:curBSCD)
     c/end-exec
     c                   If        prvAB01  <> curAB01 or prvAB02  <> curAB02
     c                             or prvAB03  <> curAB03 or prvAB04  <> curAB04
     c                             or prvAB05  <> curAB05 or prvAB06  <> curAB06
     c                             or prvAB07  <> curAB07 or prvAB08  <> curAB08
     c                             or prvAB09  <> curAB09 or prvAB10  <> curAB10
     c                             or prvAB11  <> curAB11 or prvAB12  <> curAB12
     c                             or prvAB13  <> curAB13 or prvAB14  <> curAB14
     c
     c                   return    'Y'
     c                   endif
     c
     c                   endif
     c                   return    'N'
     p billPlanChgd    E
