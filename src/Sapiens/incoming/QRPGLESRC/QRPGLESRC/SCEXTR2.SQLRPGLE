     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************************************
     H/title SCEXTR2 - DARTS Extract - CLOB data
     h option(*nodebugio: *srcstmt)
     h**********************************************************************************************
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created: 10/17/11   C:190   Project: 139WQI                      *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     H*  07/01/15 C:062 Project: 114WUU
    H*                 Correct population of workfile
     h*                                                                                            *
     H*  07/27/12 C:280 Project: 114IHH
    H*                 Added process for sis
     h*                                                                                            *
     h*                                                                                            *
     h**********************************************************************************************
     fclnot3c   if a e             disk    rename(pn301:pn30c)
     fclnot3p   if a e             disk    rename(pn301:pn30p)

     d psds          esds                  extname(szq1p)
    
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d @@FrDt          s              8
     d @@ToDt          s              8
     d kkFrDt          s              7  0
     d kkToDt          s              7  0
     d @@RunType       s              1
     d i1              s              5  0
     d i2              s              3  0
     d i3              s              3  0
     d @@clkey         s             40
     d @@clseq         s              5  0
     d @@claseq        s              5  0
     d @@rmcd          s              5
     d*** Begin Add *** 07/01/15 *********************************************
     d @@cdat          s              7  0
     d*** End   Add *** 07/01/15 *********************************************

     d BreakCLOB       pr
     d @@key                         40
     d @@seq                          5  0
     d @@aseq                         5  0
     d @@rmcd                         5
     d*** Begin Add *** 07/01/15 ******************************************
     d @@cdat                         7  0
     d*** End   Add *** 07/01/15 ******************************************

     d/copy sprdate
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     C                   parm                    @@FrDt
     C                   parm                    @@ToDt
     C                   parm                    @@RunType

     c                   if        @@Frdt <> '00000000'
     c                   eval      kkFrDt = DtCYMD(numDate(@@Frdt))
     c                   else
     c                   eval      kkFrdt = *loval
     c                   endif

     c                   if        @@Todt <> '00000000'
     c                   eval      kkToDt = DtCYMD(numDate(@@Todt))
     c                   else
     c                   eval      kkTodt = *Hival
     c                   endif
    C*
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c*** Begin Chg *** 07/01/15 ********************************************
     c***/EXEC SQL
     c***+  declare clobRec scroll cursor for
     c***+     select clkey, clseq, claseq, clrmcd from smclp
     c***+     where clcdat >= :kkFrdt and clcdat <= :kkTodt
     c***+  for read only
     c***/END-EXEC
     c/EXEC SQL
     c+  declare clobRec scroll cursor for
     c+     select clkey, clseq, claseq, clrmcd, clcdat from smclp
     c+     where clcdat >= :kkFrdt and clcdat <= :kkTodt
     c+  for read only
     c/END-EXEC
     c*** End   Chg *** 07/01/15 ********************************************
     c*
     c/exec SQL
     c+    open  clobRec
     c/end-exec

     c                   dow       SQLstt = '00000'                             (Read was success)
     c*** Begin Chg *** 07/01/15 ********************************************
     c***/exec SQL
     c***+    fetch next from clobRec
     c***+               into :@@clkey, :@@clseq, :@@claseq, :@@rmcd
     c***/end-exec
     c/exec SQL
     c+    fetch next from clobRec
     c+               into :@@clkey, :@@clseq, :@@claseq, :@@rmcd, :@@cdat
     c/end-exec
     c*** End   Chg *** 07/01/15 ********************************************
     c                   If        SQLstt <> '00000'
     c                   leave
     c                   endif

     c*** Begin Chg *** 07/01/15 ********************************************
     c**********         callp     BreakCLOB(@@clkey:@@clseq:@@claseq:@@rmcd)
     c                   callp     BreakCLOB(@@clkey:@@clseq:@@claseq:@@rmcd:
     c                                       @@cdat)
     c*** End   Chg *** 07/01/15 ********************************************

     c                   enddo
     c/exec SQL
     c+    close clobRec
     c/end-exec
     c                   seton                                        lr
     c                   return

     p*===========================================================================
     p* BreakCLOB
     p*===========================================================================
     p BreakCLOB       b
     d BreakCLOB       pi
     d inkey                         40
     d inseq                          5  0
     d inaseq                         5  0
     d inrmcd                         5
     d*** Begin Add *** 07/01/15 ********************************************
     d incdat                         7  0
     d*** End   Add *** 07/01/15 ********************************************
     d   dsClob        ds         32000    inz
     d   splitCLOB                  256    dim(125)

     c                   eval      i1 = 1
     c                   eval      i2 = 0
     c                   eval      i3 = 1

     c                   call      'DA003R'
     c                   parm      inkey         ##key            40
     c                   parm      inseq         ##Seq#            5 0
     c                   parm      inaseq        ##AltSeq#         5 0
     c                   parm      i1            ##Position       15 0
     c                   parm      32000         ##Length         15 0
     c                   parm      *blank        ##CLOB        32000

     c****               eval      splitCLOB = ##clob
     c                   eval      dsClob    = ##clob
     c                   if        ##clob  <> *blanks
     c                   do        125
     c                   if        splitCLOB(i3) <>  ' '
     c                   eval      i2 = i2 + 1
     c                   eval      n3seq#  = inseq
     c                   eval      n3Aseq  = inaseq
     c                   eval      n3nseq  = i2
     c                   eval      n3code  = inrmcd
     c                   eval      n3note  = splitCLOB(I3)
     c*** Begin Add *** 07/01/15 ********************************************
     c                   eval      clkey   = inKey
     c                   eval      clcdat  = inCdat
     c*** End   Add *** 07/01/15 ********************************************

     c                   select
     c                   When      @@RunType = 'P'
     c                   eval      n3fnd =  %int(%subst(##key:4:3))
     c                   eval      n3emp# = %int(%subst(##KEY:7:9))
     c                   eval      n3div =  %int(%subst(##KEY:16:5))
     c                   write     pn30p

     c                   When      @@RunType = 'C'
     c                   eval      n3fnd = %int(%subst(##KEY:4:3))
     c                   eval      n3fyr = %int(%subst(##KEY:7:3))
     c                   eval      n3case = %int(%subst(##KEY:10:7))
     c                   write     pn30c
     c*
     c***  beg chg ******  07/27/12  *********************************
     c*
     c                   When      @@RunType = *blanks
     c                   eval      clkey  = ##KEY
     c                   write     pn30c
     c*
     c***  end chg ******  07/27/12  *********************************
     c*                                                                         @@RunType = P or C
     c                   endsl
                                                                                @@RunType = P or C
     c                   endif                                                  splitCLOB(i3) <>  '
     c                   eval      i3 = i3 + 1
     c                   enddo                                                  'do 125
     c                   eval      i1 = i1 + 32000
     c                   endif                                                  splitCLOB = ' '
     p BreakCLOB       e
