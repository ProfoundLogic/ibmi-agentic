      * (C) Copyright Fiserv Insurance Solutions, Inc., 2008                                       *
      *============================================================================================*
      /title FIX139WJPA - Set DDAMTP by Net or Gross                                               *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
     fwdell     if   e           k disk                                         co/fnd/emp/div/fyr
     fwtddl2    if   e           k disk    rename(pdd01 : dedtable)             co/fnd/st/code/efdt
     fwdddl3    uf   e           k disk    rename(pdd01 : poldeduct)            co/fnd/fyr/emp/div
     fwmrsl     if   e           k disk                                         co/fnd/st/code/efdt

     d kkst            s                   like(RSJRST)
     d kkcode          s                   like(ELDCDE)
     d kkefdt          s                   like(ELDCEF)

     c     rsky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd

     c     ddky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    kkst
     c                   kfld                    kkcode
     c                   kfld                    kkefdt

     c     ddky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    kkst
     c                   kfld                    kkcode

     c     polky         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c                   read      pel01
     c                   dow       not(%eof)
     c                   if        ELDCDE <> *blanks
     c                   eval      kkcode = ELDCDE
     c     rsky          chain     prs01
     c                   if        %found
     c                   eval      kkst = RSJRST
     c                   move      ELCO#         Q1CO#
     c                   move      ELFND         Q1FND
     c                   move      ELFYR         Q1FYR
     c                   move      ELEMP#        Q1EMP#
     c                   move      ELDIV         Q1DIV
     c                   move      ELDCEF        Q1DATE
     c                   call      'SREFFD'
     c                   parm                    Q1CO#             3
     c                   parm                    Q1FND             3
     c                   parm                    Q1FYR             3
     c                   parm                    Q1EMP#            9
     c                   parm                    Q1DIV             5
     c                   parm                    Q1DATE            7
     c                   move      Q1DATE        kkefdt
     c     ddky          setgt     dedtable
     c     ddky1         reade     dedtable
     c                   if        not(%eof)
     c     polky         setll     poldeduct
     c     polky         reade     poldeduct
     c                   dow       not(%eof)
     c                   select
     c                   when      ddtype = 'N' and
     c                             DDAMTP <> DDAMTB
     c                   eval      DDAMTP = DDAMTB
     c                   move      'X'           DDPGM
     c                   update    poldeduct
     c                   when      ddtype = 'G' and
     c                             DDAMTP <> 0
     c                   eval      DDAMTP = 0
     c                   move      'X'           DDPGM
     c                   update    poldeduct
     c                   endsl
     c     polky         reade     poldeduct
     c                   enddo
     c                   endif                                                  pdd01 = not(%eof)
     c                   endif                                                  prs01 = %found
     c                   endif                                                  ELDCDE <> *blanks
     c                   read      pel01
     c                   enddo

     c                   seton                                        lr
