    Hcopyright('(c) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/TITLE WCPP1A - Create endorsement PP1A
    H*********************************************************************
    H* Created: 03/08/06    Programmer: C:33    Project: 135WFZ          *
    H*********************************************************************
    H* Reviions:                                                         *
    H*                                                                   *
    H*********************************************************************
     FPP1A      o    e             printer oflind(*in98)
     F                                     infds(fids01)
     FWXFML     uf   e           k disk

     D fids01        e ds                  extname(szq4p)
     D psds          esds                  extname(szq1p)

     d #ofRcds         c                   6

     d inNamesDS       ds
     d  FMNM01                             inz('Testing Name 1')
     d  FMNM02                             inz('Testing Name 2')
     d  FMNM03                             inz('Testing Name 3')
     d  FMNM04                             inz('Testing Name 4')
     d  FMNM05                             inz('Testing Name 5')
     d  FMNM06                             inz('Testing Name 6')
    d inNames                             like(FMNM01) dim(#ofRcds)
     d                                      overlay(inNamesDS)
     d inWagesDS       ds
     d  FMC101                             inz('TestWage1')
     d  FMC102                             inz('TestWage2')
     d  FMC103                             inz('TestWage3')
     d  FMC104                             inz('TestWage4')
     d  FMC105                             inz('TestWage5')
     d  FMC106                             inz('TestWage6')
    d inWages                             like(FMC101) dim(#ofRcds)
     d                                      overlay(inWagesDS)
     d outNamesDS      ds
     d  name1
     d  name2
     d  name3
     d  name4
     d  name5
     d  name6
    d outNames                            like(name1) dim(#ofRcds)
     d                                      overlay(outNamesDS)
     d outWagesDS      ds
     d  wage1
     d  wage2
     d  wage3
     d  wage4
     d  wage5
     d  wage6
    d outWages                            like(wage1) dim(#ofRcds)
     d                                      overlay(outWagesDS)

     D @@Co#           s              3
     D @@Div           s              5
     D @@Emp#          s              9
     D @@Fnd           s              3
     D @@Fyr           s              3
     D @@Mode          s              1
     D counter         s              5  0 inz
     D xxCo#           s                   like(FMCO#)
     D xxDiv           s                   like(FMDIV)
     D xxEmp#          s                   like(FMEMP#)
     D xxFnd           s                   like(FMFND)
     D xxForm          s                   like(FMFORM) inz('PP1A')
     D xxFyr           s                   like(FMFYR)
     D xxMode          s              1

     C     keyWXFML      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
     C                   kfld                    xxForm

     C     *entry        plist
     C                   parm                    @@Mode
     C                   parm                    @@Co#
     C                   parm                    @@Fnd
     C                   parm                    @@Emp#
     C                   parm                    @@Div
     C                   parm                    @@Fyr
     C*
     C                   clear                   xxMode
     C*
     C                   if        %parms >= 1
     C                   move      @@Mode        xxMode
     C                   endif
     C*
     C                   if        %parms >= 2
     C                   move      @@Co#         xxCo#
     C                   move      @@Fnd         xxFnd
     C                   move      @@Emp#        xxEmp#
     C                   move      @@Div         xxDiv
     C                   move      @@Fyr         xxFyr
     C                   endif
     C*
     C                   exsr      LoadConsts
    C*
     C     keyWXFML      setll     WXFML
     C*
     C                   dou       %eof
     C     keyWXFML      reade     WXFML
     C                   if        not %eof
     C                   exsr      LoadVars
     C                   exsr      PrintPP1A
     C                   delete    WXFML
     C                   endif
     C                   enddo
     C*
     C                   select
     C                   when      xxMode = '*'
     C                   exsr      PrintPP1A
     C                   when      xxMode = '#'
     C                   exsr      LoadTestVars
     C                   exsr      PrintPP1A
     C                   endsl
    C*
     C                   move      *on           *inlr
    C*
     C*====================================================================
    C* Load constants
     C*====================================================================
     C     LoadConsts    begsr
    C*
     C                   eval      const1A = 'Always'
     C                   eval      const1B = 'complete'
     C                   eval      const1C = 'this'
     C                   eval      const1D = 'section'
     C                   eval      const1E = 'Complete'
     C                   eval      const1F = 'this section'
     C                   eval      const1G = 'only when'
     C                   eval      const1H = 'coverage is'
     C                   eval      const1I = 'elected'
     C                   eval      const1J = 'Always'
     C                   eval      const1K = 'complete'
     C                   eval      const1L = 'this section'
    C*
     C                   endsr
    C*
     C*====================================================================
    C* Load test variables
     C*====================================================================
     C     LoadTestVars  begsr
    C*
     C                   eval      nameOfBus = 'Test Employer'
     C                   eval      elected   = 'X'
     C                   eval      rejected  = 'X'
     C                   eval      typeOfBus = 'X'
     C*
     C                   movea     inNames       outNames
     C                   movea     inWages       outWages
    C*
     C                   endsr
    C*
     C*====================================================================
    C* Load variables
     C*====================================================================
     C     LoadVars      begsr
     C*
     C                   eval      counter = counter + 1
     C*
     C                   if        counter > 1
     C                   write     newPage
     C                   endif
    C*
     C                   eval      nameOfBus = FMNM11 + FMNM12
     C                   eval      elected   = FMFL01
     C                   eval      rejected  = FMFL02
     C** **              eval      typeOfBus = FMC201
     C                   eval      typeOfBus = 'X'
     C*
     C                   movea     inNames       outNames
     C** **              movea     inWages       outWages
    C*
     C                   endsr
    C*
     C*====================================================================
    C* Print form
     C*====================================================================
     C     PrintPP1A     begsr
    C*
     C                   write     header
     C                   write     pg1Text
     C                   write     pg1Var
     C                   write     boxes1
     C                   write     lines1
     C                   write     const1
     C                   write     footer
    C*
     C                   endsr
