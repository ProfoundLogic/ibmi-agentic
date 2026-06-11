     * (C) Copyright Progressive Data Solutions, Inc., 2006
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00421A
     *===============================================================*
     * Domestic Terrorism, Earthquakes, and Catastrophic Industrial  *
     * Accidents Premium Endorsement                                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:
      *                                                               *
      * 09/15/08 C:189 PROJECT: WC1644                                *
      *                Add an Option to exclude terrorism from LCM    *
      *                                                               *
     *                                                               *
     * 06/05/08 C:197 Project 1530 Priss#6                           *
     *              - Endorsements - Generic end Approved/Excluded   *
     *                States                                         *
     *                                                               *
     * 11/01/07 C:184 Project 1530                                   *
     *              - Endorsements - Generic end Approved/Excluded   *
     *                States                                         *
     *                                                               *
     * 10/23/07 C:184 support WA099 59651                            *
     *              - New source version (that have all prior commen-*
     *                ts removed)                                    *
     *                                                               *
     * 10/02/07 C:61  support wa135 59169                            *
     *                Print TX rates for clt135.                     *
     *                                                               *
      * 03/28/07 C:184 PROJECT WC1518                                 *
      *                Endorsement "Approved by State" Edit           *
     *                                                               *
     * 01/17/07 C:61  support wa091 54752/59563                      *
     *              - Adjust rounding of printed rate.               *
     *                                                               *
     * 11/02/06 C:61  support wa091 54752                            *
     *              - Only attach for approved states.               *
     *              - Only print rates for approved states.          *
     *
     * 01/16/06  C:61  PROJECT CM027
     *                 Copy from SR00421A and change for WC000421A.
     *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
    
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
    
      ** Employer Payroll
     Fwmepl7    if   e           k disk
    
    *** BEG ADD ******* 11/01/07 ******************************************
      ** Endorsement States Approved
     F***wtesl     if   e           k disk
    *** END ADD ******* 11/01/07 ******************************************
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D

     d max             c                   const(200)

      * output fields
     d stods           ds
     d  FMC101
     d  FMC102
     d  FMC103
     d  FMC104
     d  FMC105
     d  FMC106
     d  FMC107
     d  FMC108
     d  FMC109
     d  FMC110
    d sto                                 OVERLAY(stods)
     d                                     LIKE(FMC101) DIM(10)

     d pyods           ds
     d  fmam01
     d  fmam02
     d  fmam03
     d  fmam04
     d  fmam05
     d  fmam06
     d  fmam07
     d  fmam08
     d  fmam09
     d  fmam10
    d pyo                                 OVERLAY(pyods)
     d                                     LIKE(fmam01) DIM(10)

     d rtods           ds
     d  fma541
     d  fma542
     d  fma543
     d  fma544
     d  fma545
     d  fma546
     d  fma547
     d  fma548
     d  fma549
     d  fma54a
    d rto                                 OVERLAY(rtods)
     d                                     LIKE(fma541) DIM(10)

     d prods           ds
     d  fmam11
     d  fmam12
     d  fmam13
     d  fmam14
     d  fmam15
     d  fmam16
     d  fmam17
     d  fmam18
     d  fmam19
     d  fmam20
    d pro                                 OVERLAY(prods)
     d                                     LIKE(fmam11) DIM(10)

    d st                                  like(fmc101) dim(max)
    d py                                  like(fmam01) dim(max)
    d rt                                  like(fma541) dim(max)
     d an                                  like(ansav) dim(max)
    
    
      * counters
     d x1              s              3  0
     d x2              s              3  0
    
      * temp
     d stsav           s                   like(fmc101)
     d pysav           s                   like(fmam01)
     d rtsav           s              5  2
     d prsav           s             11  0
     d ansav           s              1  0
      *
     d sttmp           s                   like(fmc101)
     d pytmp           s                   like(fmam01)
     d rttmp           s                   like(fma541)
     d antmp           s              1  0
    
     d kkefdt          s                   like(elpfdt)

     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4
    *** BEG ADD ******* 11/01/07 ******************************************
     d x1form          s             15A   inz(*blanks)
    *** END ADD ******* 11/01/07 ******************************************

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC000421A ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')


    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Statistical Reporting/Rating
      *
     C     rrky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    kkefdt
      *
     C     epky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    ELFYR
     C                   KFld                    ELEMP#
     C                   KFld                    ELDIV
    
    *** BEG ADD ******* 11/01/07 ******************************************
     C     esky3         KList
     C                   KFld                    elCO#
     C                   KFld                    x1FORM
     C                   KFld                    RSJRST
    *** END ADD ******* 11/01/07 ******************************************
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  CKSTAT - Check if endorsement approved for state                   d
     *=========================================================================
     c     ckstat        begsr
    
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              move      'N'           approved_state    1
    *** END DEL ******* 06/05/08 ******************************************
    
    *** BEG DEL ******* 11/01/07 ******************************************
     c*****              if        (rsjrst <> 'CA')
     c*****                        and (rsjrst <> 'FL')
     c*****                        and (rsjrst <> 'MA')
     c*****                        and (rsjrst <> 'PA')
     c*****                        and (rsjrst <> 'VA')
    *** END DEL ******* 11/01/07 ******************************************
     c
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              eval      approved_state = 'Y'
    *** END DEL ******* 06/05/08 ******************************************
    *** BEG DEL ******* 11/01/07 ******************************************
     c*****              endif
    *** END DEL ******* 11/01/07 ******************************************
     c                   endsr
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     c                   movel(p)  xxform        fmform
    
     ** Get state / rate information
     C     elky2         Chain     pel02                              60
     *
     C                   Dow       *in60 = *off
     C     rsky          Chain     prs01                              60
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              exsr      ckstat
    *** END DEL ******* 06/05/08 ******************************************
     *
    *** BEG ADD ******* 11/01/07 ******************************************
     c     esky3         Chain     pes01                              60
    *** END ADD ******* 11/01/07 ******************************************
     C                   if        (*in60 = *off)
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****                        and (approved_state = 'Y')
    *** END DEL ******* 06/05/08 ******************************************
     *
     *  accum payroll
     *
     c     epky          chain     pep01                              60
     *
     c                   Dow       *in60 = *off
     c                   clear                   stsav
     c                   clear                   rtsav
     c                   clear                   pysav
     c                   clear                   ansav
     *
     *  state
     *
     c                   movel(p)  rsjrst        stsav
     *
     *  anniv rate date
     *
     c                   if         (epefdt < elandt + 10000)
     c                              or (elandt = 0)
     c                   eval      ansav = 1
     *
     c                   if        elandt = 0
     c                   eval      kkefdt = elpfdt
     c                   else
     c                   eval      kkefdt = elandt
     c                   endif

     c     rrky1         setgt     prr01
     c     rrky          readpe    prr01                                  60
     *
     c     *in60         Ifeq      *off
     c     rrdtrr        andne     0
     c                   move(p)   RRCO#         #1co#
     c                   move(p)   RRFND         #1fnd
     c                   move(p)   ELRTCD        ##rtcd
     c                   move(p)   RRDTRC        ##clas
     c                   move(p)   ELPFDT        ##efdt

     c                   call      'SRLCM'
     c                   parm                    #1co#
     c                   parm                    #1fnd
     c                   parm                    ##rtcd
     c                   parm                    ##clas
     c                   parm                    ##efdt
     c                   parm                    ##lcm

     c                   move      ##lcm         xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   if        RRTERL <> 'Y'
     C*** END ADD *****  09/15/08  ************************************
     c                   eval(h)   rtsav  = RRDTRR * xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   else
     c                   eval(h)   rtsav  = RRDTRR
     C                   endif                                                      RRTERL IFNE
     C*** END ADD *****  09/15/08  ************************************
     c                   endif
     *
     c                   else
     *
     c                   eval      ansav = 2
     c                   eval      kkefdt = elandt + 10000
     C     rrky1         setgt     prr01
     C     rrky          readpe    prr01                                  60
     *
     C     *in60         Ifeq      *off
     C     rrdtrr        andne     0
     c                   move(p)   RRCO#         #1co#
     c                   move(p)   RRFND         #1fnd
     c                   move(p)   ELRTCD        ##rtcd
     c                   move(p)   RRDTRC        ##clas
     c                   move(p)   ELPFDT        ##efdt

     c                   call      'SRLCM'
     c                   parm                    #1co#
     c                   parm                    #1fnd
     c                   parm                    ##rtcd
     c                   parm                    ##clas
     c                   parm                    ##efdt
     c                   parm                    ##lcm

     c                   move      ##lcm         xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   if        RRTERL <> 'Y'
     C*** END ADD *****  09/15/08  ************************************
     c                   eval(h)   RRDTRR = RRDTRR * xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   endif                                                      RRTERL IFNE
     C*** END ADD *****  09/15/08  ************************************
     c                   z-add     rrdtrr        rtsav
     c                   endif
     *
     c                   endif
     *
     *  payroll
     c                   z-add     epeapr        pysav
     *
     *  premium
     *
     c                   if        (stsav <> *blanks) and (rtsav <> 0)
     c                   exsr      srins
     c                   endif

     c     epky          reade     pep01                                  60
     c                   EndDo
     *
     C                   EndIf
     *
     C     elky2         ReadE     pel02                                  60
     C                   EndDo
    
     ** Write the output record
    
     c                   z-add     1             x1
     c                   z-add     1             x2
    
     C     x1            Dowlt     max

     c                   if        st(x1) = *blanks
     c                   leave

     c                   else

     c                   movel(p)  st(x1)        sto(x2)
     c                   z-add     rt(x1)        rto(x2)
     c                   z-add     py(x1)        pyo(x2)
     c                   eval(h)   prsav = py(x1)/100 * rt(x1)
     c                   eval      pro(x2) = prsav
     c                   add       1             x1
     c                   add       1             x2
     c                   endif

     c                   if        x2 > 10
     C                   write     pfm01
     c                   clear                   sto
     c                   clear                   rto
     c                   clear                   pyo
     c                   clear                   pro
     c                   eval      x2 = 1
     c                   endif

     C                   EndDo
    
     c                   if        sto(1) <> *blanks
     c                   write     pfm01
     c                   endif
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     c     pgmstr        begsr
    
    *** BEG ADD ******* 11/01/07 ******************************************
    c                   movel(p)  xxform        x1form
    *** END ADD ******* 11/01/07 ******************************************
     c*
    c                   clear                   st
    c                   clear                   sto
    c                   clear                   py
    c                   clear                   pyo
    c                   clear                   rt
    c                   clear                   rto
    c                   clear                   pro
    c                   clear                   an
    c
     c                   endsr
    
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
    
    
     *=========================================================================
     *  srins - Insertion sort
     *=========================================================================
     C     srins         begsr
    
     c                   z-add     1             x1
    
     C     x1            Dowlt     max
      *
     c                   select
      *
     C                   when      (st(x1) = *blanks)
     c                   movel(p)  stsav         st(x1)
     c                   z-add     rtsav         rt(x1)
     c                   z-add     pysav         py(x1)
     c                   z-add     ansav         an(x1)
     c                   leave
      *
     C                   when      (stsav > st(x1))
     c                             or ( (stsav = st(x1)) and
     c                                  (ansav > an(x1)) )
     c                   add       1             x1
     c                   iter
      *
     C                   when      (stsav = st(x1)) and (ansav = an(x1))

     c     rtsav         ifgt      rt(x1)
     c                   z-add     rtsav         rt(x1)
     c                   endif

     c                   add       pysav         py(x1)
     c                   leave
      *
     C                   other
     c                   movel(p)  st(x1)        sttmp
     c                   z-add     rt(x1)        rttmp
     c                   z-add     py(x1)        pytmp
     c                   z-add     an(x1)        antmp
     c                   movel(p)  stsav         st(x1)
     c                   z-add     rtsav         rt(x1)
     c                   z-add     pysav         py(x1)
     c                   z-add     ansav         an(x1)
     c                   movel(p)  sttmp         stsav
     c                   z-add     rttmp         rtsav
     c                   z-add     pytmp         pysav
     c                   z-add     antmp         ansav
     c                   add       1             x1
     c                   iter
      *
     c                   endsl
      *
     C                   EndDo
    
     C                   endsr
    
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              exsr      ckstat
    *** END DEL ******* 06/05/08 ******************************************
    *** BEG ADD ******* 11/01/07 ******************************************
     c     esky3         Chain     pes01                              60
    *** END ADD ******* 11/01/07 ******************************************
     *
     c                   if        (*in60 = *off)
     c                             and (elpfdt >= 1060101)
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****                        and (approved_state = 'Y')
    *** END DEL ******* 06/05/08 ******************************************
     c                   eval      kkefdt = elpfdt
     c     rrky1         setgt     prr01
     c     rrky          readpe    prr01                                  60
     *
     *  Attach endorsement
     *    if non-zero domestic terror rate for jurisdiction state
     *
     C     *in60         ifeq      *off
     C     rrdtrr        andne     *zero
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endif
     *
     C                   endsr
    
    
    
