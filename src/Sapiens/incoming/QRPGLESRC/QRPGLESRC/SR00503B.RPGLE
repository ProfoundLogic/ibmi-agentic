     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00503B - Retrospective Rating Plan Premium Endorsment  *

      //===============================================================*
      // WC000503B - Retrospective Rating Plan Premium Endorsement.    *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //
      //  04/01/15 C:302 Support item 32953                           *
      //                 Add standards premium from all retro         *
      //                 divisions.   Calculate the 50% and           *
      //                 the 150% amounts.                            *
      //                                                              *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F
      * Billing Schedule
     fwtabl     if   e           k disk    include(pbs01)
      ** Additional Work File
     Fwxfmp3    o    e             disk    usropn
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     Fwmepl1    if   e           k disk
     FWMPRL1    IF   E           K DISK
     Fwmexl     IF   E           K DISK

      /COPY SCOPYEN01D
      /COPY WWCPOLDAT4
      * factors by state
     d fact            ds                  occurs(60)
     d  x1st                          2a
     d  x1elp                         7  4
     d  x1elpf                        7  4
     d  x1tax                         7  4
     d  x1taxf                        7  4
     d  x1rdf1                        7  4
     d  x1rdf2                        7  4
     d  x1rdf3                        7  4
     d  x1num                         5  0
      * payroll array
     d xpara           ds                  occurs(255)
     d  xpclas                        5  0
     d  xpprem                       15  2
     d  xphg                          1

     d x1              s              2  0
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     d svemp#          s              9  0
     d x               s              4  0
     d x1carr          s             20
     d xpcarr          s             20
     D QQform          S             15a   inz('WC000503B ')
      ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     D SR00503B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00503B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      * Employer Master
     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
      * Billing Schedule
     c     bsky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10

     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0

     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr

     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#

     C     stky          KList
     C                   KFld                    xxkey            10

     C     epky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxfyr
     C                   KFld                    xxemp#
      *  PAYROLL CLASS FILE
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    XXEFDT            7 0

     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS

     C     exky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
      * Employer Detail
     c     elky3         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
      * Carrier Policy # file
     c     rpky3         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    xxfyr

      /COPY SCOPYEN01C

      *=========================================================================
      *  ADDFACT - Add factors by state                                     d
      *=========================================================================
     c     addfact       begsr

     c                   eval      x1 = 1

     c     x1            Dowle     60
     c     x1            occur     fact

     c                   select

     C     x1st          wheneq    *blanks
     c                   eval      x1st   = rsjrst
     c                   eval      x1elp  = elelp
     c                   eval      x1elpf = elelpf
     c                   eval      x1tax  = eltax
     c                   eval      x1taxf = eltaxf
     c                   eval      x1rdf1 = elrdf1
     c                   eval      x1rdf2 = elrdf2
     c                   eval      x1rdf3 = elrdf3
     c                   eval      x1num  = 1
     c                   leave

     C     rsjrst        wheneq    x1st
     c                   leave

     C                   other
     c                   add       1             x1
     c                   iter

     c                   endsl

     C                   EndDo

     C                   endsr
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     c     othinf        begsr
      ** open work file
     c                   open      wxfmp3
     c     exky          Chain     pex01                              60
     c*
     c                   move      exretg        fmfl01
      *    initialize
     c                   clear                   pfm03
     c                   movel(p)  xxform        fmform
      * get info from state reporting division
     c     emky1         Chain     pem01                              60

     c                   Dow       (*in60 = *off) and (emstrp <> 'Y')
     c     emky1         reade     pem01                                  60
     c                   EndDo

     c                   If        (*in60 = *off)
     c                   eval      fmam01 = elretn
     c                   eval      fma541 = ellcvf
     c                   eval      fma542 = elmnfc
     c                   eval      fma543 = elmxfc
     c                   eval      fma544 = elbsfc
     c                   EndIf
      * get factors for each state
     c                   clear     *all          fact

     c     elky2         Chain     pel02                              60

     c                   Dow       *in60 = *off
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60

     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))

     c                   eval      fmam02 = fmam02 + elstpr

     c     rsky          Chain     prs01                              60

     c     *in60         Ifeq      *off
      *  add to factors for state
     c                   exsr      addfact
     c                   EndIf

     c                   EndIf

     c     elky2         ReadE     pel02                                  60
     c                   EndDo
      *  write factors to output work file
     c                   exsr      wrtfact

     c                   eval      fmam03 = fmam02 * .5
     c                   eval      fmam04 = fmam02 * 1.5
      ** Write the output record
     C                   write     pfm03
      ** Close the endorsement form workfile
     C                   close     wxfmp3
      ** Write records to the Policy Page Endorsement Transaction File
     c                   exsr      transwrt

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     c                   if        (qqmode = '2' and exretg = 'Y')
     c                   call      'SR00503ZB'
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    qqmode
     C                   parm                    qqrrn
     C                   parm                    qqmod2
     c                   endif
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr

     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60

     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))

     C     rsky          chain     prs01                              60

     C                   if        *in60 = *off
     C                             and (rsjrst <> 'CA')
      ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm

     C                   endif
     C                   endif

     C                   endsr
      *=========================================================================
      *  WRTFACT - Write Factors to output work file
      *=========================================================================
     c     wrtfact       begsr

     c     1             occur     fact
     c                   eval      fmcd01 = x1st
     c                   eval      fmrt01 = x1elp
     c                   eval      fmrt02 = x1elpf
     c                   eval      fmrt03 = x1tax
     c                   eval      fmrt04 = x1taxf
     c                   eval      fmrt05 = x1rdf1
     c                   eval      fmrt06 = x1rdf2
     c                   eval      fmrt07 = x1rdf3

     c     2             occur     fact
     c                   eval      fmcd11 = x1st
     c                   eval      fmrt11 = x1elp
     c                   eval      fmrt12 = x1elpf
     c                   eval      fmrt13 = x1tax
     c                   eval      fmrt14 = x1taxf
     c                   eval      fmrt15 = x1rdf1
     c                   eval      fmrt16 = x1rdf2
     c                   eval      fmrt17 = x1rdf3

     c     3             occur     fact
     c                   eval      fmcd21 = x1st
     c                   eval      fmrt21 = x1elp
     c                   eval      fmrt22 = x1elpf
     c                   eval      fmrt23 = x1tax
     c                   eval      fmrt24 = x1taxf
     c                   eval      fmrt25 = x1rdf1
     c                   eval      fmrt26 = x1rdf2
     c                   eval      fmrt27 = x1rdf3

     c     4             occur     fact
     c                   eval      fmcd31 = x1st
     c                   eval      fmrt31 = x1elp
     c                   eval      fmrt32 = x1elpf
     c                   eval      fmrt33 = x1tax
     c                   eval      fmrt34 = x1taxf
     c                   eval      fmrt35 = x1rdf1
     c                   eval      fmrt36 = x1rdf2
     c                   eval      fmrt37 = x1rdf3

     c     5             occur     fact
     c                   eval      fmcd41 = x1st
     c                   eval      fmrt41 = x1elp
     c                   eval      fmrt42 = x1elpf
     c                   eval      fmrt43 = x1tax
     c                   eval      fmrt44 = x1taxf
     c                   eval      fmrt45 = x1rdf1
     c                   eval      fmrt46 = x1rdf2
     c                   eval      fmrt47 = x1rdf3

     c                   endsr
      *=========================================================================
      *  TRANSWRT - Write records to Policy Page End Transaction File       d
      *=========================================================================
     c     transwrt      begsr
      *  If printing a policy page.  (Not called from W1054R)
     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     c     pekey         setgt     ppe02
     c     pekey1        readpe    ppe02                                  60

     c                   If        *in60 = *off
     c                   z-add     peunid        xxunid
     c                   endif

     c                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     c                   If        *in60 = *off
     c                   move      *blank        svhg              1
     c                   exsr      payhg
     c                   Z-add     1             I
     c     I             occur     fact

     c                   if        x1num <> 0
     c                   movel(p)  x1st          XXKEY
     c     stky          chain     pst01                              60

     c                   if        *in60 = *off
     c                   MOVE      STNST         PEST
     c                   endif

     c                   move      *ZEROS        E00503
     c                   z-add     fmam01        xxLLIM
     c                   z-add     fma541        XXLCON
     c                   move      SVHG          XXHG

     c                   z-add     x1elp         XXLFST
     c                   z-add     x1elpf        XXLFFE
     c                   z-add     x1tax         XXTXST
     c                   z-add     x1taxf        XXTXFE
     c                   Z-ADD     x1rdf1        XXRDF1
     c                   Z-ADD     x1rdf2        XXRDF2
     c                   Z-ADD     x1rdf3        XXRDF3
     c                   Z-ADD     0             XXM050
     c                   Z-ADD     fma542        XXM100
     c                   Z-ADD     0             XXM150
     c                   Z-ADD     0             XXX050
     c                   Z-ADD     fma543        XXX100
     c                   Z-ADD     0             XXX150
     c                   Z-ADD     0             XXB050
     c                   Z-ADD     fma544        XXB100
     c                   Z-ADD     0             XXB150
     c                   Z-ADD     0             XXE050
     c                   Z-ADD     fmam02        XXE100
     c                   Z-ADD     0             XXE150

     c                   move      *blanks       pedata
     c                   movel     E00503        pedata

     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     c                   update    PPE01
     c                   endif

     c                   ADD       1             I

     c     I             DOWLE     5
     c     I             occur     fact

     c                   if        x1num <> 0
     c                   movel(p)  x1st          XXKEY
     c     stky          chain     pst01                              60

     c                   if        *in60 = *off
     c                   MOVE      STNST         PEST
     c                   endif

     c                   move      *ZEROS        E00503
     c                   z-add     fmam01        XXLLIM
     c                   z-add     fma541        XXLCON
     c                   move      SVHG          XXHG

     c                   z-add     x1elp         XXLFST
     c                   z-add     x1elpf        XXLFFE
     c                   z-add     x1tax         XXTXST
     c                   z-add     x1taxf        XXTXFE
     c                   Z-ADD     x1rdf1        XXRDF1
     c                   Z-ADD     x1rdf2        XXRDF2
     c                   Z-ADD     x1rdf3        XXRDF3
     c                   Z-ADD     0             XXM050
     c                   Z-ADD     fma542        XXM100
     c                   Z-ADD     0             XXM150
     c                   Z-ADD     0             XXX050
     c                   Z-ADD     fma543        XXX100
     c                   Z-ADD     0             XXX150
     c                   Z-ADD     0             XXB050
     c                   Z-ADD     fma544        XXB100
     c                   Z-ADD     0             XXB150
     c                   Z-ADD     0             XXE050
     c                   Z-ADD     fmam02        XXE100
     c                   Z-ADD     0             XXE150
     c                   Z-ADD     0             XXREFD

     c                   move      *blanks       pedata
     c                   movel     E00503        pedata

     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt

     c                   write     PPE01
     c                   endif

     c                   ADD       1             i
     c                   ENDDO

     c                   endif

     C                   endif

     c                   endsr
      *=========================================================================
      * Subroutine to determine hazard group for policy.
      *=========================================================================
     c     payhg         begsr

     C                   Z-ADD     XXDIV         SVDIV             5 0
     c                   z-add     elpfdt        xxefdt

     c                   z-add     0             svcode            5 0
     C                   z-add     0             svprem           15 2

     c     1             do        255           z                 3 0
     C     z             occur     xpara
     C                   clear                   xpara
     C                   enddo
     C* check all payroll records for policy year.
     C     epky          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF

     C     EPSTAT        IFNE      'I'

     C                   Z-ADD     EPDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
      *  Set multi occr ds to matching class element or next available.
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
      * Next available
     C     XPCLAS        IFEQ      *ZERO
     C                   MOVE      EPCLAS        XPCLAS
     C                   LEAVE
     C                   ENDIF
      * Matching class
     C     XPCLAS        IFEQ      EPCLAS
     C                   LEAVE
     C                   ENDIF

     C                   ENDDO
      * Total payroll field.
     C                   ADD       EPEMPR        XPPREM

     C                   Z-ADD     XPCLAS        XXCLAS

     C     PRKEY         SETGT     PPR01
     C     PRKY2         readpe    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PRHZGR        XPHG
     C                   ENDIF

     C                   ENDIF

     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
      * Check for class with highest premium total.
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA

     C     XPPREM        IFGE      SVPREM
     C     XPCLAS        ANDNE     *ZERO
     C                   Z-ADD     XPCLAS        SVCODE
     C                   Z-ADD     XPPREM        SVPREM
     c                   MOVE      XPHG          SVHG
     C                   ENDIF

     C                   ENDDO

     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60

     c                   endsr
