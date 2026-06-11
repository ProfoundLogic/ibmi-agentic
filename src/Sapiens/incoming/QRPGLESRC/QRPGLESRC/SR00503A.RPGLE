     * (C) Copyright Progressive Data Solutions, Inc., 2004
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00503A - Retrospective Premium Endorsement - One Year Plan
     *===============================================================*
     * WC000503A                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/29/04                                          *
     * Programmer: C:61                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
     *  10/22/08 C:190 PROJECT 132WLL                                *
     *                 Modify VALID routine such that WC000503A will *
     *                 not attach unless there is coverage in a state*
     *                 other than California.                        *
     *                                                               *
     *  06/25/08 C:65  PROJECT 114WRH                                *
      *                 Allow new flag at group admin darts file to   *
      *                 display message that other policies are listed*
     *                 on 3rd page of endorsement.                   *
     *                                                               *
     * 06/04/07 C:189 PROJECT: 099/SUPP#58380                        *
     *                Endorsement print/attach programs should not   *
      *                write/update WXPEP records if single           *
     *                endorsement print request                      *
      *
     *  10/13/06 C:65  PROJECT 114WGH
      *                 Find the hazard group of the payroll class
      *                 with the highest premium.
      *
     *  10/12/06 C:61  PROJECT 114WGH
      *                 Change to print NJ information.
      *                 (base code change).
      *                                                               *
     *  05/09/05 C:61  SUPPORT 46852                                 *
      *                 This form to replace WC290503B after 01/01/05 *
      *                 (New Jersey).                                 *
      *                                                               *
     *  06/24/04 C:65  PROJECT: 114WHO                               *
     *                 Add processing to write endorsement data to   *
     *                 the WXPEP reporting file for PA/DE/NJ states. *
     *                                                               *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     * Billing Schedule
     fwtabl     if   e           k disk    include(pbs01)
    
      ** Additional Work File
     Fwxfmp3    o    e             disk    usropn
     F*** begin add ***  06/24/04  *****************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     f*** end add ***  06/24/04  *******************************************
     F*** begin add ***  10/13/06  *****************************************
     Fwmepl1    if   e           k disk
     FWMPRL1    IF   E           K DISK
     f*** end add *****  10/13/06  *****************************************
     F*** begin add ***  06/25/08  *****************************************
     Fwmexl     IF   E           K DISK
     f*** end add *****  06/25/08  *****************************************
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D*** begin add ***  06/24/04  *****************************************
     D/COPY WWCPOLDAT4
     D*** end add *****  06/24/04  *****************************************
    
     * factors by state
    d fact            ds                  occurs(60)
    d  x1st                          2a
     d  x1elp                         7  4
     d  x1elpf                        7  4
     d  x1tax                         7  4
     d  x1taxf                        7  4
     d  x1rdf1                        7  4
     d  x1rdf2                        7  4
     d  x1rdf3                        7  4
     d  x1num                         5  0
     c*** begin add ***  10/13/06  *****************************************
     * payroll array
    d xpara           ds                  occurs(255)
    d  xpclas                        5  0
     d  xpprem                       15  2
     d  xphg                          1
     c*** end add *****  10/13/06  *****************************************
    
    
    
     *=========================================================================
     *  stand alone
     *=========================================================================
     d x1              s              2  0
      *** begin add ***  06/24/04  *****************************************
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
      *** end add *****  06/24/04  *****************************************
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('WC000503A ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
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
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

      * Employer Master

     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#

      * Billing Schedule

     c     bsky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10
     C*** begin add ***  06/24/04  *****************************************

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

     C*** end add *****  06/24/04  *****************************************
    
     c*** begin add ***  10/13/06  *****************************************

     C     epky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxfyr
     C                   KFld                    xxemp#
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    XXEFDT            7 0
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     c*** end add *****  10/13/06  *****************************************
    
     c*** begin add ***  06/25/08  *****************************************
     C     exky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     c*** end add *****  06/25/08  *****************************************
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  ADDFACT - Add factors by state                                     d
     *=========================================================================
     c     addfact       begsr

     c                   eval      x1 = 1
    
     c     x1            Dowle     60
     c     x1            occur     fact
      *
     c                   select
      ******* beg del ******** 10/12/06 **********************************
     c*****rsjrst        wheneq    'NJ'
     c*****              leave
      ******* end del ******** 10/12/06 **********************************
      *
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
      *
     C     rsjrst        wheneq    x1st
     c                   leave
      *
     C                   other
     c                   add       1             x1
     c                   iter
      *
     c                   endsl
      *
     C                   EndDo

     C                   endsr
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr
    
     ** open work file
     c                   open      wxfmp3
     c*** begin add ***  06/25/08  *****************************************
     c     exky          Chain     pex01                              60
     c*
     c                   move      exretg        fmfl01
     c*** end add *****  06/25/08  *****************************************
    
     *    initialize
     c                   clear                   pfm03
     c                   movel(p)  xxform        fmform
    
    
     * get info from state reporting division
    
     c     emky1         Chain     pem01                              60
    
     c                   Dow       (*in60 = *off) and (emstrp <> 'Y')
     c     emky1         reade     pem01                                  60
     c                   EndDo
    
     c                   If        (*in60 = *off)
    c                   eval      fmam01 = elretn
    c                   eval      fma541 = ellcvf
    c                   eval      fma542 = elmnfc
    c                   eval      fma543 = elmxfc
    c                   eval      fmam02 = elstpr
    c                   eval      fma544 = elbsfc
     c                   EndIf
    
     * get factors for each state
    
    c                   clear     *all          fact
    
     c     elky2         Chain     pel02                              60
     *
     c                   Dow       *in60 = *off
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))
    
     c     rsky          Chain     prs01                              60
     *
     c     *in60         Ifeq      *off
    
     *  add to factors for state
     c                   exsr      addfact
     c                   EndIf
     *
     c                   EndIf
     *
     c     elky2         ReadE     pel02                                  60
     c                   EndDo
    
     *  write factors to output work file
     c                   exsr      wrtfact
    
     ** Write the output record
     C                   write     pfm01
     C                   write     pfm03
     *
     ** Close the endorsement form workfile
     C                   close     wxfmp3
    
     ** Write records to the Policy Page Endorsement Transaction File
     c                   exsr      transwrt
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr

     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))
    
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
      ******* beg del ****** 05/09/05 ************************************
     C***********                  and (rsjrst <> 'NJ')
      ******* end del ****** 05/09/05 ************************************
      ******* beg add ****** 10/22/08 ************************************
     C                             and (rsjrst <> 'CA')
      ******* end add ****** 10/22/08 ************************************
    
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
    
     C                   endif
    
     C                   endsr
    
    
     *=========================================================================
     *  WRTFACT - Write Factors to output work file                        d
     *=========================================================================
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
    
    
    
     *=========================================================================
     *  TRANSWRT - Write records to Policy Page End Transaction File       d
     *=========================================================================
     c     transwrt      begsr

     C*** begin add ***  06/24/04  *****************************************
     C*** begin add ***  06/04/07  *****************************************
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
     C*** End add******  06/04/07  *****************************************
      *
      * Update the endorsement reporting file with the proper data.
      *
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
      *
      * Copy data into the record.
      *
     c                   If        *in60 = *off
     c*** begin add ***  10/13/06  *****************************************
     c                   move      *blank        svhg              1
     c                   exsr      payhg
     c*** end add *****  10/13/06  *****************************************
     c                   Z-add     1             O                 2 0
     c     O             occur     fact

     c                   if        x1num <> 0
     c                   movel(p)  x1st          XXKEY
     c     stky          chain     pst01                              60

     c                   if        *in60 = *off
     c                   MOVE      STNST         PEST
     c                   endif

     c                   move      *ZEROS        E00503
     c                   z-add     fmam01        xxLLIM
     c                   z-add     fma541        XXLCON
     c*** begin add ***  10/13/06  *************************************************
     c                   move      SVHG          XXHG
     c*** end add *****  10/13/06  *************************************************

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

     c                   ADD       1             O

     c     O             DOWLE     5
     c     O             occur     fact

     c                   if        x1num <> 0
     c                   movel(p)  x1st          XXKEY
     c     stky          chain     pst01                              60

     c                   if        *in60 = *off
     c                   MOVE      STNST         PEST
     c                   endif

     c                   move      *ZEROS        E00503
     c                   z-add     fmam01        XXLLIM
     c                   z-add     fma541        XXLCON
     c*** begin add ***  10/13/06  *************************************************
     c                   move      SVHG          XXHG
     c*** end add *****  10/13/06  *************************************************

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

     c                   ADD       1             O
     c                   ENDDO
      *
     c                   endif                                                  *in60 ifeq off
      *
      *** end add *****  06/24/04  *****************************************
     C*** begin add ***  06/04/07  *****************************************
     C                   endif
     C*** End add******  06/04/07  *****************************************

     c                   endsr

     c*** begin add ***  10/13/06  *****************************************
     c*
     c* Subroutine to determine hazard group for policy.
     c*
     c     payhg         begsr
     c*
     C                   Z-ADD     XXDIV         SVDIV             5 0
     c                   z-add     elpfdt        xxefdt
     c*
     c                   z-add     0             svcode            5 0
     C                   z-add     0             svprem           15 2
     C*
     c     1             do        255           z                 3 0
     C     z             occur     xpara
     C                   clear                   xpara
     C                   enddo
     C*
     C* CHECK ALL PAYROLL RECORDS FOR POLICY YEAR.
     C*
     C     epky          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     EPDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C*  SET MULTI OCCR DS TO MATCHING CLASS ELEMENT OR NEXT AVAILABLE.
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C* NEXT AVAILABLE
     C     XPCLAS        IFEQ      *ZERO
     C                   MOVE      EPCLAS        XPCLAS
     C                   LEAVE
     C                   ENDIF
     C* MATCHING CLASS
     C     XPCLAS        IFEQ      EPCLAS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* TOTAL PAYROLL FIELD.
     C*
     C                   ADD       EPEMPR        XPPREM
     C*
     C                   Z-ADD     XPCLAS        XXCLAS
     C*
     C     PRKEY         SETGT     PPR01
     C     PRKY2         readpe    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PRHZGR        XPHG
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
     C*
     C* CHECK FOR CLASS WITH HIGHEST PREMIUM TOTAL.
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C*
     C     XPPREM        IFGE      SVPREM
     C     XPCLAS        ANDNE     *ZERO
     C                   Z-ADD     XPCLAS        SVCODE
     C                   Z-ADD     XPPREM        SVPREM
     c                   MOVE      XPHG          SVHG
     C                   ENDIF
     C*
     C                   ENDDO
     c*
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     c*
     c                   endsr
     c*** end add *****  10/13/06  *****************************************
    
