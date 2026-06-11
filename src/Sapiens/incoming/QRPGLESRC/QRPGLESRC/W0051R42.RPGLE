      * (C) Copyright Progressive Data Solutions, Inc., 2004
      *
      /title W0051R42 - New Business Tracking
      *****************************************************************
      *
      * CALLED FROM: W0051R
      *
      *****************************************************************
      * Revised:
      *
      * 12/14/07 C:136 Support: WA114 61245
      *                Default year created date to system date in ADD mode.
      *
      * 03/19/07  C:180 Project 114WMF
      *                 No changes to source - Just added to proj 114wmf
      *
      * 08/02/05  C:132 Project wa114 - 48353
      *                 Policy Tracking - Remove 'Policy Requested Date'
      *                 and change "Policy Created Date" to "Panel
      *                 Requested Date" on screen one.
      *
      * 05/31/05  C:156 Project 114WKB
      *                 Policy Tracking - Additional Dates
      *
      * Created 03/25/04  C:61  Project 114WHL
      *****************************************************************

     *
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0051f42  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwdrtp     uf a e             disk
     F                                     rename(prt01:dbrcdf)

     Fwdrtl     if a e           k disk    infds(rtfds)
     *
     *  Employer Policy year
     Fwdelp     if a e             disk


     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'Renewal Tracking'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1


     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D FN_rrn          s              9  0
     D tmp7            s              7  0
      *** beg add ***** 12/14/07 *******************************************
     D xxycdt          s              8  0
      *** end add ***** 12/14/07 *******************************************


     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------

     D rtfds           ds
     D rt_rrn                397    400b 0

     *

     D/copy SPRSTRING
     D/copy WPRGROUP
     D/copy WPREMPLR
     C/COPY SCOPYMT01C

     *
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn                          wdrtp
     C                   parm                    @@rrn1                         wdelp
     *
     * Program expects @@RRN1 from WDELP passed in.
     *
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam


     *================================================================
     *  KLIST definitions
     *================================================================
      *
      * Renewal Tracking
      *
     C     rtky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    elfyr



     *================================================================
     *  *LIKE DEFN statements
     *================================================================


     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr

      * Policy Year Created
     c                   if        (*in90 = *off) and (scycdt <> 0)

     c                   z-add     scycdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scycdt

     c                   if        srerr = *on
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Underwriter Review Date
     c                   if        (*in90 = *off) and (scuwdt <> 0)

     c                   z-add     scuwdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scuwdt

     c                   if        srerr = *on
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Quote to Agent Date
     c                   if        (*in90 = *off) and (scqadt <> 0)

     c                   z-add     scqadt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scqadt

     c                   if        srerr = *on
     c                   eval      *in73 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Agent Approval Date
     c                   if        (*in90 = *off) and (scaadt <> 0)

     c                   z-add     scaadt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scaadt

     c                   if        srerr = *on
     c                   eval      *in74 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Policy Generated Date
     c                   if        (*in90 = *off) and (scpgdt <> 0)

     c                   z-add     scpgdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpgdt

     c                   if        srerr = *on
     c                   eval      *in75 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Policy Mailed Date
     c                   if        (*in90 = *off) and (scpold <> 0)

     c                   z-add     scpold        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpold

     c                   if        srerr = *on
     c                   eval      *in76 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

      *************
      ***** BEG DEL ***** 08/02/05 *************************************
      ***  begin add  ***  05/31/05 ***********************************
      * Policy Requested Date entered
     c*************      if        (*in90 = *off) and (scprdt <> 0)
      *************
     c*************      z-add     scprdt        SRDTI
     c*************      CALL      'SRCVTDAT'    DATCHK
     c*************      movel     srdto         scprdt
      *************
     c*************      if        srerr = *on
     c*************      eval      *in81 = *on
     c*************      eval      *in90 = *on
     c*************      endif
      *************
     c*************      endif
      ***** END DEL ***** 08/02/05 *************************************

      * Policy Year Created
     c                   if        (*in90 = *off) and (scpcdt <> 0)

     c                   z-add     scpcdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpcdt

     c                   if        srerr = *on
     c                   eval      *in82 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif
      ***  begin add  ***  05/31/05 ***********************************

      * Panel Mailed Date
     c                   if        (*in90 = *off) and (scpand <> 0)

     c                   z-add     scpand        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpand

     c                   if        srerr = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

      ***  begin add  ***  05/31/05 ***********************************
      * Panel Posted Date
     c                   if        (*in90 = *off) and (scppdt <> 0)

     c                   z-add     scppdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scppdt

     c                   if        srerr = *on
     c                   eval      *in83 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif
      ***  end   add  ***  05/31/05 ***********************************


     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     C                   Z-ADD     rtycdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scycdt


     C                   Z-ADD     rtuwdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scuwdt

     C                   Z-ADD     rtqadt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scqadt

     C                   Z-ADD     rtaadt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scaadt

     C                   Z-ADD     rtpgdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpgdt

     C                   Z-ADD     rtpold        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpold

     C                   Z-ADD     rtpand        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpand

     C                   Z-ADD     rtycdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scycdt

      *** beg add ***** 12/14/07 *************************************
     C                   if        @@mode = 'A'
     C                   z-add     xxycdt        scycdt
     C                   endif
      *** end add ***** 12/14/07 *************************************

      ***** BEG DEL ***** 08/02/05 *************************************
     C***********        Z-ADD     rtprdt        SRDTI
     C***********        CALL      'SRCVTDAT'    DTMDYY
     C***********        movel     srdto         scprdt
      ***** END DEL ***** 08/02/05 *************************************

     C                   Z-ADD     rtpcdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpcdt

     C                   Z-ADD     rtppdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scppdt

     C                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     C                   z-add     elco#         rtco#
     C                   z-add     elfnd         rtfnd
     C                   z-add     elemp#        rtemp#
     C                   z-add     elfyr         rtfyr

     C                   z-add     scycdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtycdt

     C                   z-add     scuwdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtuwdt

     C                   z-add     scqadt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtqadt

     C                   z-add     scaadt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtaadt

     C                   z-add     scpgdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtpgdt

     C                   z-add     scpold        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtpold

     C                   z-add     scpand        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtpand

      ***** BEG DEL ***** 08/02/05 *************************************
     C**********         z-add     scprdt        SRDTI
     C**********         call      'SRCVTDAT'    dtcymd
     C**********         movel     srdto         rtprdt
      ***** END DEL ***** 08/02/05 *************************************

     C                   z-add     scpcdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtpcdt

     C                   z-add     scppdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtppdt

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr

     c                   eval      *inlr = *on

     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr

      * add mode
     C     @@MODE        COMP      'A'                                    40

     c                   eval      scprds = optnm
     C                   eval      scprds = centerFld(scprds)

     C     @@rrn1        chain     wdelp
     C                   eval      scfnd = elfyr
     C                   eval      scfyr = elfyr
      *
      * Get CX date
      *
     C                   eval      tmp7 = GetCxDate(ELCO#:ELFND:
     C                                                       ELEMP#:ELDIV)
      *
     C                   Z-ADD     tmp7          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         sccxdt
      *
     C                   eval      *in50 = sccxdt <> *zero
      *
      * Get Employer Name
      *
     C                   eval      scedn = EmpDivName(ELEMP#:ELDIV:EM_rrn:
     C                                                ELFND:ELCO#)
      *
      *  GET Renewal Tracking Information
      *
     C                   CLEAR                   dbrcdf
      *
     C     RTKY          CHAIN     PRT01
      *
     C                   if        %found
     C                   eval      *in40  = *off
     C                   eval      @@rrn  = rt_rrn

     c                   else

     C                   if        @@mode = 'C'
     C                   eval      *in40  = *on
     C                   eval      @@mode = 'A'
     C                   endif
      *
     C                   endif
      *
      *  GET Group Name
      *
     C                   eval      scfnam   = GetGrpName(ELFND:FN_rrn:ELCO#)
      *
      *  GET Agent/Producer Information
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C     SCAGT#        PARM                    #1AGT#            5 0
     C                   PARM                    #1ANAM           60
     C     SCPROD        PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
      *
     C                   eval      SCANAM = #1ANAM
     C                   eval      SCPNAM = #1PNAM

      *** beg add ***** 12/14/07 *******************************************************
     C                   if        @@mode = 'A'
     C                   time                    dsdttm
     C                   eval      xxycdt = dsdate
     C                   endif
      *** end add ***** 12/14/07 *******************************************************

     C                   endsr


     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb

     C                   eval      rtuser = q1user
     C                   eval      rtpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      rttime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         rtdt

     C                   update    dbrcdf
E01  C                   endif
     c                   eval      outflg = *on
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb

     C                   eval      rtuser = q1user
     C                   eval      rtiusr = q1user
     C                   eval      rtpgm = q1pgm
     C                   eval      rtipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      rttime = dstime
     C                   eval      rtitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         rtdt
     C                   movel(p)  srdto         rtidt

     C                   write     dbrcdf
     C                   feod      wdrtp
     c                   eval      outflg = *on
     C                   endsr


