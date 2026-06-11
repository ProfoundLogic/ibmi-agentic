     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=============================================================================================
      /title WA060R - API to Calculate Dev 1 for CareWest
     H option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR')
     H debug(*yes)
     H optimize(*none)
      *=============================================================================================
      *                                                                                            *
      *  This program is called from W8010R (Est Bill) and W7720R (Audit Bill). It is passed the   *
      *  WDELP rrn and passes back the DEV1 factor                                                 *
      *                                                                                            *
      *  Parameters:                                                                               *
      *    @@rrn - rrn for WDELP                                                                   *
      *                                                                                            *
      *  CALLED FROM: W8010R/W7720R                                                                *
      *                                                                                            *
      *  CREATED 12/18/05   C:136   PROJECT: 136WBH                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 05/08/12 C:279 Support: WA136  79799
      *                Modified to consider state reporting division's
      *                effective date for commission mod factor.
      *
      * 04/25/08 C:180 PROJECT: C136WDJ - PRISS 10
      *                Secondary rate to come from Secondary Agent, not
      *                from Agent of Record.
      *
      * 03/14/08 C:180 PROJECT: C136WDJ
      *                Derive accurate discount calculation.
      *
      * 04/10/06 C:136 Support: WA136 52134 (136WBH Client Specific)                               *
      *                If no hit on WTMFP, return a value of one.                                  *
      *                                                                                            *
      * 01/19/06  C:136 Project: 136WBH                                                            *
      *                 Use ELPFDT for key value in looking up Agent Rates if System date is <     *
      *                 ELPFDT.                                                                    *
      *                                                                                            *
      *=============================================================================================
     fwdelp     if   e             disk    rename( pel01 : pel0p )
     fwtmfl     if   e           k disk    rename( pmf01 : pmf0l )
     fwmahl     if   e           k disk    rename( pah01 : pah0l )
     fwmarl     if   e           k disk    rename( par01 : par0l )
     fwdell     if   e           k disk    rename( pel01 : pel0l ) prefix(l0)
     fwzexl     if   e           k disk    rename( pex01 : pex0l1)
      ******* Beg Add ******* 05/08/12 **********************************
     fwmeml     if   e           k disk
      ******* End Add ******* 05/08/12 **********************************
    d*=============================================================================================
    d* Stand alone fields
    d*=============================================================================================
     d sysDate         s                   like(ELPFDT)
     d agentName       s             40
     d AH_rrn          s              9  0
     d agtRate         s                   like(ARNWRT)
     d renewalFlag     s              1
      ******* BEGIN ADD ******* 03/14/08 ********************************
     d XXRT#           s                   LIKE(AHRT#) INZ
      ******* END ADD ********* 03/14/08 ********************************
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
     d WA060R          pr
     d  @@rrn                         9  0                                      of WDELP
     d  modFactor                     5  4
     d WA060R          pi
     d  @@rrn                         9  0
     d  modFactor                     5  4

     d/copy sprdate
    c*=============================================================================================
      *  KLIST'S
    c*=============================================================================================
     c     elky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV

     c     ahky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     ahky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c                   kfld                    sysDate

     c     arky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
      **** Begin chg *********   03/14/08 ******************************
     c****               kfld                    AHRT#
     c                   kfld                    XXRT#
      **** End chg ***********   03/14/08 ******************************

     c     arky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
      **** Begin chg *********   03/14/08 ******************************
     c****               kfld                    AHRT#
     c                   kfld                    XXRT#
      **** End chg ***********   03/14/08 ******************************
     c                   kfld                    sysDate

     c     mfky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND

     c     mfky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    agtRate
      *** beg chg ***** 04/10/06 ******************************************
     c***********        kfld                    ELPFDT

     c     mfky2         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    agtRate
     c                   kfld                    ELPFDT
      *** end chg ***** 04/10/06 ******************************************
      ******* Beg Add ******* 05/08/12 **********************************
     c* key list for WMEMP
     c     emky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
      ******* End Add ******* 05/08/12 **********************************
    c*=============================================================================================
      *  MAINLINE PROCESSING
    c*=============================================================================================
      *** beg chg ***** 04/10/06 *******************************************
     c***********        eval      modFactor = 0
     c                   eval      modFactor = 1
      *** end chg ***** 04/10/06 *******************************************
      *
     c     @@rrn         chain     pel0p
     c                   if        %found
     c                   eval      sysDate = DateTo7(SystemDate)
      *
      *** beg add ***** 01/19/06 *******************************************************
     c                   if        sysDate < ELPFDT
     c                   eval      sysDate = ELPFDT
     c                   endif
      *** end add ***** 01/19/06 *******************************************************
      *
      * Find out if policy is new or renewal
      *
     c     ahky          setll     pel0l
     c     elky          readpe    pel0l
     c                   if        not(%eof)
     c                   eval      renewalFlag = 'R'
     c                   else
     c     elky          chain     pex0l1
     c                   if        %found
     c                   eval      renewalFlag = 'R'
     c                   else
     c                   eval      renewalFlag = 'N'
     c                   endif
     c                   endif
      *
      *  Get rate plan from agent history
      *
     c     ahky1         setgt     pah0l
     c     ahky          readpe    pah0l
     c                   dow       not(%eof) and
     c                             AHSTAT = 'I'
     c     ahky          readpe    pah0l
     c                   enddo
      *
      *  Get rate from agent rates
      *
     c                   if        not(%eof)

      ***** Begin add ************** 03/14/08 ******************************
     C                   MOVE      AHRT#         XXRT#
      ***** End add **************** 03/14/08 ******************************
     C     arky1         setgt     par0l
      ***** Begin add ************** 03/14/08 ******************************
     C                   IF        NOT %FOUND
     C     *HIVAL        SETGT     par0l
     C                   ENDIF
      ***** End add **************** 03/14/08 ******************************

     c     arky          readpe    par0l
     c                   dow       not(%eof) and
     c                             ARSTAT = 'I'
     c     arky          readpe    par0l
     c                   enddo

     c                   if        not(%eof)

     c                   select
     c                   when      renewalFlag = 'N'
     c                   eval      agtRate = ARNWRT
     c                   when      renewalFlag = 'R'
     c                   eval      agtRate = ARRNRT
     c                   endsl

      ***** Begin add ************ 03/14/08 *****************************
      ****** BEGIN CHANGE ******************  04/25/08 ******************
     c******             if        ELFFL2 <> 'Y' and AH2RRC <> *ZERO
     C******             eval      XXRT# = AH2RRC
     c                   if        ELFFL2 <> 'Y' and AHSRT# <> *ZERO
     C                   eval      XXRT# = AHSRT#
      ****** END CHANGE ********************  04/25/08 ******************
     c     arky1         setgt     par0l
     c                   if        not %FOUND
     c     *HIVAL        SETGT     par0l
     c                   endif
     c     arky          readpe    par0l
     c                   dow       not(%eof) and
     c                             ARSTAT = 'I'
     c     arky          readpe    par0l
     c                   enddo

     c                   if        not(%eof)
     c                   select
     c                   when      renewalFlag = 'N'
     c                   eval      agtRate = agtRate + ARNWRT
     c                   when      renewalFlag = 'R'
     c                   eval      agtRate = agtRate + ARRNRT
     c                   endsl
     C                   endif
     C                   endif
      ***** End add ************** 03/14/08 *****************************
      ******* Beg Add ******* 05/08/12 **********************************
     c*    Get State reporting division
     c
     c     emky          setll     pem01
     c     emky          reade     pem01
     c                   dow       not %eof(wmeml)
     c
     c                   if        EMSTRP ='Y'
     c                   eval      ELDIV = EMDIV
     c*    Get policy effective date for state reporting division.
     c     ahky          chain     pel0l
     c                   eval      ELPFDT = L0ELPFDT
     c                   leave
     c                   endif
     c
     c     emky          reade     pem01
     c                   enddo
     c
      ******* End Add ******* 05/08/12 **********************************

     c     mfky1         setgt     pmf0l                              60
      ***** Begin add ************** 03/14/08 ******************************
     c                   if        not %FOUND
     c     *HIVAL        SETGT     pmf0l
     c                   endif
      ***** End add **************** 03/14/08 ******************************
     c     mfky          readpe    pmf0l
     c                   if        not(%eof)
      *** beg chg ***** 04/10/06 ******************************************
     c***********        eval(h)   modFactor = MFFCTR
     c                   eval(h)   agtRate  = MFRATE
     c     mfky2         setgt     pmf0l                              60
     c     mfky1         readpe    pmf0l
     c                   if        not(%eof)
     c                   eval(h)   modFactor = MFFCTR
     c                   endif
      *** end chg ***** 04/10/06 ******************************************
     c                   endif

     c                   endif

     c                   endif

     c                   endif

     c                   seton                                        lr
