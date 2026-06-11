     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/title Fix program for Back To Base - A/R Changes
     H*********************************************************************
     H* Created: 04/21/10    C:33    Project: 25WRB                       *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
     Fwtabl     uf   e           k disk    usropn include(ptt01)
     Fwmeal27   uf   e           k disk    usropn rename(pea01:mea27)
     Fwdeal27   uf   e           k disk    usropn rename(pea01:dea27)
     Fwmxal27   uf   e           k disk    usropn rename(pxa01:mxa27)
     Fwmeap     o    e             disk    usropn rename(pea01:mea0p)

     Dtest             s              1
     Dtesting          s              1n   inz
     Dco#              s                   like(TTCO#) inz(25)
     DkkKey            s                   like(TTKEY)
     DkkCode           s                   like(EAARCD)
     Dlib              s             10
     Dx                s              3  0
     DsysDt7           s                   like(TTDT)

     D psds          esds                  extname(szq1p)

     D#ofDltCd         c                   7
     D dltCode         s              2    dim(#ofDltCd) ctdata perrcd(1)
     D#ofInactive      c                   2
     D inactivateCode  s              2    dim(#ofInactive) ctdata perrcd(1)
     D#ofTypCatChg     c                   12
     D chgCode         s              2    dim(#ofTypCatChg) ctdata perrcd(1)
     D typeCat         s              2    dim(#ofTypCatChg) alt(chgCode)
     D#ofCodeUpdate    c                   13
     D fromCode        s              2    dim(#ofCodeUpdate) ctdata perrcd(1)
     D toCode          s              2    dim(#ofCodeUpdate) alt(fromCode)

     DCrtQTempCmd      pr            80
     D file                          10    const

     DUpdateWTTTP      pr
     D code                                const like(TTKEY)
     D chgType                        1    const
     D type                                const like(TTTYPE) options(*nopass)
     D category                            const like(TTCTYP) options(*nopass)

     DUpdateAllWMEAP   pr
     DUpdateAllWDEAP   pr
     DUpdateAllWMXAP   pr

     DUpdateWMEAP      pr
     D oldCode                             const like(EAARCD)
     D newCode                             const like(EAARCD)

     DUpdateWDEAP      pr
     D oldCode                             const like(EAARCD)
     D newCode                             const like(EAARCD)

     DUpdateWMXAP      pr
     D oldCode                             const like(EAARCD)
     D newCode                             const like(EAARCD)

     DOvrCmd           pr            80
     D file                          10    const
     D library                       10    const

     DDltOvrCmd        pr            80
     D file                          10    const

     D/copy sPrDate
     D/copy sPrRunCmd

     C     key           klist
     C                   kfld                    co#
     C                   kfld                    kkKey

     C     keyAR         klist
     C                   kfld                    co#
     C                   kfld                    kkCode

     C     *entry        plist
     C                   parm                    test

     C                   if        %parms >= 1
     C                   eval      testing = test = 'Y'
     C                   endif

     C                   if        testing
     C                   exsr      CrtQTempFiles
     C                   endif

     C                   eval      sysDt7 = DateTo7(SystemDate)
     C                   exsr      OvrWTTTP
     C* Read through codes to be deleted
     C     1             do        #ofDltCd      x
     C                   callp     UpdateWTTTP(dltCode(x):'D')
     C                   enddo
     C* Read through codes to be inactivated
     C     1             do        #ofInactive   x
     C                   callp     UpdateWTTTP(inactivateCode(x):'I')
     C                   enddo
     C* Read through codes to have the types and categories changed
     C     1             do        #ofTypCatChg  x
     C                   callp     UpdateWTTTP(chgCode(x):'T'
     C                                        :%subst(typeCat(x):1:1)
     C                                        :%subst(typeCat(x):2:1)
     C                                        )
     C                   enddo
     C*
     C* Read through codes to be changed in A/R
     C     1             do        #ofCodeUpdate x
     C*   Live
     C                   exsr      LiveOvr
     C                   callp     UpdateWMEAP(fromCode(x):toCode(x))
     C                   callp     UpdateWDEAP(fromCode(x):toCode(x))
     C                   callp     UpdateWMXAP(fromCode(x):toCode(x))
     C                   if        testing
     C                   iter
     C                   endif
     C*   Quote
     C                   exsr      QuoteOvr
     C                   callp     UpdateWMEAP(fromCode(x):toCode(x))
     C                   callp     UpdateWDEAP(fromCode(x):toCode(x))
     C*   Month-end
     C                   exsr      MthOvr
     C                   callp     UpdateWMEAP(fromCode(x):toCode(x))
     C                   callp     UpdateWMXAP(fromCode(x):toCode(x))
     C*   Test
     C                   exsr      TestOvr
     C                   callp     UpdateWMEAP(fromCode(x):toCode(x))
     C                   callp     UpdateWDEAP(fromCode(x):toCode(x))
     C                   callp     UpdateWMXAP(fromCode(x):toCode(x))
     C*
     C                   callp     UpdateWTTTP(fromCode(x):'D')
     C*
     C                   enddo
      * Read thru all A/R and update type/cat for A/R code used
     C*   Live
     C                   exsr      LiveOvr
     C                   callp     UpdateAllWMEAP
     C                   callp     UpdateAllWDEAP
     C                   callp     UpdateAllWMXAP
     C*
     C                   if        not testing
     C*   Quote
     C                   exsr      QuoteOvr
     C                   callp     UpdateAllWMEAP
     C                   callp     UpdateAllWDEAP
     C*   Month-end
     C                   exsr      MthOvr
     C                   callp     UpdateAllWMEAP
     C                   callp     UpdateAllWMXAP
     C*   Test
     C                   exsr      TestOvr
     C                   callp     UpdateAllWMEAP
     C                   callp     UpdateAllWDEAP
     C                   callp     UpdateAllWMXAP
     C*
     C                   endif
     C                   eval      *inlr = *on

     C*=========
     C     CrtQTempFiles begsr
     C*
     C* Clear QTEMP
     C                   callp     RunCmd('CLRLIB LIB(QTEMP)':80)
     C*
     C* ALL table files needed for logical
     C                   callp     RunCmd(CrtQTempCmd('WTATP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTSPP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTNCP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTNAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTPAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTCAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTRCP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTRTP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTPCP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTB2P'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTB7P'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTBSP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTCCP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTBCP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTCSP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTCNP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTRSP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTLRP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTVSP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTTRP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTHYP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTSYP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTBAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTIDP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTISP'):80)
     C*
     C                   callp     RunCmd(CrtQTempCmd('WTTTP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WTABL'):80)
     C*
     C                   callp     RunCmd(CrtQTempCmd('WMEAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WMEAL27'):80)
     C*
     C                   callp     RunCmd(CrtQTempCmd('WDEAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WDEAL27'):80)
     C*
     C                   callp     RunCmd(CrtQTempCmd('WMXAP'):80)
     C                   callp     RunCmd(CrtQTempCmd('WMXAL27'):80)
     C*
     C                   endsr

     C*=========
     C     OvrWDEAL27    begsr
     C                   if        %open(WDEAL27)
     C                   close     WDEAL27
     C                   endif
     C                   callp(e)  RunCmd(DltOvrCmd('WDEAL27'):80)
     C                   callp     RunCmd(OvrCmd('WDEAL27':lib):80)
     C                   open      WDEAL27
     C                   endsr

     C*=========
     C     OvrWMEAP      begsr
     C                   eval      lib = 'PDSFIL'
     C                   if        testing
     C                   eval      lib = 'QTEMP'
     C                   endif
     C                   if        %open(WMEAP)
     C                   close     WMEAP
     C                   endif
     C                   callp(e)  RunCmd(DltOvrCmd('WMEAP'):80)
     C                   callp     RunCmd(OvrCmd('WMEAP':lib):80)
     C                   open      WMEAP
     C                   endsr

     C*=========
     C     OvrWMEAL27    begsr
     C                   if        %open(WMEAL27)
     C                   close     WMEAL27
     C                   endif
     C                   callp(e)  RunCmd(DltOvrCmd('WMEAL27'):80)
     C                   callp     RunCmd(OvrCmd('WMEAL27':lib):80)
     C                   open      WMEAL27
     C                   endsr

     C*=========
     C     OvrWMXAL27    begsr
     C                   if        %open(WMXAL27)
     C                   close     WMXAL27
     C                   endif
     C                   callp(e)  RunCmd(DltOvrCmd('WMXAL27'):80)
     C                   callp     RunCmd(OvrCmd('WMXAL27':lib):80)
     C                   open      WMXAL27
     C                   endsr

     C*=========
     C     OvrWTTTP      begsr
     C                   eval      lib = 'PDSFIL'
     C                   if        testing
     C                   eval      lib = 'QTEMP'
     C                   endif
     C                   if        %open(WTABL)
     C                   close     WTABL
     C                   endif
     C                   callp(e)  RunCmd(DltOvrCmd('WTABL'):80)
     C                   callp     RunCmd(OvrCmd('WTABL':lib):80)
     C                   open      WTABL
     C                   endsr

     C*=========
     C     LiveOvr       begsr
     C*
     C                   if        testing
     C                   eval      lib = 'QTEMP'
     C                   else
     C                   eval      lib = 'PDSFIL'
     C                   endif
     C*
     C                   exsr      OvrWMEAL27
     C                   exsr      OvrWDEAL27
     C                   exsr      OvrWMXAL27
     C*
     C                   endsr

     C*=========
     C     MthOvr        begsr
     C*
     C                   eval      lib = 'PDSMTH'
     C*
     C                   exsr      OvrWMEAL27
     C                   exsr      OvrWMXAL27
     C*
     C                   endsr

     C*=========
     C     QuoteOvr      begsr
     C*
     C                   eval      lib = 'PDSQTE'
     C*
     C                   exsr      OvrWMEAL27
     C                   exsr      OvrWDEAL27
     C*
     C                   endsr

     C*=========
     C     TestOvr       begsr
     C*
     C                   eval      lib = 'PDSTST'
     C*
     C                   exsr      OvrWMEAL27
     C                   exsr      OvrWDEAL27
     C                   exsr      OvrWMXAL27
     C*
     C                   endsr





     P*====================================================================
     P* Update WTTTP
     P*====================================================================
     PUpdateWTTTP      b
     DUpdateWTTTP      pi
     D code                                const like(TTKEY)
     D chgType                        1    const
     D type                                const like(TTTYPE) options(*nopass)
     D category                            const like(TTCTYP) options(*nopass)

     C                   eval      kkKey = code

     C                   select
     C                   when      chgType = 'D'
     C                   exsr      DeleteCode
     C                   when      chgType = 'I'
     C                   exsr      Inactivate
     C                   when      chgType = 'T'
     C                   exsr      ChgTypeCat
     C                   endsl

     C*=========
     C     ChgTypeCat    begsr
     C     key           chain     ptt01
     C                   if        %found
     C                              and (TTTYPE <> type
     C                                or TTCTYP <> category)
     C                   eval      TTTYPE = type
     C                   eval      TTCTYP = category
     C                   exsr      MaintFlds
     C                   update    ptt01
     C                   endif
     C                   endsr
     C*=========
     C     DeleteCode    begsr
     C     key           delete    ptt01                              99
     C                   endsr
     C*=========
     C     Inactivate    begsr
     C     key           chain     ptt01
     C                   if        %found
     C                              and TTSTAT <> 'I'
     C                   eval      TTSTAT = 'I'
     C                   exsr      MaintFlds
     C                   update    ptt01
     C                   endif
     C                   endsr
     C*=========
     C     MaintFlds     begsr
     C                   eval      TTPGM  = q1Pgm
     C                   eval      TTUSER = q1User
     C                   eval      TTDT   = sysDt7
     C                   eval      TTTIME = TimeTo6(SystemTime)
     C                   endsr
     PUpdateWTTTP      e

     P*====================================================================
     P* Update all WMEAP
     P*====================================================================
     PUpdateAllWMEAP   b
     DUpdateAllWMEAP   pi

     C     co#           setll     mea27
     C                   dou       %eof
     C     co#           reade     mea27
     C                   if        not %eof
     C                   eval      kkKey = EAARCD
     C                   clear                   TTTYPE
     C                   clear                   TTCTYP
     C     key           chain(n)  ptt01
     C                   if        %found
     C                              and (EAARTY <> TTTYPE
     C                                or EACTYP <> TTCTYP)
     C                   eval      EAARTY = TTTYPE
     C                   eval      EACTYP = TTCTYP
     C                   eval      EAPGM  = q1Pgm
     C                   update    mea27
     C                   endif
     C                   endif
     C                   enddo

     PUpdateAllWMEAP   e

     P*====================================================================
     P* Update all WDEAP
     P*====================================================================
     PUpdateAllWDEAP   b
     DUpdateAllWDEAP   pi

     C     co#           setll     dea27
     C                   dou       %eof
     C     co#           reade     dea27
     C                   if        not %eof
     C                   eval      kkKey = EAARCD
     C                   clear                   TTTYPE
     C                   clear                   TTCTYP
     C     key           chain(n)  ptt01
     C                   if        %found
     C                              and (EACTYP <> TTCTYP)
     C                   eval      EACTYP = TTCTYP
     C                   eval      EAPGM  = q1Pgm
     C                   update    dea27
     C                   endif
     C                   endif
     C                   enddo

     PUpdateAllWDEAP   e

     P*====================================================================
     P* Update all WMXAP
     P*====================================================================
     PUpdateAllWMXAP   b
     DUpdateAllWMXAP   pi

     C     co#           setll     mxa27
     C                   dou       %eof
     C     co#           reade     mxa27
     C                   if        not %eof
     C                   eval      kkKey = XAARCD
     C                   clear                   TTTYPE
     C                   clear                   TTCTYP
     C     key           chain(n)  ptt01
     C                   if        %found
     C                              and (XAARTY <> TTTYPE
     C                                or XACTYP <> TTCTYP)
     C                   eval      XAARTY = TTTYPE
     C                   eval      XACTYP = TTCTYP
     C                   eval      XAPGM  = q1Pgm
     C                   update    mxa27
     C                   endif
     C                   endif
     C                   enddo

     PUpdateAllWMXAP   e

     P*====================================================================
     P* Update WMEAP
     P*====================================================================
     PUpdateWMEAP      b
     DUpdateWMEAP      pi
     D oldCode                             const like(EAARCD)
     D newCode                             const like(EAARCD)

     C                   eval      kkCode = oldCode
     C     keyAR         setll     mea27
     C                   dou       %eof
     C     keyAR         reade     mea27
     C                   if        not %eof
     C                   eval      EAARCD = newCode
     C                   eval      EAPGM = q1Pgm
     C                   update    mea27
     C                   endif
     C                   enddo

     PUpdateWMEAP      e

     P*====================================================================
     P* Update WDEAP
     P*====================================================================
     PUpdateWDEAP      b
     DUpdateWDEAP      pi
     D oldCode                             const like(EAARCD)
     D newCode                             const like(EAARCD)

     C                   eval      kkCode = oldCode
     C     keyAR         setll     dea27
     C                   dou       %eof
     C     keyAR         reade     dea27
     C                   if        not %eof
     C                   eval      EAARCD = newCode
     C                   eval      EAPGM = q1Pgm
     C                   update    dea27
     C                   endif
     C                   enddo

     PUpdateWDEAP      e

     P*====================================================================
     P* Update WMXAP
     P*====================================================================
     PUpdateWMXAP      b
     DUpdateWMXAP      pi
     D oldCode                             const like(EAARCD)
     D newCode                             const like(EAARCD)

     C                   eval      kkCode = oldCode
     C     keyAR         setll     mxa27
     C                   dou       %eof
     C     keyAR         reade     mxa27
     C                   if        not %eof
     C                   eval      XAARCD = newCode
     C                   eval      XAPGM = q1Pgm
     C                   update    mxa27
     C                   endif
     C                   enddo

     PUpdateWMXAP      e

     P*====================================================================
     P* Create overrride command
     P*====================================================================
     POvrCmd           b
     DOvrCmd           pi            80
     D file                          10    const
     D library                       10    const

     D cmd             s             80

     C                   eval      cmd = 'OVRDBF FILE(' + %trim(file) +
     C                                               ')' +
     C                                       ' TOFILE(' + %trim(library) +
     C                                             '/' +  %trim(file) +
     C                                               ')'
     C                   return    cmd

     POvrCmd           e

     P*====================================================================
     P* Create delete overrride command
     P*====================================================================
     PDltOvrCmd        b
     DDltOvrCmd        pi            80
     D file                          10    const

     C                   return    'DLTOVR FILE(' + %trim(file) + ')'

     PDltOvrCmd        e

     P*====================================================================
     P* Create QTEMP duplication command
     P*====================================================================
     PCrtQTempCmd      b
     DCrtQTempCmd      pi            80
     D file                          10    const

     C                   return    'CRTDUPOBJ OBJ(' + %trim(file) + ')' +
     C                             ' FROMLIB(PDSFIL) OBJTYPE(*FILE)' +
     C                             ' TOLIB(QTEMP) DATA(*YES)'

     PCrtQTempCmd      e

** Delete
AA
PP
UC
VD
XB
XP
XT
** Inactivate
??CL
WO
** Change Type/Cat
CAFF
CFFF
CLFR
CDQX
RADP
WCFF
CAQX
CHQF
PTQR
TCQF
TRQR
UCQF
** Change & Delete
SDCP
UNCP
RODR
CMIA
IIIA
RAPA
????CLRD
CRRF
AURF
TATD
TETD
TRTD
TXTP
