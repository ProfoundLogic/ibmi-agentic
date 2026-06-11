     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*====================================================================
    H* WPICDEVAL- Claim Code-Validation Procedures
    H*====================================================================
     H nomain
    F*
     Fwtabl     if   e           k disk    infds(tabfds)
     Fwtgtl     if   e           k disk    infds(tabgds)
     Fwmall     if   e           k disk    infds(tabloc)
     Fwmepl7    if   e           k disk    infds(tabepp)
     Fstabl     if   e           k disk    infds(tabst)
     Fwmexl     if   e           k disk
     Fwmdpl     if   e           k disk    infds(tabdep)
     Fwtocl     if   e           k disk    infds(tabocc)

     D tabocc          ds
     D occrrn                397    400b 0
     D tabdep          ds
     D deprrn                397    400b 0
     D tabst           ds
     D strrn                 397    400b 0
     D tabepp          ds
     D epprrn                397    400b 0
     D tabloc          ds
     D loctrrn               397    400b 0
     D tabfds          ds
     D coderrn               397    400b 0
     D tabgds          ds
     D codgrrn               397    400b 0

     D conbr           s              3  0
     D tabKey          s             10

     D psds          esds                  extname(szq1p)

     D/copy wprcdeval

    P*++++++++ Miscellaneous Code Tables +++++++++++++++++++++++++++++++++

    P*====================================================================
    P* ValidOccup   - Validate Occupation code WTOCL
    P*
    P* ValidOccup   - return *on if occupation is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   Occucde    - passed in for validation  (required)
    P*====================================================================
     PValidOccup       b                   export
    P*
     DValidOccup       PI             1
     D co#                            3  0 const
     D Occucde                       10    const
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     occkey        klist
     C                   kfld                    conbr
     C                   kfld                    Occucde

     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C     occkey        chain     poc01                              60
     C                   if        *in60 = *off and OCSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = occrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = OCDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidOccup       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    P*====================================================================
    P* ValidOffice  - Validate Office  WMEXL
    P*
    P* ValidOffice  - return *on if department is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   Office#    - passed in for validation  (required)
    P*====================================================================
     PValidOffice      b                   export
    P*
     DValidOffice      PI             1
     D co#                            3  0 const
     D fund                           3  0 const
     D Office#                        5    const

     C     offkey        klist
     C                   kfld                    conbr
     C                   kfld                    fund

     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C     offkey        chain     pex01                              60
     C                   if        *in60 = *off and EXSTAT <> 'A' and
     C                             Office# <> EXDOFF
     C                   eval      *in60 = *on
     C                   endif

     C                   return    not *in60

     PValidOffice      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    P*====================================================================
    P* ValidDept    - Validate Department  WMDPL
    P*
    P* ValidDept    - return *on if department is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   emp#       - passed in for validation  (required)
    P*   div        - passed in for validation  (required)
    P*   Depcode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WMDPL
    P*   depname    - returns the name from WMDPL
    P*====================================================================
     PValidDept        b                   export
    P*
     DValidDept        PI             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Depcode                       10
     D cderrn                         9  0             options(*nopass)
     D deptnam                       40                options(*nopass)

     C     depkey        klist
     C                   kfld                    conbr
     C                   kfld                    fund
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    Depcode

     C                   z-add     0             deprrn
     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C     depkey        chain     pdp01                              60
     C                   if        *in60 = *off and DPSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 5
     C                   eval      cderrn = deprrn
     C                   endif
     C                   if        %parms >= 6
     C                   eval      deptnam = DPDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidDept        e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    P*====================================================================
    P* ValidLoc     - Validate Location code   WMALL
    P*
    P* ValidLoc     - return *on if location is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   emp#       - passed in for validation  (required)
    P*   div        - passed in for validation  (required)
    P*   loccode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WMALP
    P*   locname    - returns the name from WMALP
    P*====================================================================
     PValidLoc         b                   export
    P*
     DValidLoc         PI             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Loccode                       10
     D cderrn                         9  0             options(*nopass)
     D locname                       40                options(*nopass)

     C     lockey        klist
     C                   kfld                    conbr
     C                   kfld                    fund
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    loccode

     C                   z-add     0             loctrrn
     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C     lockey        chain     pal01                              60
     C*****              if        *in60 = *off and GTSTAT <> 'A'
     C                   if        *in60 = *off and ALSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 5
     C                   eval      cderrn = loctrrn
     C                   endif
     C                   if        %parms >= 6
     C                   eval      locname = ALNAME
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidLoc         e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidClas    - Validate Class code                  WMEPP
    P*
    P* ValidClas    - return *on if class code is valid
    P*   co#        - passed in for validation  (required)
    P*   fund       - passed in for validation  (required)
    P*   fyr        - passed in for validation  (required)
    P*   emp#       - passed in for validation  (required)
    P*   div        - passed in for validation  (required)
    P*   eppcode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WMEPP
    P*====================================================================
     PValidClas        b                   export
    P*
     DValidClas        PI             1
     D co#                            3  0 const
     D fund                           3  0 const
     D fyr                            3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D Clscode                        5  0 const
     D Spec                           1    const
     D efdate                         7P 0 const
     D cderrn                         9  0 options(*nopass)

     C     clasky        klist
     C                   kfld                    conbr
     C                   kfld                    fund
     C                   kfld                    fyr
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    Clscode

     C     clsky1        klist
     C                   kfld                    conbr
     C                   kfld                    fund
     C                   kfld                    fyr
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    Clscode
     C                   kfld                    Spec
     C                   kfld                    efdate

     C                   z-add     co#           conbr
     C                   move      *off          *in62
     C     clsky1        setgt     pep01
     C     clasky        readpe    pep01                                62
     C                   if        *in62 = *off and EPSTAT <> 'A'
     C                   eval      *in62 = *on
     C                   endif
     C                   if        *in62 = *off
     C                   if        %parms >= 5
     C                   eval      cderrn = epprrn
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidClas        e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*++++++++ WTABL Table Codes ++++++++++++++++++++++++++++++++++++++++++

    P*====================================================================
    P* ValidNatr - Validate nature code
    P*
    P*  ValidNatr - return *on if nature is valid
    P*   co#      - passed in for validation  (required)
    P*   Natcde   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTNAP
    P*   cdedsc   - returns the description from WTNAP
    P*====================================================================
     PValidNatr        b                   export
    P*
     DValidNatr        PI             1
     D co#                            3  0 const
     D Natcde                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   NADESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Natcde        tabKey
     C                   move      *off          *in60
     C     cltbky        chain     pna01                              60
     C                   if        *in60 = *off and NASTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = NADESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidNatr        e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidPart - Validate part code
    P*
    P*  ValidPart - return *on if part is valid
    P*   co#      - passed in for validation  (required)
    P*   Partcde  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTPAP
    P*   cdedsc   - returns the description from WTPAP
    P*====================================================================
     PValidPart        b                   export
    P*
     DValidPart        PI             1
     D co#                            3  0 const
     D Partcde                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   PADESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Partcde       tabKey
     C                   move      *off          *in60
     C     cltbky        chain     ppa01                              60
     C                   if        *in60 = *off and PASTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = PADESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidPart        e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidCause - Validate cause code
    P*
    P* ValidCause - return *on if cause is valid
    P*   co#      - passed in for validation  (required)
    P*   Causecde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTCAP
    P*   cdedsc   - returns the description from WTCAP
    P*====================================================================
     PValidCause       b                   export
    P*
     DValidCause       PI             1
     D co#                            3  0 const
     D Causecde                       2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   CADESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Causecde      tabKey
     C                   move      *off          *in60
     C     cltbky        chain     pca01                              60
     C                   if        *in60 = *off and CASTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = CADESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidCause       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidResType - Validate reserve type code
    P*
    P* ValidResType - return *on if cause is valid
    P*   co#        - passed in for validation  (required)
    P*   Restycde   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTRTP
    P*   cdedsc     - returns the description from WTRTP
    P*====================================================================
     PValidResType     b                   export
    P*
     DValidResType     PI             1
     D co#                            3  0 const
     D Restycde                       2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   RTDESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Restycde      tabKey
     C                   move      *off          *in60
     C     cltbky        chain     prt01                              60
     C                   if        *in60 = *off and RTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = RTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidResType     e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidPmtCode - Validate payment code
    P*
    P* ValidPmtCode - return *on if payment code is valid
    P*   co#        - passed in for validation  (required)
    P*   Pmtcode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTPCP
    P*   cdedsc     - returns the description from WTPCP
    P*====================================================================
     PValidPmtCode     b                   export
    P*
     DValidPmtCode     PI             1
     D co#                            3  0 const
     D Pmtcode                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   PCDESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Pmtcode       tabKey
     C                   move      *off          *in60
     C     cltbky        chain     ppc01                              60
     C                   if        *in60 = *off and PCSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = PCDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidPmtCode     e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidBR2Code - Validate BR2 code (payment value 1)
    P*
    P* ValidBR2Code - return *on if BR2 code is valid
    P*   co#        - passed in for validation  (required)
    P*   BR2code    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTB2P
    P*   cdedsc     - returns the description from WTB2P
    P*====================================================================
     PValidBR2Code     b                   export
    P*
     DValidBR2Code     PI             1
     D co#                            3  0 const
     D BR2code                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   B2DESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  BR2code       tabKey
     C                   move      *off          *in60
     C     cltbky        chain     pb201                              60
     C                   if        *in60 = *off and B2STAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = B2DESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidBR2Code     e

    P*====================================================================
    P* ValidBR7Code - Validate BR7 code (payment value 2)
    P*
    P* ValidBR7Code - return *on if BR7 code is valid
    P*   co#        - passed in for validation  (required)
    P*   BR7code    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTB7P
    P*   cdedsc     - returns the description from WTB7P
    P*====================================================================
     PValidBR7Code     b                   export
    P*
     DValidBR7Code     PI             1
     D co#                            3  0 const
     D BR7code                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cltbky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   B7DESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  BR7code       tabKey
     C                   move      *off          *in60
     C     cltbky        chain     pb701                              60
     C                   if        *in60 = *off and B7STAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = B7DESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidBR7Code     e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    P*====================================================================
    P* ValidCounty  - Validate County Code
    P*
    P* ValidCounty  - return *on if County is valid
    P*   co#        - passed in for validation  (required)
    P*   Cntycde    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTCNP
    P*   cdedsc     - returns the description from WTCNP
    P*====================================================================
     PValidCounty      b                   export
    P*
     DValidCounty      PI             1
     D co#                            3  0 const
     D Cntycde                       10
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     cntyky        klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   CNDESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Cntycde       tabKey
     C                   move      *off          *in60
     C     cntyky        chain     pcn01                              60
     C                   if        *in60 = *off and CNSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = CNDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidCounty      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidTRCode  - Validate Transaction Code
    P*
    P* ValidTRCode  - return *on if TR code is valid
    P*   co#        - passed in for validation  (required)
    P*   Cntycde    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTPCP
    P*   cdedsc     - returns the description from WTPCP
    P*====================================================================
     PValidTRCode      b                   export
    P*
     DValidTRCode      PI             1
     D co#                            3  0 const
     D TRCode                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     trky          klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   PCDESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  TRCode        tabKey
     C                   move      *off          *in60
     C     trky          chain     ppc01                              60
     C                   if        *in60 = *off and PCSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = PCDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidTRCode      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidSPCode  - Validate Specialty Code
    P*
    P* ValidSPCode  - return *on if specialty code is valid
    P*   co#        - passed in for validation  (required)
    P*   Speccde    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTSPP
    P*   cdedsc     - returns the description from WTSPP
    P*====================================================================
     PValidSPCode      b                   export
    P*
     DValidSPCode      PI             1
     D co#                            3  0 const
     D Speccde                        3
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     C     spky          klist
     C                   kfld                    conbr
     C                   kfld                    tabKey

     C                   clear                   SPDESC
     C                   z-add     0             coderrn
     C                   z-add     co#           conbr
     C                   movel(p)  Speccde       tabKey
     C                   move      *off          *in60
     C     spky          chain     psp01                              60
     C                   if        *in60 = *off and SPSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = coderrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = SPDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidSPCode      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*+>>END<< WTABL Table Codes +++END+++END+++END++++END+++++++++++++++++

    P*++++++++ State Table Code +++++++ STABL ++++++++++++++++++++++++++++

    P*====================================================================
    P* ValidState  - Validate state code      Table file STABL
    P*
    P* ValidState - return *on if state code is valid
    P*   Statecd  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of STSTP
    P*   cdename  - returns the state name
    P*====================================================================
     PValidState       b                   export
    P*
     DValidState       PI             1
     D Statecd                        2
     D cderrn                         9  0             options(*nopass)
     D cdename                       40                options(*nopass)

     D Stabky          s                   like(STKEY)

     C                   clear                   STNAME
     C                   movel(p)  Statecd       Stabky
     C                   z-add     0             cderrn
     C                   move      *off          *in60
     C     Stabky        chain     pst01                              60
     C                   if        *in60 = *off and STSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 2
     C                   eval      cderrn = strrn
     C                   endif
     C                   if        %parms >= 3
     C                   eval      cdename = STNAME
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidState       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    P*++++++++ Generic Table Codes ++++ WTGTL ++++++++++++++++++++++++++++

    P*====================================================================
    P* ValidCntry  - Validate country code    Using TBID = 'CTRY'
    P*
    P* ValidCntry - return *on if country code is valid
    P*   co#      - passed in for validation  (required)
    P*   Cntrycde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidCntry       b                   export
    P*
     DValidCntry       PI             1
     D co#                            3  0 const
     D Cntrycde                       3
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Cntrycde      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'CTRY'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidCntry       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidNCCI   - Validate NCCI code    Using TBID = 'xxNC'  (xx=ST)
    P*                                      then TBID = '  NC'  (general)
    P*
    P* ValidNCCI  - return *on if NCCI code is valid
    P*   co#      - passed in for validation  (required)
    P*   NCCIcde  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidNCCI        b                   export
    P*
     DValidNCCI        PI             1
     D co#                            3  0 const
     D NCCIcde                        2
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  NCCIcde       tabKey
     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C                   eval      tabid = '  NC'
     C                   movel     STcde         tabid

     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *on and STcde <> *blank
     C                   move      *off          *in60
     C                   movel     '  '          tabid
     C     gtabky        chain     pgt01                              60
     C                   endif
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 4
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 5
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidNCCI        e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidLossC  - Validate Loss Coverage Using TBID = 'xxLC'  (xx=ST)
    P*                                       then TBID = '  LC'  (general)
    P*
    P* ValidLossC - return *on if Loss coverage is valid
    P*   co#      - passed in for validation  (required)
    P*   Losscde  - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidLossC       b                   export
    P*
     DValidLossC       PI             1
     D co#                            3  0 const
     D Losscde                        2
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Losscde       tabKey
     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C                   eval      tabid = '  LC'
     C                   movel     STcde         tabid

     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *on and STcde <> *blank
     C                   move      *off          *in60
     C                   movel     '  '          tabid
     C     gtabky        chain     pgt01                              60
     C                   endif
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 4
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 5
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidLossC       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidCovT   - Validate Coverage type Using TBID = 'xxCT'  (xx=ST)
    P*                                       then TBID = '  CT'  (general)
    P*
    P* ValidCovT  - return *on if Coverage type is valid
    P*   co#      - passed in for validation  (required)
    P*   Covtyp   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidCovT        b                   export
    P*
     DValidCovT        PI             1
     D co#                            3  0 const
     D Covtyp                         2
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Covtyp        tabKey
     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C                   eval      tabid = '  CT'
     C                   movel     STcde         tabid

     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *on and STcde <> *blank
     C                   move      *off          *in60
     C                   movel     '  '          tabid
     C     gtabky        chain     pgt01                              60
     C                   endif
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 4
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 5
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidCovT        e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidMethC - Validate method of contact  Using TBID = 'MCON'
    P*
    P* ValidMethC - return *on if method of contact is valid
    P*   co#      - passed in for validation  (required)
    P*   Methccde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidMethC       b                   export
    P*
     DValidMethC       PI             1
     D co#                            3  0 const
     D Methccde                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Methccde      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'MCON'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidMethC       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidLostT - Validate lost time flag   Using TBID = 'LTFL'
    P*
    P* ValidLostT - return *on if lost time flag is valid
    P*   co#      - passed in for validation  (required)
    P*   Losttcde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidLostT       b                   export
    P*
     DValidLostT       PI             1
     D co#                            3  0 const
     D Losttcde                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Losttcde      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'LTFL'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidLostT       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidLossR - Validate loss recovery    Using TBID = 'LSRV'
    P*
    P* ValidLossR - return *on if loss recovery is valid
    P*   co#      - passed in for validation  (required)
    P*   Lossrcde - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidLossR       b                   export
    P*
     DValidLossR       PI             1
     D co#                            3  0 const
     D Lossrcde                       2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Lossrcde      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'LSRV'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidLossR       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidJurSt - Validate jurisdiction ST  Using TBID = 'JRST'
    P*
    P* ValidJurSt - return *on if jurisdiction state is valid
    P*   co#      - passed in for validation  (required)
    P*   jurst    - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidJurSt       b                   export
    P*
     DValidJurSt       PI             1
     D co#                            3  0 const
     D jurst                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  jurst         tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'JRST'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidJurSt       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidShift - Validate shift       Using TBID = 'SHFT'
    P*
    P* ValidShift - return *on if shift is valid
    P*   co#      - passed in for validation  (required)
    P*   Shift    - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidShift       b                   export
    P*
     DValidShift       PI             1
     D co#                            3  0 const
     D Shift                          1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Shift         tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'SHFT'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidShift       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidEmpSt - Validate employment status  Using TBID = 'EMST'
    P*
    P* ValidEmpSt - return *on if employment status is valid
    P*   co#      - passed in for validation  (required)
    P*   Empsts   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidEmpSt       b                   export
    P*
     DValidEmpSt       PI             1
     D co#                            3  0 const
     D Empsts                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Empsts        tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'EMST'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidEmpSt       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidActSt - Validate activity status  Using TBID = 'ACST'
    P*
    P* ValidActSt - return *on if activity status is valid
    P*   co#      - passed in for validation  (required)
    P*   Actsts   - passed in for validation  (required)
    P*   cderrn   - returns the RRN of WTGTL
    P*   cdedsc   - returns the description from WTGTL
    P*====================================================================
     PValidActSt       b                   export
    P*
     DValidActSt       PI             1
     D co#                            3  0 const
     D Actsts                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Actsts        tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'ACST'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidActSt       e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidMCOtyp - Validate MCO Type  Using TBID = 'MCO '
    P*
    P* ValidMCOtyp - return *on if MCO type is valid
    P*   co#       - passed in for validation  (required)
    P*   Mctype    - passed in for validation  (required)
    P*   cderrn    - returns the RRN of WTGTL
    P*   cdedsc    - returns the description from WTGTL
    P*====================================================================
     PValidMCOtyp      b                   export
    P*
     DValidMCOtyp      PI             1
     D co#                            3  0 const
     D Mctype                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Mctype        tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'MCO '
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidMCOtyp      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidInitTr - Validate Initial treatment  Using TBID = 'INIT'
    P*
    P* ValidInitTr - return *on if Init Treatment code is valid
    P*   co#       - passed in for validation  (required)
    P*   Initrt    - passed in for validation  (required)
    P*   cderrn    - returns the RRN of WTGTL
    P*   cdedsc    - returns the description from WTGTL
    P*====================================================================
     PValidInitTr      b                   export
    P*
     DValidInitTr      PI             1
     D co#                            3  0 const
     D Initrt                         2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Initrt        tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'INIT'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidInitTr      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidAgrComp - Validate Agree to compensate  Using TBID = 'AGCP'
    P*                             field is 'AGCD'
    P* ValidAgrComp - return *on if Agree  to comp code is valid
    P*   co#        - passed in for validation  (required)
    P*   Agreecmp   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidAgrComp     b                   export
    P*
     DValidAgrComp     PI             1
     D co#                            3  0 const
     D Agreecmp                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Agreecmp      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'AGCP'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidAgrComp     e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidMethPIW - Validate Meth/determn pre-Injury wage  TBID = 'WMTH'
    P*
    P* ValidMethPIW - return *on if Meth/Det PIW is valid
    P*   co#        - passed in for validation  (required)
    P*   Mdetpiw    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidMethPIW     b                   export
    P*
     DValidMethPIW     PI             1
     D co#                            3  0 const
     D Mdetpiw                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Mdetpiw       tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'WMTH'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidMethPIW     e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidFWageP  - Validate FROI Wage period              TBID = 'FRWP'
    P*
    P* ValidFWageP  - return *on if FROI Wage Period is valid
    P*   co#        - passed in for validation  (required)
    P*   Fwageper   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidFWageP      b                   export
    P*
     DValidFWageP      PI             1
     D co#                            3  0 const
     D Fwageper                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Fwageper      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'FRWP'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidFWageP      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidRTWQual - Validate RTW qualifier              TBID = 'RTWQ'
    P*
    P* ValidRTWQual - return *on if Return to work qualifier is valid
    P*   co#        - passed in for validation  (required)
    P*   Retwqual   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidRTWQual     b                   export
    P*
     DValidRTWQual     PI             1
     D co#                            3  0 const
     D Retwqual                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Retwqual      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'RTWQ'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidRTWQual     e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidFraudI  - Validate Fraudulent indicator       TBID = 'FRAU'
    P*                             (WMLLP)
    P* ValidFraudI  - return *on if Fraud indicator is valid
    P*   co#        - passed in for validation  (required)
    P*   Fraudin    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidFraudI      b                   export
    P*
     DValidFraudI      PI             1
     D co#                            3  0 const
     D Fraudin                        2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Fraudin       tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'FRAU'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidFraudI      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P*ValidMethSettle Validate Meth/settle  Using TBID = 'xxMS'  (xx=ST)
    P*                  (WMLLP)              then TBID = '  MS'  (general)
    P*
    P* ValidMethSettle - return *on if Method of settlement is valid
    P*   co#           - passed in for validation  (required)
    P*   Methset       - passed in for validation  (required)
    P*   cderrn        - returns the RRN of WTGTL
    P*   cdedsc        - returns the description from WTGTL
    P*====================================================================
     PValidMethSettle  b                   export
    P*
     DValidMethSettle  PI             1
     D co#                            3  0 const
     D Methset                        3
     D STcde                          2
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Methset       tabKey
     C                   z-add     co#           conbr
     C                   move      *off          *in60
     C                   eval      tabid = '  MS'
     C                   movel     STcde         tabid

     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *on and STcde <> *blank
     C                   move      *off          *in60
     C                   movel     '  '          tabid
     C     gtabky        chain     pgt01                              60
     C                   endif
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 4
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 5
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidMethSettle  e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidGender  - Validate Gender code                TBID = 'GNDR'
    P*                             (WMCDP)
    P* ValidGender  - return *on if Gender code is valid
    P*   co#        - passed in for validation  (required)
    P*   GendCode   - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidGender      b                   export
    P*
     DValidGender      PI             1
     D co#                            3  0 const
     D GendCode                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  GendCode      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'GNDR'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidGender      e
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    P*====================================================================
    P* ValidMariSt  - Validate Marital status          TBID = 'MSTS'
    P*                             (WMCDP)
    P* ValidGMariSt - return *on if marital status is valid
    P*   co#        - passed in for validation  (required)
    P*   Marists    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidMariSt      b                   export
    P*
     DValidMariSt      PI             1
     D co#                            3  0 const
     D Marists                        1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Marists       tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'MSTS'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidMariSt      e


    P*====================================================================
    P* ValidSICCde  - Validate SIC code                TBID = 'SIC '
    P*
    P* ValidSICCde  - return *on if SIC code is valid
    P*   co#        - passed in for validation  (required)
    P*   Siccode    - passed in for validation  (required)
    P*   cderrn     - returns the RRN of WTGTL
    P*   cdedsc     - returns the description from WTGTL
    P*====================================================================
     PValidSICCde      b                   export
    P*
     DValidSICCde      PI             1
     D co#                            3  0 const
     D Siccde                         6
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Siccde        tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'SIC '
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidSICCde      e

    P*====================================================================
    P* ValidPayeeTyp - Validate Payee type                TBID = 'PVTP'
    P*
    P* ValidPayeeTyp - return *on if Payee type is valid
    P*   co#         - passed in for validation  (required)
    P*   Pyeetyp     - passed in for validation  (required)
    P*   cderrn      - returns the RRN of WTGTL
    P*   cdedsc      - returns the description from WTGTL
    P*====================================================================
     PValidPayeeTyp    b                   export
    P*
     DValidPayeeTyp    PI             1
     D co#                            3  0 const
     D Pyeetype                       1
     D cderrn                         9  0             options(*nopass)
     D cdedsc                        30                options(*nopass)

     D Tabid           s                   like(GTTBID)

     C     gtabky        klist
     C                   kfld                    conbr
     C                   kfld                    tabid
     C                   kfld                    tabKey

     C                   clear                   gtdesc
     C                   z-add     0             codgrrn
     C                   movel(p)  Pyeetype      tabKey
     C                   z-add     co#           conbr
     C                   eval      tabid = 'PVTP'
     C                   move      *off          *in60
     C     gtabky        chain     pgt01                              60
     C                   if        *in60 = *off and GTSTAT <> 'A'
     C                   eval      *in60 = *on
     C                   endif
     C                   if        *in60 = *off
     C                   if        %parms >= 3
     C                   eval      cderrn = codgrrn
     C                   endif
     C                   if        %parms >= 4
     C                   eval      cdedsc = GTDESC
     C                   endif
     C                   endif

     C                   return    not *in60

     PValidPayeeTyp    e

    P*+>>END<< WTGTL Table Codes +++END+++GENERIC  +++TABLE +++++++++++++++
    P*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
