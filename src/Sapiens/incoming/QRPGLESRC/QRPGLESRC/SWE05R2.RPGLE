     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SWE05R - WC Employer Info for Web Display
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a Employer info to display on web page.                    .  *
      *
     * CALLED BY : CGI programs for Web                                       *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   :                                                            *
     * PROGRAMMER: C:02                                                       *
     * PROJECT   : DR176                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
      *
     * 08/14/02  C:145  PROJECT: XXXXX                              *      XX *
     *                  Removed Unused Array, Table, Data Structure *         *
      *                   and stand-alone field definitions          *
      * 08/14/02  C:145  PROJECT: XXXXX                              *
      *                  Moved Policy Name, Policy holder, agent     *
      *                  and producer fields down 1 column           *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * Indicator Usage                                                        *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *  61 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
      ** Workers' Comp Employer Master File
     FWMEML     IF   E           K DISK
      ** Company Master File
     FSMCOL     IF   E           K DISK
      ** Group Master File
     FWMFNL     IF   E           K DISK
     *========================================================================*
     * Constant Definitions
     *========================================================================*
     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
      *
     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D I1              S              3  0
     D SavRtn          S              1a
     D err             S              1a
     D zzCXDT          S              8  0
     D  ##Fyr          S              3  0
     D  ##AGT#         S              5  0
     D  ##ANAM         S             60
     D  ##PROD         S              5  0
     D  ##PNAM         S             60
     D   Dateres       S               D   DATFMT(*USA)


     D*xxName          S                   Like(EMNAME)
     D                 DS
     D xxKey                         40a
     D xxCompany                      3s 0 Overlay(xxKey:1)
     D xxGroup                        3s 0 Overlay(xxKey:4)
     D xxEmployer                     9s 0 Overlay(xxKey:7)
     D xxDivision                     5s 0 Overlay(xxKey:16)
     D                 DS
     D TSKey                         20a
     D TSCompany                      3a   Overlay(TSKey:1)
     D TSGroup                        3a   Overlay(tSKey:4)
     D TSEmployer                     9a   Overlay(TSKey:7)
     D TSDivision                     5a   Overlay(tSKey:16)
     D******
     D                 DS
     D xxpol#                        40a
     D xpCompany                      3a   Overlay(xxPol#:1)
     D xphyp1                         1a   inz('-')overlay(xxpol#:4)
     D xpGroup                        3a   Overlay(xxpol#:5)
     D xphyp2                         1a   inz('-')overlay(xxpol#:8)
     D xpEmployer                     9a   Overlay(xxPol#:9)
     D xphyp3                         1a   inz('-')overlay(xxpol#:18)
     D xpDivision                     5a   Overlay(xxPol#:19)
      *
     D                 DS
     D EEF                     1     35  0 Dim(5)
     D Emefd1                  1      7  0
     D Emefd2                  8     14  0
     D Emefd3                 15     21  0
     D Emefd4                 22     28  0
     D Emefd5                 29     35  0
      *
     D                 DS
     D ECA                     1     35  0 Dim(5)
     D Emcad1                  1      7  0
     D Emcad2                  8     14  0
     D Emcad3                 15     21  0
     D Emcad4                 22     28  0
     D Emcad5                 29     35  0
      *
     D******
     DiDrtKey          s             40a
     DiPnam            s             10a
     DvHead1           s             40a
     DvHead2           s             40a
     DvHead3a          s             40a
     DvHead3b          s             40a
     DvDHd01           s             40a
     DvDl1c1           s             40a
     DvDl1c2           s             40a
     DvDl1c3           s             40a
     DvDl1c4           s             40a
     DvDl2c1           s             40a
     DvDl2c2           s             40a
     DvDl2c3           s             40a
     DvDl2c4           s             40a
     DvDl3c1           s             40a
     DvDl3c2           s             40a
     DvDl3c3           s             40a
     DvDl3c4           s             40a
     DvDl4c1           s             40a
     DvDl4c2           s             40a
     DvDl4c3           s             40a
     DvDl4c4           s             40a
     DvDl5c1           s             40a
     DvDl5c2           s             40a
     DvDl5c3           s             40a
     DvDl5c4           s             40a
     DvDl6c1           s             40a
     DvDl6c2           s             40a
     DvDl6c3           s             40a
     DvDl6c4           s             40a
     DvDl7c1           s             40a
     DvDl7c2           s             40a
     DvDl8c1           s             40a
     DvDl8c2           s             40a
     DvDl8c3           s             40a
     DvDl8c4           s             40a
     DvDl9c1           s             40a
     DvDl9c2           s             40a
     DvDl9c3           s             40a
     DvDl9c4           s             40a
     DvRtcd            s              2a
     D******
     Dxxdt8            s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D******
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    emco#
     C                   PARM                    emfnd
     C                   PARM                    ememp#
     C                   PARM                    emdiv
     C                   PARM                    ##Fyr
     C                   PARM                    ##AGT#
     C                   PARM                    ##ANAM
     C                   PARM                    ##PROD
     C                   PARM                    ##PNAM
     C*****
     C*
     C     *entry        PList
     C                   parm                    idrtkey
     C                   parm                    ipnam
     C                   parm                    vHead1
     C                   parm                    vHead2
     C                   parm                    vHead3a
     C                   parm                    vHead3b
     C                   parm                    vDHd01
     C                   parm                    vDl1c1
     C                   parm                    vDl1c2
     C                   parm                    vDl1c3
     C                   parm                    vDl1c4
     C                   parm                    vDl2c1
     C                   parm                    vDl2c2
     C                   parm                    vDl2c3
     C                   parm                    vDl2c4
     C                   parm                    vDl3c1
     C                   parm                    vDl3c2
     C                   parm                    vDl3c3
     C                   parm                    vDl3c4
     C                   parm                    vDl4c1
     C                   parm                    vDl4c2
     C                   parm                    vDl4c3
     C                   parm                    vDl4c4
     C                   parm                    vDl5c1
     C                   parm                    vDl5c2
     C                   parm                    vDl5c3
     C                   parm                    vDl5c4
     C                   parm                    vDl6c1
     C                   parm                    vDl6c2
     C                   parm                    vDl6c3
     C                   parm                    vDl6c4
     C                   parm                    vDl7c1
     C                   parm                    vDl7c2
     C                   parm                    vDl8c1
     C                   parm                    vDl8c2
     C                   parm                    vDl8c3
     C                   parm                    vDl8c4
     C                   parm                    vDl9c1
     C                   parm                    vDl9c2
     C                   parm                    vDl9c3
     C                   parm                    vDl9c4
     C                   parm                    vRtcd
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     EMkey         KList
     C                   KFld                    xxcompany
     C                   KFld                    xxGroup
     C                   KFld                    xxEmployer
     C                   KFld                    xxDivision
     C     COkey         KList
     C                   KFld                    xxcompany
     C     FNkey         KList
     C                   KFld                    xxcompany
     C                   KFld                    xxGroup
     *========================================================================*
     * Main
     *========================================================================*

     C                   TIME                    Dateres

     C     EMkey         Chain     PEM01                              60
     C                   if        not %eof
     C                   move      xxcompany     xpcompany
     C                   move      xxgroup       xpgroup
     c                   move      xxemployer    xpemployer
     c                   move      xxdivision    xpdivision
     C*
     C     JustGo        Tag
      ** HIT, Fill 5
     C                   exsr      Fill5
      ** We have filled up 5 items.  Time to return...


     C                   else
     C**** error Employer not found
     C                   endif


     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      SavRtn    = vrtcd
     C                   Eval      vrtcd    = *blanks

      *** test incoming key value to verify it is numeric

     C                   Eval      tskey  = iDrtKey
     C*
     c                   Testn                   tscompany            66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tscompany = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '1'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '1'
     c                   goto      doneit
     c                   else
     c                   endif

     C*
     c                   Testn                   tsGroup              66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsGroup   = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '2'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '2'
     c                   goto      doneit
     c                   else
     c                   endif

     c                   Testn                   tsEmployer           66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsEmployer = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '3'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '3'
     c                   goto      doneit
     c                   else
     c                   endif


     c                   Testn                   tsDivision           66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsDivision = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '4'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '4'
     c                   goto      doneit
     c                   else
     c                   endif


     c     doneit        tag
     c                   if        err <> ' '
     c                   eval      vRtcd = err
     c                   Select
     c                   when      err = '1'
     c                   eval      tskey = *zeros
     C                   movel     tskey         xxkey
     c                   when      err = '2'
     c                   eval      tsgroup   = *zeros
     c                   eval      tsemployer = *zeros
     c                   eval      tsdivision = *zeros
     c                   when      err = '3'
     c                   eval      tsemployer = *zeros
     c                   eval      tsdivision = *zeros
     c                   when      err = '4'
     c                   eval      tsdivision = *zeros
     c                   endsl

     c                   movel     tskey         xxkey
     c*
     c                   else
     c                   eval      xxkey = tskey
     c                   endif



     C                   ENDSR                                                  *inzsr
     *========================================================================*
     * Fill5  - Load data to return to calling CGI
     *========================================================================*
     C     Fill5         BEGSR

     C     COkey         Chain     PCO01
     C                   if        not %eof
     C                   MOVEL     CONAME        vHead1
     C                   else
     C                   MOVEL     'Not Found'   vHead1
     C                   endif
     C*
     C     FNkey         chain     PFN01
     C                   if        not %eof
     C                   MOVEL     FNNAME        vHead2
     C                   else
     C                   MOVEL     'Not Found'   vHead2
     C                   endif
     C*
     C**** vhead3a is Current Date
     C*
     C                   eval      vHead3a = ('Current Date: ' +
     C                             %CHAR(dateres))
     C*
     C*** VHead3b is Cancel Date
     C                   exsr      DtCnld
     C                   if        (zzCXDT <> *zeros)
     C*** cancel date to show
     **** MUST DO A COLOR CHANGE HERE TOO ?RED? BOLD?  does it fit?
     C                   eval      vHead3b = ('Cancel Date: ' +
     C                             %editW(zzcxdt:'  /  /    '))
     C*
     C                   else
     C***  No Cancel Date
     C                   eval      vHead3b = *blanks
     C                   endif
     C*
     C                   eval      vDHd01 = 'POLICY INFORMATION'
     C*
     C                   eval      vDl2c1 = ('Policy Holder:')
     C*
     C                   eval      vDl2c2 = EMNAME
     C*
     C                   eval      vDl2c3 = ('Policy #:')
     C*
     C                   eval      vDl2c4 = xxpol#
     C*
     C*** get agent / Producer Info
      *
     C                   Z-ADD     *ZEROS        ##FYR
     C                   CALL      'WA002R'      AGTPL
      *
     C                   eval      vDl4c1 = 'Agent:'
     C                   eval      vDl4c3 = 'Producer:'
     C                   if        (##anam <> *blanks)
     C                   eval      vDl4c2 = ##ANAM
     C                   endif
     C                   if        (##PNAM <> *blanks)
     C                   eval      vDl4c4 = ##PNAM
     C                   endif
     C
     C                   eval      vDl6c1 = 'Fed Tax ID:'
     C                   eval      vDl6c2 = (%Editw(EMFEIN:'   -  -    '))
     C*
     C*** last inv. Date
     C*
     C                   eval      vDl6c3 = 'Last Inv. Date:'
     C                   eval      srdti = emlidt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdt8
     C****               if        xxdt8 <> *zeros
     C                   eval      vDl6c4 = (%editc(xxdt8: 'Y'))
     C****               endif
     C
     C
     C*
     C     donefill5     tag

     C                   ENDSR                                                  *inzsr
     C*
     *========================================================================*
     * DtCnld - Determine date canceled (if canceled)
     *========================================================================*
     C     Dtcnld        Begsr
     C                   Z-ADD     0             zzCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         Lookup    EEF(I1)                                61
      *
     C                   if        *in61
     C                   SUB       1             I1
     C                   else
     C                   Z-ADD     5             I1
     C                   endif
      *
     C     I1            IFNE      0
      *
     C     ECA(I1)       IFNE      *ZERO
     C                   Z-ADD     ECA(I1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         zzCXDT
     C                   ENDIF
      *
     C                   ENDIF
     c                   endsr
