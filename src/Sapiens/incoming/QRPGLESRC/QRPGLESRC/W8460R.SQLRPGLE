     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE W8460R - Claim Export Program
    H*****************************************************************
    H*  Program Description:                                         *
    H*     Creates XML Claim File Layout                             *
    H*                                                               *
    H*  Called from: W8460C - Claim Export - Override                *
    H*  Calls to   : None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 06/16/2006 by c:62 for Project 139MCV             *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  03/19/07 c:62 project 139MHG                                 *
    H*                Add denial and controverted flag               *
    H*                                                               *
    H*****************************************************************
     Fwmfnl     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwdell     if   e           k disk
     Fwmchl     if   e           k disk
     Fwzxcp     o    e             disk
     d*
     d getDate         pr                  extpgm('W8460R1')
     d   lastRunDate                  1a
     d pgmend          pr
     d pgmstr          pr
     d writeh          pr
     d writed          pr
     d writef          pr
     d getClaimCnt     pr             9s 0
     d getDateLastRan  pr             7s 0
     d stripBadChars   pr           100a   varying
     D   text                       100a   varying
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
    
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D                 ds
     D data                          16    dtaara(xcdata)
     D  xxdate                       10    overlay(data)
     D  xxseq#                        6    overlay(data:11)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@dcid          s             12a
     D @@time          s              6a
     D ccdate          s              6s 0
     D dddate          s              6a

     D date_mdyy       s               d   inz datfmt(*usa)
     D date_yymd       s               d   inz datfmt(*iso)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D kkefdt          s                   like(chefdt)
     d date            s              7s 0 inz(0)
    C*---------------------------------------------------------------------
    C* Parameter List
    C*---------------------------------------------------------------------
     C     *entry        plist
     C                   parm                    @@dcid
     C                   parm                    @@time
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtyym2        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*---------------------------------------------------------------------
    C* Key Lists
    C*---------------------------------------------------------------------
     C*
     C     chky1         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    kkefdt
     C*
     C     chky2         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C     cmky          klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C*
     C     elky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     C*
     C     emky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
    C*---------------------------------------------------------------------
    C*  Main Processing
    C*---------------------------------------------------------------------
     C                   callp     pgmstr
    C*
    C* Write the header information
    C*
     C                   callp     writeh
    C*
    C* Reading thru WMCMP and write detail
    C*
     C                   callp     writed
    C*
    C* Write the footer or part that completes the file
    C*
     C                   callp     writef
    C*
    C* End of Processing
    C*
     C                   callp     pgmend
     c*=====================================================================---*
     c* getDateLastRan - get the last date ran
     c*=====================================================================---*
     p getDateLastRan  B
     d getDateLastRan  PI             7s 0

     d bbDate          s              7s 0 inz(0)
     D pldate          s              7a   inz(' ')

     c                   callp     getDate(pldate)
     C                   move      pldate        bbdate

     c                   return    bbDate

     p getDateLastRan  E
     c*=====================================================================---*
     c* getClaimCnt - gets the number of claim records
     c*=====================================================================---*
     p getClaimCnt     B
     d getClaimCnt     PI             9s 0

     d pcount          s              9s 0 inz(0)

     c*
     c/EXEC SQL
     c+ DECLARE PROJECT CURSOR FOR
     c+ SELECT COUNT(*)
     c+ FROM WMCMP, WMFNP
     c+ WHERE CMDT > :DATE AND CMCO# = FNCO# AND CMFND = FNFND AND
     c+                        FNTEST <> 'Y'
     c/END-EXEC
     c*
     c/EXEC SQL
     c+ OPEN PROJECT
     c/END-EXEC
     c*
     c                   if        sqlstt = '00000'
     c*
     c/EXEC SQL
     c+ FETCH NEXT
     c+ FROM PROJECT
     c+ INTO  :PCOUNT
     c/END-EXEC
     c*
     c                   endif

     c                   return    pcount

     p getClaimCnt     E
    C*=====================================================================
    C* pgmend - Program End
    C*=====================================================================
     p     pgmend      b
     d     pgmend      pr
    C*
     C                   eval      @@dcid = *blanks

     C                   move      dsdate        date_mdyy
     C                   if        dstime < 120000
     C                   subdur    1:*days       date_mdyy
     C                   endif
     C*
     C                   move      date_mdyy     xxdate
     C     *lock         out       data
     C*
     C     *ymd          move      date_mdyy     ccdate
     C                   move      ccdate        dddate
     C                   eval      @@dcid = 'XC' + dddate
     C                   move      dstime        @@time
    C*
     C                   eval      *inlr = *on
    C*
     p pgmend          e
    C*=====================================================================
    C* pgmstr - Set-up and Initialize
    C*=====================================================================
     P     pgmstr      b
     d     pgmstr      pr
    C*

     c                   eval      date = getDateLastRan

     C                   time                    dsdttm
     c     *lock         in        data
    C*
     p pgmstr          e
     c*=====================================================================---*
     c* stripBadChars - remove bad characters
     c*=====================================================================---*
     p stripBadChars   B
     d stripBadChars   PI           100a   varying
     d   text                       100a   varying

     d i               s              3s 0
     d text2           s            100a   varying

     c                   eval      text2 = text

     c*
     c* Replace & with amp; We first need to replace with ?amp; then change
     c* the ? back to an &
     c*

     c                   eval      i = %scan('&':text2:1)
     c                   dow       i <> 0
     c                   if        i < 26
     c                   eval      text2 = %replace('~amp;':text2:i:1)
     c                   else
     c                   eval      text2 = %replace(' ':text2:i:1)
     c                   endif
     c                   eval      i = %scan('&':text2:1)
     c                   enddo

     c                   eval      i = %scan('~':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace('&':text2:i:1)
     c                   eval      i = %scan('~':text2:1)
     c                   enddo

     c*
     c* Replace " with blanks
     c*

     c                   eval      i = %scan('"':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace('':text2:i:1)
     c                   eval      i = %scan('"':text2:1)
     c                   enddo

     c*
     c* Replace ¢ with blanks
     c*

     c                   eval      i = %scan('¢':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace('':text2:i:1)
     c                   eval      i = %scan('¢':text2:1)
     c                   enddo

     c                   return    %trim(text2)

     p stripBadChars   E
    C*=====================================================================
    C* writed - Write detail information
    C*=====================================================================
     p     writed      B
     d     writed      pi

     D* Detail
     D lastName        s             18a
     D firstName       s             12a
     D middleInit      s              1a
     D address1        s             30a
     D address2        s             30a
     D city            s             20a
     D sex             s              1a
     D dtob            s              8a
     D patientID       s             13a
     D openDate        s              8a
     D status          s              1a
     D doi             s              8a
     D lostTimeFlag    s              1a
     D fatality        s              1a
     D acl#            s             20a
     D policyNumber    s             20a
     D pfdt            s              8a
     D ptdt            s              8a
     D closeDate       s              8a
     D empName1        s             30a
     D empName2        s             30a
     D empAddress1     s             30a
     D empAddress2     s             30a
     D empCity         s             20a
     D actm            s              4a
     D msts            s              1a
     D deny            s              1a
     D controverted    s              1a

     D* Work Fields
     D input           s            100a   varying

    C*
     C     *loval        setll     pfn01
     C                   read      pfn01
     c                   dow       not %eof(wmfnl)
     C                   if        fntest <> 'Y'
     C     cmky          setll     pcm01
     C     cmky          reade     pcm01
     C                   dow       not %eof(wmcml1)
     C*
     C* Get Demographic Information
     C*
     C                   clear                   pcd01
     C     cmss#         chain     pcd01
     C*
     C* Get Employer Information
     C*
     C                   clear                   pem01
     C     emky          chain     pem01
     C                   clear                   pel01
     C     elky          chain     pel01
     C*
     C* Populate fields
     C*
     C                   eval      input = cmlsn
     C                   eval      lastName = stripBadChars(input)
     C                   eval      input = cmfsn
     C                   eval      firstName = stripBadChars(input)
     C                   eval      input = cdadr1
     C                   eval      address1 = stripBadChars(input)
     C                   eval      input = cdadr2
     C                   eval      address2 = stripBadChars(input)
     C                   eval      input = cdcty
     C                   eval      city = stripBadChars(input)
     C*
     C                   if        cdsex = 'F'
     C                   eval      sex = 'F'
     C                   else
     C                   eval      sex = 'M'
     C                   endif
     C*
     C                   if        cdmsts <> 'M' and cdmsts <> 'S'
     C                   eval      msts = 'N'
     C                   else
     C                   eval      msts = cdmsts
     C                   endif
    C*
     C                   if        cddtob = 0
     c                   eval      dtob = '19400102'
     c                   else
     C                   z-add     cddtob        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         dtob
     C                   endif
     C*
     C                   eval      patientID = %trim(%editw(cmfnd:'0   ')) +
     C                                  %trim(%editw(cmfyr:'0   ')) +
     C                                  %trim(%editw(cmcase:'0       '))
     C*
     C                   z-add     cmidt         srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         openDate
     C*
     C                   if        cmstat = 'C'
     C                   eval      status = 'C'
     C                   else
     C                   eval      status = 'O'
     C                   endif
     C*
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         doi
     C*
     C                   if        cmltfl = 'I'
     C                   eval      lostTimeFlag = 'L'
     C                   else
     C                   eval      lostTimeFlag = 'N'
     C                   endif
     C*
     C                   if        cmdod <> 0
     C                   eval      fatality = 'Y'
     C                   else
     C                   eval      fatality = 'N'
     C                   endif
     C*
     C                   eval      input = cmacl#
     C                   eval      acl# = stripBadChars(input)
     C*
     C                   eval      policyNumber = %trim(%editw(cmfnd:'0   ')) +
     C                                  %trim(%editw(cmemp#:'0         ')) +
     C                                  %trim(%editw(cmdiv:'0     ')) +
     C                                  %trim(%editw(cmfyr:'0   '))
     C*
     C                   z-add     elpfdt        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         pfdt
     C*
     C                   z-add     elptdt        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         ptdt
     C*
     C                   if        cmstat = 'C'
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     chky1         setgt     pch01
     C     chky2         readpe    pch01
     C*
     C                   z-add     chefdt        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         closeDate
     C*
     C                   else
     C                   clear                   closeDate
     C                   endif
     C*
     C                   eval      input = emname
     C                   eval      empName1 = stripBadChars(input)
     C                   eval      input = emnam2
     C                   eval      empName2 = stripBadChars(input)
     C                   eval      input = emmad1
     C                   eval      empAddress1 = stripBadChars(input)
     C                   eval      input = emmad2
     C                   eval      empAddress2 = stripBadChars(input)
     C                   eval      input = emmcty
     C                   eval      empCity = stripBadChars(input)
     C*
     C                   movel     cmactm        actm
     c*** begin add *** 03/19/07 *******************************************
     c                   if        cmdny = 'Y'
     c                   eval      deny = 'Y'
     c                   else
     c                   eval      deny = 'N'
     c                   endif
     c*
     c                   if        cmcvfl = 'Y'
     c                   eval      controverted = 'Y'
     c                   else
     c                   eval      controverted = 'N'
     c                   endif
     c*** end   add *** 03/19/07 *******************************************
     C*
     C* Write Detail
     C*
     C                   eval      xcdata = '<ClaimPatientCore> '   +
     C                                '<InjuredPartyDemographics ' +
     C                                'patientID="' + CMSS# +
     C                                '" patientLastName="' + %trim(lastName) +
     C                               '" patientFirstName="' + %trim(firstName) +
     C                                '" patientMiddleInitial="' + cmmid +
     C                                '" address1="' + %trim(address1) +
     C                                '" address2="' + %trim(address2) +
     C                                '" city="' + %trim(city) +
     C                                '" state="' + %trim(cdst) +
     C                                '" zipCode="' +
     C                                      %trim(%editw(cdzip:'0         '))+
     C                                '" countryCode="US"' +
     C                                ' gender="' + sex +
     C                                '" dateOfBirth="' + dtob +
     C                             '" externalPatientID="' + %trim(patientID) +
     C                                '" maritalStatus="' + msts + '" /> '   +
     C                                ' <Claim claimOpenDate="' + openDate +
     C                                '" claimCarrier="GIC000000"' +
     C                                ' statusCode="' + status +
     C                                '" claimNumber="' + %trim(patientID) +
     C                                '" dateOfInjury="' + doi +
     C                                '" jurisdiction="' + cmjrst +
     C                                '" info1="" info2=""' +
     C                                ' wageLostCode="' + lostTimeFlag +
     C                                '" daysLost=""' +
     C                                ' fatalityIndicator="' + fatality +
     C                                '" agencyClaimNumber=""' +
     C                                ' stateReportingID="' + %trim(acl#) +
     C                                '" policyNumber="' + %trim(policyNumber) +
     C                                '" policyEffectiveDate="' + pfdt +
     C                                '" policyCancelDate="' + ptdt +
     C                                '" closeDate="' + %trim(closeDate) +
     C                                '" payDirectCode=""' +
     C                                ' serviceBranchOffice="' + %trim(emoff#) +
     C                                '" externalClaimID="' + %trim(patientID) +
     C                                '" insuredName1="' + %trim(empName1) +
     C                                '" insuredName2="' + %trim(empName2) +
     C                             '" insuredAddress1="' + %trim(empAddress1) +
     C                             '" insuredAddress2="' + %trim(empAddress2) +
     C                                '" insuredCity="' + %trim(empCity) +
     C                                '" insuredState="' + %trim(emmst) +
     C                                '" insuredZipCode="' +
     C                                      %trim(%editw(emmzip:'0         '))+
     C                                '" insuredCountryCode="US"' +
     C                                ' externalEmployerID="' + %char(ememp#) +
     C                                '" caseOwnerID="" clientType=""' +
     C                                ' timeOfLoss="' + actm +
     C                                '" managedCareReviewed=""' +
     C                                ' insuredTaxID="' +
     C                                      %trim(%editw(emfein:'0         '))+
     C                                '" statisticalLocationCode="' +
     C                                      %trim(%editw(emdiv:'0     '))+
     C                                '" clientProgramNumber=""' +
     C                                ' diagnosisCode=""' +
     c*** begin add *** 03/19/07 *******************************************
     c                                ' claimDenied="' + deny + '" ' +
     c                                'controverted="' + controverted + '"' +
     c*** end   add *** 03/19/07 *******************************************
     C                                '/> </ClaimPatientCore>'
     C                   write     pxc01
     C*
     C     cmky          reade     pcm01
     C                   enddo
     C                   endif
     C                   read      pfn01
     C                   enddo
    C*
     p writed          E
    C*=====================================================================
    C* writef - Write footer information
    C*=====================================================================
     p     writef      B
     d     writef      pi
     C*
     C* Write footer information
    C*
     C                   eval      xcdata = '</ClaimPatientTransmission>'
    C*
     C                   write     pxc01
    C*
     p writef          E
    C*=====================================================================
    C* writeh - Write header information
    C*=====================================================================
     p     writeh      B
     d     writeh      pi

     D* Header
     D trdt            s              8a
     D trid            s              9a
     D numV            s              5a
     D seq#            s              6s 0
     D count           s              5s 0

     C*
     C* Populate variables
    C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         trdt
     C*
     C                   move      xxseq#        seq#
     C                   eval      seq# = seq# + 1
     C                   move      seq#          xxseq#
     C                   eval      trid = 'GIC' + xxseq#
    C*
     C                   eval      count = getClaimCnt
     C                   move      count         numV
     C*
     C* Write Header Information
    C*
     C                   eval      xcdata = '<?xml version= "1.0"?> ' +
     C                                '<ClaimPatientTransmission ' +
     C                                'transmissionDate= "' + trdt +
     C                                '" transmissionID= "' + trid +
     C                                '" numberOfClaims= "' + numV +
     C                                '" version="V1.1">'
     C*
     C                   write     pxc01
    C*
     p writeh          E
