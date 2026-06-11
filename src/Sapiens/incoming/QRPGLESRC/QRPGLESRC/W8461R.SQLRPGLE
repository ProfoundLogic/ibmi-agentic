     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE W8461R - Provider Export Program
    H*****************************************************************
    H*  Program Description:                                         *
    H*     Creates XML Provider File Layout                          *
    H*                                                               *
    H*  Called from: W8461C - Provider Export - Override             *
    H*  Calls to   : None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 06/13/2006 by c:62 for Project 139MCV             *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H* 11/02/09 C:232 PROJECT: 139WMR                                *
    H*                OFAC Reporting-Add Pending Status Indicator.   *
    H*                                                               *
    H*****************************************************************
     Fwmpdl     if   e           k disk
     Fwzxpp     o    e             disk
     d*
     d getDate         pr                  extpgm('W8461R1')
     d   lastRunDate                  1a
     d pgmend          pr
     d pgmstr          pr
     d writeh          pr
     d writed          pr
     d writef          pr
     d getProvCnt      pr             9s 0
     d getDateLastRan  pr             7s 0
     d stripBadChars   pr           100a   varying
     D   text                       100a   varying
     D   length                       3s 0 const
     d toUpperCase     pr           100a   varying
     D   text                       100a   varying value
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
     D data                          16    dtaara(xpdata)
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
     d date            s              7s 0 inz(0)
    C*---------------------------------------------------------------------
    C* Parameter List
    C*---------------------------------------------------------------------
     C     *entry        plist
     C                   parm                    @@dcid
     C                   parm                    @@time
     C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*---------------------------------------------------------------------
    C* Key Lists
    C*---------------------------------------------------------------------
    C*---------------------------------------------------------------------
    C*  Main Processing
    C*---------------------------------------------------------------------
     C                   callp     pgmstr
    C*
    C* Write the header information
    C*
     C                   callp     writeh
    C*
    C* Reading thru WMPDP and write detail
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
     c* getProvCnt - gets the number of provider records
     c*=====================================================================---*
     p getProvCnt      B
     d getProvCnt      PI             9s 0

     d pcount          s              9s 0 inz(0)

     c*
     c/EXEC SQL
     c+ DECLARE PROJECT CURSOR FOR
     c+ SELECT COUNT(*)
     c+ FROM WMPDP
     c+ WHERE PDDT > :DATE AND PDTYPE IN ('1', '4')
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

     p getProvCnt      E
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
     C                   eval      @@dcid = 'XP' + dddate
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
     d   length                       3s 0 const

     d i               s              3s 0
     d text2           s            100a   varying

     c                   eval      text2 = toUpperCase(text)

     c*
     c* Replace & with amp; We first need to replace with ?amp; then change
     c* the ? back to an &
     c*

     c                   eval      i = %scan('&':text2:1)
     c                   dow       i <> 0
     c                   if        i < (length - 4)
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
     c* Replace . with blanks
     c*

     c                   eval      i = %scan('.':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace(' ':text2:i:1)
     c                   eval      i = %scan('.':text2:1)
     c                   enddo

     c*
     c* Replace / with blanks
     c*

     c                   eval      i = %scan('/':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace(' ':text2:i:1)
     c                   eval      i = %scan('/':text2:1)
     c                   enddo

     c*
     c* Replace , with blanks
     c*

     c                   eval      i = %scan(',':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace(' ':text2:i:1)
     c                   eval      i = %scan(',':text2:1)
     c                   enddo

     c*
     c* Replace ¢ with blanks
     c*

     c                   eval      i = %scan('¢':text2:1)
     c                   dow       i <> 0
     c                   eval      text2 = %replace(' ':text2:i:1)
     c                   eval      i = %scan('¢':text2:1)
     c                   enddo

     c                   return    text2

     p stripBadChars   E
     c*=====================================================================---*
     c* toUpperCase - Convert from lower to upper case
     c*=====================================================================---*
     p toUpperCase     B
     d toUpperCase     PI           100a   varying
     d   text                       100a   varying value

     d text2           s            100a   varying
     d textmid         s            100a
     d lower           c                   const('abcdefghijklmnopqrstuvwxyz')
     d upper           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

     c     lower:upper   xlate     text          textmid
     c                   eval      text2 = textmid

     c                   return    %trim(text2)

     p toUpperCase     E
    C*=====================================================================
    C* writed - Write detail information
    C*=====================================================================
     p     writed      B
     d     writed      pi

     D* Detail
     D namin           s            100    varying
     D ad1in           s            100    varying
     D ad2in           s            100    varying
     D anmin           s            100    varying
     D acin            s            100    varying
     D name            s             30
     D aName           s             30
     D address1        s             30
     D address2        s             30
     D city            s             20
     D stat            s              1

    C*
     C                   read      ppd01
     C                   dow       not %eof
     C*
     C* Populate fields
     C*
     C                   if        pdfsn <> *blanks
     C                   eval      namin = %trim(pdfsn) + ' ' + %trim(pdname)
     C                   else
     C                   eval      namin = pdname
     C                   endif
     C*
     C                   eval      name = stripBadChars(namin:30)
     C*
     C                   if        pddba1 = 'Y'
     C                   eval      anmin = pdadr1
     C                   eval      ad1in = pdadr2
     C                   eval      ad2in = *blanks
     C                   else
     C                   eval      anmin = *blanks
     C                   eval      ad1in = pdadr1
     C                   eval      ad2in = pdadr2
     C                   endif
     C*
     C                   eval      aName    = stripBadChars(anmin:30)
     C                   eval      address1 = stripBadChars(ad1in:30)
     C                   eval      address2 = stripBadChars(ad2in:30)
     C*
     C                   eval      acin = pdcty
     C                   eval      city = stripBadChars(acin:20)
     C*
     C*** BEG CHG *** 11/02/09 **************************************
     C*****              if        pdstat <> 'I'
     C                   if        pdstat = 'A' or pdstat = *blanks
     C*** END CHG *** 11/02/09 **************************************
     C                   eval      stat = 'A'
     C                   else
     C                   eval      stat = 'I'
     C                   endif
     C*
     C* Write Detail
     C*
     C                   eval      xpdata = '<VendorCore >'
     C                   write     pxp01
     C                   eval      xpdata ='<Vendor vendorNumber= "' +
     C                                   %trim(%editw(pdfein:'0         ')) +
     C                                   %trim(%editw(pdmod:'0   ')) +
     C                                '" taxID="'+
     C                                   %trim(%editw(pdfein:'0         ')) +
     C                                '" statusCode="' + stat +
     C                                '" name="' + %trim(name) +
     C                                '" alternateName="' + %trim(aName)  +
     C                                '" streetLine1="' + %trim(address1)  +
     C                                '" streetLine2="' + %trim(address2)  +
     C                                '" city="' + %trim(city) +
     C                                '" state="' + %trim(pdst) +
     C                                '" zipCode="' + %char(pdzip) +
     C                                '" countryCode="US"' +
     C                                ' phoneNumber="' +
     C                                  %trim(%editw(pdarea:'0   ')) +
     C                                  %trim(%editw(pdphn#:'0       ')) +
     C                                '" notes="" />'
     C                   write     pxp01
     C                   eval         xpdata = '</VendorCore>'
     C                   write     pxp01
     C*
     C                   read      ppd01
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
     C                   eval      xpdata = '</VendorTransmission>'
    C*
     C                   write     pxp01
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
     D updP            s              1a
     D orgID           s             13a
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
     C                   eval      count = getProvCnt
     C                   move      count         numV
    C*
     C                   if        xxdate = *blanks
     C                   eval      updP = 'F'
     C                   else
     C                   eval      updP = 'I'
     C                   endif
     C*
     C                   eval      orgID  = 'GIC000000'
     C*
     C* Write Header Information
    C*
     C                   eval      xpdata = '<?xml version= "1.0"?>'
     C                   write     pxp01
     C                   eval      xpdata = '<VendorTransmission ' +
     C                                'transmissionDate= "' + trdt +
     C                                '" transmissionID= "' + trid +
     C                                '" numberOfVendors= "' + numV +
     C                                '" updateProcess= "' + updP +
     C                                '" organizationID= "' + %trim(orgID) +
     C                                '" version="V1.0">'
     C                   write     pxp01
    C*
     p writeh          E
