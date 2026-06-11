     H*(c) 2012 StoneRiver, Inc.  All rights reserved.                    *
     H*StoneRiver is a trademark of StoneRiver, Inc.                      *
     H dftactgrp(*no)
    H*********************************************************************
    H/TITLE DOCVSPDFI - Check for existence of images
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
    H*********************************************************************
     FIMDOP     uf   e             disk    rename(PDO01:PDO0P)
     F                                     infds(imdopFDS)
     FSXCMP     if a e           k disk
     Fsmoll     if   e           k disk    rename(pol01:pol00)
     Fsmdll     if   e           k disk    rename(pdl01:pdl00)
     Fsdcmle    if   e           k disk
     Fqsysprt   o    f  198        printer oflind(*inof) usropn

     D*entry
     D DocVsPDFI       pr                  extPgm('DOCVSPDFI')
     D  inPrtUpdFlag                  1
     D  inLastRRN                     9
     D  inList                        1
     D DocVsPDFI       pi
     D  inPrtUpdFlag                  1
     D  inLastRRN                     9
     D  inList                        1

     DChkDlo           pr                  extPgm('SRCHKDLO')
     D docName                       12    const
     D docFlr                        32    const
     D docType                        4    const
     D error                          1n   const
     D UNCN                         255    const
     D FSF                            1    const
     DChkIFSObj        pr                  extPgm('CHKIFSOBJ')
     D ifsObj                       256
     D ifsObjFound                    1n
     DRtvFldPth        pr                  extPgm('SA039R')
     D folder                        80

     DActualImgF       pr                   like(DOFL01)
     D inImgF                               like(DOFL01)
     DcenterFld        PR         32766    opdesc
     D fieldToCtr                 32766    options(*varsize) const
     D size                           5  0 options(*nopass)  const
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     DDloExists        pr             1n
     DIfsObjExists     pr             1n
     DNumericSuffix    pr             1n
     D doc                                 like(DODCID)
     DObjExists        pr             1n
     DRtvPath          pr            80
     D inFolder                      80                     const
     D inRqst                         1    options(*nopass) const

     D imdopFDS        ds
     D imdopRRN              397    400b 0

     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D*
     D psds          esds                  extname(szq1p)

     D actualDCID      s                   like(DODCID)
     D countNotExist   s              7  0
     D countScanned    s              7  0 inz
     D existOnly       s              1n
     D ifsDCID         s             12
     D ifsPath         s            256
     D key             s             79
     D lastRRN         s              9  0
     D list            s              1
     D openPrt         s              1n
     D printOnly       s              1n
     D prtUpdFlag      s              1
     D reasonDesc      s             30
     D sysDt           s               d
     D sysDt7          s              7  0
     D subTitle        s             60
     D ##Pgm           s             10
     D ##Key           s             79
     D ##Name          s             79
     D ##RtCd          s              1

     D notExist        c                   'Does not exist'
     D docNameInc      c                   'Document name incorrect'
     D dartNameInc     c                   'DARTS name incorrect'
     D flagInc         c                   'DOFL01 flag incorrect'
     D title           c                   'DARTS remarks not referencing-
     D                                      valid documents'

     C                   select
     C                   when      %parms >= 1 and inPrtUpdFlag = 'E'
     C                   eval      prtUpdFlag = 'P'
     C                   eval      existOnly  = *on
     C                   when      %parms >= 1
     C                   eval      prtUpdFlag = inPrtUpdFlag
     C                   other
     C                   eval      prtUpdFlag = 'P'
     C                   endsl

     C                   eval      printOnly = prtUpdFlag = 'P'

     C                   time                    dsdttm
     C     *mdy          move      dsdate        sysDt
     C     *cymd         move      sysDt         sysDt7

     C                   if        %parms >= 2
     C                   move      inLastRRN     lastRRN
     C                   endif

     C                   if        %parms >= 3
     C                   eval      list = inList
     C                   endif

     C                   select
     C                   when      list = 'A'
     C                   eval      subTitle = '* * All Inclusive * *'
     C                   when      list = 'N'
     C                   eval      subTitle = '* * New Since Last Run * *'
     C                   endsl
     C                   eval      subTitle = CenterFld(subTitle)

     C                   select
     C                   when      %parms < 2
     C     *start        setll     IMDOP
     C                   when      %parms >= 2 and lastRRN <> *zero
     C     lastRRN       chain     IMDOP
     C                   other
     C     *start        setll     IMDOP
     C                   endsl

     C                   dou       %eof(IMDOP)
     C                   read      IMDOP
     C                   if        not %eof(IMDOP)
     C                              and DODCID <> *blank
     C                              and DODCID <> 'W0070C'
     C                              and DODCID <> 'WOO70C'
     C                              and DODCID <> 'W0050C01'
     C                   exsr      ChkDocument
     C                   eval      countScanned=countScanned+1
     C                   endif
     C                   enddo

     C                   select
     C                   when      existOnly
     C                   if        countNotExist = 0
     C                   exsr      PrintHdr
     C                   endif
     C                   except    countE
     C                   endsl
     C                   move      imdopRRN      inLastRRN

     C                   eval      *inlr = *on

     C*====================================================================
     C     ChkDocument   begsr

     C     imdopRRN      chain     SXCMP                              96

     C                   select
     C                   when      %found(SXCMP) and existOnly
     C                   when      not *in96     and existOnly
     C                   when      not ObjExists
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = notExist
     C                   select
     C                   when      existOnly
     C                   eval      CMRRN  = imdopRRN
     C                   eval      CMIDT  = sysDt7
     C                   eval      CMIPGM = q1pgm
     C                   write     pcm0x
     C                   eval      countNotExist = countNotExist + 1
     C                   other
     C                   exsr      PrintReason
     C                   endsl
     C                   when      existOnly
     C                   when         (DOFL01 = ' ' and
     C                                  NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  not NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = docNameInc
     C                   exsr      PrintReason
     C                   when         (DOFL01 = ' ' and
     C                                  not NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = docNameInc
     C                   exsr      PrintReason
     C                   eval      reasonDesc = dartNameInc
     C                   exsr      PrintReason
     C                   when         (DOFL01 = ' ' and
     C                                  not NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = dartNameInc
     C                   exsr      PrintReason
     C                   if        not printOnly
     C                   eval      reasonDesc = 'DARTS record updated'
     C                   exsr      PrintReason
     C                   eval      DOLPGM = 'DOCVSPDFI'
     C                   eval      DODCID = actualDCID
     C                   update    pdo0p
     C                   endif
     C                   when         (DOFL01 = ' ' and
     C                                  NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  not NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = dartNameInc
     C                   exsr      PrintReason
     C                   eval      reasonDesc = flagInc
     C                   exsr      PrintReason
     C                   if        not printOnly
     C                   eval      reasonDesc = 'DARTS record updated'
     C                   exsr      PrintReason
     C                   eval      DOFL01 = ActualImgF(DOFL01)
     C                   eval      DODCID = actualDCID
     C                   eval      DOLPGM = 'DOCVSPDFI'
     C                   update    pdo0p
     C                   endif
     C                   when         (DOFL01 = ' ' and
     C                                  NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  not NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = docNameInc
     C                   exsr      PrintReason
     C                   eval      reasonDesc = dartNameInc
     C                   exsr      PrintReason
     C                   eval      reasonDesc = flagInc
     C                   exsr      PrintReason
     C                   when         (DOFL01 = ' ' and
     C                                  not NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = flagInc
     C                   exsr      PrintReason
     C                   if        not printOnly
     C                   eval      reasonDesc = 'DARTS record updated'
     C                   exsr      PrintReason
     C                   eval      DOFL01 = ActualImgF(DOFL01)
     C                   eval      DOLPGM = 'DOCVSPDFI'
     C                   update    pdo0p
     C                   endif
     C                   when         (DOFL01 = ' ' and
     C                                  not NumericSuffix(DODCID) and
     C                                  NumericSuffix(actualDCID))
     C                             or
     C                                (DOFL01 = 'O' and
     C                                  NumericSuffix(DODCID) and
     C                                  not NumericSuffix(actualDCID))
     C                   exsr      PrintInfo
     C                   eval      reasonDesc = docNameInc
     C                   exsr      PrintReason
     C                   eval      reasonDesc = flagInc
     C                   exsr      PrintReason
     C                   endsl

     C                   endsr

     C*====================================================================
     C     ExpandKey     begsr


     C     cmKmey        klist
     C                   kfld                    DODKEY
     C                   kfld                    DODSQ#

     C     cmKmey        chain     PCM01

     C                   call      'SRKCVT1'
     C                   parm      CMPNAM        ##Pgm
     C     key           parm      CMKEY         ##Key
     C                   parm                    ##Name
     C                   parm                    ##RtCd

     C                   if        key = *blank
     C                   eval      key = DODKEY
     C                   endif

     C                   endsr

     C*====================================================================
     C     PrintDtl      begsr

     C                   select
     C                   when      existOnly
     C                   exsr      ExpandKey
     C                   except    detailE
     C                   other
     C                   except    detail
     C                   endsl

     C                   endsr

     C*====================================================================
     C     PrintHdr      begsr

     C                   time                    dsdttm

     C                   if        not openPrt
     C                   open      QSYSPRT
     C                   eval      openPrt = *on
     C                   eval      *inof = *on
     C                   endif

     C                   if        *inof
     C                   select
     C                   when      existOnly
     C                   except    headE
     C                   other
     C                   except    head
     C                   endsl
     C                   eval      *inof = *off
     C                   endif

     C                   endsr

     C*====================================================================
     C     PrintInfo     begsr

     C                   exsr      PrintHdr
     C                   exsr      PrintDtl

     C                   endsr

     C*====================================================================
     C     PrintReason   begsr

     C                   if        *inof
     C                   exsr      PrintHdr
     C                   exsr      PrintDtl
     C                   endif
     C                   except    reason

     C                   endsr

     Oqsysprt   e            head           2 03
     O                       title               88
     O          e            head           2
     O                                            4 'Key'
     O                                           48 'Seq #'
     O                                           55 'DOFL01'
     O                                           62 'CMFL05'
     O                                           75 'DARTS Doc Nm'
     O                                           88 'Should be Nm'
     O                                           96 'Issue'
     O          e            detail         0
     O                       CMKEY               42
     O                       CMSEQ#        X     48
     O                       DOFL01              52
     O                       CMFL05              59
     O                       DODCID              75
     O                       actualDCID          88
     O                       imdopRRN      Z    132
     O          e            headE          1 03
     O                       dsDate             + 2 '  /  /  '
     O                       dsTime             + 3 '  :  :  '
     O                       title               88
     O          e            headE          2
     O                       subTitle           +36
     O          e            headE          2
     O                                            4 'Key'
     O                                           86 'Seq #'
     O                                           99 'DARTS Doc Nm'
     O                                          106 'DOFL01'
     O                                          113 'CMFL05'
     O          e            detailE        1
     O                       key                 80
     O                       DODSQ#        X     86
     O                       DODCID              99
     O                       DOFL01             103
     O                       CMFL05             110
     O          e            countE      2
     O                                          + 2 'Total missing documents:'
     O                       countNotExist J    + 2
     O          e            countE      2  2
     O                                          + 2 'Total scanned documents:'
     O                       countScanned  J    + 2
     O          e            reason         1
     O                       reasonDesc         121





     PActualImgF       b
     DActualImgF       pi                   like(DOFL01)
     D inImgF                               like(DOFL01)

     C                   select
     C                   when      inImgF = ' '
     C                   return    'O'
     C                   when      inImgF = 'O'
     C                   return    ' '
     C                   other
     C                   return    inImgF
     C                   endsl

     PActualImgF       e





     PcenterFld        B                   Export
     DcenterFld        PI         32766    opdesc
     D fieldToCtr                 32766    options(*varsize) const
     D size                           5  0 options(*nopass)  const
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D x               s              5  0
     D y               s              5  0
     D z               s              5  0
     D returnFld       s                   like(fieldToCtr)
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   eval      x = inLen
     C                   if        %parms >=2 and size <> 0
     C                   eval      x = size
     C                   endif
     C     ' '           checkr    fieldToCtr:x  y
     C                   eval      z = ((x - y) / 2) + 1
     C                   eval      %subst(returnFld:z:y) =
     C                             %subst(fieldToCtr:1:y)
     C
     C                   return    returnFld
     PcenterFld        E





     PDloExists        b
     DDloExists        pi             1n

     D dloFlr          s             32
     D error           s              1n
     D found           s              1n
     D ifsObj          s            256

     C                   eval      dloFlr = %trim(RtvPath((%TRIM(DOFLR) + '/' +
     C                                      %trim(DOSFLR))))
     C                   callp     ChkDlo(DODCID:dloflr:'*DOC':error:
     C                                       DOUNCN:DOIFSP)
     C                   eval      found = not error

     C                   if        not found
     C                   select
     C                   when      NumericSuffix(DODCID)
     C                   eval      actualDCID = %subst(DODCID:1:2)
     C                                        + %subst(DODCID:6:3)
     C                                        + %subst(DODCID:10:3)
     C                                        + '.PDF'
     C                   when      not NumericSuffix(DODCID)
     C                   eval      actualDCID = %subst(DODCID:1:2) + '000'
     C                                        + %subst(DODCID:3:3) + '.'
     C                                        + %subst(DODCID:6:3)
     C                                        + '.PDF'
     C                   endsl
     C                   callp     ChkDlo(actualDCID:dloFlr:'*DOC':error:
     C                              DOUNCN:DOIFSP)
     C                   eval      found = not error
     C                   endif

     C                   return    found

     PDloExists        e





     PIfsObjExists     b
     DIfsObjExists     pi             1n

     D found           s              1n   inz
     D ifsObj          s            256

     C     dartsKey      klist
     C                   kfld                    DODKEY
     C                   kfld                    DODSQ#

     C                   clear                   DLVOL1
     C                   clear                   DLOFLD
     C                   clear                   DLOSBD
     C                   clear                   OLVOL1
     C                   clear                   OLOFLD
     C                   clear                   OLOSBD

     C     dartsKey      chain     SMDLL
     C     dartsKey      chain     SMOLL

     C                   eval      ifsDCID = DLDCID
     C                   eval      ifsPath = 'OPTMLB01/'
     C                                     + %trim(DLVOL1) + '/'
     C                                     + %trim(DLOFLD) + '/'
     C                                     + %trim(DLOSBD)
     C                   eval      ifsObj = '/' +
     C                                      %trim(RtvPath(ifsPath)) + '/' +
     C                                      %trim(ifsDCID)
     C                   callp(e)  ChkIFSObj(IFSObj:found)

     C                   if        not found
     C                   eval      ifsDCID = DLDCID
     C                   eval      ifsPath = 'OPTMLB01/'
     C                                     + %trim(OLVOL1) + '/'
     C                                     + %trim(OLOFLD) + '/'
     C                                     + %trim(OLOSBD)
     C                   eval      ifsObj = '/' +
     C                                      %trim(RtvPath(ifsPath)) + '/' +
     C                                      %trim(ifsDCID)
     C                   callp(e)  ChkIFSObj(IFSObj:found)
     C                   endif

     C                   return    found

     PIfsObjExists     e





     PNumericSuffix    b
     DNumericSuffix    pi             1n
     D doc                                 like(DODCID)

     D numeric         s              1n
     D suffix          s              3

     C                   eval      suffix = %subst(doc:10:3)
     C                   testn                   suffix               979899
     C                   eval      numeric = *in97

     C                   return    numeric

     PNumericSuffix    e





     PObjExists        b
     DObjExists        pi             1n

     D found           s              1n

     C                   eval      actualDCID = DODCID

     C                   select
     C                   when      CMFL05 = 'O'
     C                   eval      found = IfsObjExists
     C                   other
     C                   eval      found = DloExists
     C                   endsl
     C                   return    found
     PObjExists        e





     PRtvPath          b
     DRtvPath          pi            80
     D inFolder                      80                     const
     D inRqst                         1    options(*nopass) const

     D request         s                   like(inRqst) inz
     D rtnFolder       s                   like(RtvPath)
     D root            s             10    inz

     C                   if        %parms >= 2
     C                   eval      request = inRqst
     C                   endif
     C                   call      'RTVDOCROOT'
     C                   parm                    root

     C                   eval      rtnFolder = inFolder
     C                   callp     RtvFldPth(rtnFolder)

     C                   select
     C                   when      request = 'Q'
     C                   return    '/' + %trim(root) + '/' + rtnfolder
     C                   other
     C                   return    rtnFolder
     C                   endsl

     PRtvPath          e




