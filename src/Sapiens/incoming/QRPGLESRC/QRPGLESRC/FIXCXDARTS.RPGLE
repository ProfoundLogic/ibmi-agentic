     hcopyright('(c) Copyright Fiserv, Inc., 2006')

     Fsdcml     if   e           k disk
     Fwdxhl16   if   e           k disk    rename(pxh01:pxh16)
     Fwdxhl17   if   e           k disk    rename(pxh01:pxh17)

     Fqsysprt   o    f  132        printer oflind(*inof)

     DreprintCX        pr                  extPgm('W7166C')
     D company                        3    const
     D group                          3    const
     D year                           3    const
     D emp#                           9    const
     D div                            5    const
     D req#                           5    const
     D certLtr                        1    const
     D reprint                        1    const
     D outq                          10    const
     D bat#                           9    const
     D type                           1    const
     D certified                      1    const

     DreprintReins     pr                  extPgm('W7168C')
     D company                        3    const
     D group                          3    const
     D year                           3    const
     D emp#                           9    const
     D div                            5    const
     D req#                           5    const
     D certLtr                        1    const
     D outq                          10    const
     D reprint                        1    const
     D type                           1    const

     DCrtDocPath       pr            80
     D folder                              like(CMFOLD)
     D document                            like(CMDCID)

     DCrtPDF           pr
     D original                            like(splfName)
     D folder                              like(CMFOLD)
     D document                            like(CMDCID)

     DDocExists        pr             1n
     D folder                              like(CMFOLD)
     D document                            like(CMDCID)

     DSrChkDlo         pr                  extPgm('SRCHKDLO')
     D dloName                       12    const
     D flrPath                       32    const
     D dloType                        4    const
     D error                          1n

     D/copy sPrDate
     D/copy sPrRunCmd

     D command         s            255
     D cxDate          s             25
     D cx              s              1n
     D countReq#       s              3  0
     D countMissing    s              5  0
     D countUpdate     s              5  0
     D countNotUpdate  s              5  0
     D dateUSA         s               d   datFmt(*USA)
     D inPrtUpd        s              1
     D print           s              1n   inz(*on)
     D prtUpd          s              1    inz('P')
     D outUpdate       s             15
     D reinsDate       s             25
     D reins           s              1n
     D reinsCx         s             15
     D splfName        s             10
     D saveXHREQ#      s                   like(XHREQ#)
     D update          s              1n   inz(*off)
     D totUpdated      s             80
     D totNotUpdated   s             80

     D                 ds
     D CMKEY                         40
     D  xxCo#                         3  0 overlay(CMKEY)
     D  xxFnd                         3  0 overlay(CMKEY:4)
     D  xxEmp#                        9  0 overlay(CMKEY:7)
     D  xxDiv                         5  0 overlay(CMKEY:16)

     C     *entry        plist
     C                   parm                    inPrtUpd

     C     keyWDXHL      klist
     C                   kfld                    CMDT
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv

     C                   if        %parms >= 1
     C                   eval      prtUpd = inPrtUpd
     C                   endif

     C                   select
     C                   when      prtUpd = 'B'
     C                   eval      update = *on
     C                   when      prtUpd = 'U'
     C                   eval      update = *on
     C                   eval      print = *off
     C                   endsl

     C     *loval        setll     sdcml
     C                   dou       %eof(sdcml)
     C                   read      sdcml
     C                   if        %subst(CMDCID:10:3) = 'PDF' and
     C                             (%subst(CMFOLD:1:5) = 'CANCL' or
     C                              %subst(CMFOLD:1:5) = 'REINS') and
     C                             not DocExists(CMFOLD:CMDCID)
     C                   except    empInfo
     C                   exsr      ProcessCx
     C                   endif
     C                   enddo

     C                   select
     C                   when      print
     C                   eval      totUpdated = 'Total to be updated: ' +
     C                                          %editc(countUpdate:'Z')
     C                   eval      totNotUpdated = 'Total NOT to be updated: '
     C                                           + %editc(countNotUpdate:'Z')
     C                   when      update
     C                   eval      totUpdated = 'Total updated: ' +
     C                                            %editc(countUpdate:'Z')
     C                   eval      totNotUpdated = 'Total NOT updated: '
     C                                             + %editc(countNotUpdate:'Z')
     C                   endsl

     C                   except    total

     C                   eval      *inlr = *on

     C*====================================================================
     C*====================================================================
     C     ProcessCx     begsr

     C                   eval      countMissing = countMissing + 1

     C                   eval      cx     = *off
     C                   eval      reins  = *off

     C                   select
     C                   when      %subst(CMFOLD:1:5) = 'CANCL'
     C                   eval      cx = *on
     C                   when      %subst(CMFOLD:1:5) = 'REINS'
     C                   eval      reins = *on
     C                   endsl

     C                   exsr      setllWDXHL

     C                   eval      saveXHREQ# = *zeros
     C                   eval      countReq#  = *zeros

     C                   dou       %eof
     C                   exsr      readWDXHL
     C                   if        not %eof
     C                   exsr      ProcessDtl
     C                   endif
     C                   enddo

     C                   exsr      ProcessUpdt

     C                   endsr

     C*====================================================================
     C*====================================================================
     C     ProcessDtl    begsr

     C                   if        XHREQ# <> saveXHREQ#
     C                   eval      countReq# = countReq# + 1
     C                   endif
     C                   eval      saveXHREQ# = XHREQ#

     C                   eval      dateUSA = CvtToDate(CMDT)

     C                   eval      reinsCx = *blanks
     C                   select
     C                   when      cx = *on
     C                   eval      reinsCx = 'Cancellation'
     C                   when      reins = *on
     C                   eval      reinsCx = 'Reinstatement'
     C                   endsl

     C                   eval      cxDate = 'Date of Cx: ' +
     C                                      AlphaUSA(CvtToDate(XHCDAT))

     C                   clear                   reinsDate
     C                   if        XHRDAT <> *zero
     C                   eval      reinsDate = 'Date of Reins: ' +
     C                                         AlphaUSA(CvtToDate(XHRDAT))
     C                   endif

     C                   if        print
     C                   except    detail
     C                   endif

     C                   endsr

     C*====================================================================
     C*====================================================================
     C     ProcessUpdt   begsr

     C                   select
     C                   when      countReq# = 1 and not update
     C                   eval      outUpdate = 'To Be Updated'
     C                   eval      countUpdate = countUpdate + 1
     C                   when      countReq# = 1 and update
     C                   eval      outUpdate = 'Updated'
     C                   eval      countUpdate = countUpdate + 1
     C                   when      not update
     C                   eval      outUpdate = 'Will not update'
     C                   eval      countNotUpdate = countNotUpdate + 1
     C                   when      update
     C                   eval      outUpdate = 'Not Updated'
     C                   eval      countNotUpdate = countNotUpdate + 1
     C                   endsl

     C                   if        print
     C                   except    updtDtl
     C                   endif

     C                   if        outUpdate = 'Updated'
     C                   select
     C                   when      cx
     C                   callp     reprintCX(%editc(XHCO#:'X'):
     C                                       %editc(XXFND:'X'):
     C                                       %editc(XHFYR:'X'):
     C                                       %editc(XHEMP#:'X'):
     C                                       %editc(XHDIV:'X'):
     C                                       %editc(XHREQ#:'X'):
     C                                       'N':'Y':'QPRINT    ':
     C                                       *all'1':' ':'Y')
     C                   eval      splfName = 'CNLFORM'
     C                   when      reins
     C                   callp     reprintReins(%editc(XHCO#:'X'):
     C                                          %editc(XXFND:'X'):
     C                                          %editc(XHFYR:'X'):
     C                                          %editc(XHEMP#:'X'):
     C                                          %editc(XHDIV:'X'):
     C                                          %editc(XHREQ#:'X'):
     C                                          'N':'QPRINT    ':
     C                                          'Y':' ')
     C                   eval      splfName = 'REINFRM'
     C                   endsl
     C                   callp     CrtPDF(splfName:CMFOLD:CMDCID)
     C                   endif

     C                   endsr

     C*====================================================================
     C*====================================================================
     C     readWDXHL     begsr

     C                   select
     C                   when      cx
     C     keyWDXHL      reade     wdxhl16
     C                   when      reins
     C     keyWDXHL      reade     wdxhl17
     C                   endsl

     C                   endsr

     C*====================================================================
     C*====================================================================
     C     setllWDXHL    begsr

     C                   clear                   XHREQ#

     C                   select
     C                   when      cx
     C     keyWDXHL      setll     wdxhl16
     C                   when      reins
     C     keyWDXHL      setll     wdxhl17
     C                   endsl

     C                   endsr

     Oqsysprt   e            empInfo     1  1
     O                       xxCo#         z
     O                       xxFnd         z     +2
     O                       xxEmp#        z     +1
     O                       xxDiv         z     +1
     O                       CMUSER              +2
     O                       CMDCID              +2
     O                       CMFOLD              +2
     Oqsysprt   e            detail         1
     O                                           +1 'Yr:'
     O                       XHFYR         z     +1
     O                                           +2 'Req#:'
     O                       XHREQ#        z     +1
     O                       dateUSA             +2
     O                       reinsCX             +2
     O                       cxDate              +2
     O                       reinsDate           +2
     Oqsysprt   e            updtDtl        1
     O                       outUpdate           +5
     Oqsysprt   e            total       3  1
     O                                           +3 'Missing PDFs:'
     O                       countMissing  z     +1
     Oqsysprt   e            total          1
     O                       totUpdated          +3
     Oqsysprt   e            total          1
     O                       totNotUpdated       +3

     PCrtDocPath       b
     DCrtDocPath       pi            80
     D flr                                 like(CMFOLD)
     D doc                                 like(CMDCID)

     C                   return    '/QDLS/' + %trim(flr) + '/' + %trim(doc)

     PCrtDocPath       e


     PCrtPDF           b
     DCrtPDF           pi
     D original                            like(splfName)
     D flr                                 like(CMFOLD)
     D doc                                 like(CMDCID)

     C                   eval      command = 'SPLAMATIC/SPLCONVERT +
     C                                        FILE(' + original + ') +
     C                                        TOOBJ(''' +
     C                                          %trim(CrtDocPath(flr:doc)) +
     C                                            ''') +
     C                                        SPLNBR(*LAST) +
     C                                        TRANSFORM(*PDF811)'
     C                   callp(e)  RunCmd(command:%len(command))
     C                   if        %error
     C                   select
     C                   when      original = 'CNLFORM'
     C                   eval      original = 'STCX.FL'
     C                   when      original = 'STCX.FL'
     C                   eval      original = 'STCX.MA'
     C                   when      original = 'STCX.MA'
     C                   eval      original = 'STCX.NY'
     C                   when      original = 'STCX.NY'
     C                   eval      original = 'STCX.VA'
     C                   when      original = 'STCX.VA'
     C                   eval      original = 'STCX.WI'
     C                   when      original = 'STCX.WI'
     C                   return
     C                   when      original = 'REINFRM'
     C                   eval      original = 'STRE.FL'
     C                   when      original = 'STRE.FL'
     C                   eval      original = 'STRE.GA'
     C                   when      original = 'STRE.GA'
     C                   eval      original = 'STRE.MA'
     C                   when      original = 'STRE.MA'
     C                   eval      original = 'STRE.NY'
     C                   when      original = 'STRE.NY'
     C                   eval      original = 'STRE.VA'
     C                   when      original = 'STRE.VA'
     C                   eval      original = 'STRE.WI'
     C                   when      original = 'STRE.WI'
     C                   return
     C                   endsl
     C                   callp     CrtPDF(original:flr:doc)
     C                   endif

     C                   eval      command = 'DLTSPLF FILE(' + original +
     C                                       ') SPLNBR(*LAST)'
     C                   do        3
     C                   callp(e)  RunCmd(command:%len(command))
     C                   enddo

     C                   eval      command = 'DLTSPLF FILE(REINCOV) ' +
     C                                       'SPLNBR(*LAST)'
     C                   do        2
     C                   callp(e)  RunCmd(command:%len(command))
     C                   enddo

     C                   eval      command = 'DLTSPLF FILE(MCXCOV) ' +
     C                                       'SPLNBR(*LAST)'
     C                   do        2
     C                   callp(e)  RunCmd(command:%len(command))
     C                   enddo

     C                   eval      command = 'DLTSPLF FILE(QSYSPRT) ' +
     C                                       'SPLNBR(*LAST)'
     C                   do        2
     C                   callp(e)  RunCmd(command:%len(command))
     C                   enddo

     C                   eval      command = 'DLTF FILE(QTEMP/WXEBL*)'
     C                   callp(e)  RunCmd(command:%len(command))
     C                   eval      command = 'DLTF FILE(QTEMP/WXEBP)'
     C                   callp(e)  RunCmd(command:%len(command))

     C                   return

     PCrtPDF           e


     PDocExists        b
     DDocExists        pi             1n
     D folder                              like(CMFOLD)
     D document                            like(CMDCID)

     D xxErr           s              1n

     C                   callp     SrChkDlo(document:%subst(folder:1:32):
     C                                      '*DOC':xxErr)

     C                   return    not xxErr
     PDocExists        e
