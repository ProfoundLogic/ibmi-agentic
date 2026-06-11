     hcopyright('(c) Copyright Fiserv, Inc., 2006')

     h/title ('Fix quote tracking where tracking is not accurate')

     h* Description:
     h*   This program will check the quote status of an employer and
     h*    determine if it is correct.  If not, it will be listed on the    Originally, the program
     h*    report (and corrected, if requestd).                             Originally, the program

     h* Created: 10/19/06   C:33  Seq#: WA133/54760
     ***********************************************************************************************
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwdcxl2    if   e           k disk    rename(pcx01:pcx02)
     fwaqtl16   if   e           k disk    rename(pqt01:pqt16)
     fwaqtp     o    e             disk    rename(pqt01:pqt0p)
     fqSysPrt   o    f  132        printer

     d*entry
     d FixWAQTP3       pr
     d  updtPrtFl                     1
     d FixWAQTP3       pi
     d  updtPrtFl                     1

     dGetCurrAgt       pr                  like(QTAGT#)
     dGetCurrSts       pr                  like(QTTYPE)
     dGetQteTAgt       pr                  like(QTAGT#)
     d getQteTSts                          like(QTTYPE)
     dIssues           pr

     d/copy sPrDate
     d/copy wPrEmplr

     dcurrentAgt       s                   like(QTAGT#)
     dcurrentSts       s                   like(QTTYPE)
     dqteTrckAgt       s                   like(QTAGT#)
     dqteTrckSts       s                   like(QTTYPE)
     drecordHdr        s             40    inz
     dsysDt7           s                   like(QTEFDT)
     dsysTime6         s                   like(QTTIME)
     dsvFnd            s                   like(QTFND)  inz
     dupdtPrtFlag      s              1    inz
     dusaQTEFDT        s               d   datFmt(*usa)
     dusaQTNBDT        s               d   datFmt(*usa)
     dusaQTORDT        s               d   datFmt(*usa)
     dusaQTWKDT        s               d   datFmt(*usa)

     dtoBeWritten      c                   'Records to be written'
     dwritten          c                   'Recrods written'

     d psds          esds                  extname(szq1p)

     c     qtky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0

     c                   eval      *inoa = *on

     c                   if        %parms >= 1
     c                   eval      updtPrtFlag = updtPrtFl
     c                   endif

     c                   if        updtPrtFlag = *blank
     c                   eval      updtPrtFlag = 'P'
     c                   endif

     c                   select
     c                   when      updtPrtFlag = 'P'
     c                   eval      recordHdr = toBeWritten
     c                   other
     c                   eval      recordHdr = written
     c                   endsl

     c                   eval      sysDt7 = DateTo7(SystemDate)
     c                   eval      sysTime6 = TimeTo6(SystemTime)

     c                   dou       %eof(WMEML)
     c                   read      WMEML
     c                   if        not %eof(WMEML)
     c                   exsr      Process
     c                   eval      svFnd = EMFND
     c                   endif
     c                   enddo

     c                   eval      *inlr = *on

     c*====================================================================
     c* Process employer
     c*====================================================================
     c     Process       begsr

     c                   eval      currentAgt = GetCurrAgt
     c                   eval      currentSts = GetCurrSts
     c                   eval      qteTrckAgt = GetQteTAgt(qteTrckSts)

     c                   if           currentAgt <> qteTrckAgt
     c                             or currentSts <> qteTrckSts
     c                   callp     Issues
     c                   endif


     c                   endsr

     oqSysPrt   e            header         1  4
     o                                           25 ' '
     o                                         +  1 'Current'
     o                                           35 ' '
     o                                         +  1 'Qte Trk'
     o                                           55 ' '
     o                       recordHdr         +  0
     o          e            header         1
     o                                         +  2 'Grp'
     o                                         +  2 ' Employer'
     o                                         +  2 '  Div'
     o                                           25 ' '
     o                                         +  0 'Agent'
     o                                         +  1 'Sts'
     o                                           35 ' '
     o                                         +  0 'Agent'
     o                                         +  1 'Sts'
     o                                           55 ' '
     o                                         +  0 'Agent'
     o                                         +  1 'Sts'
     o                                         +  2 'Eff Date'
     o                                         +  3 'Typ Chg Dt'
     o                                         +  2 'Org Sub Dt'
     o                                         +  2 'New Bus Dt'
     o          e            record         0
     o                       EMFND         z   +  2
     o                       EMEMP#        z   +  2
     o                       EMDIV         z   +  2
     o                                           25 ' '
     o                       currentAgt    z   +  0
     o                       currentSts        +  2
     o                                           35 ' '
     o                       qteTrckAgt    z   +  0
     o                       qteTrckSts        +  2
     o          e            detail         1
     o                                           55 ' '
     o                       QTAGT#        z   +  0
     o                       QTTYPE            +  2
     o                       usaQTEFDT         +  2
     o                       usaQTWKDT         +  2
     o                       usaQTORDT         +  2
     o                       usaQTNBDT         +  2
     o          e            skipLine       1



     p*====================================================================
     pGetCurrAgt       b
     dGetCurrAgt       pi                  like(QTAGT#)

     d agent#          s                   like(QTAGT#)
     d agtName         s             60
     d outFyr          s                   like(CXFYR)  inz(*hival)

     d AgtAPI          pr                  extpgm('WA002R')
     d  co#                                like(EMCO#)  const
     d  fnd                                like(EMFND)  const
     d  emp#                               like(EMEMP#) const
     d  div                                like(EMDIV)  const
     d  fyr                                like(CXFYR)  const
     d  agt#                               like(QTAGT#)
     d  agtNm                        60

     c                   callp     AgtAPI(EMCO#:EMFND:EMEMP#:EMDIV:outFyr:
     c                                    agent#:agtName)

     c                   return    agent#

     pGetCurrAgt       e



     p*====================================================================
     pGetCurrSts       b
     dGetCurrSts       pi                  like(QTTYPE)

     c     keyWDCXL2     klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c                   kfld                    EMEMP#
     c                   kfld                    EMDIV

     c                   clear                   CXCFDT
     c                   clear                   CXREDT

     c     keyWDCXL2     setgt     pcx02
     c                   dou       %eof or
     c                             (CXCFDT <> *zero and CXREDT <> *zero)
     c     keyWDCXL2     readpe    pcx02
     c                   enddo

     c                   select
     c                   when      currentAgt = *zero
     c                   return    ' '
     c                   when      GetCxDate(EMCO#:EMFND:EMEMP#:EMDIV) <> *zero
     c                   return    'C'
     c                   when      %eof
     c                   return    'W'
     c                   when      CXCFDT <> *zero and CXREDT <> *zero
     c                   return    'R'
     c                   when      CXCFDT <> *zero
     c                   return    'C'
     c                   other
     c                   return    'W'
     c                   endsl

     pGetCurrSts       e



     p*====================================================================
     pGetQteTAgt       b
     dGetQteTAgt       pi                  like(QTAGT#)
     d getQteTSts                          like(QTTYPE)

     d outFyr          s                   like(CXFYR)  inz(*hival)

     c     keyWAQTL16    klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c                   kfld                    EMEMP#
     c                   kfld                    EMDIV

     c                   clear                   QTAGT#
     c                   clear                   QTTYPE

     c     keyWAQTL16    setgt     pqt16
     c     keyWAQTL16    readpe    pqt16

     c                   select
     c                   when      QTSTAT = 'Q'
     c                   eval      getQteTSts = *blank
     c                   return    *zeros
     c                   when      QTTYPE = 'T' and QTXFER <> *blank
     c                   eval      getQteTSts = QTXFER
     c                   other
     c                   eval      getQteTSts = QTTYPE
     c                   endsl

     c                   return    QTAGT#

     pGetQteTAgt       e



     p*====================================================================
     pIssues           b
     dIssues           pi

     dprintOnly        s              1n   inz
     dsameEmplr        s              1n   inz

     c                   select
     c*_____
     c                   when          currentAgt = qteTrckAgt
     c                             and currentSts = 'W'
     c                             and qteTrckSts = 'T'
     c                   return
     c*_____ 3,4,5
     c                   when          currentAgt = qteTrckAgt
     c                             and currentSts = 'W'
     c                   eval      QTWKDT = sysDt7
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'W'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 6,7,8
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'W'
     c                             and qteTrckAgt <> *zero
     c                   eval      QTWKDT = sysDt7
     c                   eval      QTAGT# = qteTrckAgt
     c                   eval      QTTYPE = 'X'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'T'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 9
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'W'
     c                   eval      QTWKDT = sysDt7
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'W'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 10,12,13
     c                   when          currentAgt = qteTrckAgt
     c                             and currentSts = 'C'
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 14
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'C'
     c                             and (qteTrckSts = 'W' or qteTrckSts = 'T')
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = *blanks
     c                   eval      QTAGT# = qteTrckAgt
     c                   eval      QTTYPE = 'X'
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'T'
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 15
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'C'
     c                             and qteTrckSts = 'C'
     c                   eval      QTWKDT = sysDt7
     c                   eval      QTXFER = 'C'
     c                   eval      QTAGT# = qteTrckAgt
     c                   eval      QTTYPE = 'X'
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'T'
     c                   exsr      WriteWAQTP
     c*_____ 16
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'C'
     c                             and qteTrckSts = 'R'
     c                   select
     c                   when      QTWKDT < CXREDT and CXCFDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = 'R'
     c                   eval      QTAGT# = qteTrckAgt
     c                   eval      QTTYPE = 'X'
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'T'
     c                   exsr      WriteWAQTP
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 17
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'C'
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'W'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   eval      QTXFER = *blanks
     c                   exsr      WriteWAQTP
     c*_____ 18,21
     c                   when          currentAgt = qteTrckAgt
     c                             and currentSts = 'R'
     c                             and qteTrckSts <> 'C'
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = *blanks
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   exsr      WriteWAQTP
     c                   select
     c                   when      QTWKDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'R'
     c                   exsr      WriteWAQTP
     c*_____ 19
     c                   when          currentAgt = qteTrckAgt
     c                             and currentSts = 'R'
     c                             and qteTrckSts = 'C'
     c                   select
     c                   when      QTWKDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = *blanks
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'R'
     c                   exsr      WriteWAQTP
     c*_____ 22
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'R'
     c                             and qteTrckSts = 'W'
     c                   select
     c                   when      QTWKDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = *blanks
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'R'
     c                   exsr      WriteWAQTP
     c*_____ 23
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'R'
     c                             and qteTrckSts = 'C'
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = *blanks
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   exsr      WriteWAQTP
     c                   select
     c                   when      QTWKDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'R'
     c                   exsr      WriteWAQTP
     c*_____ 24
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'R'
     c                             and qteTrckSts = 'R'
     c                   select
     c                   when      QTWKDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = 'R'
     c                   eval      QTAGT# = qteTrckAgt
     c                   eval      QTTYPE = 'X'
     c                   exsr      WriteWAQTP
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'T'
     c                   exsr      WriteWAQTP
     c*_____ 25
     c                   when          currentAgt <> qteTrckAgt
     c                             and currentSts = 'R'
     c                             and qteTrckSts = ' '
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTXFER = *blanks
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'W'
     c                   exsr      WriteWAQTP
     c                   select
     c                   when      QTWKDT < CXCFDT
     c                   eval      QTWKDT = CXCFDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'C'
     c                   exsr      WriteWAQTP
     c                   select
     c                   when      QTWKDT < CXREDT
     c                   eval      QTWKDT = CXREDT
     c                   other
     c                   eval      QTWKDT = sysDt7
     c                   endsl
     c                   eval      QTAGT# = currentAgt
     c                   eval      QTTYPE = 'R'
     c                   exsr      WriteWAQTP
     c*_____
     c                   other
     c                   eval      printOnly = *on
     c                   exsr      WriteWAQTP

     c                   endsl

     c                   return

     c*====================================================================
     c     WriteWAQTP    begsr

     c                   eval      sysTime6 = sysTime6 + 1

     c                   select
     c                   when      qteTrckSts = *blank
     c                   eval      QTCO#  = EMCO#
     c                   eval      QTFND  = EMFND
     c                   eval      QTEMP# = EMEMP#
     c                   eval      QTDIV  = EMDIV
     c                   eval      QTCHAP = EMCHAP
     c                   eval      QTNAME = EMNAME
     c                   eval      QTEFDT = EMEFD1
     c                   eval      QTCARR = *blanks
     c                   eval      QTPREM = *zeros
     c                   eval      QTCOMM = *blanks
     c                   eval      QTSTAT = 'W'
     c                   eval      QTMOD  = *zeros
     c                   eval      QTARAP = *zeros
     c                   eval      QTHLR  = *zeros
     c                   eval      QTPLR  = *zeros
     c                   eval      QTFIN  = *blanks
     c                   eval      QTNBDT = sysDt7
     c                   eval      QTRVDT = *zeros
     c                   eval      QTBFPR = *zeros
     c                   eval      QTORDT = sysDt7
     c                   eval      QTFXDT = *zeros
     c                   eval      QTCOM2 = *blanks
     c                   eval      QTAULS = *blanks
     c                   endsl

     c                   select
     c                   when      printOnly
     c                   exsr      PrintRcds
     c                   when      updtPrtFlag = 'B'
     c                   exsr      PrintRcds
     c                   exsr      UpdateRcds
     c                   when      updtPrtFlag = 'U'
     c                   exsr      UpdateRcds
     c                   other
     c                   exsr      PrintRcds
     c                   endsl

     c                   eval      sameEmplr = *on

     c                   endsr

     c*====================================================================
     c     PrintRcds     begsr

     c                   eval      usaQTEFDT = CvtToDate(QTEFDT)
     c                   eval      usaQTNBDT = CvtToDate(QTNBDT)
     c                   eval      usaQTORDT = CvtToDate(QTORDT)
     c                   eval      usaQTWKDT = CvtToDate(QTWKDT)

     c                   select
     c                   when      *inoa
     c                   except    header
     c                   eval      *inoa = *off
     c                   when      svFnd <> EMFND
     c                   except    skipLine
     c                   endsl

     c                   select
     c                   when      sameEmplr
     c                   except    detail
     c                   when      printOnly
     c                   except    skipLine
     c                   except    record
     c                   except    skipLine
     c                   other
     c                   except    skipLine
     c                   except    record
     c                   except    detail
     c                   endsl

     c                   endsr

     c*====================================================================
     c     UpdateRcds    begsr

     c                   eval      QTUSER = Q1USER
     c                   eval      QTPGM  = Q1PGM
     c                   eval      QTTIME = sysTime6
     c                   eval      QTDT   = sysDt7
     c                   eval      QTIUSR = QTUSER
     c                   eval      QTIPGM = QTPGM
     c                   eval      QTITM  = QTTIME
     c                   eval      QTIDT  = QTDT

     c                   write     pqt0P

     c                   endsr

     pIssues           e
