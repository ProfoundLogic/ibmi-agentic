    H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE LOADQTXFER - LOAD WAQTP/QTXFER
    H*********************************************************************
    H* CREATED: 11/01/00  C:33  PROJECT: WC1008                          *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     F* This file will be overwritten and the keys will be changed
     Fwaqtl7    if   e           k disk    rename(pqt01:pqt07) usropn
     F                                     infds(qtfds7)
     Fwmahl     if   e           k disk    rename(pah01:pah00)

     Fwaqtp     uf a e             disk    rename(pqt01:pqt0p)
     Fqsysprt   o    f  132        printer oflind(*inoa)

     D/copy sprruncmd
     D/copy sprstring

     DsystemDate       PR              d
     DsystemTime       PR              t

     D psds          esds                  extname(szq1p)

     D qtfds7          ds
     D QTrrn7                397    400b 0

     D ovrdbf          c                   'OVRDBF FILE(WAQTL7) SHARE(*YES)'
     D opnqryf         c                   'OPNQRYF FILE((WAQTL7)) -
     D                                     KEYFLD((QTCO#) (QTFND) (QTEMP#) -
     D                                     (QTDIV) (QTSTAT) (QTWKDT))'
     D closeFile       c                   'CLOF OPNID(WAQTL7)'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@update        s              1
     D update          s              1
     D xxstat          s                   like(QTSTAT) inz('W')
     D svxfer          s                   like(QTXFER)
     D svco#           s                   like(EMCO#)  inz
     D svfnd           s                   like(EMFND)  inz
     D svemp#          s                   like(EMEMP#) inz
     D svdiv           s                   like(EMDIV)  inz
     D title           s             60
     D oldAgt          s              1
     D addCorrect      s             10
     D QTWKDT_usa      s               d   datfmt(*usa)
     D AHEFDT_usa      s               d   datfmt(*usa)
     D sysDate_n       s              7  0
     D sysDate         s               d   datfmt(*usa)
     D sysTime         s               t   timfmt(*hms:)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*____________________________________________________________________
    C*
     C     qtky7         klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C                   kfld                    xxstat
    C*
     C     ahky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
    C*____________________________________________________________________
    C*
     C     *entry        plist
     C                   parm                    @@update
    C*____________________________________________________________________
    C*
     C     *start        setll     wmeml
     C                   read      wmeml                                  60
    C*
     C                   dow       not *in60
     C*
     C                   exsr      quoteSts
     C*
     C                   exsr      agtHistory
     C*
     C                   read      wmeml                                  60
    C*
     C                   enddo
    C*
     C                   move      *on           *inlr
    C*____________________________________________________________________
    C*
     C*====================================================================
     C* *INZSR  - Initialization subroutine
     C*====================================================================
    C*
     C     *INZSR        begsr
     C*
     C                   if        %parms >= 1
     C                   eval      update  = @@update
     C                   endif
     C*
     C                   callp     runCmd(ovrdbf:255)
     C*
     C                   exsr      openWAQTL7
     C*
     C                   eval      sysDate = systemDate
     C                   eval      sysTime = systemTime
     C*
     C     *cymd         move      sysDate       sysDate_n
     C*
     C                   select
     C                   when      update = 'N' or update = *blank
     C                   eval      title = 'Employers that need corrections -
     C                             to the Quote Tracking file'
     C                   when      update = 'Y'
     C                   eval      title = 'Employers that whose Quote -
     C                             Tracking file has been corrected'
     C                   endsl
     C*
     C                   eval      title = centerFld(title)
     C*
     C                   eval      *inoa = *on
    C*
     C                   endsr                                                  *INZSR
    C*
     C*====================================================================
     C* agtHistory - Compare records in agent history to quote status
     C*====================================================================
    C*
     C     agtHistory    begsr
     C*
     C                   select
     C                   when      update     = 'Y'
     C                   eval      addCorrect = 'Added'
     C                   other
     C                   eval      addCorrect = 'Add'
     C                   endsl
     C*
     C     qtky7         setgt     pqt07
     C     qtky7         readpe    pqt07                                  60
     C*
     C                   if        *in60
     C                   clear                   QTAGT#
     C                   endif
     C*
     C     ahky          setgt     pah00
     C     ahky          readpe    pah00                                  60
     C*
     C                   dow       not *in60 and AHSTAT = 'I'
     C     ahky          readpe    pah00                                  60
     C                   enddo
     C*
     C                   if        *in60
     C                   clear                   AHAGT#
     C                   endif
     C*
     C                   if        QTAGT# <> AHAGT# and AHAGT# <> *zero
     C*
     C     *cymd         test(d)                 AHEFDT                 99
     C  n99*cymd         move      AHEFDT        AHEFDT_usa
     C*
     C                   exsr      quote
     C*
     C                   endif
    C*
     C                   endsr                                                  *INZSR
    C*
     C*====================================================================
     C* chkEmpBrk - Check for an employer break
     C*====================================================================
    C*
     C     chkEmpBrk     begsr
     C*
     C                   exsr      chkPageBrk
     C*
     C                   if        EMCO#  <> svco#  or EMFND <> svfnd or
     C                             EMEMP# <> svemp# or EMDIV <> svdiv
     C                   except    emplrDtl
     C                   eval      svco#  = EMCO#
     C                   eval      svfnd  = EMFND
     C                   eval      svemp# = EMEMP#
     C                   eval      svdiv  = EMDIV
     C                   eval      svco#  = EMCO#
     C                   endif
    C*
     C                   endsr                                                  *INZSR
    C*
     C*====================================================================
     C* chkPageBrk - Check for an page break
     C*====================================================================
    C*
     C     chkPageBrk    begsr
     C*
     C                   if        *inoa
     C                   except    head
     C                   except    heading
     C                   eval      *inoa = *off
     C                   endif
    C*
     C                   endsr                                                  *INZSR
    C*
    C*====================================================================
    C* empAgt - Check to see if this emp already has an agent assigned
    C*====================================================================
    C*
     C     empAgt        begsr
    C*
     C                   eval      oldAgt = 'N'
    C*
     C** **qtky7         chain     pqt07                              60
    C*
     C                   if        QTAGT# <> *zero
    C*
    C                   select
    C                   when      QTTYPE = 'C' or QTXFER = 'C'
    C                   eval      QTXFER = 'C'
    C                   when      QTTYPE = 'R' or QTXFER = 'R'
    C                   eval      QTXFER = 'R'
    C                   other
    C                   clear                   QTXFER
    C                   endsl
    C*
     C                   eval      QTTYPE = 'X'
    C*
     C                   exsr      writeWAQTP
    C*
     C                   eval      QTAGT# = AHAGT#
     C                   eval      QTTYPE = 'T'
    C*
     C                   exsr      writeWAQTP
    C*
     C                   clear                   QTXFER
    C*
     C                   eval      oldAgt = 'Y'
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* openWAQTL7 - Execute query selection and open file
    C*================================================================
    C*
     C     openWAQTL7    begsr
     C*
     C                   callp     runCmd(opnqryf:255)
     C*
     C                   open      waqtl7
    C*
     C                   endsr                                                  *INZSR
    C*
    C*====================================================================
    C* quote - Add record to quote file if it does not exist
    C*====================================================================
    C*
     C     quote         begsr
     C*
     C** **              close     waqtl7
     C** **              callp     runCmd(closeFile:255)
     C** **              open      waqtl7
    C*
     C                   exsr      empAgt
    C*
     C                   if        oldAgt = 'N'
    C*
     C                   clear                   pqt0p
    C*
     C                   eval      QTCO#  = EMCO#
     C                   eval      QTFND  = EMFND
     C                   eval      QTEMP# = EMEMP#
     C                   eval      QTDIV  = EMDIV
     C                   eval      QTAGT# = AHAGT#
    C*
     C                   eval      QTORDT = sysDate_n
     C                   eval      QTEFDT = EMEFD1
     C                   eval      QTCHAP = EMCHAP
     C                   eval      QTNAME = EMNAME
     C                   eval      QTTYPE = 'W'
     C                   eval      QTNBDT = sysDate_n
     C                   eval      QTPREM = *zeros
     C                   eval      QTSTAT = 'W'
     C*
     C                   exsr      writeWAQTP
    C*
     C                   endif
    C*
     C** **              close     waqtl7
     C** **              exsr      openWAQTL7
    C*
     C                   endsr
    C*
     C*====================================================================
     C* quoteSts - Compare records in WAQTP
     C*====================================================================
    C*
     C     quoteSts      begsr
     C*
     C                   select
     C                   when      update     = 'Y'
     C                   eval      addCorrect = 'Corrected'
     C                   other
     C                   eval      addCorrect = 'Correct'
     C                   endsl
     C*
     C                   clear                   svxfer
     C*
     C     qtky7         chain     pqt07                              60
     C*
     C                   dow       not *in60
     C*
     C                   if        QTTYPE = 'C' or QTTYPE = 'R'
     C                   eval      svxfer = QTTYPE
     C                   endif
     C*
     C     *cymd         test(d)                 QTWKDT                 98
     C  n98*cymd         move      QTWKDT        QTWKDT_usa
     C*
     C                   if        (QTTYPE = 'T' or QTTYPE = 'X') and
     C                             svxfer <> *blank
     C                   exsr      updatWAQTP
     C                   exsr      chkEmpBrk
     C                   except    quoteDtl
     C                   endif
     C*
     C     qtky7         reade     pqt07                                  60
     C*
     C                   enddo
    C*
     C                   endsr                                                  *INZSR
    C*
    C*================================================================
    C* updatWAQTP - Update records in WAQTP
    C*================================================================
    C*
     C     updatWAQTP    begsr
     C*
     C     QTrrn7        chain     waqtp                              60
     C*
     C                   eval      QTXFER = svxfer
     C*
     C                   exsr      WAQTPmaint
     C*
     C                   if        not *in60 and update = 'Y'
     C                   update    pqt0p
     C                   endif
    C*
     C                   endsr                                                  *INZSR
    C*
    C*====================================================================
    C* WAQTPmaint  - Load WAQTP maintenance fields
    C*====================================================================
    C*
     C     WAQTPmaint    begsr
     C*
     C                   adddur    1:*s          sysTime
    C*
     C                   eval      QTUSER = q1user
     C                   eval      QTPGM  = q1pgm
     C                   eval      QTDT   = sysDate_n
     C                   move      sysTime       QTTIME
    C*
     C                   endsr
    C*
    C*====================================================================
    C* writeWAQTP  - Write records to the WAQTP file
    C*====================================================================
    C*
     C     writeWAQTP    begsr
    C*
     C                   eval      QTWKDT = sysDate_n
     C*
     C                   exsr      WAQTPmaint
    C*
     C                   eval      QTIUSR = QTUSER
     C                   eval      QTIPGM = QTPGM
     C                   eval      QTIDT  = QTDT
     C                   eval      QTITM  = QTTIME
    C*
     C                   if        update = 'Y'
     C                   write     pqt0p
     C                   endif
     C*
     C     *cymd         test(d)                 QTWKDT                 98
     C  n98*cymd         move      QTWKDT        QTWKDT_usa
     C*
     C                   exsr      chkEmpBrk
     C                   except    agentDtl
     C                   except    quoteDtl
    C*
     C                   endsr
    C*
    C*____________________________________________________________________
    O*
     Oqsysprt   e            head           1 06
     O                       sysTime
     O                       title               96
     O                       sysDate            132
     O          e            head           2
     O                       q1pgm               10
     O                                          126 'PAGE:'
     O                       page          z    132
     O          e            heading        1
     O                                           33 ' CURR'
     O                                           45 ' AGT HIST '
     O                                           52 'QUOTE'
     O                                           70 'QTE'
     O                                           87 'ADDED OR '
     O          e            heading        1
     O                                              'CO#'
     O                                         +  2 'GRP'
     O                                         +  2 ' EMPLOYER'
     O                                         +  2 '  DIV'
     O                                           33 'AGENT'
     O                                           45 ' EFF DATE '
     O                                           52 'AGENT'
     O                                         +  2 'TYPE CHG DT'
     O                                         +  2 'TYP'
     O                                         +  2 'XFER'
     O                                         +  2 'CORRECTED'
     O          e            emplrDtl    2  0
     O                       EMCO#         z
     O                       EMFND         z   +  2
     O                       EMEMP#        z   +  2
     O                       EMDIV         z   +  2
     O          e            quoteDtl       1
     O                       QTAGT#        z     52
     O              n98      QTWKDT_usa        +  3
     O                       QTTYPE            +  3
     O                       QTXFER            +  5
     O                       addCorrect        +  3
     O          e            agentDtl       0
     O                       AHAGT#        z     33
     O              n99      AHEFDT_usa          45





     P*=====================================================================
     P* systemDate - get the system date as a date field
     P*  Returns - system date
     P*=====================================================================
     PsystemDate       B                   Export
     DsystemDate       PI              d
     D
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D
     D date            s               d
     C*
     C                   time                    dsdttm
     C     *mdy          move      dsdate        date
     C
     C                   return    date
     C*
     PsystemDate       E





     P*=====================================================================
     P* systemTime - get the system time as a time field
     P*  Returns - system time
     P*=====================================================================
     PsystemTime       B                   Export
     DsystemTime       PI              t
     D
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D
     D time            s               t
     C*
     C                   time                    dsdttm
     C                   move      dstime        time
     C
     C                   return    time
     C*
     PsystemTime       E





