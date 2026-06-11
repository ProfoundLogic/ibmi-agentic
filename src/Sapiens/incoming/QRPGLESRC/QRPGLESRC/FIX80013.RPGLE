    Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE FIX80013                                                                       *
    H**************************************************************************************
    H*  Description:  Correct Rising Medical                                              *
    H*                                                                                    *
    H*  Created: 06/29/12   Programmer C:280   Project:seq80013                           *
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FFAEOLG    IP   E           K DISK
    F*
    F* EOMB Transaction File by CO/SUB/FND/FYR/CASE/EOB#
    F*
     FFAEAL1    UF   E           K DISK
    F*
    F* EOMB Header Amount Transaction File by CO#/SUB#/FND/FYR/CASE/EOB#/RCN
    F*
     FWDPALA    IF   E           K DISK
    F*
    F* Claims checks payment file by co/fnd/fyr/case/eob#/seq#
    F*
     FQSYSPRT   O    F   80        printer oflind(*inof)
    F*
    D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D date            s              6  0
    D*
     DPSDS           ESDS                  Extname(SZQ1P)
    C*
    C* Define Fields
    C*
     C     *like         define    eatime        wktm
     C     *like         define    eadt          wkdt
    C*
    C* Key List
    C*
     C     key1          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C     key2          klist
     C                   kfld                    eoco#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
    C*
    C* Paramater for Update
    C*
     C     *entry        plist
     C                   parm                    Updt              1
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*----------------------------------------------------------------------
    C*  Read through FAEOP file
    C*----------------------------------------------------------------------
    C*
    C* Select RMS
    C*
b01  C                   If        eontwk = 'RMS'
    C*
    C* get faeap
    C*
     C     key1          chain     FAEAL1                             90
    C*
b02  C                   Dow       Not *in90
    C*
b03  C                   If        eapaid = *zeros
     C                   eval      eawcpc = *zeros
     C                   eval      eawcpd = *zeros
e03  C                   Endif
    C*
    C* get wdpap
    C*
     C     key2          chain     WDPALA                             91
    C*
b03  C                   Dow       Not *in91
    C*
b04  C                   If        patype = 'P'
    C*
    C* convert date for printing
    C*
     C                   eval      srdti = eawcpd
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cpd               6 0
    C*
     C                   eval      srdti = eawcnd
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cnd               6 0
    C*
b05  C                   If        papgtp = 'G' and pafein = 364276352
     C                             and pamod = 001
    C*
     C                   eval      eawcnc = pachk#
    C*
     C                   eval      srdti = cnd
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         patrdt
    C*
     C*                  eval      eawcnd = patrdt
    C*
x05  C                   else
    C*
     C                   eval      eawcpc = pachk#
    C*
     C                   eval      srdti = cpd
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         patrdt
    C*
     C*                  eval      eawcpd = patrdt
    C*
e05  C                   endif
    C*
b05  C                   if        *inof = *on
     C                   except    header
     C                   eval      *inof = *off
e05  C                   endif
    C*
    C* print
    C*
     C                   Except    detail
    C*
e04  C                   endif
    C*
     C     key2          reade     WDPALA                                 91
    C*
e03  C                   enddo
    C*
    C*
    C* update ?
    C*
b02  C                   If        Updt = 'Y'
     C                   update    pea01
e02  C                   Endif
    C*
     C     key1          reade     FAEAL1                                 90
    C*
e02  C                   enddo
    C*
e01  C                   endif
     C*
     C**********************************************************************
     C* *inzsr - Initialization
     C**********************************************************************
     C*
     C     *inzsr        begsr
     C*
     C                   Eval      *inof = *on
     C*
     C                   endsr
     C*
     C**********************************************************************
     O*
     Oqsysprt   e            header      3  1  3
     O                                           48 'Rising Medical'
     O          e            header         2
     O                                           49 'Changes to FAEOP'
     O          e            header         1
     O                                           50 '* ---- Provider ----*'
     O                                           76 '* ------ NWS -------*'
     O          e            header         2
     O                                           13 'EOB #'
     O                                           25 'Recon #'
     O                                           36 'Check #'
     O                                           47 'Date'
     O                                           62 'Check #'
     O                                           72 'Date'
     O*
     O          e            detail         1
     O                       EAEOB#        z     15
     O                       EARCN         z     23
     O                       EAWCPC        z     36
     O                       CPD                 50 '  /  /  '
     O                       EAWCNC        z     62
     O                       CND                 76 '  /  /  '
