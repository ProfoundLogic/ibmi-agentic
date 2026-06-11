     H* (C) Copyright Progressive Data Solutions, 2007
     H/TITLE FIXPOLST                                                                       *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
    H*  CREATED: 04/04/07  C:188 Project: FL061 Multi State Claim Processing              *
     H*                                                                                    *
     H**************************************************************************************
     Ffaeolg    UF   E           K DISK
     Ffmcml1    IF   E           K DISK
     Fwdell     IF   E           K DISK    usropn
     Fwmrsl     IF   E           K DISK    usropn

     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)

     D  ##wcif         S              1
     D  ##mcif         S              1
     D  ##wcin         S              1
     D  ##ar           S              1
     D  ##ntwk         S              1
     D  ##ur           S              1
     D  ##med          S              1
     D  ##edi          S              1
     D  ##wcdk         S              1
     D  ##rhab         S              1
     D  ##mss          S              1
     D  ##fsch         S              1
     D  ##prvb         S              1
     D  ##npwc         S              1
     D  kkco#          S              2
     D  ##SUB#         S              5
     D  #1FND          S              3
     D  isWC           S              1
     D  kkdiv          S                   like(eldiv)
     D  kkemp#         S                   like(elemp#)

     C     clky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase

     C     elky          klist
     C                   kfld                    eoco#
     C                   kfld                    eofnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     *
     C     rsky          klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
     *
     * Call API for System Flags
     *
     C                   call      'SA001R'
     C                   parm      ' '           ##wcif
     C                   parm      ' '           ##mcif
     C                   parm      ' '           ##wcin
     C                   parm      ' '           ##ar
     C                   parm      ' '           ##ntwk
     C                   parm      ' '           ##ur
     C                   parm      ' '           ##med
     C                   parm      ' '           ##edi
     C                   parm      ' '           ##wcdk
     C                   parm      ' '           ##rhab
     C                   parm      ' '           ##mss
     C                   parm      ' '           ##fsch
     C                   parm      ' '           ##prvb
     C                   parm      ' '           ##npwc
     C*
     C* If WC is loaded, then open WDELL/WTABL, set flag
     C*
     C                   if        ##WCIF = 'Y' and ##NPWC <> 'Y'
     C                   open      wmrsl
     C                   open      wdell
     C                   eval      isWC = 'Y'
     C                   else
     C                   eval      isWC = 'N'
     C                   endif
     C*
     C* Process FAEOP where EOSYS='P' or EOSYS='H'
     C*
     C     *loval        setll     peo01
     C                   read      peo01                                  99
     C                   dow       *in99 = *off
     C                   if        eosys = 'P' or eosys = 'H'
     C*
     C* Determine if standalone account
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        ##SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
     C*
     C* Retrieve Claim
     C*
     C     clky          chain     pcm01                              98
     C                   if        *in98 = *off
     C*
     C* If outside acct or Worker's Comp not installed then set EOPOST=CMPOST
     C*
     C                   if        ##OUTS = 'Y' or  isWC = 'N'
     C                   eval      eopost = cmpost
     C*
     C                   else
      *
      * Else chain to Employer Detail File
      *
     C                   eval      kkemp# = cmemp#
     C                   eval      kkdiv = cmdiv
     C     elky          chain     pel01                              96
     C                   if        *in96 = *off
      *
      * then to Rate Set Code Master File, use rsjrst
      *
     C     rsky          chain     prs01                              97
     C                   if        *in97 = *off
     C                   eval      eopost = rsjrst
     C                   endif
     C                   endif
     C                   endif                                                  end ##OUTS Y
      *
      * Update pgm/date/time
      *
     C                   eval      eopgm = q1pgm
     C                   exsr      Timestamp
     C                   move      sys_time      eotime
     C     *cymd         move      sys_date      eodt
      *
     C                   update    peo01
      *
     C                   endif                                                  claim found
     C                   endif                                                  eosys P/H
     C                   read      peo01                                  99
     C                   enddo
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Timestamp - Updates time/date
     C*=====================================================================
     C     Timestamp     begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C                   endsr
     C*
