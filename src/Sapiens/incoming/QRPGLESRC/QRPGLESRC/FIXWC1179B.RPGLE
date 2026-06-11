    hcopyright('(c) Copyright StoneRiver, Inc., 2006 - 2008')
    h*********************************************************************
    h/TITLE FIXWC1179B - Load Cash Dissolve Hierarchy for a new group
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    h*********************************************************************
    h* Called from:                                                      *
    h*   Command line                                                    *
    h*                                                                   *
    h* Calls to:                                                         *
    h*   None                                                            *
    h*                                                                   *
    h* Created:  01/20/06     C:136    PROJECT: WC1179                   *
    h*********************************************************************
    h* Revisions:                                                        *
    h*                                                                   *
    h* 07/30/21 C:300  Project: WC2196 - Group Configuraion              *
    h*                 Include WTGRPPOLCG program in callstack.          *
    h*                                                                   *
    h* 11/24/09 C:235  Support: SEQ69661                                 *
    h*                 Add record in WTC3P independent of WDCDP records. *
    h*                                                                   *
    h* 06/04/09 C:136  Project: WC1769                                   *
    h*                 Correction for PRISS 10                           *
    h*                                                                   *
     h* 01/07/09  C:33  Project: WC1769    Priss# 10                      *
    h*                 Allow for rerunable processing.  Only write the   *
    h*                  new records once.                                *
    h*********************************************************************

     fwdcdp     uf a e             disk
     f***  Begin Add  ***  01/07/09  **************************************
     fwdcdl1    if   e           k disk    rename(pcd01:pcdl1)
     fwdcdl     if   e           k disk    rename(pcd01:pcdl0)
     f***  End   Add  ***  01/07/09  **************************************
     fwtctl     if   e           k disk
     fwtc3p     uf a e             disk
     f***  Begin Add  ***  11/24/09  **************************************
     fwtc3l     if   e           k disk    rename(pc301:pc3l0)
     f***  End   Add  ***  11/24/09  **************************************
     fwdf2l     uf a e           k disk

     d psds          esds                  extname(szq1p)
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
     d FIXWC1179B      pr
     d  @@co#                         3  0
     d  @@fnd                         3  0
     d FIXWC1179B      pi
     d  @@co#                         3  0
     d  @@fnd                         3  0

     d/copy sprdate

     d***  Begin Add  ***  01/07/09  **************************************
     d InCallStack     pr             1n
     d  pgm                          10    const
     d***  End   Add  ***  01/07/09  **************************************

     d  kkfyr          s              3  0
     d  xxhier         s                   like(cdhier)
     d  x1hier         s                   like(cdhier)
     d  x              s              2  0
     d***  Begin Add  ***  01/07/09  **************************************
     d  xxPgm          s                   like(Q1PGM)
     d***  End   Add  ***  01/07/09  **************************************

     c     ctky          klist
     c                   kfld                    @@co#

     c***  Begin Add  ***  01/07/09  **************************************
     c     keyWDCDL1     klist
     c                   kfld                    @@Co#
     c                   kfld                    @@Fnd
     c                   kfld                    CTCTYP
     c*
     c     keyWDCDL      klist
     c                   kfld                    @@Co#
     c                   kfld                    @@Fnd
     c***  End   Add  ***  01/07/09  **************************************

     c***  Begin Add  ***  01/07/09  **************************************
     c     keyWDCDL      setgt     pcdl0
     c     keyWDCDL      readpe    pcdl0
     c                   select
      *** beg chg ***** 06/04/09 *****************************************
     c***********        when      %found
     c***********        eval      xxhier = %rem(CDHIER:1000)
     c***********        eval      xxhier = CDHIER - %rem(CDHIER:1000)
     c***********                         + 1000
     c                   when      not(%eof)
     c                   if        CDHIER > 10000
     c                   eval      xxhier = CDHIER + 1000
     c                   else
     c                   eval      xxhier = 10000
     c                   endif
      *** end chg ***** 06/04/09 *****************************************
     c                   other
     c                   eval      xxhier = 10000
     c                   endsl

     c                   select
     c                   when      InCallStack('W0010R62')
     c                   eval      xxPgm = 'W0010R62'
      *** beg add ***** 07/30/21 *****************************************
     c                   when      InCallStack('WTGRPPOLCG')
     c                   eval      xxPgm = 'WTGRPPOLCG'
      *** end add ***** 07/30/21 *****************************************
     c                   other
     c                   eval      xxPgm = Q1PGM
     c                   endsl
     c***  End   Add  ***  01/07/09  **************************************
      *** beg del ***** 06/04/09 *****************************************
     c***********        eval      CDCO#  = @@CO#
     c***********        eval      CDFND  = @@FND
     c***********        eval      CDIUSR = Q1USER
     c***********        eval      CDUSER = Q1USER
     c*****Begin*Chg  ***  01/07/09  **************************************
     c***********        eval      CDIPGM = Q1PGM
     c***********        eval      CDPGM  = Q1PGM
     c***********        eval      CDIPGM = xxPgm
     c***********        eval      CDPGM  = xxPgm
     c*****End***Chg  ***  01/07/09  **************************************
     c***********        eval      CDIDT  = DateTo7(SystemDate)
     c***********        eval      CDDT   = CDIDT
     c***********        eval      CDTIME = TimeTo6(SystemTime)
     c***********        eval      C3CO#  = @@CO#
     c***********        eval      C3FND  = @@FND
     c***********        eval      C3IUSR = Q1USER
     c***********        eval      C3USER = Q1USER
     c*****Begin*Chg  ***  01/07/09  **************************************
     c***********        eval      C3IPGM = Q1PGM
     c***********        eval      C3PGM  = Q1PGM
     c***********        eval      C3IPGM = xxPgm
     c***********        eval      C3PGM  = xxPgm
     c*****End***Chg  ***  01/07/09  **************************************
     c***********        eval      C3IDT  = CDDT
     c***********        eval      C3DT   = CDDT
     c***********        eval      C3TIME = CDTIME
      *** end del ***** 06/04/09 *****************************************

     c***  Begin Del  ***  01/07/09  **************************************
     c**********         eval      xxhier = 10000
     c***  End   Del  ***  01/07/09  **************************************

     c     ctky          setll     pct01
     c     ctky          reade     pct01
     c                   dow       not(%eof)

     c***  Begin Add  ***  01/07/09  **************************************
     c     keyWDCDL1     chain     pcdl1
     c                   if        not %found
     c                   exsr      LoadCDValues
     c***  End   Add  ***  01/07/09  **************************************
     c                   eval      CDCTYP = CTCTYP
     c                   eval      CDNBCD = *blank
     c                   eval      CDCPCD = *blank
     c                   eval      CDHIER = xxhier
     c                   add       1000          xxhier
     c                   write     pcd01
      *
      * Write another record for Installment Fees or Deposit Tax
      *
     c                   if        CTCTYP = 'F' or
     c                             CTCTYP = 'D'
     c                   add       1000          xxhier
     c                   write     pcd01
     c                   endif
      *
      * Write Premium Payment code record.
      *
     c***  Begin Del  ***  11/24/09  **************************************
     c******             if        CTCTYP = 'P'
     c******             eval      C3CTYP = 'P'
     c******             exsr      LoadCDValues
     c******             write     pc301
     c******             endif
     c***  End  Del *****  11/24/09  **************************************

     c***  Begin Add  ***  01/07/09  **************************************
     c                   endif
     c***  Begin Add  ***  11/24/09  **************************************
     c     keyWDCDL      chain     pc3l0
     c                   if        not %found
     c                   if        CTCTYP = 'P'
     c                   eval      C3CTYP = 'P'
     c                   exsr      LoadCDValues
     c                   write     pc301
     c                   endif
     c                   endif
     c***  End   Add  ***  11/24/09  **************************************
     c***  End   Add  ***  01/07/09  **************************************

     c     ctky          reade     pct01
     c                   enddo

     c                   eval      *inlr = *on
     c***  Begin Add  ***  01/07/09  **************************************
     c*====================================================================
     c* Load standard values for WDCDP
     c*====================================================================
     c     LoadCDValues  begsr

     c                   eval      CDCO#  = @@CO#
     c                   eval      CDFND  = @@FND
     c                   eval      CDIUSR = Q1USER
     c                   eval      CDUSER = Q1USER
     c                   eval      CDIPGM = xxPgm
     c                   eval      CDPGM  = xxPgm
     c                   eval      CDIDT  = DateTo7(SystemDate)
     c                   eval      CDDT   = CDIDT
     c                   eval      CDTIME = TimeTo6(SystemTime)
     c                   eval      C3CO#  = @@CO#
     c                   eval      C3FND  = @@FND
     c                   eval      C3IUSR = Q1USER
     c                   eval      C3USER = Q1USER
     c                   eval      C3IPGM = Q1PGM
     c                   eval      C3PGM  = Q1PGM
     c                   eval      C3IPGM = xxPgm
     c                   eval      C3PGM  = xxPgm
     c                   eval      C3IDT  = CDDT
     c                   eval      C3DT   = CDDT
     c                   eval      C3TIME = CDTIME

     c                   endsr
     c***  End   Add  ***  01/07/09  **************************************





     p***  Begin Add  ***  01/07/09  **************************************
     p*====================================================================
     p* Determine if a program is in the call stack
     p*====================================================================
     p InCallStack     b
     d InCallStack     pi             1n
     d  pgm                          10    const

     d xxPgm           s             10
     d found           s              1

     c                   call      'SRCHKPGM'
     c                   parm      pgm           xxPgm
     c                   parm                    found

     c                   return    found

     p InCallStack     e
     p***  End   Add  ***  01/07/09  **************************************
